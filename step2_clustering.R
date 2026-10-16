run_step2 <- function(regions_info, sdg_groups, stopwords_clust_file = NULL,
                      minPts = 15, max_keywords = 10, status_callback = NULL) {
  set.seed(1234)
  update_status <- function(msg) {
    if(!is.null(status_callback)) status_callback(msg)
    cat(msg, "\n")
  }
  
  # Библиотеки
  library(reticulate)
  library(tm)
  library(Matrix)
  library(uwot)
  library(dbscan)
  library(dplyr)
  
  update_status("Начало Этапа 2: Кластеризация и SDG классификация")
  
  # Загрузка модели Sentence-BERT
  update_status("Загрузка модели Sentence-BERT...")
  transformers <- import("sentence_transformers")
  sbert_model <- transformers$SentenceTransformer('intfloat/multilingual-e5-large')
  
  # Загрузка стоп-слов для кластеризации
  if(!is.null(stopwords_clust_file) && file.exists(stopwords_clust_file)) {
    stop_words_clust <- readLines(stopwords_clust_file, encoding = "UTF-8")
  } else {
    stop_words_clust <- c("и", "в", "не", "что", "он", "на", "я", "с", "как")
  }
  
  # Предвычисление эмбеддингов групп
  update_status("Предвычисление эмбеддингов групп...")
  
  precompute_sdg_embeddings <- function(sdg_groups, model) {
    sdg_embeddings <- list()
    for(sdg_name in names(sdg_groups)) {
      keywords <- sdg_groups[[sdg_name]]
      embeddings <- model$encode(keywords, normalize_embeddings = TRUE)
      centroid <- colMeans(embeddings)
      sdg_embeddings[[sdg_name]] <- list(centroid = centroid, keywords = keywords)
    }
    return(sdg_embeddings)
  }
  
  sdg_cache <- precompute_sdg_embeddings(sdg_groups, sbert_model)
  update_status(sprintf("Предвычислено групп: %d", length(sdg_cache)))
  
  # Функции для обработки
  create_tfidf_matrix <- function(texts) {
    corpus <- Corpus(VectorSource(texts))
    corpus <- tm_map(corpus, removeWords, stop_words_clust)
    dtm <- DocumentTermMatrix(corpus)
    dtm_tfidf <- weightTfIdf(dtm)
    dtm_reduced <- removeSparseTerms(dtm_tfidf, sparse = 0.95)
    
    term_names <- dtm_reduced$dimnames$Terms
    
    sparse_matrix <- sparseMatrix(
      i = dtm_reduced$i, j = dtm_reduced$j, x = dtm_reduced$v,
      dims = c(dtm_reduced$nrow, dtm_reduced$ncol),
      dimnames = list(NULL, term_names)
    )
    colnames(sparse_matrix) <- term_names
    
    row_sums <- rowSums(sparse_matrix)
    zero_rows <- which(row_sums == 0)
    
    if(length(zero_rows) > 0) {
      valid_rows <- which(row_sums > 0)
      clean_matrix <- sparse_matrix[valid_rows, ]
      col_sums <- colSums(clean_matrix)
      valid_cols <- which(col_sums > 0)
      clean_matrix <- as.matrix(clean_matrix[, valid_cols])
      term_names <- colnames(clean_matrix)
    } else {
      clean_matrix <- as.matrix(sparse_matrix)
      valid_rows <- 1:nrow(sparse_matrix)
      term_names <- colnames(clean_matrix)
    }
    
    return(list(matrix = clean_matrix, terms = term_names, valid_rows = valid_rows))
  }
  
  extract_cluster_keywords <- function(clusters, tfidf_data, max_kw = 10) {
    if(is.matrix(tfidf_data)) {
      tfidf_matrix <- tfidf_data
      all_terms <- colnames(tfidf_data)
      valid_rows <- 1:nrow(tfidf_data)
    } else {
      tfidf_matrix <- tfidf_data$matrix
      all_terms <- tfidf_data$terms
      valid_rows <- tfidf_data$valid_rows
    }
    
    if(is.null(all_terms) || length(all_terms) == 0) {
      all_terms <- paste0("term_", 1:ncol(tfidf_matrix))
    }
    
    cluster_keywords <- list()
    unique_clusters <- setdiff(unique(clusters$cluster), 0)
    
    for(cluster_id in sort(unique_clusters)) {
      cluster_indices <- which(clusters$cluster == cluster_id)
      if(length(cluster_indices) < 1) next
      
      term_scores <- if(length(cluster_indices) == 1) {
        tfidf_matrix[cluster_indices, ]
      } else {
        colMeans(tfidf_matrix[cluster_indices, , drop = FALSE])
      }
      
      top_indices <- order(term_scores, decreasing = TRUE)
      top_scores <- term_scores[top_indices]
      nonzero_scores <- top_scores[top_scores > 0]
      if(length(nonzero_scores) == 0) next
      
      n_terms <- min(length(nonzero_scores), max_kw)
      top_term_indices <- top_indices[1:n_terms]
      
      if(length(all_terms) >= max(top_term_indices)) {
        top_terms <- all_terms[top_term_indices]
      } else {
        top_terms <- all_terms[top_term_indices[top_term_indices <= length(all_terms)]]
      }
      
      clean_terms <- unique(top_terms[nchar(top_terms) >= 3])
      
      if(length(clean_terms) > 0) {
        cluster_keywords[[as.character(cluster_id)]] <- list(
          keywords = clean_terms,
          size = length(cluster_indices),
          indices = valid_rows[cluster_indices]
        )
      }
    }
    
    return(cluster_keywords)
  }
  
  classify_clusters_fast <- function(cluster_keywords, sdg_cache, model) {
    results <- list()
    
    for(cluster_id in names(cluster_keywords)) {
      cluster_data <- cluster_keywords[[cluster_id]]
      keywords <- cluster_data$keywords
      if(length(keywords) == 0) next
      
      kw_embeddings <- tryCatch({
        emb <- model$encode(keywords, normalize_embeddings = TRUE)
        if(is.vector(emb)) emb <- matrix(emb, nrow = 1)
        emb
      }, error = function(e) NULL)
      
      if(is.null(kw_embeddings)) next
      
      cluster_centroid <- if(nrow(kw_embeddings) > 1) {
        colMeans(kw_embeddings)
      } else {
        kw_embeddings[1, ]
      }
      
      scores <- sapply(sdg_cache, function(sdg_data) {
        sum(cluster_centroid * sdg_data$centroid)
      })
      
      best_group <- names(which.max(scores))
      sorted_scores <- sort(scores, decreasing = TRUE)
      
      results[[cluster_id]] <- list(
        assigned_group = best_group,
        confidence = max(scores),
        top_3_groups = names(sorted_scores)[1:min(3, length(sorted_scores))],
        keywords = keywords
      )
    }
    
    return(results)
  }
  
  # Обработка всех регионов
  all_results <- list()
  region_names <- names(regions_info)
  
  for(i in seq_along(region_names)) {
    region_name <- region_names[i]
    region_data <- regions_info[[region_name]]
    texts <- region_data$тексты
    
    update_status(sprintf("[%d/%d] %s (%d док.)", 
                          i, length(region_names), region_name, length(texts)))
    
    if(length(texts) < 10) {
      update_status(sprintf("  Пропущен - мало документов"))
      next
    }
    
    tryCatch({
      tfidf_data <- create_tfidf_matrix(texts)
      
      n_neighbors <- min(15, nrow(tfidf_data$matrix) - 1)
      n_components <- min(10, ncol(tfidf_data$matrix))
      
      umap_result <- uwot::umap(
        tfidf_data$matrix,
        n_components = n_components,
        metric = "cosine",
        n_neighbors = n_neighbors,
        min_dist = 0.1,
        verbose = FALSE
      )
      
      actual_minPts <- min(minPts, nrow(umap_result) - 1)
      clusters <- hdbscan(umap_result, minPts = actual_minPts)
      
      n_clusters <- length(setdiff(unique(clusters$cluster), 0))
      update_status(sprintf("  Кластеров: %d", n_clusters))
      
      if(n_clusters == 0) next
      
      cluster_keywords <- extract_cluster_keywords(clusters, tfidf_data, max_keywords)
      classification_results <- classify_clusters_fast(cluster_keywords, sdg_cache, sbert_model)
      
      # Формирование результатов
      dataset_rows <- list()
      for(cluster_id in names(classification_results)) {
        result <- classification_results[[cluster_id]]
        cluster_info <- cluster_keywords[[cluster_id]]
        
        dataset_rows[[length(dataset_rows) + 1]] <- data.frame(
          region = region_name,
          cluster = as.numeric(cluster_id),
          n_documents = cluster_info$size,
          doc_numbers = paste(cluster_info$indices, collapse = "; "),
          keywords = paste(result$keywords, collapse = ", "),
          assigned_group = result$assigned_group,
          confidence = round(result$confidence, 3),
          alternative_groups = paste(
            setdiff(result$top_3_groups, result$assigned_group), 
            collapse = "; "
          ),
          stringsAsFactors = FALSE
        )
      }
      
      region_results <- do.call(rbind, dataset_rows)
      all_results[[region_name]] <- region_results
      
      # Сохранение отдельного файла региона
      output_file <- sprintf("region_%s_classification.csv", 
                             gsub("[/ ]", "_", region_name))
      write.csv(region_results, output_file, row.names = FALSE)
      
    }, error = function(e) {
      update_status(sprintf("  Ошибка: %s", e$message))
    })
  }
  
  # Объединение и сохранение
  if(length(all_results) > 0) {
    final_dataset <- do.call(rbind, all_results)
    write.csv(final_dataset, "all_regions_classification.csv", row.names = FALSE)
    update_status(sprintf("Этап 2 завершен. Всего строк: %d", nrow(final_dataset)))
    return(final_dataset)
  }
  
  update_status("Этап 2 завершен без результатов")
  return(NULL)
}