run_step3 <- function(regions_info, classification_results, 
                      sentiment_dict_file = NULL, status_callback = NULL) {
  set.seed(1234)
  update_status <- function(msg) {
    if(!is.null(status_callback)) status_callback(msg)
    cat(msg, "\n")
  }
  
  library(ggplot2)
  library(reshape2)
  library(plotly)
  library(dplyr)
  library(tidyr)
  
  update_status("Начало Этапа 3: Анализ тональности")
  
  # Загрузка словаря тональности
  if(!is.null(sentiment_dict_file) && file.exists(sentiment_dict_file)) {
    update_status("Загрузка словаря тональности...")
    lines <- readLines(sentiment_dict_file, warn = FALSE)
    data_lines <- lines[!grepl("^!", lines)]
    
    parse_line <- function(line) {
      parts <- strsplit(line, ", ")[[1]]
      if(length(parts) < 6) parts <- c(parts, rep(NA, 6 - length(parts)))
      if(!is.na(parts[6])) parts[6] <- gsub('^"|"$', '', parts[6])
      return(parts[1:6])
    }
    
    parsed_data <- lapply(data_lines, parse_line)
    rusentilex <- data.frame(
      word = sapply(parsed_data, `[`, 1),
      pos = sapply(parsed_data, `[`, 2),
      lemma = sapply(parsed_data, `[`, 3),
      sentiment = sapply(parsed_data, `[`, 4),
      source = sapply(parsed_data, `[`, 5),
      concept = sapply(parsed_data, `[`, 6),
      stringsAsFactors = FALSE
    )
    rm(lines, data_lines, parsed_data)
    
    # Создание словарей
    unigrams <- rusentilex[
      rusentilex$sentiment %in% c("positive", "negative") & 
        rusentilex$pos %in% c("Noun", "Adj", "Verb", "Adv"), 
    ]
    unigrams$weight <- ifelse(unigrams$sentiment == "positive", 1, -1)
    unigram_dict <- setNames(unigrams$weight, unigrams$lemma)
    
    bigrams <- rusentilex[
      rusentilex$pos %in% c("NG", "VG", "AdjG", "AdvG", "PredicativeG", "ParticipleG") & 
        rusentilex$sentiment %in% c("positive", "negative"), 
    ]
    bigrams$weight <- ifelse(bigrams$sentiment == "positive", 1, -1)
    bigrams$word_lower <- tolower(bigrams$word)
    bigram_dict <- setNames(bigrams$weight, bigrams$word_lower)
  } else {
    # Пустые словари если файл не найден
    unigram_dict <- c()
    bigram_dict <- c()
  }
  
  # Функция расчета тональности документа (возвращает суммарный вес)
  get_document_sentiment_score <- function(text, unigram_dict, bigram_dict) {
    text <- as.character(text)
    if(is.na(text) || nchar(text) == 0) {
      return(NA)
    }
    
    text_processed <- tolower(text)
    total_score <- 0
    
    # Униграммы
    words <- strsplit(text_processed, "\\s+")[[1]]
    for(word in words) {
      if(word %in% names(unigram_dict)) {
        total_score <- total_score + unigram_dict[[word]]
      }
    }
    
    # Биграммы
    for(bg in names(bigram_dict)) {
      if(grepl(bg, text_processed, fixed = TRUE)) {
        total_score <- total_score + bigram_dict[[bg]]
      }
    }
    
    return(total_score)
  }
  
  # Функция определения класса тональности документа
  get_sentiment_class <- function(score) {
    if(is.na(score)) return(NA)
    if(score > 0) return("positive")
    if(score < 0) return("negative")
    return("neutral")
  }
  
  # Агрегация по целям
  update_status("Объединение текстов по целям внутри регионов...")
  
  region_goal_texts <- list()
  unique_regions <- unique(classification_results$region)
  goal_names <- unique(classification_results$assigned_group)
  
  for(reg in unique_regions) {
    region_class <- classification_results[classification_results$region == reg, ]
    region_texts <- regions_info[[reg]]$тексты
    
    goal_texts <- list()
    for(g in goal_names) {
      goal_texts[[g]] <- character(0)
    }
    
    for(i in 1:nrow(region_class)) {
      goal <- region_class$assigned_group[i]
      doc_indices <- as.numeric(strsplit(region_class$doc_numbers[i], "; ")[[1]])
      
      if(length(doc_indices) > 0 && all(doc_indices <= length(region_texts))) {
        goal_texts[[goal]] <- c(goal_texts[[goal]], region_texts[doc_indices])
      }
    }
    
    region_goal_texts[[reg]] <- goal_texts
  }
  
  # Расчет индекса тональности
  update_status("Расчет индекса тональности (позитивные - негативные) / общее количество...")
  
  regions <- names(region_goal_texts)
  
  # Матрицы для хранения результатов
  sentiment_index_matrix <- matrix(NA, nrow = length(regions), ncol = length(goal_names))
  rownames(sentiment_index_matrix) <- regions
  colnames(sentiment_index_matrix) <- goal_names
  
  count_matrix <- matrix(0, nrow = length(regions), ncol = length(goal_names))
  rownames(count_matrix) <- regions
  colnames(count_matrix) <- goal_names
  
  positive_count_matrix <- matrix(0, nrow = length(regions), ncol = length(goal_names))
  rownames(positive_count_matrix) <- regions
  colnames(positive_count_matrix) <- goal_names
  
  negative_count_matrix <- matrix(0, nrow = length(regions), ncol = length(goal_names))
  rownames(negative_count_matrix) <- regions
  colnames(negative_count_matrix) <- goal_names
  
  neutral_count_matrix <- matrix(0, nrow = length(regions), ncol = length(goal_names))
  rownames(neutral_count_matrix) <- regions
  colnames(neutral_count_matrix) <- goal_names
  
  # Проход по всем регионам и SDG целям
  for(reg in regions) {
    update_status(sprintf("Обработка региона: %s", reg))
    
    for(goal in goal_names) {
      texts <- region_goal_texts[[reg]][[goal]]
      
      if(length(texts) > 0) {
        # Рассчитываем тональность для каждого документа
        scores <- sapply(texts, function(t) {
          get_document_sentiment_score(t, unigram_dict, bigram_dict)
        })
        
        # Классифицируем документы
        sentiment_classes <- sapply(scores, get_sentiment_class)
        
        # Подсчет количества
        n_positive <- sum(sentiment_classes == "positive", na.rm = TRUE)
        n_negative <- sum(sentiment_classes == "negative", na.rm = TRUE)
        n_neutral <- sum(sentiment_classes == "neutral", na.rm = TRUE)
        n_total <- length(texts)
        
        # Расчет индекса тональности
        # Индекс = (позитивные - негативные) / общее количество
        sentiment_index <- (n_positive - n_negative) / n_total
        
        # Сохраняем результаты
        sentiment_index_matrix[reg, goal] <- round(sentiment_index, 4)
        count_matrix[reg, goal] <- n_total
        positive_count_matrix[reg, goal] <- n_positive
        negative_count_matrix[reg, goal] <- n_negative
        neutral_count_matrix[reg, goal] <- n_neutral
      }
    }
  }
  
  # Создание датасета с подробной информацией
  final_data <- list()
  for(reg in regions) {
    for(goal in goal_names) {
      n_total <- count_matrix[reg, goal]
      if(n_total > 0) {
        final_data[[length(final_data) + 1]] <- data.frame(
          region = reg,
          goal = goal,
          n_documents = n_total,
          n_positive = positive_count_matrix[reg, goal],
          n_negative = negative_count_matrix[reg, goal],
          n_neutral = neutral_count_matrix[reg, goal],
          sentiment_index = sentiment_index_matrix[reg, goal],
          sentiment_class = ifelse(sentiment_index_matrix[reg, goal] > 0, "positive",
                                   ifelse(sentiment_index_matrix[reg, goal] < 0, "negative", "neutral")),
          stringsAsFactors = FALSE
        )
      } else {
        final_data[[length(final_data) + 1]] <- data.frame(
          region = reg,
          goal = goal,
          n_documents = 0,
          n_positive = 0,
          n_negative = 0,
          n_neutral = 0,
          sentiment_index = NA,
          sentiment_class = "no_data",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  final_df <- do.call(rbind, final_data)
  write.csv(final_df, "region_goal_sentiment.csv", row.names = FALSE)
  
  # Сохранение отдельных матриц для визуализации
  saveRDS(sentiment_index_matrix, "sentiment_index_matrix.rds")
  saveRDS(positive_count_matrix, "positive_count_matrix.rds")
  saveRDS(negative_count_matrix, "negative_count_matrix.rds")
  saveRDS(neutral_count_matrix, "neutral_count_matrix.rds")
  
  # Создание статической тепловой карты (индекс тональности)
  update_status("Создание тепловой карты...")
  
  sentiment_df <- melt(sentiment_index_matrix)
  colnames(sentiment_df) <- c("Region", "Goal", "SentimentIndex")
  
  # Добавляем информацию о количестве документов
  count_df <- melt(count_matrix)
  colnames(count_df) <- c("Region", "Goal", "Count")
  plot_data <- merge(sentiment_df, count_df, by = c("Region", "Goal"))
  plot_data$Label <- ifelse(is.na(plot_data$SentimentIndex), "NA", 
                            sprintf("%.3f", plot_data$SentimentIndex))
  
  p <- ggplot(plot_data, aes(x = Goal, y = Region, fill = SentimentIndex)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), size = 3) +
    scale_fill_gradient2(
      low = "#d73027", 
      mid = "#FAC503", 
      high = "#1a9850", 
      midpoint = 0, 
      na.value = "#e0e0e0",
      limits = c(-1, 1),
      name = "Индекс\nтональности"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = "Индекс тональности по регионам и SDG целям",
      subtitle = "Индекс = (позитивные - негативные) / общее количество документов",
      x = "SDG Цели",
      y = "Регионы"
    )
  
  ggsave("sentiment_heatmap.png", p, width = 14, height = 8, dpi = 150)
  
  # Интерактивная тепловая карта
  p2 <- plot_ly(
    x = colnames(sentiment_index_matrix),
    y = rownames(sentiment_index_matrix),
    z = sentiment_index_matrix,
    type = "heatmap",
    colorscale = list(
      list(0, "#d73027"),
      list(0.5, "#FAC503"),
      list(1, "#1a9850")
    ),
    zmin = -1,
    zmax = 1,
    zmid = 0,
    hovertemplate = paste(
      "<b>Регион:</b> %{y}<br>",
      "<b>SDG:</b> %{x}<br>",
      "<b>Индекс тональности:</b> %{z:.3f}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(
        text = "Индекс тональности: Регионы × SDG цели",
        font = list(size = 14)
      ),
      xaxis = list(title = "SDG Цели", tickangle = 45),
      yaxis = list(title = "Регионы"),
      coloraxis = list(
        colorbar = list(
          title = "Индекс<br>тональности",
          tickvals = c(-1, -0.5, 0, 0.5, 1),
          ticktext = c("-1.0", "-0.5", "0.0", "0.5", "1.0")
        )
      )
    )
  
  htmlwidgets::saveWidget(p2, "sentiment_heatmap.html", selfcontained = TRUE)
  
  # Вывод статистики
  update_status(sprintf("Всего строк в результате: %d", nrow(final_df)))
  update_status(sprintf("Регионов: %d", length(unique(final_df$region))))
  update_status(sprintf("SDG целей: %d", length(unique(final_df$goal))))
  
  sentiment_dist <- final_df %>%
    filter(!is.na(sentiment_class), sentiment_class != "no_data") %>%
    group_by(sentiment_class) %>%
    summarise(
      count = n(),
      avg_index = mean(sentiment_index, na.rm = TRUE),
      .groups = 'drop'
    )
  
  for(i in 1:nrow(sentiment_dist)) {
    update_status(sprintf("  %s: %d комбинаций (ср. индекс: %.3f)", 
                          sentiment_dist$sentiment_class[i],
                          sentiment_dist$count[i],
                          sentiment_dist$avg_index[i]))
  }
  
  update_status(sprintf("\nОбщий средний индекс тональности: %.3f", 
                        mean(final_df$sentiment_index, na.rm = TRUE)))
  
  update_status(sprintf("\nЭтап 3 завершен. Результаты сохранены."))
  
  return(list(
    final_dataset = final_df,
    sentiment_index_matrix = sentiment_index_matrix,
    count_matrix = count_matrix,
    positive_count_matrix = positive_count_matrix,
    negative_count_matrix = negative_count_matrix,
    neutral_count_matrix = neutral_count_matrix
  ))
}