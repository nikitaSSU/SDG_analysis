run_step1 <- function(base_dir, stopwords_file = NULL, status_callback = NULL) {
  set.seed(1234)
  # Функция для обновления статуса
  update_status <- function(msg) {
    if(!is.null(status_callback)) status_callback(msg)
    cat(msg, "\n")
  }
  
  # Загрузка библиотек
  library(readxl)
  library(dplyr)
  library(udpipe)
  library(tm)
  library(stringr)
  
  update_status("Начало Этапа 1: Сбор и предобработка данных")
  
  # Проверка существования директории
  if(!dir.exists(base_dir)) {
    stop("Директория не найдена: ", base_dir)
  }
  
  # Сбор xlsx файлов
  update_status("Поиск xlsx файлов...")
  all_files <- list.files(
    path = base_dir,
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )
  update_status(sprintf("Найдено файлов: %d", length(all_files)))
  
  if(length(all_files) == 0) {
    stop("Не найдено xlsx файлов в директории")
  }
  
  # Чтение файлов
  results <- list()
  for(file_path in all_files) {
    region <- basename(dirname(file_path))
    file_name <- basename(file_path)
    
    tryCatch({
      data <- read_excel(file_path, col_names = FALSE)
      if(ncol(data) >= 1 && nrow(data) > 0) {
        texts <- data[[1]][!is.na(data[[1]])]
        file_results <- data.frame(
          регион = rep(region, length(texts)),
          файл = rep(file_name, length(texts)),
          текст = texts,
          stringsAsFactors = FALSE
        )
        results[[length(results) + 1]] <- file_results
      }
    }, error = function(e) {
      warning("Ошибка чтения ", file_path, ": ", e$message)
    })
  }
  
  # Объединение
  if(length(results) > 0) {
    dataset <- bind_rows(results)
  } else {
    stop("Не удалось прочитать данные из файлов")
  }
  
  update_status(sprintf("Собрано записей: %d", nrow(dataset)))
  update_status(sprintf("Регионов: %d", length(unique(dataset$регион))))
  
  # Лемматизация
  update_status("Загрузка модели UDPipe...")
  udpipe_model <- udpipe_load_model("preprocess/russian-gsd-ud-2.5-191206.udpipe")
  
  # Загрузка стоп-слов
  if(!is.null(stopwords_file) && file.exists(stopwords_file)) {
    stop_words <- readLines(stopwords_file, encoding = "UTF-8")
  } else {
    stop_words <- c("и", "в", "не", "что", "он", "на", "я", "с", "как", "а")
  }
  
  # Лемматизация текстов
  update_status("Лемматизация текстов...")
  total_texts <- nrow(dataset)
  
  dataset$лемма_текст <- sapply(seq_along(dataset$текст), function(i) {
    if(i %% 10 == 0) {
      update_status(sprintf("Лемматизация: %d/%d (%.0f%%)", 
                            i, total_texts, i/total_texts*100))
    }
    
    annotated <- udpipe_annotate(udpipe_model, x = dataset$текст[i])
    annotated <- as.data.frame(annotated)
    
    lemmas <- annotated %>% 
      filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
      pull(lemma) %>% 
      paste(collapse = " ")
    
    if(length(lemmas) == 0 || lemmas == "") return("")
    return(lemmas)
  })
  
  # Предобработка
  update_status("Предобработка текстов...")
  
  keep_ru <- function(text) {
    text <- gsub("[^а-яёА-ЯЁ\\s]", " ", text, perl = TRUE)
    text <- gsub("\\s+", " ", text)
    trimws(text)
  }
  
  dataset$обработанный_текст <- sapply(dataset$лемма_текст, function(text) {
    if(text != "" && !is.na(text)) {
      text <- tolower(text)
      text <- keep_ru(text)
      text <- removePunctuation(text)
      text <- removeNumbers(text)
      text <- removeWords(text, stop_words)
      text <- stripWhitespace(text)
      as.character(text)
    } else {
      ""
    }
  })
  
  # Удаление пустых
  dataset <- dataset[dataset$обработанный_текст != "", ]
  update_status(sprintf("Осталось записей после очистки: %d", nrow(dataset)))
  
  # Создание структуры по регионам
  regions_data <- dataset %>%
    group_by(регион) %>%
    summarise(
      тексты = list(обработанный_текст),
      исходные_тексты = list(текст),
      количество_документов = n(),
      файлы = list(unique(файл)),
      количество_файлов = n_distinct(файл),
      .groups = 'drop'
    )
  
  regions_info <- setNames(
    lapply(seq_len(nrow(regions_data)), function(i) {
      list(
        название = regions_data$регион[i],
        тексты = regions_data$тексты[[i]],
        исходные_тексты = regions_data$исходные_тексты[[i]],
        количество_документов = regions_data$количество_документов[i],
        файлы = regions_data$файлы[[i]],
        количество_файлов = regions_data$количество_файлов[i]
      )
    }),
    regions_data$регион
  )
  
  all_regions <- names(regions_info)
  
  # Сохранение
  saveRDS(regions_info, "regions_info.rds")
  saveRDS(dataset, "dataset_step1.rds")
  save(regions_info, all_regions, file = "texts.RData")
  
  update_status(sprintf("Этап 1 завершен. Регионов: %d, Документов: %d", 
                        length(all_regions), nrow(dataset)))
  
  return(list(
    dataset = dataset,
    regions_info = regions_info,
    all_regions = all_regions
  ))
}