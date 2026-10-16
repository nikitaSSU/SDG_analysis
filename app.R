library(shiny)
library(shinyFiles)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(tidyr)
set.seed(1234)
#файлы этапов
source("step1_collect_data.R")
source("step2_clustering.R")
source("step3_sentiment.R")

# Проверка наличия файлов и загрузка результатов Этапа 1
check_and_load_step1 <- function() {
  result <- list(
    loaded = FALSE,
    dataset = NULL,
    regions_info = NULL,
    all_regions = NULL,
    message = ""
  )
  
  # Проверяем наличие файла с данными
  if(file.exists("texts.RData")) {
    tryCatch({
      load("texts.RData")
      result$dataset <- if(exists("regions_info")) regions_info else NULL
      result$regions_info <- if(exists("regions_info")) regions_info else NULL
      result$all_regions <- if(exists("all_regions")) all_regions else NULL
      result$loaded <- TRUE
      result$message <- sprintf(
        "Загружены данные Этапа 1: %d регионов", 
        length(result$all_regions)
      )
    }, error = function(e) {
      result$message <- paste("Ошибка загрузки texts.RData:", e$message)
    })
    
  } else if(file.exists("regions_info.rds")) {
    tryCatch({
      result$regions_info <- readRDS("regions_info.rds")
      result$all_regions <- names(result$regions_info)
      result$loaded <- TRUE
      result$message <- sprintf(
        "Загружены данные Этапа 1 из regions_info.rds: %d регионов", 
        length(result$all_regions)
      )
    }, error = function(e) {
      result$message <- paste("Ошибка загрузки regions_info.rds:", e$message)
    })
    
  } else if(file.exists("dataset_step1.rds")) {
    tryCatch({
      result$dataset <- readRDS("dataset_step1.rds")
      result$loaded <- TRUE
      result$message <- sprintf(
        "Загружен датасет Этапа 1: %d записей", 
        nrow(result$dataset)
      )
    }, error = function(e) {
      result$message <- paste("Ошибка загрузки dataset_step1.rds:", e$message)
    })
  }
  
  return(result)
}

# Проверка наличия файлов и загрузка результатов Этапа 2
check_and_load_step2 <- function() {
  result <- list(
    loaded = FALSE,
    classification_results = NULL,
    message = ""
  )
  
  # Основной файл с результатами классификации
  if(file.exists("all_regions_classification.csv")) {
    tryCatch({
      result$classification_results <- read.csv(
        "all_regions_classification.csv", 
        stringsAsFactors = FALSE
      )
      result$loaded <- TRUE
      result$message <- sprintf(
        "Загружены результаты Этапа 2: %d строк, %d регионов, %d групп",
        nrow(result$classification_results),
        length(unique(result$classification_results$region)),
        length(unique(result$classification_results$assigned_group))
      )
    }, error = function(e) {
      result$message <- paste("Ошибка загрузки результатов Этапа 2:", e$message)
    })
  }
  
  return(result)
}

# Проверка наличия файлов и загрузка результатов Этапа 3
check_and_load_step3 <- function() {
  result <- list(
    loaded = FALSE,
    sentiment_results = NULL,
    sentiment_matrix = NULL,
    count_matrix = NULL,
    message = ""
  )
  
  # Файл с результатами тональности
  if(file.exists("region_goal_sentiment.csv")) {
    tryCatch({
      result$sentiment_results <- read.csv(
        "region_goal_sentiment.csv", 
        stringsAsFactors = FALSE
      )
      result$loaded <- TRUE
      result$message <- sprintf(
        "Загружены результаты Этапа 3: %d строк, %d регионов",
        nrow(result$sentiment_results),
        length(unique(result$sentiment_results$region))
      )
    }, error = function(e) {
      result$message <- paste("Ошибка загрузки результатов Этапа 3:", e$message)
    })
  }
  
  return(result)
}

# Проверка всех этапов и возврат сводки
check_all_results <- function() {
  step1 <- check_and_load_step1()
  step2 <- check_and_load_step2()
  step3 <- check_and_load_step3()
  
  messages <- c()
  if(step1$loaded) messages <- c(messages, "Этап 1: " %+% step1$message)
  if(step2$loaded) messages <- c(messages, "Этап 2: " %+% step2$message)
  if(step3$loaded) messages <- c(messages, "Этап 3: " %+% step3$message)
  
  if(length(messages) == 0) {
    messages <- "Нет сохраненных результатов. Выполните этапы обработки."
  }
  
  return(list(
    step1 = step1,
    step2 = step2,
    step3 = step3,
    summary = paste(messages, collapse = "\n")
  ))
}

# Оператор для конкатенации строк
`%+%` <- function(x, y) paste0(x, y)

# UI

ui <- fluidPage(
  theme = shinytheme("flatly"),
  # CSS стили
  tags$head(
    tags$style(HTML("
      .status-loaded { color: #28a745; font-weight: bold; }
      .status-not-loaded { color: #dc3545; }
      .status-running { color: #007bff; }
      .word-tag {
        display: inline-block;
        padding: 3px 8px;
        margin: 2px;
        background-color: #e3f2fd;
        border: 1px solid #90caf9;
        border-radius: 12px;
        font-size: 12px;
      }
      .stopword-item {
        display: inline-block;
        padding: 2px 6px;
        margin: 2px;
        background-color: #ffebee;
        border: 1px solid #ef9a9a;
        border-radius: 10px;
        font-size: 11px;
      }
      .group-header {
        background-color: #f5f5f5;
        padding: 8px;
        border-radius: 4px;
        margin-top: 10px;
        font-weight: bold;
      }
    "))
  ),
  
  titlePanel("Анализ новостных статей: классификация и тональность"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Выбор директории
      h4("Данные"),
      shinyDirButton("dir", "Выбрать директорию", "Выберите папку с новостями"),
      verbatimTextOutput("dir_path", placeholder = TRUE),
      br(),
      
      # Этапы обработки с индикаторами
      h4("Этапы обработки"),
      
      div(
        actionButton("step1", "1. Сбор данных", 
                     icon = icon("database"), 
                     class = "btn-primary btn-block"),
        uiOutput("step1_status_icon")
      ),
      br(),
      
      div(
        actionButton("step2", "2. Кластеризация и разделение по группам", 
                     icon = icon("project-diagram"), 
                     class = "btn-info btn-block"),
        uiOutput("step2_status_icon")
      ),
      br(),
      
      div(
        actionButton("step3", "3. Анализ тональности", 
                     icon = icon("chart-bar"), 
                     class = "btn-success btn-block"),
        uiOutput("step3_status_icon")
      ),
      br(),
      
      # Загрузка сохраненных данных (дополнительно)
      h4("Загрузить другие результаты"),
      fileInput("load_data", "Выберите файл данных",
                accept = c(".csv", ".rds", ".RData")),
      
      hr(),
      
      # Параметры
      h4("Параметры"),
      numericInput("minPts", "minPts для HDBSCAN:", 
                   value = 15, min = 2, max = 200),
      numericInput("max_keywords", "Макс. ключевых слов:", 
                   value = 10, min = 3, max = 50),
      
      hr(),
      
      # Информация
      h4("Текущий статус"),
      verbatimTextOutput("status", placeholder = TRUE)
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        
        # Вкладка с данными
        tabPanel("Данные",
                 h4("Результаты обработки"),
                 DT::dataTableOutput("data_table"),
                 br(),
                 downloadButton("download_data", "Скачать CSV")
        ),
        
        # Вкладка с классификацией по группам
        tabPanel("Классификация по группам",
                 h4("Распределение документов по группам"),
                 plotlyOutput("sdg_distribution"),
                 br(),
                 h4("Детали классификации"),
                 DT::dataTableOutput("sdg_table")
        ),
        
        # Вкладка с тепловой картой
        tabPanel("Тональность",
                 h4("Тепловая карта: Регионы × SDG цели"),
                 plotlyOutput("heatmap_plotly", height = "600px"),
                 br(),
                 h4("Статистика тональности"),
                 verbatimTextOutput("sentiment_stats")
        ),
        
        # Вкладка групп
        tabPanel("Группы",
                 fluidRow(
                   column(6,
                          h4("Список групп и ключевых слов"),
                          uiOutput("sdg_groups_display")
                   ),
                   column(6,
                          h4("Управление ключевыми словами"),
                          
                          wellPanel(
                            h5("Добавить ключевое слово"),
                            selectInput("add_word_group", "Выберите группу:", choices = NULL),
                            textInput("new_keyword", "Новое ключевое слово:",
                                      placeholder = "Введите слово..."),
                            actionButton("add_keyword_btn", "Добавить слово",
                                         class = "btn-success btn-sm")
                          ),
                          
                          wellPanel(
                            h5("Удалить ключевое слово"),
                            selectInput("remove_word_group", "Выберите группу:", choices = NULL),
                            selectInput("remove_keyword", "Выберите слово:", choices = NULL),
                            actionButton("remove_keyword_btn", "Удалить слово",
                                         class = "btn-danger btn-sm")
                          ),
                          
                          wellPanel(
                            h5("Управление группами"),
                            textInput("new_group_name", "Новая группа:", placeholder = "sdg17"),
                            textInput("new_group_keywords", "Ключевые слова (через запятую):"),
                            actionButton("add_group_btn", "Добавить группу", class = "btn-success btn-sm"),
                            br(), br(),
                            selectInput("remove_group", "Удалить группу:", choices = NULL),
                            actionButton("remove_group_btn", "Удалить группу", class = "btn-danger btn-sm")
                          ),
                          
                          wellPanel(
                            h5("Сохранение"),
                            actionButton("save_goals_file", "Сохранить в файл", class = "btn-primary btn-sm"),
                            br(), br(),
                            fileInput("load_goals_file", "Загрузить из файла:", accept = ".txt"),
                            actionButton("reset_goals", "Сбросить", class = "btn-warning btn-sm")
                          )
                   )
                 )
        ),
        
        # Вкладка стоп-слов
        tabPanel("Стоп-слова",
                 fluidRow(
                   column(6,
                          h4("Текущий список стоп-слов"),
                          verbatimTextOutput("stopwords_count"),
                          uiOutput("stopwords_display")
                   ),
                   column(6,
                          h4("Управление стоп-словами"),
                          
                          wellPanel(
                            h5("Добавить стоп-слова"),
                            textAreaInput("add_stopwords", "Слова (по одному на строку):", rows = 5),
                            actionButton("add_stopwords_btn", "Добавить", class = "btn-success btn-sm")
                          ),
                          
                          wellPanel(
                            h5("Удалить стоп-слова"),
                            selectInput("remove_stopwords", "Выберите слова:", 
                                        choices = NULL, multiple = TRUE),
                            actionButton("remove_stopwords_btn", "Удалить выбранные", 
                                         class = "btn-danger btn-sm"),
                            br(), br(),
                            actionButton("clear_stopwords_btn", "Очистить все", 
                                         class = "btn-warning btn-sm")
                          ),
                          
                          wellPanel(
                            h5("Сохранение"),
                            actionButton("save_stops_file", "Сохранить в файл", 
                                         class = "btn-primary btn-sm"),
                            br(), br(),
                            fileInput("load_stops_file", "Загрузить из файла:", accept = ".txt"),
                            actionButton("reset_stops", "Сбросить", class = "btn-warning btn-sm")
                          )
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  values <- reactiveValues(
    dataset = NULL,
    regions_info = NULL,
    all_regions = NULL,
    classification_results = NULL,
    sentiment_results = NULL,
    sentiment_matrix = NULL,
    count_matrix = NULL,
    base_dir = NULL,
    status_msg = "Проверка сохраненных результатов...",
    step1_done = FALSE,
    step2_done = FALSE,
    step3_done = FALSE
  )
  
  # АВТОМАТИЧЕСКАЯ ЗАГРУЗКА ПРИ ЗАПУСКЕ
  observe({
    step1 <- check_and_load_step1()
    if(step1$loaded) {
      values$dataset <- step1$dataset
      values$regions_info <- step1$regions_info
      values$all_regions <- step1$all_regions
      values$step1_done <- TRUE
      cat("Этап 1 загружен\n")
    }
    step2 <- check_and_load_step2()
    if(step2$loaded) {
      values$classification_results <- step2$classification_results
      values$step2_done <- TRUE
      cat("Этап 2 загружен\n")
    }
    step3 <- check_and_load_step3()
    if(step3$loaded) {
      values$sentiment_results <- step3$sentiment_results
      values$sentiment_matrix <- step3$sentiment_matrix
      values$count_matrix <- step3$count_matrix
      values$step3_done <- TRUE
      cat("Этап 3 загружен\n")
    }
    all_checks <- check_all_results()
    values$status_msg <- all_checks$summary
    cat("\nСтатус загрузки:\n", all_checks$summary, "\n")
  })
  
  output$step1_status_icon <- renderUI({
    if(values$step1_done) {
      tags$span(class = "status-loaded", "Загружен")
    } else {
      tags$span(class = "status-not-loaded", "Не выполнен")
    }
  })
  
  output$step2_status_icon <- renderUI({
    if(values$step2_done) {
      tags$span(class = "status-loaded", "Загружен")
    } else {
      tags$span(class = "status-not-loaded", "Не выполнен")
    }
  })
  
  output$step3_status_icon <- renderUI({
    if(values$step3_done) {
      tags$span(class = "status-loaded", "Загружен")
    } else {
      tags$span(class = "status-not-loaded", "Не выполнен")
    }
  })
  
  # SDG ГРУППЫ И СТОП-СЛОВА
  sdg_groups <- reactiveVal({
    list(
      sdg1=c(
        "бедность", "доход", "малоимущий", "неравенство", "благосостояние"
      ),
      sdg2=c(
        "сельскохозяйственный", "питание", "продовольственный", "безопасность", "голод", "агрономический"
      ),
      sdg3=c(
        "психический", "здоровье", "здравоохранение", "инвалидность", "болезнь", "инфекционный", "вакцина", "смертность", "неонатальный", "младенческий",  "репродуктивный", "эпидемия"
      ),
      sdg4=c(
        "педагогика", "обучение", "грамотность", "образование", "начальный", "средний", "высший"
      ),
      sdg5=c(
        "гендерный", "женщина", "девочка", "женский", "феминистский", "равенство", "брак"
      ),
      sdg6=c(
        "вода", "рекультивация",  "засушливый", "засуха", "сточный", "неочищенный", "опреснение", "истощение", "опустынивание", "гидрологический"
      ),
      sdg7=c(
        "энергия", "возобновляемый", "ветряной", "солнечный", "геотермальный", "гидроэлектрический", "выброс", "парниковый", "биотопливо"
      ),
      sdg8=c(
        "занятость", "устойчивый", "развитие", "труд", "рабочий", "зарплата", "предпринимательский", "малый", "средний", "предприятие", "туризм", "молодёжный", "работа"
      ),
      sdg9=c(
        "инфраструктура", "инвестиционный", "интернет", "глобализация", "индустриализация", "диверсификация"
      ),
      sdg10=c(
        "торговля", "финансовый", "рынок", "налогообложение", "равенство", "беженец", "неравенство"
      ),
      sdg11=c(
        "городской", "общественный", "транспорт", "метро", "жилищный", "зелёный", "инфраструктура", "малоэтажный", "застройка", "здание", "пешеходный", "пространство", "доступность", "обустройство"
      ),
      sdg12=c(
        "отходы", "природный", "ресурс", "переработка", "экология", "декарбонизация", "углеродный", "ископаемый", "топливо"
      ),
      sdg13=c(
        "климат", "парниковый", "потепление", "погода", "экологический", "антропогенный", "ископаемый", "топливо", "выброс", "диоксид", "углерод", "углеродно-нейтральный", "улавливание", "низкоуглеродный", "климат-позитивный"
      ),
      sdg14=c(
        "океан", "морской", "загрязнение", "рыба", "биоразнообразие", "экосистема", "перелов"
      ),
      sdg15=c(
        "лес", "биоразнообразие", "землепользование", "регенерация", "земля", "восстановление", "охрана", "почва", "эрозия", "экосистема", "обезлесение", "лесовосстановление", "природа", "флора", "фауна"
      ),
      sdg16=c(
        "управление", "мир", "социальный", "справедливость", "несправедливость", "уголовный", "правосудие", "право", "демократический", "избиратель", "правовой", "коррумпированный", "национализм", "демократия", "авторитарный", "судебный", "экоюстисный", "самоопределение", "насилие", "эксплуатация", "пытка", "верховенство", "закон", "незаконный", "преступность", "взятка", "терроризм"
      )
    )
  })
  
  # Инициализация стоп-слов с проверкой существования файла
  stopwords_list <- reactiveVal({
    if(file.exists("preprocess/stop-ru.txt")) {
      readLines("preprocess/stop-ru.txt", warn = FALSE)
    } else {
      # Базовый список стоп-слов если файл не найден
      c("и", "в", "не", "что", "он", "на", "я", "с", "как", "а", "но", "так", "все", 
        "или", "без", "бы", "до", "по", "при", "уже", "еще", "более", "менее", "очень",
        "такой", "этот", "тот", "свой", "который", "весь")
    }
  })
  
  # Обновление выпадающих списков
  observe({
    groups <- sdg_groups()
    updateSelectInput(session, "add_word_group", choices = names(groups))
    updateSelectInput(session, "remove_word_group", choices = names(groups))
    updateSelectInput(session, "remove_group", choices = names(groups))
  })
  
  observe({
    req(input$remove_word_group)
    groups <- sdg_groups()
    updateSelectInput(session, "remove_keyword", 
                      choices = groups[[input$remove_word_group]])
  })
  
  observe({
    updateSelectInput(session, "remove_stopwords", choices = stopwords_list())
  })
  
  # УПРАВЛЕНИЕ ГРУППАМИ
  output$sdg_groups_display <- renderUI({
    groups <- sdg_groups()
    group_elements <- lapply(names(groups), function(group_name) {
      keywords <- groups[[group_name]]
      word_tags <- lapply(keywords, function(kw) {
        tags$span(class = "word-tag", kw)
      })
      tags$div(
        tags$div(class = "group-header", 
                 sprintf("%s (%d слов)", group_name, length(keywords))),
        tags$div(style = "padding: 5px;", word_tags),
        tags$hr()
      )
    })
    do.call(tagList, group_elements)
  })
  
  observeEvent(input$add_keyword_btn, {
    req(input$add_word_group, input$new_keyword)
    new_word <- trimws(tolower(input$new_keyword))
    if(new_word == "") return()
    groups <- sdg_groups()
    if(!new_word %in% groups[[input$add_word_group]]) {
      groups[[input$add_word_group]] <- c(groups[[input$add_word_group]], new_word)
      sdg_groups(groups)
      updateTextInput(session, "new_keyword", value = "")
      showNotification(sprintf("'%s' добавлено в %s", new_word, input$add_word_group), 
                       type = "success")
    }
  })
  
  observeEvent(input$remove_keyword_btn, {
    req(input$remove_word_group, input$remove_keyword)
    groups <- sdg_groups()
    groups[[input$remove_word_group]] <- setdiff(
      groups[[input$remove_word_group]], input$remove_keyword
    )
    sdg_groups(groups)
    showNotification("Слово удалено", type = "success")
  })
  
  observeEvent(input$add_group_btn, {
    req(input$new_group_name)
    group_name <- trimws(input$new_group_name)
    if(group_name == "") return()
    groups <- sdg_groups()
    keywords <- if(input$new_group_keywords != "") {
      trimws(strsplit(input$new_group_keywords, ",\\s*")[[1]])
    } else character(0)
    groups[[group_name]] <- keywords
    sdg_groups(groups)
    updateTextInput(session, "new_group_name", value = "")
    updateTextInput(session, "new_group_keywords", value = "")
    showNotification(sprintf("Группа '%s' добавлена", group_name), type = "success")
  })
  
  observeEvent(input$remove_group_btn, {
    req(input$remove_group)
    groups <- sdg_groups()
    if(length(groups) <= 1) {
      showNotification("Нельзя удалить последнюю группу", type = "error")
      return()
    }
    groups[[input$remove_group]] <- NULL
    sdg_groups(groups)
    showNotification("Группа удалена", type = "success")
  })
  
  observeEvent(input$reset_goals, {
    sdg_groups(list(
      sdg1=c(
        "бедность", "доход", "малоимущий", "неравенство", "благосостояние"
      ),
      sdg2=c(
        "сельскохозяйственный", "питание", "продовольственный", "безопасность", "голод", "агрономический"
      ),
      sdg3=c(
        "психический", "здоровье", "здравоохранение", "инвалидность", "болезнь", "инфекционный", "вакцина", "смертность", "неонатальный", "младенческий",  "репродуктивный", "эпидемия"
      ),
      sdg4=c(
        "педагогика", "обучение", "грамотность", "образование", "начальный", "средний", "высший"
      ),
      sdg5=c(
        "гендерный", "женщина", "девочка", "женский", "феминистский", "равенство", "брак"
      ),
      sdg6=c(
        "вода", "рекультивация",  "засушливый", "засуха", "сточный", "неочищенный", "опреснение", "истощение", "опустынивание", "гидрологический"
      ),
      sdg7=c(
        "энергия", "возобновляемый", "ветряной", "солнечный", "геотермальный", "гидроэлектрический", "выброс", "парниковый", "биотопливо"
      ),
      sdg8=c(
        "занятость", "устойчивый", "развитие", "труд", "рабочий", "зарплата", "предпринимательский", "малый", "средний", "предприятие", "туризм", "молодёжный", "работа"
      ),
      sdg9=c(
        "инфраструктура", "инвестиционный", "интернет", "глобализация", "индустриализация", "диверсификация"
      ),
      sdg10=c(
        "торговля", "финансовый", "рынок", "налогообложение", "равенство", "беженец", "неравенство"
      ),
      sdg11=c(
        "городской", "общественный", "транспорт", "метро", "жилищный", "зелёный", "инфраструктура", "малоэтажный", "застройка", "здание", "пешеходный", "пространство", "доступность", "обустройство"
      ),
      sdg12=c(
        "отходы", "природный", "ресурс", "переработка", "экология", "декарбонизация", "углеродный", "ископаемый", "топливо"
      ),
      sdg13=c(
        "климат", "парниковый", "потепление", "погода", "экологический", "антропогенный", "ископаемый", "топливо", "выброс", "диоксид", "углерод", "углеродно-нейтральный", "улавливание", "низкоуглеродный", "климат-позитивный"
      ),
      sdg14=c(
        "океан", "морской", "загрязнение", "рыба", "биоразнообразие", "экосистема", "перелов"
      ),
      sdg15=c(
        "лес", "биоразнообразие", "землепользование", "регенерация", "земля", "восстановление", "охрана", "почва", "эрозия", "экосистема", "обезлесение", "лесовосстановление", "природа", "флора", "фауна"
      ),
      sdg16=c(
        "управление", "мир", "социальный", "справедливость", "несправедливость", "уголовный", "правосудие", "право", "демократический", "избиратель", "правовой", "коррумпированный", "национализм", "демократия", "авторитарный", "судебный", "экоюстисный", "самоопределение", "насилие", "эксплуатация", "пытка", "верховенство", "закон", "незаконный", "преступность", "взятка", "терроризм"
      )
    ))
    showNotification("Сброшено к стандартным", type = "success")
  })
  
  observeEvent(input$save_goals_file, {
    groups <- sdg_groups()
    lines <- sapply(names(groups), function(n) {
      sprintf("%s = %s", n, paste(groups[[n]], collapse = ", "))
    })
    fname <- sprintf("sdg_groups_%s.txt", format(Sys.Date(), "%Y%m%d"))
    writeLines(lines, fname)
    showNotification(sprintf("Сохранено в %s", fname), type = "success")
  })
  
  observeEvent(input$load_goals_file, {
    req(input$load_goals_file)
    lines <- readLines(input$load_goals_file$datapath)
    groups <- list()
    for(line in lines) {
      if(trimws(line) == "") next
      parts <- strsplit(line, "=")[[1]]
      if(length(parts) >= 2) {
        name <- trimws(parts[1])
        keywords <- trimws(strsplit(trimws(parts[2]), ",\\s*")[[1]])
        groups[[name]] <- keywords
      }
    }
    if(length(groups) > 0) {
      sdg_groups(groups)
      showNotification(sprintf("Загружено %d групп", length(groups)), type = "success")
    }
  })
  
  # УПРАВЛЕНИЕ СТОП-СЛОВАМИ
  
  output$stopwords_count <- renderText({
    sprintf("Всего стоп-слов: %d", length(stopwords_list()))
  })
  
  output$stopwords_display <- renderUI({
    stops <- stopwords_list()
    if(length(stops) > 0) {
      tags_div <- lapply(stops, function(w) {
        tags$span(class = "stopword-item", w)
      })
      tags$div(style = "padding: 10px; max-height: 400px; overflow-y: auto;", 
               tags_div)
    } else {
      tags$div(style = "padding: 10px; color: #999;", 
               "Список стоп-слов пуст")
    }
  })
  
  observeEvent(input$add_stopwords_btn, {
    text <- input$add_stopwords
    if(is.null(text) || nchar(text) == 0) return()
    words <- trimws(tolower(strsplit(text, "[\n,]+")[[1]]))
    words <- words[words != ""]
    current <- stopwords_list()
    new_words <- setdiff(words, current)
    if(length(new_words) > 0) {
      stopwords_list(c(current, new_words))
      updateTextAreaInput(session, "add_stopwords", value = "")
      showNotification(sprintf("Добавлено %d слов", length(new_words)), type = "success")
    }
  })
  
  observeEvent(input$remove_stopwords_btn, {
    req(input$remove_stopwords)
    current <- stopwords_list()
    stopwords_list(setdiff(current, input$remove_stopwords))
    showNotification(sprintf("Удалено %d слов", length(input$remove_stopwords)), 
                     type = "success")
  })
  
  observeEvent(input$clear_stopwords_btn, {
    stopwords_list(character(0))
    showNotification("Список очищен", type = "warning")
  })
  
  observeEvent(input$reset_stops, {
    if(file.exists("preprocess/stop-ru.txt")) {
      stopwords_list(readLines("preprocess/stop-ru.txt", warn = FALSE))
      showNotification("Сброшено", type = "success")
    } else {
      showNotification("Файл stop-ru.txt не найден", type = "error")
    }
  })
  
  observeEvent(input$save_stops_file, {
    stops <- stopwords_list()
    fname <- sprintf("stopwords_%s.txt", format(Sys.Date(), "%Y%m%d"))
    writeLines(stops, fname)
    showNotification(sprintf("Сохранено в %s", fname), type = "success")
  })
  
  observeEvent(input$load_stops_file, {
    req(input$load_stops_file)
    words <- readLines(input$load_stops_file$datapath, warn = FALSE)
    words <- trimws(tolower(words))
    words <- words[words != ""]
    stopwords_list(unique(c(stopwords_list(), words)))
    showNotification(sprintf("Загружено %d слов", length(words)), type = "success")
  })
  
  shinyDirChoose(input, "dir", roots = c(wd = "."))
  
  observeEvent(input$dir, {
    req(input$dir)
    values$base_dir <- parseDirPath(c(wd = "."), input$dir)
    output$dir_path <- renderText(values$base_dir)
  })
  
  observeEvent(input$step1, {
    req(values$base_dir)
    
    values$status_msg <- "Выполняется Этап 1: Сбор данных..."
    
    result <- run_step1(
      base_dir = values$base_dir,
      status_callback = function(msg) {
        values$status_msg <- msg
      }
    )
    
    if(!is.null(result)) {
      values$dataset <- result$dataset
      values$regions_info <- result$regions_info
      values$all_regions <- result$all_regions
      values$step1_done <- TRUE
      values$status_msg <- "Этап 1 завершен!"
    }
  })

  observeEvent(input$step2, {
    req(values$regions_info)
    
    values$status_msg <- "Выполняется Этап 2: Кластеризация..."
    
    result <- run_step2(
      regions_info = values$regions_info,
      sdg_groups = sdg_groups(),
      stopwords_clust_file = "preprocess/stop-ru.txt",
      minPts = input$minPts,
      max_keywords = input$max_keywords,
      status_callback = function(msg) {
        values$status_msg <- msg
      }
    )
    
    if(!is.null(result)) {
      values$classification_results <- result
      values$step2_done <- TRUE
      values$status_msg <- "Этап 2 завершен!"
    }
  })
  
  observeEvent(input$step3, {
    req(values$classification_results, values$regions_info)
    
    values$status_msg <- "Выполняется Этап 3: Анализ тональности..."
    
    result <- run_step3(
      regions_info = values$regions_info,
      classification_results = values$classification_results,
      sentiment_dict_file = "preprocess/rusentilex_2017.txt",
      status_callback = function(msg) {
        values$status_msg <- msg
      }
    )
    
    if(!is.null(result)) {
      values$sentiment_results <- result$final_dataset
      values$sentiment_matrix <- result$sentiment_matrix
      values$count_matrix <- result$count_matrix
      values$step3_done <- TRUE
      values$status_msg <- "Этап 3 завершен!"
    }
  })
  
  observeEvent(input$load_data, {
    req(input$load_data)
    file_path <- input$load_data$datapath
    file_ext <- tools::file_ext(input$load_data$name)
    
    tryCatch({
      if(file_ext == "csv") {
        data <- read.csv(file_path, stringsAsFactors = FALSE)
        if("assigned_group" %in% names(data)) {
          values$classification_results <- data
          values$step2_done <- TRUE
        } else if("avg_sentiment" %in% names(data)) {
          values$sentiment_results <- data
          values$step3_done <- TRUE
        }
      } else if(file_ext == "rds") {
        values$dataset <- readRDS(file_path)
        values$step1_done <- TRUE
      }
      values$status_msg <- paste("Загружен:", input$load_data$name)
    }, error = function(e) {
      values$status_msg <- paste("Ошибка:", e$message)
    })
  })
  
  output$data_table <- DT::renderDataTable({
    data <- values$sentiment_results
    if(is.null(data)) data <- values$classification_results
    if(is.null(data)) data <- values$dataset
    
    if(!is.null(data)) {
      DT::datatable(head(data, 100), options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  output$sdg_table <- DT::renderDataTable({
    req(values$classification_results)
    DT::datatable(values$classification_results, 
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ДИАГРАММА
  output$sdg_distribution <- renderPlotly({
    req(values$classification_results)
    
    # Агрегируем по assigned_group, суммируя n_documents
    df <- values$classification_results %>%
      group_by(assigned_group) %>%
      summarise(total_documents = sum(n_documents, na.rm = TRUE), .groups = 'drop')
    
    names(df) <- c("SDG", "Count")
    
    plot_ly(df, x = ~reorder(SDG, Count), y = ~Count, type = "bar",
            marker = list(color = ~Count, colorscale = "Viridis"),
            text = ~Count, textposition = 'auto') %>%
      layout(title = "Распределение документов по SDG целям",
             xaxis = list(title = "SDG цель"), 
             yaxis = list(title = "Количество документов"))
  })
  
  # ТЕПЛОВАЯ КАРТА
  output$heatmap_plotly <- renderPlotly({
    req(values$sentiment_results)
    
    # Получаем все возможные SDG цели из sdg_groups()
    all_sdg_goals <- names(sdg_groups())
    
    df <- values$sentiment_results
    
    all_regions <- unique(df$region)
    complete_grid <- expand.grid(
      region = all_regions,
      goal = all_sdg_goals,
      stringsAsFactors = FALSE
    )
    
    # Объединяем с существующими данными
    full_data <- complete_grid %>%
      left_join(df, by = c("region", "goal")) %>%
      mutate(sentiment_index = ifelse(is.na(sentiment_index), NA, sentiment_index))
    
    # Преобразуем в матрицу для тепловой карты
    mat <- full_data %>%
      select(region, goal, sentiment_index) %>%
      pivot_wider(names_from = goal, values_from = sentiment_index)
    
    regions <- mat$region
    mat_matrix <- as.matrix(mat[, -1])
    rownames(mat_matrix) <- regions
    
    # Создаем тепловую карту
    plot_ly(
      x = colnames(mat_matrix), 
      y = rownames(mat_matrix), 
      z = mat_matrix, 
      type = "heatmap",
      colorscale = list(
        list(0, "red"),
        list(0.5, "yellow"),
        list(1, "green")
      ),
      zmid = 0,
      hovertemplate = "Регион: %{y}<br>SDG: %{x}<br>Тональность: %{z:.3f}<extra></extra>"
    ) %>%
      layout(
        title = "Тональность: Регионы x SDG цели",
        xaxis = list(title = "SDG Цели", tickangle = 45),
        yaxis = list(title = "Регионы")
      )
  })
  
  output$sentiment_stats <- renderPrint({
    req(values$sentiment_results)
    df <- values$sentiment_results
    cat("Статистика тональности:\n")
    cat(sprintf("Строк: %d\n", nrow(df)))
    cat(sprintf("Регионов: %d\n", length(unique(df$region))))
    cat(sprintf("SDG целей: %d\n", length(unique(df$goal))))
    cat(sprintf("Средняя тональность: %.3f\n", mean(df$sentiment_index, na.rm = TRUE)))
    cat(sprintf("Мин. тональность: %.3f\n", min(df$sentiment_index, na.rm = TRUE)))
    cat(sprintf("Макс. тональность: %.3f\n", max(df$sentiment_index, na.rm = TRUE)))
  })
  
  # Статус
  output$status <- renderText({ values$status_msg })
  
  # Скачивание
  output$download_data <- downloadHandler(
    filename = function() { paste0("results_", Sys.Date(), ".csv") },
    content = function(file) {
      data <- values$sentiment_results
      if(is.null(data)) data <- values$classification_results
      if(!is.null(data)) write.csv(data, file, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)