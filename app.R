# --- APLIKACJA SHINY: ANALIZA NARRACJI 1948 ---
# Wymaga pliku: "wyniki_sentymentu_1948_final.csv" w katalogu roboczym

if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
if (!require("DT")) install.packages("DT")
if (!require("scales")) install.packages("scales")

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

# --- 1. WCZYTANIE I PRZYGOTOWANIE DANYCH ---

# Mapa kodów języków na pełne nazwy polskie
lang_map <- c(
  "en" = "Angielski",
  "pl" = "Polski",
  "ru" = "Rosyjski",
  "de" = "Niemiecki",
  "it" = "Włoski",
  "fr" = "Francuski",
  "zh" = "Chiński",
  "tr" = "Turecki",
  "he" = "Hebrajski",
  "ar" = "Arabski",
  "fa" = "Perski",
  "arz" = "Arabski (Egipt)",
  "az" = "Azerski",
  "azb" = "Azerski (Płd.)",
  "es" = "Hiszpański",
  "uk" = "Ukraiński",
  "cs" = "Czeski"
)

# Funkcja pomocnicza do odtwarzania kolejności bloków i tłumaczenia języków
prepare_data <- function(df) {
  geo_order <- c("Zachód", "Blok Wschodni/Inne", "Izrael", "Państwa Arabskie", "Bliski Wschód (Inne)", "Inne")
  
  df <- df %>%
    filter(!is.na(geopolitical_bloc)) %>%
    mutate(
      geopolitical_bloc = factor(geopolitical_bloc, levels = geo_order),
      # Tłumaczenie kodów języków
      lang_full = sapply(lang, function(x) ifelse(x %in% names(lang_map), lang_map[[x]], x)),
      # Formatowanie do tooltipów (zaokrąglenia)
      sentiment_rounded = round(weighted_sentiment, 3),
      progress_percent = round(narrative_progress * 100, 1)
    )
  return(df)
}

# Próba wczytania danych
tryCatch({
  df_raw <- read.csv("wyniki_sentymentu_1948_final.csv")
  df <- prepare_data(df_raw)
}, error = function(e) {
  stop("Nie znaleziono pliku 'wyniki_sentymentu_1948_final.csv'. Upewnij się, że jest w katalogu roboczym aplikacji.")
})

# Dostępne zakresy do filtra
available_scopes <- c("Wszystkie (Połączone)" = "all")
if("study_scope" %in% names(df)) {
  unique_scopes <- unique(df$study_scope)
  # Mapujemy nazwy z pliku na klucze
  for(s in unique_scopes) {
    available_scopes[s] <- s
  }
}

# Agregacja długości artykułów (bazowa, filtry nakładane w serwerze)
df_length_base <- df %>%
  group_by(lang_full, article_title, geopolitical_bloc, study_scope) %>%
  summarise(
    total_chunks = max(total_chunks),
    avg_sentiment = mean(weighted_sentiment, na.rm = TRUE),
    .groups = 'drop'
  )

# --- 2. DEFINICJA UI (INTERFEJS) ---

ui <- fluidPage(
  titlePanel("Analiza Narracji: Powstanie Izraela 1948"),
  
  tabsetPanel(
    
    # --- STRONA 1: ANALIZA GEOPOLITYCZNA (BLOKI) ---
    tabPanel("Analiza Bloków Politycznych",
             sidebarLayout(
               sidebarPanel(
                 h4("Filtry"),
                 # Wybór zakresu badania
                 radioButtons("scopeSelectionBloc", "Zakres artykułów:",
                              choices = available_scopes,
                              selected = "all"),
                 hr(),
                 checkboxGroupInput("blocsA", "Wybierz bloki:", 
                                    choices = levels(df$geopolitical_bloc),
                                    selected = levels(df$geopolitical_bloc)),
                 hr(),
                 p("Wybór opcji 'Faza regularna' ogranicza widok do narracji dotyczącej inwazji państw arabskich (V 1948). Brak Izraela w tej kategorii wynika z braku odrębnego artykułu na ten temat w hebrajskiej Wikipedii.")
               ),
               mainPanel(
                 h3("Trajektoria Emocjonalna (Średnia dla Bloku)"),
                 plotlyOutput("plotTrajectoryBloc", height = "400px"),
                 br(),
                 h3("Stabilność i Polaryzacja Narracji"),
                 plotlyOutput("plotBoxplotBloc", height = "300px")
               )
             )
    ),
    
    # --- STRONA 2: SZCZEGÓŁY (JĘZYKI) ---
    tabPanel("Analiza Szczegółowa (Państwa)",
             sidebarLayout(
               sidebarPanel(
                 h4("Wybór Języków"),
                 # Wybór zakresu badania (niezależny dla tej zakładki)
                 radioButtons("scopeSelectionLang", "Zakres artykułów:",
                              choices = available_scopes,
                              selected = "all"),
                 hr(),
                 selectInput("selectedBlocDetail", "Filtruj wg Bloku:", 
                             choices = c("Wszystkie", levels(df$geopolitical_bloc)),
                             selected = "Wszystkie"),
                 uiOutput("langSelector"),
                 hr(),
                 p("Panel umożliwia porównanie konkretnych narracji narodowych.")
               ),
               mainPanel(
                 h3("Porównanie Trajektorii Narracji"),
                 plotlyOutput("plotTrajectoryLang", height = "450px"),
                 br(),
                 h3("Długość i Szczegółowość Artykułów"),
                 p("Liczba fragmentów (chunks) świadczy o szczegółowości opisu."),
                 plotlyOutput("plotLength", height = "300px")
               )
             )
    ),
    
    # --- STRONA 3: DANE SUROWE ---
    tabPanel("Tabela Danych",
             DTOutput("dataTable")
    ),
    
    # --- STRONA 4: METODOLOGIA ---
    tabPanel("Metodologia",
             fluidPage(
               h3("Metodologia Badania"),
               p("Projekt ma na celu porównanie narracji historycznych dotyczących powstania państwa Izrael w 1948 roku w różnych wersjach językowych Wikipedii. Analiza opiera się na założeniu, że dobór słownictwa i struktura emocjonalna tekstu odzwierciedlają stanowiska geopolityczne poszczególnych kręgów kulturowych."),
               
               h4("1. Pozyskiwanie Danych"),
               tags$ul(
                 tags$li("Źródło: Wikipedia API."),
                 tags$li("Języki: Analizie poddano artykuły w językach reprezentujących główne strony konfliktu oraz mocarstwa (m.in. hebrajski, arabski, angielski, rosyjski, polski)."),
                 tags$li("Zakres: Badanie podzielono na dwie grupy artykułów: ogólne (cały konflikt 1947-1949) oraz szczegółowe (faza regularna od maja 1948).")
               ),
               
               h4("2. Przetwarzanie Języka Naturalnego (NLP)"),
               tags$ul(
                 tags$li("Model: Wykorzystano model `cardiffnlp/twitter-xlm-roberta-base-sentiment` (architektura XLM-RoBERTa)."),
                 tags$li("Wielojęzyczność: Model wspiera natywnie języki semickie (arabski), co pozwala na skuteczny transfer wiedzy (zero-shot transfer) również na język hebrajski."),
                 tags$li("Metryka: Zastosowano 'Bilans Sentymentu' obliczany jako różnica prawdopodobieństwa klasyfikacji pozytywnej i negatywnej (P_pos - P_neg). Pozwala to na uchwycenie niuansów nawet w tekstach o charakterze encyklopedycznym.")
               ),
               
               h4("3. Segmentacja Tekstu"),
               p("Teksty podzielono na fragmenty o długości ok. 1000 znaków, z uwzględnieniem specyfiki interpunkcji języków azjatyckich (chiński) oraz zachodnich."),
               
               h4("4. Ograniczenia"),
               p("Należy pamiętać, że model trenowany na danych z mediów społecznościowych (Twitter) może interpretować suchy język wojskowy jako neutralny, co wymagało zastosowania ważonych metryk prawdopodobieństwa.")
             )
    )
  )
)

# --- 3. SERVER (LOGIKA) ---

server <- function(input, output, session) {
  
  # --- LOGIKA ZAKŁADKI 1 (BLOKI) ---
  
  filtered_bloc_data <- reactive({
    data <- df %>% filter(geopolitical_bloc %in% input$blocsA)
    
    # Filtr zakresu
    if (input$scopeSelectionBloc != "all") {
      data <- data %>% filter(study_scope == input$scopeSelectionBloc)
    }
    
    data
  })
  
  output$plotTrajectoryBloc <- renderPlotly({
    req(nrow(filtered_bloc_data()) > 0)
    
    plot_data <- filtered_bloc_data() %>%
      rename(
        `Moment Artykułu` = narrative_progress,
        `Sentyment` = weighted_sentiment,
        `Blok Polityczny` = geopolitical_bloc
      )
    
    p <- ggplot(plot_data, aes(x = `Moment Artykułu`, y = `Sentyment`, color = `Blok Polityczny`)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_smooth(method = "loess", se = FALSE, size = 1.2, alpha = 0.8, span = 0.4) +
      
      annotate("text", x = 0.5, y = 1.0, label = "Pozytywny", size = 3, color = "gray40", fontface = "italic") +
      annotate("text", x = 0.5, y = -1.0, label = "Negatywny", size = 3, color = "gray40", fontface = "italic") +
      annotate("text", x = 0, y = -0.1, label = "Początek artykułu", hjust = 0, vjust = 0, size = 3, color = "gray40") +
      annotate("text", x = 1, y = -0.1, label = "Koniec artykułu", hjust = 1, vjust = 0, size = 3, color = "gray40") +
      
      scale_x_continuous(labels = scales::percent, name = "Postęp narracji") +
      scale_y_continuous(name = "Bilans Sentymentu (Pos - Neg)") +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("color", "x", "y")) %>% 
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3),
        margin = list(b = 100)
      )
  })
  
  output$plotBoxplotBloc <- renderPlotly({
    req(nrow(filtered_bloc_data()) > 0)
    
    plot_data <- filtered_bloc_data() %>%
      rename(
        `Blok Polityczny` = geopolitical_bloc,
        `Sentyment` = weighted_sentiment
      )
    
    p <- ggplot(plot_data, aes(x = `Blok Polityczny`, y = `Sentyment`, fill = `Blok Polityczny`)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_boxplot(alpha = 0.6, outlier.shape = NA) +
      theme_light() +
      labs(x = "", y = "Bilans Sentymentu") +
      theme(legend.position = "none") 
    
    ggplotly(p)
  })
  
  # --- LOGIKA ZAKŁADKI 2 (JĘZYKI) ---
  
  output$langSelector <- renderUI({
    data_source <- df
    if (input$scopeSelectionLang != "all") {
      data_source <- data_source %>% filter(study_scope == input$scopeSelectionLang)
    }

    if (input$selectedBlocDetail != "Wszystkie") {
      data_source <- data_source %>% filter(geopolitical_bloc == input$selectedBlocDetail)
    }
    
    avail_langs <- unique(data_source$lang_full)
    checkboxGroupInput("selectedLangs", "Wybierz Języki:", 
                       choices = avail_langs, 
                       selected = avail_langs)
  })
  
  filtered_lang_data <- reactive({
    req(input$selectedLangs)
    data <- df %>% filter(lang_full %in% input$selectedLangs)
    
    # Filtr zakresu
    if (input$scopeSelectionLang != "all") {
      data <- data %>% filter(study_scope == input$scopeSelectionLang)
    }
    data
  })
  
  filtered_length_data <- reactive({
    req(input$selectedLangs)
    data <- df_length_base %>% filter(lang_full %in% input$selectedLangs)
    
    # Filtr zakresu
    if (input$scopeSelectionLang != "all") {
      data <- data %>% filter(study_scope == input$scopeSelectionLang)
    }
    data
  })
  
  output$plotTrajectoryLang <- renderPlotly({
    req(nrow(filtered_lang_data()) > 0)
    
    plot_data <- filtered_lang_data() %>%
      rename(
        `Moment Artykułu` = narrative_progress,
        `Sentyment` = weighted_sentiment,
        `Język` = lang_full
      )
    
    p <- ggplot(plot_data, aes(x = `Moment Artykułu`, y = `Sentyment`, color = `Język`, group = `Język`)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.7, span = 0.4) +
      
      annotate("text", x = 0.5, y = 1.0, label = "Pozytywny", size = 3, color = "gray40", fontface = "italic") +
      annotate("text", x = 0.5, y = -1.0, label = "Negatywny", size = 3, color = "gray40", fontface = "italic") +
      annotate("text", x = 0, y = -0.1, label = "Początek artykułu", hjust = 0, vjust = 0, size = 3, color = "gray40") +
      annotate("text", x = 1, y = -0.1, label = "Koniec artykułu", hjust = 1, vjust = 0, size = 3, color = "gray40") +
      
      scale_x_continuous(labels = scales::percent, name = "Postęp narracji") +
      scale_y_continuous(name = "Bilans Sentymentu") +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal() +
      labs(title = "Szczegółowy przebieg narracji")
    
    ggplotly(p, tooltip = c("color", "x", "y"))
  })
  
  output$plotLength <- renderPlotly({
    req(nrow(filtered_length_data()) > 0)
    
    plot_data <- filtered_length_data()
    plot_data$lang_full <- reorder(plot_data$lang_full, -plot_data$total_chunks)
    
    plot_data <- plot_data %>%
      rename(
        `Język` = lang_full,
        `Liczba fragmentów 1000 znakowych` = total_chunks,
        `Blok Polityczny` = geopolitical_bloc,
        `Tytuł` = article_title
      )
    
    p <- ggplot(plot_data, aes(x = `Język`, y = `Liczba fragmentów 1000 znakowych`, fill = `Blok Polityczny`, 
                               text = paste("Tytuł:", `Tytuł`))) +
      geom_bar(stat = "identity") +
      theme_light() +
      labs(x = "Język", y = "Liczba fragmentów (1000 znaków)", fill = "Blok Polityczny") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y", "text", "fill"))
  })
  
  # --- LOGIKA ZAKŁADKI 3 (TABELA) ---
  output$dataTable <- renderDT({
    data_to_show <- df %>% 
      select(lang_full, article_title, study_scope, chunk_id, snippet, weighted_sentiment, geopolitical_bloc) %>%
      rename(
        `Język` = lang_full,
        `Tytuł Artykułu` = article_title,
        `Zakres (Scope)` = study_scope,
        `ID Fragmentu` = chunk_id,
        `Fragment Tekstu` = snippet,
        `Bilans Sentymentu` = weighted_sentiment,
        `Blok Polityczny` = geopolitical_bloc
      ) %>%
      mutate(`Bilans Sentymentu` = round(`Bilans Sentymentu`, 3))
    
    datatable(data_to_show,
              options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)