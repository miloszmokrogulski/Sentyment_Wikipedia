# ==============================================================================
# SKRYPT PRZYGOTOWANIA DANYCH (ETL)
# ==============================================================================
# Cel: Pobranie artykułów z Wikipedii, wielojęzyczna analiza sentymentu przez
#      API Hugging Face i zapisanie gotowej ramki danych do pliku CSV.
# Autor: Miłosz Mokrogulski
# Czas wykonania: ok. 20-40 minut (zależnie od obciążenia API Hugging Face)
# ==============================================================================

# 1. Ładowanie bibliotek
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("pbapply")) install.packages("pbapply")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(jsonlite)
library(httr)
library(pbapply)
library(dplyr)
library(tidyr)

# 2. Konfiguracja API
# file.create(".Renviron") #odkomentować przy pierwszym użyciu
# file.edit(".Renviron") # w pliku należy wpisać: HF_TOKEN="token_huggingface"

HF_TOKEN <- Sys.getenv("HF_TOKEN") 

if (HF_TOKEN == "") {
  warning("UWAGA: Brak tokenu HF_TOKEN. Skrypt może nie działać poprawnie. Ustaw go za pomocą Sys.setenv(HF_TOKEN='twój_token').")
}

# Model XLM-RoBERTa (CardiffNLP) - wielojęzyczny, wspiera arabski/hebrajski
API_URL <- "https://router.huggingface.co/hf-inference/models/cardiffnlp/twitter-xlm-roberta-base-sentiment"

# ==============================================================================
# 3. FUNKCJE POMOCNICZE
# ==============================================================================

# Funkcja pobierająca artykuł (używa httr::GET dla bezpiecznego kodowania URL)
downloadArticle <- function(term, lang = "pl") {
  term_clean <- gsub("_", " ", term)
  base_url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
  
  params <- list(
    action = "query",
    prop = "extracts",
    explaintext = "true",
    titles = term_clean,
    format = "json",
    redirects = "1" # Podążanie za przekierowaniami (w przypadku złej nazwy artykułu)
  )
  
  tryCatch({
    resp <- GET(base_url, query = params)
    if (status_code(resp) != 200) return(NULL)
    
    content_text <- content(resp, "text", encoding = "UTF-8")
    req <- fromJSON(content_text)
    pages <- req[["query"]][["pages"]]
    page_id <- names(pages)[1]
    
    if (page_id == "-1") return(NULL)
    
    article <- pages[[page_id]][["extract"]]
    if (is.null(article) || nchar(trimws(article)) == 0) return(NULL)
    
    return(article)
  }, error = function(e) {
    message(paste("Błąd przy pobieraniu:", term, "-", e$message))
    return(NULL)
  })
}

# Hybrydowa funkcja podziału tekstu (Obsługa interpunkcji zachodniej i azjatyckiej)
chunkBySentence <- function(article, max_chars = 1000) {
  # 1. Azjatyckie znaki (。！？) -> tniemy zawsze
  # 2. Zachodnie znaki (.!?) -> tniemy gdy jest spacja
  # 3. Nowe linie (\n+) -> naturalny podział akapitów/nagłówków
  sentences <- strsplit(article, "(?<=[。！？])\\s*|(?<=[.!?])\\s+|\n+", perl = TRUE)[[1]]
  
  chunks <- c()
  current_chunk <- ""
  
  for (sentence in sentences) {
    sentence <- trimws(sentence)
    if (nchar(sentence) == 0) next
    
    # Jeśli zdanie jest dłuższe niż limit (np. cały akapit po chińsku),
    # zostaje pocięte na kawałki zamiast pomijać resztę.
    while (nchar(sentence) > max_chars) {
      if (nchar(current_chunk) > 0) {
        chunks <- c(chunks, current_chunk)
        current_chunk <- ""
      }
      sub_part <- substr(sentence, 1, max_chars)
      chunks <- c(chunks, sub_part)
      sentence <- substr(sentence, max_chars + 1, nchar(sentence))
    }
    
    if (nchar(current_chunk) + nchar(sentence) + 1 > max_chars) {
      chunks <- c(chunks, current_chunk)
      current_chunk <- sentence
    } else {
      sep_char <- if (nchar(current_chunk) == 0) "" else " "
      current_chunk <- paste(current_chunk, sentence, sep = sep_char)
    }
  }
  
  if (nchar(current_chunk) > 0) chunks <- c(chunks, current_chunk)
  return(chunks)
}

# Zapytanie do API Hugging Face
hf_query <- function(inputs, token = HF_TOKEN) {
  tryCatch({
    resp <- POST(
      API_URL,
      add_headers(Authorization = paste("Bearer", token), `Content-Type` = "application/json"),
      body = toJSON(list(inputs = inputs), auto_unbox = TRUE)
    )
    raw <- content(resp, as = "text", encoding = "UTF-8")
    if (status_code(resp) != 200) return(NULL)
    fromJSON(raw)
  }, error = function(e) return(NULL))
}

# Główna pętla analityczna
analyze_narrative <- function(target_list, max_chars_limit = 1000) {
  full_dataset <- data.frame()
  
  for (item in target_list) {
    article_name <- item[1]
    lang <- item[2]
    
    cat(sprintf("Przetwarzanie: %s [%s]...\n", article_name, lang))
    
    full_text <- downloadArticle(article_name, lang = lang)
    if (is.null(full_text)) {
      cat(" -> Pusty lub błąd. Pominięcie artykułu.\n")
      next
    }
    
    chunks <- chunkBySentence(full_text, max_chars_limit)
    total_chunks <- length(chunks)
    
    cat(sprintf(" -> Pobrano treść. Liczba fragmentów do analizy: %d\n", total_chunks))
    
    if (total_chunks == 0) next
    
    raw_results <- pblapply(chunks, hf_query)
    
    article_rows <- list()
    
    for (i in seq_along(raw_results)) {
      res <- raw_results[[i]]
      if (is.null(res) || length(res) == 0) next
      
      df_res <- res[[1]]
      
      # Wyciągamy prawdopodobieństwa (scores) dla każdej klasy
      score_pos <- df_res$score[grepl("positive|label_2", tolower(df_res$label))]
      score_neg <- df_res$score[grepl("negative|label_0", tolower(df_res$label))]
      
      if(length(score_pos) == 0) score_pos <- 0
      if(length(score_neg) == 0) score_neg <- 0
      
      # WYNIK KOMPOZYTOWY: Siła pozytywu minus siła negatywu
      sentiment_composite <- score_pos - score_neg
      
      top_prediction <- df_res[which.max(df_res$score), ]
      
      article_rows[[i]] <- data.frame(
        article_title = article_name,
        lang = lang,
        chunk_id = i,
        total_chunks = total_chunks,
        narrative_progress = i / total_chunks, 
        top_label = top_prediction$label,
        confidence = top_prediction$score,
        sentiment_value = sentiment_composite, # Wartość ciągła (-1 do 1)
        snippet = substr(chunks[i], 1, 50)
      )
    }
    
    if (length(article_rows) > 0) {
      article_df <- do.call(rbind, article_rows)
      full_dataset <- rbind(full_dataset, article_df)
    }
  }
  
  return(full_dataset)
}

# Funkcja przetwarzająca bloki geopolityczne
process_geopolitics <- function(df) {
  df <- df %>%
    mutate(
      geopolitical_bloc = case_when(
        lang == "he" ~ "Izrael",
        lang %in% c("ar", "arz") ~ "Państwa Arabskie",
        lang %in% c("fa", "tr", "az", "azb") ~ "Bliski Wschód (Inne)",
        lang %in% c("en", "fr", "de", "it", "es") ~ "Zachód",
        lang %in% c("ru", "pl", "cs", "zh", "uk") ~ "Blok Wschodni/Inne",
        TRUE ~ "Inne"
      ),
      weighted_sentiment = sentiment_value
    )
  
  # Ustalona kolejność bloków na wykresach
  geo_order <- c("Zachód", "Blok Wschodni/Inne", "Izrael", "Państwa Arabskie", "Bliski Wschód (Inne)", "Inne")
  df$geopolitical_bloc <- factor(df$geopolitical_bloc, levels = geo_order)
  
  df <- df %>% filter(!is.na(geopolitical_bloc))
  return(df)
}

# ==============================================================================
# 4. DEFINICJA DANYCH WEJŚCIOWYCH
# ==============================================================================

# Grupa A: Cały konflikt (1947-1949)
articles_group_full <- list(
  c("1948 Palestine war", "en"),
  c("I wojna izraelsko-arabska", "pl"),
  c("Арабо-израильская война (1947—1949)", "ru"),
  c("Palästinakrieg", "de"),
  c("Guerra in Palestina del 1947-1949", "it"),
  c("Guerre_israélo-arabe_de_1948", "fr"),
  c("第一次中东战争", "zh"),
  c("1948 Filistin Savaşı", "tr"),
  c("מלחמת_העצמאות", "he"),
  c("حرب_فلسطين_1947–1949", "ar"),
  c("جنگ_۱۹۴۸_فلسطین", "fa"),
  c("حرب_48", "arz"),
  c("عرب-ایسرائیل_ساواشی_(۱۹۴۸)", "azb")
)

# Grupa B: Faza regularna (od maja 1948)
articles_group_state <- list(
  c("1948 Arab–Israeli War", "en"),
  c("Guerra arabo-israeliana del 1948", "it"),
  c("Guerra árabe-israelí de 1948", "es"),
  c("Guerre_israélo-arabe_de_1948-1949", "fr"),
  c("1948年阿拉伯-以色列戰爭", "zh"),
  c("1948 Arap-İsrail Savaşı", "tr"),
  c("حرب_1948", "ar"),
  c("جنگ_۱۹۴۸_اعراب_و_اسرائیل", "fa"),
  c("حرب_العرب_و_اسرائيل_1948", "arz"),
  c("Ərəb–İsrail_müharibəsi_(1948)", "az")
)

# ==============================================================================
# 5. URUCHOMIENIE PROCESU
# ==============================================================================

cat("\n=== ROZPOCZYNANIE PRZYGOTOWANIA DANYCH ===\n")

# 1. Analiza grupy A
cat("\n--- Grupa A: Cały konflikt ---\n")
final_data_A <- analyze_narrative(articles_group_full)
if(nrow(final_data_A) > 0) final_data_A$study_scope <- "Cały konflikt (1947-1949)"

# 2. Analiza grupy B
cat("\n--- Grupa B: Faza regularna ---\n")
final_data_B <- analyze_narrative(articles_group_state)
if(nrow(final_data_B) > 0) final_data_B$study_scope <- "Faza regularna (od maja 1948)"

# 3. Łączenie i przetwarzanie
cat("\n--- Przetwarzanie końcowe ---\n")
final_data <- rbind(final_data_A, final_data_B)
final_data_processed <- process_geopolitics(final_data)

# 4. Zapis do pliku
output_filename <- "wyniki_sentymentu_1948_final.csv"
write.csv(final_data_processed, output_filename, row.names = FALSE)

cat(sprintf("\n[SUKCES] Dane zostały zapisane w pliku: %s\n", output_filename))
cat("Można teraz uruchomić aplikację 'app.R'.\n")
