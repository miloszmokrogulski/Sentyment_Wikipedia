# Analiza Narracji Historycznej: Powstanie Izraela 1948 w Wikipedii
Celem projektu jest porównanie sentymentu i struktury narracyjnej artykułów Wikipedii dotyczących wojny izraelsko-arabskiej z 1948 roku w kilkunastu wersjach językowych, reprezentujących różne bloki geopolityczne.

## O projekcie

Wikipedia dąży do "neutralnego punktu widzenia" (NPOV), jednak dobór słownictwa, długość artykułów i sposób opisywania wydarzeń historycznych różnią się w zależności od kręgu kulturowego. Projekt wykorzystuje techniki przetwarzania języka naturalnego (NLP), aby zbadać te różnice.

Badanie obejmuje języki stron bezpośrednio zaangażowanych (hebrajski, arabski), mocarstw (angielski, rosyjski), państw regionu (turecki, perski) oraz innych (polski, chiński).

## Funkcjonalności

ETL Proces: Automatyczne pobieranie artykułów z API Wikipedii (z obsługą przekierowań i specyficznych znaków diakrytycznych) oraz podział tekstu na fragmenty.

Analiza Sentymentu: Wykorzystanie modelu LLM (XLM-RoBERTa) do oceny emocjonalnego nacechowania tekstu.

Interaktywny Dashboard: Aplikacja w R Shiny pozwalająca na:

Porównanie trajektorii emocjonalnych całych bloków politycznych (Zachód, Blok Wschodni, Izrael, Państwa Arabskie).

Szczegółową analizę poszczególnych języków.

Badanie długości i szczegółowości artykułów.

## Metodologia

1. Pozyskiwanie Danych

Dane pobierane są dynamicznie z API Wikipedii. Skrypt obsługuje specyficzne problemy kodowania znaków (URL encoding dla arabskiego/chińskiego) oraz przekierowania (redirects).

2. Segmentacja Tekstu (Chunking)

Zastosowano hybrydowy algorytm podziału tekstu, który:

Dla języków zachodnich dzieli tekst po znakach interpunkcyjnych następujących po spacji (chroni to daty np. 15.05.1948).

Dla języków azjatyckich (chiński) dzieli tekst po znakach 。！？ bez wymogu spacji.

Dla list i nagłówków uwzględnia podział nowej linii.

3. Model NLP

Wykorzystano model cardiffnlp/twitter-xlm-roberta-base-sentiment.

Jest to model wielojęzyczny, trenowany m.in. na języku arabskim, co pozwala na skuteczny zero-shot transfer na język hebrajski (rodzina semicka).

Metryka: Zamiast prostej klasyfikacji (Positive/Negative), zastosowano Bilans Sentymentu ($P_{pos} - P_{neg}$). Pozwoliło to na wykrycie subtelnych odchyleń w wysoce sformalizowanym, "encyklopedycznym" języku hebrajskim, który standardowo byłby klasyfikowany jako w 100% neutralny.

## Struktura Projektu

przygotowanie_danych.R - Skrypt ETL. Pobiera dane, komunikuje się z API Hugging Face i zapisuje wynik do CSV.

app.R - Aplikacja wizualizacyjna (R Shiny). Wczytuje gotowe dane i generuje wykresy.

wyniki_sentymentu_1948_final.csv - Przetworzony zbiór danych (wynik działania skryptu ETL).

## Instrukcja Uruchomienia

Wymagania

R oraz RStudio

Konto na Hugging Face (dla tokenu API)

**Instalacja bibliotek**

Uruchom w konsoli R:

install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "DT", "scales", "httr", "jsonlite", "pbapply", "tidyr"))


**Konfiguracja**

Aby uruchomić proces pobierania danych (przygotowanie_danych.R), musisz ustawić zmienną środowiskową z tokenem Hugging Face:

Sys.setenv(HF_TOKEN = "twój_token_tutaj")


**Uwaga: Proces ETL może trwać ok. 20-40 minut ze względu na limity darmowego API.**

## Uruchomienie Aplikacji

Projekt zawiera gotowy plik csv, krok ETL został zachowany jedynie w celu replikowalności metody. Wizualizacja danych dostępna jest, otwierając plik app.R i klikając Run App.
