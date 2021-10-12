library(tidyverse)
library(lubridate)

billboard <- readr::read_csv("billboard.csv")
audio_features <- readr::read_csv("audio_features.csv")

song_data <- left_join(billboard, audio_features, 
                       by = c('song_id', 'song', 'performer')) 

fix_quotes <- function(data) {
  data %>%
    mutate(
      spotify_genre = str_replace(spotify_genre, 
                                  "\"children's music\"",
                                  "'childrens music'"),
      spotify_genre = str_replace(spotify_genre, 
                                  "\"australian children's music\"",
                                  "'australian childrens music'")
    )
}

extract_first_genre <- function(data) {
  data %>%
    mutate(
      first_genre = str_extract(spotify_genre, "(?<=')[-a-z0-9 &]+(?=')")
    )
}

top_genres_new <- function() {
  song_data <- song_data %>% 
    mutate(
      date = mdy(week_id),
      year = year(date)
    )
  
  song_data <- song_data %>%
    fix_quotes() %>%
    extract_first_genre()
  
  genre_count <- song_data %>% 
    count(year, first_genre) %>% 
    rename(total = "n")
  
  top_3 <- genre_count %>%                                      
    arrange(desc(total)) %>% 
    group_by(year) %>%
    slice(1:3) %>%
    filter(!is.na(first_genre))
  
  top_3 %>% 
    ungroup() %>% 
    select(first_genre) %>% 
    distinct() %>%
    arrange(first_genre)
}

top_genres_old <- function() {
  g_by_year2_rds <- read_rds("g_by_year2.rds")
  
  g_by_year2_rds %>% 
    select(test.1) %>% 
    distinct() %>%
    arrange(test.1)
}

top_genres_old()
top_genres_new()
