# Create a lookup table for old and new genre columns
library(tidyverse)

# From the TidyTuesday repository
billboard <- readr::read_csv("billboard.csv")
audio_features <- readr::read_csv("audio_features.csv")

# A checkpoint created in the original code
songs_rds  <- read_rds("songs.rds")

# Create the genres using the original code
old_genres <- function(songs_var) {
  genres <- transform(songs_var, 
                      test=do.call(rbind, strsplit(spotify_genre, ',', fixed=TRUE)), 
                      stringsAsFactors=F)
  
  genres <- genres %>% 
    select(year, test.1)
  
  genres$test.1 <- gsub("","",as.character(genres$test.1))
  genres$test.1 <- gsub("'","",as.character(genres$test.1))
  genres$test.1 <- gsub('"',"",as.character(genres$test.1))
  
  
  genres$test.1 <- substr(genres$test.1, 2, nchar(genres$test.1)-1)
  
  return(genres)
}

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

# Create the genres using the new code
new_genres <- function(data) {
  data %>%
    fix_quotes() %>%
    extract_first_genre()
}

# Combine old and new genres to compare
make_genre_lookup <- function() {
  # Use checkpoint data to get genres
  old_genres(songs_rds)
  new_genres(songs_rds)
  
  tibble(first_genre = new_genres(songs_rds)$first_genre,
                       test.1 =old_genres(songs_rds)$test.1)
}

lookup <- make_genre_lookup()

lookup %>% 
  filter(is.na(first_genre) & test.1 == "")
