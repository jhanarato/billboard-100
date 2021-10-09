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

# Create the genres using the new code
new_genres <- function(songs_var) {
  songs_var %>%
    mutate(
      first_genre = str_extract(spotify_genre, "(?<=')[a-z ]+(?=')")
    )
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

# Debug!

lookup %>% filter(!is.na(test.1), is.na(first_genre))

# Some issues here:
#   test.1 has empty strings & NAs but first_genre has only NAs.
#   test.1 has genres where first_genre doesn't. eg/ "childrens music"

# Create variables to mark these issues.
lookup <- lookup %>% 
  mutate(
    na_vs_empty  = is.na(first_genre) & test.1 == "",
    na_vs_exists = is.na(first_genre) & !is.na(test.1) & test.1 != "",
    both_na = is.na(first_genre) & is.na(test.1)
  )

lookup %>% filter(na_vs_empty)
lookup %>% filter(na_vs_exists)
lookup %>% filter(both_na)
