library(readr)
library(dplyr)
library(stringr)

# ---- Load Stathead batting file dynamically ----
file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))
stathead <- read_csv(file_path)

# ---- Load MLBID master file ----
mlbid <- read_csv("MLBID.csv")

# ---- Normalize names in both datasets ----
stathead_clean <- stathead %>%
  mutate(Player_clean = str_to_lower(Player))

mlbid_clean <- mlbid %>%
  mutate(Name_clean = str_to_lower(Name))

# ---- Join Stathead → MLBID by cleaned name ----
joined <- stathead_clean %>%
  left_join(mlbid_clean, by = c("Player_clean" = "Name_clean"))

# ---- Build name-variant lookup table ----
lookup <- joined %>%
  select(Player, MLBAMID) %>%
  filter(!is.na(MLBAMID)) %>%
  rowwise() %>%
  mutate(
    name_lower  = str_to_lower(Player),
    name_upper  = str_to_upper(Player),
    name_title  = str_to_title(Player)
  ) %>%
  ungroup()

# ---- Convert to named list for your app ----
mlb_lookup <- list()

for (i in 1:nrow(lookup)) {
  id <- lookup$MLBAMID[i]
  mlb_lookup[[ lookup$name_title[i] ]] <- id
  mlb_lookup[[ lookup$name_upper[i] ]] <- id
  mlb_lookup[[ lookup$name_lower[i] ]] <- id
}

# ---- Example: print Bryce Harper ----
mlb_lookup[["Bryce Harper"]]
mlb_lookup[["BRYCE HARPER"]]
mlb_lookup[["bryce harper"]]

