#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)
library(stringi)   # <-- add this

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

# ---------------------------------------------------------
# PATCH: Normalize player names (accents → ASCII, uppercase)
# ---------------------------------------------------------
df <- df %>%
  mutate(
    Name = stri_trans_general(Name, "Latin-ASCII"),
    Name = toupper(Name)
  )

df <- df %>%
  mutate(
    Kpct  = round((SO / PA) * 100, 1),
    BBpct = round((BB / PA) * 100, 1),
    BA    = round(BA, 3),
    OBP   = round(OBP, 3),
    SLG   = round(SLG, 3)
  )

cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))
