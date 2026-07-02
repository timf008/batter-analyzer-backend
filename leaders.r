#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

csv_path <- sprintf("stathead_batting_%s.csv", season)

df <- read.csv(csv_path, stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    BA    = round(BA, 3),
    OBP   = round(OBP, 3),
    SLG   = round(SLG, 3),
    Kpct  = round(Kpct, 1),
    BBpct = round(BBpct, 1)
  )

cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))
