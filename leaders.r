#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    Kpct  = round((SO / PA) * 100, 1),
    BBpct = round((BB / PA) * 100, 1),
    BA    = round(BA, 3),
    OBP   = round(OBP, 3),
    SLG   = round(SLG, 3)
  )

cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))
