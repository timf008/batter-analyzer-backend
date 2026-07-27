#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

# -------------------------------
# DEBUG: Print CSV structure
# -------------------------------
message("DEBUG: Loaded CSV")
message("DEBUG: File path: ", file_path)
message("DEBUG: Column names: ", paste(names(df), collapse = ", "))
message("DEBUG: First 5 rows:")
print(head(df, 5))
flush.console()

# -------------------------------
# Clamp helper
# -------------------------------
clamp <- function(x, min_val = 0, max_val = 10) {
  pmax(min_val, pmin(max_val, x))
}

# -------------------------------
# Scoring functions
# -------------------------------
scoreBA <- function(ba) clamp(10 * (ba - 0.240) / (0.300 - 0.240))
scoreOBP <- function(obp) clamp(10 * (obp - 0.300) / (0.380 - 0.300))
scoreSLG <- function(slg) clamp(10 * (slg - 0.380) / (0.550 - 0.380))
scoreKpct <- function(kpct) clamp(10 * (30 - kpct) / (30 - 15))
scoreBBpct <- function(bbpct) clamp(10 * (bbpct - 5) / (12 - 5))

computeWeightedOverall <- function(baScore, obpScore, slgScore, kpctScore, bbpctScore) {
  baScore*0.25 + obpScore*0.25 + slgScore*0.25 + kpctScore*0.15 + bbpctScore*0.10
}

# -------------------------------
# MAIN PROCESSING
# -------------------------------

message("DEBUG: Converting numeric columns...")
df <- df %>%
  rename(HR2 = HR.1) %>%   # handle duplicate HR column safely
  mutate(
    PA  = suppressWarnings(as.numeric(PA)),
    AB  = suppressWarnings(as.numeric(AB)),
    BB  = suppressWarnings(as.numeric(BB)),
    SO  = suppressWarnings(as.numeric(SO)),
    BA  = suppressWarnings(as.numeric(BA)),
    OBP = suppressWarnings(as.numeric(OBP)),
    SLG = suppressWarnings(as.numeric(SLG))
  )
flush.console()

message("DEBUG: Structure after numeric conversion:")
print(str(df))
flush.console()

message("DEBUG: Filtering AB > 50 and PA > 0...")
df <- df %>% filter(!is.na(AB), !is.na(PA), AB > 50, PA > 0)
message("DEBUG: Rows after filter: ", nrow(df))
message("DEBUG: First 5 rows after filter:")
print(head(df, 5))
flush.console()

message("DEBUG: Running mutate scoring block...")
df <- df %>%
  mutate(
    Kpct  = round((SO / PA) * 100, 1),
    BBpct = round((BB / PA) * 100, 1),

    baScore    = scoreBA(BA),
    obpScore   = scoreOBP(OBP),
    slgScore   = scoreSLG(SLG),
    kpctScore  = scoreKpct(Kpct),
    bbpctScore = scoreBBpct(BBpct),

    overall = round(
      computeWeightedOverall(
        baScore, obpScore, slgScore, kpctScore, bbpctScore
      ),
      1
    ),

    XP = (BA * 1000) +
         (OBP * 1000) +
         (SLG * 1000) +
         (BBpct * 2) -
         (Kpct * 1.5)
  ) %>%
  arrange(desc(overall)) %>%
  slice(1:50)

message("DEBUG: Final DF rows: ", nrow(df))
message("DEBUG: Final DF preview:")
print(head(df, 10))
flush.console()

cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))
