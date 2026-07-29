#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

# Clamp helper
clamp <- function(x, min_val = 0, max_val = 10) {
  pmax(min_val, pmin(max_val, x))
}

# Scoring functions
scoreBA <- function(ba) clamp(10 * (ba - 0.240) / (0.300 - 0.240))
scoreOBP <- function(obp) clamp(10 * (obp - 0.300) / (0.380 - 0.300))
scoreSLG <- function(slg) clamp(10 * (slg - 0.380) / (0.550 - 0.380))
scoreKpct <- function(kpct) clamp(10 * (30 - kpct) / (30 - 15))
scoreBBpct <- function(bbpct) clamp(10 * (bbpct - 5) / (12 - 5))

computeWeightedOverall <- function(baScore, obpScore, slgScore, kpctScore, bbpctScore) {
  (baScore * 0.25 +
   obpScore * 0.25 +
   slgScore * 0.25 +
   kpctScore * 0.15 +
   bbpctScore * 0.10)
}

# Main processing
df <- df %>%
  mutate(
    Kpct  = round((SO / PA) * 100, 1),
    BBpct = round((BB / PA) * 100, 1),
    BA    = round(BA, 3),
    OBP   = round(OBP, 3),
    SLG   = round(SLG, 3),

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
  )

# League averages (same AB > 50 rule)
league_avgs <- df %>%
  filter(AB > 50) %>%
  summarise(
    league_avg_XP      = mean(XP, na.rm = TRUE),
    league_avg_overall = mean(overall, na.rm = TRUE)
  )

cat(toJSON(league_avgs, pretty = FALSE, auto_unbox = TRUE))
