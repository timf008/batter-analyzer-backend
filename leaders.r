#!/usr/bin/env Rscript

library(dplyr)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
season <- args[1]

file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))

df <- read.csv(file_path, stringsAsFactors = FALSE)

# -------------------------------
# Clamp helper
# -------------------------------
clamp <- function(x, min_val = 0, max_val = 10) {
  pmax(min_val, pmin(max_val, x))
}

# -------------------------------
# Scoring functions (Batting 5‑metric model)
# -------------------------------

scoreBA <- function(ba) {
  score <- 10 * (ba - 0.240) / (0.300 - 0.240)
  clamp(score)
}

scoreOBP <- function(obp) {
  score <- 10 * (obp - 0.300) / (0.380 - 0.300)
  clamp(score)
}

scoreSLG <- function(slg) {
  score <- 10 * (slg - 0.380) / (0.550 - 0.380)
  clamp(score)
}

scoreKpct <- function(kpct) {
  score <- 10 * (30 - kpct) / (30 - 15)
  clamp(score)
}

scoreBBpct <- function(bbpct) {
  score <- 10 * (bbpct - 5) / (12 - 5)
  clamp(score)
}

# -------------------------------
# Weighted Overall Score
# -------------------------------
computeWeightedOverall <- function(baScore, obpScore, slgScore, kpctScore, bbpctScore) {
  (baScore   * 0.25 +
   obpScore  * 0.25 +
   slgScore  * 0.25 +
   kpctScore * 0.15 +
   bbpctScore* 0.10)
}

# -------------------------------
# Main DF processing
# -------------------------------
df <- df %>%
  mutate(
    Kpct  = round((SO / PA) * 100, 1),
    BBpct = round((BB / PA) * 100, 1),
    BA    = round(BA, 3),
    OBP   = round(OBP, 3),
    SLG   = round(SLG, 3),

    # Normalized scores (0–10)
    baScore    = scoreBA(BA),
    obpScore   = scoreOBP(OBP),
    slgScore   = scoreSLG(SLG),
    kpctScore  = scoreKpct(Kpct),
    bbpctScore = scoreBBpct(BBpct),

    # Weighted overall score (rounded to .1)
    overall = round(
      computeWeightedOverall(
        baScore, obpScore, slgScore, kpctScore, bbpctScore
      ),
      1
    ),

    # XP (your sabermetric formula)
    XP = (BA * 1000) +
         (OBP * 1000) +
         (SLG * 1000) +
         (BBpct * 2) -
         (Kpct * 1.5),

    # -------------------------------
# Fantasy Identity (Batters)
# Mirrors JS xpTier + applySkillModifier
# -------------------------------

base_identity = case_when(
  XP >= 1200 ~ "breakout",
  XP >= 1100 ~ "overperformer",
  XP >= 1000 ~ "sleeper",
  XP >= 900  ~ "consistent",
  TRUE       ~ "neutral"
),

identity_index = case_when(
  base_identity == "neutral"       ~ 1,
  base_identity == "consistent"    ~ 2,
  base_identity == "sleeper"       ~ 3,
  base_identity == "overperformer" ~ 4,
  base_identity == "breakout"      ~ 5
),

identity_index = case_when(
  overall >= 8.0 ~ pmin(identity_index + 1, 5),
  overall <= 6.0 ~ pmax(identity_index - 1, 1),
  TRUE           ~ identity_index
),

identity = case_when(
  identity_index == 5 ~ "Breakout",
  identity_index == 4 ~ "Overperformer",
  identity_index == 3 ~ "Sleeper",
  identity_index == 2 ~ "Consistent",
  TRUE                ~ "Neutral"
),


    # -------------------------------
    # Draft Tiers (Pure Analyzer Style) DRAFT KIT
    # -------------------------------
    tier = case_when(
      overall >= 9.0 ~ "Tier 1",
      overall >= 8.0 ~ "Tier 2",
      overall >= 7.0 ~ "Tier 3",
      overall >= 6.0 ~ "Tier 4",
      TRUE ~ "Tier 5"
    )
  ) %>%
  filter(AB > 50) %>%
  arrange(desc(overall)) %>%
  slice(1:50)

cat(toJSON(df, pretty = FALSE, auto_unbox = TRUE))
