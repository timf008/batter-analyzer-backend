#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(jsonlite)
library(stringr)
library(stringi)

args <- commandArgs(trailingOnly = TRUE)
player_name <- args[1]
season <- args[2]

# ============================================================
# Name Normalization (UTF-8 SAFE)
# Converts ALL formats → "FIRST LAST"
# ============================================================
normalize_name <- function(x) {
    x <- stri_trans_general(x, "Latin-ASCII")
    x <- gsub("[,*#†+]", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)

    if (grepl(",", x)) {
        parts <- unlist(strsplit(x, ","))
        last  <- trimws(parts[1])
        first <- trimws(parts[2])
        return(toupper(paste(first, last)))
    }

    parts <- unlist(strsplit(x, " "))
    if (length(parts) == 2) {
        first <- parts[1]
        last  <- parts[2]
        return(toupper(paste(first, last)))
    }

    return(toupper(x))
}

player_name_clean <- normalize_name(player_name)

# ============================================================
# Load CSV (ABSOLUTE PATH, BATTING)
# ============================================================
file_path <- file.path(getwd(), sprintf("stathead_batting_%s.csv", season))

if (!file.exists(file_path)) {
    cat(toJSON(list(error = paste("CSV not found:", file_path)), auto_unbox = TRUE))
    quit(status = 1)
}

df <- read_csv(file_path, show_col_types = FALSE)

# ============================================================
# Normalize column names
# ============================================================
names(df) <- names(df) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

# ============================================================
# Detect Player column
# ============================================================
name_col <- names(df)[str_detect(names(df), regex("^Player$", ignore_case = TRUE))][1]

if (is.na(name_col)) {
    cat(toJSON(list(error = "No Player column found"), auto_unbox = TRUE))
    quit(status = 1)
}

# ============================================================
# Normalize CSV names (UTF-8 SAFE)
# ============================================================
df$NameClean <- sapply(df[[name_col]], normalize_name)

# ============================================================
# Clean Season column
# ============================================================
df$Season <- as.numeric(gsub("[^0-9]", "", as.character(df$Season)))

# ============================================================
# Detect key batting columns safely
# ============================================================
get_col <- function(pattern) {
    cols <- names(df)[str_detect(names(df), pattern)]
    if (length(cols) == 0) return(NA_character_)
    cols[1]
}

pa_col  <- get_col("^PA$")
ab_col  <- get_col("^AB$")
h_col   <- get_col("^H$")
bb_col  <- get_col("^BB$")
so_col  <- get_col("^SO")
tb_col  <- get_col("^TB$")
hbp_col <- get_col("^HBP$")
sf_col  <- get_col("^SF$")
sh_col  <- get_col("^SH$")
ibb_col <- get_col("^IBB$")

hr_col  <- get_col("^HR$")
b1_col  <- get_col("^1B$")
b2_col  <- get_col("^2B$")
b3_col  <- get_col("^3B$")

team_col <- get_col("^Team$")   # ⭐ NEW

# ============================================================
# Fallback for singles if 1B missing
# ============================================================
if (is.na(b1_col) && !is.na(h_col) && !is.na(b2_col) && !is.na(b3_col) && !is.na(hr_col)) {
    df$Singles_calc <- df[[h_col]] - (df[[b2_col]] + df[[b3_col]] + df[[hr_col]])
    b1_col <- "Singles_calc"
}

# ============================================================
# Recalculate BA, OBP, SLG for ALL players
# ============================================================
df$BA_calc <- ifelse(!is.na(ab_col) & df[[ab_col]] > 0, df[[h_col]] / df[[ab_col]], NA_real_)

if (!is.na(ab_col) && !is.na(h_col) && !is.na(bb_col)) {
    HBP <- if (!is.na(hbp_col)) df[[hbp_col]] else 0
    SF  <- if (!is.na(sf_col))  df[[sf_col]]  else 0

    num <- df[[h_col]] + df[[bb_col]] + HBP
    den <- df[[ab_col]] + df[[bb_col]] + HBP + SF

    df$OBP_calc <- ifelse(den > 0, num / den, NA_real_)
} else {
    df$OBP_calc <- NA_real_
}

df$SLG_calc <- ifelse(!is.na(tb_col) & !is.na(ab_col) & df[[ab_col]] > 0,
                      df[[tb_col]] / df[[ab_col]],
                      NA_real_)

# ============================================================
# Compute K% and BB% (per PA)
# ============================================================
df$Kpct <- ifelse(!is.na(pa_col) & df[[pa_col]] > 0,
                  (df[[so_col]] / df[[pa_col]]) * 100,
                  NA_real_)

df$BBpct <- ifelse(!is.na(pa_col) & df[[pa_col]] > 0,
                   (df[[bb_col]] / df[[pa_col]]) * 100,
                   NA_real_)

# ============================================================
# Percentile helper
# ============================================================
percentile <- function(x, higher_is_better = TRUE) {
    valid <- !is.na(x)
    if (higher_is_better) {
        return(rank(x, na.last = "keep") / sum(valid) * 100)
    } else {
        return(rank(-x, na.last = "keep") / sum(valid) * 100)
    }
}

# ============================================================
# Backend Overall Score (same formula as frontend)
# ============================================================
score_ba <- function(ba) {
    pmin(pmax(10 * (ba - 0.240) / (0.300 - 0.240), 0), 10)
}

score_obp <- function(obp) {
    pmin(pmax(10 * (obp - 0.300) / (0.380 - 0.300), 0), 10)
}

score_slg <- function(slg) {
    pmin(pmax(10 * (slg - 0.380) / (0.550 - 0.380), 0), 10)
}

score_kpct <- function(kpct) {
    pmin(pmax(10 * (30 - kpct) / (30 - 15), 0), 10)
}

score_bbpct <- function(bbpct) {
    pmin(pmax(10 * (bbpct - 5) / (12 - 5), 0), 10)
}

compute_overall <- function(ba, obp, slg, kpct, bbpct) {
    score_ba(ba)   * 0.25 +
    score_obp(obp) * 0.25 +
    score_slg(slg) * 0.25 +
    score_kpct(kpct) * 0.15 +
    score_bbpct(bbpct) * 0.10
}

df$OverallScore <- compute_overall(df$BA_calc, df$OBP_calc, df$SLG_calc, df$Kpct, df$BBpct)

# ============================================================
# Component Scores
# ============================================================
df$BA_score <- score_ba(df$BA_calc)
df$OBP_score <- score_obp(df$OBP_calc)
df$SLG_score <- score_slg(df$SLG_calc)
df$Kpct_score <- score_kpct(df$Kpct)
df$BBpct_score <- score_bbpct(df$BBpct)

# ============================================================
# Cross-Sectional Expected Overall
#
# Uses ONLY the current season dataset.
# Each player's profile is compared with the 10 closest
# complete profiles using the five component scores.
#
# The player himself is excluded from his peer group.
# ============================================================

profile_cols <- c(
    "BA_score",
    "OBP_score",
    "SLG_score",
    "Kpct_score",
    "BBpct_score"
)

valid_profiles <- complete.cases(df[, profile_cols]) &
                  !is.na(df$OverallScore)

profile_indices <- which(valid_profiles)

# Standardize the current population once.
profile_matrix <- scale(df[valid_profiles, profile_cols])

# Store the scaling parameters so every player is transformed
# into the same standardized five-dimensional space.
profile_centers <- attr(profile_matrix, "scaled:center")
profile_scales  <- attr(profile_matrix, "scaled:scale")

calculate_expected_overall <- function(player_index, k = 10) {

    player_profile <- as.numeric(df[player_index, profile_cols])

    # Standardize this player's five component scores
    player_z <- (player_profile - profile_centers) / profile_scales

    # Euclidean distance to every valid player
    distances <- rowSums(
        (sweep(profile_matrix, 2, player_z, "-"))^2
    )^0.5

    # Exclude the player himself
    self_position <- which(profile_indices == player_index)

    if (length(self_position) > 0) {
        distances[self_position] <- Inf
    }

    # Select nearest neighbors
    finite_positions <- which(is.finite(distances))

    if (length(finite_positions) == 0) {
        return(NA_real_)
    }

    neighbor_positions <- order(distances[finite_positions])

    neighbor_positions <- finite_positions[
        neighbor_positions[1:min(k, length(neighbor_positions))]
    ]

    neighbor_indices <- profile_indices[neighbor_positions]

    mean(df$OverallScore[neighbor_indices], na.rm = TRUE)
}

df$ExpectedOverall <- NA_real_

for (i in profile_indices) {
    df$ExpectedOverall[i] <- calculate_expected_overall(i, k = 10)
}

# ============================================================
# Overall Divergence
#
# Positive = Actual Overall is above comparable-player expectation
# Negative = Actual Overall is below comparable-player expectation
# ============================================================

df$OverallDivergence <- df$OverallScore - df$ExpectedOverall

overall_divergence_sd <- sd(df$OverallDivergence, na.rm = TRUE)

Overall = as.numeric(OverallScore),
ExpectedOverall = as.numeric(ExpectedOverall),
OverallDivergence = as.numeric(OverallDivergence),
OverallDivergenceSD = as.numeric(overall_divergence_sd),

# ============================================================
# Compute Overall Percentile
# ============================================================
df$Overall_pct <- percentile(df$OverallScore, higher_is_better = TRUE)

# ============================================================
# XP Formula (NEW)
# ============================================================

compute_xp <- function(ba, obp, slg, bbpct, kpct) {
    xp <- (ba * 1000) +
          (obp * 1000) +
          (slg * 1000) +
          (bbpct * 2) -
          (kpct * 1.5)

    return(xp)
}

df$XP <- compute_xp(df$BA_calc, df$OBP_calc, df$SLG_calc, df$BBpct, df$Kpct)

# ============================================================
# League Averages (NEW)
# ============================================================
league_avg_overall <- mean(df$OverallScore, na.rm = TRUE)
league_avg_xp <- mean(df$XP, na.rm = TRUE)

# ============================================================
# Filter for player + season
# ============================================================

p <- df %>%
  filter(
    NameClean == player_name_clean,
    Season == as.numeric(season)
  )

if (nrow(p) == 0) {
    cat(toJSON(list(error = "Player not found"), auto_unbox = TRUE))
    quit(status = 1)
}

result <- p %>%
  transmute(
    BA   = as.numeric(BA_calc),
    OBP  = as.numeric(OBP_calc),
    SLG  = as.numeric(SLG_calc),
    Kpct = as.numeric(Kpct),
    BBpct = as.numeric(BBpct),

    BA_score   = as.numeric(BA_score),
    OBP_score  = as.numeric(OBP_score),
    SLG_score  = as.numeric(SLG_score),
    Kpct_score = as.numeric(Kpct_score),
    BBpct_score = as.numeric(BBpct_score),

    Overall = as.numeric(OverallScore),
    ExpectedOverall = as.numeric(ExpectedOverall),
    OverallDivergence = as.numeric(OverallDivergence),

    Overall_pct = as.numeric(Overall_pct),
    XP = as.numeric(XP),

    LeagueAvgOverall = league_avg_overall,
    LeagueAvgXP = league_avg_xp,

    Team = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_,

    PA   = if (!is.na(pa_col))  as.numeric(.data[[pa_col]])  else NA_real_,
    AB   = if (!is.na(ab_col))  as.numeric(.data[[ab_col]])  else NA_real_,
    H    = if (!is.na(h_col))   as.numeric(.data[[h_col]])   else NA_real_,
    BB   = if (!is.na(bb_col))  as.numeric(.data[[bb_col]])  else NA_real_,
    SO   = if (!is.na(so_col))  as.numeric(.data[[so_col]])  else NA_real_,
    TB   = if (!is.na(tb_col))  as.numeric(.data[[tb_col]])  else NA_real_,
    HR   = if (!is.na(hr_col))  as.numeric(.data[[hr_col]])  else NA_real_,
    `1B` = if (!is.na(b1_col))  as.numeric(.data[[b1_col]])  else NA_real_,
    `2B` = if (!is.na(b2_col))  as.numeric(.data[[b2_col]])  else NA_real_,
    `3B` = if (!is.na(b3_col))  as.numeric(.data[[b3_col]])  else NA_real_,
    HBP  = if (!is.na(hbp_col)) as.numeric(.data[[hbp_col]]) else NA_real_,
    SF   = if (!is.na(sf_col))  as.numeric(.data[[sf_col]])  else NA_real_,
    SH   = if (!is.na(sh_col))  as.numeric(.data[[sh_col]])  else NA_real_,
    IBB  = if (!is.na(ibb_col)) as.numeric(.data[[ibb_col]]) else NA_real_
  )

cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE))
