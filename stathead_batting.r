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
    # Remove accents reliably
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

# ============================================================
# Detect key batting columns safely
# ============================================================
get_col <- function(pattern) {
    cols <- names(p)[str_detect(names(p), pattern)]
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

# ============================================================
# Fallback for singles if 1B missing
# ============================================================
if (is.na(b1_col) && !is.na(h_col) && !is.na(b2_col) && !is.na(b3_col) && !is.na(hr_col)) {
    p$Singles_calc <- p[[h_col]] - (p[[b2_col]] + p[[b3_col]] + p[[hr_col]])
    b1_col <- "Singles_calc"
}

# ============================================================
# Recalculate BA, OBP, SLG
# ============================================================
p$BA_calc <- NA_real_
p$OBP_calc <- NA_real_
p$SLG_calc <- NA_real_

if (!is.na(ab_col) && !is.na(h_col)) {
    p$BA_calc <- ifelse(p[[ab_col]] > 0, p[[h_col]] / p[[ab_col]], NA_real_)
}

# OBP: (H + BB + HBP) / (AB + BB + HBP + SF)
if (!is.na(ab_col) && !is.na(h_col) && !is.na(bb_col)) {
    HBP <- if (!is.na(hbp_col)) p[[hbp_col]] else 0
    SF  <- if (!is.na(sf_col))  p[[sf_col]]  else 0

    num <- p[[h_col]] + p[[bb_col]] + HBP
    den <- p[[ab_col]] + p[[bb_col]] + HBP + SF

    p$OBP_calc <- ifelse(den > 0, num / den, NA_real_)
}

# SLG: TB / AB
if (!is.na(tb_col) && !is.na(ab_col)) {
    p$SLG_calc <- ifelse(p[[ab_col]] > 0, p[[tb_col]] / p[[ab_col]], NA_real_)
}

# ============================================================
# Compute K% and BB% (per PA)
# ============================================================
p$Kpct <- NA_real_
p$BBpct <- NA_real_

if (!is.na(pa_col) && !is.na(so_col)) {
    p$Kpct <- ifelse(p[[pa_col]] > 0, (p[[so_col]] / p[[pa_col]]) * 100, NA_real_)
}

if (!is.na(pa_col) && !is.na(bb_col)) {
    p$BBpct <- ifelse(p[[pa_col]] > 0, (p[[bb_col]] / p[[pa_col]]) * 100, NA_real_)
}

# ============================================================
# Build JSON output (core metrics + extra raw stats)
# ============================================================
result <- p %>%
  transmute(
    # Core analyzer metrics
    BA   = as.numeric(BA_calc),
    OBP  = as.numeric(OBP_calc),
    SLG  = as.numeric(SLG_calc),
    Kpct = as.numeric(Kpct),
    BBpct = as.numeric(BBpct),

    # Raw stats for debugging / future metrics
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
