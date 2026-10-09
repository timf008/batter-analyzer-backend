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

cat(
    "RAW NAME TEST:",
    as.character(df[[2]][grepl("Gim", df[[2]])][1]),
    "\n",
    file = stderr()
)

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
# MLBAM player IDs for headshots (optional enrichment)
# Match using Stathead's Baseball-Reference identifier rather
# than names, which can be duplicated or formatted differently.
# ============================================================
mlbid_file <- file.path(getwd(), "stathead_mlbid_reconciled_updated.csv")
df$mlbId <- NA_integer_

if (file.exists(mlbid_file) && "Player-additional" %in% names(df)) {
    mlbid_map <- read_csv(mlbid_file, show_col_types = FALSE)

    if (all(c("BBRefID", "MLBAMID") %in% names(mlbid_map))) {
        bbref_key <- tolower(trimws(as.character(mlbid_map$BBRefID)))
        mlbam_values <- suppressWarnings(as.integer(mlbid_map$MLBAMID))

        # Never guess if a BBRef identifier maps to conflicting MLB IDs.
        valid <- !is.na(bbref_key) & nzchar(bbref_key) &
                 !is.na(mlbam_values)
        lookup <- split(mlbam_values[valid], bbref_key[valid])
        safe_lookup <- vapply(lookup, function(ids) {
            ids <- unique(ids)
            if (length(ids) == 1) ids[[1]] else NA_integer_
        }, integer(1))

        stathead_key <- tolower(trimws(as.character(df[["Player-additional"]])))
        df$mlbId <- unname(safe_lookup[match(stathead_key, names(safe_lookup))])
    } else {
        cat("MLBID WARNING: mapping file lacks BBRefID or MLBAMID\n", file = stderr())
    }
} else {
    cat("MLBID WARNING: mapping file or Player-additional column unavailable\n", file = stderr())
}

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
r_col   <- get_col("^R$")
rbi_col <- get_col("^RBI$")
bb_col  <- get_col("^BB$")
so_col  <- get_col("^SO")
tb_col  <- get_col("^TB$")
hbp_col <- get_col("^HBP$")
sf_col  <- get_col("^SF$")
sh_col  <- get_col("^SH$")
ibb_col <- get_col("^IBB$")

hr_col  <- get_col("^HR")
b1_col  <- get_col("^1B$")
b2_col  <- get_col("^2B$")
b3_col  <- get_col("^3B$")

team_col <- get_col("^Team$")

decode_stathead_name <- function(x) {

    pattern <- "<U\\+([0-9A-Fa-f]{4,6})>"

    while (grepl(pattern, x)) {

        tag <- regmatches(x, regexpr(pattern, x))

        hex <- sub(
            "<U\\+([0-9A-Fa-f]{4,6})>",
            "\\1",
            tag
        )

        replacement <- intToUtf8(
            strtoi(hex, base = 16)
        )

        x <- sub(
            pattern,
            replacement,
            x
        )
    }

    x
}

format_browser_name <- function(x) {

    name <- str_to_title(x)

    # Known two-letter initial-style first names
    initials <- c(
        "Aj", "Bj", "Cj", "Dj", "Jj",
        "Jt", "Tj"
    )

    parts <- str_split(name, " ", simplify = TRUE)

    if (parts[1] %in% initials) {
        parts[1] <- toupper(parts[1])
    }

    paste(parts[parts != ""], collapse = " ")
}

# ============================================================
# Player Browser Mode
# ============================================================

if (player_name == "__LIST__") {

    players <- df %>%
        transmute(
            Player = sapply(
    NameClean,
    format_browser_name,
    USE.NAMES = FALSE
),
            Team = if (!is.na(team_col))
                as.character(.data[[team_col]])
            else
                NA_character_
        ) %>%
        filter(!is.na(Player), Player != "") %>%
        arrange(Player)

    cat(
        toJSON(
            players,
            pretty = TRUE,
            auto_unbox = TRUE
        )
    )

    quit(status = 0)
}


# ============================================================
# Load Park Factors
# ============================================================
park_file <- file.path(getwd(), "mlb_park_factors_2024_2026.csv")

if (!file.exists(park_file)) {
    cat(toJSON(list(error = paste("Park factor CSV not found:", park_file)), auto_unbox = TRUE))
    quit(status = 1)
}

park_factors <- read_csv(park_file, show_col_types = FALSE)

names(park_factors) <- names(park_factors) |>
  str_replace_all("%", "pct") |>
  str_replace_all("/", "_") |>
  str_replace_all("\\.", "") |>
  str_replace_all(" ", "_")

required_park_cols <- c("TeamCode", "Team", "Venue", "Park_Factor", "H", "1B", "2B", "3B", "HR", "BB")

missing_park_cols <- setdiff(required_park_cols, names(park_factors))

if (length(missing_park_cols) > 0) {
    cat(toJSON(
        list(error = paste("Missing park factor columns:", paste(missing_park_cols, collapse = ", "))),
        auto_unbox = TRUE
    ))
    quit(status = 1)
}

park_lookup <- park_factors %>%
  transmute(
    ParkTeamCode = as.character(TeamCode),
    ParkTeam = as.character(Team),
    ParkVenue = as.character(Venue),
    ParkFactor = as.numeric(Park_Factor),
    PF_H = as.numeric(H),
    PF_1B = as.numeric(`1B`),
    PF_2B = as.numeric(`2B`),
    PF_3B = as.numeric(`3B`),
    PF_HR = as.numeric(HR),
    PF_BB = as.numeric(BB)
  )


# ============================================================
# Fallback for singles if 1B missing
# ============================================================
if (is.na(b1_col) && !is.na(h_col) && !is.na(b2_col) && !is.na(b3_col) && !is.na(hr_col)) {
    df$Singles_calc <- df[[h_col]] - (df[[b2_col]] + df[[b3_col]] + df[[hr_col]])
    b1_col <- "Singles_calc"
}


# ============================================================
# Match each player to his current / most recent home park
#
# Single-team codes:
#   TBR -> TBR
#
# Multi-team Stathead codes:
#   MIATBR -> TBR
#   PHISFG -> SFG
#
# Original team value remains unchanged for display purposes.
# ============================================================

if (!is.na(team_col)) {

    park_codes <- park_lookup$ParkTeamCode

    get_park_team <- function(team) {

        code <- toupper(trimws(as.character(team)))

        # Single-team player
        if (code %in% park_codes) {
            return(code)
        }

        # Multi-team player:
        # use the team code found at the END of the Stathead value
        matches <- park_codes[
            vapply(
                park_codes,
                function(x) endsWith(code, x),
                logical(1)
            )
        ]

        if (length(matches) > 0) {
            return(matches[which.max(nchar(matches))])
        }

        NA_character_
    }

    df$ParkLookupTeam <- vapply(
        df[[team_col]],
        get_park_team,
        character(1)
    )

    df <- df %>%
        left_join(
            park_lookup,
            by = c("ParkLookupTeam" = "ParkTeamCode")
        )

} else {

    df$ParkTeam <- NA_character_
    df$ParkVenue <- NA_character_
    df$ParkFactor <- NA_real_
    df$PF_H <- NA_real_
    df$PF_1B <- NA_real_
    df$PF_2B <- NA_real_
    df$PF_3B <- NA_real_
    df$PF_HR <- NA_real_
    df$PF_BB <- NA_real_
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
# Hitter Archetype / Profile Shape
#
# Archetype is based on the SHAPE of the five component scores,
# not the player's overall skill level.
#
# Step 1: Center each player's five scores around his own mean.
# Step 2: Measure Euclidean distance from that centered profile
#         to each multi-season archetype landmark.
# Step 3: Assign the nearest landmark as the primary archetype.
# Step 4: Compare nearest vs second-nearest distance to determine
#         archetype match strength.
#
# Landmarks derived from combined 2024-2026 qualified hitters.
# ============================================================

archetype_landmarks <- rbind(

    "Contact First" = c(
        BA  =  0.50,
        OBP = -1.22,
        SLG = -2.27,
        K   =  4.81,
        BB  = -1.82
    ),

    "Patient / Low Impact" = c(
        BA  = -2.56,
        OBP = -0.88,
        SLG = -2.79,
        K   =  3.38,
        BB  =  2.84
    ),

    "Power / Patience" = c(
        BA  = -2.18,
        OBP =  0.17,
        SLG = -0.66,
        K   = -1.53,
        BB  =  4.20
    ),

    "Balanced Profile" = c(
        BA  =  0.07,
        OBP = -0.62,
        SLG =  0.73,
        K   =  0.26,
        BB  = -0.43
    )
)


classify_archetype <- function(
    ba_score,
    obp_score,
    slg_score,
    k_score,
    bb_score
) {

    scores <- c(
        BA  = ba_score,
        OBP = obp_score,
        SLG = slg_score,
        K   = k_score,
        BB  = bb_score
    )

    # Cannot classify an incomplete profile
    if (any(!is.finite(scores))) {
        return(c(
            Archetype = NA_character_,
            ArchetypeMatch = NA_character_,
            ArchetypeStrength = NA_character_
        ))
    }

    # Remove general player strength.
    # What remains is the relative five-metric shape.
    profile_mean <- mean(scores)

    shape <- scores - profile_mean

    # Euclidean distance from this player's shape
    # to each archetype landmark.
    distances <- apply(
        archetype_landmarks,
        1,
        function(landmark) {
            sqrt(sum((shape - landmark)^2))
        }
    )

# Rank archetypes from closest to farthest
ranked <- sort(distances)

nearest_distance <- unname(ranked[1])
second_distance  <- unname(ranked[2])

archetype <- names(ranked)[1]

# Measures how decisively the nearest archetype
# beats the second-nearest archetype.
if (
    !is.finite(second_distance) ||
    second_distance <= 0
) {
    match_strength <- NA_real_
} else {
    match_strength <-
        1 - (nearest_distance / second_distance)
}

# Empirical match-strength bands
match_label <- case_when(
    is.na(match_strength)   ~ NA_character_,
    match_strength >= 0.50 ~ "Strong Match",
    match_strength >= 0.30 ~ "Moderate Match",
    TRUE                   ~ "Weak Match"
)

return(c(
    Archetype = unname(archetype),
    ArchetypeMatch = unname(match_label),
    ArchetypeStrength = ifelse(
        is.na(match_strength),
        NA_character_,
        sprintf("%.3f", unname(match_strength))
    )
))
}

# ============================================================
# Classify all hitters
# ============================================================

archetype_results <- lapply(
    seq_len(nrow(df)),
    function(i) {
        classify_archetype(
            df$BA_score[i],
            df$OBP_score[i],
            df$SLG_score[i],
            df$Kpct_score[i],
            df$BBpct_score[i]
        )
    }
)

df$Archetype <- vapply(
    archetype_results,
    function(x) x[["Archetype"]],
    character(1)
)

df$ArchetypeMatch <- vapply(
    archetype_results,
    function(x) x[["ArchetypeMatch"]],
    character(1)
)

df$ArchetypeStrength <- vapply(
    archetype_results,
    function(x) {
        value <- x[["ArchetypeStrength"]]

        if (is.na(value)) {
            return(NA_real_)
        }

        as.numeric(value)
    },
    numeric(1)
)


# ============================================================
# Park-Adjusted Overall
#
# Park exposure approximation:
#   50% home park + 50% neutral environment
#
# Only BA / OBP / SLG inputs are park-adjusted.
# K% and BB% scores remain unchanged.
# Original Overall, XP, divergence, and percentile calculations
# continue to use the unadjusted statistics.
# ============================================================
effective_pf <- function(pf) {
    0.5 * (pf / 100) + 0.5
}

df$H_park_adj  <- df[[h_col]]  / effective_pf(df$PF_H)
df$BB_park_adj <- df[[bb_col]] / effective_pf(df$PF_BB)
df$B1_park_adj <- df[[b1_col]] / effective_pf(df$PF_1B)
df$B2_park_adj <- df[[b2_col]] / effective_pf(df$PF_2B)
df$B3_park_adj <- df[[b3_col]] / effective_pf(df$PF_3B)
df$HR_park_adj <- df[[hr_col]] / effective_pf(df$PF_HR)

df$BA_park_adj <- ifelse(
    !is.na(ab_col) & df[[ab_col]] > 0,
    df$H_park_adj / df[[ab_col]],
    NA_real_
)

HBP_park <- if (!is.na(hbp_col)) df[[hbp_col]] else 0
SF_park  <- if (!is.na(sf_col))  df[[sf_col]]  else 0

obp_park_den <- df[[ab_col]] + df[[bb_col]] + HBP_park + SF_park
obp_park_num <- df$H_park_adj + df$BB_park_adj + HBP_park

df$OBP_park_adj <- ifelse(
    obp_park_den > 0,
    obp_park_num / obp_park_den,
    NA_real_
)

park_tb <- df$B1_park_adj +
           (2 * df$B2_park_adj) +
           (3 * df$B3_park_adj) +
           (4 * df$HR_park_adj)

df$SLG_park_adj <- ifelse(
    !is.na(ab_col) & df[[ab_col]] > 0,
    park_tb / df[[ab_col]],
    NA_real_
)

df$ParkAdjustedOverall <- compute_overall(
    df$BA_park_adj,
    df$OBP_park_adj,
    df$SLG_park_adj,
    df$Kpct,
    df$BBpct
)

df$ParkAdjustment <- df$ParkAdjustedOverall - df$OverallScore


# ============================================================
# Cross-Sectional Expected Overall
#
# Uses the current season population.
# Each player's profile is compared with the 10 closest
# complete profiles using the five component scores.
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

# Standardize profiles
profile_matrix <- scale(
    as.matrix(df[valid_profiles, profile_cols])
)

# ============================================================
# Calculate all pairwise distances ONCE
# ============================================================

distance_matrix <- as.matrix(dist(profile_matrix))

# Prevent each player from selecting himself
diag(distance_matrix) <- Inf

# ============================================================
# Similar Profiles Distance Matrix
#
# Uses the raw 0-10 component scores rather than standardized
# scores. This preserves the original five-metric profile
# geometry for player-to-player similarity.
# ============================================================

similarity_matrix <- as.matrix(
    df[valid_profiles, profile_cols]
)

similarity_distance_matrix <- as.matrix(
    dist(similarity_matrix)
)

# Prevent each player from matching himself
diag(similarity_distance_matrix) <- Inf

# ============================================================
# Expected Overall from 10 nearest neighbors
# ============================================================

k <- 10

expected_overall_valid <- apply(
    distance_matrix,
    1,
    function(d) {

        finite <- which(is.finite(d))

        if (length(finite) == 0) {
            return(NA_real_)
        }

        neighbors <- finite[
            order(d[finite])[
                1:min(k, length(finite))
            ]
        ]

        neighbor_indices <- profile_indices[neighbors]

        mean(
            df$OverallScore[neighbor_indices],
            na.rm = TRUE
        )
    }
)

# Store result back into full dataframe
df$ExpectedOverall <- NA_real_

df$ExpectedOverall[profile_indices] <-
    expected_overall_valid


# ============================================================
# Overall Divergence
# ============================================================

df$OverallDivergence <-
    df$OverallScore - df$ExpectedOverall


# ============================================================
# Overall Divergence Standard Deviation
# ============================================================

overall_divergence_sd <-
    sd(df$OverallDivergence, na.rm = TRUE)

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

cat(
    "PLAYER DEBUG:",
    "input =", player_name,
    "| clean =", player_name_clean,
    "| season arg =", season,
    "| rows =", nrow(df),
    "| name matches =", sum(df$NameClean == player_name_clean, na.rm = TRUE),
    "| season matches =", sum(df$Season == as.numeric(season), na.rm = TRUE),
    "\n",
    file = stderr()
)

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
# Find 3 Nearest Similar Profiles
# ============================================================

selected_df_index <- which(
    df$NameClean == player_name_clean &
    df$Season == as.numeric(season)
)[1]

# Find selected player's position inside the valid-profile matrix
selected_profile_position <- match(
    selected_df_index,
    profile_indices
)

similar_profiles <- list()

if (!is.na(selected_profile_position)) {

    distances <- similarity_distance_matrix[
        selected_profile_position,
    ]

    finite <- which(is.finite(distances))

    if (length(finite) > 0) {

        nearest_positions <- finite[
            order(distances[finite])[
                1:min(3, length(finite))
            ]
        ]

        nearest_df_indices <-
            profile_indices[nearest_positions]

        similar_profiles <- lapply(
            seq_along(nearest_df_indices),
            function(i) {

                idx <- nearest_df_indices[i]

                list(
                    Player = format_browser_name(
                        df$NameClean[idx]
                    ),
                    mlbId = as.integer(df$mlbId[idx]),

                    Team = if (!is.na(team_col))
                        as.character(df[[team_col]][idx])
                    else
                        NA_character_,

                    Overall = as.numeric(
                        df$OverallScore[idx]
                    ),

                    XP = as.numeric(
                        df$XP[idx]
                    ),

                    PA = if (!is.na(pa_col))
                        as.numeric(df[[pa_col]][idx])
                    else
                        NA_real_,

                    Distance = as.numeric(
                        distances[nearest_positions[i]]
                    )
                )
            }
        )
    }
}

result <- p %>%
  transmute(
    mlbId = as.integer(mlbId),
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

    Archetype = as.character(Archetype),
    ArchetypeMatch = as.character(ArchetypeMatch),
    ArchetypeStrength = as.numeric(ArchetypeStrength),

    Overall = as.numeric(OverallScore),
    ParkAdjustedOverall = as.numeric(ParkAdjustedOverall),
    ParkAdjustment = as.numeric(ParkAdjustment),
    ParkVenue = as.character(ParkVenue),
    ParkFactor = as.numeric(ParkFactor),
    ExpectedOverall = as.numeric(ExpectedOverall),
    OverallDivergence = as.numeric(OverallDivergence),
    OverallDivergenceSD = as.numeric(overall_divergence_sd),

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
    R    = if (!is.na(r_col))   as.numeric(.data[[r_col]])   else NA_real_,
    RBI  = if (!is.na(rbi_col)) as.numeric(.data[[rbi_col]]) else NA_real_,
    HBP  = if (!is.na(hbp_col)) as.numeric(.data[[hbp_col]]) else NA_real_,
    SF   = if (!is.na(sf_col))  as.numeric(.data[[sf_col]])  else NA_real_,
    SH   = if (!is.na(sh_col))  as.numeric(.data[[sh_col]])  else NA_real_,
    IBB  = if (!is.na(ibb_col)) as.numeric(.data[[ibb_col]]) else NA_real_
  )

cat(
    "SIMILAR PROFILE COUNT:",
    length(similar_profiles),
    "\n",
    file = stderr()
)

result$SimilarProfiles <- list(similar_profiles)

cat(
    "RESULT COLUMNS:",
    paste(names(result), collapse = ", "),
    "\n",
    file = stderr()
)

cat(
    toJSON(
        result,
        pretty = TRUE,
        auto_unbox = TRUE,
        na = "null"
    )
)
