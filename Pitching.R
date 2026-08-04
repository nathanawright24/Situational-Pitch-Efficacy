# =============================================================================
# Pitching.R  —  Statcast data acquisition
# =============================================================================

# ---- 0. Dependency bootstrap ------------------------------------------------
ensure_pkgs <- function(pkgs, repos = "https://cloud.r-project.org") {
  installed <- rownames(installed.packages())
  missing   <- setdiff(pkgs, installed)
  if (length(missing)) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = repos)
  }
  invisible(lapply(pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Package '%s' could not be installed/loaded.", p))
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}

cat("R version   :", R.version.string, "\n")
cat("Library path :", paste(.libPaths(), collapse = " ; "), "\n\n")

ensure_pkgs(c("dplyr", "lubridate", "fst", "data.table"))

# ---- 1. Configuration -------------------------------------------------------
seasons     <- 2021:2025
chunk_days  <- 4
sleep_secs  <- 6
max_tries   <- 3
cap_warn    <- 24000
out_dir     <- "data"
out_fst     <- file.path(out_dir, "statcast_2021_2025_raw.fst")
out_csv     <- file.path(out_dir, "statcast_2021_2025_raw.csv")
WRITE_CSV   <- TRUE

season_start <- function(y) as.Date(sprintf("%d-04-01", y))
season_end   <- function(y) as.Date(sprintf("%d-09-30", y))

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

keep_cols <- c(
  "game_pk", "game_date", "at_bat_number", "pitch_number",
  "balls", "strikes", "outs_when_up", "inning", "inning_topbot",
  "on_1b", "on_2b", "on_3b", "bat_score", "fld_score",
  "stand", "p_throws", "batter", "pitcher",
  "pitch_type", "pitch_name", "release_speed", "release_spin_rate",
  "plate_x", "plate_z", "pfx_x", "pfx_z", "zone", "type",
  "sz_top", "sz_bot", "api_break_x", "api_break_z", "spin_axis", "release_extension",
  "events", "description", "bb_type",
  "launch_speed_angle", "launch_speed", "launch_angle", "babip_value",
  "delta_run_exp", "estimated_woba_using_speedangle", "woba_value", "woba_denom",
  "game_type", "home_team", "away_team",
  "fielder_2", "fielder_3", "fielder_4", "fielder_5",
  "fielder_6", "fielder_7", "fielder_8", "fielder_9"
)

# ---- 2. One resilient chunk pull -------------------------------------------
build_statcast_url <- function(start, end) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?",
    "all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=",
    "&hfGT=R%7CPO%7CS%7C&hfC=&hfSea=", lubridate::year(start), "%7C",
    "&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=",
    "&player_type=batter&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=",
    "&game_date_gt=", start, "&game_date_lt=", end,
    "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0",
    "&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed",
    "&sort_order=desc&min_abs=0&type=details"
  )
}

pull_chunk <- function(start, end) {
  url <- build_statcast_url(start, end)
  for (i in seq_len(max_tries)) {
    res <- tryCatch(
      as.data.frame(data.table::fread(url, encoding = "UTF-8")),
      warning = function(w) { message("  ! ", conditionMessage(w)); NULL },
      error   = function(e) { message("  ! ", conditionMessage(e)); NULL }
    )
    if (!is.null(res) && nrow(res) > 0) {
      if (nrow(res) >= cap_warn)
        warning(sprintf("Chunk %s..%s returned %d rows (>= cap). It may be TRUNCATED; reduce chunk_days.",
                        start, end, nrow(res)))
      return(res)
    }
    Sys.sleep(sleep_secs * 2)
  }
  message(sprintf("  -- no data for %s..%s (empty or failed after %d tries)", start, end, max_tries))
  NULL
}

# ---- 3. Loop seasons -> date chunks ----------------------------------------
all_seasons <- list()

for (yr in seasons) {
  cat(sprintf("\n=== Season %d ===\n", yr))
  starts <- seq(season_start(yr), season_end(yr), by = chunk_days)
  season_chunks <- vector("list", length(starts))

  for (k in seq_along(starts)) {
    s <- starts[k]
    e <- min(s + (chunk_days - 1), season_end(yr))
    cat(sprintf("  [%3d/%3d] %s .. %s\n", k, length(starts), s, e))
    df <- pull_chunk(s, e)
    if (!is.null(df)) {
      have <- intersect(keep_cols, names(df))
      season_chunks[[k]] <- dplyr::select(df, dplyr::all_of(have))
    }
    Sys.sleep(sleep_secs)
  }

  season_df <- dplyr::bind_rows(season_chunks)
  cat(sprintf("  season %d raw rows (all game_types): %d\n", yr, nrow(season_df)))
  all_seasons[[as.character(yr)]] <- season_df
}

raw <- dplyr::bind_rows(all_seasons)

# ---- 4. Filter to regular season, add year, de-duplicate -------------------
raw <- raw |>
  dplyr::filter(game_type == "R") |>
  dplyr::mutate(game_year = lubridate::year(game_date))

dedup_key <- intersect(c("game_pk", "at_bat_number", "pitch_number"), names(raw))
before <- nrow(raw)
if (length(dedup_key) == 3) {
  raw <- dplyr::distinct(raw, dplyr::across(dplyr::all_of(dedup_key)), .keep_all = TRUE)
} else {
  warning("Dedup key incomplete (", paste(dedup_key, collapse = ","), "); using full-row distinct().")
  raw <- dplyr::distinct(raw)
}
cat(sprintf("\nDe-duplication removed %d rows (%d -> %d)\n", before - nrow(raw), before, nrow(raw)))

# ---- 5. Per-season sanity check --------------------------------------------
cat("\nRegular-season pitch counts by year (sanity-check against ~700k-720k/full season):\n")
print(dplyr::count(raw, game_year))

# ---- 6. Write ---------------------------------------------------------------
fst::write_fst(raw, out_fst, compress = 80)
cat(sprintf("\nWrote %s  (%d rows x %d cols)\n", out_fst, nrow(raw), ncol(raw)))
if (WRITE_CSV) {
  data.table_ok <- requireNamespace("data.table", quietly = TRUE)
  if (data.table_ok) data.table::fwrite(raw, out_csv) else utils::write.csv(raw, out_csv, row.names = FALSE)
  cat(sprintf("Wrote %s\n", out_csv))
}

cat("\nDone. Next step: run pitch_effectiveness.R against", out_fst, "\n")
