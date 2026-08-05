# =============================================================================
# pitch_effectiveness.R  —  cleaning, feature engineering, aggregation
# Situational Pitch Efficacy (rebuild)
#
# Consumes the raw pull from Pitching.R and produces:
#   (1) a per-pitch cleaned file           -> statcast_2021_2025_mod.(fst|csv)
#   (2) a tidy PITCHER success table        -> pitcher_success_table.csv/.json
#   (3) an EB-shrunk HITTER zone table      -> hitter_zone_table.csv/.json
#
# Key corrections vs. the original:
#   * success is a clean 0/1 over EVERY pitch (no NA). Verified on 3.09M real
#     rows: old effective rate 0.1745 -> corrected 0.3830, driven mostly by the
#     ~508k CALLED STRIKES the old logic silently dropped into NA.
#   * called strikes restored to the definition (matches the project deck).
#   * weak contact uses launch_speed_angle <= 3 (deck's threshold, not < 3).
#   * plate_x is mirrored by batter handedness so "inside/outside" is consistent.
#   * batter primary position derived from the fielder_2..9 alignment columns.
# =============================================================================

# ---- 0. Dependency bootstrap ------------------------------------------------
ensure_pkgs <- function(pkgs, repos = "https://cloud.r-project.org") {
  missing <- setdiff(pkgs, rownames(installed.packages()))
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
cat("R version    :", R.version.string, "\n")
cat("Library path :", paste(.libPaths(), collapse = " ; "), "\n\n")
ensure_pkgs(c("dplyr", "tidyr", "fst", "jsonlite"))

# ---- 1. Load raw pull -------------------------------------------------------
in_fst  <- file.path("data", "statcast_2021_2025_raw.fst")
out_dir <- "data"
if (!file.exists(in_fst)) stop("Run Pitching.R first; missing: ", in_fst)
raw <- fst::read_fst(in_fst)
cat(sprintf("Loaded %d rows x %d cols from %s\n", nrow(raw), ncol(raw), in_fst))

# ---- 2. Feature engineering -------------------------------------------------
# 2a. success components (all NA-safe -> success is strictly 0/1, never NA)
raw <- raw |>
  dplyr::mutate(
    whiff        = as.integer(grepl("swinging_strike", description)),         
    called       = as.integer(!is.na(description) & description == "called_strike"),
    weak_contact = as.integer(!is.na(launch_speed_angle) & launch_speed_angle <= 3),
    success      = as.integer(whiff == 1L | called == 1L | weak_contact == 1L)
  )

# ---- OPTIONAL foul reclassification (disabled by default) -------------------
# Fouls carry NO contact-quality data in Statcast (bb_type + launch_speed_angle
# are blank), so they are counted as failures. IF the new pull has decent foul
# exit-velocity coverage, low-EV fouls can be flagged as weak-contact successes.
# First inspect coverage, THEN decide whether to enable the block below.
foul_idx <- which(!is.na(raw$description) & raw$description == "foul")
if (length(foul_idx)) {
  ev_cov <- mean(!is.na(raw$launch_speed[foul_idx]))
  cat(sprintf("\nFoul EV coverage: %.1f%% of %d fouls have a launch_speed.\n",
              100 * ev_cov, length(foul_idx)))
}
RECLASSIFY_WEAK_FOULS <- FALSE
foul_ev_cut <- 60  
if (RECLASSIFY_WEAK_FOULS) {
  weak_foul <- !is.na(raw$description) & raw$description == "foul" &
               !is.na(raw$launch_speed) & raw$launch_speed < foul_ev_cut
  raw$weak_contact[weak_foul] <- 1L
  raw$success[weak_foul]      <- 1L
  cat(sprintf("Reclassified %d low-EV fouls as weak-contact successes.\n", sum(weak_foul)))
}

# 2b. bases_occupied string (on_Xb holds a runner's player id, else NA)
raw <- raw |>
  dplyr::mutate(
    bases_occupied = paste0(
      ifelse(is.na(on_1b), "", "1"),
      ifelse(is.na(on_2b), "", "2"),
      ifelse(is.na(on_3b), "", "3")
    ),
    count = paste0(balls, "-", strikes)
  )

# 2c. handedness-mirrored plate_x.
#   Statcast plate_x is catcher's-view (feet from plate center). Mirroring puts
#   both batter sides on a common axis. CONVENTION HERE: positive = INSIDE to the
#   batter. Verify the sign once with a quick plot of a known pitcher; flip the
#   multiplier if inside/outside looks reversed.
raw <- raw |>
  dplyr::mutate(plate_x_bat = ifelse(stand == "R", -plate_x, plate_x))

# 2d. binary handedness (kept alongside the original letters for the dashboard)
raw <- raw |>
  dplyr::mutate(p_throws_bin = as.integer(p_throws == "R"),
                stand_bin    = as.integer(stand == "R"))

# ---- 3. Derive each batter's primary position from fielder alignment --------
# A batter's id appears in fielder_2..9 only on pitches where they were fielding.
# Count appearances per slot across all pitches -> modal slot = primary position.
# Batters who essentially never field (full-time DH) fall through to "DH".
fielder_slots <- c(fielder_2 = "C",  fielder_3 = "1B", fielder_4 = "2B",
                   fielder_5 = "3B", fielder_6 = "SS", fielder_7 = "LF",
                   fielder_8 = "CF", fielder_9 = "RF")
have_fielders <- intersect(names(fielder_slots), names(raw))

if (length(have_fielders)) {
  pos_long <- raw |>
    dplyr::select(dplyr::all_of(have_fielders)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "slot", values_to = "player_id") |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::mutate(pos = fielder_slots[slot]) |>
    dplyr::count(player_id, pos, name = "appearances")

  primary_pos <- pos_long |>
    dplyr::group_by(player_id) |>
    dplyr::slice_max(appearances, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(batter = player_id, primary_pos = pos)
} else {
  warning("No fielder_* columns found; every batter will bucket to DH.")
  primary_pos <- data.frame(batter = integer(0), primary_pos = character(0))
}

# 6-bucket scheme (C / corner IF / middle IF / CF / corner OF / DH)
bucket_of <- function(p) dplyr::case_when(
  p == "C"            ~ "C",
  p %in% c("1B","3B") ~ "CI",
  p %in% c("2B","SS") ~ "MI",
  p == "CF"           ~ "CF",
  p %in% c("LF","RF") ~ "COF",
  TRUE                ~ "DH"
)

raw <- raw |>
  dplyr::left_join(primary_pos, by = "batter") |>
  dplyr::mutate(pos_bucket = dplyr::coalesce(bucket_of(primary_pos), "DH"))

cat("\nBatter rows by position bucket:\n")
print(dplyr::count(raw, pos_bucket))

# ---- 4. Diagnostics ---------------------------------------------------------
cat(sprintf("\nOverall corrected success rate: %.4f  (n = %d)\n",
            mean(raw$success), nrow(raw)))
cat("Success by component (non-exclusive):\n")
cat(sprintf("  whiffs=%d  called=%d  weak_contact=%d\n",
            sum(raw$whiff), sum(raw$called), sum(raw$weak_contact)))

# ---- 4b. Batter-season caliber: xwOBA and OPS ------------------------------
# Tag every pitch with the FACED batter's season line, so the dashboard can
# band pitches by hitter quality ("how do my pitches play against hitters this
# good"). Both metrics come from columns already in the pull.
#   xwOBA : sum of expected wOBA value per PA / wOBA denominator.
#           expected value = xwOBAcon on batted balls (estimated_woba_using_
#           speedangle), falling back to the actual wOBA weight for K/BB/HBP.
#   OPS   : OBP + SLG, reconstructed from the `events` column.
ev <- raw$events
is_pa   <- !is.na(ev) & ev != ""
is_1b   <- ev == "single";  is_2b <- ev == "double"
is_3b   <- ev == "triple";  is_hr <- ev == "home_run"
is_h    <- is_1b | is_2b | is_3b | is_hr
is_bb   <- ev %in% c("walk", "intent_walk")
is_hbp  <- ev == "hit_by_pitch"
is_sf   <- ev %in% c("sac_fly", "sac_fly_double_play")
is_sh   <- ev %in% c("sac_bunt", "sac_bunt_double_play")
is_ci   <- ev == "catcher_interf"
is_ab   <- is_pa & !(is_bb | is_hbp | is_sf | is_sh | is_ci) 
tb      <- as.integer(is_1b) + 2L*as.integer(is_2b) + 3L*as.integer(is_3b) + 4L*as.integer(is_hr)

ops_tbl <- data.frame(
  batter = raw$batter, game_year = raw$game_year,
  ab = as.integer(is_ab), h = as.integer(is_h), bb = as.integer(is_bb),
  hbp = as.integer(is_hbp), sf = as.integer(is_sf), tb = tb
) |>
  dplyr::group_by(batter, game_year) |>
  dplyr::summarise(dplyr::across(c(ab, h, bb, hbp, sf, tb), sum), .groups = "drop") |>
  dplyr::mutate(
    obp = ifelse((ab + bb + hbp + sf) > 0, (h + bb + hbp) / (ab + bb + hbp + sf), NA_real_),
    slg = ifelse(ab > 0, tb / ab, NA_real_),
    bat_ops = obp + slg
  ) |>
  dplyr::select(batter, game_year, bat_ops)

xw_tbl <- raw |>
  dplyr::filter(!is.na(woba_denom), woba_denom > 0) |>
  dplyr::mutate(xw_num = dplyr::coalesce(estimated_woba_using_speedangle, woba_value)) |>
  dplyr::group_by(batter, game_year) |>
  dplyr::summarise(bat_xwoba = sum(xw_num, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE),
                   .groups = "drop")

raw <- raw |>
  dplyr::left_join(xw_tbl,  by = c("batter", "game_year")) |>
  dplyr::left_join(ops_tbl, by = c("batter", "game_year"))

cat(sprintf("\nBatter-season lines attached: xwOBA on %.1f%% of pitches, OPS on %.1f%%.\n",
            100 * mean(!is.na(raw$bat_xwoba)), 100 * mean(!is.na(raw$bat_ops))))

# ---- 5. Per-pitch cleaned output (dashboard-compatible) --------------------
fst::write_fst(raw, file.path(out_dir, "statcast_2021_2025_mod.fst"), compress = 80)

# ---- 6. TIDY PITCHER SUCCESS TABLE -----------------------------------------
# One row per (situation x pitch type x zone) with sample size + success rate,
# plus mean run value and xwOBA. Replaces the old hundreds-of-split-CSVs.
pitcher_success_table <- raw |>
  dplyr::group_by(p_throws, stand, balls, strikes, outs_when_up,
                  bases_occupied, pitch_type, zone) |>
  dplyr::summarise(
    n_pitches    = dplyr::n(),
    n_successes  = sum(success),
    success_rate = mean(success),
    mean_run_exp = mean(delta_run_exp, na.rm = TRUE),
    mean_xwoba   = { d <- sum(woba_denom, na.rm = TRUE)
                     if (d > 0) sum(estimated_woba_using_speedangle * woba_denom, na.rm = TRUE) / d else NA_real_ },
    .groups = "drop"
  )
utils::write.csv(pitcher_success_table,
                 file.path(out_dir, "pitcher_success_table.csv"), row.names = FALSE)

# ---- 7. HITTER ZONE TABLE with empirical-Bayes shrinkage -------------------
# Metric shrunk: run value (delta_run_exp), which is defined on every pitch.
# Prior group g = (pos_bucket x stand x p_throws x count x zone). Each batter's
# cell mean is pulled toward its position-bucket prior; the pull strength comes
# from the data (method-of-moments EB, per Morris 1983), not a hand-set constant.

# 7a. batter cell aggregates
batter_cells <- raw |>
  dplyr::filter(!is.na(delta_run_exp), !is.na(zone)) |>
  dplyr::group_by(batter, pos_bucket, stand, p_throws, count, zone) |>
  dplyr::summarise(n = dplyr::n(),
                   ybar = mean(delta_run_exp),
                   yvar = stats::var(delta_run_exp),   # NA when n == 1
                   .groups = "drop")

# 7b. prior-group moments
grp_keys <- c("pos_bucket", "stand", "p_throws", "count", "zone")

#   Step 1: pure aggregates per group -> prior mean (mu), pooled within-batter
#   variance (sigma2), weighted variance of the batter means (Vybar), #batters.
group_base <- batter_cells |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grp_keys))) |>
  dplyr::summarise(
    n_bat  = dplyr::n(),
    mu     = stats::weighted.mean(ybar, n),
    sigma2 = stats::weighted.mean(yvar, n, na.rm = TRUE),
    Vybar  = sum(n * (ybar - stats::weighted.mean(ybar, n))^2) / sum(n),
    .groups = "drop"
  )

#   Step 2: Ebar = mean sampling variance across the group's batter cells,
#   using each cell's own within variance (falling back to the group's sigma2
#   for singletons). tau2 = between-batter variance = Vybar - Ebar (floored).
group_moments <- batter_cells |>
  dplyr::left_join(group_base, by = grp_keys) |>
  dplyr::mutate(se2_cell = dplyr::coalesce(yvar, sigma2) / pmax(n, 1)) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(grp_keys))) |>
  dplyr::summarise(
    mu     = dplyr::first(mu),
    sigma2 = dplyr::first(sigma2),
    Vybar  = dplyr::first(Vybar),
    n_bat  = dplyr::first(n_bat),
    Ebar   = mean(se2_cell, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # between-batter variance; floored to a small positive value => heavy shrinkage
  # toward the prior when the data can't separate batters within the group
  dplyr::mutate(tau2 = pmax(Vybar - Ebar, 1e-6))

# 7c. shrink each batter cell toward its group prior
hitter_zone_table <- batter_cells |>
  dplyr::left_join(dplyr::select(group_moments, dplyr::all_of(grp_keys), mu, sigma2, tau2, n_bat),
                   by = grp_keys) |>
  dplyr::mutate(
    # a lone-observation cell (yvar NA) borrows the group's within variance
    sigma2_i   = dplyr::coalesce(yvar, sigma2),
    se2        = sigma2_i / n,               
    B          = se2 / (se2 + tau2),          
    rv_shrunk  = (1 - B) * ybar + B * mu      
  ) |>
  dplyr::select(batter, pos_bucket, stand, p_throws, count, zone,
                n, rv_raw = ybar, rv_prior = mu, shrink_weight = B, rv_shrunk, n_batters_in_group = n_bat)

utils::write.csv(hitter_zone_table,
                 file.path(out_dir, "hitter_zone_table.csv"), row.names = FALSE)

# ---- 8. Compact JSON for embedding in a static HTML dashboard --------------
# These aggregated tables are small enough to bake straight into an HTML file
# (no server / no 400MB CSV at runtime). Check the printed sizes.
jsonlite::write_json(pitcher_success_table,
                     file.path(out_dir, "pitcher_success_table.json"), dataframe = "rows")
jsonlite::write_json(hitter_zone_table,
                     file.path(out_dir, "hitter_zone_table.json"), dataframe = "rows")

cat("\nOutputs written to '", out_dir, "/':\n", sep = "")
for (f in c("statcast_2021_2025_mod.fst", "pitcher_success_table.csv",
            "pitcher_success_table.json", "hitter_zone_table.csv", "hitter_zone_table.json")) {
  p <- file.path(out_dir, f)
  if (file.exists(p)) cat(sprintf("  %-34s %8.1f KB\n", f, file.info(p)$size / 1024))
}
cat("\nDone.\n")
