# =============================================================================
# build_dashboard_data.R  —  export the FULL pitch set for the dashboard  (v3)
#
# Consumes the cleaned per-pitch file from pitch_effectiveness.R
#   data/statcast_2021_2025_mod.fst
# and produces a SELF-CONTAINED dashboard:
#   situational_pitch_efficacy_full.html   (data baked in; nothing else needed)
# It also drops pitch_data.js beside it as a convenience.
#
# --- BINARY CONTRACT (must match decodePacked() in the dashboard) -----------
# b64 -> Uint8Array of length 8*N, column-major: b0|b1|b2|b3|b4|b5|b6|b7
#   b0 : bit7 p_throws(1=R) | bit6 stand(1=R) | bits5-4 balls | bits3-2 strikes | bits1-0 outs
#   b1 : bits7-5 bases (bit0=1st,bit1=2nd,bit2=3rd) | bits4-0 pitch-type index
#   b2 : bits7-4 zone index (into zoneVals) | bit3 success | bits2-0 spare
#   b3 : velo scaled   = round((release_speed      - veloFloor)/veloStep);  255 = NA
#   b4 : spin scaled   = round((release_spin_rate   - spinFloor)/spinStep);  255 = NA
#   b5 : h-break inches = round((pfx_x*12 - breakFloor)/breakStep);          255 = NA
#   b6 : v-break inches = round((pfx_z*12 - breakFloor)/breakStep);          255 = NA
#   b7 : bits7-4 xwOBA band (0-14, 15=NA) | bits3-0 OPS band (0-14, 15=NA)
#        xwOBA band = floor((bat_xwoba-0.100)/0.050);  OPS band = floor((bat_ops-0.300)/0.075)
# =============================================================================

ensure_pkgs <- function(pkgs, repos = "https://cloud.r-project.org") {
  miss <- setdiff(pkgs, rownames(installed.packages()))
  if (length(miss)) install.packages(miss, repos = repos)
  invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
}
ensure_pkgs(c("dplyr", "fst", "jsonlite"))

in_fst   <- file.path("data", "statcast_2021_2025_mod.fst")
html_in  <- "situational_pitch_efficacy.html"          # the delivered dashboard
html_out <- "Situational Pitch Efficacy.html"          # self-contained result
js_out   <- "pitch_data.js"
if (!file.exists(in_fst)) stop("Run pitch_effectiveness.R first; missing: ", in_fst)

# ---- encoding constants (mirrored in the dashboard) -------------------------
VELO_STEP <- 0.5
SPIN_FLOOR <- 0;  SPIN_STEP <- 25
BREAK_FLOOR <- -30; BREAK_STEP <- 0.5          # inches
XW_FLOOR <- 0.100;  XW_STEP <- 0.050
OPS_FLOOR <- 0.300; OPS_STEP <- 0.075
BAND_NA <- 15L
ZONE_VALS <- c(1:9, 11:14)

need <- c("p_throws_bin","stand_bin","balls","strikes","outs_when_up","bases_occupied",
          "pitch_type","zone","success","release_speed","release_spin_rate",
          "pfx_x","pfx_z","bat_xwoba","bat_ops")
have <- fst::metadata_fst(in_fst)$columnNames
miss <- setdiff(need, have)
if (length(miss))
  stop("Missing columns in mod.fst (re-run the updated pitch_effectiveness.R): ",
       paste(miss, collapse = ", "))

df <- fst::read_fst(in_fst, columns = need)
cat(sprintf("Loaded %d rows.\n", nrow(df)))

df <- df |>
  dplyr::filter(!is.na(p_throws_bin), !is.na(stand_bin),
                balls>=0, balls<=3, strikes>=0, strikes<=2, outs_when_up>=0, outs_when_up<=2,
                !is.na(pitch_type), pitch_type!="", zone %in% ZONE_VALS, !is.na(success))
cat(sprintf("After packable filter: %d rows.\n", nrow(df)))

pt_levels <- names(sort(table(df$pitch_type), decreasing = TRUE))
if (length(pt_levels) > 31) stop("More than 31 pitch types; widen ty field.")
ty_idx <- match(df$pitch_type, pt_levels) - 1L
VELO_FLOOR <- floor(min(df$release_speed, na.rm = TRUE))

bases  <- as.integer(ifelse(grepl("1", df$bases_occupied),1L,0L) +
                     ifelse(grepl("2", df$bases_occupied),2L,0L) +
                     ifelse(grepl("3", df$bases_occupied),4L,0L))
zone_i <- match(df$zone, ZONE_VALS) - 1L

scale_u8 <- function(x, floor, step){v <- round((x-floor)/step); v[is.na(v)|v<0|v>254] <- 255L; as.integer(v)}
xw_band  <- ifelse(is.na(df$bat_xwoba), BAND_NA, pmin(pmax(floor((df$bat_xwoba-XW_FLOOR)/XW_STEP),0),14))
ops_band <- ifelse(is.na(df$bat_ops),   BAND_NA, pmin(pmax(floor((df$bat_ops -OPS_FLOOR)/OPS_STEP),0),14))

velo_u8 <- scale_u8(df$release_speed,     VELO_FLOOR,  VELO_STEP)
spin_u8 <- scale_u8(df$release_spin_rate, SPIN_FLOOR,  SPIN_STEP)
hb_u8   <- scale_u8(df$pfx_x*12,          BREAK_FLOOR, BREAK_STEP)
vb_u8   <- scale_u8(df$pfx_z*12,          BREAK_FLOOR, BREAK_STEP)

b0 <- bitwOr(bitwOr(bitwOr(bitwOr(bitwShiftL(df$p_throws_bin,7L),bitwShiftL(df$stand_bin,6L)),
        bitwShiftL(df$balls,4L)),bitwShiftL(df$strikes,2L)),df$outs_when_up)
b1 <- bitwOr(bitwShiftL(bases,5L), ty_idx)
b2 <- bitwOr(bitwShiftL(zone_i,4L), bitwShiftL(df$success,3L))
b3 <- velo_u8; b4 <- spin_u8; b5 <- hb_u8; b6 <- vb_u8
b7 <- bitwOr(bitwShiftL(as.integer(xw_band),4L), as.integer(ops_band))

N <- nrow(df)
packed <- as.raw(c(b0,b1,b2,b3,b4,b5,b6,b7))
stopifnot(length(packed) == 8L*N)

meta <- list(
  nTotal = N,
  veloMin = floor(min(df$release_speed,na.rm=TRUE)), veloMax = ceiling(max(df$release_speed,na.rm=TRUE)),
  spinMin = SPIN_FLOOR, spinMax = ceiling(max(df$release_spin_rate,na.rm=TRUE)/50)*50,
  veloFloor = VELO_FLOOR, veloStep = VELO_STEP, spinFloor = SPIN_FLOOR, spinStep = SPIN_STEP,
  breakFloor = BREAK_FLOOR, breakStep = BREAK_STEP,
  xwFloor = XW_FLOOR, xwStep = XW_STEP, opsFloor = OPS_FLOOR, opsStep = OPS_STEP, bandNA = BAND_NA,
  zoneVals = ZONE_VALS, pitchTypes = pt_levels, overallRate = mean(df$success), sampled = FALSE)

# base64 MUST be a single line: any newline inside the JS string literal below
# would be a syntax error and silently blank out window.PITCH_DATA.
b64 <- gsub("[[:space:]]", "", jsonlite::base64_enc(packed))
data_js <- paste0("window.PITCH_DATA = {\n  \"meta\": ",
                  jsonlite::toJSON(meta, auto_unbox = TRUE, digits = 6),
                  ",\n  \"b64\": \"", b64, "\"\n};")

# convenience sidecar
writeLines(data_js, js_out)

# self-contained HTML: inject the data inline into a copy of the dashboard
if (file.exists(html_in)) {
  html <- paste(readLines(html_in, warn = FALSE), collapse = "\n")
  inject <- paste0("<script>", data_js, "</script>")
  # literal split/paste (no regex replacement quirks on a large base64 payload)
  split_on <- function(s, tok) strsplit(s, tok, fixed = TRUE)[[1]]
  if (grepl("<!--__PITCH_DATA__-->", html, fixed = TRUE)) {
    p <- split_on(html, "<!--__PITCH_DATA__-->")
    html <- paste0(p[1], inject, paste(p[-1], collapse = "<!--__PITCH_DATA__-->"))
  } else if (grepl("</body>", html, fixed = TRUE)) {
    p <- split_on(html, "</body>")
    html <- paste0(p[1], inject, "\n</body>", paste(p[-1], collapse = "</body>"))
  } else {
    stop("Could not find an injection point in ", html_in)
  }
  writeLines(html, html_out)
  ok <- any(grepl("window.PITCH_DATA", readLines(html_out, warn = FALSE), fixed = TRUE))
  cat(sprintf("\nWrote %s : %.1f MB  (self-contained, data embedded: %s)\n",
              html_out, file.info(html_out)$size/1024/1024, if (ok) "YES" else "NO -- CHECK"))
  cat("\n>>> OPEN THIS FILE IN THE BROWSER:  ", normalizePath(html_out), "\n")
  cat(">>> Its subtitle should read \"... pitches (full)\", NOT \"... sample of ...\".\n\n")
} else {
  stop("Dashboard HTML not found: ", html_in,
       "\n     Put situational_pitch_efficacy.html in this folder and re-run so the",
       "\n     single-file build (", html_out, ") can be created.")
}
cat(sprintf("Sidecar %s : %.1f MB  (%d pitches, overall success %.4f)\n",
            js_out, file.info(js_out)$size/1024/1024, N, meta$overallRate))
