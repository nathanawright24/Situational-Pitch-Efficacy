# Situational Pitch Efficacy

An interactive dashboard for exploring how well each pitch type performs across the
strike zone in specific game situations, built on five seasons of MLB Statcast data.
It is designed to **inform** pitch-planning decisions, not to prescribe them.

**Created by Nathaniel Wright.**

---

## What it does

For the pitch types you select, the dashboard draws one strike-zone heat map per pitch
(small multiples). Each zone is colored on the Baseball Savant red/blue spectrum relative
to the league average, for whatever game state you dial in.

- **Data:** MLB Statcast pitch-by-pitch, 2021–2025 regular seasons (~3.09M pitches).
- **Success metric:** a pitch counts as a success if it produces a swing-and-miss, a
  called strike, or weak contact (Statcast `launch_speed_angle` ≤ 3). The overall league
  success rate is ≈ 38.3%.
- **Filters:** pitcher and batter handedness, count (balls/strikes), outs, runners on base,
  per-pitch velocity / spin / horizontal & vertical break ranges, and optional batter
  caliber bands based on season xwOBA or OPS.
- **Design philosophy:** panels are ordered by how often a pitch is thrown (a neutral
  measure), never by which pitch "looks best," so the tool informs the read without
  telegraphing tendencies to opponents.

Each pitch card also shows a 95% confidence range next to its pitch count and flags
thin samples, so low-volume situations are read with appropriate caution.

---

## Pipeline

```
Pitching.R                → data/statcast_2021_2025_raw.fst      (raw pull)
pitch_effectiveness.R     → data/statcast_2021_2025_mod.fst      (cleaned + features)
build_dashboard_data.R    → Situational Pitch Efficacy.html      (self-contained dashboard)
                            (injects the packed data into a copy of the sample HTML)
```

Run the three R scripts in order. The first is a slow one-time scrape; the other two are
fast reprocessing steps.

```r
Rscript Pitching.R                # pull all seasons (slow; hits Statcast)
Rscript pitch_effectiveness.R     # clean, engineer features, attach batter season lines
Rscript build_dashboard_data.R    # pack the data and build the shareable HTML
```

Then open the generated **`Situational Pitch Efficacy.html`** in any browser. The badge next
to the title reads **Full data · <count>** when the real dataset is loaded, or
**Preview sample** when it is not.

---

## Files

### `Pitching.R` — data acquisition
Pulls pitch-by-pitch Statcast data for the 2021–2025 regular seasons in short date
windows, de-duplicates on `game_pk` / `at_bat_number` / `pitch_number`, and keeps a
curated set of columns (location, movement `pfx_x`/`pfx_z`, `api_break_x`/`api_break_z`,
`sz_top`/`sz_bot`, `spin_axis`, `release_extension`, velocity, spin, count/outs/bases,
handedness, pitch type, event outcomes, and the fields needed to compute xwOBA and OPS).
Writes `data/statcast_2021_2025_raw.fst`. This is the only step that scrapes the network,
so it is the slow one; re-run it only when you need to refresh the underlying data.

### `pitch_effectiveness.R` — cleaning, features, and batter caliber
Reads the raw pull and produces the analysis-ready per-pitch file the exporter consumes.
It:
- defines the NA-safe **success** flag (whiff OR called strike OR weak contact),
- engineers situational fields, including a handedness-mirrored horizontal location
  (`plate_x_bat`), binary pitcher/batter hand, a `bases_occupied` string, and the count,
- computes each hitter's **season xwOBA** (expected wOBA on batted balls plus the actual
  wOBA weights for strikeouts, walks, and hit-by-pitch) and **season OPS** (reconstructed
  from the `events` column), then joins those onto every pitch so the dashboard can band
  pitches by the quality of the hitter faced.

Writes `data/statcast_2021_2025_mod.fst` (the cleaned per-pitch file) along with
population-level aggregate tables.

### `build_dashboard_data.R` — data packer and single-file build
Reads `data/statcast_2021_2025_mod.fst` and bit-packs every pitch into 8 bytes, base64-
encodes the result, and injects it inline into a copy of the sample dashboard to produce
the self-contained **`Situational Pitch Efficacy.html`** (nothing else required to view it).
It also writes a `pitch_data.js` sidecar and prints the output path plus a check confirming
the data actually embedded.

The packed layout (kept in sync with the dashboard's decoder):

| Byte | Contents |
|------|----------|
| b0 | pitcher hand, batter hand, balls, strikes, outs |
| b1 | runners on base, pitch-type index |
| b2 | strike-zone index, success flag |
| b3 | velocity (scaled) |
| b4 | spin rate (scaled) |
| b5 | horizontal break, inches (scaled) |
| b6 | vertical break, inches (scaled) |
| b7 | batter xwOBA band, batter OPS band |

Batter bands are 50-point steps for xwOBA and 75-point steps for OPS.

### `situational pitch efficacy (sample).html` — the dashboard
The front end itself, and the template `build_dashboard_data.R` injects the full data into.
It ships with an embedded ~18,000-pitch sample so it runs standalone as a preview (badge:
**Preview sample**). Features:

- small-multiple strike-zone panels per selected pitch type, with per-zone success rates
  (inner 3×3 and the four outer chase corners) on the Savant red/blue scale;
- a grouped, checkbox pitch selector (Fastballs / Offspeed / Breaking) defaulting to the
  four most common pitches;
- per-pitch velocity, spin, and (for offspeed/breaking) break sliders, each bounded to that
  pitch's own observed range;
- an optional, collapsible **Batter Info** section for saving hitter profiles (name, hand,
  xwOBA, OPS) and clicking between them, with CSV template / import / export;
- light and dark themes and a **How it Works / Methods** help panel.

In the preview build, break values and batter bands are placeholders (the sample carries no
batter identity); both become real once the full data is injected.

---

## Requirements

- R (4.1 or newer).
- R packages used across the scripts include `baseballr` (Statcast scrape), `dplyr`, `fst`,
  and `jsonlite`. See the `library()` calls at the top of each script for the exact set.
- A modern web browser to view the generated HTML.

---

## Viewing and hosting

The generated `Situational Pitch Efficacy.html` is a single self-contained file. For a single
viewer, open it directly in a browser; it renders and runs fully offline. Because it embeds the
whole dataset it is a large file, which is expected.

For hosting the dashboard for external viewers on the full dataset, the planned path is a
server-side stack (partitioned Parquet queried by DuckDB behind a Dash/Streamlit app) so that
viewers hit a URL rather than downloading the data. Publishing is handled through IMG Academy
support.

---

## Methodology notes

- **Success** = whiff OR called strike OR weak contact (`launch_speed_angle` ≤ 3), computed
  NA-safe. League-wide rate ≈ 38.3%.
- **Zone coloring** diverges red (above the league success rate) to blue (below), centered on
  the league average; faded cells indicate few pitches.
- **Confidence** is a 95% Wilson score interval shown per pitch; thin samples are flagged.
- **Batter caliber bands** group hitters by season xwOBA (50-point bands) or OPS (75-point
  bands). Loading a batter filters to pitches thrown to hitters in that same band — a
  "hitters like this" view, not the individual, so it holds up for hitters not in the data.

---

*Data via MLB Statcast / Baseball Savant. Created by Nathaniel Wright.*
