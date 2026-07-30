########################################
#  10_visualizations.R                 #
#  Created: 13/7/2026                  #
#  Updated: 28/7/2026                  #
########################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

#--- Paths -----------------------------------------------------------------
# Run order: 01 -> 02-08 (any order among themselves) -> 09 (data
# investigations, reads only 01's outputs) -> this script (10, reads 09
# plus 02-08's regression/weather outputs).
home       <- "C:/Users/HP/Documents/GitHub/datharm-placement"
mch_dir    <- file.path(home, "03_output/01_mchtrack_data")
inv_dir    <- file.path(home, "03_output/09_data_investigations")
reg_dir    <- file.path(home, "03_output/03_regression")
resid_dir  <- file.path(home, "03_output/04_ward_residuals")
era_dir    <- file.path(home, "03_output/06_era5_analysis")
chirps_dir <- file.path(home, "03_output/02_chirps_analysis")
ndvi_dir   <- file.path(home, "03_output/08_ndvi_analysis")
out_dir    <- file.path(home, "03_output/10_visualizations")
figs_dir   <- file.path(out_dir, "figs")
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(figs_dir, showWarnings = FALSE, recursive = TRUE)

library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(geodata)
library(ggrepel)

#----------------------------------------------------------------------------

##############################
# Helpers — parsing & saving #
##############################

# Reads a modelsummary plain-text (pipe-delimited) table into a term/values
# tibble. Same helper as the original 09_visualization_markdown.Rmd — kept
# unchanged since it already worked correctly for b_n/b_r2 there.
# BUGFIX (14/7/2026): `.[. != ""]` used to strip EVERY empty cell from a
# row, not just the leading/trailing ones produced by the line's outer
# pipe characters. modelsummary's SE rows have a genuinely blank term
# cell (coefficient on one row with a real label, SE directly below with
# an empty label) -- stripping that blank cell shifted the whole row one
# column left, so extract_coef_se()'s "is the next row's term blank"
# check never matched. This is why every SE (and therefore every CI and
# every ci_cell() "NA — check upstream file") failed 100% of the time,
# confirmed against the uploaded 10_artifacts.rds: coefficients parsed
# fine, standard errors were NA/NaN across every single term. Now only
# the first/last cell of a row is dropped when empty (the real pipe-
# boundary artifact), leaving a genuinely blank interior term cell intact.
parse_ms_txt <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  rows  <- lines[str_detect(lines, "\\|") & !str_detect(lines, "^[+=]+$") &
                   !str_detect(lines, "^\\+[-=+]+\\+$")]
  map_dfr(rows, function(r) {
    cells <- str_split(r, "\\|")[[1]] %>% str_trim()
    if (length(cells) >= 2 && cells[1] == "") cells <- cells[-1]
    if (length(cells) >= 2 && cells[length(cells)] == "") cells <- cells[-length(cells)]
    if (length(cells) >= 1) tibble(term = cells[1], values = list(cells[-1])) else NULL
  })
}

# Single-cell lookup (e.g. Num.Obs, R2) — unchanged from the original.
ev <- function(p, l, c = 1) {
  if (is.null(p)) return(NA_character_)
  row <- p %>% filter(str_detect(term, fixed(l)))
  if (nrow(row) == 0 || length(row$values[[1]]) < c) return(NA_character_)
  str_trim(row$values[[1]][c])
}
fb <- function(a, b) if (is.null(a) || is.na(a) || a == "") b else a

# Coefficient/SE pair lookup. modelsummary's plain-text output puts the
# coefficient (with stars) on the row matching the term label, and the SE
# (in parentheses) on the following row, which has a blank term. Not yet
# run against a real generated .txt file — spot-check the first real run
# against the actual file before trusting any chart built from it. If
# parsing fails, coef/se come back NA and the figure/table cell will
# visibly show NA rather than a silently wrong number.
extract_coef_se <- function(parsed, term_pattern, col = 1) {
  if (is.null(parsed)) return(list(coef = NA_real_, se = NA_real_))
  idx <- which(str_detect(parsed$term, term_pattern))
  if (length(idx) == 0) return(list(coef = NA_real_, se = NA_real_))
  i <- idx[1]
  coef_raw <- parsed$values[[i]][col]
  coef <- suppressWarnings(as.numeric(str_remove_all(coef_raw, "\\*|,")))
  se <- NA_real_
  if ((i + 1) <= nrow(parsed) && (is.na(parsed$term[i + 1]) || parsed$term[i + 1] == "")) {
    se_raw <- parsed$values[[i + 1]][col]
    se <- suppressWarnings(as.numeric(str_remove_all(se_raw, "[()]|,")))
  }
  list(coef = coef, se = se)
}

star2 <- function(coef, se) {
  if (is.na(coef) || is.na(se)) return("")
  t <- abs(coef / se)
  if (t > 2.576) "***" else if (t > 1.96) "**" else ""
}
ci_cell <- function(coef, se) {
  if (is.na(coef) || is.na(se)) return("NA — check upstream file")
  st <- star2(coef, se)
  lo <- round(coef - 1.96 * se, 3); hi <- round(coef + 1.96 * se, 3)
  paste0(formatC(coef, format = "f", digits = 3), st,
         "<br><small style='color:#777;font-style:normal'>[", lo, ", ", hi, "]</small>")
}

# File-existence gate — warns loudly and returns FALSE instead of silently
# falling back to an old hardcoded number. This is the whole point of the
# rewrite: a missing upstream file should be visibly broken, not quietly
# plausible.
require_file <- function(path, what) {
  if (!file.exists(path)) {
    warning(paste0("MISSING INPUT for ", what, ": ", path,
                   " — rerun the upstream script before trusting this output."), call. = FALSE)
    return(FALSE)
  }
  TRUE
}
placeholder_plot <- function(msg) {
  ggplot() +
    annotate("text", x = .5, y = .5, label = msg, family = "serif", size = 3.6,
             colour = "#C0312D", lineheight = 1.2) +
    theme_void()
}

# Saves a figure as PNG (locked format, no re-render drift at knit time)
# and returns the path to store in the manifest.
save_fig <- function(plot, name, width, height, dpi = 300) {
  path <- file.path(figs_dir, paste0(name, ".png"))
  ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
  cat("Saved:", path, "\n")
  path
}

#--- Colours & theme ---
# Recovery/outcome triad (col_confirmed/col_offnet/col_notrec) changed
# 28/7/2026 from green/orange/red to blue/orange/grey per Lucy's comment:
# the original pairing put green and red at the two ends of the same
# 3-category scale a reader most needs to tell apart, which is exactly the
# red-green contrast protanopia/deuteranopia (~4.5% of readers) struggle
# with most. Blue-orange-grey keeps three visually distinct categories
# without relying on the red-green channel at all.
col_sig <- "#1D6FA4"; col_nonsig <- "#B0B0B0"; col_strict <- "#BA7517"
pal_state <- c("Kano" = "#1D6FA4", "Katsina" = "#BA7517")
pal_zd <- c("Zero-dose" = "#C0312D", "Vaccinated" = "#5A9FD4")
col_confirmed <- "#1D6FA4"; col_offnet <- "#E69F00"; col_notrec <- "#666666"

# Caption/subtitle grey darkened and enlarged per Prabin's readability
# comment (c49, "grey text bit hard to read") — #888 at size 9 falls below
# WCAG AA contrast on white; #595959 at size 10 clears it while still
# reading as visually secondary to the plot itself.
theme_diss <- function(bs = 12) {
  theme_minimal(base_size = bs) %+replace% theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = bs, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = bs - 1, colour = "#4d4d4d", hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, colour = "#595959", hjust = 0, margin = margin(t = 8), lineheight = 1.15),
    axis.title = element_text(size = bs - 1), axis.text = element_text(size = bs - 1, colour = "#333"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#e5e5e5", linewidth = 0.4),
    panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_blank())
}

# DATHARM audit doc house style — theme_minimal(), sans-serif, matches
# 11_reccs_doc_updated_v7.Rmd's existing chunks exactly (not theme_diss(),
# which is thesis-specific with its serif font). Kept as a separate theme
# rather than reusing theme_diss() so DATHARM figures render identically
# to the hand-typed versions they are replacing.
theme_datharm <- function(bs = 12.5) {
  theme_minimal(base_size = bs) %+replace% theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = bs + 0.5),
    plot.subtitle = element_text(color = "#4d4d4d", size = bs - 1))
}

#----------------------------------------------------------------------------

##############################
# Geospatial helpers          #
# (NEW, 19/7/2026 -- ported  #
# from map_style_helpers.R    #
# after standalone prototype  #
# sign-off: fig_1_1,          #
# fig_weather_maps,           #
# fig_offnetwork_ward_map and #
# fig_ward_residual_map)      #
##############################
# Every function below fixes a bug found and corrected during standalone
# prototyping (see the four fig_*_prototype.R scripts' own headers for the
# full history): NA-safe LGA name matching that rejects, rather than force-
# fits, known wrong-state rows (02_chirps_import_analysis.R's own PCODE
# VERIFICATION FLAG comment); NA_real_ (not bare NA, which is logical, not
# numeric) in the point-sampling fallback; a length-guard on st_sample();
# and a fail-loudly helper for empty results. Nothing here should need to
# change again without a corresponding fix in the prototype scripts.

gadm_cache <- file.path(home, "02_data/03_geodata")
dir.create(gadm_cache, showWarnings = FALSE, recursive = TRUE)

get_nga_boundaries <- function() {
  nga_adm1 <- gadm(country = "NGA", level = 1, path = gadm_cache) %>% st_as_sf()
  nga_adm2 <- gadm(country = "NGA", level = 2, path = gadm_cache) %>% st_as_sf()
  list(adm1 = nga_adm1, adm2 = nga_adm2)
}

strip_lga_suffix <- function(x) x %>% str_remove("(?i)\\s*LGA$") %>% str_trim()

match_lga_name <- function(name, state_name, lgas_sf) {
  if (is.na(name) || is.na(state_name) || name == "") return(NA_character_)
  candidates <- lgas_sf$NAME_2[lgas_sf$NAME_1 == state_name]
  if (length(candidates) == 0) return(NA_character_)
  if (name %in% candidates) return(name)
  dists <- adist(tolower(name), tolower(candidates))[1, ]
  if (all(is.na(dists))) return(NA_character_)
  best_i <- which.min(dists)
  best   <- candidates[best_i]
  best_d <- dists[best_i]
  # Plausibility check, not just "closest available" -- some LGAs are
  # recorded under the WRONG state in the source data (Ungogo/Gabasawa/
  # Kiru/Nassarawa/Madobi/Shanono/Warawa/Wudil, all real Kano LGAs,
  # mislabelled Katsina in the pcode lookup table). A genuine near-match is
  # either very close by edit distance or one name contains the other;
  # anything worse is more likely a wrong-state row than a spelling
  # variant, so it's dropped with a warning instead of guessed.
  substr_ok <- str_detect(tolower(best), fixed(tolower(name))) ||
    str_detect(tolower(name), fixed(tolower(best)))
  if (best_d > 2 && !substr_ok) {
    cat("  NO PLAUSIBLE MATCH for '", name, "' within ", state_name, "'s LGA list ",
        "(closest was '", best, "', edit distance ", best_d, "). Dropping this row.\n", sep = "")
    return(NA_character_)
  }
  cat("  Fuzzy-matched '", name, "' (", state_name, ") -> '", best, "'\n", sep = "")
  best
}

# Cached by (state, LGA, ward): fig_1_1, fig_3_4b and fig_3_7b each plot
# wards drawn from a different source table, so without a shared cache the
# same ward could land at a different point on each map even under an
# identical seed, since the three tables don't feed rows in the same order.
ward_point_cache <- new.env()

sample_point_in_lga <- function(state_name, lga_name_matched, ward, lgas_sf = kk_lgas) {
  na_point <- st_sfc(st_point(c(NA_real_, NA_real_)), crs = st_crs(lgas_sf))
  key <- paste(state_name, lga_name_matched, ward, sep = "|||")
  if (!is.null(ward_point_cache[[key]])) return(ward_point_cache[[key]])
  poly <- lgas_sf %>% filter(NAME_1 == state_name, NAME_2 == lga_name_matched)
  if (nrow(poly) == 0) return(na_point)
  pt <- st_sample(poly, size = 1, type = "random")
  if (length(pt) != 1) return(na_point)
  ward_point_cache[[key]] <- pt
  pt
}

stop_if_empty <- function(x, what) {
  if (nrow(x) == 0) {
    stop(what, " has zero rows after matching/filtering -- nothing to plot.", call. = FALSE)
  }
  invisible(x)
}

bbox_with_buffer <- function(sf_obj, buffer_pct = 0.08) {
  bb <- st_bbox(sf_obj)
  dx <- (bb["xmax"] - bb["xmin"]) * buffer_pct
  dy <- (bb["ymax"] - bb["ymin"]) * buffer_pct
  list(xlim = c(bb["xmin"] - dx, bb["xmax"] + dx), ylim = c(bb["ymin"] - dy, bb["ymax"] + dy))
}

theme_map_diss <- function(base_size = 12) {
  theme_void(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size + 2, hjust = 0.5, margin = margin(b = 8)),
      plot.caption    = element_text(size = base_size - 2.5, colour = "#595959", hjust = 0, margin = margin(t = 10), lineheight = 1.15),
      legend.position = "bottom", legend.box = "horizontal",
      legend.title    = element_text(size = base_size + 0.5, face = "bold"),
      legend.text     = element_text(size = base_size),
      legend.key.size = unit(1.3, "lines"), legend.spacing.x = unit(0.6, "cm"),
      plot.margin     = margin(t = 10, r = 18, b = 8, l = 18)
    )
}

geom_lga_labels <- function(label_df, label_col = "label", seed = 2026) {
  geom_col_name <- attr(label_df, "sf_column")
  list(ggrepel::geom_label_repel(
    # size = 3.6 (~10pt) per Lucy's comment that map labels were unreadable
    # without zooming; was 3.1 (~8.8pt).
    data = label_df, mapping = aes(label = .data[[label_col]], geometry = .data[[geom_col_name]]),
    stat = "sf_coordinates", inherit.aes = FALSE, seed = seed, size = 3.6, family = "serif",
    fontface = "bold", colour = "#222", fill = alpha("white", 0.85), label.size = 0,
    label.padding = unit(0.18, "lines"), box.padding = unit(0.7, "lines"), point.padding = unit(0.3, "lines"),
    min.segment.length = 0.15, segment.colour = "#888", segment.size = 0.3, max.overlaps = 20
  ))
}

# Wider, evenly-spaced legend for binned colour/fill scales -- fixes
# legend numbers rendering smushed together on the heat/UTCI and NDVI/VIM
# panels, and (paired with scale_*_steps()/steps2() below instead of a
# continuous gradient) makes mid-range values on the point maps visually
# distinct from each other, not just from the two extremes of the scale.
steps_guide <- function(barwidth = 6, barheight = 0.45) {
  guide_coloursteps(barwidth = unit(barwidth, "cm"), barheight = unit(barheight, "cm"), show.limits = TRUE)
}

# Boundary fetch is a network call, not a local file -- wrapped so a
# network hiccup at knit time degrades to placeholders for the 4 map
# figures instead of halting this entire ~1750-line script.
map_boundaries_loaded <- tryCatch({
  bounds_geo <- get_nga_boundaries()
  kk_states  <- bounds_geo$adm1 %>% filter(NAME_1 %in% c("Kano", "Katsina"))
  kk_lgas    <- bounds_geo$adm2 %>% filter(NAME_1 %in% c("Kano", "Katsina"))
  TRUE
}, error = function(e) {
  warning("Could not fetch GADM boundaries (network/geodata issue) -- all 4 map figures ",
          "(fig_1_1, fig_3_4b, fig_3_7b, fig_weather_maps) will fall back to placeholders: ",
          conditionMessage(e), call. = FALSE)
  FALSE
})

# Seeded once here, not per-figure -- ward_point_cache above means each
# ward is only actually sampled the first time it's encountered (fig_1_1
# runs first), so one seed covers all three ward-point maps.
set.seed(2026)

#----------------------------------------------------------------------------

suppressWarnings({
  for (loc in c("English", "en_US.UTF-8", "en_GB.UTF-8", "en_US", "C")) {
    if (!inherits(try(Sys.setlocale("LC_TIME", loc), silent = TRUE), "try-error")) {
      if (Sys.getlocale("LC_TIME") != "") break
    }
  }
})

artifacts <- list()          # thesis figures/tables
artifacts_datharm <- list()  # DATHARM audit doc figures/tables

# Figure titles are no longer baked into the saved PNGs (16/7/2026 — see
# note at top of Part 1). Each figure's suggested "Figure X.X.  Title text"
# is captured here instead, keyed by the same name used in artifacts$*_path,
# and exported as 10_figure_titles.rds. The Rmd draft applies these (or
# overrides them) as knitr chunk captions, so titling, renumbering, and
# reordering figures during drafting no longer requires rerunning this
# script — only actually changing what a figure SHOWS does.
fig_titles <- list()

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#  PART 1 — THESIS FIGURES AND TABLES
#  Unchanged from 09_visualizations.R except fig_2_2 and fig_3_2, both of
#  which now read from 09_data_investigations.R instead of using a manual
#  constant / an incomplete residual category. See each section for detail.
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

########################################
# Model A (zero-dose) — dynamic coefs #
########################################

ma_txt_path <- file.path(reg_dir, "01_model_a_zerodose_predictors.txt")
ma_parsed   <- if (require_file(ma_txt_path, "Model A coefficients")) parse_ms_txt(ma_txt_path) else NULL

a1_dist_raw <- extract_coef_se(ma_parsed, "HF distance", col = 1)  # Primary (penta-ZD)
a2_dist_raw <- extract_coef_se(ma_parsed, "HF distance", col = 2)  # Truly ZD (strict)
a1_age_raw  <- extract_coef_se(ma_parsed, "Age at registration", col = 1)
a2_age_raw  <- extract_coef_se(ma_parsed, "Age at registration", col = 2)
a1_sex_raw  <- extract_coef_se(ma_parsed, "^Female", col = 1)
a2_sex_raw  <- extract_coef_se(ma_parsed, "^Female", col = 2)

a1_dist <- ci_cell(a1_dist_raw$coef, a1_dist_raw$se); a2_dist <- ci_cell(a2_dist_raw$coef, a2_dist_raw$se)
a1_age  <- ci_cell(a1_age_raw$coef,  a1_age_raw$se);  a2_age  <- ci_cell(a2_age_raw$coef,  a2_age_raw$se)
a1_sex  <- ci_cell(a1_sex_raw$coef,  a1_sex_raw$se);  a2_sex  <- ci_cell(a2_sex_raw$coef,  a2_sex_raw$se)

a_n  <- fb(ev(ma_parsed, "Num.Obs", 1), "NA — check 01_model_a_zerodose_predictors.txt")
a_r2 <- fb(ev(ma_parsed, "R2", 1), "NA")
a_r2_strict <- fb(ev(ma_parsed, "R2", 2), "NA")

cat("--- Model A coefficient extraction check ---\n")
cat("  Primary distance:", a1_dist_raw$coef, "(se", a1_dist_raw$se, ")\n")
cat("  Strict distance:  ", a2_dist_raw$coef, "(se", a2_dist_raw$se, ")\n")
cat("  If these show NA, parse_ms_txt()/extract_coef_se() need adjusting\n")
cat("  against the real column layout of the txt file before proceeding.\n\n")

########################################
# Model B (recovery) — dynamic coefs  #
########################################

mb_robust_path <- file.path(reg_dir, "02_model_b_tracing_effectiveness.txt")
mb_robust      <- if (require_file(mb_robust_path, "Model B robustness table")) parse_ms_txt(mb_robust_path) else NULL

b1_sms_raw  <- extract_coef_se(mb_robust, "SMS / phone contact", col = 1)
b1_dist_raw <- extract_coef_se(mb_robust, "HF distance", col = 1)
b1_age_raw  <- extract_coef_se(mb_robust, "Age at tracing", col = 1)
b1_lag_raw  <- extract_coef_se(mb_robust, "days_since_visit|Days since", col = 1)

b1_sms  <- ci_cell(b1_sms_raw$coef,  b1_sms_raw$se)
b1_dist <- ci_cell(b1_dist_raw$coef, b1_dist_raw$se)
b1_age  <- ci_cell(b1_age_raw$coef,  b1_age_raw$se)
b1_lag  <- ci_cell(b1_lag_raw$coef,  b1_lag_raw$se)

b_n  <- fb(ev(mb_robust, "Num.Obs", 1), "NA")
b_r2 <- fb(ev(mb_robust, "R2", 1), "NA")

# Table 3.1b — full sample vs lag-time subset. Reads the DEDICATED file
# 03_regression.R (-> 04_regression.R once renumbered) produces
# (03_table_3_1b_full_vs_lagtime.txt, from the 13/7/2026 fix adding
# m_b0_full). If this file is missing, it means that script has not been
# rerun with the fix — do NOT fall back to duplicating the robustness-table
# numbers, that IS the bug being fixed.
tab31b_path <- file.path(reg_dir, "03_table_3_1b_full_vs_lagtime.txt")
tab31b_ok   <- require_file(tab31b_path, "Table 3.1b full-sample vs lag-time subset")
tab31b_parsed <- if (tab31b_ok) parse_ms_txt(tab31b_path) else NULL

b_full_sms_raw  <- extract_coef_se(tab31b_parsed, "SMS / phone contact", col = 1)
b_full_dist_raw <- extract_coef_se(tab31b_parsed, "HF distance", col = 1)
b_full_age_raw  <- extract_coef_se(tab31b_parsed, "Age at tracing", col = 1)
b_lag_sms_raw   <- extract_coef_se(tab31b_parsed, "SMS / phone contact", col = 2)
b_lag_dist_raw  <- extract_coef_se(tab31b_parsed, "HF distance", col = 2)
b_lag_age_raw   <- extract_coef_se(tab31b_parsed, "Age at tracing", col = 2)
b_lag_lag_raw   <- extract_coef_se(tab31b_parsed, "Days since last visit", col = 2)

b_full_n  <- fb(ev(tab31b_parsed, "Num.Obs", 1), "NA")
b_full_r2 <- fb(ev(tab31b_parsed, "R2", 1), "NA")
b_lag_n   <- fb(ev(tab31b_parsed, "Num.Obs", 2), "NA")
b_lag_r2  <- fb(ev(tab31b_parsed, "R2", 2), "NA")

if (tab31b_ok && !is.na(b_full_n) && !is.na(b_lag_n) && b_full_n == b_lag_n) {
  warning("Table 3.1b: Full sample N equals Lag-time subset N — this is the ",
          "exact symptom of the original bug. Check that 03_regression.R's ",
          "m_b0_full was actually rebuilt without days_since_visit.", call. = FALSE)
}

#----------------------------------------------------------------------------

########################################
# Figure 1.1 — MCHTrack footprint map  #
# (NEW, 19/7/2026 -- ported from        #
# fig_1_1_map_prototype.R after         #
# standalone sign-off)                  #
########################################
# Ward-level footprint: one random point per ward (NOT true facility
# coordinates -- illustrative of coverage/density only, see the standalone
# prototype's own header note), sized by enrolled children. Rimi LGA is
# INCLUDED here even though it's excluded from the regression analytic
# sample elsewhere (rimi_flag/backfill, II.A) -- this figure describes
# MCHTrack's overall operating footprint, not the analytic sample.

ll_path_map <- file.path(mch_dir, "01_linelisted_clean.rds")
if (map_boundaries_loaded && require_file(ll_path_map, "Figure 1.1 footprint map")) {
  ward_counts_11 <- readRDS(ll_path_map) %>%
    filter(woman_or_child == "child") %>%
    group_by(state, lga_name, facility_ward) %>%
    summarise(enrolled_n = n(), .groups = "drop") %>%
    filter(!is.na(lga_name), !is.na(facility_ward), enrolled_n > 0) %>%
    mutate(lga_clean = strip_lga_suffix(lga_name)) %>%
    rowwise() %>%
    mutate(lga_matched = match_lga_name(lga_clean, state, kk_lgas)) %>%
    ungroup()
  
  ward_pts_11 <- ward_counts_11 %>%
    rowwise() %>%
    mutate(geometry = list(sample_point_in_lga(state, lga_matched, facility_ward))) %>%
    ungroup()
  
  ward_counts_11_sf <- st_as_sf(
    ward_pts_11 %>% select(state, lga_name, facility_ward, enrolled_n, lga_matched),
    geometry = do.call(c, ward_pts_11$geometry), crs = st_crs(kk_lgas)
  ) %>% filter(!is.na(st_coordinates(.)[, 1]))
  
  if (nrow(ward_counts_11_sf) == 0) {
    warning("Figure 1.1: no wards could be placed after LGA matching -- check the fuzzy-match log above.", call. = FALSE)
    fig_1_1 <- placeholder_plot("NO WARDS MATCHED\nsee console log for LGA name-matching issues")
  } else {
    top_lgas_11 <- ward_counts_11_sf %>% st_drop_geometry() %>%
      group_by(state, lga_matched) %>% summarise(total_n = sum(enrolled_n), .groups = "drop") %>%
      group_by(state) %>% slice_max(total_n, n = 3) %>% ungroup()
    
    labels_11 <- kk_lgas %>% filter(NAME_2 %in% top_lgas_11$lga_matched) %>% st_centroid() %>%
      left_join(top_lgas_11, by = c("NAME_1" = "state", "NAME_2" = "lga_matched")) %>%
      filter(!is.na(total_n)) %>%
      mutate(label = paste0(NAME_2, " (", comma(total_n), ")"))
    
    # Cropped to the actual ward points plotted, not the full Kano/Katsina
    # state outline -- MCHTrack operates in a subset of wards within each
    # state, so a state-wide extent left large stretches of empty map with
    # no data in them.
    extent_11 <- bbox_with_buffer(ward_counts_11_sf, 0.12)
    
    fig_1_1 <- ggplot() +
      geom_sf(data = bounds_geo$adm1, fill = "#F2F2F2", colour = "white", linewidth = 0.15) +
      geom_sf(data = kk_states, fill = "#EAF1F8", colour = "#10243B", linewidth = 0.5) +
      geom_sf(data = kk_lgas, fill = NA, colour = "#9AA7B4", linewidth = 0.2) +
      geom_sf(data = ward_counts_11_sf, aes(size = enrolled_n, colour = state), alpha = 0.55) +
      geom_lga_labels(labels_11, "label") +
      scale_colour_manual(values = pal_state, name = "State") +
      scale_size_continuous(name = "Enrolled children", range = c(1.5, 13), labels = comma) +
      coord_sf(xlim = extent_11$xlim, ylim = extent_11$ylim, expand = FALSE, clip = "off") +
      guides(colour = guide_legend(override.aes = list(size = 5))) +
      theme_map_diss(12)
  }
} else {
  fig_1_1 <- placeholder_plot("MISSING INPUT\n01_linelisted_clean.rds, or GADM boundaries unavailable")
}

fig_titles[["fig_1_1"]] <- "Figure 1.1.  MCHTrack's ward-level footprint across Kano and Katsina"
artifacts$fig_1_1_path <- save_fig(fig_1_1, "fig_1_1", width = 9.5, height = 8.2, dpi = 150)

#----------------------------------------------------------------------------

########################################
# Figure 2.1 — MCHTrack pipeline       #
# (illustrative schematic — no data    #
# dependency, unchanged from original) #
########################################

boxes <- tibble(
  label   = c("Household\nenumeration", "Zero-dose\nflag", "Facility\nvisit", "Defaulter\ntracing"),
  dataset = c("linelisted", "identifiedZD", "facility_visits", "defaulterTracing"),
  x       = c(1, 3, 5, 7))

fig_2_1 <- ggplot() +
  annotate("rect", xmin = 0.45, xmax = 7.75, ymin = -0.55, ymax = 0.55,
           fill = "#EAF2FB", colour = "#1D6FA4", linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 7.68, y = 0.47, label = "MCHTrack system boundary",
           hjust = 1, size = 3.4, colour = "#1D6FA4", fontface = "italic", family = "serif") +
  geom_rect(data = boxes, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -0.32, ymax = 0.32),
            fill = "white", colour = "#333", linewidth = 0.6) +
  geom_text(data = boxes, aes(x = x, y = 0.1, label = label),
            size = 3.6, fontface = "bold", lineheight = 0.9, family = "serif") +
  geom_text(data = boxes, aes(x = x, y = -0.18, label = dataset),
            size = 3.0, colour = "#555", fontface = "italic", family = "serif") +
  annotate("segment", x = c(1.55, 3.55, 5.55), xend = c(2.45, 4.45, 6.45), y = 0, yend = 0,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed"), colour = "#333", linewidth = 0.5) +
  annotate("text", x = 1, y = -0.72, label = "~1 in 5 households\nnot reached in enumeration",
           size = 3.0, colour = "#C0312D", fontface = "italic", family = "serif") +
  annotate("segment", x = 1, xend = 1, y = -0.32, yend = -0.5, colour = "#C0312D", linewidth = 0.4,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("text", x = 7, y = -0.75, label = "Off-network care recorded\nverbally, cannot be verified",
           size = 3.0, colour = "#BA7517", fontface = "italic", family = "serif") +
  annotate("segment", x = 7, xend = 7, y = -0.32, yend = -0.5, colour = "#BA7517", linewidth = 0.4,
           linetype = "dotted", arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  scale_x_continuous(limits = c(0.4, 8.0)) + scale_y_continuous(limits = c(-0.95, 0.62)) +
  theme_void() +
  theme(plot.title = element_text(family = "serif", face = "bold", size = 12, margin = margin(b = 6, l = 2)))

fig_titles[["fig_2_1"]] <- "Figure 2.1.  MCHTrack data pipeline and its structural blind spots"
artifacts$fig_2_1_path <- save_fig(fig_2_1, "fig_2_1", width = 8.6, height = 4.2)

########################################
# Figure 2.2 — Duplicate record rates  #
# NOW DYNAMIC. Previously a MANUAL     #
# CONSTANT (22.1% / 38.9%) with no     #
# source in 01-08. Reads              #
# 09_dedup_summary_by_state.rds from   #
# 09_data_investigations.R instead.    #
########################################

# CHANGED (14/7/2026): a single pct_duplicate bar per state/table is
# uninformative for Kano, which is 0% throughout — that comparison tells
# the reader nothing about what the duplication actually looks like.
# Replaced with the same set-size distribution used in DATHARM's fig1a
# (how many rows come from a record synced 2x, 3x, 8x, 82x...), faceted
# by state so Kano's cleanliness is shown directly (empty panel) rather
# than collapsed into a single flat number.

dedup_summary_path <- file.path(inv_dir, "09_dedup_summary_by_state.rds")
dedup_dist_path    <- file.path(inv_dir, "09_dedup_set_size_distribution.rds")

if (require_file(dedup_dist_path, "Figure 2.2 duplicate set-size distribution")) {
  dedup_dist_22 <- readRDS(dedup_dist_path) %>%
    mutate(table = factor(table, levels = c("Linelisted (children enrolled)", "Facility visits (vaccination records)")),
           state = factor(state, levels = c("Kano", "Katsina")))
  
  # CHANGED 28/7/2026 per Lucy's comment: the bar height was previously
  # rows_from_duplication (total excess rows, n_sets * set_size), which
  # over-counts by conflating "how many records were duplicated" with "how
  # many times". Switched to n_sets -- the number of distinct records/
  # children affected -- which is the more intuitive quantity and matches
  # what the caption and Section II.A prose actually report (record counts,
  # not row counts).
  fig_2_2 <- ggplot(dedup_dist_22,
                    aes(x = fct_reorder(set_size_label, suppressWarnings(as.numeric(set_size_label)), .na_rm = FALSE),
                        y = n_sets, fill = table)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62) +
    facet_wrap(~state) +
    scale_fill_manual(values = c("Linelisted (children enrolled)" = "#3498db",
                                 "Facility visits (vaccination records)" = "#e74c3c")) +
    scale_y_continuous(labels = comma) +
    labs(x = "Number of times a record appears (duplicate set size)",
         y = "Distinct records affected", fill = NULL) +
    theme_diss(11) +
    theme(panel.grid.major.x = element_blank())
} else {
  fig_2_2 <- placeholder_plot("MISSING INPUT\n09_dedup_set_size_distribution.rds\n(run 09_data_investigations.R)")
}

fig_titles[["fig_2_2"]] <- "Figure 2.2.  Duplicate record set sizes by state and MCHTrack table"
artifacts$fig_2_2_path <- save_fig(fig_2_2, "fig_2_2", width = 9, height = 3.8)

########################################
# Figure 2.2b — Data reliability by    #
# state (NEW, 16/7/2026)               #
########################################
# Motivated by Lucy's comments (c205/c206, c252) on why Katsina shows
# greater ward-level heterogeneity and a different off-network share than
# Kano, and by the state-scope decision (RQ3 = Kano-only) that currently
# lives only in prose. Shows monthly immunisation-visit volume by state
# across the full study window, with the point where Katsina's reliability
# breaks down (2025-09-30, the same cutoff already applied as a hard filter
# in 01_mchtrack_import.R Section 9/10's in_window logic) marked directly
# on the chart rather than just asserted in text.

lga_panel_path <- file.path(mch_dir, "01_panel_lga_month.rds")
if (require_file(lga_panel_path, "Figure 2.2b data reliability by state")) {
  reliability_monthly <- readRDS(lga_panel_path) %>%
    mutate(year_month_date = as.Date(year_month_date)) %>%
    group_by(state, year_month, year_month_date) %>%
    summarise(imm_visits = sum(imm_visits, na.rm = TRUE), .groups = "drop")
  
  katsina_cutoff <- as.Date("2025-09-30")
  
  fig_2_2b <- ggplot(reliability_monthly, aes(x = year_month_date, y = imm_visits, colour = state)) +
    annotate("rect", xmin = katsina_cutoff, xmax = max(reliability_monthly$year_month_date),
             ymin = -Inf, ymax = Inf, fill = "#C0312D", alpha = 0.06) +
    geom_vline(xintercept = katsina_cutoff, linetype = "dashed", colour = "#C0312D", linewidth = 0.4) +
    annotate("text", x = katsina_cutoff, y = max(reliability_monthly$imm_visits, na.rm = TRUE),
             label = "Katsina excluded from weather\nmodel beyond this point", hjust = -0.05, vjust = 1,
             size = 3.4, colour = "#C0312D", fontface = "italic", family = "serif") +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.3) +
    scale_colour_manual(values = pal_state) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Immunisation visits", colour = NULL) +
    theme_diss(11)
} else {
  fig_2_2b <- placeholder_plot("MISSING INPUT\n01_panel_lga_month.rds\n(run 01_mchtrack_import.R)")
}

fig_titles[["fig_2_2b"]] <- "Figure 2.2b.  Data reliability by state — monthly immunisation visit volume"
artifacts$fig_2_2b_path <- save_fig(fig_2_2b, "fig_2_2b", width = 9, height = 4.2)

########################################
# Figure 2.3 — Overdispersion          #
########################################

vm_path <- file.path(era_dir, "06_panel_daily.rds")
if (require_file(vm_path, "Figure 2.3 overdispersion")) {
  pj_23 <- readRDS(vm_path)
  vm <- pj_23 %>% group_by(lga_clean) %>%
    summarise(Mean = mean(n_visits, na.rm = TRUE), Variance = var(n_visits, na.rm = TRUE), .groups = "drop") %>%
    mutate(ratio = round(Variance / Mean, 1))
  vm_long <- vm %>% pivot_longer(c(Mean, Variance), names_to = "stat", values_to = "value")
  
  fig_2_3 <- ggplot(vm_long, aes(x = lga_clean, y = value, fill = stat)) +
    geom_col(position = position_dodge(0.6), width = 0.5, alpha = 0.9) +
    geom_text(data = vm, aes(x = lga_clean, y = Variance, label = paste0("V/M = ", ratio)),
              inherit.aes = FALSE, vjust = -0.5, size = 3.4, fontface = "bold",
              colour = "#333", family = "serif") +
    scale_fill_manual(values = c("Mean" = "#1D6FA4", "Variance" = "#C0312D")) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = "Daily visits") +
    theme_diss(11)
} else {
  fig_2_3 <- placeholder_plot("MISSING INPUT\n06_panel_daily.rds")
}

fig_titles[["fig_2_3"]] <- "Figure 2.3.  Overdispersion in daily facility visit counts"
artifacts$fig_2_3_path <- save_fig(fig_2_3, "fig_2_3", width = 7.5, height = 3.6)

########################################
# Table 2.1 — Baseline characteristics #
########################################

bl_path <- file.path(reg_dir, "03_model_a_dataset.rds")
if (require_file(bl_path, "Table 2.1 baseline characteristics")) {
  bl <- readRDS(bl_path) %>% filter(in_primary_sample)
  
  bl_row <- function(df, label) {
    tibble(
      Site                          = label,
      N                             = comma(nrow(df)),
      `Female (%)`                  = paste0(round(mean(df$gender_female, na.rm = TRUE) * 100, 1), "%"),
      `Age at registration, months` = paste0(round(median(df$age_months_at_reg, na.rm = TRUE), 1), " (",
                                             round(quantile(df$age_months_at_reg, .25, na.rm = TRUE), 1), "–",
                                             round(quantile(df$age_months_at_reg, .75, na.rm = TRUE), 1), ")"),
      `Distance to facility, km`    = paste0(round(median(df$hf_distance_km, na.rm = TRUE), 2), " (",
                                             round(quantile(df$hf_distance_km, .25, na.rm = TRUE), 2), "–",
                                             round(quantile(df$hf_distance_km, .75, na.rm = TRUE), 2), ")"),
      `Enrolment window`            = paste0(format(min(df$registration_date, na.rm = TRUE), "%b %Y"), "–",
                                             format(max(df$registration_date, na.rm = TRUE), "%b %Y")),
      `Zero-dose (%)`               = paste0(round(mean(df$zero_dose_penta, na.rm = TRUE) * 100, 1), "%")
    )
  }
  
  tab_2_1 <- bind_rows(
    bl_row(bl %>% filter(state == "Kano"),    "Kano"),
    bl_row(bl %>% filter(state == "Katsina"), "Katsina"),
    bl_row(bl,                                "Overall")
  )
} else {
  tab_2_1 <- tibble(Site = "MISSING INPUT", N = "03_model_a_dataset.rds")
}

fig_titles[["tab_2_1"]] <- "Table 2.1.  Baseline characteristics of enrolled children, by site (median, IQR in parentheses)"
artifacts$tab_2_1 <- tab_2_1

########################################
# Figure 2.4 — Baseline distributions  #
########################################

if (require_file(bl_path, "Figure 2.4 baseline distributions")) {
  # CHANGED 28/7/2026 per Lucy: 1x3 layout -> 2x2 with one shared legend
  # (guide_area(), 4th cell) instead of a legend repeated on every panel;
  # text sizes bumped to >=10pt; subtitles trimmed to bare A/B/C tags, all
  # other descriptive text moved to the Rmd caption.
  p_age24 <- ggplot(bl %>% filter(age_months_at_reg <= 60), aes(x = age_months_at_reg, fill = state)) +
    geom_histogram(bins = 40, position = "identity", alpha = 0.6, colour = NA) +
    scale_fill_manual(values = pal_state) +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "A. Age at registration", x = "Age at registration (months)", y = "Children", fill = NULL) +
    theme_diss(12)
  
  p_dist24 <- ggplot(bl %>% filter(hf_distance_km <= 5), aes(x = hf_distance_km, fill = state)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.6, colour = NA) +
    scale_fill_manual(values = pal_state) +
    scale_x_continuous(labels = label_number(suffix = " km")) +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "B. Distance to health facility", x = "Distance (km)", y = "Children", fill = NULL) +
    theme_diss(12)
  
  p_time24 <- ggplot(bl, aes(x = registration_date, fill = state)) +
    geom_histogram(bins = 30, position = "identity", alpha = 0.6, colour = NA) +
    scale_fill_manual(values = pal_state) +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "C. Enrolment over time", x = NULL, y = "Children", fill = NULL) +
    theme_diss(12)
  
  fig_2_4 <- (p_age24 + p_dist24 + p_time24 + guide_area()) +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(
      theme = theme(legend.position = "bottom", legend.text = element_text(family = "serif", size = 11)))
} else {
  fig_2_4 <- placeholder_plot("MISSING INPUT\n03_model_a_dataset.rds")
}

fig_titles[["fig_2_4"]] <- "Figure 2.4.  Baseline characteristics by site — age, distance, and enrolment timing"
artifacts$fig_2_4_path <- save_fig(fig_2_4, "fig_2_4", width = 9, height = 7.5)

#----------------------------------------------------------------------------

########################################
# Figure 3.1 — Predictor distributions #
########################################

ma_path <- file.path(reg_dir, "03_model_a_dataset.rds")
if (require_file(ma_path, "Figure 3.1 predictor distributions")) {
  pm <- readRDS(ma_path) %>% filter(in_primary_sample) %>%
    mutate(zd = if_else(zero_dose_penta == 1, "Zero-dose", "Vaccinated"),
           zd = factor(zd, levels = c("Zero-dose", "Vaccinated")))
  n_fig31 <- nrow(pm)
  
  pdist <- ggplot(pm %>% filter(hf_distance_km <= 5), aes(x = hf_distance_km, fill = zd)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.6, colour = NA) +
    scale_fill_manual(values = pal_zd, name = NULL) +
    scale_x_continuous(labels = label_number(suffix = " km")) +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "A. Distance to health facility", x = "Distance (km)", y = "Children") +
    theme_diss(12)
  page31 <- ggplot(pm %>% filter(age_months_at_reg <= 60), aes(x = age_months_at_reg, fill = zd)) +
    geom_histogram(bins = 40, position = "identity", alpha = 0.6, colour = NA) +
    scale_fill_manual(values = pal_zd, name = NULL) +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "B. Age at registration", x = "Age at registration (months)", y = NULL) +
    theme_diss(12)
  
  fig_3_1 <- (pdist | page31) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
} else {
  fig_3_1 <- placeholder_plot("MISSING INPUT\n03_model_a_dataset.rds")
  n_fig31 <- NA
}

fig_titles[["fig_3_1"]] <- "Figure 3.1.  Predictor distributions by zero-dose status"
artifacts$fig_3_1_path <- save_fig(fig_3_1, "fig_3_1", width = 8.4, height = 4.2)

########################################
# Table 3.1a — Zero-dose model summary #
########################################

tab_3_1a <- tribble(
  ~term, ~c1, ~c2,
  "Distance to health facility (km)", a1_dist, a2_dist,
  "Age at registration (months)",     a1_age,  a2_age,
  "Female (ref: male)",               a1_sex,  a2_sex,
  "<em>N</em>",                       a_n,     a_n,
  "<em>R²</em>",                 a_r2,    a_r2_strict
)
artifacts$tab_3_1a <- tab_3_1a

########################################
# Figure 3.2 — Sample construction     #
# waterfall                            #
# UPDATED (16/7/2026): duplicate rows  #
# are now a real waterfall step, not a #
# caption note. 01_mchtrack_import.R   #
# dedupes at source (15/7/2026 patch), #
# so the pre-dedup raw count and the   #
# removed-row count both genuinely     #
# exist and genuinely feed into        #
# n_final now — they belong in the     #
# chart itself. Null-LGA is still a    #
# separate, NOT-yet-applied filter     #
# (03_regression.R doesn't drop it),   #
# so that one stays caption-only.      #
########################################

ll_path <- file.path(mch_dir, "01_linelisted_clean.rds")
null_lga_path <- file.path(inv_dir, "09_null_lga_summary.rds")
dedup_import_path <- file.path(mch_dir, "01_dedup_summary.rds")

if (require_file(ll_path, "Figure 3.2 sample waterfall") && require_file(ma_path, "Figure 3.2 sample waterfall") &&
    require_file(dedup_import_path, "Figure 3.2 pre-dedup counts")) {
  data_ll_raw <- readRDS(ll_path)
  n_all     <- nrow(data_ll_raw)   # already deduplicated, per 01's 15/7/2026 patch
  n_start   <- data_ll_raw %>% filter(woman_or_child == "child") %>% nrow()
  n_dist_ok <- data_ll_raw %>% filter(woman_or_child == "child") %>%
    left_join(readRDS(file.path(mch_dir, "01_any_vaccine_flag.rds")),
              by = c("pseudo_id" = "patient_id")) %>%
    filter(!is.na(hf_distance_km)) %>% nrow()
  n_rimi_ok <- readRDS(ma_path) %>% filter(!is.na(hf_distance_km), !rimi_flag) %>% nrow()
  n_final   <- readRDS(ma_path) %>% filter(in_primary_sample) %>% nrow()
  
  # Pre-dedup counts, captured in 01_mchtrack_import.R right before its own
  # distinct() step collapses them (same source as Figure 2.2's set-size
  # distribution).
  dedup_ll_import <- readRDS(dedup_import_path) %>% filter(table == "linelisted")
  n_raw          <- dedup_ll_import$rows_before
  n_dup_removed  <- dedup_ll_import$duplicates_removed
  
  # Reconciliation checks, not plotted bars.
  dedup_gap <- (n_raw - n_dup_removed) - n_all
  if (abs(dedup_gap) > 0) {
    warning("Fig 3.2: post-dedup linelisted count (", comma(n_all), ") does not match ",
            "01_dedup_summary.rds's rows_before - duplicates_removed (", comma(n_raw - n_dup_removed),
            "). Either 01 hasn't been rerun since its latest patch, or the two counts are ",
            "now out of sync — investigate before trusting this chart.", call. = FALSE)
  }
  reconciliation_gap <- n_rimi_ok - n_final
  if (abs(reconciliation_gap) > 0.01 * n_rimi_ok) {
    warning("Fig 3.2: reconciliation_gap (", comma(reconciliation_gap), ") exceeds 1% of ",
            "n_rimi_ok — an exclusion step in 03_regression.R is not accounted for in this ",
            "waterfall. Investigate before trusting the chart.", call. = FALSE)
  }
  
  # Null-LGA is the one remaining exclusion 03_regression.R does NOT yet
  # apply — kept as a caption-only "known, not yet excluded" note rather
  # than a bar, since including it as a subtracted step would double-count
  # against a total (n_final) that doesn't actually reflect it.
  null_lga_n <- if (require_file(null_lga_path, "Figure 3.2 Null-LGA count")) {
    readRDS(null_lga_path)$n_null_lga
  } else NA_integer_
  
  wf <- tribble(
    ~step, ~label, ~n, ~type,
    1, "Raw linelisted\n(pre-dedup, both states)", n_raw, "start",
    2, "Duplicate rows\nremoved at import", n_dup_removed, "exclude",
    3, "Women excluded\n(not a child record)", n_all - n_start, "exclude",
    4, "Missing / implausible\ndistance", n_start - n_dist_ok, "exclude",
    5, "Rimi LGA\n(backfill)", n_dist_ok - n_rimi_ok, "exclude",
    6, "Primary analytic\nsample", n_final, "end"
  ) %>%
    mutate(delta = case_when(type == "start" ~ n, type == "end" ~ 0, TRUE ~ -n),
           remaining = cumsum(delta))
  
  # This caveat (Null-LGA rows not yet excluded from the sample shown) is
  # substantive, not decorative, so it is exported for the Rmd's external
  # caption rather than baked into the plot image itself.
  known_issues_lab <- paste0(
    comma(null_lga_n), " Null-LGA rows are not yet excluded from the primary sample shown here. ",
    "If applied, the primary sample would fall to approximately ",
    comma(n_final - coalesce(null_lga_n, 0L)), "."
  )
  artifacts$fig_3_2_known_issues <- known_issues_lab
  
  fig_3_2 <- ggplot(wf, aes(x = step)) +
    geom_rect(aes(xmin = step - 0.4, xmax = step + 0.4,
                  ymin = if_else(type == "exclude", remaining, 0),
                  ymax = if_else(type == "exclude", remaining + n, remaining),
                  fill = type), colour = "white", linewidth = 0.3) +
    geom_text(data = ~subset(.x, type != "exclude"),
              aes(y = remaining / 2, label = comma(n)), size = 2.9, fontface = "bold",
              colour = "white", family = "serif") +
    geom_text(data = ~subset(.x, type == "exclude" & n > 0),
              aes(y = remaining + n + max(wf$n) * 0.06, label = paste0("−", comma(n))),
              size = 2.6, fontface = "bold", colour = "#BA7517", family = "serif") +
    geom_text(aes(y = -max(wf$remaining, na.rm = TRUE) * 0.05, label = label), size = 2.15, lineheight = 0.9,
              colour = "#333", family = "serif") +
    # Colourblind-safe triad (blue / amber / navy) replaces the previous
    # blue / red / green, which paired a red "exclude" bar against a green
    # "end" bar -- the one combination protanopia/deuteranopia readers
    # struggle to tell apart.
    scale_fill_manual(values = c("start" = "#1D6FA4", "exclude" = "#BA7517", "end" = "#10243B"),
                      labels = c("start" = "Starting N", "exclude" = "Excluded", "end" = "Analytic sample"),
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = "Row count") +
    theme_diss(12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
} else {
  fig_3_2 <- placeholder_plot("MISSING INPUT\n01_linelisted_clean.rds, 01_dedup_summary.rds or 03_model_a_dataset.rds")
}

fig_titles[["fig_3_2"]] <- "Figure 3.2.  Analytic sample construction — zero-dose model"
artifacts$fig_3_2_path <- save_fig(fig_3_2, "fig_3_2", width = 8.6, height = 4.6)

########################################
# Figure 3.2b — Sample construction,   #
# split by state (duplicates/Null-LGA  #
# land very differently on Kano vs     #
# Katsina; a pooled chart hides that)  #
########################################

null_lga_state_path <- file.path(inv_dir, "09_null_lga_by_state.rds")
dedup_state_path <- file.path(mch_dir, "01_dedup_summary_by_state.rds")
if (require_file(ll_path, "Figure 3.2b sample waterfall") && require_file(ma_path, "Figure 3.2b sample waterfall") &&
    require_file(null_lga_state_path, "Figure 3.2b Null-LGA by state") &&
    require_file(dedup_state_path, "Figure 3.2b duplicate count by state")) {
  
  ma_all <- readRDS(ma_path)
  vax_flag <- readRDS(file.path(mch_dir, "01_any_vaccine_flag.rds"))
  null_lga_state <- readRDS(null_lga_state_path)
  dedup_state <- readRDS(dedup_state_path) %>% filter(table == "linelisted")
  
  build_state_waterfall <- function(st) {
    ll_st <- data_ll_raw %>% filter(state == st)
    n_all_st   <- nrow(ll_st)   # already deduplicated
    n_start_st <- ll_st %>% filter(woman_or_child == "child") %>% nrow()
    n_dist_st  <- ll_st %>% filter(woman_or_child == "child") %>%
      left_join(vax_flag, by = c("pseudo_id" = "patient_id")) %>%
      filter(!is.na(hf_distance_km)) %>% nrow()
    n_rimi_st  <- ma_all %>% filter(state == st, !is.na(hf_distance_km), !rimi_flag) %>% nrow()
    n_final_st <- ma_all %>% filter(state == st, in_primary_sample) %>% nrow()
    
    dedup_st   <- dedup_state %>% filter(state == st)
    n_raw_st   <- if (nrow(dedup_st) == 1) dedup_st$rows_before else NA_integer_
    n_dup_st   <- if (nrow(dedup_st) == 1) dedup_st$duplicates_removed else NA_integer_
    
    tribble(
      ~step, ~label, ~n, ~type,
      1, "Raw linelisted\n(pre-dedup)", n_raw_st, "start",
      2, "Duplicate rows\nremoved at import", n_dup_st, "exclude",
      3, "Women excluded", n_all_st - n_start_st, "exclude",
      4, "Missing / implausible\ndistance", n_start_st - n_dist_st, "exclude",
      5, "Rimi LGA\n(backfill)", n_dist_st - n_rimi_st, "exclude",
      6, "Primary analytic\nsample", n_final_st, "end"
    ) %>%
      mutate(state = st,
             delta = case_when(type == "start" ~ n, type == "end" ~ 0, TRUE ~ -n),
             remaining = cumsum(delta))
  }
  
  wf_state <- bind_rows(build_state_waterfall("Kano"), build_state_waterfall("Katsina")) %>%
    mutate(state = factor(state, levels = c("Kano", "Katsina")))
  
  # Null-LGA remains the one not-yet-applied filter (03_regression.R
  # doesn't drop it), so it's still caption-only. Duplicates are now a real
  # bar in the chart above, sourced from 01_dedup_summary_by_state.rds —
  # this is exactly where the Kano/Katsina asymmetry Khem flagged shows up
  # (Kano's duplicate bar should be at or near zero; Katsina's should not).
  not_yet_lab <- null_lga_state %>%
    mutate(lab = paste0(state, ": ", comma(n_null_lga), " Null-LGA rows (not yet excluded)")) %>%
    pull(lab) %>% paste(collapse = "  ·  ")
  artifacts$fig_3_2b_not_yet <- not_yet_lab
  
  fig_3_2b <- ggplot(wf_state, aes(x = step)) +
    geom_rect(aes(xmin = step - 0.4, xmax = step + 0.4,
                  ymin = if_else(type == "exclude", remaining, 0),
                  ymax = if_else(type == "exclude", remaining + n, remaining),
                  fill = type), colour = "white", linewidth = 0.3) +
    geom_text(data = ~subset(.x, type != "exclude"),
              aes(y = remaining / 2, label = comma(n)), size = 2.6, fontface = "bold",
              colour = "white", family = "serif") +
    geom_text(data = ~subset(.x, type == "exclude" & n > 0),
              aes(y = remaining + n, label = paste0("−", comma(n))),
              vjust = -0.6, size = 2.3, fontface = "bold", colour = "#BA7517", family = "serif") +
    geom_text(aes(y = 0, label = label), vjust = 1.8, size = 1.95, lineheight = 0.85,
              colour = "#333", family = "serif") +
    facet_wrap(~state, scales = "free_y") +
    # Same colourblind-safe triad as Figure 3.2 -- see note there.
    scale_fill_manual(values = c("start" = "#1D6FA4", "exclude" = "#BA7517", "end" = "#10243B"),
                      labels = c("start" = "Starting N", "exclude" = "Excluded", "end" = "Analytic sample"),
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.15, 0.15))) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = "Row count") +
    theme_diss(12) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 12))
} else {
  fig_3_2b <- placeholder_plot("MISSING INPUT\nsee Figure 3.2b requirements")
}

fig_titles[["fig_3_2b"]] <- "Figure 3.2b.  Analytic sample construction, by state"
artifacts$fig_3_2b_path <- save_fig(fig_3_2b, "fig_3_2b", width = 9.4, height = 4.8)

########################################
# Figure 3.3 — ZD model coefficients   #
########################################

coef_a <- tribble(~label, ~estimate, ~se,
                  "Distance to\nhealth facility (km)", a1_dist_raw$coef, a1_dist_raw$se,
                  "Age at registration\n(months)",     a1_age_raw$coef,  a1_age_raw$se,
                  "Female\n(ref: male)",               a1_sex_raw$coef,  a1_sex_raw$se) %>%
  mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
         label = fct_rev(fct_inorder(label)))

pA33 <- ggplot(coef_a, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.15, linewidth = 0.9, colour = col_sig) +
  geom_point(shape = 21, size = 3.6, stroke = 0.8, colour = "white", fill = col_sig) +
  labs(subtitle = "A. Primary specification (all predictors)", x = "Coefficient (log-odds), 95% CI", y = NULL) +
  theme_diss(12) + theme(axis.text.y = element_text(lineheight = 0.9))

coef_b33 <- tribble(~definition, ~estimate, ~se,
                    "Primary\n(penta-ZD flag)",    a1_dist_raw$coef, a1_dist_raw$se,
                    "Strict\n(no vaccine at all)", a2_dist_raw$coef, a2_dist_raw$se) %>%
  mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
         definition = fct_rev(fct_inorder(definition)))

amplification <- round(a2_dist_raw$coef / a1_dist_raw$coef, 1)

pB33 <- ggplot(coef_b33, aes(x = estimate, y = definition)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.12, linewidth = 1.0, colour = col_strict) +
  geom_point(shape = 21, size = 4.4, stroke = 0.9, colour = "white", fill = col_strict) +
  labs(subtitle = "B. Distance coefficient by zero-dose definition",
       x = "Coefficient on distance (log-odds), 95% CI", y = NULL) +
  theme_diss(12) + theme(axis.text.y = element_text(lineheight = 0.9))

fig_3_3 <- (pA33 / pB33)

fig_titles[["fig_3_3"]] <- "Figure 3.3.  Zero-dose model — primary specification and definitional amplification"
artifacts$fig_3_3_path <- save_fig(fig_3_3, "fig_3_3", width = 8.4, height = 5.4)

########################################
# Figure 3.4 — Ward-level residuals    #
########################################

wr_path <- file.path(resid_dir, "04_ward_residuals_classified.csv")
if (require_file(wr_path, "Figure 3.4 ward residuals")) {
  wr <- read_csv(wr_path, show_col_types = FALSE)
  dup_lgas <- c("Funtua LGA", "Mani LGA", "Safana LGA", "Batagarawa LGA", "Katsina LGA", "Baure LGA")
  # fct_reorder() is used nowhere below: it errors ("idx must contain one
  # integer for each level of f") whenever two rows produce an identical
  # ward_lab string, which a residuals file built on data with any
  # remaining duplicate/near-duplicate rows can easily trigger. arrange()
  # + factor(levels = unique(...)) sorts the same way but never depends on
  # every label being unique to a single row.
  wr_top <- wr %>%
    group_by(state) %>% slice_max(residual, n = 6) %>% ungroup() %>%
    mutate(ward_lab = paste0(facility_ward, "  (", str_remove(lga_name, " LGA"), ", ", state, ")"),
           provisional = state == "Katsina" & lga_name %in% dup_lgas,
           flag_lab = if_else(provisional, "Katsina, duplication-affected list", "Not flagged")) %>%
    arrange(residual) %>%
    mutate(ward_lab = factor(ward_lab, levels = unique(ward_lab)))
  
  fig_3_4 <- ggplot(wr_top, aes(x = residual, y = ward_lab)) +
    geom_segment(aes(x = 0, xend = residual, y = ward_lab, yend = ward_lab), colour = "#ddd", linewidth = 0.5) +
    geom_point(aes(fill = flag_lab, size = n_children), shape = 21, stroke = 0.7, colour = "white") +
    geom_text(aes(x = residual + 1.4, label = paste0("+", formatC(residual, format = "f", digits = 1), " pp")),
              hjust = 0, size = 2.9, family = "serif", colour = "#555") +
    scale_fill_manual(values = c("Katsina, duplication-affected list" = "#C0312D", "Not flagged" = "#1D6FA4")) +
    scale_size_continuous(range = c(2.5, 7), guide = "none") +
    scale_x_continuous(labels = label_number(suffix = " pp")) +
    labs(x = "Residual above model prediction (percentage points)", y = NULL) +
    theme_diss(12) + theme(axis.text.y = element_text(size = 10))
} else {
  fig_3_4 <- placeholder_plot("MISSING INPUT\n04_ward_residuals_classified.csv")
}

fig_titles[["fig_3_4"]] <- "Figure 3.4.  Ward-level residuals from the zero-dose model"
artifacts$fig_3_4_path <- save_fig(fig_3_4, "fig_3_4", width = 8.6, height = 6.0)

########################################
# Figure 3.4b — Ward residual map      #
# (NEW, 19/7/2026 -- ported from        #
# fig_ward_residual_map_prototype.R)    #
########################################
# Geographic companion to Figure 3.4's ranked list: IV.C's argument is that
# Katsina's largest residuals should cluster in its least reliable LGAs
# while Kano's look flat everywhere, a pattern a ranked list can state but
# not show. Deliberately NOT a targeting map (same caution as III.A.ii/
# IV.C): a large residual is a candidate for field verification, not a
# confirmed allocation priority.

resid_classified_path <- file.path(resid_dir, "04_ward_residuals_classified.rds")
if (map_boundaries_loaded && require_file(resid_classified_path, "Figure 3.4b ward residual map")) {
  data_ward_resid_34b <- readRDS(resid_classified_path) %>%
    filter(!is.na(lga_name), !is.na(facility_ward)) %>%
    # n_ward is n_children (the real exported column). zd_count/non_zd_count
    # are transient columns computed only inside 04_ward_residuals.R's own
    # Plot D pipeline and are NOT saved to this file.
    mutate(lga_clean = strip_lga_suffix(lga_name), n_ward = n_children) %>%
    rowwise() %>%
    mutate(lga_matched = match_lga_name(lga_clean, state, kk_lgas)) %>%
    ungroup()
  
  ward_pts_34b <- data_ward_resid_34b %>%
    rowwise() %>%
    mutate(geometry = list(sample_point_in_lga(state, lga_matched, facility_ward))) %>%
    ungroup()
  
  resid_sf_34b <- st_as_sf(
    ward_pts_34b %>% select(state, lga_name, facility_ward, residual, resid_tier, n_ward, lga_matched),
    geometry = do.call(c, ward_pts_34b$geometry), crs = st_crs(kk_lgas)
  ) %>% filter(!is.na(st_coordinates(.)[, 1]))
  
  if (nrow(resid_sf_34b) == 0) {
    warning("Figure 3.4b: no wards could be placed after LGA matching -- check the fuzzy-match log above.", call. = FALSE)
    fig_3_4b <- placeholder_plot("NO WARDS MATCHED\nsee console log for LGA name-matching issues")
  } else {
    top_resid_lgas_34b <- resid_sf_34b %>% st_drop_geometry() %>%
      group_by(state, lga_matched) %>%
      summarise(mean_resid = mean(residual, na.rm = TRUE), n_wards = n(), .groups = "drop") %>%
      filter(n_wards >= 2) %>% slice_max(mean_resid, n = 3)
    
    labels_34b <- kk_lgas %>% filter(NAME_2 %in% top_resid_lgas_34b$lga_matched) %>% st_centroid() %>%
      left_join(top_resid_lgas_34b, by = c("NAME_1" = "state", "NAME_2" = "lga_matched")) %>%
      filter(!is.na(mean_resid)) %>%
      mutate(label = paste0(NAME_2, " (+", round(mean_resid, 1), "pp)"))
    
    # Cropped to the wards actually plotted rather than the full state
    # outline, for the same reason as Figure 1.1's extent above.
    extent_34b <- bbox_with_buffer(resid_sf_34b, 0.12)
    resid_range_34b <- max(abs(resid_sf_34b$residual), na.rm = TRUE)
    
    # Larger points drawn first so smaller ones sit on top and are not
    # hidden underneath -- z-order otherwise follows row order, which is
    # arbitrary. Points get a white outline (shape 21) so mid-range,
    # near-zero residuals stay visible against the pale basemap rather
    # than blending into a grey-on-grey wash.
    resid_sf_34b <- resid_sf_34b %>% arrange(desc(n_ward))
    
    fig_3_4b <- ggplot() +
      geom_sf(data = bounds_geo$adm1, fill = "#EDEDED", colour = "white", linewidth = 0.15) +
      geom_sf(data = kk_states, fill = "#FFFFFF", colour = "#10243B", linewidth = 0.5) +
      geom_sf(data = kk_lgas, fill = NA, colour = "#9AA7B4", linewidth = 0.2) +
      geom_sf(data = resid_sf_34b, aes(size = n_ward, fill = residual), shape = 21,
              colour = "white", stroke = 0.35, alpha = 0.9) +
      geom_lga_labels(labels_34b, "label") +
      # Binned diverging scale, not a continuous gradient -- otherwise
      # anything short of the most extreme residuals reads as the same
      # washed-out pale grey (a ward at +2pp and one at +8pp become nearly
      # indistinguishable). Steps keep every band visibly separated.
      # Midpoint fixed at zero, with the white point outline above providing
      # additional contrast against the basemap for near-zero residuals.
      scale_fill_steps2(low = "#1D6FA4", mid = "#D9D9D9", high = "#C0312D", midpoint = 0,
                        limits = c(-resid_range_34b, resid_range_34b),
                        breaks = scales::breaks_pretty(n = 6)(c(-resid_range_34b, resid_range_34b)),
                        name = "Residual (observed\nminus predicted ZD, pp)",
                        guide = steps_guide(barwidth = 6.5)) +
      scale_size_continuous(name = "Children in\nward (N)", range = c(1.5, 11), labels = comma) +
      coord_sf(xlim = extent_34b$xlim, ylim = extent_34b$ylim, expand = FALSE, clip = "off") +
      theme_map_diss(12)
  }
} else {
  fig_3_4b <- placeholder_plot("MISSING INPUT\n04_ward_residuals_classified.rds, or GADM boundaries unavailable")
}

fig_titles[["fig_3_4b"]] <- "Figure 3.4b.  Ward-level zero-dose model residuals, mapped"
artifacts$fig_3_4b_path <- save_fig(fig_3_4b, "fig_3_4b", width = 9.5, height = 8.4, dpi = 150)

#----------------------------------------------------------------------------

########################################
# Figure 3.5 — Tracing outcomes/timing #
########################################

mb_path <- file.path(reg_dir, "03_model_b_dataset.rds")
if (require_file(mb_path, "Figure 3.5 tracing outcomes")) {
  d35 <- readRDS(mb_path) %>% filter(in_primary_sample, !is.na(method_sms))
  method_out <- d35 %>% mutate(
    method = if_else(method_sms == 1, "SMS / phone", "Home visit"),
    outcome = case_when(
      tracing_outcome == "yes_ok" ~ "Confirmed (in-network)",
      tracing_outcome == "yes_off_network_care" ~ "Off-network (verbal)",
      TRUE ~ "Not recovered"),
    outcome = factor(outcome, levels = c("Confirmed (in-network)", "Off-network (verbal)", "Not recovered"))) %>%
    count(state, method, outcome) %>% group_by(state, method) %>% mutate(pct = n / sum(n) * 100) %>% ungroup()
  
  lag_d <- d35 %>% filter(!is.na(days_since_visit), days_since_visit <= 300) %>%
    mutate(rec = if_else(recovered_strict == 1, "Recovered", "Not recovered"))
  
  pL35 <- ggplot(method_out, aes(x = method, y = pct, fill = outcome)) +
    geom_col(width = 0.62, colour = "white", linewidth = 0.3) +
    geom_text(aes(label = if_else(pct > 6, paste0(round(pct), "%"), "")),
              position = position_stack(vjust = 0.5), size = 2.7, colour = "white",
              fontface = "bold", family = "serif") +
    scale_fill_manual(values = c("Confirmed (in-network)" = col_confirmed,
                                 "Off-network (verbal)" = col_offnet, "Not recovered" = col_notrec)) +
    scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.02))) +
    facet_wrap(~state) +
    labs(subtitle = "A. Tracing outcome by contact method", x = NULL, y = "Share of attempts") +
    theme_diss(12)
  
  lag_meds <- lag_d %>% group_by(rec) %>% summarise(med = median(days_since_visit, na.rm = TRUE), .groups = "drop")
  # Median labels are anchored to a fixed point in the plot's empty upper-right
  # corner rather than to each vline's own x-position, which is what caused
  # them to sit on top of the histogram bars when a median fell in the dense
  # part of the distribution.
  lag_lab_x <- max(lag_d$days_since_visit, na.rm = TRUE) * 0.97
  pR35 <- ggplot(lag_d, aes(x = days_since_visit, fill = rec)) +
    geom_histogram(bins = 38, position = "identity", alpha = 0.55, colour = NA) +
    geom_vline(data = lag_meds, aes(xintercept = med, colour = rec), linetype = "dashed", linewidth = 0.8) +
    geom_text(data = lag_meds, aes(colour = rec, label = paste0(rec, ": median ", med, " d")),
              x = lag_lab_x, y = Inf, vjust = c(1.6, 3.4), hjust = 1, size = 2.9,
              family = "serif", fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("Recovered" = col_confirmed, "Not recovered" = col_notrec)) +
    scale_colour_manual(values = c("Recovered" = col_confirmed, "Not recovered" = col_notrec), guide = "none") +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "B. Days since last visit, by recovery", x = "Days since last facility visit", y = "Attempts") +
    theme_diss(12)
  
  fig_3_5 <- (pL35 | pR35) + plot_layout(widths = c(1.25, 1))
  fig_titles[["fig_3_5"]] <- "Figure 3.5.  Tracing outcomes and timing"
} else {
  fig_3_5 <- placeholder_plot("MISSING INPUT\n03_model_b_dataset.rds")
}

artifacts$fig_3_5_path <- save_fig(fig_3_5, "fig_3_5", width = 8.6, height = 5.4)

########################################
# Table 3.1b — Full sample vs lag-time #
########################################

tab_3_1b <- tribble(~term, ~c3, ~c4,
                    "SMS / phone contact (ref: home visit)", ci_cell(b_full_sms_raw$coef, b_full_sms_raw$se), ci_cell(b_lag_sms_raw$coef, b_lag_sms_raw$se),
                    "Distance to health facility (km)",      ci_cell(b_full_dist_raw$coef, b_full_dist_raw$se), ci_cell(b_lag_dist_raw$coef, b_lag_dist_raw$se),
                    "Age at tracing (months)",               ci_cell(b_full_age_raw$coef, b_full_age_raw$se), ci_cell(b_lag_age_raw$coef, b_lag_age_raw$se),
                    "Days since last facility visit",        "n/a — not in full-sample spec", ci_cell(b_lag_lag_raw$coef, b_lag_lag_raw$se),
                    "<em>N</em>",                            b_full_n, b_lag_n,
                    "<em>R²</em>",                      b_full_r2, b_lag_r2)
artifacts$tab_3_1b <- tab_3_1b

########################################
# Figure 3.6 — Recovery model coefs    #
########################################

coef_r36 <- tribble(~label, ~estimate, ~se,
                    "SMS / phone contact\n(ref: home visit)", b1_sms_raw$coef,  b1_sms_raw$se,
                    "Distance to\nhealth facility (km)",      b1_dist_raw$coef, b1_dist_raw$se,
                    "Age at tracing\n(months)",               b1_age_raw$coef,  b1_age_raw$se,
                    "Days since\nlast visit",                 b1_lag_raw$coef,  b1_lag_raw$se) %>%
  mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
         label = fct_rev(fct_inorder(label)))

fig_3_6 <- ggplot(coef_r36, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.16, linewidth = 0.9, colour = "#BA7517") +
  geom_point(shape = 21, size = 3.8, stroke = 0.8, colour = "white", fill = "#BA7517") +
  labs(x = "Coefficient (log-odds scale), 95% CI", y = NULL) +
  theme_diss(12) + theme(axis.text.y = element_text(lineheight = 0.9))

fig_titles[["fig_3_6"]] <- "Figure 3.6.  Recovery model — coefficient plot"
artifacts$fig_3_6_path <- save_fig(fig_3_6, "fig_3_6", width = 8, height = 3.8)

########################################
# Figure 3.7 — Strict vs permissive    #
# recovery by state                    #
########################################

if (require_file(mb_path, "Figure 3.7 strict vs permissive recovery")) {
  rec_data <- readRDS(mb_path) %>% filter(in_primary_sample) %>%
    group_by(state) %>%
    summarise(Strict = mean(recovered_strict, na.rm = TRUE) * 100,
              Permissive = mean(recovered_permissive, na.rm = TRUE) * 100, .groups = "drop") %>%
    pivot_longer(c(Strict, Permissive), names_to = "definition", values_to = "pct") %>%
    mutate(definition = factor(definition, levels = c("Strict", "Permissive")))
  
  gaps37 <- rec_data %>% group_by(state) %>%
    summarise(s = pct[definition == "Strict"], p = pct[definition == "Permissive"],
              gap = p - s, ymid = (s + p) / 2, .groups = "drop")
  
  fig_3_7 <- ggplot(rec_data, aes(x = definition, y = pct, fill = definition)) +
    geom_col(width = 0.55, alpha = 0.92) +
    geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 3.4, fontface = "bold", family = "serif") +
    geom_segment(data = gaps37, aes(x = 2.42, xend = 2.42, y = s, yend = p), inherit.aes = FALSE,
                 colour = "#444", linewidth = 0.5, arrow = arrow(ends = "both", length = unit(0.07, "cm"))) +
    geom_text(data = gaps37, aes(x = 2.52, y = ymid, label = paste0("+", round(gap, 1), " pp")),
              inherit.aes = FALSE, hjust = 0, size = 3.0, family = "serif", colour = "#444") +
    scale_fill_manual(values = c("Strict" = col_notrec, "Permissive" = col_offnet), guide = "none") +
    scale_y_continuous(limits = c(0, 100), labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(expand = expansion(add = c(0.6, 0.95))) +
    facet_wrap(~state) +
    labs(x = NULL, y = "Recovery rate") +
    theme_diss(12)
} else {
  fig_3_7 <- placeholder_plot("MISSING INPUT\n03_model_b_dataset.rds")
}

fig_titles[["fig_3_7"]] <- "Figure 3.7.  Strict versus permissive recovery rate by state"
artifacts$fig_3_7_path <- save_fig(fig_3_7, "fig_3_7", width = 8, height = 4.2)

########################################
# Figure 3.7b — Off-network share map  #
# (NEW, 19/7/2026 -- ported from        #
# fig_offnetwork_ward_map_prototype.R)  #
########################################
# Geographic companion to Figure 3.7's strict-vs-permissive gap: answers
# Lucy's comment (c252) on whether the Kano/Katsina difference in
# off-network reporting is a uniform state-level pattern or concentrated
# in particular LGAs.

dt_clean_path_37b <- file.path(mch_dir, "01_defaultertracing_clean.rds")
if (map_boundaries_loaded && require_file(dt_clean_path_37b, "Figure 3.7b off-network map")) {
  ward_offnet_37b <- readRDS(dt_clean_path_37b) %>%
    filter(!is.na(lga_name), !is.na(facility_ward)) %>%
    group_by(state, lga_name, facility_ward) %>%
    summarise(
      n_recovered  = sum(tracing_outcome %in% c("yes_ok", "yes_off_network_care"), na.rm = TRUE),
      n_offnetwork = sum(tracing_outcome == "yes_off_network_care", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_recovered >= 5) %>%
    mutate(offnet_share = round(100 * n_offnetwork / n_recovered, 1),
           lga_clean = strip_lga_suffix(lga_name)) %>%
    rowwise() %>%
    mutate(lga_matched = match_lga_name(lga_clean, state, kk_lgas)) %>%
    ungroup()
  
  ward_pts_37b <- ward_offnet_37b %>%
    rowwise() %>%
    mutate(geometry = list(sample_point_in_lga(state, lga_matched, facility_ward))) %>%
    ungroup()
  
  ward_offnet_37b_sf <- st_as_sf(
    ward_pts_37b %>% select(state, lga_name, facility_ward, n_recovered, n_offnetwork, offnet_share, lga_matched),
    geometry = do.call(c, ward_pts_37b$geometry), crs = st_crs(kk_lgas)
  ) %>% filter(!is.na(st_coordinates(.)[, 1]))
  
  if (nrow(ward_offnet_37b_sf) == 0) {
    warning("Figure 3.7b: no wards could be placed after LGA matching -- check the fuzzy-match log above.", call. = FALSE)
    fig_3_7b <- placeholder_plot("NO WARDS MATCHED\nsee console log for LGA name-matching issues")
  } else {
    top_offnet_37b <- ward_offnet_37b_sf %>% st_drop_geometry() %>%
      group_by(state, lga_matched) %>%
      summarise(n_recovered = sum(n_recovered), n_offnetwork = sum(n_offnetwork), .groups = "drop") %>%
      filter(n_recovered >= 20) %>%
      mutate(share = round(100 * n_offnetwork / n_recovered, 1)) %>%
      slice_max(share, n = 3)
    
    labels_37b <- kk_lgas %>% filter(NAME_2 %in% top_offnet_37b$lga_matched) %>% st_centroid() %>%
      left_join(top_offnet_37b, by = c("NAME_1" = "state", "NAME_2" = "lga_matched")) %>%
      filter(!is.na(share)) %>%
      mutate(label = paste0(NAME_2, " (", share, "%)"))
    
    # Cropped to the wards actually plotted rather than the full state
    # outline, for the same reason as Figure 1.1's extent above.
    extent_37b <- bbox_with_buffer(ward_offnet_37b_sf, 0.12)
    
    fig_3_7b <- ggplot() +
      geom_sf(data = bounds_geo$adm1, fill = "#F2F2F2", colour = "white", linewidth = 0.15) +
      geom_sf(data = kk_states, fill = "#FAFAF7", colour = "#10243B", linewidth = 0.5) +
      geom_sf(data = kk_lgas, fill = NA, colour = "#9AA7B4", linewidth = 0.2) +
      geom_sf(data = ward_offnet_37b_sf, aes(size = n_recovered, colour = offnet_share), alpha = 0.85) +
      geom_lga_labels(labels_37b, "label") +
      # Single-hue viridis scale replaces the earlier green-to-red gradient,
      # which was difficult to read for colourblind viewers and had a hard-
      # to-distinguish midrange (~30-70%).
      scale_colour_viridis_b(option = "viridis", name = "Off-network share\nof recovery (%)",
                             breaks = scales::breaks_pretty(n = 5), labels = label_number(suffix = "%"),
                             guide = steps_guide()) +
      scale_size_continuous(name = "Recovered cases\n(N, ward)", range = c(1.5, 11), labels = comma) +
      coord_sf(xlim = extent_37b$xlim, ylim = extent_37b$ylim, expand = FALSE, clip = "off") +
      theme_map_diss(12)
  }
} else {
  fig_3_7b <- placeholder_plot("MISSING INPUT\n01_defaultertracing_clean.rds, or GADM boundaries unavailable")
}

fig_titles[["fig_3_7b"]] <- "Figure 3.7b.  Off-network share of recovered defaulters, by ward"
artifacts$fig_3_7b_path <- save_fig(fig_3_7b, "fig_3_7b", width = 9.5, height = 8.2, dpi = 150)

########################################
# Figure 3.8 — Daily visit diagnostics #
########################################

pj_path <- file.path(era_dir, "06_panel_daily.rds")
if (require_file(pj_path, "Figure 3.8 daily visit diagnostics")) {
  pj38 <- readRDS(pj_path) %>% mutate(vd = as.Date(visit_date))
  
  hist_means <- pj38 %>% group_by(lga_clean) %>% summarise(mv = mean(n_visits, na.rm = TRUE), .groups = "drop")
  hist_lab_x <- max(pj38$n_visits, na.rm = TRUE) * 0.97
  p_hist <- ggplot(pj38, aes(x = n_visits, fill = lga_clean)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.55, colour = NA) +
    geom_vline(data = hist_means, aes(xintercept = mv, colour = lga_clean), linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
    geom_text(data = hist_means, aes(colour = lga_clean, label = paste0(lga_clean, ": mean ", round(mv), " /day")),
              x = hist_lab_x, y = Inf, vjust = c(1.6, 3.4), hjust = 1, size = 2.9, family = "serif", fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("Gabasawa" = "#1D6FA4", "Ungogo" = "#BA7517"), name = "LGA") +
    scale_colour_manual(values = c("Gabasawa" = "#1D6FA4", "Ungogo" = "#BA7517"), guide = "none") +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "A. Visit count distribution", x = "Daily visits", y = "LGA-days") + theme_diss(12)
  
  dow_lab <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  p_dow <- pj38 %>% group_by(dow_num) %>% summarise(mv = mean(n_visits, na.rm = TRUE), .groups = "drop") %>%
    mutate(dl = factor(dow_lab[dow_num], levels = dow_lab), wk = dow_num %in% c(1, 6, 7)) %>%
    ggplot(aes(x = dl, y = mv, fill = wk)) + geom_col(width = 0.6, alpha = 0.9) +
    scale_fill_manual(values = c("TRUE" = "#C0312D", "FALSE" = "#1D6FA4"),
                      labels = c("TRUE" = "Weekend / Friday", "FALSE" = "Weekday"), name = NULL) +
    labs(subtitle = "B. Mean visits by weekday", x = NULL, y = "Mean visits") + theme_diss(12) +
    theme(axis.text.x = element_text(size = 10))
  
  p_zero <- pj38 %>% mutate(ym = floor_date(vd, "month"), z = as.integer(n_visits == 0)) %>%
    group_by(ym) %>% summarise(zd = sum(z), .groups = "drop") %>%
    ggplot(aes(x = ym, y = zd)) + geom_col(fill = "#BA7517", alpha = 0.8, width = 22) +
    scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
    labs(subtitle = "C. Zero-visit days per month", x = NULL, y = "Zero-visit days") + theme_diss(12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))
  
  fig_3_8 <- (p_hist | p_dow | p_zero)
  fig_titles[["fig_3_8"]] <- "Figure 3.8.  Daily facility visit diagnostics"
} else {
  fig_3_8 <- placeholder_plot("MISSING INPUT\n06_panel_daily.rds")
}

artifacts$fig_3_8_path <- save_fig(fig_3_8, "fig_3_8", width = 9, height = 3.6)

#----------------------------------------------------------------------------

################################################
# Table (weather) — W1-W6, now genuinely real  #
# Reads 02 (precip), 06 (heat), 08 (NDVI)      #
################################################

# NB is primary per Prabin's second-draft review (c84). W2-W6 all have an
# NB counterpart (built in 02/06/08's NB-comparison sections) and are read
# from there; W1 (precip monthly, LGA FE only) never got an NB version in
# 02, so it stays OLS — a specification-building row, not a headline number,
# same treatment it's always had. The primary OLS files are still loaded
# too: the robustness table further down needs the original no-offset OLS
# coefficients as its baseline column.

precip_txt    <- file.path(chirps_dir, "02_regression_precip_visits.txt")
heat_txt      <- file.path(era_dir, "06_regression_daily.txt")
ndvi_txt      <- file.path(ndvi_dir, "08_regression_ndvi_kano.txt")
precip_nb_txt <- file.path(chirps_dir, "02_regression_precip_nb_comparison.txt")
heat_nb_txt   <- file.path(era_dir, "06_regression_nb_comparison.txt")
ndvi_nb_txt   <- file.path(ndvi_dir, "08_regression_ndvi_nb_comparison.txt")

precip_ok    <- require_file(precip_txt, "weather table — precipitation")
heat_ok      <- require_file(heat_txt, "weather table — heat")
ndvi_ok      <- require_file(ndvi_txt, "weather table — NDVI")
precip_nb_ok <- require_file(precip_nb_txt, "NB comparison — precipitation")
heat_nb_ok   <- require_file(heat_nb_txt, "NB comparison — heat")
ndvi_nb_ok   <- require_file(ndvi_nb_txt, "NB comparison — NDVI")

precip_parsed    <- if (precip_ok) parse_ms_txt(precip_txt) else NULL
heat_parsed      <- if (heat_ok) parse_ms_txt(heat_txt) else NULL
ndvi_parsed      <- if (ndvi_ok) parse_ms_txt(ndvi_txt) else NULL
precip_nb_parsed <- if (precip_nb_ok) parse_ms_txt(precip_nb_txt) else NULL
heat_nb_parsed   <- if (heat_nb_ok) parse_ms_txt(heat_nb_txt) else NULL
ndvi_nb_parsed   <- if (ndvi_nb_ok) parse_ms_txt(ndvi_nb_txt) else NULL

# W1: monthly precip anomaly, LGA FE (P1, col 1) — no NB version exists
w1 <- extract_coef_se(precip_parsed, "precip_anomaly_pct", col = 1)
# W2: daily precip anomaly, LGA+DOW+month-year FE (P4_nb, col 1 of NB file)
w2 <- extract_coef_se(precip_nb_parsed, "precip_anomaly_pct", col = 1)
# W3: binary extreme heat, LGA+DOW+month-year FE (D3_nb, col 1 of NB file)
w3 <- extract_coef_se(heat_nb_parsed, "extreme_heat_38", col = 1)
# W4: continuous UTCI, LGA+DOW+month-year FE (D4_nb, col 3 of NB file)
w4 <- extract_coef_se(heat_nb_parsed, "utci_dt_c", col = 3)
# W5: NDVI vim level, LGA FE (N1_nb, col 1 of NB file)
w5 <- extract_coef_se(ndvi_nb_parsed, "vim_c", col = 1)
# W6: NDVI viq anomaly, LGA+month-year FE (N2_nb, col 3 of NB file)
w6 <- extract_coef_se(ndvi_nb_parsed, "viq_c", col = 3)

w1_n <- fb(ev(precip_parsed, "Num.Obs", 1), "NA");    w1_r2 <- fb(ev(precip_parsed, "R2", 1), "NA")
w2_n <- fb(ev(precip_nb_parsed, "Num.Obs", 1), "NA"); w2_r2 <- fb(ev(precip_nb_parsed, "R2", 1), "NA")
w3_n <- fb(ev(heat_nb_parsed, "Num.Obs", 1), "NA");   w3_r2 <- fb(ev(heat_nb_parsed, "R2", 1), "NA")
w4_n <- fb(ev(heat_nb_parsed, "Num.Obs", 3), "NA");   w4_r2 <- fb(ev(heat_nb_parsed, "R2", 3), "NA")
w5_n <- fb(ev(ndvi_nb_parsed, "Num.Obs", 1), "NA");   w5_r2 <- fb(ev(ndvi_nb_parsed, "R2", 1), "NA")
w6_n <- fb(ev(ndvi_nb_parsed, "Num.Obs", 3), "NA");   w6_r2 <- fb(ev(ndvi_nb_parsed, "R2", 3), "NA")

tab_weather <- tribble(~Spec, ~Variable, ~Measure, ~Coef_CI, ~Panel, ~FE, ~N, ~R2,
                       "W1", "Precipitation", "Monthly precipitation anomaly (% of long-term average)", ci_cell(w1$coef, w1$se), "Monthly", "LGA", w1_n, w1_r2,
                       "W2", "Precipitation", "Daily precipitation anomaly (% of long-term average)",   ci_cell(w2$coef, w2$se), "Daily",   "LGA, day-of-week, month-year", w2_n, w2_r2,
                       "W3", "Heat", "Extreme heat day (Universal Thermal Climate Index ≥ 38°C, binary)", ci_cell(w3$coef, w3$se), "Daily",   "LGA, day-of-week, month-year", w3_n, w3_r2,
                       "W4", "Heat", "Daytime mean Universal Thermal Climate Index (°C, centred)", ci_cell(w4$coef, w4$se), "Daily",   "LGA, day-of-week, month-year", w4_n, w4_r2,
                       "W5", "Vegetation", "NDVI seasonal level (centred on multi-year mean)",         ci_cell(w5$coef, w5$se), "Monthly", "LGA", w5_n, w5_r2,
                       "W6", "Vegetation", "NDVI within-baseline anomaly (centred, month-year adjusted)", ci_cell(w6$coef, w6$se), "Monthly", "LGA, month-year", w6_n, w6_r2)
artifacts$tab_weather <- tab_weather

########################################
# Figure 3.10 slot — NB (primary) vs   #
# OLS, all three weather variables     #
########################################

precip_nb_nb  <- extract_coef_se(precip_nb_parsed, "precip_anomaly_pct", col = 1)
precip_ols_nb <- extract_coef_se(precip_nb_parsed, "precip_anomaly_pct", col = 2)
heat_nb_nb    <- extract_coef_se(heat_nb_parsed, "extreme_heat_38", col = 1)
heat_ols_nb   <- extract_coef_se(heat_nb_parsed, "extreme_heat_38", col = 2)
ndvi_nb_nb    <- extract_coef_se(ndvi_nb_parsed, "vim_c", col = 1)
ndvi_ols_nb   <- extract_coef_se(ndvi_nb_parsed, "vim_c", col = 2)

nb_ci <- function(coef, se) {
  if (is.na(coef) || is.na(se)) return("NA")
  paste0("[", round(coef - 1.96 * se, 4), ", ", round(coef + 1.96 * se, 4), "]")
}
nb_sig <- function(coef, se) { s <- star2(coef, se); if (s == "") "n.s." else s }

fig_3_10 <- tribble(~Variable, ~Specification, ~Coef, ~SE, ~CI, ~Sig,
                    "Precipitation", "Negative binomial — counts (primary)", precip_nb_nb$coef, precip_nb_nb$se, nb_ci(precip_nb_nb$coef, precip_nb_nb$se), nb_sig(precip_nb_nb$coef, precip_nb_nb$se),
                    "Precipitation", "OLS — log(visits + 1)",                precip_ols_nb$coef, precip_ols_nb$se, nb_ci(precip_ols_nb$coef, precip_ols_nb$se), nb_sig(precip_ols_nb$coef, precip_ols_nb$se),
                    "Heat",          "Negative binomial — counts (primary)", heat_nb_nb$coef, heat_nb_nb$se, nb_ci(heat_nb_nb$coef, heat_nb_nb$se), nb_sig(heat_nb_nb$coef, heat_nb_nb$se),
                    "Heat",          "OLS — log(visits + 1)",                heat_ols_nb$coef, heat_ols_nb$se, nb_ci(heat_ols_nb$coef, heat_ols_nb$se), nb_sig(heat_ols_nb$coef, heat_ols_nb$se),
                    "NDVI",          "Negative binomial — counts (primary)", ndvi_nb_nb$coef, ndvi_nb_nb$se, nb_ci(ndvi_nb_nb$coef, ndvi_nb_nb$se), nb_sig(ndvi_nb_nb$coef, ndvi_nb_nb$se),
                    "NDVI",          "OLS — log(visits + 1)",                ndvi_ols_nb$coef, ndvi_ols_nb$se, nb_ci(ndvi_ols_nb$coef, ndvi_ols_nb$se), nb_sig(ndvi_ols_nb$coef, ndvi_ols_nb$se))
artifacts$tab_3_10 <- fig_3_10

########################################
# Table (supplementary) — offset +     #
# spline robustness, Prabin 15/7/2026  #
########################################

# Reads the three NEW *_robustness_prabin.txt files added to 02/06/08.
# Deliberately separate from tab_weather/tab_3_10 above — this is a
# robustness check, not a replacement primary spec, and hasn't been
# assigned a table number in the dissertation yet (that's Khem's call once
# it's clear whether any of this actually moves the null finding).
#
# Coverage note: only the three variable/spec pairs that share an identical
# FE structure with an offset/spline counterpart are compared here (the
# richest-FE spec in each script — P4/D3/D4/N4/N2, matching what 02/06/08's
# Section 12b/5c/13b actually built). Spline terms are basis-expanded
# (ns(x, df=3) produces 3 coefficients, not 1), so they can't be reduced to
# a single coefficient/CI cell the way a linear term can — the spline
# column below instead reports whether ANY of the 3 basis terms reaches
# p<0.05, checked via the same t-stat threshold used everywhere else in
# this file (star2()/1.96). Consult the underlying .txt files directly for
# the full spline coefficient set.

precip_rob_txt <- file.path(chirps_dir, "02_regression_precip_robustness_prabin.txt")
heat_rob_txt   <- file.path(era_dir, "06_regression_daily_robustness_prabin.txt")
ndvi_rob_txt   <- file.path(ndvi_dir, "08_regression_ndvi_robustness_prabin.txt")

precip_rob_ok <- require_file(precip_rob_txt, "Weather robustness — precipitation")
heat_rob_ok   <- require_file(heat_rob_txt, "Weather robustness — heat")
ndvi_rob_ok   <- require_file(ndvi_rob_txt, "Weather robustness — NDVI")

precip_rob_parsed <- if (precip_rob_ok) parse_ms_txt(precip_rob_txt) else NULL
heat_rob_parsed   <- if (heat_rob_ok) parse_ms_txt(heat_rob_txt) else NULL
ndvi_rob_parsed   <- if (ndvi_rob_ok) parse_ms_txt(ndvi_rob_txt) else NULL

# Any-spline-term-significant check. Matches on the bare variable name
# (e.g. "precip_anomaly_pct"), which also matches its ns()-wrapped basis
# rows since str_detect() is substring matching — safe here because the
# spline column is blank/NA for the plain-linear-term row (that row only
# has a value in the offset models' columns, not the spline model's own
# column), so it's silently skipped rather than producing a false read.
spline_any_sig <- function(parsed, term_pattern, col) {
  if (is.null(parsed)) return("NA — check upstream file")
  idx <- which(str_detect(parsed$term, term_pattern))
  if (length(idx) == 0) return("NA — check upstream file")
  any_sig <- FALSE
  any_valid <- FALSE
  for (i in idx) {
    coef_raw <- parsed$values[[i]][col]
    coef <- suppressWarnings(as.numeric(str_remove_all(coef_raw, "\\*|,")))
    se <- NA_real_
    if ((i + 1) <= nrow(parsed) && (is.na(parsed$term[i + 1]) || parsed$term[i + 1] == "")) {
      se_raw <- parsed$values[[i + 1]][col]
      se <- suppressWarnings(as.numeric(str_remove_all(se_raw, "[()]|,")))
    }
    if (!is.na(coef) && !is.na(se)) {
      any_valid <- TRUE
      if (abs(coef / se) > 1.96) any_sig <- TRUE
    }
  }
  if (!any_valid) return("NA — check upstream file")
  if (any_sig) "Yes — at least one spline term p<0.05" else "No spline term significant"
}

# OLS-only baselines for the "Original" column — independent of w2/w3/w4/w6
# above, which now hold NB values. Sourced fresh from the primary OLS files.
w2_orig    <- extract_coef_se(precip_parsed, "precip_anomaly_pct", col = 4)  # precip daily, P4
w3_orig    <- extract_coef_se(heat_parsed, "extreme_heat_38", col = 3)       # heat binary, D3
w4_orig    <- extract_coef_se(heat_parsed, "utci_dt_c", col = 4)             # heat continuous, D4
w6_orig    <- extract_coef_se(ndvi_parsed, "viq_c", col = 2)                 # NDVI viq, N2
w5b_orig   <- extract_coef_se(ndvi_parsed, "vim_c", col = 4)  # NDVI vim, N4 (not N1/w5 — different FE)

precip_off    <- extract_coef_se(precip_rob_parsed, "precip_anomaly_pct", col = 2)
precip_nb_off <- extract_coef_se(precip_rob_parsed, "precip_anomaly_pct", col = 3)
precip_spline_sig <- spline_any_sig(precip_rob_parsed, "precip_anomaly_pct", col = 4)

heat_bin_off    <- extract_coef_se(heat_rob_parsed, "extreme_heat_38", col = 1)
heat_bin_nb_off <- extract_coef_se(heat_rob_parsed, "extreme_heat_38", col = 2)
heat_cont_off   <- extract_coef_se(heat_rob_parsed, "utci_dt_c", col = 3)
heat_spline_sig <- spline_any_sig(heat_rob_parsed, "utci_dt_c", col = 4)

ndvi_vim_off    <- extract_coef_se(ndvi_rob_parsed, "vim_c", col = 1)
ndvi_vim_nb_off <- extract_coef_se(ndvi_rob_parsed, "vim_c", col = 2)
ndvi_viq_off    <- extract_coef_se(ndvi_rob_parsed, "viq_c", col = 3)
ndvi_spline_sig <- spline_any_sig(ndvi_rob_parsed, "vim_c", col = 4)

tab_weather_robustness <- tribble(
  ~Variable, ~Original_CI, ~Offset_CI, ~NB_offset_CI, ~Non_linearity_check,
  "Precipitation (daily anomaly)",                ci_cell(w2_orig$coef, w2_orig$se), ci_cell(precip_off$coef, precip_off$se), ci_cell(precip_nb_off$coef, precip_nb_off$se), precip_spline_sig,
  "Heat (extreme-heat-day threshold)",            ci_cell(w3_orig$coef, w3_orig$se), ci_cell(heat_bin_off$coef, heat_bin_off$se), ci_cell(heat_bin_nb_off$coef, heat_bin_nb_off$se), "N/A — binary term, no non-linearity check built",
  "Heat (continuous UTCI)",                       ci_cell(w4_orig$coef, w4_orig$se), ci_cell(heat_cont_off$coef, heat_cont_off$se), "N/A — negative-binomial offset only built for the binary heat threshold", heat_spline_sig,
  "Vegetation (NDVI seasonal level)",             ci_cell(w5b_orig$coef, w5b_orig$se), ci_cell(ndvi_vim_off$coef, ndvi_vim_off$se), ci_cell(ndvi_vim_nb_off$coef, ndvi_vim_nb_off$se), ndvi_spline_sig,
  "Vegetation (NDVI within-baseline anomaly)",    ci_cell(w6_orig$coef, w6_orig$se), ci_cell(ndvi_viq_off$coef, ndvi_viq_off$se), "N/A — negative-binomial offset only built for NDVI seasonal level", "N/A — no non-linearity check built for this term"
)
artifacts$tab_weather_robustness <- tab_weather_robustness

########################################
# Figure — Weather robustness          #
# coefficient plot (NEW, 16/7/2026)    #
# Visual companion to                  #
# tab_weather_robustness above.        #
########################################
# Point estimate + 95% CI across specifications for each weather variable.
# Spline models are NOT plotted as points — ns(x, df=3) has 3 basis
# coefficients, not one, so there is no single point to place on this
# axis (same reasoning as tab_weather_robustness's Spline_check column).
# Whether any spline term reached significance is annotated as text next
# to each panel instead. NB-offset coefficients are on a count-model log
# link, broadly comparable in direction/scale to the OLS-on-log-visits
# specifications but not identical in construction — flagged in the
# caption rather than silently treated as equivalent.

rob_coef_rows <- tribble(
  ~variable, ~spec, ~coef, ~se,
  "Precipitation\n(daily)", "Original",  w2_orig$coef,        w2_orig$se,
  "Precipitation\n(daily)", "Offset",    precip_off$coef,     precip_off$se,
  "Precipitation\n(daily)", "NB-offset", precip_nb_off$coef,  precip_nb_off$se,
  "Heat\n(binary threshold)",         "Original",  w3_orig$coef,        w3_orig$se,
  "Heat\n(binary threshold)",         "Offset",    heat_bin_off$coef,   heat_bin_off$se,
  "Heat\n(binary threshold)",         "NB-offset", heat_bin_nb_off$coef, heat_bin_nb_off$se,
  "Heat\n(continuous UTCI)", "Original", w4_orig$coef,       w4_orig$se,
  "Heat\n(continuous UTCI)", "Offset",   heat_cont_off$coef, heat_cont_off$se,
  "NDVI\n(seasonal level)",     "Original",  w5b_orig$coef,       w5b_orig$se,
  "NDVI\n(seasonal level)",     "Offset",    ndvi_vim_off$coef,   ndvi_vim_off$se,
  "NDVI\n(seasonal level)",     "NB-offset", ndvi_vim_nb_off$coef, ndvi_vim_nb_off$se,
  "NDVI\n(within-baseline anomaly)",   "Original",  w6_orig$coef,        w6_orig$se,
  "NDVI\n(within-baseline anomaly)",   "Offset",    ndvi_viq_off$coef,   ndvi_viq_off$se
) %>%
  mutate(
    ci_lo = coef - 1.96 * se, ci_hi = coef + 1.96 * se,
    spec  = factor(spec, levels = c("NB-offset", "Offset", "Original"))
  ) %>%
  filter(!is.na(coef))

fig_weather_robustness <- ggplot(rob_coef_rows, aes(x = coef, y = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.18, linewidth = 0.9, colour = col_sig) +
  geom_point(size = 2.8, colour = col_sig) +
  facet_wrap(~variable, scales = "free_x", ncol = 2) +
  labs(x = "Coefficient (log-visits scale), 95% CI", y = NULL) +
  theme_diss(12) +
  theme(strip.text = element_text(size = 11, face = "bold"), panel.spacing = unit(1.1, "lines"))

fig_titles[["fig_weather_robustness"]] <- "Figure 3.9.  Weather effect estimates across specifications — offset, NB-offset and spline robustness"
artifacts$fig_weather_robustness_path <- save_fig(fig_weather_robustness, "fig_weather_robustness", width = 10, height = 5.2)

########################################
# Figure 3.9b — Weather variable maps  #
# (NEW, 19/7/2026 -- ported from        #
# fig_weather_maps_prototype.R)         #
########################################
# Spatial companion to Figure 3.9's robustness coefficients: gives the
# null weather result a physical reference (what the landscape looks
# like, not just point estimates). Panel B (heat) is Kano-only by
# construction, not a display choice -- that IS the full extent of the
# ERA5/UTCI panel used in the heat model (06_era5_analysis.R filters to
# Ungogo/Gabasawa before anything else); showing that sparseness plainly
# is more honest than padding the map with LGAs the heat model never used.

attach_lga_polygons_map <- function(df, state_col, lga_col) {
  df <- df %>%
    mutate(state_ = .data[[state_col]], lga_raw_ = .data[[lga_col]]) %>%
    filter(!is.na(state_), !is.na(lga_raw_), lga_raw_ != "") %>%
    mutate(lga_clean_ = strip_lga_suffix(lga_raw_))
  lgas_in_scope <- bounds_geo$adm2 %>% filter(NAME_1 %in% unique(df$state_))
  df <- df %>% rowwise() %>%
    mutate(lga_matched_ = match_lga_name(lga_clean_, state_, lgas_in_scope)) %>%
    ungroup() %>%
    filter(!is.na(lga_matched_))
  lgas_in_scope %>% inner_join(df, by = c("NAME_1" = "state_", "NAME_2" = "lga_matched_"))
}

# NOTE on paths: 02_chirps_data_kk_monthly.rds and 07_ndvi_monthly.rds are
# written by 02_chirps_import_analysis.R / 08_ndvi_analysis.R to their OWN
# import_dir (03_output/02_chirps_data, 03_output/07_ndvi respectively) --
# NOT chirps_dir/ndvi_dir as defined at the top of this script, which point
# at those scripts' separate regression-output directories
# (03_output/02_chirps_analysis, 03_output/08_ndvi_analysis) used for the
# weather regression .txt tables above. Set explicitly here to avoid
# reusing a variable that resolves to the wrong folder.
precip_path_39b <- file.path(home, "03_output/02_chirps_data/02_chirps_data_kk_monthly.rds")
heat_path_39b   <- file.path(era_dir, "06_panel_daily.rds")
ndvi_path_39b   <- file.path(home, "03_output/07_ndvi/07_ndvi_monthly.rds")

weather_maps_ok <- map_boundaries_loaded &&
  require_file(precip_path_39b, "Figure 3.9b rainfall map") &&
  require_file(heat_path_39b, "Figure 3.9b heat map") &&
  require_file(ndvi_path_39b, "Figure 3.9b NDVI map")

if (weather_maps_ok) {
  precip_lga_39b <- readRDS(precip_path_39b) %>%
    group_by(state, lga_name_mchtrack) %>%
    summarise(precip_mm = mean(precip_actual_mm, na.rm = TRUE), .groups = "drop")
  precip_sf_39b <- attach_lga_polygons_map(precip_lga_39b, "state", "lga_name_mchtrack")
  
  heat_lga_39b <- readRDS(heat_path_39b) %>%
    mutate(state = "Kano") %>%
    group_by(state, lga_clean) %>%
    summarise(utci = mean(utci_daytime_mean, na.rm = TRUE), .groups = "drop")
  heat_sf_39b <- attach_lga_polygons_map(heat_lga_39b, "state", "lga_clean")
  
  ndvi_lga_39b <- readRDS(ndvi_path_39b) %>%
    group_by(state, lga_name) %>%
    summarise(vim = mean(vim_monthly, na.rm = TRUE), .groups = "drop")
  ndvi_sf_39b <- attach_lga_polygons_map(ndvi_lga_39b, "state", "lga_name")
  
  if (nrow(precip_sf_39b) == 0 || nrow(heat_sf_39b) == 0 || nrow(ndvi_sf_39b) == 0) {
    warning("Figure 3.9b: at least one weather panel had zero LGAs matched -- check the fuzzy-match log above.", call. = FALSE)
    fig_weather_maps <- placeholder_plot("NO LGAs MATCHED for at least one weather panel\nsee console log")
  } else {
    precip_extent_39b <- bbox_with_buffer(precip_sf_39b, 0.08)
    precip_labels_39b <- precip_sf_39b %>% st_centroid() %>% slice_max(precip_mm, n = 2) %>%
      mutate(label = paste0(NAME_2, " (", round(precip_mm), " mm)"))
    
    p_precip_39b <- ggplot() +
      geom_sf(data = bounds_geo$adm1 %>% filter(NAME_1 %in% unique(precip_sf_39b$NAME_1)),
              fill = "#F7F7F7", colour = "white", linewidth = 0.15) +
      geom_sf(data = precip_sf_39b, aes(fill = precip_mm), colour = "white", linewidth = 0.25) +
      geom_lga_labels(precip_labels_39b, "label") +
      scale_fill_steps(low = "#F5EFE0", high = "#1D6FA4", name = "Mean monthly\nrainfall (mm)",
                       breaks = scales::breaks_pretty(n = 5), labels = comma, guide = steps_guide()) +
      coord_sf(xlim = precip_extent_39b$xlim, ylim = precip_extent_39b$ylim, expand = FALSE, clip = "off") +
      labs(subtitle = "A. Rainfall (CHIRPS)") +
      theme_map_diss(12) + theme(plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13))
    
    # Cropped to Ungogo and Gabasawa specifically -- the previous version
    # used all of Kano's ~44 LGAs as the extent basis, even though the
    # ERA5/UTCI panel this figure draws on covers only these two.
    heat_extent_39b <- bbox_with_buffer(heat_sf_39b, 0.15)
    heat_labels_39b <- heat_sf_39b %>% st_centroid() %>%
      mutate(label = paste0(NAME_2, " (", round(utci, 1), "°C)"))
    
    p_heat_39b <- ggplot() +
      geom_sf(data = bounds_geo$adm2 %>% filter(NAME_1 == "Kano"), fill = "#F7F7F7", colour = "white", linewidth = 0.15) +
      geom_sf(data = heat_sf_39b, aes(fill = utci), colour = "white", linewidth = 0.25) +
      geom_lga_labels(heat_labels_39b, "label") +
      # FIX: legend numbers were previously smushed together on this panel --
      # binned scale (one label per discrete box) + accuracy=0.1 breaks +
      # a wider guide bar (steps_guide()) instead of a continuous colourbar.
      scale_fill_steps(low = "#FCE9C9", high = "#C0312D", name = "Mean daytime\nUTCI (°C)",
                       breaks = scales::breaks_pretty(n = 4), labels = label_number(accuracy = 0.1),
                       guide = steps_guide()) +
      coord_sf(xlim = heat_extent_39b$xlim, ylim = heat_extent_39b$ylim, expand = FALSE, clip = "off") +
      labs(subtitle = "B. Heat (ERA5/UTCI)") +
      theme_map_diss(12) + theme(plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13))
    
    ndvi_extent_39b <- bbox_with_buffer(ndvi_sf_39b, 0.08)
    ndvi_labels_39b <- ndvi_sf_39b %>% st_centroid() %>% slice_max(vim, n = 2) %>%
      mutate(label = paste0(NAME_2, " (", round(vim, 3), ")"))
    
    p_ndvi_39b <- ggplot() +
      geom_sf(data = bounds_geo$adm1 %>% filter(NAME_1 %in% unique(ndvi_sf_39b$NAME_1)),
              fill = "#F7F7F7", colour = "white", linewidth = 0.15) +
      geom_sf(data = ndvi_sf_39b, aes(fill = vim), colour = "white", linewidth = 0.25) +
      geom_lga_labels(ndvi_labels_39b, "label") +
      # Same legend fix as the heat panel -- NDVI values are small decimals
      # that previously crowded together under a continuous colourbar.
      scale_fill_steps(low = "#F1E9D2", high = "#2E7D32", name = "Mean vegetation\nindex (NDVI)",
                       breaks = scales::breaks_pretty(n = 4), labels = label_number(accuracy = 0.01),
                       guide = steps_guide()) +
      coord_sf(xlim = ndvi_extent_39b$xlim, ylim = ndvi_extent_39b$ylim, expand = FALSE, clip = "off") +
      labs(subtitle = "C. Vegetation greenness (NDVI)") +
      theme_map_diss(12) + theme(plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 13))
    
    # 2x2-style grid (one cell left empty) rather than a single cramped row
    # of three, so each map gets more room -- panels are wide, low-aspect-
    # ratio choropleths that lose legibility when squeezed side by side.
    fig_weather_maps <- (p_precip_39b + p_heat_39b + p_ndvi_39b) + plot_layout(ncol = 2)
  }
} else {
  fig_weather_maps <- placeholder_plot("MISSING INPUT\nsee Figure 3.9b requirements, or GADM boundaries unavailable")
}

fig_titles[["fig_weather_maps"]] <- "Figure 3.9b.  Weather variables across Kano and Katsina's LGAs"
artifacts$fig_weather_maps_path <- save_fig(fig_weather_maps, "fig_weather_maps", width = 11, height = 10, dpi = 150)

#----------------------------------------------------------------------------

########################################
# Table 4.1 — Synthesis                #
########################################

strict_pooled <- if (require_file(mb_path, "Table 4.1")) {
  d41 <- readRDS(mb_path) %>% filter(in_primary_sample)
  round(mean(d41$recovered_strict, na.rm = TRUE) * 100, 1)
} else NA
permissive_pooled <- if (!is.na(strict_pooled)) round(mean(d41$recovered_permissive, na.rm = TRUE) * 100, 1) else NA

a2_dist_sig_label <- if (!is.na(a2_dist_raw$coef) && !is.na(a2_dist_raw$se) &&
                         abs(a2_dist_raw$coef / a2_dist_raw$se) > 2.576) {
  "highly significant"
} else {
  "significant"
}

# BUGFIX (14/7/2026): the previous version of this tribble supplied only
# 2 values per row (Theme, Finding) against 3 declared columns
# (~Theme, ~Finding, ~Field). tribble() only checks that the value count
# is a multiple of the column count, so 6 values / 3 columns silently
# parsed as 2 misaligned rows instead of erroring — "Recordkeeping" ended
# up in the Field column of row 1, and the Timing row was dropped
# entirely. Rebuilt below with all 4 themes and a real Field value each.

age_sig_label_41 <- if (!is.na(a1_age_raw$coef) && !is.na(a1_age_raw$se) &&
                        abs(a1_age_raw$coef / a1_age_raw$se) > 1.96) {
  "predicts ZD status (p < 0.05)"
} else {
  "does not reach significance for ZD status"
}

lag_medians_41 <- if (require_file(mb_path, "Table 4.1 lag medians")) {
  readRDS(mb_path) %>%
    filter(in_primary_sample, !is.na(days_since_visit), days_since_visit <= 300) %>%
    mutate(rec = if_else(recovered_strict == 1, "Recovered", "Not recovered")) %>%
    group_by(rec) %>% summarise(med = median(days_since_visit, na.rm = TRUE), .groups = "drop")
} else NULL

lag_txt_41 <- if (!is.null(lag_medians_41) && all(c("Recovered", "Not recovered") %in% lag_medians_41$rec)) {
  paste0("recovered children have shorter median lag (",
         lag_medians_41$med[lag_medians_41$rec == "Recovered"], " vs ",
         lag_medians_41$med[lag_medians_41$rec == "Not recovered"], " days)")
} else {
  "median lag by recovery status — NA, check 03_model_b_dataset.rds"
}

timing_finding_41 <- paste0("Age at registration ", age_sig_label_41, "; ", lag_txt_41, ".")

tab_4_1 <- tribble(~Theme, ~Finding, ~Field,
                   "Distance", paste0("Not significant under the permissive flag; ", amplification,
                                      "× larger and ", a2_dist_sig_label,
                                      " under the strict definition (β ", round(a1_dist_raw$coef, 3), " → ", round(a2_dist_raw$coef, 3), ")."),
                   "Coordinator confirmed distance is the primary barrier at enrolment, not at follow-up.",
                   "Timing", timing_finding_41,
                   "Long-open cases described as qualitatively harder to close; CHW attrition noted.",
                   "Recordkeeping", paste0("Strict recovery ", strict_pooled, "% vs permissive ", permissive_pooled, "%."),
                   "Off-network care verbal only; card not retained. Confirmed as standard practice.",
                   "Weather", "Null across all three variables and all specifications (see weather table).",
                   "No programme staff named weather as a barrier in any conversation."
)
artifacts$tab_4_1 <- tab_4_1

########################################
# Table 4.2 — Comparator registries    #
# (static literature comparison — no  #
# data dependency, unchanged)          #
########################################

tab_4_2 <- tribble(
  ~Study, ~Setting, ~Registry, ~Exposure, ~Result,
  "This thesis (2026)", "Kano & Katsina, Nigeria", "MCHTrack zero-dose tracking", "Rainfall, heat, NDVI", "Null — no significant effect on any variable (see weather table)",
  "Siddiqi et al. (2025)", "Sindh, Pakistan", "Provincial electronic immunisation registry (132M doses)", "Heat wave days", "Significant reduction — outreach hit hardest (−21.2%), fixed-site least (−5.8%)",
  "Samano et al. (2021)", "Miami, USA", "HIV clinic appointment registry", "Extreme heat & precipitation", "Significant increase in missed appointments (+13% for extreme precipitation)"
)
artifacts$tab_4_2 <- tab_4_2

########################################
# Table 4.3 — Closing synthesis        #
########################################

tab_4_3 <- tribble(~Theme, ~Quantitative, ~Field, ~Convergence,
                   "Distance", paste0("Predicts zero-dose status (β = ", round(a1_dist_raw$coef,3), " primary; ",
                                      round(a2_dist_raw$coef,3), " strict). No effect on recovery."),
                   "Coordinator: distance is the barrier to being found, not to being recovered by phone.", "Agreement",
                   "Timing", paste0("SMS contact predicts recovery (β = ", round(b1_sms_raw$coef,3), ")."),
                   "Long-open cases described as harder to close; contact speed matters more than reach.", "Agreement",
                   "Recordkeeping", paste0("Strict recovery ", strict_pooled, "% vs permissive ", permissive_pooled, "%."),
                   "Card check closes a case with no record of what the card showed. Confirmed as protocol.", "Agreement",
                   "Weather", "Null across rainfall, heat, and NDVI, all specifications.",
                   "No programme staff named weather as a barrier in any conversation, even when prompted.", "Agreement by absence"
)
artifacts$tab_4_3 <- tab_4_3

#--------------------------(PART 1 END)------------------------------#

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#  PART 2 — DATHARM AUDIT DOCUMENT FIGURES AND TABLES
#  Every chunk below replaces a hand-typed data.frame/tribble currently
#  inline in 11_reccs_doc_updated_v7.Rmd. All read from
#  09_data_investigations.R's outputs — no fresh computation happens here,
#  matching the same compute/present split as Part 1 and as the thesis's
#  09/10 scripts. Three DATHARM chunks are DELIBERATELY left out of this
#  rebuild: fig3a-timeline and fig4a are explicitly illustrative
#  (constructed examples, not MCHTrack data, per their own captions in the
#  Rmd) and table6a is a literature-evidence table with no data dependency
#  at all — none of the three have anything to compute. table-ladder-legend
#  and table5a are definitional/guidance tables (what each verification
#  level means; what each diagnostic check catches), not results tables,
#  so they stay as static tribbles here too, matching the Rmd's own
#  originals exactly.
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

########################################
# Fig1A — Duplicate-record set sizes,  #
# Katsina only                         #
########################################

dup_dist_path <- file.path(inv_dir, "09_dedup_set_size_distribution.rds")
if (require_file(dup_dist_path, "Fig1A duplicate set-size distribution")) {
  dup_dist_katsina <- readRDS(dup_dist_path) %>% filter(state == "Katsina")
  
  fig1a_datharm <- ggplot(dup_dist_katsina,
                          aes(x = fct_reorder(set_size_label,
                                              suppressWarnings(as.numeric(set_size_label)), .na_rm = FALSE),
                              y = rows_from_duplication, fill = table)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62) +
    scale_fill_manual(values = c("Linelisted (children enrolled)" = "#3498db",
                                 "Facility visits (vaccination records)" = "#e74c3c")) +
    labs(x = "Number of times a record appears (duplicate set size)",
         y = "Rows created by duplication", fill = NULL) +
    theme_datharm(13) +
    theme(panel.grid.major.x = element_blank(), legend.position = "top")
} else {
  fig1a_datharm <- placeholder_plot("MISSING INPUT\n09_dedup_set_size_distribution.rds")
}

artifacts_datharm$fig1a_path <- save_fig(fig1a_datharm, "datharm_fig1a", width = 8.2, height = 4.4)

########################################
# Table1A — Sample duplicated rows,    #
# Katsina facility visits              #
########################################

dup_sample_path <- file.path(inv_dir, "09_dedup_sample_rows.rds")
if (require_file(dup_sample_path, "Table1A duplicate row sample")) {
  dup_samples <- readRDS(dup_sample_path)
  table1a_datharm <- dup_samples$facility_visits %>%
    select(any_of(c("patient_id", "visit_date", "health_center_id", "vaccines_administered", "state")))
} else {
  table1a_datharm <- tibble(note = "MISSING INPUT — 09_dedup_sample_rows.rds")
}
artifacts_datharm$table1a <- table1a_datharm

########################################
# Table2A — Off-network care sample +  #
# share text inputs                    #
########################################

offnetwork_sample_path <- file.path(inv_dir, "09_offnetwork_sample.rds")
offnetwork_share_path  <- file.path(inv_dir, "09_offnetwork_share_by_state.rds")

if (require_file(offnetwork_sample_path, "Table2A off-network sample")) {
  table2a_datharm <- readRDS(offnetwork_sample_path)
} else {
  table2a_datharm <- tibble(note = "MISSING INPUT — 09_offnetwork_sample.rds")
}
artifacts_datharm$table2a <- table2a_datharm

if (require_file(offnetwork_share_path, "Table2A narrative — off-network share")) {
  offnetwork_share <- readRDS(offnetwork_share_path)
  kano_offnet_txt     <- offnetwork_share %>% filter(state == "Kano")
  katsina_offnet_txt  <- offnetwork_share %>% filter(state == "Katsina")
  offnetwork_narrative <- if (nrow(kano_offnet_txt) == 1 && nrow(katsina_offnet_txt) == 1) {
    paste0("Of ", comma(kano_offnet_txt$n_total), " Kano defaulter tracing records, ",
           comma(kano_offnet_txt$n_offnetwork), " (", kano_offnet_txt$pct_offnetwork,
           "%) close as off-network care with no supporting detail, compared with ",
           comma(katsina_offnet_txt$n_offnetwork), " of ", comma(katsina_offnet_txt$n_total),
           " (", katsina_offnet_txt$pct_offnetwork, "%) in Katsina.")
  } else "NA — check 09_offnetwork_share_by_state.rds has one row per state"
} else {
  offnetwork_narrative <- "NA — MISSING INPUT 09_offnetwork_share_by_state.rds"
}
artifacts_datharm$offnetwork_narrative <- offnetwork_narrative
cat("--- Table2A narrative sentence ---\n"); cat(offnetwork_narrative, "\n\n")

########################################
# Table-ladder-legend — definitional,  #
# no data dependency (unchanged)       #
########################################

ladder_legend_datharm <- tribble(
  ~Level, ~Definition, ~Source,
  "L1 — Maximalist", "yes_ok or yes_off_network_care", "defaulter_tracing",
  "L2 — Strict", "yes_ok only", "defaulter_tracing",
  "L3 — Matched", "yes_ok, and the child has a matching facility_visits record (any time)", "defaulter_tracing joined to facility_visits on patient_id",
  "L4 — Verified", "yes_ok, matching facility_visits record, and that record names a vaccine", "defaulter_tracing joined to facility_visits on patient_id and vaccines_administered"
)
artifacts_datharm$ladder_legend <- ladder_legend_datharm

########################################
# Fig2A — Recovery ladder (L1-L4) by   #
# state                                #
########################################

ladder_path <- file.path(inv_dir, "09_recovery_ladder.rds")
na_outcome_path <- file.path(inv_dir, "09_ladder_na_outcome_by_state.rds")
if (require_file(ladder_path, "Fig2A recovery ladder")) {
  ladder_na_lab <- if (require_file(na_outcome_path, "Fig2A NA-outcome caption")) {
    na_tab <- readRDS(na_outcome_path) %>% filter(n_na_tracing_outcome > 0)
    if (nrow(na_tab) > 0) {
      paste0("Excludes rows with missing tracing_outcome from each state's denominator: ",
             paste0(na_tab$state, " (", comma(na_tab$n_na_tracing_outcome), " of ",
                    comma(na_tab$n_total), ")", collapse = ", "), ".")
    } else NA_character_
  } else NA_character_
  
  ladder_datharm <- readRDS(ladder_path) %>%
    mutate(
      Level = case_when(
        Level == "L1" ~ "L1 — Maximalist", Level == "L2" ~ "L2 — Strict",
        Level == "L3" ~ "L3 — Matched",    Level == "L4" ~ "L4 — Verified"),
      Level = factor(Level, levels = c("L1 — Maximalist", "L2 — Strict", "L3 — Matched", "L4 — Verified")),
      state = factor(state, levels = c("Kano", "Katsina")))
  
  # Sequential blue scale replaces the previous 4-colour categorical
  # palette (green/blue/orange/red), which paired L1's green against L4's
  # red -- the hardest combination for red-green colourblind readers.
  # A single-hue ramp also better reflects that L1-L4 are an ordered,
  # nested sequence rather than four unrelated categories.
  level_colour <- c("L1 — Maximalist" = "#AFD1E7", "L2 — Strict" = "#6AA8D8",
                    "L3 — Matched" = "#2C6FA8", "L4 — Verified" = "#0B3D5C")
  
  # Point-to-point drops between consecutive levels, computed per state
  # rather than typed in, so a change in the underlying data automatically
  # updates the "-X.Xpp" annotations rather than silently going stale.
  drops_datharm <- ladder_datharm %>%
    arrange(state, Level) %>%
    group_by(state) %>%
    mutate(drop_pp = Pct - lead(Pct)) %>%
    filter(!is.na(drop_pp)) %>%
    ungroup() %>%
    mutate(y_pos = as.numeric(Level))
  
  fig2a_datharm <- ggplot(ladder_datharm, aes(x = Level, y = Pct, fill = Level)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(round(Pct, 1), "%")), hjust = -0.15, size = 4, fontface = "bold") +
    facet_wrap(~state) +
    coord_flip(clip = "off") +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    scale_fill_manual(values = level_colour, guide = "none") +
    labs(x = NULL, y = "Share of traced defaulters") +
    theme_datharm(12.5) +
    theme(plot.margin = margin(5.5, 46, 5.5, 5.5), strip.text = element_text(face = "bold", size = 12))
} else {
  fig2a_datharm <- placeholder_plot("MISSING INPUT\n09_recovery_ladder.rds")
  drops_datharm <- tibble()
  ladder_na_lab <- NA_character_
}

artifacts_datharm$fig2a_path <- save_fig(fig2a_datharm, "datharm_fig2a", width = 8.5, height = 4.6)
artifacts_datharm$fig2a_drops <- drops_datharm
# Caveat text (which rows were excluded from each state's denominator for
# missing tracing_outcome) used to be baked into the plot as a caption --
# moved here so it can be pulled into the Rmd's fig.cap text instead.
artifacts_datharm$fig2a_na_lab <- ladder_na_lab

########################################
# Table3B — ZD 90-day reconciliation   #
# PROXY — see 09_data_investigations.R #
# Section 5 caveat: standard exports   #
# lack the expanded resolution-date    #
# fields the original table used       #
########################################

zd_recon_path  <- file.path(inv_dir, "09_zd_reconciliation_proxy.rds")
zd_sample_path <- file.path(inv_dir, "09_zd_reconciliation_sample.rds")

if (require_file(zd_recon_path, "Table3B reconciliation")) {
  zd_reconciliation_datharm <- readRDS(zd_recon_path)
  zd_recon_narrative <- {
    overall_resolved <- sum(zd_reconciliation_datharm$n_resolved)
    overall_no_match <- sum(zd_reconciliation_datharm$n_no_matching_visit)
    overall_pct <- round(overall_no_match / overall_resolved * 100, 1)
    paste0(comma(overall_no_match), " of ", comma(overall_resolved), " resolved cases overall (",
           overall_pct, "%) have no matching visit within 90 days. PROXY figure — see script caveat, ",
           "not a reproduction of the original expanded-export reconciliation.")
  }
} else {
  zd_reconciliation_datharm <- tibble(note = "MISSING INPUT — 09_zd_reconciliation_proxy.rds")
  zd_recon_narrative <- "NA — MISSING INPUT"
}
artifacts_datharm$table3b_summary <- zd_reconciliation_datharm
artifacts_datharm$table3b_narrative <- zd_recon_narrative
cat("--- Table3B narrative (PROXY) ---\n"); cat(zd_recon_narrative, "\n\n")

if (require_file(zd_sample_path, "Table3B row sample")) {
  table3b_datharm <- readRDS(zd_sample_path)
} else {
  table3b_datharm <- tibble(note = "MISSING INPUT — 09_zd_reconciliation_sample.rds")
}
artifacts_datharm$table3b <- table3b_datharm

########################################
# Table5A — Diagnostic checklist,      #
# definitional/guidance, no data       #
# dependency (unchanged)               #
########################################

table5a_datharm <- tribble(
  ~Check, ~Catches,
  "visit_date substantially earlier than created_on", "Retroactive / backfilled entry",
  "Same patient_id + visit_date + facility appearing more than once", "Duplicate sync",
  "Any individual mobiliser's death or refusal rate far above the ward average", "Likely entry error",
  "zero_dose flag still TRUE for a child with a post-enrolment vaccination record", "Stale status",
  "Enrolled children count declining month-over-month in a ward", "Possible data loss"
)
artifacts_datharm$table5a <- table5a_datharm

########################################
# Fig5A — Deceased-at-tracing rate by  #
# ward, Kano                           #
########################################

ward_dead_path <- file.path(inv_dir, "09_ward_deceased_rate.rds")
if (require_file(ward_dead_path, "Fig5A ward deceased rate")) {
  ward_dead_obj <- readRDS(ward_dead_path)
  # Same fct_reorder() fragility as Figure 3.4 above — replaced with
  # arrange() + factor(levels = unique(...)) so a duplicate "ward (LGA)"
  # label (e.g. two wards that clean to the same name) can't crash this
  # chunk. If duplicates do exist here, that's itself worth flagging as a
  # data-quality finding, not just working around silently.
  ward_dead_datharm <- ward_dead_obj$by_ward %>%
    mutate(ward_lab = paste0(facility_ward, " (", lga_name, ")"),
           flag = if_else(row_number() == which.max(deceased_rate_pct), "Highest", "Other")) %>%
    arrange(deceased_rate_pct) %>%
    mutate(ward_lab = factor(ward_lab, levels = unique(ward_lab)))
  if (anyDuplicated(paste0(ward_dead_obj$by_ward$facility_ward, "|", ward_dead_obj$by_ward$lga_name)) > 0) {
    warning("Fig5A: duplicate ward+LGA combinations found in 09_ward_deceased_rate.rds — ",
            "two rows share the same label. Check ward_outcome_rates in ",
            "09_data_investigations.R Section 6 for a lingering spelling/case variant.", call. = FALSE)
  }
  kano_avg_dead <- ward_dead_obj$kano_avg
  
  fig5a_datharm <- ggplot(ward_dead_datharm, aes(x = ward_lab, y = deceased_rate_pct, fill = flag)) +
    geom_col(width = 0.65) +
    geom_hline(yintercept = kano_avg_dead, linetype = "dashed", colour = "#555") +
    annotate("text", x = 2, y = kano_avg_dead, label = paste0("Kano average: ", kano_avg_dead, "%"),
             vjust = -0.6, hjust = 0, size = 3.3, colour = "#555") +
    scale_fill_manual(values = c("Highest" = "#e74c3c", "Other" = "#b0b0b0"), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
    coord_flip() +
    labs(x = NULL, y = "Deceased-at-tracing rate") +
    theme_datharm(12.5)
} else {
  fig5a_datharm <- placeholder_plot("MISSING INPUT\n09_ward_deceased_rate.rds")
  kano_avg_dead <- NA
}

artifacts_datharm$fig5a_path <- save_fig(fig5a_datharm, "datharm_fig5a", width = 8, height = 5.5)
artifacts_datharm$fig5a_kano_avg <- kano_avg_dead
# "Highest" flags whichever ward has the largest rate in the current data,
# which is expected to be Mekiya but isn't guaranteed to stay that way if
# the underlying data changes -- this verification caveat used to be baked
# into the plot as a caption; moved here for the Rmd's fig.cap instead.
artifacts_datharm$fig5a_verify_note <- paste0(
  "'Highest' marks the ward with the largest rate in the current data, not necessarily Mekiya by name -- ",
  "verify the flagged ward matches the Mekiya finding described in the text before citing it as such.")

########################################
# Fig5B panel 1 — Top 40 facility-day  #
# visit volumes, Katsina               #
########################################

fac_vol_path <- file.path(inv_dir, "09_facility_day_volume.rds")
if (require_file(fac_vol_path, "Fig5B facility-day volume")) {
  fac_vol <- readRDS(fac_vol_path)
  fac_vol_katsina <- fac_vol$katsina
  
  fig5b_panel1_datharm <- ggplot(fac_vol_katsina$top40, aes(x = rank, y = n_visits, colour = tier)) +
    geom_point(size = 2.4) +
    scale_colour_manual(values = c("30-49" = "#5dade2", "50-99" = "#f0b429",
                                   "100-199" = "#c0392b", "200+" = "#7b241c"), guide = "none") +
    labs(x = "Rank", y = "Visits / day") +
    theme_datharm(12) +
    theme(panel.grid.minor = element_blank())
  
  ########################################
  # Fig5B panel 2 — Volume-band          #
  # distribution, Katsina                #
  ########################################
  
  band_dist_datharm <- fac_vol_katsina$bands
  fig5b_panel2_datharm <- ggplot(band_dist_datharm, aes(x = band, y = N)) +
    geom_col(fill = "#5dade2", width = 0.65) +
    geom_text(aes(label = paste0(N, " (", Pct, "%)")), hjust = -0.05, size = 3.1) +
    scale_y_continuous(limits = c(0, max(band_dist_datharm$N, na.rm = TRUE) * 1.25), expand = c(0, 0)) +
    coord_flip() +
    labs(x = NULL, y = "Facility-days") +
    theme_datharm(12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
} else {
  fig5b_panel1_datharm <- placeholder_plot("MISSING INPUT\n09_facility_day_volume.rds")
  fig5b_panel2_datharm <- placeholder_plot("MISSING INPUT\n09_facility_day_volume.rds")
  fac_vol_katsina <- list(n_facility_days = NA, p95 = NA, p99 = NA)
}

artifacts_datharm$fig5b_panel1_path <- save_fig(fig5b_panel1_datharm, "datharm_fig5b_panel1", width = 8, height = 4)
artifacts_datharm$fig5b_panel2_path <- save_fig(fig5b_panel2_datharm, "datharm_fig5b_panel2", width = 8, height = 4.5)
# n and percentile figures used to be baked into panel 2's subtitle/caption;
# exported here so the Rmd's fig.cap can carry them (and stay in sync with
# the data on rerun) instead of a hardcoded string.
artifacts_datharm$fig5b_n_facility_days <- fac_vol_katsina$n_facility_days
artifacts_datharm$fig5b_p95 <- fac_vol_katsina$p95
artifacts_datharm$fig5b_p99 <- fac_vol_katsina$p99

########################################
# Table6A — Literature evidence table, #
# no data dependency (unchanged)       #
########################################

table6a_datharm <- tribble(
  ~Variable, ~Why, ~How,
  "Maternal education", "Strongest predictor of full immunisation across sub-Saharan Africa (Farrenkopf et al., 2023)",
  "\"Did the child's mother attend school?\" (yes / no / unknown)",
  "Children under 5 in household", "Later-born children are consistently less likely to be fully vaccinated (Farrenkopf et al., 2023)",
  "Count field: \"How many children under 5 live here?\"",
  "Who decided to vaccinate", "Gender-skewed household decision-making is a documented barrier in Kano specifically (Bolarinwa et al., 2025)",
  "Dropdown: mother / father / grandparent / other caregiver",
  "Prior refusal at this household", "A household that has refused before needs a different follow-up approach than one not yet reached",
  "Flag on tracing record: \"Was vaccination declined at this contact?\" (yes / no)",
  "Roof material (wealth proxy)", "Predicts access barriers independent of distance; standard proxy in national DHS surveys",
  "Dropdown: thatch / zinc / concrete / other"
)
artifacts_datharm$table6a <- table6a_datharm

#--------------------------(PART 2 END)------------------------------#

#----------------------------------------------------------------------------

##########
# Export #
##########

# Two manifests saved: one for the thesis draft (10_artifacts.rds — same
# name pattern as the old 09_artifacts.rds), one for a future rebuilt
# DATHARM Rmd (10_artifacts_datharm.rds). Also saved combined, in case one
# knitting document ever needs to pull from both (e.g. a shared appendix).

saveRDS(artifacts,         file.path(out_dir, "10_artifacts.rds"))
saveRDS(artifacts_datharm, file.path(out_dir, "10_artifacts_datharm.rds"))
saveRDS(list(thesis = artifacts, datharm = artifacts_datharm),
        file.path(out_dir, "10_artifacts_combined.rds"))

# Figure titles manifest (16/7/2026) — thesis figures only (Part 1). Each
# entry is the suggested "Figure X.X.  Title text" for that PNG, which is
# no longer baked into the image itself. The Rmd draft applies these (or
# its own overrides) as knitr chunk captions — see the note where
# fig_titles is initialised, near the top of Part 1.
saveRDS(as_tibble(fig_titles) %>% pivot_longer(everything(), names_to = "fig_id", values_to = "suggested_title"),
        file.path(out_dir, "10_figure_titles.rds"))

cat("\n=== BUILD COMPLETE ===\n")
cat("Manifests saved to:", out_dir, "\n")
cat("Figures saved to:", figs_dir, "\n\n")
cat("Thesis artifacts:\n");  print(names(artifacts))
cat("\nDATHARM artifacts:\n"); print(names(artifacts_datharm))

cat("\n--- Reminders before trusting this output ---\n")
cat("1. extract_coef_se() has not been tested against a real modelsummary\n")
cat("   .txt file. Check the 'Model A coefficient extraction check' cat()\n")
cat("   output near the top of this run — if coefficients show NA, the\n")
cat("   term-matching patterns need adjusting against the real file layout.\n")
cat("2. Figure 2.2 and DATHARM Fig1A are now both dynamic, sourced from\n")
cat("   09_data_investigations.R. Run that script first, or both come back\n")
cat("   as MISSING INPUT placeholders.\n")
cat("3. Figure 3.2/3.2b's waterfall now shows duplicate-row removal as a\n")
cat("   real, uncapped step (sourced from 01_dedup_summary.rds /\n")
cat("   01_dedup_summary_by_state.rds — rerun 01 if either is missing).\n")
cat("   Null-LGA remains caption-only, since 03_regression.R doesn't\n")
cat("   apply that filter yet.\n")
cat("4. Table3B (DATHARM) and its narrative sentence are PROXY figures —\n")
cat("   built from identified_zd's own visit_date standing in for the\n")
cat("   expanded resolution_date field the original table used. Confirm\n")
cat("   whether DATHARM's data manager can supply the real expanded export\n")
cat("   before citing these numbers as a reproduction of the original.\n")
cat("5. Fig5A (DATHARM, ward deceased rate) uses a str_detect pattern for\n")
cat("   'deceased' tracing outcomes (deceased_pattern in\n")
cat("   09_data_investigations.R) that has not been verified against the\n")
cat("   real distinct tracing_outcome values. Check that script's console\n")
cat("   output before citing the Mekiya figure specifically.\n")
cat("6. This script assumes 09_data_investigations.R, 03_regression.R and\n")
cat("   06_era5_analysis.R have all been run with their 13/7/2026 fixes. If\n")
cat("   Table 3.1b's Full sample and Lag-time subset N still come out\n")
cat("   equal, or any DATHARM chunk shows a MISSING INPUT placeholder,\n")
cat("   check which upstream script needs rerunning first.\n")
cat("7. Scripts 02-08 keep their original numbers (no renumbering cascade —\n")
cat("   only 09_data_investigations.R and this file changed names/numbers).\n")
cat("   reg_dir/resid_dir/era_dir/chirps_dir/ndvi_dir below are already\n")
cat("   final and should not need further changes.\n")
cat("8. fig3a-timeline, fig4a and table6a stay static by design (no data\n")
cat("   dependency) — table6a is exported here for convenience so the\n")
cat("   future DATHARM Rmd can pull every table from one manifest, but it\n")
cat("   was never computed from MCHTrack data and never will be.\n")
cat("9. Thesis figures (Part 1) no longer have a title baked into the PNG —\n")
cat("   see 10_figure_titles.rds for the suggested 'Figure X.X.  Title'\n")
cat("   text per figure. Apply as knitr chunk captions in the Rmd; titling,\n")
cat("   renumbering and reordering figures no longer requires rerunning\n")
cat("   this script. DATHARM figures (Part 2) are UNCHANGED — still have\n")
cat("   baked-in titles, out of scope for this pass.\n")
cat("10. Two new figures this pass: fig_weather_robustness (coefficient\n")
cat("    plot companion to tab_weather_robustness) and fig_2_2b (data\n")
cat("    reliability by state, motivating RQ3's Kano-only scope). Neither\n")
cat("    is wired into 11_dissertation_draft.Rmd yet, same as fig_3_2b.\n")
cat("11. FOUR new geospatial figures (19/7/2026): fig_1_1 (footprint map,\n")
cat("    fills the Figure 1.1 placeholder), fig_3_4b (ward residual map,\n")
cat("    companion to fig_3_4), fig_3_7b (off-network share map, companion\n")
cat("    to fig_3_7), fig_weather_maps (rainfall/heat/NDVI choropleths,\n")
cat("    companion to fig_weather_robustness / Figure 3.9). Ported from the\n")
cat("    standalone fig_*_map_prototype.R scripts after sign-off there —\n")
cat("    same LGA name-matching, point-sampling and binned-legend logic,\n")
cat("    now sourced from the real 01/02/04/06/07_*.rds files instead of a\n")
cat("    proof-of-concept run. Boundary data is GADM v4.1 (gadm.org),\n")
cat("    fetched over the network on first run and cached to\n")
cat("    02_data/03_geodata thereafter — if that fetch fails (no network),\n")
cat("    all four fall back to a placeholder rather than halting the whole\n")
cat("    script (see map_boundaries_loaded near the top). None of the four\n")
cat("    is wired into 11_dissertation_body.Rmd yet, same as fig_3_2b/\n")
cat("    fig_weather_robustness/fig_2_2b above -- update 11_dissertation_\n")
cat("    body.Rmd's Figure 1.1 placeholder (and add new show_fig() calls\n")
cat("    for the other three) once you're happy with these.\n")

#--------------------------(END)------------------------------#