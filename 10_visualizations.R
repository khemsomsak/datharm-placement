########################################################
#  Figures and Tables                                  #
#     Build Script (thesis + DATHARM)                  #
#  Created on: 13/07/2026                              #
#  Updated on: 13/07/2026                              #
########################################################

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

#----------------------------------------------------------------------------

##############################
# Helpers — parsing & saving #
##############################

# Reads a modelsummary plain-text (pipe-delimited) table into a term/values
# tibble. Same helper as the original 09_visualization_markdown.Rmd — kept
# unchanged since it already worked correctly for b_n/b_r2 there.
parse_ms_txt <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  rows  <- lines[str_detect(lines, "\\|") & !str_detect(lines, "^[+=]+$") &
                   !str_detect(lines, "^\\+[-=+]+\\+$")]
  map_dfr(rows, function(r) {
    cells <- str_split(r, "\\|")[[1]] %>% str_trim() %>% .[. != ""]
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

#--- Colours & theme (unchanged from original 09_visualizations.R) ---
col_sig <- "#1D6FA4"; col_nonsig <- "#B0B0B0"; col_strict <- "#BA7517"
pal_state <- c("Kano" = "#1D6FA4", "Katsina" = "#BA7517")
pal_zd <- c("Zero-dose" = "#C0312D", "Vaccinated" = "#5A9FD4")
col_confirmed <- "#1D9E75"; col_offnet <- "#F4A261"; col_notrec <- "#C0312D"

theme_diss <- function(bs = 12) {
  theme_minimal(base_size = bs) %+replace% theme(
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold", size = bs, hjust = 0, margin = margin(b = 4)),
    plot.subtitle = element_text(size = bs - 2, colour = "#666", hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, colour = "#888", hjust = 0, margin = margin(t = 8), lineheight = 1.1),
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
    plot.subtitle = element_text(color = "#666", size = bs - 2))
}

suppressWarnings({
  for (loc in c("English", "en_US.UTF-8", "en_GB.UTF-8", "en_US", "C")) {
    if (!inherits(try(Sys.setlocale("LC_TIME", loc), silent = TRUE), "try-error")) {
      if (Sys.getlocale("LC_TIME") != "") break
    }
  }
})

artifacts <- list()          # thesis figures/tables
artifacts_datharm <- list()  # DATHARM audit doc figures/tables

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
a_r2 <- fb(ev(ma_parsed, "R2 ", 1), "NA")
a_r2_strict <- fb(ev(ma_parsed, "R2 ", 2), "NA")

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
b_r2 <- fb(ev(mb_robust, "R2 ", 1), "NA")

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
b_full_r2 <- fb(ev(tab31b_parsed, "R2 ", 1), "NA")
b_lag_n   <- fb(ev(tab31b_parsed, "Num.Obs", 2), "NA")
b_lag_r2  <- fb(ev(tab31b_parsed, "R2 ", 2), "NA")

if (tab31b_ok && !is.na(b_full_n) && !is.na(b_lag_n) && b_full_n == b_lag_n) {
  warning("Table 3.1b: Full sample N equals Lag-time subset N — this is the ",
          "exact symptom of the original bug. Check that 03_regression.R's ",
          "m_b0_full was actually rebuilt without days_since_visit.", call. = FALSE)
}

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
           hjust = 1, size = 2.8, colour = "#1D6FA4", fontface = "italic", family = "serif") +
  geom_rect(data = boxes, aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -0.32, ymax = 0.32),
            fill = "white", colour = "#333", linewidth = 0.6) +
  geom_text(data = boxes, aes(x = x, y = 0.1, label = label),
            size = 3.1, fontface = "bold", lineheight = 0.9, family = "serif") +
  geom_text(data = boxes, aes(x = x, y = -0.18, label = dataset),
            size = 2.5, colour = "#555", fontface = "italic", family = "serif") +
  annotate("segment", x = c(1.55, 3.55, 5.55), xend = c(2.45, 4.45, 6.45), y = 0, yend = 0,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed"), colour = "#333", linewidth = 0.5) +
  annotate("text", x = 1, y = -0.72, label = "~1 in 5 households\nnot reached in enumeration",
           size = 2.5, colour = "#C0312D", fontface = "italic", family = "serif") +
  annotate("segment", x = 1, xend = 1, y = -0.32, yend = -0.5, colour = "#C0312D", linewidth = 0.4,
           arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("text", x = 7, y = -0.75, label = "Off-network care recorded\nverbally, cannot be verified",
           size = 2.5, colour = "#BA7517", fontface = "italic", family = "serif") +
  annotate("segment", x = 7, xend = 7, y = -0.32, yend = -0.5, colour = "#BA7517", linewidth = 0.4,
           linetype = "dotted", arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  scale_x_continuous(limits = c(0.4, 8.0)) + scale_y_continuous(limits = c(-0.95, 0.62)) +
  labs(title = "Figure 2.1.  MCHTrack data pipeline and its structural blind spots",
       caption = "Each stage writes to one dataset. Two events fall outside the system boundary: children never enumerated, and vaccination\ndelivered off-network. Neither is recorded, and both bound what any downstream analysis can observe.") +
  theme_void() +
  theme(plot.title = element_text(family = "serif", face = "bold", size = 12, margin = margin(b = 6, l = 2)),
        plot.caption = element_text(family = "serif", colour = "#888", size = 8.5, hjust = 0, margin = margin(t = 8, l = 2), lineheight = 1.1))

artifacts$fig_2_1_path <- save_fig(fig_2_1, "fig_2_1", width = 8.6, height = 4.2)

########################################
# Figure 2.2 — Duplicate record rates  #
# NOW DYNAMIC. Previously a MANUAL     #
# CONSTANT (22.1% / 38.9%) with no     #
# source in 01-08. Reads              #
# 09_dedup_summary_by_state.rds from   #
# 09_data_investigations.R instead.    #
########################################

dedup_summary_path <- file.path(inv_dir, "09_dedup_summary_by_state.rds")
if (require_file(dedup_summary_path, "Figure 2.2 duplicate rates")) {
  dedup_summary <- readRDS(dedup_summary_path) %>%
    mutate(table = factor(table, levels = c("Linelisted (children enrolled)", "Facility visits (vaccination records)")))
  
  fig_2_2 <- ggplot(dedup_summary, aes(x = table, y = pct_duplicate, fill = state)) +
    geom_col(position = position_dodge(0.62), width = 0.55, alpha = 0.92) +
    geom_text(aes(label = paste0(round(pct_duplicate, 1), "%")),
              position = position_dodge(0.62), vjust = -0.4, size = 3.1,
              fontface = "bold", family = "serif") +
    scale_fill_manual(values = pal_state) +
    scale_y_continuous(limits = c(0, max(dedup_summary$pct_duplicate, na.rm = TRUE) * 1.2 + 3),
                       labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Figure 2.2.  Duplicate record rate by state and MCHTrack table",
         subtitle = "Computed live from 09_data_investigations.R · Katsina affected, Kano clean throughout",
         x = NULL, y = "Duplicate rows",
         caption = "Computed live from 09_dedup_summary_by_state.rds (compound-key duplicate detection: pseudo_id for linelisted;\npatient_id + visit_date + health_center_id + vaccines_administered for facility visits). Duplication traced to a device\nhandover causing records to sync more than once, confirmed by the Katsina coordinator.") +
    theme_diss(11)
} else {
  fig_2_2 <- placeholder_plot("MISSING INPUT\n09_dedup_summary_by_state.rds\n(run 09_data_investigations.R)")
}

artifacts$fig_2_2_path <- save_fig(fig_2_2, "fig_2_2", width = 8, height = 3.8)

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
              inherit.aes = FALSE, vjust = -0.5, size = 3.2, fontface = "bold",
              colour = "#333", family = "serif") +
    scale_fill_manual(values = c("Mean" = "#1D6FA4", "Variance" = "#C0312D")) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Figure 2.3.  Overdispersion in daily facility visit counts",
         subtitle = "Variance far exceeds the mean in both LGAs · rules out a standard Poisson specification",
         x = NULL, y = "Daily visits",
         caption = paste0("Variance-to-mean ratios computed live from 06_panel_daily.rds. Log-transformed OLS is retained for consistency\n",
                          "with the zero-dose and recovery models; a negative binomial check appears in Figure 3.10.")) +
    theme_diss(11)
} else {
  fig_2_3 <- placeholder_plot("MISSING INPUT\n06_panel_daily.rds")
}

artifacts$fig_2_3_path <- save_fig(fig_2_3, "fig_2_3", width = 7.5, height = 3.6)

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
    scale_fill_manual(values = pal_zd) +
    scale_x_continuous(labels = label_number(suffix = " km")) +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "A. Distance to health facility", x = "Distance (km)", y = "Children") +
    theme_diss(11)
  page31 <- ggplot(pm %>% filter(age_months_at_reg <= 60), aes(x = age_months_at_reg, fill = zd)) +
    geom_histogram(bins = 40, position = "identity", alpha = 0.6, colour = NA) +
    scale_fill_manual(values = pal_zd) +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "B. Age at registration", x = "Age at registration (months)", y = NULL) +
    theme_diss(11)
  
  fig_3_1 <- (pdist | page31) +
    plot_annotation(title = "Figure 3.1.  Predictor distributions by zero-dose status",
                    subtitle = paste0("Primary analytic sample · N = ", comma(n_fig31), " · Rimi and GPS-error distances excluded"),
                    caption = "Zero-dose children sit further along the distance tail and skew older at registration, though both distributions overlap\nsubstantially. A child can live close to a facility and still be missed.",
                    theme = theme(plot.title = element_text(family = "serif", face = "bold", size = 12.5),
                                  plot.subtitle = element_text(family = "serif", size = 10, colour = "#666"),
                                  plot.caption = element_text(family = "serif", size = 9, colour = "#888", hjust = 0, lineheight = 1.1)))
} else {
  fig_3_1 <- placeholder_plot("MISSING INPUT\n03_model_a_dataset.rds")
  n_fig31 <- NA
}

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
# NOW MORE COMPLETE. Previously the    #
# gap between Rimi exclusion and the   #
# final N was one lump "Other /        #
# unverified exclusions" bar. Now      #
# split into Null-LGA and duplicate-   #
# pseudo-ID sub-counts using           #
# 09_null_lga_summary.rds and          #
# 09_dedup_summary_by_state.rds.       #
########################################

# APPROXIMATION, still flagged rather than hidden: null_lga_n and
# dedup_ll_n below are counts within the full linelisted table, not
# counts specifically within the subset that survives the distance and
# Rimi filters. A row could in principle be excluded by more than one
# filter, so these two new categories are capped with min() against the
# remaining gap to avoid a negative bar, but the split should be read as
# an informative approximation, not an exact accounting reconciliation.

ll_path <- file.path(mch_dir, "01_linelisted_clean.rds")
null_lga_path <- file.path(inv_dir, "09_null_lga_summary.rds")

if (require_file(ll_path, "Figure 3.2 sample waterfall") && require_file(ma_path, "Figure 3.2 sample waterfall")) {
  data_ll_raw <- readRDS(ll_path)
  n_start   <- data_ll_raw %>% filter(woman_or_child == "child") %>% nrow()
  n_dist_ok <- data_ll_raw %>% filter(woman_or_child == "child") %>%
    left_join(readRDS(file.path(mch_dir, "01_any_vaccine_flag.rds")),
              by = c("pseudo_id" = "patient_id")) %>%
    filter(!is.na(hf_distance_km)) %>% nrow()
  n_rimi_ok <- readRDS(ma_path) %>% filter(!is.na(hf_distance_km), !rimi_flag) %>% nrow()
  n_final   <- readRDS(ma_path) %>% filter(in_primary_sample) %>% nrow()
  
  other_gap <- max(n_rimi_ok - n_final, 0)
  
  if (require_file(null_lga_path, "Figure 3.2 Null-LGA count") &&
      require_file(dedup_summary_path, "Figure 3.2 duplicate count")) {
    null_lga_n <- readRDS(null_lga_path)$n_null_lga
    dedup_ll_n <- readRDS(dedup_summary_path) %>%
      filter(table == "Linelisted (children enrolled)") %>%
      summarise(n = sum(duplicate_rows)) %>% pull(n)
    
    null_lga_step  <- min(null_lga_n, other_gap)
    remaining_gap  <- other_gap - null_lga_step
    dup_step       <- min(dedup_ll_n, remaining_gap)
    other_residual <- remaining_gap - dup_step
  } else {
    null_lga_step <- 0; dup_step <- 0; other_residual <- other_gap
  }
  
  wf <- tribble(
    ~step, ~label, ~n, ~type,
    1, "Children in\nlinelisted (both states)", n_start, "start",
    2, "Missing / implausible\ndistance", n_start - n_dist_ok, "exclude",
    3, "Rimi LGA\n(backfill)", n_dist_ok - n_rimi_ok, "exclude",
    4, "Unattributed\n(Null) LGA", null_lga_step, "exclude",
    5, "Duplicate\npseudo-ID rows", dup_step, "exclude",
    6, "Other / unverified\nexclusions", other_residual, "exclude",
    7, "Primary analytic\nsample", n_final, "end"
  ) %>%
    mutate(delta = case_when(type == "start" ~ n, type == "end" ~ 0, TRUE ~ -n),
           remaining = cumsum(delta))
  
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
              size = 2.6, fontface = "bold", colour = "#C0312D", family = "serif") +
    geom_text(aes(y = -max(wf$remaining, na.rm = TRUE) * 0.05, label = label), size = 2.15, lineheight = 0.9,
              colour = "#333", family = "serif") +
    scale_fill_manual(values = c("start" = "#1D6FA4", "exclude" = "#C0312D", "end" = "#1D9E75"),
                      labels = c("start" = "Starting N", "exclude" = "Excluded", "end" = "Analytic sample"),
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = NULL) +
    labs(title = "Figure 3.2.  Analytic sample construction — zero-dose model",
         subtitle = paste0("Primary sample: ", comma(n_final), " children after all exclusions"),
         x = NULL, y = "Row count",
         caption = "Computed live from 01_linelisted_clean.rds, 03_model_a_dataset.rds and 09_data_investigations.R. Null-LGA and\nduplicate-row splits are approximations (see script comment) — 'Other / unverified' is the true residual once those are removed.") +
    theme_diss(11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
} else {
  fig_3_2 <- placeholder_plot("MISSING INPUT\n01_linelisted_clean.rds or 03_model_a_dataset.rds")
}

artifacts$fig_3_2_path <- save_fig(fig_3_2, "fig_3_2", width = 8.6, height = 4.6)

########################################
# Figure 3.3 — ZD model coefficients   #
########################################

coef_a <- tribble(~label, ~estimate, ~se,
                  "Distance to\nhealth facility (km)", a1_dist_raw$coef, a1_dist_raw$se,
                  "Age at registration\n(months)",     a1_age_raw$coef,  a1_age_raw$se,
                  "Female\n(ref: male)",               a1_sex_raw$coef,  a1_sex_raw$se) %>%
  mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
         sig = abs(estimate / se) > 1.96, star = map2_chr(estimate, se, star2),
         label = fct_rev(fct_inorder(label)))

pA33 <- ggplot(coef_a, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi, colour = sig), height = 0.15, linewidth = 0.9) +
  geom_point(aes(fill = sig), shape = 21, size = 3.6, stroke = 0.8, colour = "white") +
  geom_text(aes(x = ci_hi + 0.004, label = paste0(formatC(estimate, format = "f", digits = 3),
                                                  if_else(sig, star, " n.s."))), hjust = 0, size = 3.0, family = "serif", colour = "#333") +
  scale_colour_manual(values = c("TRUE" = col_sig, "FALSE" = col_nonsig), guide = "none") +
  scale_fill_manual(values = c("TRUE" = col_sig, "FALSE" = "#d5d5d5"), guide = "none") +
  labs(subtitle = "A. Primary specification (all predictors)", x = "Coefficient (log-odds)", y = NULL) +
  theme_diss(11) + theme(axis.text.y = element_text(lineheight = 0.9))

coef_b33 <- tribble(~definition, ~estimate, ~se,
                    "Primary\n(penta-ZD flag)",    a1_dist_raw$coef, a1_dist_raw$se,
                    "Strict\n(no vaccine at all)", a2_dist_raw$coef, a2_dist_raw$se) %>%
  mutate(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
         sig = abs(estimate / se) > 1.96, star = map2_chr(estimate, se, star2),
         definition = fct_rev(fct_inorder(definition)))

amplification <- round(a2_dist_raw$coef / a1_dist_raw$coef, 1)

pB33 <- ggplot(coef_b33, aes(x = estimate, y = definition)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi, colour = sig), height = 0.12, linewidth = 1.0) +
  geom_point(aes(fill = sig), shape = 21, size = 4.4, stroke = 0.9, colour = "white") +
  geom_text(aes(x = ci_hi + 0.03, label = paste0("β = ", formatC(estimate, format = "f", digits = 3),
                                                 if_else(sig, star, " n.s."))), hjust = 0, size = 3.2, family = "serif",
            fontface = "bold", colour = "#333") +
  annotate("text", x = max(coef_b33$ci_hi, na.rm = TRUE) * 0.55, y = 1.62,
           label = paste0(amplification, "× amplification"), size = 3.2,
           family = "serif", fontface = "italic", colour = "#666") +
  scale_colour_manual(values = c("TRUE" = col_strict, "FALSE" = col_nonsig), guide = "none") +
  scale_fill_manual(values = c("TRUE" = col_strict, "FALSE" = "#d5d5d5"), guide = "none") +
  labs(subtitle = "B. Distance coefficient by zero-dose definition",
       x = "Coefficient on distance (log-odds)", y = NULL) +
  theme_diss(11) + theme(axis.text.y = element_text(lineheight = 0.9))

fig_3_3 <- (pA33 / pB33) + plot_annotation(
  title = "Figure 3.3.  Zero-dose model — primary specification and definitional amplification",
  subtitle = paste0("95% confidence intervals · LGA fixed effects · Ward-clustered SE · N = ", a_n),
  caption = paste0("Panel A: blue = significant at p < 0.05. Panel B: amber = significant at p < 0.01; grey = not significant.\n",
                   "Distance coefficient is ", amplification, "x larger under the strict definition than the primary one. Computed live from 01_model_a_zerodose_predictors.txt."),
  theme = theme(plot.title = element_text(family = "serif", face = "bold", size = 12.5),
                plot.subtitle = element_text(family = "serif", size = 10, colour = "#666"),
                plot.caption = element_text(family = "serif", size = 9, colour = "#888", hjust = 0, lineheight = 1.1)))

artifacts$fig_3_3_path <- save_fig(fig_3_3, "fig_3_3", width = 8.4, height = 5.4)

########################################
# Figure 3.4 — Ward-level residuals    #
########################################

wr_path <- file.path(resid_dir, "04_ward_residuals_classified.csv")
if (require_file(wr_path, "Figure 3.4 ward residuals")) {
  wr <- read_csv(wr_path, show_col_types = FALSE)
  dup_lgas <- c("Funtua LGA", "Mani LGA", "Safana LGA", "Batagarawa LGA", "Katsina LGA", "Baure LGA")
  wr_top <- wr %>% slice_max(residual, n = 12) %>%
    mutate(ward_lab = paste0(facility_ward, "  (", str_remove(lga_name, " LGA"), ", ", str_sub(state, 1, 3), ")"),
           ward_lab = fct_reorder(ward_lab, residual),
           provisional = state == "Katsina" & lga_name %in% dup_lgas,
           flag_lab = if_else(provisional, "Katsina, duplication-affected list", "Not flagged"))
  
  fig_3_4 <- ggplot(wr_top, aes(x = residual, y = ward_lab)) +
    geom_segment(aes(x = 0, xend = residual, y = ward_lab, yend = ward_lab), colour = "#ddd", linewidth = 0.5) +
    geom_point(aes(fill = flag_lab, size = n_children), shape = 21, stroke = 0.7, colour = "white") +
    geom_text(aes(x = residual + 1.4, label = paste0("+", formatC(residual, format = "f", digits = 1), " pp")),
              hjust = 0, size = 2.9, family = "serif", colour = "#555") +
    scale_fill_manual(values = c("Katsina, duplication-affected list" = "#C0312D", "Not flagged" = "#1D6FA4")) +
    scale_size_continuous(range = c(2.5, 7), guide = "none") +
    scale_x_continuous(labels = label_number(suffix = " pp")) +
    labs(title = "Figure 3.4.  Ward-level residuals from the zero-dose model",
         subtitle = "Observed minus predicted ZD rate · top 12 wards · dot size = enrolled children",
         x = "Residual above model prediction (percentage points)", y = NULL,
         caption = "Computed live from 04_ward_residuals_classified.csv. Red points fall in Katsina LGAs on the duplication-affected\nfacility list (Fig 2.2) and may reflect recording artefacts rather than genuine unmet need.") +
    theme_diss(11) + theme(axis.text.y = element_text(size = 9))
} else {
  fig_3_4 <- placeholder_plot("MISSING INPUT\n04_ward_residuals_classified.csv")
}

artifacts$fig_3_4_path <- save_fig(fig_3_4, "fig_3_4", width = 8.6, height = 6.0)

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
    theme_diss(11)
  
  lag_meds <- lag_d %>% group_by(rec) %>% summarise(med = median(days_since_visit, na.rm = TRUE), .groups = "drop")
  pR35 <- ggplot(lag_d, aes(x = days_since_visit, fill = rec)) +
    geom_histogram(bins = 38, position = "identity", alpha = 0.55, colour = NA) +
    geom_vline(data = lag_meds, aes(xintercept = med, colour = rec), linetype = "dashed", linewidth = 0.8) +
    geom_text(data = lag_meds, aes(x = med, colour = rec, label = paste0(rec, "\nmedian ", med, " d")),
              y = Inf, vjust = c(1.4, 3.0), hjust = -0.05, size = 2.7,
              family = "serif", fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("Recovered" = col_confirmed, "Not recovered" = col_notrec)) +
    scale_colour_manual(values = c("Recovered" = col_confirmed, "Not recovered" = col_notrec), guide = "none") +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "B. Days since last visit, by recovery", x = "Days since last facility visit", y = "Attempts") +
    theme_diss(11)
  
  fig_3_5 <- (pL35 | pR35) + plot_layout(widths = c(1.25, 1)) +
    plot_annotation(title = "Figure 3.5.  Tracing outcomes and timing",
                    subtitle = paste0("N = ", comma(nrow(d35)), " tracing attempts, lag-time subset N = ", comma(nrow(lag_d))),
                    caption = "Computed live from 03_model_b_dataset.rds. Panel A: outcomes as share of attempts within each method and state.\nPanel B: lag-time subset only (children with a matched prior facility visit).",
                    theme = theme(plot.title = element_text(family = "serif", face = "bold", size = 12.5),
                                  plot.caption = element_text(family = "serif", size = 9, colour = "#888", hjust = 0, lineheight = 1.1)))
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
         sig = abs(estimate / se) > 1.96, star = map2_chr(estimate, se, star2),
         label = fct_rev(fct_inorder(label)))

fig_3_6 <- ggplot(coef_r36, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#aaa", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi, colour = sig), height = 0.16, linewidth = 0.9) +
  geom_point(aes(fill = sig), shape = 21, size = 3.8, stroke = 0.8, colour = "white") +
  geom_text(aes(x = ci_hi + 0.02, label = paste0(formatC(estimate, format = "f", digits = 3),
                                                 if_else(sig, star, " n.s."))), hjust = 0, size = 3.1, family = "serif", colour = "#333") +
  scale_colour_manual(values = c("TRUE" = "#BA7517", "FALSE" = col_nonsig), guide = "none") +
  scale_fill_manual(values = c("TRUE" = "#BA7517", "FALSE" = "#d5d5d5"), guide = "none") +
  labs(title = "Figure 3.6.  Recovery model — coefficient plot",
       subtitle = paste0("Outcome: confirmed in-network return · LGA fixed effects · LGA-clustered SE · N = ", b_n),
       x = "Coefficient (log-odds scale)", y = NULL,
       caption = "Computed live from 02_model_b_tracing_effectiveness.txt. Amber = significant at p < 0.01. Grey = not significant.") +
  theme_diss(11) + theme(axis.text.y = element_text(lineheight = 0.9))

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
    labs(title = "Figure 3.7.  Strict versus permissive recovery rate by state",
         subtitle = "Strict = confirmed in-network return · Permissive = including unverified off-network reports",
         x = NULL, y = "Recovery rate",
         caption = "Computed live from 03_model_b_dataset.rds. The gap is the share of reported recovery resting on unverifiable\noff-network claims.") +
    theme_diss(11)
} else {
  fig_3_7 <- placeholder_plot("MISSING INPUT\n03_model_b_dataset.rds")
}

artifacts$fig_3_7_path <- save_fig(fig_3_7, "fig_3_7", width = 8, height = 4.2)

########################################
# Figure 3.8 — Daily visit diagnostics #
########################################

pj_path <- file.path(era_dir, "06_panel_daily.rds")
if (require_file(pj_path, "Figure 3.8 daily visit diagnostics")) {
  pj38 <- readRDS(pj_path) %>% mutate(vd = as.Date(visit_date))
  
  hist_means <- pj38 %>% group_by(lga_clean) %>% summarise(mv = mean(n_visits, na.rm = TRUE), .groups = "drop")
  p_hist <- ggplot(pj38, aes(x = n_visits, fill = lga_clean)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.55, colour = NA) +
    geom_vline(data = hist_means, aes(xintercept = mv, colour = lga_clean), linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
    geom_text(data = hist_means, aes(x = mv, colour = lga_clean, label = paste0(lga_clean, "\nmean ", round(mv), " /day")),
              y = Inf, vjust = c(1.4, 3.0), hjust = -0.05, size = 2.4, family = "serif", fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(values = c("Gabasawa" = "#1D6FA4", "Ungogo" = "#BA7517")) +
    scale_colour_manual(values = c("Gabasawa" = "#1D6FA4", "Ungogo" = "#BA7517"), guide = "none") +
    scale_y_continuous(labels = comma) +
    labs(subtitle = "A. Visit count distribution", x = "Daily visits", y = "LGA-days") + theme_diss(10)
  
  dow_lab <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  p_dow <- pj38 %>% group_by(dow_num) %>% summarise(mv = mean(n_visits, na.rm = TRUE), .groups = "drop") %>%
    mutate(dl = factor(dow_lab[dow_num], levels = dow_lab), wk = dow_num %in% c(1, 6, 7)) %>%
    ggplot(aes(x = dl, y = mv, fill = wk)) + geom_col(width = 0.6, alpha = 0.9) +
    scale_fill_manual(values = c("TRUE" = "#C0312D", "FALSE" = "#1D6FA4"), guide = "none") +
    labs(subtitle = "B. Mean visits by weekday", x = NULL, y = "Mean visits") + theme_diss(10) +
    theme(axis.text.x = element_text(size = 8))
  
  p_zero <- pj38 %>% mutate(ym = floor_date(vd, "month"), z = as.integer(n_visits == 0)) %>%
    group_by(ym) %>% summarise(zd = sum(z), .groups = "drop") %>%
    ggplot(aes(x = ym, y = zd)) + geom_col(fill = "#BA7517", alpha = 0.8, width = 22) +
    scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
    labs(subtitle = "C. Zero-visit days per month", x = NULL, y = "Zero-visit days") + theme_diss(10) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
  
  fig_3_8 <- (p_hist | p_dow | p_zero) +
    plot_annotation(title = "Figure 3.8.  Daily facility visit diagnostics",
                    subtitle = "Kano · Ungogo and Gabasawa · Aug 2024 – Mar 2026",
                    caption = "Computed live from 06_panel_daily.rds. Counts are zero-inflated and right-skewed (A). Quiet days cluster on weekends (B)\nand around religious holidays, a calendar-driven rather than weather-driven pattern that motivates the null weather findings.",
                    theme = theme(plot.title = element_text(family = "serif", face = "bold", size = 12.5),
                                  plot.subtitle = element_text(family = "serif", size = 10, colour = "#666"),
                                  plot.caption = element_text(family = "serif", size = 9, colour = "#888", hjust = 0, lineheight = 1.1)))
} else {
  fig_3_8 <- placeholder_plot("MISSING INPUT\n06_panel_daily.rds")
}

artifacts$fig_3_8_path <- save_fig(fig_3_8, "fig_3_8", width = 9, height = 3.6)

#----------------------------------------------------------------------------

################################################
# Table (weather) — W1-W6, now genuinely real  #
# Reads 02 (precip), 06 (heat), 08 (NDVI)      #
################################################

precip_txt  <- file.path(chirps_dir, "02_regression_precip_visits.txt")
heat_txt    <- file.path(era_dir, "06_regression_daily.txt")
ndvi_txt    <- file.path(ndvi_dir, "08_regression_ndvi_kano.txt")

precip_ok <- require_file(precip_txt, "weather table — precipitation")
heat_ok   <- require_file(heat_txt, "weather table — heat (needs 06's new .txt export)")
ndvi_ok   <- require_file(ndvi_txt, "weather table — NDVI")

precip_parsed <- if (precip_ok) parse_ms_txt(precip_txt) else NULL
heat_parsed   <- if (heat_ok) parse_ms_txt(heat_txt) else NULL
ndvi_parsed   <- if (ndvi_ok) parse_ms_txt(ndvi_txt) else NULL

# W1: monthly precip anomaly, LGA FE (P1, col 1)
w1 <- extract_coef_se(precip_parsed, "precip_anomaly_pct", col = 1)
# W2: daily precip anomaly, LGA+DOW+month-year FE (P4, col 4)
w2 <- extract_coef_se(precip_parsed, "precip_anomaly_pct", col = 4)
# W3: binary extreme heat, LGA+DOW+month-year FE (D3, col 3)
w3 <- extract_coef_se(heat_parsed, "extreme_heat_38", col = 3)
# W4: continuous UTCI, LGA+DOW+month-year FE (D4, col 4)
w4 <- extract_coef_se(heat_parsed, "utci_dt_c", col = 4)
# W5: NDVI vim level, LGA FE (N1, col 1)
w5 <- extract_coef_se(ndvi_parsed, "vim_c", col = 1)
# W6: NDVI viq anomaly, LGA+month-year FE (N2, col 2)
w6 <- extract_coef_se(ndvi_parsed, "viq_c", col = 2)

w1_n <- fb(ev(precip_parsed, "Num.Obs", 1), "NA"); w1_r2 <- fb(ev(precip_parsed, "R2 ", 1), "NA")
w2_n <- fb(ev(precip_parsed, "Num.Obs", 4), "NA"); w2_r2 <- fb(ev(precip_parsed, "R2 ", 4), "NA")
w3_n <- fb(ev(heat_parsed, "Num.Obs", 3), "NA");   w3_r2 <- fb(ev(heat_parsed, "R2 ", 3), "NA")
w4_n <- fb(ev(heat_parsed, "Num.Obs", 4), "NA");   w4_r2 <- fb(ev(heat_parsed, "R2 ", 4), "NA")
w5_n <- fb(ev(ndvi_parsed, "Num.Obs", 1), "NA");   w5_r2 <- fb(ev(ndvi_parsed, "R2 ", 1), "NA")
w6_n <- fb(ev(ndvi_parsed, "Num.Obs", 2), "NA");   w6_r2 <- fb(ev(ndvi_parsed, "R2 ", 2), "NA")

tab_weather <- tribble(~Spec, ~Variable, ~Measure, ~Coef_CI, ~Panel, ~FE, ~N, ~R2,
                       "W1", "Precipitation", "Monthly precipitation anomaly (% of long-term average)", ci_cell(w1$coef, w1$se), "Monthly", "LGA", w1_n, w1_r2,
                       "W2", "Precipitation", "Daily precipitation anomaly (% of long-term average)",   ci_cell(w2$coef, w2$se), "Daily",   "LGA, DOW, month-year", w2_n, w2_r2,
                       "W3", "Heat", "Extreme heat day (UTCI ≥ 38°C, binary)",                ci_cell(w3$coef, w3$se), "Daily",   "LGA, DOW, month-year", w3_n, w3_r2,
                       "W4", "Heat", "Daytime mean UTCI (°C, centred)",                            ci_cell(w4$coef, w4$se), "Daily",   "LGA, DOW, month-year", w4_n, w4_r2,
                       "W5", "Vegetation", "NDVI seasonal level (vim, centred)",                        ci_cell(w5$coef, w5$se), "Monthly", "LGA", w5_n, w5_r2,
                       "W6", "Vegetation", "NDVI within-baseline anomaly (viq, centred)",               ci_cell(w6$coef, w6$se), "Monthly", "LGA, month-year", w6_n, w6_r2)
artifacts$tab_weather <- tab_weather

########################################
# Figure 3.10 slot — NB comparison,    #
# all three weather variables          #
########################################

precip_nb_txt <- file.path(chirps_dir, "02_regression_precip_nb_comparison.txt")
heat_nb_txt   <- file.path(era_dir, "06_regression_nb_comparison.txt")
ndvi_nb_txt   <- file.path(ndvi_dir, "08_regression_ndvi_nb_comparison.txt")

precip_nb_ok <- require_file(precip_nb_txt, "NB comparison — precipitation")
heat_nb_ok   <- require_file(heat_nb_txt, "NB comparison — heat")
ndvi_nb_ok   <- require_file(ndvi_nb_txt, "NB comparison — NDVI")

precip_nb_parsed <- if (precip_nb_ok) parse_ms_txt(precip_nb_txt) else NULL
heat_nb_parsed   <- if (heat_nb_ok) parse_ms_txt(heat_nb_txt) else NULL
ndvi_nb_parsed   <- if (ndvi_nb_ok) parse_ms_txt(ndvi_nb_txt) else NULL

precip_ols_nb <- extract_coef_se(precip_nb_parsed, "precip_anomaly_pct", col = 1)
precip_nb_nb  <- extract_coef_se(precip_nb_parsed, "precip_anomaly_pct", col = 2)
heat_ols_nb   <- extract_coef_se(heat_nb_parsed, "extreme_heat_38", col = 1)
heat_nb_nb    <- extract_coef_se(heat_nb_parsed, "extreme_heat_38", col = 2)
ndvi_ols_nb   <- extract_coef_se(ndvi_nb_parsed, "vim_c", col = 1)
ndvi_nb_nb    <- extract_coef_se(ndvi_nb_parsed, "vim_c", col = 2)

nb_ci <- function(coef, se) {
  if (is.na(coef) || is.na(se)) return("NA")
  paste0("[", round(coef - 1.96 * se, 4), ", ", round(coef + 1.96 * se, 4), "]")
}
nb_sig <- function(coef, se) { s <- star2(coef, se); if (s == "") "n.s." else s }

fig_3_10 <- tribble(~Variable, ~Specification, ~Coef, ~SE, ~CI, ~Sig,
                    "Precipitation", "OLS — log(visits + 1)",   precip_ols_nb$coef, precip_ols_nb$se, nb_ci(precip_ols_nb$coef, precip_ols_nb$se), nb_sig(precip_ols_nb$coef, precip_ols_nb$se),
                    "Precipitation", "Negative binomial — counts", precip_nb_nb$coef, precip_nb_nb$se, nb_ci(precip_nb_nb$coef, precip_nb_nb$se), nb_sig(precip_nb_nb$coef, precip_nb_nb$se),
                    "Heat",          "OLS — log(visits + 1)",   heat_ols_nb$coef, heat_ols_nb$se, nb_ci(heat_ols_nb$coef, heat_ols_nb$se), nb_sig(heat_ols_nb$coef, heat_ols_nb$se),
                    "Heat",          "Negative binomial — counts", heat_nb_nb$coef, heat_nb_nb$se, nb_ci(heat_nb_nb$coef, heat_nb_nb$se), nb_sig(heat_nb_nb$coef, heat_nb_nb$se),
                    "NDVI",          "OLS — log(visits + 1)",   ndvi_ols_nb$coef, ndvi_ols_nb$se, nb_ci(ndvi_ols_nb$coef, ndvi_ols_nb$se), nb_sig(ndvi_ols_nb$coef, ndvi_ols_nb$se),
                    "NDVI",          "Negative binomial — counts", ndvi_nb_nb$coef, ndvi_nb_nb$se, nb_ci(ndvi_nb_nb$coef, ndvi_nb_nb$se), nb_sig(ndvi_nb_nb$coef, ndvi_nb_nb$se))
artifacts$tab_3_10 <- fig_3_10

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

tab_4_1 <- tribble(~Theme, ~Finding, ~Field,
                   "Distance", paste0("Not significant under the permissive flag; ", amplification,
                                      "× larger and ", a2_dist_sig_label,
                                      " under the strict definition (β ", round(a1_dist_raw$coef, 3), " → ", round(a2_dist_raw$coef, 3), ")."),
                   "Recordkeeping", paste0("Strict recovery ", strict_pooled, "% vs permissive ", permissive_pooled, "%."),
                   "Weather", "Null across all three variables and all specifications (see weather table)."
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
    labs(title = "How many times is the same record being copied via sync?",
         subtitle = "Katsina only — Kano had no duplicates",
         x = "Number of times a record appears (duplicate set size)",
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
if (require_file(ladder_path, "Fig2A recovery ladder")) {
  ladder_datharm <- readRDS(ladder_path) %>%
    mutate(
      Level = case_when(
        Level == "L1" ~ "L1 — Maximalist", Level == "L2" ~ "L2 — Strict",
        Level == "L3" ~ "L3 — Matched",    Level == "L4" ~ "L4 — Verified"),
      Level = factor(Level, levels = c("L1 — Maximalist", "L2 — Strict", "L3 — Matched", "L4 — Verified")),
      state = factor(state, levels = c("Kano", "Katsina")))
  
  level_colour <- c("L1 — Maximalist" = "#27ae60", "L2 — Strict" = "#2980b9",
                    "L3 — Matched" = "#d68910", "L4 — Verified" = "#c0392b")
  
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
    labs(title = "\"Recovered\" defaulter rate varies widely by definition used",
         x = NULL, y = "Share of traced defaulters") +
    theme_datharm(12.5) +
    theme(plot.margin = margin(5.5, 46, 5.5, 5.5), strip.text = element_text(face = "bold", size = 12))
} else {
  fig2a_datharm <- placeholder_plot("MISSING INPUT\n09_recovery_ladder.rds")
  drops_datharm <- tibble()
}

artifacts_datharm$fig2a_path <- save_fig(fig2a_datharm, "datharm_fig2a", width = 8.5, height = 4.6)
artifacts_datharm$fig2a_drops <- drops_datharm

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
  ward_dead_datharm <- ward_dead_obj$by_ward %>%
    mutate(ward_lab = paste0(facility_ward, " (", lga_name, ")"),
           ward_lab = fct_reorder(ward_lab, deceased_rate_pct),
           flag = if_else(row_number() == which.max(deceased_rate_pct), "Highest", "Other"))
  kano_avg_dead <- ward_dead_obj$kano_avg
  
  fig5a_datharm <- ggplot(ward_dead_datharm, aes(x = ward_lab, y = deceased_rate_pct, fill = flag)) +
    geom_col(width = 0.65) +
    geom_hline(yintercept = kano_avg_dead, linetype = "dashed", colour = "#555") +
    annotate("text", x = 2, y = kano_avg_dead, label = paste0("Kano average: ", kano_avg_dead, "%"),
             vjust = -0.6, hjust = 0, size = 3.3, colour = "#555") +
    scale_fill_manual(values = c("Highest" = "#e74c3c", "Other" = "#b0b0b0"), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
    coord_flip() +
    labs(title = "This pattern would be visible in routine data review",
         subtitle = "It was only found in retrospective analysis",
         x = NULL, y = "Deceased-at-tracing rate",
         caption = "Computed live from 09_ward_deceased_rate.rds. 'Highest' marks the ward with the largest rate, not necessarily\nMekiya by name — verify the flagged ward matches the Mekiya finding described in the text before citing it as such.") +
    theme_datharm(12.5)
} else {
  fig5a_datharm <- placeholder_plot("MISSING INPUT\n09_ward_deceased_rate.rds")
  kano_avg_dead <- NA
}

artifacts_datharm$fig5a_path <- save_fig(fig5a_datharm, "datharm_fig5a", width = 8, height = 5.5)
artifacts_datharm$fig5a_kano_avg <- kano_avg_dead

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
    labs(title = "Highest single-day volumes", subtitle = "Katsina facilities",
         x = "Rank", y = "Visits / day",
         caption = "Computed live from 09_facility_day_volume.rds.") +
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
    labs(title = "Distribution of all facility-days",
         subtitle = paste0("n = ", comma(fac_vol_katsina$n_facility_days), " facility-days total"),
         x = NULL, y = "Facility-days",
         caption = paste0("Computed live from 09_facility_day_volume.rds. 95th percentile: ", fac_vol_katsina$p95,
                          "/day. 99th percentile: ", fac_vol_katsina$p99, "/day.")) +
    theme_datharm(12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
} else {
  fig5b_panel1_datharm <- placeholder_plot("MISSING INPUT\n09_facility_day_volume.rds")
  fig5b_panel2_datharm <- placeholder_plot("MISSING INPUT\n09_facility_day_volume.rds")
}

artifacts_datharm$fig5b_panel1_path <- save_fig(fig5b_panel1_datharm, "datharm_fig5b_panel1", width = 8, height = 4)
artifacts_datharm$fig5b_panel2_path <- save_fig(fig5b_panel2_datharm, "datharm_fig5b_panel2", width = 8, height = 4.5)

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
cat("3. Figure 3.2's waterfall now splits out Null-LGA and duplicate-row\n")
cat("   counts, but both are capped approximations against the residual\n")
cat("   gap, not an exact reconciliation — see the script comment above\n")
cat("   that chunk.\n")
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

#--------------------------(END)------------------------------#