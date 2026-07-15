########################################
#  09_data_investigations.R            #
#  Created: 13/7/2026                  #
#  Updated: 15/7/2026                  #
########################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

home    <- "C:/Users/HP/Documents/GitHub/datharm-placement"
mch_dir <- file.path(home, "03_output/01_mchtrack_data")
out_dir <- file.path(home, "03_output/09_data_investigations")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

library(janitor)
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)

#----------------------------------------------------------------------------

###################
# Load 01 outputs #
###################

data_ll <- readRDS(file.path(mch_dir, "01_linelisted_clean.rds"))
data_fv <- readRDS(file.path(mch_dir, "01_facility_visits_clean.rds"))
data_zd <- readRDS(file.path(mch_dir, "01_identifiedzd_clean.rds"))
data_dt <- readRDS(file.path(mch_dir, "01_defaultertracing_clean.rds"))

cat("Loaded from 01_mchtrack_import.R outputs:\n")
cat("  linelisted:        ", nrow(data_ll), "\n")
cat("  facility_visits:   ", nrow(data_fv), "\n")
cat("  identified_zd:     ", nrow(data_zd), "\n")
cat("  defaulter_tracing: ", nrow(data_dt), "\n\n")

# NOTE: 01_mchtrack_import.R does not deduplicate any of these tables at
# the row level (confirmed by reading it directly — its group_by()/
# distinct() calls are all for building aggregate panels, none drop
# duplicate rows from the tables above). Everything below that reports a
# duplicate rate is reporting on data that is STILL duplicated at this
# point in the pipeline, unless this script's own deduplicated exports
# (Section 1) are the ones downstream scripts actually load.

#----------------------------------------------------------------------------

##########################################################
# 1. Duplicate-record detection and deduplication         #
# Feeds: thesis fig-2-2, DATHARM audit fig1a/table1a       #
##########################################################

# Compound keys match the DATHARM audit's original methodology exactly:
#   linelisted:       pseudo_id
#   facility_visits:  patient_id + visit_date + health_center_id +
#                      vaccines_administered
# Blank/NA key rows excluded from duplicate detection (can't be compared).

detect_duplicates <- function(df, key_cols, table_label) {
  df_keyed <- df %>%
    filter(if_all(all_of(key_cols), ~ !is.na(.) & . != ""))
  set_sizes <- df_keyed %>%
    count(across(all_of(key_cols)), name = "set_size")
  summary_by_state <- df_keyed %>%
    left_join(set_sizes, by = key_cols) %>%
    group_by(state) %>%
    summarise(
      total_rows       = n(),
      duplicate_rows    = sum(set_size > 1),
      pct_duplicate     = round(duplicate_rows / total_rows * 100, 1),
      .groups = "drop"
    )
  dist <- df_keyed %>%
    left_join(set_sizes, by = key_cols) %>%
    filter(set_size > 1) %>%
    distinct(across(all_of(key_cols)), state, set_size) %>%
    count(state, set_size, name = "n_sets") %>%
    mutate(rows_from_duplication = n_sets * set_size,
           table = table_label)
  list(summary = summary_by_state, distribution = dist, keyed = df_keyed %>% left_join(set_sizes, by = key_cols))
}

dup_ll <- detect_duplicates(data_ll, "pseudo_id", "Linelisted (children enrolled)")
dup_fv <- detect_duplicates(data_fv, c("patient_id", "visit_date", "health_center_id", "vaccines_administered"),
                            "Facility visits (vaccination records)")

cat("--- Duplicate summary: linelisted ---\n"); print(dup_ll$summary)
cat("\n--- Duplicate summary: facility visits ---\n"); print(dup_fv$summary)
cat("\n")

dup_summary_all <- bind_rows(
  dup_ll$summary %>% mutate(table = "Linelisted (children enrolled)"),
  dup_fv$summary %>% mutate(table = "Facility visits (vaccination records)")
)

# Set-size distribution, pooling rare sizes into "Other" — same treatment
# as the original DATHARM audit chart, but computed live instead of typed
# in. Which sizes count as "rare" (i.e. get pooled) is data-driven here:
# any size representing fewer than 1% of duplicated rows within its table,
# rather than a hand-picked list, so this doesn't need updating by hand if
# the underlying data changes. STATE IS KEPT (not collapsed) so a
# downstream visualization script can build either an all-states chart
# (thesis) or a Katsina-only chart (DATHARM audit fig1a, which never
# covered Kano since Kano had no duplicates) from the same export.
# NOTE: no plot is built or saved here — this script produces numbers
# only. All chart-building lives in 10_visualizations.R, matching the
# same compute/presentation split already applied to the thesis's 09/10
# scripts and now extended to the DATHARM audit doc's inputs too.
build_dist_for_plot <- function(dist) {
  dist %>%
    group_by(table) %>%
    mutate(pct_of_dup_rows = rows_from_duplication / sum(rows_from_duplication)) %>%
    ungroup() %>%
    mutate(set_size_label = if_else(pct_of_dup_rows < 0.01, "Other", as.character(set_size))) %>%
    group_by(table, state, set_size_label) %>%
    summarise(rows_from_duplication = sum(rows_from_duplication), .groups = "drop")
}

dup_dist_plot <- bind_rows(dup_ll$distribution, dup_fv$distribution) %>%
  build_dist_for_plot()

# Sample of actual duplicate rows, for the "here's what one looks like" table
dup_sample_ll <- dup_ll$keyed %>% filter(set_size > 1) %>% arrange(pseudo_id) %>% head(6)
dup_sample_fv <- dup_fv$keyed %>% filter(set_size > 1) %>% arrange(patient_id, visit_date) %>% head(6)

#--- Deduplicated exports — the actual fix, not just the report -------------
# IMPORTANT: 03_regression.R, and any
# other script currently reading 01_linelisted_clean.rds /
# 01_facility_visits_clean.rds directly, should be updated to read these
# deduplicated versions instead, or Katsina-inclusive results (RQ1, RQ2)
# remain built on the same inflated data the audit flagged. This has not
# been done yet in the scripts already rewritten today — flagging clearly
# rather than silently leaving it broken.

data_ll_deduped <- dup_ll$keyed %>%
  group_by(across(-set_size)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-set_size)

data_fv_deduped <- dup_fv$keyed %>%
  group_by(across(-set_size)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-set_size)

cat("Deduplication result:\n")
cat("  linelisted:      ", nrow(data_ll), "->", nrow(data_ll_deduped),
    "(", nrow(data_ll) - nrow(data_ll_deduped), "rows removed)\n")
cat("  facility_visits: ", nrow(data_fv), "->", nrow(data_fv_deduped),
    "(", nrow(data_fv) - nrow(data_fv_deduped), "rows removed)\n\n")

saveRDS(dup_summary_all,     file.path(out_dir, "09_dedup_summary_by_state.rds"))
saveRDS(dup_dist_plot,       file.path(out_dir, "09_dedup_set_size_distribution.rds"))
saveRDS(list(linelisted = dup_sample_ll, facility_visits = dup_sample_fv),
        file.path(out_dir, "09_dedup_sample_rows.rds"))
saveRDS(data_ll_deduped, file.path(out_dir, "09_linelisted_deduped.rds"))
saveRDS(data_fv_deduped, file.path(out_dir, "09_facility_visits_deduped.rds"))

#----------------------------------------------------------------------------

##########################################################
# 2. Unattributed ("Null") LGA rows                       #
# Feeds: thesis fig-3-2 waterfall                          #
##########################################################

# "Null LGA" in the original waterfall chart meant rows where lga_name
# could not be attributed to a real LGA after cleaning. clean_lga_name()
# in 01_mchtrack_import.R doesn't appear to introduce an explicit "Null"
# sentinel, so this checks for NA / blank / literal "NA" string, which is
# the closest verifiable proxy. Flag to check against the real output
# once run — if this comes back 0, the original chart's "Null LGA" step
# may have meant something more specific that needs tracing separately.

null_lga_summary <- data_ll %>%
  mutate(lga_is_null = is.na(lga_name) | lga_name == "" | str_to_lower(lga_name) == "na") %>%
  summarise(n_null_lga = sum(lga_is_null), pct_null_lga = round(mean(lga_is_null) * 100, 2))

null_lga_by_state <- data_ll %>%
  mutate(lga_is_null = is.na(lga_name) | lga_name == "" | str_to_lower(lga_name) == "na") %>%
  group_by(state) %>%
  summarise(n_null_lga = sum(lga_is_null), pct_null_lga = round(mean(lga_is_null) * 100, 2), .groups = "drop")

cat("--- Unattributed LGA rows (linelisted) ---\n"); print(null_lga_summary); cat("\n")
cat("--- Unattributed LGA rows by state ---\n"); print(null_lga_by_state); cat("\n")

saveRDS(null_lga_summary, file.path(out_dir, "09_null_lga_summary.rds"))
saveRDS(null_lga_by_state, file.path(out_dir, "09_null_lga_by_state.rds"))

#----------------------------------------------------------------------------

##########################################################
# 3. Recovery verification ladder (L1-L4)                 #
# Feeds: DATHARM audit fig2a/table-ladder-legend            #
##########################################################

# L1 Maximalist: yes_ok or yes_off_network_care
# L2 Strict:     yes_ok only
# L3 Matched:    yes_ok AND a facility_visits record exists for that
#                patient_id at any time (not lag-restricted, unlike
#                03_regression.R's days_since_visit construction — this is
#                deliberately looser, matching the audit's own definition)
# L4 Verified:   L3, and that matched facility_visits record names a
#                vaccine (vaccines_administered not blank)

fv_by_patient <- data_fv %>%
  filter(!is.na(vaccines_administered) & vaccines_administered != "") %>%
  distinct(patient_id) %>%
  mutate(has_named_vaccine_visit = TRUE)

fv_any_by_patient <- data_fv %>%
  distinct(patient_id) %>%
  mutate(has_any_visit = TRUE)

ladder_base <- data_dt %>%
  left_join(fv_any_by_patient, by = "patient_id") %>%
  left_join(fv_by_patient, by = "patient_id") %>%
  mutate(
    l1 = tracing_outcome %in% c("yes_ok", "yes_off_network_care"),
    l2 = tracing_outcome == "yes_ok",
    l3 = l2 & !is.na(has_any_visit),
    l4 = l3 & !is.na(has_named_vaccine_visit)
  )

# l1 uses %in%, which never returns NA even when tracing_outcome is NA.
# l2 uses ==, which does propagate NA. l3/l4 inherit any NA from l2. So a
# state with any NA tracing_outcome rows would previously return Pct = NA
# for L2-L4 only (L1 still fine), leaking through the ggplot as bars that
# just don't draw -- exactly the missing-Kano-bars pattern. Fixed with
# na.rm = TRUE, and NA-outcome rows are now surfaced explicitly instead of
# silently vanishing from the denominator's numerator only.
n_na_outcome <- ladder_base %>%
  group_by(state) %>%
  summarise(n_na_tracing_outcome = sum(is.na(tracing_outcome)), n_total = n(), .groups = "drop")
cat("--- NA tracing_outcome rows by state (denominator check for ladder) ---\n")
print(n_na_outcome); cat("\n")

recovery_ladder <- ladder_base %>%
  group_by(state) %>%
  summarise(
    n_traced = n(),
    L1 = round(mean(l1, na.rm = TRUE) * 100, 1),
    L2 = round(mean(l2, na.rm = TRUE) * 100, 1),
    L3 = round(mean(l3, na.rm = TRUE) * 100, 1),
    L4 = round(mean(l4, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  pivot_longer(c(L1, L2, L3, L4), names_to = "Level", values_to = "Pct")

cat("--- Recovery ladder (L1-L4) by state ---\n"); print(recovery_ladder); cat("\n")

saveRDS(recovery_ladder, file.path(out_dir, "09_recovery_ladder.rds"))
saveRDS(n_na_outcome, file.path(out_dir, "09_ladder_na_outcome_by_state.rds"))

#----------------------------------------------------------------------------

##########################################################
# 4. Off-network care share by state                      #
# Feeds: DATHARM audit table2a narrative, cross-check for  #
# thesis fig-3-7                                            #
##########################################################

offnetwork_share <- data_dt %>%
  group_by(state) %>%
  summarise(
    n_total = n(),
    n_offnetwork = sum(tracing_outcome == "yes_off_network_care", na.rm = TRUE),
    pct_offnetwork = round(n_offnetwork / n_total * 100, 1),
    .groups = "drop"
  )

cat("--- Off-network care share by state ---\n"); print(offnetwork_share); cat("\n")

saveRDS(offnetwork_share, file.path(out_dir, "09_offnetwork_share_by_state.rds"))

# Real sample of off-network-care rows, both states, for a table2a-style
# illustration. vaccine_on_card is not a field defaulter_tracing collects
# at all (that gap is the whole point of this recommendation) — included
# here as an explicit NOT RECORDED column rather than omitted, so the
# table visibly shows the missing field instead of just not mentioning it.
offnetwork_sample <- data_dt %>%
  filter(tracing_outcome == "yes_off_network_care") %>%
  mutate(vaccine_on_card = "NOT RECORDED") %>%
  select(patient_id, state, tracing_outcome, any_of("tracing_method"), any_of("tracing_date"), vaccine_on_card) %>%
  slice_sample(n = min(8, nrow(.)))

saveRDS(offnetwork_sample, file.path(out_dir, "09_offnetwork_sample.rds"))

#----------------------------------------------------------------------------

##########################################################
# 5. Zero-dose table reconciliation (resolved vs verified) #
# Feeds: DATHARM audit table3b                              #
##########################################################

# CAVEAT, read before trusting this section: the original audit table used
# an EXPANDED identified_zd export DATHARM's data manager built specially
# for this study — identification_date, resolution_date and
# resolution_reason are not columns 01_mchtrack_import.R currently selects
# from the standard identified_zd raw file, and may not exist in the
# regular export at all. This block uses identified_zd's own visit_date as
# a best-effort proxy for a resolution-adjacent date. If you have that
# expanded export saved separately, point this section at it instead and
# drop the proxy — the numbers below should NOT be treated as a
# reproduction of the original table3b until that's confirmed.

if ("visit_date" %in% names(data_zd)) {
  
  # data_zd's child-identifier column name hasn't been confirmed directly
  # against 01_mchtrack_import.R's data_zd_clean select() in this session.
  # facility_visits and defaulter_tracing both key on patient_id, but the
  # DATHARM audit's own table3b used "child_id" as its column label for
  # this exact table, so this checks both rather than guessing.
  zd_id_col <- if ("patient_id" %in% names(data_zd)) "patient_id" else
    if ("child_id" %in% names(data_zd)) "child_id" else NA_character_
  
  if (is.na(zd_id_col)) {
    cat("SKIPPED: ZD reconciliation — no patient_id or child_id column found",
        "on identified_zd. Check the real column name and update zd_id_col.\n\n")
  } else {
    
    zd_resolved <- data_zd %>%
      filter(tracing_outcome %in% c("yes_ok", "yes_off_network_care"), !is.na(visit_date)) %>%
      rename(zd_child_id = all_of(zd_id_col))
    
    # Non-cartesian join: check per-child whether ANY facility_visits row
    # falls within 90 days after the zd record's own visit_date (the proxy
    # resolution date — see caveat above), and capture the health centre
    # name from the zd record itself for the sample table below.
    zd_check <- zd_resolved %>%
      rowwise() %>%
      mutate(
        has_visit_within_90d = any(
          data_fv$patient_id == zd_child_id &
            data_fv$visit_date >= visit_date &
            data_fv$visit_date <= visit_date + days(90),
          na.rm = TRUE
        )
      ) %>%
      ungroup()
    
    zd_reconciliation <- zd_check %>%
      group_by(state) %>%
      summarise(
        n_resolved = n(),
        n_no_matching_visit = sum(!has_visit_within_90d),
        pct_no_matching_visit = round(mean(!has_visit_within_90d) * 100, 1),
        .groups = "drop"
      )
    
    cat("--- ZD reconciliation (PROXY — see caveat above) ---\n")
    print(zd_reconciliation); cat("\n")
    saveRDS(zd_reconciliation, file.path(out_dir, "09_zd_reconciliation_proxy.rds"))
    
    # Sample of unresolved-match rows for a table3b-style illustration.
    # health_center/facility_ward field name also unconfirmed against the
    # real data_zd schema — grabs whichever of the two exists.
    hc_col <- if ("health_center_id" %in% names(zd_check)) "health_center_id" else
      if ("facility_ward" %in% names(zd_check)) "facility_ward" else NA_character_
    
    zd_sample <- zd_check %>%
      filter(!has_visit_within_90d) %>%
      { if (!is.na(hc_col)) rename(., health_centre = all_of(hc_col)) else mutate(., health_centre = NA_character_) } %>%
      select(zd_child_id, state, any_of("health_centre"), tracing_outcome, visit_date, has_visit_within_90d) %>%
      head(8)
    
    saveRDS(zd_sample, file.path(out_dir, "09_zd_reconciliation_sample.rds"))
  }
} else {
  cat("SKIPPED: ZD reconciliation — identified_zd has no visit_date field",
      "to use as a resolution-date proxy. Table3b needs the expanded",
      "DATHARM export (identification_date/resolution_date/resolution_reason)",
      "confirmed available before this can be built at all.\n\n")
}

#----------------------------------------------------------------------------

##########################################################
# 6. Ward-level tracing-outcome anomaly check              #
# Feeds: DATHARM audit table5a/fig5a (Mekiya pattern)       #
##########################################################

cat("--- Distinct tracing_outcome values (verify 'deceased' label below) ---\n")
print(data_dt %>% count(tracing_outcome) %>% arrange(desc(n)))
cat("\n")

# Uses str_detect rather than an exact match since the precise string used
# for a deceased-child outcome in the raw data hasn't been confirmed in
# this session (no distinct-value printout was available beforehand).
# Check the printout above against this pattern before trusting the chart.
deceased_pattern <- "decease|dead|died"

ward_outcome_rates <- data_dt %>%
  filter(state == "Kano") %>%
  mutate(is_deceased = str_detect(str_to_lower(tracing_outcome), deceased_pattern)) %>%
  group_by(lga_name, facility_ward) %>%
  summarise(n_traced = n(), n_deceased = sum(is_deceased), .groups = "drop") %>%
  filter(n_traced >= 30) %>%
  mutate(deceased_rate_pct = round(n_deceased / n_traced * 100, 2)) %>%
  arrange(desc(deceased_rate_pct))

kano_avg_deceased <- ward_outcome_rates %>%
  summarise(avg = round(sum(n_deceased) / sum(n_traced) * 100, 2)) %>% pull(avg)

cat("--- Ward-level deceased-at-tracing rate, Kano, wards with n>=30 ---\n")
print(head(ward_outcome_rates, 15))
cat("Kano-wide average:", kano_avg_deceased, "%\n\n")

saveRDS(list(by_ward = ward_outcome_rates, kano_avg = kano_avg_deceased),
        file.path(out_dir, "09_ward_deceased_rate.rds"))

#----------------------------------------------------------------------------

##########################################################
# 7. Facility-day visit-volume anomaly detection           #
# Feeds: DATHARM audit fig5b (panels 1 and 2)               #
##########################################################

facility_day_volume <- data_fv %>%
  group_by(state, health_center_id, visit_date) %>%
  summarise(n_visits = n(), .groups = "drop")

build_volume_outputs <- function(df, state_label) {
  d <- df %>% filter(state == state_label)
  top40 <- d %>% arrange(desc(n_visits)) %>% head(40) %>%
    mutate(rank = row_number(),
           tier = cut(n_visits, breaks = c(0, 30, 50, 100, Inf),
                      labels = c("30-49", "50-99", "100-199", "200+")))
  bands <- d %>%
    mutate(band = cut(n_visits, breaks = c(0, 4, 9, 19, 29, 49, 99, 199, Inf),
                      labels = c("1-4", "5-9", "10-19", "20-29", "30-49", "50-99", "100-199", "200+"))) %>%
    count(band, name = "N") %>%
    mutate(Pct = round(N / sum(N) * 100, 1))
  p95 <- round(quantile(d$n_visits, 0.95, na.rm = TRUE), 0)
  p99 <- round(quantile(d$n_visits, 0.99, na.rm = TRUE), 0)
  list(state = state_label, n_facility_days = nrow(d), top40 = top40, bands = bands, p95 = p95, p99 = p99)
}

vol_katsina <- build_volume_outputs(facility_day_volume, "Katsina")
vol_kano    <- build_volume_outputs(facility_day_volume, "Kano")

cat("--- Facility-day volume, Katsina ---\n")
cat("  n facility-days:", vol_katsina$n_facility_days, "\n")
cat("  95th pctile:", vol_katsina$p95, "/day | 99th pctile:", vol_katsina$p99, "/day\n\n")
cat("--- Facility-day volume, Kano (not in original audit doc, added for completeness) ---\n")
cat("  n facility-days:", vol_kano$n_facility_days, "\n")
cat("  95th pctile:", vol_kano$p95, "/day | 99th pctile:", vol_kano$p99, "/day\n\n")

saveRDS(list(katsina = vol_katsina, kano = vol_kano),
        file.path(out_dir, "09_facility_day_volume.rds"))

#----------------------------------------------------------------------------

##########################################################
# 8. Ward-name spelling variants — already fixed upstream #
##########################################################

# clean_ward_name() in 01_mchtrack_import.R already collapses known
# "Garindanga" spelling variants into "Garun Danga" (regex match on
# "^Garindanga.*"). This check confirms the fix is holding rather than
# re-investigating a problem that's already been addressed — if this
# comes back non-empty, something changed in 01's cleaning function.

remaining_variants <- data_ll %>%
  filter(str_detect(str_to_lower(facility_ward), "garindanga")) %>%
  count(facility_ward)

if (nrow(remaining_variants) > 0) {
  cat("WARNING: 'Garindanga' spelling variants still present — check\n")
  cat("clean_ward_name() in 01_mchtrack_import.R:\n")
  print(remaining_variants)
} else {
  cat("Ward-name spelling check: no remaining 'Garindanga' variants —",
      "clean_ward_name()'s existing fix is holding.\n")
}
cat("\n")

#----------------------------------------------------------------------------

##########################################################
# 9. Rimi backfill — NOT reproducible, documented gap      #
##########################################################

cat("--- Rimi backfill check: SKIPPED ---\n")
cat("The original audit's 'visit_date substantially earlier than created_on'\n")
cat("finding needs a created_on field on facility_visits. This was already\n")
cat("searched for directly in an earlier session across every facility_visits\n")
cat("export, raw and clean, both states — it does not exist. rimi_flag remains\n")
cat("a name-based flag only (lga_name == \"Rimi LGA\"), not a reproducible\n")
cat("quantitative backfill metric. If DATHARM can supply a created_on field\n")
cat("for facility_visits, this section can be built; until then, Rimi's\n")
cat("exclusion rests on the qualitative pattern already documented in the\n")
cat("audit, not on a number this pipeline can regenerate.\n\n")

#----------------------------------------------------------------------------

##########
# Export #
##########

cat("=== ALL INVESTIGATIONS COMPLETE ===\n")
cat("Outputs saved to:", out_dir, "\n\n")
cat("  09_dedup_summary_by_state.rds\n")
cat("  09_dedup_set_size_distribution.rds\n")
cat("  09_dedup_sample_rows.rds\n")
cat("  09_linelisted_deduped.rds          <- downstream scripts should switch to this\n")
cat("  09_facility_visits_deduped.rds     <- downstream scripts should switch to this\n")
cat("  09_null_lga_summary.rds\n")
cat("  09_null_lga_by_state.rds\n")
cat("  09_recovery_ladder.rds\n")
cat("  09_ladder_na_outcome_by_state.rds\n")
cat("  09_offnetwork_share_by_state.rds\n")
cat("  09_offnetwork_sample.rds\n")
cat("  09_zd_reconciliation_proxy.rds     <- PROXY, see Section 5 caveat\n")
cat("  09_zd_reconciliation_sample.rds    <- PROXY, see Section 5 caveat\n")
cat("  09_ward_deceased_rate.rds          <- verify 'deceased_pattern' first, see Section 6\n")
cat("  09_facility_day_volume.rds\n")
cat("  (Rimi backfill and ward-spelling checks print to console only,\n")
cat("   nothing to export — one is a confirmed non-issue, the other a\n")
cat("   confirmed non-reproducible metric)\n\n")

cat("--- Open items before this feeds anything downstream ---\n")
cat("1. Confirm 01_identifiedzd_clean.rds / 01_defaultertracing_clean.rds are\n")
cat("   the actual saved filenames from 01_mchtrack_import.R -- inferred from\n")
cat("   its naming pattern (01_linelisted_clean.rds, 01_facility_visits_clean.rds)\n")
cat("   but not confirmed directly against its Export section in this session.\n")
cat("2. Decide whether 03_regression.R should be updated to read\n")
cat("   09_linelisted_deduped.rds / 09_facility_visits_deduped.rds instead of\n")
cat("   01's originals -- as written today it still reads the un-deduplicated\n")
cat("   tables, which undercuts the fix this script produces.\n")
cat("3. Verify deceased_pattern (Section 6) against the real tracing_outcome\n")
cat("   value list printed above before trusting the ward death-rate chart.\n")
cat("4. Section 5's ZD reconciliation is a proxy, not a reproduction --\n")
cat("   confirm whether the expanded identified_zd export DATHARM's data\n")
cat("   manager built is available as a separate file before citing it.\n")

#--------------------------(END)------------------------------#
