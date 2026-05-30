########################################################
#  Ward-Level Residual Analysis                        #
#       Geographic Targeting from Model A Residuals    #
#  Created on 29/5/2026                                #
#  Last Updated 29/5/2026                              #
########################################################

# Reset environment -----------------------------------------------------

rm(list = ls())
setwd("C:/Users/HP/Documents/GitHub/datharm-placement")

# Turn off scientific notation globally
options(scipen = 999)
Sys.setlocale("LC_TIME", "English")

#Set link shortcuts
home         <- "C:/Users/HP/Documents/GitHub/datharm-placement"
reg_dir      <- file.path(home, "03_output/03_regression")
mchtrack_dir <- file.path(home, "03_output/01_mchtrack_data")
out_dir      <- file.path(home, "03_output/04_ward_residuals")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#Routine Packages
library(janitor)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scales)
library(fixest)

#----------------------------------------------------------------------------

###################
# Import Data     #
###################

# 1. Load model A dataset and residuals from 03_regression.R outputs ----------

data_model_a      <- readRDS(file.path(reg_dir, "03_model_a_dataset.rds"))
data_ward_resid   <- readRDS(file.path(reg_dir, "03_ward_residuals_model_a.rds"))

# 2. Validation summary -------------------------------------------------------

data_ward_resid %>%
  group_by(state) %>%
  summarise(
    n_wards         = n(),
    median_residual = round(median(residual), 1),
    mean_residual   = round(mean(residual),   1),
    max_residual    = round(max(residual),     1),
    min_residual    = round(min(residual),     1)
  ) %>%
  print()


############
# Analysis #
############

#----------------------------#Residual Classification#----------------------#

# 3. Classify wards by residual tier ------------------------------------------

#Thresholds: top 10% = high priority; bottom 10% = performing above expectation ----
resid_p90 <- quantile(data_ward_resid$residual, 0.90)
resid_p10 <- quantile(data_ward_resid$residual, 0.10)

data_ward_resid <- data_ward_resid %>%
  mutate(
    resid_tier = case_when(
      residual >= resid_p90  ~ "High priority",
      residual <= resid_p10  ~ "Outperforming",
      TRUE                   ~ "Expected range"
    ),
    resid_tier = factor(resid_tier,
                        levels = c("High priority","Expected range","Outperforming"))
  )

# 4. Tier summary table -------------------------------------------------------

data_ward_resid %>%
  group_by(state, resid_tier) %>%
  summarise(
    n_wards          = n(),
    mean_observed    = round(mean(zd_observed_pct), 1),
    mean_fitted      = round(mean(zd_fitted_pct),   1),
    mean_residual    = round(mean(residual),         1),
    total_children   = sum(n_children)
  ) %>%
  arrange(state, resid_tier) %>%
  print()


#----------------------------#Visualizations#--------------------------------#

# 5. Plot A: observed vs fitted ZD rate, coloured by residual tier -------------
# dots above the diagonal = higher ZD than predictors explain (concerning)
# dots below = lower ZD than predicted (outperforming)

plot_obs_vs_fitted <- ggplot(
  data_ward_resid,
  aes(x = zd_fitted_pct, y = zd_observed_pct,
      colour = resid_tier, size = n_children)
) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey60", linewidth = 0.7) +
  geom_point(alpha = 0.75) +
  scale_colour_manual(
    values = c(
      "High priority"  = "#C0312D",
      "Expected range" = "#85B7EB",
      "Outperforming"  = "#1D9E75"
    )
  ) +
  scale_size_continuous(range = c(1.5, 7), guide = "none") +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  facet_wrap(~state, scales = "free") +
  labs(
    title    = "Observed vs fitted zero-dose rate by ward",
    subtitle = "Above diagonal = higher ZD than model predicts · dot size = enrolled children",
    x        = "Fitted ZD rate (%)",
    y        = "Observed ZD rate (%)",
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "01_obs_vs_fitted_by_ward.png"),
       plot = plot_obs_vs_fitted, width = 10, height = 5, dpi = 150)

# 6. Plot B: ranked residual dot plot — top and bottom 20 wards ---------------

#Build top and bottom 20 for display ----
data_resid_ranked <- bind_rows(
  head(data_ward_resid, 20) %>% mutate(group = "Highest residual (under-performing)"),
  tail(data_ward_resid, 20) %>% mutate(group = "Lowest residual (over-performing)")
) %>%
  mutate(
    ward_label = paste0(facility_ward, "\n(", lga_name, ")"),
    ward_label = fct_reorder(ward_label, residual)
  )

plot_ranked <- ggplot(
  data_resid_ranked,
  aes(x = residual, y = ward_label, colour = resid_tier)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey60", linewidth = 0.7) +
  geom_segment(aes(x = 0, xend = residual,
                   y = ward_label, yend = ward_label),
               colour = "grey80", linewidth = 0.5) +
  geom_point(aes(size = n_children), alpha = 0.85) +
  scale_colour_manual(
    values = c(
      "High priority"  = "#C0312D",
      "Expected range" = "#85B7EB",
      "Outperforming"  = "#1D9E75"
    )
  ) +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  scale_x_continuous(labels = label_number(suffix = "pp")) +
  facet_wrap(~group, scales = "free_y") +
  labs(
    title    = "Ward-level residuals from zero-dose prediction model",
    subtitle = "Residual = observed ZD rate minus model-predicted rate · dot size = enrolled children",
    x        = "Residual (percentage points)",
    y        = NULL,
    colour   = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 8)
  )

ggsave(file.path(out_dir, "02_residual_ranked_wards.png"),
       plot = plot_ranked, width = 12, height = 9, dpi = 150)

# 7. Plot C: residual distribution by LGA — boxplot ---------------------------

plot_lga_box <- ggplot(
  data_ward_resid,
  aes(x = fct_reorder(lga_name, residual, .fun = median),
      y = residual, fill = state)
) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey60", linewidth = 0.7) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2,
               outlier.fill  = "white", alpha = 0.75) +
  geom_jitter(aes(size = n_children), width = 0.15,
              alpha = 0.5, colour = "grey30") +
  scale_fill_manual(values = c("Kano" = "#185FA5", "Katsina" = "#BA7517")) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = "pp")) +
  labs(
    title    = "Distribution of ward residuals within each LGA",
    subtitle = "Positive residual = ward has higher ZD than predicted by distance and age",
    x        = NULL,
    y        = "Residual (percentage points)",
    fill     = "State"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 35, hjust = 1)
  )

ggsave(file.path(out_dir, "03_residuals_by_lga.png"),
       plot = plot_lga_box, width = 11, height = 6, dpi = 150)

# 8. Plot D: ZD rate profile for high-priority wards — operational output -----
# stacked bar showing observed ZD vs non-ZD, sorted by residual
# designed to share directly with programme team

data_high_priority <- data_ward_resid %>%
  filter(resid_tier == "High priority") %>%
  arrange(desc(residual)) %>%
  mutate(
    ward_label    = paste0(facility_ward, " (", lga_name, ")"),
    ward_label    = fct_reorder(ward_label, residual),
    zd_count      = round(n_children * zd_observed_pct / 100),
    non_zd_count  = n_children - zd_count
  ) %>%
  select(ward_label, state, zd_count, non_zd_count,
         zd_observed_pct, zd_fitted_pct, residual, n_children) %>%
  pivot_longer(cols = c(zd_count, non_zd_count),
               names_to = "group", values_to = "count") %>%
  mutate(
    group = recode(group,
                   "zd_count"     = "Zero-dose",
                   "non_zd_count" = "Vaccinated")
  )

plot_priority <- ggplot(
  data_high_priority,
  aes(x = ward_label, y = count, fill = group)
) +
  geom_col(width = 0.7) +
  geom_text(
    data = data_high_priority %>%
      filter(group == "Zero-dose") %>%
      distinct(ward_label, zd_observed_pct, zd_fitted_pct, residual),
    aes(x = ward_label, label = paste0(zd_observed_pct, "%\n(+", round(residual, 1), "pp)"),
        y = Inf),
    inherit.aes = FALSE,
    vjust = 1.3, size = 3, colour = "#791F1F", fontface = "bold"
  ) +
  scale_fill_manual(values = c("Zero-dose" = "#C0312D", "Vaccinated" = "#E8E6E0")) +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  facet_wrap(~state, scales = "free_y") +
  labs(
    title    = "High-priority wards: enrolled children by vaccination status",
    subtitle = "Label = observed ZD rate (residual above model prediction) · sorted by residual",
    x        = NULL,
    y        = "Enrolled children",
    fill     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "top",
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 9)
  )

ggsave(file.path(out_dir, "04_high_priority_wards.png"),
       plot = plot_priority, width = 11, height = 8, dpi = 150)


##########
# Export #
##########

#Save full residuals table with tier classification ----
saveRDS(data_ward_resid,
        file.path(out_dir, "04_ward_residuals_classified.rds"))

write_csv(data_ward_resid,
          file.path(out_dir, "04_ward_residuals_classified.csv"))

#Save high priority ward list for programme team ----
data_ward_resid %>%
  filter(resid_tier == "High priority") %>%
  select(state, lga_name, facility_ward,
         n_children, zd_observed_pct, zd_fitted_pct, residual) %>%
  arrange(desc(residual)) %>%
  write_csv(file.path(out_dir, "04_high_priority_wards_list.csv"))

cat("All outputs saved to:", out_dir, "\n")
cat("  01_obs_vs_fitted_by_ward.png\n")
cat("  02_residual_ranked_wards.png\n")
cat("  03_residuals_by_lga.png\n")
cat("  04_high_priority_wards.png\n")
cat("  04_ward_residuals_classified.rds\n")
cat("  04_ward_residuals_classified.csv\n")
cat("  04_high_priority_wards_list.csv\n")

#--------------------------(END)------------------------------#