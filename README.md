# MCHTrack Zero-Dose Analysis

R pipeline and dissertation source behind the analysis of DATHARM's MCHTrack programme data (Kano and Katsina, Nigeria).

Raw MCHTrack data is not included (data use agreement, not redistributable). Point the import scripts at your own local copy.

## Pipeline (run in order)

| Script | Purpose |
|---|---|
| `01_mchtrack_import.R` | Import/clean MCHTrack tables; dedupe; apply exclusions |
| `02_chirps_import_analysis.R` | Rainfall import + rainfall weather model |
| `03_regression.R` | Zero-dose and recovery regressions |
| `04_ward_residuals.R` | Ward-level residuals |
| `05_era5_import.R` | Heat (ERA5-HEAT/UTCI) import |
| `06_era5_analysis.R` | Heat weather model |
| `07_ndvi_import.R` | Vegetation (NDVI) import |
| `08_ndvi_analysis.R` | NDVI weather model |
| `09_data_investigations.R` | Data-quality audit |
| `10_visualizations.R` | All figures/tables for the dissertation |

Each script writes `.rds` output to `03_output/<script_name>/`.

## Dissertation

- `11_dissertation_setup.R` — shared helpers, sourced by both formats
- `11_dissertation_body.Rmd` — full content (not knitted directly)
- `11_dissertation_html.Rmd` / `11_dissertation_word.Rmd` — output wrappers
- `11_word_reference.docx` — Word styling template

Run `10_visualizations.R` first, then knit either wrapper Rmd.

## Requirements

R, with `dplyr`, `ggplot2`, `fixest`, `knitr`, `kableExtra`, `rmarkdown`, `sf`.
