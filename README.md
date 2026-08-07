# MSc International Health and Tropical Medicine Dissertation | Suntiparp (Khem) Somsak 

R pipeline and dissertation source behind the analysis of DATHARM's MCHTrack programme data (Kano and Katsina, Nigeria). Submitted as partial fulfillment of requirements for the MSc IHTM at University of Oxford, August 2026.

---

### 📄 [Read the dissertation](https://khemsomsak.github.io/datharm-placement/11_dissertation_html.html)

---

Raw MCHTrack data is not included (data use agreement, not redistributable). Point the import scripts at your own local copy.

## Pipeline (run in order)

| Script | Purpose |
| --- | --- |
| [`01_mchtrack_import.R`](https://github.com/khemsomsak/datharm-placement/blob/main/01_mchtrack_import.R) | Import/clean MCHTrack tables; dedupe; apply exclusions |
| [`02_chirps_import_analysis.R`](https://github.com/khemsomsak/datharm-placement/blob/main/02_chirps_import_analysis.R) | Rainfall import + rainfall weather model |
| [`03_regression.R`](https://github.com/khemsomsak/datharm-placement/blob/main/03_regression.R) | Zero-dose and recovery regressions |
| [`04_ward_residuals.R`](https://github.com/khemsomsak/datharm-placement/blob/main/04_ward_residuals.R) | Ward-level residuals |
| [`05_era5_import.R`](https://github.com/khemsomsak/datharm-placement/blob/main/05_era5_import.R) | Heat (ERA5-UTCI) import |
| [`06_era5_analysis.R`](https://github.com/khemsomsak/datharm-placement/blob/main/06_era5_analysis.R) | Heat weather model |
| [`07_ndvi_import.R`](https://github.com/khemsomsak/datharm-placement/blob/main/07_ndvi_import.R) | Vegetation (NDVI) import |
| [`08_ndvi_analysis.R`](https://github.com/khemsomsak/datharm-placement/blob/main/08_ndvi_analysis.R) | NDVI weather model |
| [`09_data_investigations.R`](https://github.com/khemsomsak/datharm-placement/blob/main/09_data_investigations.R) | Data-quality audit |
| [`10_visualizations.R`](https://github.com/khemsomsak/datharm-placement/blob/main/10_visualizations.R) | All figures/tables for the dissertation |

Each script writes `.rds` output to `03_output/<script_name>/`.

## Dissertation

- [`11_dissertation_setup.R`](https://github.com/khemsomsak/datharm-placement/blob/main/11_dissertation_setup.R) — shared helpers, sourced by both formats
- [`11_dissertation_body.Rmd`](https://github.com/khemsomsak/datharm-placement/blob/main/11_dissertation_body.Rmd) — full content (not knitted directly)
- [`11_dissertation_html.Rmd`](https://github.com/khemsomsak/datharm-placement/blob/main/11_dissertation_html.Rmd) / [`11_dissertation_word.Rmd`](https://github.com/khemsomsak/datharm-placement/blob/main/11_dissertation_word.Rmd) — output wrappers
- [`11_word_reference.docx`](https://github.com/khemsomsak/datharm-placement/blob/main/11_word_reference.docx) — Word styling template

Run `10_visualizations.R` first, then knit either wrapper Rmd.

## Requirements

R, with `dplyr`, `ggplot2`, `fixest`, `knitr`, `kableExtra`, `rmarkdown`, `sf`.
