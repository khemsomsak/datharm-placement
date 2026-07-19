#---------------------------------------------------------------------------#
# _dissertation_setup.R
# Shared helpers loaded by dissertation_html.Rmd and dissertation_word.Rmd
# via `source()` in each document's first chunk. Kept in one file so a fix
# only has to be made once for every output format.
#---------------------------------------------------------------------------#

library(knitr)
library(kableExtra)
library(dplyr)

#--- Load manifests, no computation -----------------------------------------
# Point this at your own local checkout. 10_artifacts.rds is regenerated
# by 10_visualizations.R each time you rerun the pipeline; this script does
# not compute anything itself, only reads what 09/10 already produced.
home    <- "C:/Users/HP/Documents/GitHub/datharm-placement"
viz_dir <- file.path(home, "03_output/10_visualizations")

manifest_path     <- file.path(viz_dir, "10_artifacts.rds")
fig_titles_path   <- file.path(viz_dir, "10_figure_titles.rds")

artifacts  <- if (file.exists(manifest_path))   readRDS(manifest_path)   else list()
fig_titles <- if (file.exists(fig_titles_path)) readRDS(fig_titles_path) else
  data.frame(fig_id = character(), suggested_title = character())

#--- Figure captions ---------------------------------------------------------
# Looks up the pre-agreed "Figure X.X.  Title" string from 10_figure_titles.rds
# so the same caption text is used as both the in-text bold label and the
# fig.cap fed to knitr (which is what populates a PDF's List of Figures).
fig_caption <- function(id) {
  hit <- fig_titles$suggested_title[fig_titles$fig_id == id]
  if (length(hit) == 0 || is.na(hit[1])) return(paste0("[", id, " — title not found in 10_figure_titles.rds]"))
  hit[1]
}

#--- Output-format detection --------------------------------------------------
is_word_output <- function() {
  tryCatch(knitr::pandoc_to("docx"), error = function(e) FALSE)
}

#--- Loud-failure helpers ----------------------------------------------------
# A missing figure/table shows a visible red-bordered message in the knitted
# document rather than silently falling back to an old hardcoded number or
# breaking the knit outright.
show_fig <- function(key) {
  path <- artifacts[[key]]
  if (is.null(path) || !file.exists(path)) {
    if (knitr::is_latex_output()) {
      cat(paste0("\\fbox{\\color{red}\\textbf{MISSING FIGURE --- '", key,
                 "' not found in 10\\_artifacts.rds. Rerun 09\\_data\\_investigations.R ",
                 "and 10\\_visualizations.R, then re-knit.}}"))
    } else if (is_word_output()) {
      cat(paste0("**[MISSING FIGURE --- '", key,
                 "' not found in 10_artifacts.rds. Rerun 09_data_investigations.R ",
                 "and 10_visualizations.R, then re-knit.]**"))
    } else {
      cat(paste0('<p class="missing-artifact">MISSING FIGURE — \'', key,
                 "' not found in 10_artifacts.rds. Rerun 09_data_investigations.R ",
                 "and 10_visualizations.R, then re-knit.</p>"))
    }
  } else {
    knitr::include_graphics(path)
  }
}

show_tab <- function(key) {
  df <- artifacts[[key]]
  if (is.null(df)) {
    if (knitr::is_latex_output()) {
      cat(paste0("\\fbox{\\color{red}\\textbf{MISSING TABLE --- '", key,
                 "' not found in 10\\_artifacts.rds. Rerun 09\\_data\\_investigations.R ",
                 "and 10\\_visualizations.R, then re-knit.}}"))
    } else if (is_word_output()) {
      cat(paste0("**[MISSING TABLE --- '", key,
                 "' not found in 10_artifacts.rds. Rerun 09_data_investigations.R ",
                 "and 10_visualizations.R, then re-knit.]**"))
    } else {
      cat(paste0('<p class="missing-artifact">MISSING TABLE — \'', key,
                 "' not found in 10_artifacts.rds. Rerun 09_data_investigations.R ",
                 "and 10_visualizations.R, then re-knit.</p>"))
    }
    return(invisible(NULL))
  }
  df
}

#--- HTML-in-cell -> LaTeX-in-cell conversion --------------------------------
# The manifest's regression tables (tab_3_1a, tab_3_1b, tab_weather, ...)
# store each cell as an HTML string, e.g. "0.042<br><small style='...'>
# [-0.001, 0.085]</small>", built that way so escape=FALSE + kableExtra
# renders it correctly in HTML. LaTeX doesn't understand <br>/<small>, so
# when knitting to PDF every such cell needs converting first.
html_cell_to_latex <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("<br\\s*/?>", " \\\\newline ", x)
  x <- gsub("<small[^>]*>", "{\\\\footnotesize ", x)
  x <- gsub("</small>", "}", x)
  x <- gsub("<em>", "\\\\textit{", x)
  x <- gsub("</em>", "}", x)
  x <- gsub("<strong>", "\\\\textbf{", x)
  x <- gsub("</strong>", "}", x)
  x <- gsub("<[^>]+>", "", x)          # strip anything else
  x <- gsub("%", "\\\\%", x)           # escape literal percent signs
  x
}

#--- HTML-in-cell -> plain-text conversion (for Word) -------------------------
# Word/docx tables via pandoc's pipe-table conversion can't hold a literal
# line break inside a cell, so <br> becomes a separator instead of a newline,
# and <small>/<em>/<strong> markup is just stripped rather than styled.
html_cell_to_plain <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("<br\\s*/?>", "  ", x)
  x <- gsub("<[^>]+>", "", x)
  x
}

#--- Format-aware regression/summary table renderer --------------------------
# One call handles all three output formats: HTML gets the original
# bootstrap styling kableExtra was already using; LaTeX gets cells run
# through html_cell_to_latex() then a booktabs-styled table; Word gets
# cells run through html_cell_to_plain() then a plain knitr::kable(),
# which pandoc turns into a native (if unstyled) Word table.
render_table <- function(df, col_names, align, header_label,
                          header_color_html = "#1D6FA4",
                          col1_width_html = "26em", other_width_html = "13em",
                          row_lines = NULL, italic_rows = NULL) {
  if (is.null(df)) return(invisible(NULL))

  if (is_word_output()) {
    df[] <- lapply(df, html_cell_to_plain)
    names(df) <- col_names
    knitr::kable(df, align = align, format = "pipe")
  } else if (knitr::is_latex_output()) {
    df[] <- lapply(df, html_cell_to_latex)
    k <- df %>%
      kbl(col.names = col_names, align = align, escape = FALSE,
          format = "latex", booktabs = TRUE, longtable = FALSE) %>%
      kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                    font_size = 9) %>%
      add_header_above(setNames(c(1, ncol(df) - 1), c(" ", header_label)))
    if (!is.null(italic_rows)) k <- k %>% row_spec(italic_rows, italic = TRUE)
    k
  } else {
    k <- df %>%
      kbl(col.names = col_names, align = align, escape = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"),
                    full_width = TRUE, font_size = 13) %>%
      add_header_above(setNames(c(1, ncol(df) - 1), c(" ", header_label)),
                        background = c("white", header_color_html),
                        color = c("white", "white"), bold = TRUE) %>%
      column_spec(1, width = col1_width_html)
    if (ncol(df) > 1) k <- k %>% column_spec(2:ncol(df), width = other_width_html)
    if (!is.null(row_lines)) k <- k %>% row_spec(row_lines, extra_css = "border-bottom:1px solid #ccc;")
    if (!is.null(italic_rows)) k <- k %>% row_spec(italic_rows, extra_css = "font-style:italic;border-top:1px solid #ccc;")
    k
  }
}

#--- Simple synthesis-table renderer -----------------------------------------
# For tab_4_1 / tab_4_2 / tab_4_3-style tables: plain column headers, no
# grouped header row, but still needs the HTML->plain/LaTeX cell conversion.
render_simple_table <- function(df, header_color_html = "#10243B", widths_em = NULL) {
  if (is.null(df)) return(invisible(NULL))
  if (is_word_output()) {
    df[] <- lapply(df, html_cell_to_plain)
    knitr::kable(df, align = "l", format = "pipe")
  } else if (knitr::is_latex_output()) {
    df[] <- lapply(df, html_cell_to_latex)
    df %>%
      kbl(align = "l", escape = FALSE, format = "latex", booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position", "scale_down"), font_size = 8) %>%
      column_spec(1, bold = TRUE)
  } else {
    k <- df %>%
      kbl(align = "l", escape = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"),
                    full_width = TRUE, font_size = 12) %>%
      row_spec(0, background = header_color_html, color = "white", bold = TRUE) %>%
      column_spec(1, bold = TRUE, width = "9em")
    if (!is.null(widths_em)) {
      for (i in seq_along(widths_em)) k <- k %>% column_spec(i + 1, width = widths_em[i])
    }
    k
  }
}

# Force English locale so all rendered dates use the Western Gregorian
# calendar with English month names rather than the machine's default.
suppressWarnings({
  for (loc in c("English", "en_US.UTF-8", "en_GB.UTF-8", "en_US", "C")) {
    if (!inherits(try(Sys.setlocale("LC_TIME", loc), silent = TRUE), "try-error")) {
      if (Sys.getlocale("LC_TIME") != "") break
    }
  }
})
