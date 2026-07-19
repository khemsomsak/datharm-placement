#---------------------------------------------------------------------------#
# 11_dissertation_setup.R
# Shared helpers loaded by 11_dissertation_html.Rmd and 11_dissertation_word.Rmd
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
# fig.cap fed to knitr.
fig_caption <- function(id) {
  hit <- fig_titles$suggested_title[fig_titles$fig_id == id]
  if (length(hit) == 0 || is.na(hit[1])) return(paste0("[", id, " — title not found in 10_figure_titles.rds]"))
  hit[1]
}

#--- Output-format detection --------------------------------------------------
is_word_output <- function() {
  tryCatch(knitr::pandoc_to("docx"), error = function(e) FALSE)
}

#--- Locale-independent English date ------------------------------------------
# Sys.setlocale("LC_TIME", ...) is not reliable on machines whose Windows
# regional settings use a non-Gregorian calendar or a non-English display
# language (this is what produced Thai month names/years previously): the
# locale call can silently "succeed" without actually changing how %B/%Y
# render. This sidesteps locale entirely: %d/%m/%Y are pulled as plain
# digits (locale-independent), the month name is looked up from a hardcoded
# English vector, and a year above 2100 is assumed to be a Buddhist-era
# year and corrected back to Gregorian.
format_date_en <- function(d = Sys.Date()) {
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")
  y  <- as.integer(format(d, "%Y"))
  if (y > 2100) y <- y - 543
  m  <- as.integer(format(d, "%m"))
  dy <- as.integer(format(d, "%d"))
  sprintf("%d %s %d", dy, months[m], y)
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

#--- HTML-in-cell -> LaTeX-in-cell conversion (kept for completeness) --------
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
# HTML gets the original bootstrap/kableExtra styling. Word gets a plain
# knitr::kable(format="pipe") table -- this now looks properly styled (real
# borders, a shaded/bold header row, light row banding) because 11_word_
# reference.docx redefines Word's built-in "Table" style, which is what
# pandoc applies to every pipe table it writes to docx. No LaTeX branch:
# the PDF output was replaced by Word per the candidate's own workflow.
render_table <- function(df, col_names, align, header_label,
                          header_color_html = "#1D6FA4",
                          col1_width_html = "26em", other_width_html = "13em",
                          row_lines = NULL, italic_rows = NULL) {
  if (is.null(df)) return(invisible(NULL))

  if (is_word_output()) {
    df[] <- lapply(df, html_cell_to_plain)
    names(df) <- col_names
    knitr::kable(df, align = align, format = "pipe")
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
# grouped header row, but still needs the HTML->plain cell conversion.
render_simple_table <- function(df, header_color_html = "#10243B", widths_em = NULL) {
  if (is.null(df)) return(invisible(NULL))
  if (is_word_output()) {
    df[] <- lapply(df, html_cell_to_plain)
    knitr::kable(df, align = "l", format = "pipe")
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

#--- Manual page break before each new chapter -------------------------------
# Word paginates by content flow with no concept of "start a new chapter on
# its own page" unless told to explicitly. A raw OOXML page-break block only
# means anything in docx output -- it is silently dropped by pandoc when
# rendering to any other format, so this only ever emits something for Word.
# HTML instead gets a CSS page-break-before rule on h1 (11_dissertation_html.Rmd),
# which only takes visible effect when the page is printed or exported to
# PDF, since HTML has no concept of a "page" on screen.
# Called from a chunk with results='asis' immediately before each H1 heading
# EXCEPT the first ("I. Background..."), which already starts on its own
# page via the explicit break 11_dissertation_word.Rmd inserts right after
# the declaration section -- adding a second break there would produce an
# empty page between the declaration and Chapter I.
page_break <- function() {
  if (is_word_output()) {
    cat('\n\n```{=openxml}\n<w:p><w:r><w:br w:type="page"/></w:r></w:p>\n```\n\n')
  }
}

#--- Shrink figure height slightly for Word ----------------------------------
# Word paginates less generously than a browser does, and a figure sized for
# a wide HTML page can be just tall enough to no longer fit the remaining
# space on the current page; Word then pushes the whole figure (and its
# caption/blurb, kept together) onto the next page, leaving a visible gap
# at the bottom of the page before it. Shrinking figures ~15% for Word only
# makes that overflow far less frequent; removing "keep with next" from the
# CaptionedFigure/ImageCaption styles (done in 11_word_reference.docx) means
# that on the rarer occasion a figure still doesn't fit, the image and its
# caption can split across the page break instead of both jumping together
# and leaving a blank gap behind.
knitr::opts_hooks$set(fig.height = function(options) {
  if (is_word_output() && !is.null(options$fig.height)) {
    options$fig.height <- options$fig.height * 0.85
  }
  options
})
