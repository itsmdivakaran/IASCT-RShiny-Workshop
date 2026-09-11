library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gt)
library(DT)
library(plotly)
library(haven)
library(tools)
library(shinyWidgets)
library(pharmaverseadam)
library(flextable)
library(officer)
library(r2rtf)

# ─────────────────────────────────────────────────────────────────────────────
# Constants
# ─────────────────────────────────────────────────────────────────────────────

BRAND_BLUE  <- "#2C3E7A"
TRT_PALETTE <- c("#546E7A","#1565C0","#B71C1C","#2E7D32","#6A1B9A",
                 "#E65100","#00695C","#4527A0","#37474F","#880E4F")

TABLE_CHOICES <- c(
  "Demographics (T14.1.1)"              = "demog",
  "Disposition (T14.1.2)"               = "dispos",
  "AE Overview (T14.3.1)"              = "ae_overview",
  "AE by SOC / PT (T14.3.2)"           = "ae_socpt",
  "AE by Max Severity (T14.3.3)"       = "ae_sev"
)

FIGURE_CHOICES <- c(
  "Boxplot by Treatment (F14.2.1)"      = "boxplot",
  "Histogram (F14.2.2)"                 = "histogram",
  "AE Incidence Bar Chart (F14.3.1)"   = "ae_bar"
)

POP_LABELS <- c(SAFFL = "Safety Analysis Set", ITTFL = "Intent-to-Treat Population",
                EFFFL = "Efficacy Population", RANDFL = "Randomized Population")

pop_label <- function(pop_flag) {
  if (identical(pop_flag, "__ALL__")) return("All Subjects")
  if (pop_flag %in% names(POP_LABELS)) POP_LABELS[[pop_flag]]
  else paste0(pop_flag, " = Y Population")
}

# A4 page geometry (inches) for exported tables/figures
A4_PORTRAIT  <- c(w = 8.27,  h = 11.69)
A4_LANDSCAPE <- c(w = 11.69, h = 8.27)

# Wide tables (many arm columns / a Level column) print better in landscape
table_orientation <- function(df) {
  n_cols <- ncol(select(df, -any_of("Level")))
  if (n_cols > 5) "landscape" else "portrait"
}

a4_dims <- function(orientation) {
  if (identical(orientation, "landscape")) A4_LANDSCAPE else A4_PORTRAIT
}

TABLE_TITLES <- c(
  demog       = "Table 14.1.1 — Summary of Demographic and Baseline Characteristics",
  dispos      = "Table 14.1.2 — Subject Disposition",
  ae_overview = "Table 14.3.1 — Overview of Treatment-Emergent Adverse Events",
  ae_socpt    = "Table 14.3.2 — TEAEs by System Organ Class and Preferred Term",
  ae_sev      = "Table 14.3.3 — TEAEs by Maximum Severity"
)

FIGURE_TITLES <- c(
  boxplot   = "Figure 14.2.1 — Distribution of Continuous Variable by Treatment Group",
  histogram = "Figure 14.2.2 — Histogram of Continuous Variable",
  ae_bar    = "Figure 14.3.1 — Top TEAEs by Incidence (%)"
)

# Built-in pharmaverseadam datasets
BUILTIN <- list(
  ADSL = pharmaverseadam::adsl,
  ADAE = pharmaverseadam::adae,
  ADLB = pharmaverseadam::adlb,
  ADVS = pharmaverseadam::advs
)

# ─────────────────────────────────────────────────────────────────────────────
# Formatters
# ─────────────────────────────────────────────────────────────────────────────

fmt_n_pct   <- function(n, N)  sprintf("%d (%.1f%%)", n, 100 * n / N)
fmt_mean_sd <- function(x)     sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_med_rng <- function(x)     sprintf("%.1f [%.1f, %.1f]", median(x, na.rm = TRUE),
                                       min(x, na.rm = TRUE), max(x, na.rm = TRUE))

# ─────────────────────────────────────────────────────────────────────────────
# Shared: apply population + treatment filters, build N vector
# ─────────────────────────────────────────────────────────────────────────────

prep_adsl <- function(adsl, trt_var, pop_flag, trts, show_total) {
  d <- if (pop_flag == "__ALL__") adsl else filter(adsl, .data[[pop_flag]] == "Y")
  d <- filter(d, .data[[trt_var]] %in% trts) |>
    mutate(TRT = factor(.data[[trt_var]], levels = trts))

  N <- setNames(as.integer(table(d$TRT)), trts)
  all_lvls <- if (show_total) c(trts, "Total") else trts

  if (show_total) {
    d <- bind_rows(d, mutate(d, TRT = factor("Total", levels = all_lvls)))
    d$TRT <- factor(as.character(d$TRT), levels = all_lvls)
    N <- c(N, Total = sum(N))
  }
  list(d = d, N = N, all_lvls = all_lvls)
}

wide_counts <- function(d, grp_cols, N, all_lvls) {
  d |>
    count(TRT, across(all_of(grp_cols))) |>
    right_join(
      expand.grid(c(list(TRT = factor(all_lvls, levels = all_lvls)),
                    setNames(lapply(grp_cols, function(g) unique(d[[g]])), grp_cols)),
                  stringsAsFactors = FALSE),
      by = c("TRT", grp_cols)
    ) |>
    mutate(n  = replace_na(n, 0L),
           Nv = N[as.character(TRT)],
           val = fmt_n_pct(n, Nv)) |>
    select(TRT, all_of(grp_cols), val) |>
    pivot_wider(names_from = TRT, values_from = val)
}

hdr_row <- function(first_col, first_val, N, all_lvls) {
  tibble(!!first_col := first_val,
         !!!setNames(paste0("(N=", N[all_lvls], ")"), all_lvls))
}

# Long-format n/pct incidence by an arbitrary grouping column — used by the
# Explore Data tab so the same summary can drive both a chart and a table.
group_incidence <- function(d_ae, group_col, N, lvls) {
  d_ae |>
    distinct(USUBJID, TRT, !!sym(group_col)) |>
    count(TRT, !!sym(group_col), name = "n") |>
    tidyr::complete(TRT = factor(lvls, levels = lvls), !!sym(group_col),
                     fill = list(n = 0L)) |>
    filter(!is.na(!!sym(group_col))) |>
    mutate(Nv = N[as.character(TRT)],
           pct = 100 * n / Nv,
           val = fmt_n_pct(n, Nv))
}

group_incidence_wide <- function(long_df, group_col) {
  long_df |>
    select(TRT, !!sym(group_col), val) |>
    pivot_wider(names_from = TRT, values_from = val)
}

# ─────────────────────────────────────────────────────────────────────────────
# Table builders
# ─────────────────────────────────────────────────────────────────────────────

build_demog <- function(adsl, trt_var, pop_flag, trts, show_total) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, show_total)
  d <- p$d; N <- p$N; lvls <- p$all_lvls

  d <- mutate(d,
              SEX  = factor(SEX, levels = c("F","M"), labels = c("Female","Male")),
              RACE = toTitleCase(tolower(as.character(RACE)))
  )

  age_tbl <- d |>
    group_by(TRT) |>
    summarise(`Mean (SD)` = fmt_mean_sd(AGE),
              `Median [Min, Max]` = fmt_med_rng(AGE), .groups = "drop") |>
    pivot_longer(-TRT, names_to = "Statistic") |>
    pivot_wider(names_from = TRT, values_from = value) |>
    mutate(Variable = c("Age (years)", rep("", n()-1)), .before = 1)

  cat_blk <- function(col, label) {
    wide_counts(d, col, N, lvls) |>
      rename(Statistic = all_of(col)) |>
      mutate(Variable = c(label, rep("", n()-1)), .before = 1)
  }

  bind_rows(
    hdr_row("Variable", "Characteristic", N, lvls) |> mutate(Statistic = "", .after = 1),
    age_tbl,
    cat_blk("SEX",  "Sex, n (%)"),
    cat_blk("RACE", "Race, n (%)")
  )
}

build_dispos <- function(adsl, trt_var, pop_flag, trts, show_total) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, show_total)
  d <- p$d; N <- p$N; lvls <- p$all_lvls

  if (!"EOSSTT" %in% names(d)) {
    return(tibble(Category = "Disposition data unavailable",
                  !!!setNames(rep("—", length(lvls)), lvls)))
  }

  status <- wide_counts(mutate(d, EOSSTT = as.character(EOSSTT)), "EOSSTT", N, lvls) |>
    rename(Category = EOSSTT) |>
    mutate(Category = c("Completed", "Discontinued")[match(Category, c("COMPLETED","DISCONTINUED"))]) |>
    replace_na(list(Category = "Other"))

  bind_rows(
    hdr_row("Category", "Disposition", N, lvls),
    status
  )
}

build_ae_overview <- function(adsl, adae, trt_var, pop_flag, trts, show_total) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, show_total)
  d_sl <- p$d; N <- p$N; lvls <- p$all_lvls

  base_ae <- adae |>
    filter(USUBJID %in% d_sl$USUBJID[d_sl$TRT != "Total"],
           TRTEMFL == "Y") |>
    left_join(select(d_sl, USUBJID, TRT) |> filter(TRT != "Total"), by = "USUBJID")

  if ("Total" %in% lvls)
    base_ae <- bind_rows(base_ae, mutate(base_ae, TRT = factor("Total", levels = lvls)))

  base_ae$TRT <- factor(as.character(base_ae$TRT), levels = lvls)

  cnt <- function(ae_data, cond_expr) {
    ae_data |>
      filter(!!cond_expr) |>
      distinct(USUBJID, TRT) |>
      count(TRT) |>
      right_join(tibble(TRT = factor(lvls, levels = lvls)), by = "TRT") |>
      mutate(n = replace_na(n, 0L), Nv = N[as.character(TRT)],
             val = fmt_n_pct(n, Nv)) |>
      arrange(TRT) |>
      pull(val) |>
      setNames(lvls)
  }

  rows <- list(
    list("Any TEAE",                     quote(TRUE)),
    list("Any Serious TEAE",             quote(AESER == "Y")),
    list("TEAE Leading to Death",        quote(!is.na(AESDTH) & AESDTH == "Y")),
    list("TEAE Leading to Withdrawal",   quote(!is.na(AEACN) & AEACN == "DRUG WITHDRAWN")),
    list("Any Severe TEAE",              quote(!is.na(AESEV) & AESEV == "SEVERE"))
  )

  result <- map_dfr(rows, function(r) {
    vals <- tryCatch(cnt(base_ae, r[[2]]), error = function(e) setNames(rep("N/A", length(lvls)), lvls))
    bind_cols(tibble(Category = r[[1]]), as_tibble(t(vals)))
  })

  bind_rows(hdr_row("Category", "TEAE Category", N, lvls), result)
}

build_ae_socpt <- function(adsl, adae, trt_var, pop_flag, trts, show_total, min_pct = 0, sevs = NULL) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, show_total)
  d_sl <- p$d; N <- p$N; lvls <- p$all_lvls

  d_ae <- adae |>
    filter(USUBJID %in% d_sl$USUBJID[d_sl$TRT != "Total"],
           TRTEMFL == "Y") |>
    left_join(select(d_sl, USUBJID, TRT) |> filter(TRT != "Total"), by = "USUBJID")

  if (!is.null(sevs) && length(sevs) > 0)
    d_ae <- filter(d_ae, AESEV %in% toupper(sevs))

  if ("Total" %in% lvls)
    d_ae <- bind_rows(d_ae, mutate(d_ae, TRT = factor("Total", levels = lvls)))

  d_ae$TRT <- factor(as.character(d_ae$TRT), levels = lvls)

  mk_counts <- function(id_cols, ...) {
    d_ae |>
      distinct(USUBJID, TRT, !!!syms(id_cols)) |>
      count(TRT, !!!syms(id_cols)) |>
      mutate(Nv = N[as.character(TRT)], pct = 100 * n / Nv,
             val = fmt_n_pct(n, Nv)) |>
      pivot_wider(id_cols = all_of(id_cols), names_from = TRT,
                  values_from = c(val, pct), values_fill = list(val="0 (0.0%)", pct=0))
  }

  soc_df <- mk_counts("AEBODSYS")
  pt_df  <- mk_counts(c("AEBODSYS","AEDECOD"))

  # Filter by min incidence
  pct_cols <- paste0("pct_", lvls)
  pct_cols_soc <- intersect(pct_cols, names(soc_df))
  if (min_pct > 0 && length(pct_cols_soc) > 0) {
    max_pct <- apply(soc_df[pct_cols_soc], 1, max, na.rm = TRUE)
    soc_df  <- soc_df[max_pct >= min_pct, ]
  }

  val_cols <- paste0("val_", lvls)

  soc_sorted <- soc_df |>
    mutate(sort_pct = rowMeans(across(all_of(intersect(pct_cols, names(soc_df)))))) |>
    arrange(desc(sort_pct))

  rows <- map_dfr(soc_sorted$AEBODSYS, function(soc) {
    soc_row <- filter(soc_df, AEBODSYS == soc) |>
      select(Term = AEBODSYS, all_of(intersect(val_cols, names(soc_df)))) |>
      mutate(Level = "SOC", .before = 1)

    pt_rows <- filter(pt_df, AEBODSYS == soc) |>
      arrange(desc(.data[[intersect(pct_cols, names(pt_df))[1]]])) |>
      select(Term = AEDECOD, all_of(intersect(val_cols, names(pt_df)))) |>
      mutate(Term = paste0("   ", Term), Level = "PT", .before = 1)

    bind_rows(soc_row, pt_rows)
  })

  names(rows) <- c("Level", "Term", lvls)

  bind_rows(
    hdr_row("Term", "System Organ Class / Preferred Term", N, lvls) |> mutate(Level = "HDR", .before = 1),
    rows
  )
}

build_ae_sev <- function(adsl, adae, trt_var, pop_flag, trts, show_total) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, show_total)
  d_sl <- p$d; N <- p$N; lvls <- p$all_lvls

  d_ae <- adae |>
    filter(USUBJID %in% d_sl$USUBJID[d_sl$TRT != "Total"],
           TRTEMFL == "Y") |>
    left_join(select(d_sl, USUBJID, TRT) |> filter(TRT != "Total"), by = "USUBJID")

  if ("Total" %in% lvls)
    d_ae <- bind_rows(d_ae, mutate(d_ae, TRT = factor("Total", levels = lvls)))

  d_ae$TRT <- factor(as.character(d_ae$TRT), levels = lvls)

  sev_lvls <- c("MILD","MODERATE","SEVERE")

  result <- wide_counts(
    mutate(d_ae, AESEV = factor(as.character(AESEV), levels = sev_lvls)) |>
      distinct(USUBJID, TRT, AESEV),
    "AESEV", N, lvls
  ) |>
    rename(Severity = AESEV) |>
    mutate(Severity = c("Mild","Moderate","Severe")[match(Severity, sev_lvls)])

  bind_rows(hdr_row("Severity", "Max Severity", N, lvls), result)
}

# ─────────────────────────────────────────────────────────────────────────────
# GT renderer
# ─────────────────────────────────────────────────────────────────────────────

to_gt <- function(df, tbl_type, title, lvls) {
  first_col  <- names(df)[!names(df) %in% c("Level", lvls)][1]
  trt_cols   <- intersect(lvls, names(df))
  has_total  <- "Total" %in% trt_cols
  active_trt <- setdiff(trt_cols, "Total")

  gt_tbl <- df |>
    select(-any_of("Level")) |>
    gt(rowname_col = first_col) |>
    tab_header(title = md(paste0("**", title, "**"))) |>
    tab_spanner(label = md("**Treatment Group**"), columns = all_of(active_trt)) |>
    tab_style(
      style = list(cell_fill(color = "#E8EEF7"), cell_text(weight = "bold")),
      locations = cells_body(rows = .data[[first_col]] %in%
                               c("Characteristic","Category","Disposition",
                                 "TEAE Category","System Organ Class / Preferred Term","Max Severity"))
    ) |>
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_title(groups = "title")
    ) |>
    tab_options(
      table.font.size = px(12), table.width = pct(100),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#FAFAFA",
      heading.background.color = BRAND_BLUE,
      column_labels.background.color = "#EEF2FF",
      column_labels.font.weight = "bold"
    )

  # Bold SOC rows in AE SOC/PT table
  if (tbl_type == "ae_socpt" && "Level" %in% names(df)) {
    soc_rows <- which(df$Level == "SOC")
    if (length(soc_rows) > 0)
      gt_tbl <- gt_tbl |>
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_body(rows = soc_rows, columns = first_col))
  }

  # Total column style
  if (has_total)
    gt_tbl <- gt_tbl |>
    tab_style(style = list(cell_fill(color = "#F0F4FF"), cell_text(weight = "bold")),
              locations = cells_body(columns = "Total")) |>
    tab_style(style = list(cell_fill(color = "#F0F4FF"), cell_text(weight = "bold")),
              locations = cells_column_labels(columns = "Total"))

  gt_tbl |>
    tab_source_note(md(paste0(
      "*Safety Analysis Set. Screen Failures excluded. ",
      "Percentages based on treatment group N.*"
    )))
}

# ─────────────────────────────────────────────────────────────────────────────
# Clinical (TFL-style) table export — shared by the RTF / Word / PDF exporters
#
# All table builders above emit a first data row (via hdr_row()) that carries
# the "(N=xx)" counts per arm. For a genuine TFL layout those counts belong in
# the column header (spanning under each arm), not in the table body — so this
# splits that row out and turns it into a proper two-line header.
# ─────────────────────────────────────────────────────────────────────────────

clinical_table_parts <- function(df, lvls) {
  df   <- select(df, -any_of("Level"))
  label_cols <- setdiff(names(df), lvls)
  hdr  <- df[1, , drop = FALSE]
  body <- df[-1, , drop = FALSE]
  n_vals <- vapply(lvls, function(l) gsub("[()]", "", as.character(hdr[[l]][1])),
                    character(1))
  list(label_cols = label_cols, body = body, n_vals = n_vals)
}

# r2rtf: produces a genuine clinical-style .rtf table (title/subtitle block,
# N-under-arm column header, single/double rule lines, footnote)
build_clinical_rtf <- function(df, lvls, table_no, title, subtitle, footnote,
                               orientation = "portrait") {
  parts <- clinical_table_parts(df, lvls)
  label_cols <- parts$label_cols; n_vals <- parts$n_vals
  body <- parts$body
  body[is.na(body)] <- ""
  body <- select(body, all_of(label_cols), all_of(lvls))

  n_label <- length(label_cols); n_arm <- length(lvls)
  col_rel_width <- c(rep(2.4, n_label), rep(1.6, n_arm))

  header1 <- paste(c(label_cols, lvls), collapse = " | ")
  header2 <- paste(c(rep("", n_label), paste0("(", n_vals[lvls], ")")), collapse = " | ")

  dims <- a4_dims(orientation)

  body |>
    rtf_title(table_no, c(title, subtitle)) |>
    rtf_colheader(header1, col_rel_width = col_rel_width,
                  border_bottom = "", text_format = "b") |>
    rtf_colheader(header2, col_rel_width = col_rel_width,
                  border_top = "", border_bottom = "single", text_format = "b") |>
    rtf_body(col_rel_width = col_rel_width,
             text_justification = c(rep("l", n_label), rep("c", n_arm))) |>
    rtf_footnote(footnote) |>
    rtf_page(orientation = orientation, width = unname(dims["w"]), height = unname(dims["h"]))
}

# flextable: same clinical layout (title/subtitle, N-under-arm header, black
# rules, serif font, no dashboard colors) — shared by the Word and PDF exports
build_clinical_flextable <- function(df, lvls, table_no, title, subtitle, footnote) {
  parts <- clinical_table_parts(df, lvls)
  label_cols <- parts$label_cols; n_vals <- parts$n_vals
  body <- parts$body
  body[is.na(body)] <- ""
  body <- select(body, all_of(label_cols), all_of(lvls)) |>
    mutate(across(everything(), as.character))

  hdr_labels <- setNames(c(label_cols, paste0(lvls, "\n(", n_vals[lvls], ")")),
                          c(label_cols, lvls))
  thin  <- officer::fp_border(width = 0.75, color = "black")
  thick <- officer::fp_border(width = 1.5,  color = "black")

  flextable(body) |>
    set_header_labels(values = as.list(hdr_labels)) |>
    add_header_lines(values = c(subtitle, title, table_no)) |>
    theme_booktabs() |>
    border_remove() |>
    hline(i = 3, part = "header", border = thick) |>
    hline_bottom(part = "header", border = thin) |>
    hline_bottom(part = "body",   border = thick) |>
    align(align = "center", i = 1:3, part = "header") |>
    align(j = label_cols, align = "left",   part = "body") |>
    align(j = lvls,       align = "center", part = "body") |>
    bold(i = 1, part = "header") |>
    bold(i = 4, part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 9, part = "all") |>
    fontsize(size = 11, i = 1, part = "header") |>
    padding(padding = 3, part = "all") |>
    add_footer_lines(values = footnote) |>
    fontsize(size = 8, part = "footer") |>
    italic(part = "footer") |>
    autofit()
}

# ─────────────────────────────────────────────────────────────────────────────
# Figure builders
# ─────────────────────────────────────────────────────────────────────────────

tfl_theme <- theme_classic(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8, hjust = 0),
    legend.position = "bottom",
    legend.title  = element_text(face = "bold"),
    strip.background = element_rect(fill = "#E3EBF6"),
    strip.text    = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey92")
  )

build_boxplot <- function(adsl, trt_var, pop_flag, trts, yvar, facet_var,
                          show_pts, show_violin) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, FALSE)
  d <- p$d; N <- p$N

  ylbl <- switch(yvar,
                 AGE     = "Age (years)",
                 TRTDURD = "Treatment Duration (days)",
                 yvar
  )

  colors <- setNames(TRT_PALETTE[seq_along(trts)], trts)

  pl <- ggplot(d, aes(x = TRT, y = .data[[yvar]], fill = TRT))

  if (show_violin)
    pl <- pl + geom_violin(alpha = 0.2, width = 0.8, color = NA)

  pl <- pl + geom_boxplot(alpha = 0.7, linewidth = 0.6, width = 0.5,
                          outlier.shape = if (show_pts) NA else 19)

  if (show_pts)
    pl <- pl + geom_jitter(aes(color = TRT), width = 0.12, size = 1.3,
                           alpha = 0.5, show.legend = FALSE)

  if (!is.null(facet_var) && facet_var != "None")
    pl <- pl + facet_wrap(vars(.data[[facet_var]]))

  n_lbl <- paste0(trts, "\n(n=", N[trts], ")")

  pl +
    scale_fill_manual(values = colors, labels = n_lbl, name = "Treatment") +
    scale_color_manual(values = colors) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, 12)) +
    labs(title    = paste("Distribution of", ylbl, "by Treatment Group"),
         subtitle = paste0("Safety Analysis Set  (N=", sum(N), ")"),
         caption  = "Box: IQR with median; whiskers: 1.5 × IQR",
         x = NULL, y = ylbl) +
    tfl_theme
}

build_histogram <- function(adsl, trt_var, pop_flag, trts, xvar, bins,
                            show_density, facet_trt) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, FALSE)
  d <- p$d; N <- p$N

  xlbl   <- switch(xvar, AGE = "Age (years)", TRTDURD = "Treatment Duration (days)", xvar)
  colors <- setNames(TRT_PALETTE[seq_along(trts)], trts)

  pl <- ggplot(d, aes(x = .data[[xvar]], fill = TRT))

  if (facet_trt) {
    pl <- pl +
      geom_histogram(bins = bins, alpha = 0.85, color = "white", linewidth = 0.25) +
      facet_wrap(vars(TRT), ncol = 1, labeller = label_wrap_gen(20))
    if (show_density)
      pl <- pl +
        geom_density(aes(y = after_stat(count * diff(range(d[[xvar]])) / bins), color = TRT),
                     linewidth = 0.9, fill = NA, show.legend = FALSE)
  } else {
    pl <- pl +
      geom_histogram(bins = bins, alpha = 0.55, color = "white",
                     linewidth = 0.25, position = "identity")
    if (show_density)
      pl <- pl +
        geom_density(aes(y = after_stat(count * diff(range(d[[xvar]])) / bins), color = TRT),
                     linewidth = 0.9, fill = NA, show.legend = FALSE)
  }

  pl +
    scale_fill_manual(values = colors, name = "Treatment") +
    scale_color_manual(values = colors) +
    labs(title    = paste("Histogram of", xlbl),
         subtitle = paste0("Safety Analysis Set  (N=", sum(N), ")   Bins=", bins),
         caption  = if (show_density) "Density curve scaled to frequency axis" else NULL,
         x = xlbl, y = "Number of Subjects") +
    tfl_theme
}

build_ae_bar <- function(adsl, adae, trt_var, pop_flag, trts, top_n = 10) {
  p <- prep_adsl(adsl, trt_var, pop_flag, trts, FALSE)
  d_sl <- p$d; N <- p$N

  colors <- setNames(TRT_PALETTE[seq_along(trts)], trts)

  ae_plot_data <- adae |>
    filter(USUBJID %in% d_sl$USUBJID, TRTEMFL == "Y") |>
    left_join(select(d_sl, USUBJID, TRT), by = "USUBJID") |>
    distinct(USUBJID, TRT, AEDECOD) |>
    count(TRT, AEDECOD) |>
    mutate(pct = 100 * n / N[as.character(TRT)]) |>
    group_by(AEDECOD) |>
    mutate(max_pct = max(pct)) |>
    ungroup()

  top_terms <- ae_plot_data |>
    distinct(AEDECOD, max_pct) |>
    slice_max(order_by = max_pct, n = top_n, with_ties = FALSE) |>
    pull(AEDECOD)

  ae_plot_data |>
    filter(AEDECOD %in% top_terms) |>
    mutate(AEDECOD = forcats::fct_reorder(AEDECOD, max_pct)) |>
    ggplot(aes(x = pct, y = AEDECOD, fill = TRT)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    scale_fill_manual(values = colors, name = "Treatment") +
    scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
    labs(title    = paste0("Top ", top_n, " TEAEs by Incidence (%)"),
         subtitle = paste0("Safety Analysis Set  (N=", sum(N), ")"),
         caption  = "Subjects with ≥1 event; each subject counted once per preferred term",
         x = "Incidence (%)", y = NULL) +
    tfl_theme +
    theme(legend.position = "right")
}

# Stamp a ggplot figure with a clinical TFL title block: "Figure X.X.X — <desc>"
# as the title (prefixed onto whatever the builder already set), and a
# population/generated-on line appended to the existing statistical caption —
# used for the PDF export so the file carries the same title the app shows.
apply_clinical_figure_titles <- function(plt, fig_no, footnote) {
  old_title   <- plt$labels$title %||% ""
  old_caption <- plt$labels$caption
  plt + labs(
    title   = paste0(fig_no, if (nzchar(old_title)) paste0(" — ", old_title) else ""),
    caption = paste(c(old_caption, footnote), collapse = "\n")
  )
}

build_link_bar <- function(long_df, group_col, colors, xlab = "Incidence (%)", order = NULL) {
  if (!is.null(order)) {
    ord <- rev(unique(as.character(order)))
  } else {
    ord <- long_df |>
      filter(TRT != "Total") |>
      group_by(.data[[group_col]]) |>
      summarise(m = max(pct), .groups = "drop") |>
      arrange(m) |>
      pull(!!sym(group_col))
    if (!length(ord)) ord <- unique(as.character(long_df[[group_col]]))
  }

  long_df <- long_df |>
    mutate(cat = factor(as.character(.data[[group_col]]), levels = unique(as.character(ord))))

  ggplot(long_df, aes(x = pct, y = cat, fill = TRT,
                      text = paste0(cat, "<br>", TRT, ": ", val))) +
    geom_col(position = position_dodge(0.7), width = 0.65, alpha = 0.88) +
    scale_fill_manual(values = colors, name = "Treatment") +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(x = xlab, y = NULL) +
    tfl_theme +
    theme(legend.position = "right")
}

# ─────────────────────────────────────────────────────────────────────────────
# UI helpers
# ─────────────────────────────────────────────────────────────────────────────

status_chip <- function(label, ok, detail = NULL) {
  col   <- if (ok) "#198754" else "#6c757d"
  icon  <- if (ok) "✓" else "○"
  tags$div(
    style = paste0("display:inline-flex;align-items:center;gap:4px;",
                   "background:", if(ok)"#d1e7dd" else "#f8f9fa", ";",
                   "color:", col, ";border-radius:4px;padding:3px 8px;",
                   "font-size:0.8rem;margin-bottom:3px;"),
    tags$span(icon), tags$span(label),
    if (!is.null(detail)) tags$span(detail, style="color:#666;font-size:0.75rem;")
  )
}

ctrl_card <- function(...) {
  card(
    height = "auto",
    style = "background:#f8f9fa;border:1px solid #dee2e6;",
    card_body(padding = "12px", ...)
  )
}

section_label <- function(text, ic = NULL) {
  tags$div(
    class = "section-label",
    if (!is.null(ic)) icon(ic), text
  )
}

breadcrumb_ui <- function(items) {
  # items: named list label -> inputId (last one is not a link)
  n <- length(items)
  tags$div(
    class = "explore-breadcrumb",
    purrr::imap(items, function(id, label) {
      is_last <- which(names(items) == label) == n
      tagList(
        if (is_last) tags$span(class = "crumb-current", label)
        else tags$a(class = "crumb-link", href = "#", id = id, label,
                    onclick = "Shiny.setInputValue(this.id, Math.random()); return false;"),
        if (!is_last) tags$span(class = "crumb-sep", icon("angle-right"))
      )
    })
  )
}

APP_CSS <- "
:root{
  --brand:#2C3E7A; --brand-dark:#1e2c58; --accent:#3D6BE0;
  --surface:#ffffff; --surface-muted:#f5f7fb; --border-soft:#e3e7f1;
}
body{ background: var(--surface-muted); }
.navbar, .bslib-page-title{ background: linear-gradient(90deg, var(--brand), var(--brand-dark)) !important; }
.card{
  border:1px solid var(--border-soft); border-radius:14px;
  box-shadow:0 2px 10px rgba(30,44,88,.06), 0 1px 2px rgba(30,44,88,.04);
  transition: box-shadow .15s ease;
}
.card:hover{ box-shadow:0 4px 16px rgba(30,44,88,.10), 0 1px 3px rgba(30,44,88,.06); }
.card-header{
  border-radius:14px 14px 0 0 !important; border:none !important;
  font-size:.92rem; letter-spacing:.02em; padding:.6rem 1rem;
}
.card-header.bg-primary{ background: linear-gradient(90deg, var(--brand), #3a4f9c) !important; }
.btn{ border-radius:8px; }
.btn-sm{ font-weight:500; }
.form-control, .form-select{ border-radius:8px; }
.sidebar, .bslib-sidebar-layout > .sidebar{ background: var(--surface-muted) !important; }
.section-label{
  font-size:.7rem; text-transform:uppercase; letter-spacing:.08em;
  font-weight:700; color:#5a6072; margin:4px 0 8px;
}
.nav-tabs{ border-bottom:2px solid var(--border-soft); gap:2px; }
.nav-tabs .nav-link{
  border:none; border-radius:10px 10px 0 0; color:#5a6072; font-weight:600;
  padding:.55rem 1.1rem; transition:.12s;
}
.nav-tabs .nav-link:hover{ background:#eef1fa; color:var(--brand); }
.nav-tabs .nav-link.active{
  background:var(--surface); color:var(--brand);
  border-bottom:3px solid var(--accent); box-shadow:0 -2px 8px rgba(30,44,88,.06);
}
.value-box-grid{ margin-bottom:14px; }
.explore-breadcrumb{
  display:flex; align-items:center; flex-wrap:wrap; gap:6px;
  background:#eef1fa; border:1px solid var(--border-soft); border-radius:10px;
  padding:8px 12px; margin-bottom:10px; font-size:.87rem;
}
.explore-breadcrumb .crumb-link{ color:var(--brand); font-weight:600; text-decoration:none; }
.explore-breadcrumb .crumb-link:hover{ text-decoration:underline; }
.explore-breadcrumb .crumb-current{ color:#333; font-weight:700; }
.explore-breadcrumb .crumb-sep{ color:#9aa1b5; font-size:.75rem; }
.drill-hint{ color:#7a8095; font-size:.78rem; margin-top:6px; }
::-webkit-scrollbar{ width:9px; height:9px; }
::-webkit-scrollbar-thumb{ background:#c3cae0; border-radius:6px; }
::-webkit-scrollbar-track{ background:transparent; }
"

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = tags$span(
    icon("flask-vial"), " ",
    tags$b("TFL", style = paste0("color:", " #FFFFFF", ";")), " Builder"
  ),
  theme = bs_theme(
    bootswatch = "flatly", primary = BRAND_BLUE, font_scale = 0.95,
    "navbar-bg" = BRAND_BLUE, base_font = font_google("Inter"),
    "border-radius" = "0.65rem"
  ),
  tags$head(tags$style(HTML(APP_CSS))),

  # ── Global left sidebar ──────────────────────────────────────────────────
  sidebar = sidebar(
    width = 290, bg = "#f0f2f8", open = "open",

    # Data source
    section_label("Data Source", "database"),
    radioButtons(
      "data_source", NULL,
      choices  = c("pharmaverseadam (built-in)" = "builtin",
                   "Upload my own data files"   = "upload"),
      selected = "builtin"
    ),

    conditionalPanel(
      "input.data_source == 'upload'",
      fileInput(
        "upload_files",
        label    = tags$span("Upload ADaM files", tags$small(" (.sas7bdat / .csv / .rds)",
                                                             style="color:#888;")),
        multiple = TRUE,
        accept   = c(".sas7bdat", ".csv", ".rds"),
        placeholder = "No files selected"
      ),
      tags$small(
        style = "color:#888;",
        "Files are matched to datasets by filename (e.g. adsl.sas7bdat → ADSL)."
      )
    ),

    uiOutput("data_status_ui"),
    hr(style = "margin:10px 0;"),

    # View data picker (for View Data tab)
    section_label("Dataset Preview", "eye"),
    uiOutput("view_dataset_ui"),
    tags$small(style = "color:#888;",
               "Analysis settings (population, treatment) are configured per TFL in the Tables / Figures tabs."
    )
  ),

  # ── Main area: 3 tabs ────────────────────────────────────────────────────
  navset_tab(
    id = "main_tabs",

    # ── Tab 1: View Data ───────────────────────────────────────────────────
    nav_panel(
      "View Data",
      icon = icon("table"),
      div(class = "value-box-grid", uiOutput("view_valueboxes_ui")),
      layout_columns(
        col_widths = c(9, 3),
        card(
          card_header(icon("table"), " Dataset Preview", class = "bg-primary text-white fw-bold"),
          card_body(uiOutput("data_viewer_ui"), padding = "8px"),
          full_screen = TRUE
        ),
        card(
          card_header(icon("chart-pie"), " Dataset Summary", class = "bg-primary text-white fw-bold"),
          card_body(
            uiOutput("data_summary_ui"),
            padding = "8px"
          )
        )
      )
    ),

    # ── Tab 2: Tables ──────────────────────────────────────────────────────
    nav_panel(
      "Tables",
      icon = icon("file-medical"),
      layout_columns(
        col_widths = c(3, 9),

        # Left: table controls
        div(
          style = "overflow-y:auto; max-height:calc(100vh - 120px);",
          ctrl_card(
            tags$div(class = "fw-bold mb-2", icon("sliders"), " Table Controls"),

            # Table selector
            selectInput("table_type", "Select Table",
                        choices = TABLE_CHOICES, selected = "demog"),
            hr(style = "margin:8px 0;"),

            # Per-table analysis settings
            tags$div(
              style = "font-size:0.7rem;text-transform:uppercase;letter-spacing:.08em;
                       font-weight:700;color:#666;margin-bottom:4px;",
              icon("filter"), " Analysis Settings"
            ),
            uiOutput("tbl_pop_flag_ui"),
            uiOutput("tbl_trt_var_ui"),
            uiOutput("tbl_trt_arms_ui"),
            hr(style = "margin:8px 0;"),

            # Show Total
            switchInput("show_total", "Show Total column",
                        value = TRUE, onLabel = "Yes", offLabel = "No",
                        size = "small", inline = FALSE),
            hr(style = "margin:8px 0;"),

            # Table-specific controls (dynamic)
            uiOutput("table_specific_ui"),

            # Export buttons — clinical TFL layout (title/N-header/footnote)
            tags$div(class = "fw-bold mb-1 mt-2", icon("download"), " Export"),
            downloadButton("dl_tbl_rtf",  "Table as RTF (clinical)",
                           class = "btn-sm btn-outline-dark w-100 mb-1"),
            downloadButton("dl_tbl_docx", "Table as Word (.docx)",
                           class = "btn-sm btn-outline-secondary w-100 mb-1"),
            downloadButton("dl_tbl_pdf",  "Table as PDF",
                           class = "btn-sm btn-outline-danger w-100 mb-1"),
            downloadButton("dl_tbl_html", "Table as HTML",
                           class = "btn-sm btn-outline-primary w-100 mb-1"),
            downloadButton("dl_tbl_csv",  "Data as CSV",
                           class = "btn-sm btn-outline-success w-100")
          )
        ),

        # Right: table output
        card(
          card_header(textOutput("tbl_header", inline = TRUE),
                      class = "bg-primary text-white fw-bold"),
          card_body(gt_output("tbl_output"), padding = "8px"),
          full_screen = TRUE
        )
      )
    ),

    # ── Tab 3: Figures ─────────────────────────────────────────────────────
    nav_panel(
      "Figures",
      icon = icon("chart-simple"),
      layout_columns(
        col_widths = c(3, 9),

        # Left: figure controls
        div(
          style = "overflow-y:auto; max-height:calc(100vh - 120px);",
          ctrl_card(
            tags$div(class = "fw-bold mb-2", icon("sliders"), " Figure Controls"),

            # Figure selector
            selectInput("figure_type", "Select Figure",
                        choices = FIGURE_CHOICES, selected = "boxplot"),
            hr(style = "margin:8px 0;"),

            # Per-figure analysis settings
            tags$div(
              style = "font-size:0.7rem;text-transform:uppercase;letter-spacing:.08em;
                       font-weight:700;color:#666;margin-bottom:4px;",
              icon("filter"), " Analysis Settings"
            ),
            uiOutput("fig_pop_flag_ui"),
            uiOutput("fig_trt_var_ui"),
            uiOutput("fig_trt_arms_ui"),
            hr(style = "margin:8px 0;"),

            # Figure-specific controls
            uiOutput("figure_specific_ui"),

            # Export
            tags$div(class = "fw-bold mb-1 mt-2", icon("download"), " Export"),
            downloadButton("dl_fig_png", "Figure as PNG",
                           class = "btn-sm btn-outline-primary w-100 mb-1"),
            downloadButton("dl_fig_pdf", "Figure as PDF",
                           class = "btn-sm btn-outline-danger w-100 mb-1"),
            downloadButton("dl_fig_svg", "Figure as SVG",
                           class = "btn-sm btn-outline-secondary w-100")
          )
        ),

        # Right: figure output
        card(
          card_header(textOutput("fig_header", inline = TRUE),
                      class = "bg-primary text-white fw-bold"),
          card_body(uiOutput("fig_output_ui"), padding = "8px"),
          full_screen = TRUE
        )
      )
    ),

    # ── Tab 4: Explore Data ────────────────────────────────────────────────
    nav_panel(
      "Explore Data",
      icon = icon("magnifying-glass-chart"),
      layout_columns(
        col_widths = c(3, 9),

        # Left: explore controls
        div(
          style = "overflow-y:auto; max-height:calc(100vh - 120px);",
          ctrl_card(
            tags$div(class = "fw-bold mb-2", icon("sliders"), " Explore Controls"),

            section_label("Domain", "layer-group"),
            radioButtons("exp_domain", NULL,
                        choices = c("Adverse Events" = "ae", "Demographics" = "demog"),
                        selected = "ae"),

            conditionalPanel(
              "input.exp_domain == 'ae'",
              tags$p(class = "drill-hint", style = "margin-top:-4px;",
                     "AE drill-down: System Organ Class → Preferred Term → Subject listing.
                      Click a bar in the chart, or a row in the table, to drill down.")
            ),
            conditionalPanel(
              "input.exp_domain == 'demog'",
              selectInput("exp_demog_var", "Variable",
                          choices = c("Age (years)"               = "AGE",
                                      "Age Group"                  = "AGEGR1",
                                      "Sex"                        = "SEX",
                                      "Race"                       = "RACE",
                                      "Treatment Duration (days)"  = "TRTDURD")),
              conditionalPanel(
                "input.exp_demog_var == 'AGE' || input.exp_demog_var == 'TRTDURD'",
                sliderInput("exp_demog_bins", "Bins", min = 4, max = 20, value = 8)
              ),
              tags$p(class = "drill-hint", style = "margin-top:-4px;",
                     "Click a bar in the chart, or a row in the table, to see the
                      matching subject listing.")
            ),
            hr(style = "margin:8px 0;"),

            section_label("Analysis Settings", "filter"),
            uiOutput("exp_pop_flag_ui"),
            uiOutput("exp_trt_var_ui"),
            uiOutput("exp_trt_arms_ui"),
            hr(style = "margin:8px 0;"),

            switchInput("exp_show_total", "Show Total column",
                        value = TRUE, onLabel = "Yes", offLabel = "No",
                        size = "small", inline = FALSE),
            conditionalPanel(
              "input.exp_domain == 'ae'",
              sliderInput("exp_top_n", "Show top N categories", min = 5, max = 25, value = 12)
            ),
            hr(style = "margin:8px 0;"),

            actionButton("exp_reset", "Reset drill-down",
                         icon = icon("rotate-left"),
                         class = "btn-sm btn-outline-secondary w-100 mb-2"),

            tags$div(class = "fw-bold mb-1 mt-2", icon("download"), " Export current view"),
            downloadButton("dl_exp_csv", "Table as CSV",
                           class = "btn-sm btn-outline-success w-100 mb-1"),
            downloadButton("dl_exp_png", "Figure as PNG",
                           class = "btn-sm btn-outline-primary w-100")
          )
        ),

        # Right: linked figure + table
        div(
          uiOutput("exp_breadcrumb_ui"),
          card(
            card_header(icon("chart-column"), " ", textOutput("exp_fig_header", inline = TRUE),
                        class = "bg-primary text-white fw-bold"),
            card_body(plotly::plotlyOutput("exp_plot", height = "360px"), padding = "8px"),
            full_screen = TRUE
          ),
          card(
            card_header(icon("table-list"), " ", textOutput("exp_tbl_header", inline = TRUE),
                        class = "bg-primary text-white fw-bold"),
            card_body(DTOutput("exp_table"), padding = "8px"),
            full_screen = TRUE
          )
        )
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Dataset loading ────────────────────────────────────────────────────────

  datasets <- reactive({
    if (input$data_source == "builtin") return(BUILTIN)

    req(input$upload_files)
    withProgress(message = "Loading datasets…", {
      result <- list()
      for (i in seq_len(nrow(input$upload_files))) {
        nm  <- toupper(file_path_sans_ext(input$upload_files$name[i]))
        fp  <- input$upload_files$datapath[i]
        ext <- tolower(file_ext(input$upload_files$name[i]))
        df  <- tryCatch(
          switch(ext,
                 "sas7bdat" = as.data.frame(haven::read_sas(fp)),
                 "csv"      = read.csv(fp, stringsAsFactors = FALSE),
                 "rds"      = readRDS(fp),
                 NULL
          ),
          error = function(e) NULL
        )
        if (!is.null(df)) result[[nm]] <- df
      }
      result
    })
  })

  ds_names <- reactive(names(datasets()))

  # ── Data status UI ─────────────────────────────────────────────────────────

  output$data_status_ui <- renderUI({
    nms <- ds_names()
    tagList(
      tags$div(style = "margin:6px 0 2px;",
               status_chip("ADSL", "ADSL" %in% nms,
                           if ("ADSL" %in% nms) paste0("(", nrow(datasets()$ADSL), " rows)")),
               status_chip("ADAE", "ADAE" %in% nms,
                           if ("ADAE" %in% nms) paste0("(", nrow(datasets()$ADAE), " rows)"))
      )
    )
  })

  # ── Shared data helpers ────────────────────────────────────────────────────

  adsl_cols <- reactive({
    req(datasets()$ADSL); names(datasets()$ADSL)
  })

  pop_flags_available <- reactive({
    fl    <- adsl_cols()[grepl("FL$", adsl_cols())]
    adsl  <- datasets()$ADSL
    valid <- fl[sapply(fl, function(v) all(adsl[[v]] %in% c("Y","N",NA)))]
    c("All Subjects (no filter)" = "__ALL__",
      setNames(valid, paste0(valid, " = Y")))
  })

  trt_vars_available <- reactive({
    cands <- c("TRT01A","TRT01P","ARM","ACTARM")
    found <- intersect(cands, adsl_cols())
    if (!length(found)) found <- grep("^TRT[0-9]", adsl_cols(), value = TRUE)
    found
  })

  trt_levels_for <- function(trt_var_input, pop_flag_input) {
    reactive({
      req(trt_var_input(), datasets()$ADSL)
      adsl <- datasets()$ADSL
      pop  <- if (!is.null(pop_flag_input()) && pop_flag_input() != "__ALL__")
        filter(adsl, .data[[pop_flag_input()]] == "Y")
      else adsl
      lvls <- sort(unique(as.character(pop[[trt_var_input()]])))
      lvls[!grepl("screen failure", lvls, ignore.case = TRUE)]
    })
  }

  # ── Per-Table analysis settings ────────────────────────────────────────────

  output$tbl_pop_flag_ui <- renderUI({
    selectInput("tbl_pop_flag", "Population Flag",
                choices = pop_flags_available(), selected = "SAFFL")
  })

  output$tbl_trt_var_ui <- renderUI({
    ch <- trt_vars_available()
    selectInput("tbl_trt_var", "Treatment Variable",
                choices = ch, selected = if ("TRT01A" %in% ch) "TRT01A" else ch[1])
  })

  tbl_trt_levels <- trt_levels_for(
    reactive(req(input$tbl_trt_var)),
    reactive(input$tbl_pop_flag)
  )

  output$tbl_trt_arms_ui <- renderUI({
    ch <- tbl_trt_levels()
    checkboxGroupInput("tbl_trt_arms", "Treatment Arms", choices = ch, selected = ch)
  })

  # ── Per-Figure analysis settings ───────────────────────────────────────────

  output$fig_pop_flag_ui <- renderUI({
    selectInput("fig_pop_flag", "Population Flag",
                choices = pop_flags_available(), selected = "SAFFL")
  })

  output$fig_trt_var_ui <- renderUI({
    ch <- trt_vars_available()
    selectInput("fig_trt_var", "Treatment Variable",
                choices = ch, selected = if ("TRT01A" %in% ch) "TRT01A" else ch[1])
  })

  fig_trt_levels <- trt_levels_for(
    reactive(req(input$fig_trt_var)),
    reactive(input$fig_pop_flag)
  )

  output$fig_trt_arms_ui <- renderUI({
    ch <- fig_trt_levels()
    checkboxGroupInput("fig_trt_arms", "Treatment Arms", choices = ch, selected = ch)
  })

  output$view_dataset_ui <- renderUI({
    selectInput("view_dataset", NULL,
                choices  = ds_names(),
                selected = ds_names()[1])
  })

  # ── Tab 1: View Data ───────────────────────────────────────────────────────

  output$data_viewer_ui <- renderUI({
    if (requireNamespace("ViewR", quietly = TRUE)) {
      ViewR::viewdtOutput("data_viewer", height = "70vh")
    } else {
      DTOutput("data_viewer")
    }
  })

  output$data_viewer <- if (requireNamespace("ViewR", quietly = TRUE)) {
    ViewR::renderViewdt({
      req(input$view_dataset)
      df <- datasets()[[input$view_dataset]]
      req(df)
      ViewR::viewdt(
        df,
        dataset_name = input$view_dataset,
        options = ViewR::viewdt_options(
          theme      = "auto",
          na_string  = "—",
          page_size  = 15
        )
      )
    })
  } else {
    renderDT({
      req(input$view_dataset)
      df <- datasets()[[input$view_dataset]]
      req(df)
      datatable(
        df,
        rownames  = FALSE,
        filter    = "top",
        options   = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "lftip",
          initComplete = JS(
            "function(s,j){$(this.api().table().header()).css({'background':'#2C3E7A','color':'#fff'});}"
          )
        ),
        class = "stripe hover compact"
      )
    })
  }

  output$view_valueboxes_ui <- renderUI({
    ds   <- datasets()
    adsl <- ds$ADSL
    adae <- ds$ADAE

    has_col <- function(d, col) !is.null(d) && col %in% names(d)
    fmt     <- function(x) if (is.null(x) || is.na(x)) "—" else format(x, big.mark = ",")
    fmt_pct <- function(n, N) {
      if (is.null(n) || is.na(n) || is.null(N) || is.na(N) || N == 0) return("—")
      sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / N)
    }

    n_ds   <- length(ds)
    n_subj <- if (!is.null(adsl)) nrow(adsl) else NA
    n_saf  <- if (has_col(adsl, "SAFFL")) sum(adsl$SAFFL == "Y", na.rm = TRUE) else NA
    n_ae   <- if (!is.null(adae)) nrow(adae) else NA

    safety_ids <- if (has_col(adsl, "SAFFL")) adsl$USUBJID[adsl$SAFFL == "Y"] else
      if (!is.null(adsl)) adsl$USUBJID else character(0)
    saf_denom  <- if (!is.na(n_saf)) n_saf else n_subj

    n_any_ae <- if (!is.null(adae) && has_col(adae, "TRTEMFL"))
      n_distinct(adae$USUBJID[adae$TRTEMFL == "Y" & adae$USUBJID %in% safety_ids]) else NA
    n_any_sae <- if (!is.null(adae) && has_col(adae, "AESER"))
      n_distinct(adae$USUBJID[adae$AESER == "Y" & adae$USUBJID %in% safety_ids]) else NA
    n_deaths <- if (has_col(adsl, "DTHFL")) sum(adsl$DTHFL == "Y", na.rm = TRUE) else NA

    trt_period <- if (has_col(adsl, "TRTSDT") && has_col(adsl, "TRTEDT") &&
                       any(!is.na(adsl$TRTSDT))) {
      paste0(format(min(adsl$TRTSDT, na.rm = TRUE), "%d %b %Y"), " – ",
             format(max(adsl$TRTEDT, na.rm = TRUE), "%d %b %Y"))
    } else "—"

    med_durn <- if (has_col(adsl, "TRTDURD") && any(!is.na(adsl$TRTDURD)))
      paste0(round(median(adsl$TRTDURD, na.rm = TRUE), 0), " days") else "—"

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Datasets Loaded", value = n_ds,
                showcase = icon("database"), theme = "primary"),
      value_box(title = "Subjects (ADSL)", value = fmt(n_subj),
                showcase = icon("users"), theme = "info"),
      value_box(title = "Safety Population", value = fmt(n_saf),
                showcase = icon("shield-heart"), theme = "success"),
      value_box(title = "AE Records (ADAE)", value = fmt(n_ae),
                showcase = icon("notes-medical"), theme = "warning"),
      value_box(title = "Subjects with ≥1 AE", value = fmt_pct(n_any_ae, saf_denom),
                showcase = icon("kit-medical"), theme = "warning"),
      value_box(title = "Subjects with ≥1 SAE", value = fmt_pct(n_any_sae, saf_denom),
                showcase = icon("triangle-exclamation"), theme = "danger"),
      value_box(title = "Deaths", value = fmt_pct(n_deaths, n_subj),
                showcase = icon("cross"), theme = "secondary"),
      value_box(title = "Median Trt. Duration", value = med_durn,
                subtitle = tags$span(icon("calendar-days"), " ", trt_period,
                                     style = "font-size:.72rem;color:#888;"),
                showcase = icon("hourglass-half"), theme = "info")
    )
  })

  output$data_summary_ui <- renderUI({
    req(input$view_dataset)
    df <- datasets()[[input$view_dataset]]
    req(df)
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    chr_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]

    tagList(
      tags$table(
        class = "table table-sm table-bordered",
        style = "font-size:0.82rem;",
        tags$tbody(
          tags$tr(tags$th("Rows"), tags$td(format(n_rows, big.mark = ","))),
          tags$tr(tags$th("Columns"), tags$td(n_cols)),
          tags$tr(tags$th("Numeric"), tags$td(length(num_cols))),
          tags$tr(tags$th("Character/Factor"), tags$td(length(chr_cols)))
        )
      ),
      if ("TRT01A" %in% names(df)) {
        tbl <- table(df$TRT01A)
        tagList(
          tags$p(tags$b("Subjects by Treatment (TRT01A):"), style = "margin-top:10px;font-size:0.82rem;"),
          tags$ul(style = "padding-left:16px;font-size:0.82rem;",
                  map(seq_along(tbl), function(i) tags$li(paste0(names(tbl)[i], ": ", tbl[i])))
          )
        )
      }
    )
  })

  # ── Tab 2: Tables — dynamic controls ──────────────────────────────────────

  output$table_specific_ui <- renderUI({
    switch(input$table_type,
           "ae_overview" = tagList(),
           "ae_socpt" = tagList(
             sliderInput("min_pct", "Min. incidence % (any arm)",
                         min = 0, max = 20, value = 0, step = 1),
             checkboxGroupInput("sev_filter", "AE Severity",
                                choices  = c("Mild"="MILD","Moderate"="MODERATE","Severe"="SEVERE"),
                                selected = c("MILD","MODERATE","SEVERE"), inline = TRUE)
           ),
           "ae_sev" = tagList(),
           tagList()  # demog, dispos have no extra controls
    )
  })

  output$tbl_header <- renderText({
    TABLE_TITLES[input$table_type]
  })

  # ── Table data reactive ────────────────────────────────────────────────────

  table_data <- reactive({
    req(input$tbl_trt_var, input$tbl_trt_arms, length(input$tbl_trt_arms) > 0,
        input$tbl_pop_flag, datasets()$ADSL)

    adsl <- datasets()$ADSL
    adae <- datasets()$ADAE
    trt  <- input$tbl_trt_var
    pop  <- input$tbl_pop_flag
    trts <- input$tbl_trt_arms
    tot  <- isTRUE(input$show_total)

    withProgress(message = "Building table…", {
      switch(input$table_type,
             demog       = build_demog(adsl, trt, pop, trts, tot),
             dispos      = build_dispos(adsl, trt, pop, trts, tot),
             ae_overview = { req(adae); build_ae_overview(adsl, adae, trt, pop, trts, tot) },
             ae_socpt    = { req(adae)
               build_ae_socpt(adsl, adae, trt, pop, trts, tot,
                              min_pct = input$min_pct %||% 0,
                              sevs    = input$sev_filter) },
             ae_sev      = { req(adae); build_ae_sev(adsl, adae, trt, pop, trts, tot) },
             NULL
      )
    })
  })

  output$tbl_output <- render_gt({
    df   <- req(table_data())
    lvls <- c(req(input$tbl_trt_arms), if (isTRUE(input$show_total)) "Total")
    to_gt(df, input$table_type, TABLE_TITLES[input$table_type], lvls)
  })

  # ── Table exports ──────────────────────────────────────────────────────────

  output$dl_tbl_csv <- downloadHandler(
    filename = function() paste0(input$table_type, "_", Sys.Date(), ".csv"),
    content  = function(f) {
      df <- req(table_data())
      write.csv(df |> select(-any_of("Level")), f, row.names = FALSE)
    }
  )

  # Table number / title / subtitle / footnote shared by every "clinical" export
  tbl_titling <- reactive({
    full  <- TABLE_TITLES[[input$table_type]]
    parts <- strsplit(full, " — ", fixed = TRUE)[[1]]  # split on " — "
    list(
      table_no = parts[1],
      title    = if (length(parts) > 1) paste(parts[-1], collapse = " — ") else full,
      subtitle = pop_label(input$tbl_pop_flag),
      footnote = paste0(pop_label(input$tbl_pop_flag), ". ",
                        "Percentages based on treatment group N. ",
                        "Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), ".")
    )
  })

  output$dl_tbl_html <- downloadHandler(
    filename = function() paste0(input$table_type, "_", Sys.Date(), ".html"),
    content  = function(f) {
      df   <- req(table_data())
      lvls <- c(req(input$tbl_trt_arms), if (isTRUE(input$show_total)) "Total")
      ti   <- tbl_titling()
      ft   <- build_clinical_flextable(df, lvls, ti$table_no, ti$title, ti$subtitle, ti$footnote)
      htmltools::save_html(htmltools_value(ft), file = f)
    }
  )

  output$dl_tbl_docx <- downloadHandler(
    filename = function() paste0(input$table_type, "_", Sys.Date(), ".docx"),
    content  = function(f) {
      df   <- req(table_data())
      lvls <- c(req(input$tbl_trt_arms), if (isTRUE(input$show_total)) "Total")
      ti   <- tbl_titling()
      ft   <- build_clinical_flextable(df, lvls, ti$table_no, ti$title, ti$subtitle, ti$footnote)
      save_as_docx(ft, path = f)
    }
  )

  output$dl_tbl_pdf <- downloadHandler(
    filename = function() paste0(input$table_type, "_", Sys.Date(), ".pdf"),
    content  = function(f) {
      df   <- req(table_data())
      lvls <- c(req(input$tbl_trt_arms), if (isTRUE(input$show_total)) "Total")
      ti   <- tbl_titling()

      orient <- table_orientation(df)
      dims   <- a4_dims(orient)
      w <- unname(dims["w"]); h <- unname(dims["h"])

      # Paginate long tables across multiple A4 pages, repeating the
      # title/N-header/footnote on each page (standard TFL pagination)
      hdr_row   <- df[1, , drop = FALSE]
      body_rows <- df[-1, , drop = FALSE]
      usable_h  <- h - 3.4
      rows_per_page <- max(5, floor(usable_h / 0.22))
      page_idx <- split(seq_len(nrow(body_rows)),
                        ceiling(seq_len(nrow(body_rows)) / rows_per_page))

      grDevices::cairo_pdf(f, width = w, height = h, onefile = TRUE)
      on.exit(grDevices::dev.off(), add = TRUE)
      for (idx in page_idx) {
        chunk_df <- bind_rows(hdr_row, body_rows[idx, , drop = FALSE])
        ft <- build_clinical_flextable(chunk_df, lvls, ti$table_no, ti$title,
                                       ti$subtitle, ti$footnote)
        ft <- fit_to_width(ft, max_width = w - 1.2)
        plot(ft)
      }
    }
  )

  output$dl_tbl_rtf <- downloadHandler(
    filename = function() paste0(input$table_type, "_", Sys.Date(), ".rtf"),
    content  = function(f) {
      df   <- req(table_data())
      lvls <- c(req(input$tbl_trt_arms), if (isTRUE(input$show_total)) "Total")
      ti   <- tbl_titling()
      orient <- table_orientation(df)
      rtf  <- build_clinical_rtf(df, lvls, ti$table_no, ti$title, ti$subtitle, ti$footnote,
                                 orientation = orient)
      rtf_encode(rtf) |> write_rtf(f)
    }
  )

  # ── Tab 3: Figures — dynamic controls ─────────────────────────────────────

  output$figure_specific_ui <- renderUI({
    switch(input$figure_type,
           "boxplot" = tagList(
             selectInput("box_yvar", "Y-axis Variable",
                         choices = c("Age (years)" = "AGE",
                                     "Treatment Duration (days)" = "TRTDURD")),
             selectInput("box_facet", "Facet by",
                         choices = c("None", "Sex" = "SEX", "Age Group" = "AGEGR1"),
                         selected = "None"),
             switchInput("box_pts",    "Show jitter points",
                         value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"),
             switchInput("box_violin", "Overlay violin",
                         value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")
           ),
           "histogram" = tagList(
             selectInput("hist_xvar", "Variable",
                         choices = c("Age (years)" = "AGE",
                                     "Treatment Duration (days)" = "TRTDURD")),
             sliderInput("hist_bins", "Bins", min = 5, max = 50, value = 20),
             switchInput("hist_density",   "Overlay density curve",
                         value = TRUE, onLabel = "Yes", offLabel = "No", size = "small"),
             switchInput("hist_facet_trt", "Facet by treatment",
                         value = FALSE, onLabel = "Yes", offLabel = "No", size = "small")
           ),
           "ae_bar" = tagList(
             sliderInput("ae_bar_topn", "Show top N terms", min = 5, max = 25, value = 10)
           )
    )
  })

  output$fig_header <- renderText({ FIGURE_TITLES[[input$figure_type]] })

  # Figure number / title / population subtitle / footnote — mirrors tbl_titling()
  fig_titling <- reactive({
    full  <- FIGURE_TITLES[[input$figure_type]]
    parts <- strsplit(full, " — ", fixed = TRUE)[[1]]
    list(
      fig_no   = parts[1],
      title    = if (length(parts) > 1) paste(parts[-1], collapse = " — ") else full,
      subtitle = pop_label(input$fig_pop_flag),
      footnote = paste0(pop_label(input$fig_pop_flag), ". ",
                        "Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), ".")
    )
  })

  # ── Figure reactive ────────────────────────────────────────────────────────

  active_plot <- reactive({
    req(input$fig_trt_var, input$fig_trt_arms, length(input$fig_trt_arms) > 0,
        input$fig_pop_flag, datasets()$ADSL)

    adsl <- datasets()$ADSL
    adae <- datasets()$ADAE
    trt  <- input$fig_trt_var
    pop  <- input$fig_pop_flag
    trts <- input$fig_trt_arms

    switch(input$figure_type,
           "boxplot" = {
             req(input$box_yvar)
             build_boxplot(adsl, trt, pop, trts,
                           yvar        = input$box_yvar,
                           facet_var   = input$box_facet,
                           show_pts    = isTRUE(input$box_pts),
                           show_violin = isTRUE(input$box_violin))
           },
           "histogram" = {
             req(input$hist_xvar, input$hist_bins)
             build_histogram(adsl, trt, pop, trts,
                             xvar         = input$hist_xvar,
                             bins         = input$hist_bins,
                             show_density = isTRUE(input$hist_density),
                             facet_trt    = isTRUE(input$hist_facet_trt))
           },
           "ae_bar" = {
             req(adae, input$ae_bar_topn)
             build_ae_bar(adsl, adae, trt, pop, trts, top_n = input$ae_bar_topn)
           }
    )
  })

  output$fig_output_ui <- renderUI({
    if (requireNamespace("plotly", quietly = TRUE)) {
      plotly::plotlyOutput("fig_output", height = "540px")
    } else {
      plotOutput("fig_output", height = "540px")
    }
  })

  output$fig_output <- if (requireNamespace("plotly", quietly = TRUE)) {
    renderPlotly({
      plt <- active_plot()
      if (inherits(plt, "ggplot")) {
        plotly::ggplotly(plt, tooltip = c("x", "y", "fill")) |>
          plotly::layout(legend = list(orientation = "h", x = 0, y = -0.15),
                         margin = list(l = 10, r = 10, b = 60, t = 60))
      } else {
        plt
      }
    })
  } else {
    renderPlot(active_plot(), res = 110)
  }

  output$dl_fig_png <- downloadHandler(
    filename = function() paste0(input$figure_type, "_", Sys.Date(), ".png"),
    content  = function(f) ggsave(f, active_plot(), width = 10, height = 6.5, dpi = 300)
  )

  output$dl_fig_svg <- downloadHandler(
    filename = function() paste0(input$figure_type, "_", Sys.Date(), ".svg"),
    content  = function(f) ggsave(f, active_plot(), width = 10, height = 6.5, device = "svg")
  )

  output$dl_fig_pdf <- downloadHandler(
    filename = function() paste0(input$figure_type, "_", Sys.Date(), ".pdf"),
    content  = function(f) {
      ti   <- fig_titling()
      plt  <- apply_clinical_figure_titles(active_plot(), ti$fig_no, ti$footnote)
      dims <- A4_LANDSCAPE
      ggsave(f, plt, width = unname(dims["w"]), height = unname(dims["h"]),
             device = grDevices::cairo_pdf)
    }
  )

  # ── Tab 4: Explore Data — linked table + figure with drill-down ───────────

  output$exp_pop_flag_ui <- renderUI({
    selectInput("exp_pop_flag", "Population Flag",
                choices = pop_flags_available(), selected = "SAFFL")
  })

  output$exp_trt_var_ui <- renderUI({
    ch <- trt_vars_available()
    selectInput("exp_trt_var", "Treatment Variable",
                choices = ch, selected = if ("TRT01A" %in% ch) "TRT01A" else ch[1])
  })

  exp_trt_levels <- trt_levels_for(
    reactive(req(input$exp_trt_var)),
    reactive(input$exp_pop_flag)
  )

  output$exp_trt_arms_ui <- renderUI({
    ch <- exp_trt_levels()
    checkboxGroupInput("exp_trt_arms", "Treatment Arms", choices = ch, selected = ch)
  })

  # Drill-down state — AE domain: soc -> pt -> subject listing (opt. filtered by severity)
  exp_state <- reactiveValues(level = "soc", soc = NULL, pt = NULL, sev = NULL)
  # Drill-down state — Demographics domain: top distribution -> subject listing for one bin
  exp_demog_sel <- reactiveValues(bin = NULL)

  DEMOG_VAR_LABELS <- c(AGE = "Age (years)", AGEGR1 = "Age Group", SEX = "Sex",
                        RACE = "Race", TRTDURD = "Treatment Duration (days)")
  DEMOG_LISTING_COLS <- c("USUBJID","TRT","AGE","AGEGR1","SEX","RACE",
                          "TRTSDT","TRTEDT","TRTDURD","EOSSTT")

  reset_exp_state <- function() {
    exp_state$level <- "soc"; exp_state$soc <- NULL
    exp_state$pt    <- NULL;  exp_state$sev <- NULL
    exp_demog_sel$bin <- NULL
  }

  observeEvent(input$exp_reset, reset_exp_state())

  observeEvent({
    input$exp_pop_flag; input$exp_trt_var; input$exp_trt_arms
    input$exp_domain; input$exp_demog_var; input$exp_demog_bins
  }, reset_exp_state(), ignoreInit = TRUE)

  observeEvent(input$exp_crumb_top, {
    exp_state$level <- "soc"; exp_state$soc <- NULL
    exp_state$pt <- NULL; exp_state$sev <- NULL
    exp_demog_sel$bin <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$exp_crumb_soc, {
    exp_state$level <- "pt"; exp_state$pt <- NULL; exp_state$sev <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$exp_crumb_pt, {
    exp_state$sev <- NULL
  }, ignoreInit = TRUE)

  output$exp_breadcrumb_ui <- renderUI({
    items <- list()
    if (identical(input$exp_domain, "demog")) {
      items[["All Subjects"]] <- "exp_crumb_top"
      if (!is.null(exp_demog_sel$bin)) {
        lbl <- DEMOG_VAR_LABELS[[input$exp_demog_var %||% "AGE"]] %||% "Group"
        items[[paste0(lbl, ": ", exp_demog_sel$bin)]] <- "exp_crumb_demog"
      }
    } else {
      items[["All SOCs"]] <- "exp_crumb_top"
      if (!is.null(exp_state$soc)) items[[exp_state$soc]] <- "exp_crumb_soc"
      if (!is.null(exp_state$pt))  items[[exp_state$pt]]  <- "exp_crumb_pt"
      if (!is.null(exp_state$sev))
        items[[paste0("Severity: ", exp_state$sev)]] <- "exp_crumb_sev"
    }
    breadcrumb_ui(items)
  })

  # ---- Demographics domain -------------------------------------------------

  exp_demog_base <- reactive({
    req(input$exp_trt_var, input$exp_trt_arms, length(input$exp_trt_arms) > 0,
        input$exp_pop_flag, datasets()$ADSL)
    p <- prep_adsl(datasets()$ADSL, input$exp_trt_var, input$exp_pop_flag,
                   input$exp_trt_arms, isTRUE(input$exp_show_total))
    list(d = p$d, N = p$N, lvls = p$all_lvls)
  })

  exp_demog_view <- reactive({
    b <- exp_demog_base(); d <- b$d; N <- b$N; lvls <- b$lvls
    colors  <- setNames(TRT_PALETTE[seq_along(lvls)], lvls)
    var     <- input$exp_demog_var %||% "AGE"
    is_cont <- var %in% c("AGE", "TRTDURD")
    varlbl  <- DEMOG_VAR_LABELS[[var]] %||% var

    if (is_cont) {
      bins <- input$exp_demog_bins %||% 8
      rng  <- range(d[[var]][d$TRT != "Total"], na.rm = TRUE)
      brks <- pretty(rng, n = bins)
      d <- mutate(d, BIN = cut(.data[[var]], breaks = brks, include.lowest = TRUE, dig.lab = 5))
      ord <- levels(d$BIN)
    } else {
      d   <- mutate(d, BIN = as.character(.data[[var]]))
      ord <- sort(unique(d$BIN[d$TRT != "Total"]))
    }

    long <- group_incidence(d, "BIN", N, lvls)
    wide <- group_incidence_wide(long, "BIN") |> rename(Term = BIN)

    base_list <- list(long = long, wide = wide, group_col = "BIN", order = ord,
                      colors = colors, lvls = lvls,
                      fig_title = paste0("Distribution of ", varlbl, " by Treatment"))

    if (is.null(exp_demog_sel$bin)) {
      c(base_list, list(
        tbl_title = paste0(varlbl, " — counts (click a bar or row to view subjects)")))
    } else {
      listing <- d |>
        filter(TRT != "Total", as.character(BIN) == exp_demog_sel$bin) |>
        select(any_of(DEMOG_LISTING_COLS)) |>
        arrange(TRT, USUBJID)
      c(base_list, list(
        listing = listing,
        tbl_title = paste0("Subject listing — ", varlbl, ": ", exp_demog_sel$bin)))
    }
  })

  # ---- Adverse Events domain -----------------------------------------------

  # Base AE data filtered by the shared population/treatment/arm settings
  exp_base <- reactive({
    req(input$exp_trt_var, input$exp_trt_arms, length(input$exp_trt_arms) > 0,
        input$exp_pop_flag, datasets()$ADSL, datasets()$ADAE)

    adsl <- datasets()$ADSL
    adae <- datasets()$ADAE
    p <- prep_adsl(adsl, input$exp_trt_var, input$exp_pop_flag, input$exp_trt_arms,
                   isTRUE(input$exp_show_total))
    d_sl <- p$d; N <- p$N; lvls <- p$all_lvls

    d_ae <- adae |>
      filter(USUBJID %in% d_sl$USUBJID[d_sl$TRT != "Total"], TRTEMFL == "Y") |>
      left_join(select(d_sl, USUBJID, TRT) |> filter(TRT != "Total"), by = "USUBJID")

    if ("Total" %in% lvls)
      d_ae <- bind_rows(d_ae, mutate(d_ae, TRT = factor("Total", levels = lvls)))
    d_ae$TRT <- factor(as.character(d_ae$TRT), levels = lvls)

    list(d_ae = d_ae, N = N, lvls = lvls)
  })

  # Current chart + table content, driven by exp_base() and the drill-down state
  exp_view <- reactive({
    b <- exp_base()
    d_ae <- b$d_ae; N <- b$N; lvls <- b$lvls
    colors <- setNames(TRT_PALETTE[seq_along(lvls)], lvls)
    top_n  <- input$exp_top_n %||% 12

    top_terms_of <- function(long, col) {
      long |>
        filter(TRT != "Total") |>
        group_by(.data[[col]]) |>
        summarise(m = max(pct), .groups = "drop") |>
        slice_max(order_by = m, n = top_n, with_ties = FALSE) |>
        pull(!!sym(col))
    }

    if (exp_state$level == "soc") {
      long <- group_incidence(d_ae, "AEBODSYS", N, lvls)
      keep <- top_terms_of(long, "AEBODSYS")
      long <- filter(long, AEBODSYS %in% keep)
      wide <- group_incidence_wide(long, "AEBODSYS") |> rename(Term = AEBODSYS)
      list(long = long, wide = wide, group_col = "AEBODSYS", colors = colors, lvls = lvls,
           fig_title = "AE Incidence by System Organ Class",
           tbl_title = "System Organ Class — incidence (click a bar or row to drill down)")

    } else if (exp_state$level == "pt") {
      d_soc <- filter(d_ae, AEBODSYS == exp_state$soc)
      long  <- group_incidence(d_soc, "AEDECOD", N, lvls)
      keep  <- top_terms_of(long, "AEDECOD")
      long  <- filter(long, AEDECOD %in% keep)
      wide  <- group_incidence_wide(long, "AEDECOD") |> rename(Term = AEDECOD)
      list(long = long, wide = wide, group_col = "AEDECOD", colors = colors, lvls = lvls,
           fig_title = paste0("Preferred Terms within ", exp_state$soc),
           tbl_title = paste0("Preferred Term incidence — ", exp_state$soc,
                              " (click a bar or row to drill down)"))

    } else {
      d_pt <- filter(d_ae, AEBODSYS == exp_state$soc, AEDECOD == exp_state$pt)
      long <- group_incidence(d_pt, "AESEV", N, lvls)
      wide <- group_incidence_wide(long, "AESEV") |> rename(Term = AESEV)

      listing <- d_pt |>
        filter(TRT != "Total") |>
        { \(x) if (!is.null(exp_state$sev)) filter(x, AESEV == exp_state$sev) else x }() |>
        select(any_of(c("USUBJID","TRT","AEDECOD","AESEV","AESER","AEACN",
                        "AESTDTC","AEENDTC"))) |>
        arrange(TRT, USUBJID)

      list(long = long, wide = wide, group_col = "AESEV", colors = colors, lvls = lvls,
           listing = listing,
           fig_title = paste0("Severity — ", exp_state$pt,
                              " (click a bar to filter the subject listing)"),
           tbl_title = paste0("Subject listing — ", exp_state$pt,
                              if (!is.null(exp_state$sev)) paste0(" — ", exp_state$sev) else ""))
    }
  })

  # Dispatch to whichever domain is active — keeps the rest of the tab domain-agnostic
  exp_current <- reactive({
    if (identical(input$exp_domain, "demog")) exp_demog_view() else exp_view()
  })
  exp_is_listing <- function(v) !is.null(v$listing)

  output$exp_fig_header <- renderText({ exp_current()$fig_title })
  output$exp_tbl_header <- renderText({ exp_current()$tbl_title })

  output$exp_plot <- plotly::renderPlotly({
    v  <- exp_current()
    gg <- build_link_bar(v$long, v$group_col, v$colors, order = v$order)
    plotly::ggplotly(gg, tooltip = "text", source = "exp_plot_src") |>
      plotly::layout(legend = list(orientation = "h", y = -0.2),
                     margin = list(l = 10, r = 10, t = 10, b = 40))
  })

  output$exp_table <- renderDT({
    v <- exp_current()
    if (exp_is_listing(v)) {
      datatable(v$listing, rownames = FALSE, selection = "none", filter = "top",
                options = list(pageLength = 10, scrollX = TRUE, dom = "lftip"))
    } else {
      datatable(v$wide, rownames = FALSE,
                selection = list(mode = "single", target = "row"),
                options = list(pageLength = 10, scrollX = TRUE, dom = "lftip"))
    }
  })

  exp_tbl_proxy <- DT::dataTableProxy("exp_table")

  # Table selection drills down (SOC row -> PT list -> subject listing;
  # or, in Demographics, bin/category row -> matching subject listing)
  observeEvent(input$exp_table_rows_selected, {
    sel <- input$exp_table_rows_selected
    req(length(sel) > 0)

    if (identical(isolate(input$exp_domain), "demog")) {
      v <- isolate(exp_demog_view())
      if (!exp_is_listing(v)) exp_demog_sel$bin <- as.character(v$wide$Term[sel])
    } else {
      v <- isolate(exp_view())
      if (isolate(exp_state$level) == "soc") {
        exp_state$soc <- v$wide$Term[sel]; exp_state$level <- "pt"; exp_state$pt <- NULL
      } else if (isolate(exp_state$level) == "pt") {
        exp_state$pt <- v$wide$Term[sel]; exp_state$level <- "subject"; exp_state$sev <- NULL
      }
    }
    DT::selectRows(exp_tbl_proxy, NULL)
  }, ignoreNULL = TRUE)

  # Clicking a bar in the chart drills down the same way as selecting a table row
  observeEvent(plotly::event_data("plotly_click", source = "exp_plot_src"), {
    ep <- plotly::event_data("plotly_click", source = "exp_plot_src")
    req(ep, ep$y)
    clicked <- as.character(ep$y)

    if (identical(input$exp_domain, "demog")) {
      exp_demog_sel$bin <- clicked
    } else if (exp_state$level == "soc") {
      exp_state$soc <- clicked; exp_state$level <- "pt"; exp_state$pt <- NULL
    } else if (exp_state$level == "pt") {
      exp_state$pt <- clicked; exp_state$level <- "subject"; exp_state$sev <- NULL
    } else {
      exp_state$sev <- clicked
    }
  })

  output$dl_exp_csv <- downloadHandler(
    filename = function() paste0("explore_", input$exp_domain, "_", Sys.Date(), ".csv"),
    content  = function(f) {
      v   <- exp_current()
      out <- if (exp_is_listing(v)) v$listing else v$wide
      write.csv(out, f, row.names = FALSE)
    }
  )

  output$dl_exp_png <- downloadHandler(
    filename = function() paste0("explore_", input$exp_domain, "_", Sys.Date(), ".png"),
    content  = function(f) {
      v  <- exp_current()
      gg <- build_link_bar(v$long, v$group_col, v$colors, order = v$order) +
        labs(title = v$fig_title)
      ggsave(f, gg, width = 10, height = 6.5, dpi = 300)
    }
  )
}

# ── NULL coalescing helper ────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)
