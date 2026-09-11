# =========================================================
# R Shiny Learning Dashboard
# Equal chart and table windows with responsive sizing
# =========================================================

required_packages <- c("shiny", "ggplot2", "plotly", "DT", "dplyr", "scales")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Please install these packages first: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(scales)

set.seed(123)

create_gapminder_sample <- function() {
  base <- expand.grid(
    country = c("India", "United States", "Germany", "Japan", "Brazil"),
    continent = c("Asia", "Americas", "Europe"),
    year = seq(2000, 2020, by = 5),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  base$lifeExp <- round(runif(nrow(base), 58, 84) + (base$year - 2000) / 6, 1)
  base$pop <- round(runif(nrow(base), 5000000, 1400000000))
  base$gdpPercap <- round(runif(nrow(base), 1000, 65000), 0)
  base <- base[!duplicated(base[c("country", "year")]), ]
  rownames(base) <- NULL
  base
}

create_clinical_sample <- function() {
  base <- expand.grid(
    STUDY = c("Oncology", "Diabetes", "Cardiology"),
    SUBGROUP = c("Male", "Female"),
    MONTH_NUMBER = 1:12,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  base$MONTH <- month.abb[base$MONTH_NUMBER]
  base$ENROLLMENT <- ifelse(
    base$STUDY == "Oncology",
    20 + base$MONTH_NUMBER * 12 + ifelse(base$SUBGROUP == "Female", 6, 0),
    ifelse(
      base$STUDY == "Diabetes",
      15 + base$MONTH_NUMBER * 10 + ifelse(base$SUBGROUP == "Female", 5, 0),
      18 + base$MONTH_NUMBER * 11 + ifelse(base$SUBGROUP == "Female", 4, 0)
    )
  )
  base$SITE_COUNT <- ifelse(
    base$STUDY == "Oncology",
    5 + round(base$MONTH_NUMBER / 2),
    ifelse(
      base$STUDY == "Diabetes",
      4 + round(base$MONTH_NUMBER / 2),
      6 + round(base$MONTH_NUMBER / 2)
    )
  )
  base$SAE_COUNT <- sample(0:8, nrow(base), replace = TRUE)
  base$COMPLETION_RATE <- round(runif(nrow(base), 0.78, 0.99), 2)
  base
}

mtcars_data <- data.frame(model = rownames(mtcars), mtcars, row.names = NULL)
diamonds_data <- ggplot2::diamonds[sample(seq_len(nrow(ggplot2::diamonds)), 1200), ]

data_store <- list(
  "Level 1 - mtcars" = mtcars_data,
  "Level 2 - iris" = iris,
  "Level 3 - diamonds" = diamonds_data,
  "Level 4 - gapminder sample" = create_gapminder_sample(),
  "Level 5 - clinical trial sample" = create_clinical_sample()
)

level_meta <- data.frame(
  Level = paste("Level", 1:5),
  Title = c("R Fundamentals", "First Shiny App", "Intermediate Shiny", "Advanced Modular Shiny", "Enterprise Deployment"),
  Dataset = c("mtcars", "iris", "diamonds", "gapminder sample", "clinical trial sample"),
  Status = c("Completed", "Completed", "Current", "Pending", "Pending"),
  stringsAsFactors = FALSE
)

get_numeric_columns <- function(data) names(data)[vapply(data, is.numeric, logical(1))]
get_level_from_dataset <- function(dataset_name) sub(" -.*$", "", dataset_name)

make_kpi_card <- function(icon_name, label, value, note, color = "#0f6bca") {
  div(
    class = "kpi-card",
    div(class = "kpi-icon", style = paste0("background:", color, ";"), icon(icon_name)),
    div(class = "kpi-text", div(class = "kpi-label", label), div(class = "kpi-value", value), div(class = "kpi-note", note))
  )
}

ui <- fluidPage(
  style = "width:100vw; max-width:100vw; overflow-x:hidden; padding:0; margin:0;",
  tags$head(
    tags$title("R Shiny Learning Dashboard"),
    tags$script(HTML('
      function resizeAllPlotly() {
        setTimeout(function() {
          var plots = document.querySelectorAll(".js-plotly-plot");
          for (var i = 0; i < plots.length; i++) {
            if (window.Plotly) Plotly.Plots.resize(plots[i]);
          }
        }, 200);
      }
      Shiny.addCustomMessageHandler("resizePlots", resizeAllPlotly);
      window.addEventListener("resize", resizeAllPlotly);
    ')),
    tags$style(HTML('
      :root {
        --navy:#072a4a; --blue:#0f6bca; --teal:#00a6a6; --light:#f5f8fb;
        --line:#d8e2ea; --text:#11233f; --muted:#657789; --purple:#7757d9;
        --green:#45a857; --orange:#f5761a; --visual-height:340px;
      }
      html,body { width:100%; max-width:100%; min-height:100%; margin:0; padding:0; overflow-x:hidden; }
      body { background:var(--light); color:var(--text); font-family:Arial,Helvetica,sans-serif; }
      .app-shell { display:grid; grid-template-columns:230px minmax(0,1fr); width:100vw; min-height:100vh; overflow-x:hidden; box-sizing:border-box; }
      .sidebar { background:linear-gradient(180deg,#062643 0%,#083b68 100%); color:white; padding:20px 16px; }
      .brand { display:flex; align-items:center; gap:12px; margin-bottom:26px; }
      .brand-badge { width:52px; height:52px; border-radius:16px; background:white; color:var(--blue); display:flex; align-items:center; justify-content:center; font-weight:800; font-size:30px; box-shadow:0 6px 14px rgba(0,0,0,.20); }
      .brand-title { font-size:18px; font-weight:700; line-height:1.15; white-space:pre-line; }
      .nav-item { padding:12px 14px; border-radius:10px; margin:8px 0; background:rgba(255,255,255,.05); color:#e8f5ff; display:flex; align-items:center; gap:10px; font-weight:600; }
      .nav-item.active { background:linear-gradient(90deg,var(--teal),#0077bd); color:white; }
      .progress-panel { margin-top:28px; border:1px solid rgba(255,255,255,.18); border-radius:14px; padding:16px; background:rgba(255,255,255,.06); }
      .progress-circle { width:96px; height:96px; border-radius:50%; margin:10px auto; background:conic-gradient(var(--teal) 0 75%,rgba(255,255,255,.18) 75% 100%); display:flex; align-items:center; justify-content:center; }
      .progress-circle-inner { width:72px; height:72px; border-radius:50%; background:#083b68; display:flex; align-items:center; justify-content:center; font-size:22px; font-weight:800; }
      .main { width:100%; min-width:0; max-width:100%; padding:15px; box-sizing:border-box; overflow-x:hidden; }
      .topbar { display:flex; align-items:center; justify-content:space-between; margin-bottom:20px; gap:12px; }
      .top-title h1 { margin:0; font-weight:800; color:var(--navy); letter-spacing:.2px; }
      .top-title p { margin:4px 0 0; color:var(--muted); font-size:16px; }
      .theme-badge { padding:10px 14px; border:1px solid var(--line); border-radius:10px; background:white; color:var(--navy); font-weight:700; white-space:nowrap; }
      .kpi-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(190px,1fr)); gap:12px; width:100%; margin-bottom:14px; box-sizing:border-box; }
      .kpi-card { background:white; border:1px solid var(--line); border-radius:14px; box-shadow:0 6px 18px rgba(20,50,80,.08); padding:14px; display:flex; min-height:90px; align-items:center; gap:12px; min-width:0; }
      .kpi-icon { width:48px; height:48px; flex:0 0 48px; border-radius:50%; display:flex; align-items:center; justify-content:center; color:white; font-size:20px; }
      .kpi-label { color:var(--muted); font-weight:700; font-size:11px; text-transform:uppercase; }
      .kpi-value { font-size:23px; font-weight:800; color:var(--navy); line-height:1; margin-top:4px; }
      .kpi-note,.small-muted { color:var(--muted); font-size:12px; margin-top:4px; }
      .dashboard-grid { display:grid; grid-template-columns:240px minmax(0,1fr) 240px; gap:14px; width:100%; max-width:100%; box-sizing:border-box; align-items:start; }
      .panel-card { background:white; border:1px solid var(--line); border-radius:14px; box-shadow:0 6px 18px rgba(20,50,80,.08); padding:14px; margin-bottom:14px; width:100%; min-width:0; box-sizing:border-box; }
      .panel-title { font-size:15px; font-weight:800; color:var(--navy); margin-bottom:10px; display:flex; align-items:center; gap:8px; }
      label { color:var(--navy); font-size:12px; font-weight:700; }
      .btn-apply { width:100%; background:linear-gradient(90deg,var(--teal),#0077bd)!important; color:white!important; border:none!important; border-radius:8px!important; font-weight:700!important; }
      .chart-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); grid-auto-rows:var(--visual-height); gap:14px; width:100%; max-width:100%; box-sizing:border-box; align-items:stretch; }
      .visual-panel { height:var(--visual-height); min-height:var(--visual-height); max-height:var(--visual-height); margin-bottom:0; padding:12px; display:flex; flex-direction:column; overflow:hidden; }
      .visual-panel .panel-title { flex:0 0 auto; min-height:22px; margin-bottom:6px; }
      .visual-body,.table-visual-body { flex:1 1 auto; width:100%; min-width:0; min-height:0; overflow:hidden; position:relative; }
      .visual-body .shiny-plot-output,.visual-body .html-widget,.visual-body .plotly,.visual-body .plot-container,.visual-body .svg-container,.visual-body .js-plotly-plot { width:100%!important; max-width:100%!important; height:100%!important; min-width:0!important; min-height:0!important; box-sizing:border-box!important; }
      .table-visual-body .datatables,.table-visual-body .dataTables_wrapper { width:100%!important; max-width:100%!important; height:100%!important; min-width:0!important; box-sizing:border-box!important; }
      .table-visual-body .dataTables_wrapper { overflow:hidden; }
      .table-visual-body .dataTables_scroll { width:100%!important; }
      .table-visual-body .dataTables_scrollBody { max-height:190px!important; height:190px!important; overflow:auto!important; }
      .table-visual-body .dt-buttons { margin-bottom:3px; }
      .table-visual-body .dt-button { padding:2px 7px!important; font-size:10px!important; }
      .table-visual-body .dataTables_filter,.table-visual-body .dataTables_length,.table-visual-body .dataTables_info,.table-visual-body .dataTables_paginate,.table-visual-body table.dataTable { font-size:10px!important; }
      .table-visual-body table.dataTable th,.table-visual-body table.dataTable td { padding:3px 6px!important; white-space:nowrap; }
      .deploy-card { text-align:center; }
      .cloud-icon { margin:12px auto 18px; width:120px; height:76px; border-radius:42px; background:linear-gradient(135deg,#c9e9ff,var(--blue)); box-shadow:0 10px 22px rgba(15,107,202,.18); display:flex; align-items:center; justify-content:center; color:white; font-size:32px; }
      .deploy-list { text-align:left; margin-top:12px; color:var(--text); font-weight:600; }
      .deploy-list div { margin:8px 0; }
      .journey { margin-top:14px; background:var(--navy); color:white; border-radius:16px; padding:16px; }
      .journey-title { font-size:18px; font-weight:800; margin-bottom:12px; }
      .journey-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(160px,1fr)); gap:10px; width:100%; }
      .journey-step { background:white; color:var(--navy); border-radius:12px; padding:12px; text-align:center; min-height:92px; position:relative; }
      .journey-step.current { outline:3px solid #77dfaa; }
      .step-status { position:absolute; top:8px; right:8px; width:22px; height:22px; border-radius:50%; background:var(--teal); color:white; font-size:12px; display:flex; align-items:center; justify-content:center; }
      .step-title { font-weight:800; margin-top:16px; }
      .step-subtitle { margin-top:5px; color:var(--muted); font-size:12px; }
      .left-column,.center-column,.right-column { min-width:0!important; max-width:100%!important; width:100%!important; box-sizing:border-box; }
      @media (min-width:1600px) { .dashboard-grid { grid-template-columns:240px minmax(0,1fr) 240px; } }
      @media (max-width:1500px) {
        :root { --visual-height:330px; }
        .dashboard-grid { grid-template-columns:230px minmax(0,1fr); }
        .right-column { grid-column:1 / span 2; display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:14px; }
        .table-visual-body .dataTables_scrollBody { height:180px!important; max-height:180px!important; }
      }
      @media (max-width:1100px) {
        :root { --visual-height:350px; }
        .dashboard-grid { grid-template-columns:1fr; }
        .chart-grid { grid-template-columns:1fr; }
        .right-column { grid-column:auto; display:block; }
        .table-visual-body .dataTables_scrollBody { height:205px!important; max-height:205px!important; }
      }
      @media (max-width:768px) {
        :root { --visual-height:330px; }
        .sidebar { display:none; }
        .app-shell { grid-template-columns:1fr; }
        .main { padding:10px; }
        .topbar { display:block; }
        .theme-badge { display:inline-block; margin-top:10px; }
        .kpi-grid,.chart-grid,.journey-grid { grid-template-columns:1fr; }
        .table-visual-body .dataTables_scrollBody { height:185px!important; max-height:185px!important; }
      }
    '))
  ),
  div(
    class = "app-shell",
    div(
      class = "sidebar",
      div(class = "brand", div(class = "brand-badge", "R"), div(class = "brand-title", "R Shiny\nLearning Dashboard")),
      div(class = "nav-item active", icon("home"), "Dashboard"),
      div(class = "nav-item", icon("database"), "Data Explorer"),
      div(class = "nav-item", icon("chart-line"), "Visualizations"),
      div(class = "nav-item", icon("file-alt"), "Reports"),
      div(class = "nav-item", icon("cloud-upload-alt"), "Deploy & Share"),
      div(class = "nav-item", icon("book"), "Learn"),
      div(class = "nav-item", icon("question-circle"), "Help & Docs"),
      div(class = "progress-panel", h4("Learning Progress"), div(class = "progress-circle", div(class = "progress-circle-inner", "75%")), div(style = "text-align:center;font-weight:700;", "Level 3"), div(style = "text-align:center;color:#cdeeff;", "Intermediate"))
    ),
    div(
      class = "main",
      div(class = "topbar", div(class = "top-title", h1("R Shiny Learning Dashboard"), p("Interactive Data Science with R")), div(class = "theme-badge", icon("palette"), " Modern Blue Theme")),
      uiOutput("kpi_cards"),
      div(
        class = "dashboard-grid",
        div(
          class = "left-column",
          div(class = "panel-card", div(class = "panel-title", icon("filter"), "Input Controls"), selectInput("dataset", "Select Dataset", choices = names(data_store), selected = "Level 1 - mtcars"), selectInput("xvar", "X Axis", choices = NULL), selectInput("yvar", "Y Axis", choices = NULL), selectInput("colorvar", "Color By Optional", choices = NULL), sliderInput("point_size", "Point Size", min = 1, max = 7, value = 3, step = 1), sliderInput("row_limit", "Rows Displayed", min = 25, max = 1200, value = 300, step = 25), actionButton("apply", "Apply Filters", icon = icon("play"), class = "btn-apply")),
          div(class = "panel-card", div(class = "panel-title", icon("info-circle"), "Dashboard Info"), p("Use the controls above to explore datasets, visualizations, and curriculum levels."), p(class = "small-muted", textOutput("current_level_text")))
        ),
        div(
          class = "center-column",
          div(
            class = "chart-grid",
            div(class = "panel-card visual-panel", div(class = "panel-title", icon("chart-line"), "Scatter Plot"), div(class = "visual-body", plotlyOutput("scatter", width = "100%", height = "100%"))),
            div(class = "panel-card visual-panel", div(class = "panel-title", icon("chart-bar"), "Distribution"), div(class = "visual-body", plotlyOutput("distribution", width = "100%", height = "100%"))),
            div(class = "panel-card visual-panel", div(class = "panel-title", icon("chart-line"), "Trend Line"), div(class = "visual-body", plotlyOutput("trend", width = "100%", height = "100%"))),
            div(class = "panel-card visual-panel", div(class = "panel-title", icon("table"), "Data Table"), div(class = "table-visual-body", DTOutput("table", width = "100%", height = "100%")))
          )
        ),
        div(
          class = "right-column",
          div(class = "panel-card deploy-card", div(class = "panel-title", icon("cloud-upload-alt"), "Deploy & Share"), div(class = "cloud-icon", icon("cloud-upload-alt")), h4("Deploy your app to the cloud"), p(class = "small-muted", "Share insights securely with your team or organization."), div(class = "deploy-list", div(icon("check-circle"), " Connect to Posit Connect"), div(icon("check-circle"), " Publish application"), div(icon("check-circle"), " Manage access"), div(icon("check-circle"), " Monitor usage"))),
          div(class = "panel-card", div(class = "panel-title", icon("graduation-cap"), "Current Module"), h4(textOutput("module_name")), p(textOutput("module_dataset")), p(class = "small-muted", textOutput("module_tip")))
        )
      ),
      div(class = "journey", div(class = "journey-title", "Your Learning Journey"), uiOutput("journey"))
    )
  )
)

server <- function(input, output, session) {
  session$onFlushed(function() session$sendCustomMessage("resizePlots", list()), once = FALSE)

  observeEvent(input$dataset, {
    data <- data_store[[input$dataset]]
    numeric_choices <- get_numeric_columns(data)
    updateSelectInput(session, "xvar", choices = numeric_choices, selected = numeric_choices[1])
    updateSelectInput(session, "yvar", choices = numeric_choices, selected = numeric_choices[min(2, length(numeric_choices))])
    updateSelectInput(session, "colorvar", choices = c("None", names(data)), selected = "None")
    session$sendCustomMessage("resizePlots", list())
  }, ignoreInit = FALSE)

  observeEvent(input$apply, session$sendCustomMessage("resizePlots", list()))

  selected_data <- reactive({
    req(input$dataset)
    data <- data_store[[input$dataset]]
    n_limit <- if (is.null(input$row_limit)) 300 else input$row_limit
    data[seq_len(min(nrow(data), n_limit)), , drop = FALSE]
  })

  current_level <- reactive({ req(input$dataset); get_level_from_dataset(input$dataset) })
  output$current_level_text <- renderText(paste("Currently viewing:", input$dataset))
  output$module_name <- renderText({ meta <- level_meta[level_meta$Level == current_level(), ]; if (!nrow(meta)) "Learning Module" else meta$Title })
  output$module_dataset <- renderText({ meta <- level_meta[level_meta$Level == current_level(), ]; if (!nrow(meta)) "" else paste("Dataset:", meta$Dataset) })
  output$module_tip <- renderText(switch(current_level(), "Level 1" = "Focus on loading data, inspecting columns, and creating a scatter plot.", "Level 2" = "Focus on UI, server, input controls, and plot output.", "Level 3" = "Focus on reactive filtering and interactive graphs.", "Level 4" = "Focus on modular architecture and reusable components.", "Level 5" = "Focus on deployment, governance, and clinical reporting patterns.", "Explore the selected dataset and visual outputs."))

  output$kpi_cards <- renderUI({
    data <- selected_data(); source_data <- data_store[[input$dataset]]
    color_filter_active <- isTRUE(!is.null(input$colorvar) && input$colorvar != "None")
    active_filter_count <- sum(!is.null(input$dataset), !is.null(input$xvar), !is.null(input$yvar), color_filter_active)
    div(class = "kpi-grid",
      make_kpi_card("database", "Datasets Loaded", length(data_store), "mtcars, iris, diamonds, gapminder, clinical", "#0f6bca"),
      make_kpi_card("chart-line", "Visualizations", 4, "Scatter, distribution, trend, table", "#00a6a6"),
      make_kpi_card("filter", "Active Filters", active_filter_count, "Dataset and variable selections", "#7757d9"),
      make_kpi_card("users", "Records Filtered", scales::comma(nrow(data)), paste("of", scales::comma(nrow(source_data)), "total"), "#f5761a"),
      make_kpi_card("check-circle", "Last Updated", format(Sys.Date(), "%d %b %Y"), format(Sys.time(), "%I:%M %p"), "#45a857")
    )
  })

  compact_plotly <- function(p) {
    ggplotly(p + theme_minimal(base_size = 10) + theme(legend.position = "bottom", plot.margin = margin(2, 2, 2, 2))) %>%
      layout(autosize = TRUE, margin = list(l = 45, r = 10, b = 42, t = 8, pad = 1), legend = list(orientation = "h", x = 0, y = -0.16)) %>%
      config(responsive = TRUE, displaylogo = FALSE)
  }

  output$scatter <- renderPlotly({
    data <- selected_data(); req(input$xvar, input$yvar)
    if (!all(c(input$xvar, input$yvar) %in% names(data))) return(NULL)
    if (isTRUE(input$colorvar != "None") && input$colorvar %in% names(data)) {
      p <- ggplot(data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = .data[[input$colorvar]])) + geom_point(size = input$point_size, alpha = .78) + labs(x = input$xvar, y = input$yvar, color = input$colorvar)
    } else {
      p <- ggplot(data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) + geom_point(size = input$point_size, alpha = .78, color = "#0f6bca") + labs(x = input$xvar, y = input$yvar)
    }
    compact_plotly(p)
  })

  output$distribution <- renderPlotly({
    data <- selected_data(); req(input$xvar)
    if (!input$xvar %in% names(data)) return(NULL)
    x <- data[[input$xvar]]; x <- x[is.finite(x)]
    req(length(x) > 1)
    bins <- 25
    bin_width <- diff(range(x)) / bins
    if (!is.finite(bin_width) || bin_width <= 0) bin_width <- 1
    p <- ggplot(data, aes(x = .data[[input$xvar]])) +
      geom_histogram(fill = "#9cc9f3", color = "white", bins = bins) +
      geom_density(aes(y = after_stat(density * length(x) * bin_width)), color = "#0f6bca", linewidth = 1.1) +
      labs(x = input$xvar, y = "Count")
    compact_plotly(p)
  })

  output$trend <- renderPlotly({
    data <- selected_data(); req(input$yvar)
    if (all(c("MONTH_NUMBER", "ENROLLMENT", "STUDY", "SUBGROUP") %in% names(data))) {
      p <- ggplot(data, aes(x = MONTH_NUMBER, y = ENROLLMENT, color = STUDY, group = interaction(STUDY, SUBGROUP))) + geom_line(linewidth = 1) + geom_point(size = 2.2) + scale_x_continuous(breaks = 1:12, labels = month.abb) + labs(x = "Month", y = "Enrollment", color = "Study")
    } else if ("year" %in% names(data) && input$yvar %in% names(data)) {
      grouping_var <- if ("country" %in% names(data)) "country" else names(data)[1]
      p <- ggplot(data, aes(x = year, y = .data[[input$yvar]], color = .data[[grouping_var]], group = .data[[grouping_var]])) + geom_line(linewidth = 1) + geom_point(size = 2.2) + labs(x = "Year", y = input$yvar, color = grouping_var)
    } else {
      if (!input$yvar %in% names(data)) return(NULL)
      data$row_index_temp <- seq_len(nrow(data)); bucket_size <- max(1, floor(nrow(data) / 12)); data$bucket_temp <- ceiling(data$row_index_temp / bucket_size)
      trend_data <- aggregate(data[[input$yvar]], by = list(bucket = data$bucket_temp), FUN = mean, na.rm = TRUE); names(trend_data)[2] <- "value"
      p <- ggplot(trend_data, aes(x = bucket, y = value)) + geom_line(color = "#0f6bca", linewidth = 1) + geom_point(color = "#0f6bca", size = 2.2) + labs(x = "Sequence", y = paste("Average", input$yvar))
    }
    compact_plotly(p)
  })

  output$table <- renderDT({
    DT::datatable(
      selected_data(), filter = "top", extensions = "Buttons", rownames = FALSE,
      options = list(
        dom = "Bfrtip", buttons = c("copy", "csv", "excel"), pageLength = 5,
        lengthChange = FALSE, scrollX = TRUE, scrollY = "180px", scrollCollapse = TRUE,
        autoWidth = TRUE, pagingType = "simple",
        columnDefs = list(list(targets = "_all", className = "dt-nowrap"))
      ),
      class = "compact stripe hover nowrap", width = "100%"
    )
  })

  output$journey <- renderUI({
    div(class = "journey-grid", lapply(seq_len(nrow(level_meta)), function(i) {
      row <- level_meta[i, ]; current_class <- if (row$Level == current_level()) "journey-step current" else "journey-step"
      status_icon <- if (row$Status == "Completed") "✓" else if (row$Status == "Current") "●" else "○"
      div(class = current_class, div(class = "step-status", status_icon), div(class = "step-title", row$Level), div(row$Title), div(class = "step-subtitle", row$Dataset))
    }))
  })
}

shinyApp(ui = ui, server = server)
