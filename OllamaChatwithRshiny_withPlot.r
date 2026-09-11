library(shiny)
library(httr2)
library(jsonlite)
library(evaluate)   # install from your internal Artifactory R repo, not CRAN directly

# ---- Helpers ----------------------------------------------------------

# Pull out ```r ... ``` (or ```R ... ```) fenced code blocks from AI text
extract_r_blocks <- function(text) {
  pattern <- "```\\{?[rR]\\}?\\s*\\n([\\s\\S]*?)```"
  m <- gregexpr(pattern, text, perl = TRUE)
  blocks <- regmatches(text, m)[[1]]
  if (length(blocks) == 0) return(character(0))
  vapply(blocks, function(b) sub(pattern, "\\1", b, perl = TRUE), character(1), USE.NAMES = FALSE)
}

# Run code with evaluate(), turning the result into text + base64 PNGs
run_r_code <- function(code, envir) {
  ev <- tryCatch(
    evaluate::evaluate(code, envir = envir, stop_on_error = 0L),
    error = function(e) list(paste("Fatal error:", conditionMessage(e)))
  )

  text_output <- c()
  plot_images <- list()

  for (item in ev) {
    if (inherits(item, "source")) {
      next  # skip echoed input lines, we show the code separately
    } else if (is.character(item)) {
      text_output <- c(text_output, item)
    } else if (inherits(item, "recordedplot")) {
      tmp <- tempfile(fileext = ".png")
      grDevices::png(tmp, width = 700, height = 450, res = 100)
      tryCatch(grDevices::replayPlot(item), error = function(e) NULL)
      grDevices::dev.off()
      if (file.exists(tmp) && file.info(tmp)$size > 0) {
        raw_bytes <- readBin(tmp, "raw", file.info(tmp)$size)
        plot_images[[length(plot_images) + 1]] <- jsonlite::base64_enc(raw_bytes)
      }
      unlink(tmp)
    } else if (inherits(item, "simpleError")) {
      text_output <- c(text_output, paste("Error:", conditionMessage(item)))
    } else if (inherits(item, "simpleWarning")) {
      text_output <- c(text_output, paste("Warning:", conditionMessage(item)))
    } else if (inherits(item, "message")) {
      text_output <- c(text_output, paste("Message:", conditionMessage(item)))
    }
  }

  list(text = paste(text_output, collapse = "\n"), plots = plot_images)
}

# ---- UI -----------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Ollama Chat with R Code Execution"),

  sidebarLayout(
    sidebarPanel(
      textInput("model_name", "Ollama Model Name:", value = "llama3.2"),
      textAreaInput(
        "user_prompt", "Enter your prompt:",
        value = "Write R code that plots a histogram of 1000 random normal values.",
        rows = 4
      ),
      actionButton("submit_btn", "Generate & Run", class = "btn-primary"),
      hr(),
      actionButton("reset_btn", "Reset R session", class = "btn-warning"),
      helpText("Resetting clears variables carried over between prompts.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Chat", uiOutput("conversation")),
        tabPanel("Plots", uiOutput("plots_tab"))
      )
    )
  )
)

# ---- Server ---------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    history = list(),     # list of list(prompt, response, exec_text, exec_plots)
    env = new.env()       # persists variables across turns until reset
  )

  observeEvent(input$reset_btn, {
    rv$env <- new.env()
    showNotification("R session reset.", type = "message")
  })

  observeEvent(input$submit_btn, {

    body_list <- list(
      model = input$model_name,
      prompt = input$user_prompt,
      stream = FALSE
    )

    ai_text <- tryCatch({
      req <- request("http://localhost:11434/api/generate") |>
        req_body_json(body_list) |>
        req_timeout(60)
      resp <- req_perform(req)
      resp_json <- resp_body_json(resp)
      if (is.null(resp_json$response)) {
        paste("Unexpected response:", jsonlite::toJSON(resp_json, auto_unbox = TRUE))
      } else {
        resp_json$response
      }
    }, httr2_http_404 = function(e) {
      "Model not found. Check `ollama list` and that the name matches exactly."
    }, error = function(e) {
      paste("Error connecting to Ollama:", e$message)
    })

    r_blocks <- extract_r_blocks(ai_text)

    exec_result <- if (length(r_blocks) > 0) {
      combined_code <- paste(r_blocks, collapse = "\n\n")
      run_r_code(combined_code, envir = rv$env)
    } else {
      list(text = NULL, plots = list())
    }

    rv$history[[length(rv$history) + 1]] <- list(
      prompt      = input$user_prompt,
      response    = ai_text,
      has_code    = length(r_blocks) > 0,
      exec_text   = exec_result$text,
      exec_plots  = exec_result$plots
    )
  })

  output$conversation <- renderUI({
    if (length(rv$history) == 0) {
      return(p("No messages yet."))
    }

    blocks <- lapply(rev(rv$history), function(turn) {

      exec_panel <- if (turn$has_code) {
        tagList(
          h5("Execution result:"),
          if (!is.null(turn$exec_text) && nzchar(turn$exec_text)) {
            tags$pre(turn$exec_text)
          },
          if (length(turn$exec_plots) > 0) {
            tags$p(tags$em(sprintf(
              "%d plot(s) generated \u2014 see the Plots tab.", length(turn$exec_plots)
            )))
          }
        )
      } else {
        NULL
      }

      tagList(
        tags$div(
          style = "border:1px solid #ccc; border-radius:6px; padding:12px; margin-bottom:16px;",
          tags$strong("Prompt: "), turn$prompt, tags$br(), tags$br(),
          tags$strong("AI response:"),
          tags$pre(turn$response),
          exec_panel
        )
      )
    })

    do.call(tagList, blocks)
  })

  output$plots_tab <- renderUI({
    # Flatten all plots across turns, newest first, each labeled with its prompt
    plot_blocks <- lapply(rev(rv$history), function(turn) {
      if (length(turn$exec_plots) == 0) return(NULL)

      imgs <- lapply(turn$exec_plots, function(b64) {
        tags$img(
          src = paste0("data:image/png;base64,", b64),
          style = "max-width:100%; border:1px solid #ddd; margin-bottom:8px; display:block;"
        )
      })

      tagList(
        tags$div(
          style = "margin-bottom:20px;",
          tags$strong("From prompt: "), tags$em(turn$prompt),
          imgs
        )
      )
    })

    plot_blocks <- Filter(Negate(is.null), plot_blocks)

    if (length(plot_blocks) == 0) {
      return(p("No plots generated yet."))
    }

    do.call(tagList, plot_blocks)
  })
}

shinyApp(ui = ui, server = server)