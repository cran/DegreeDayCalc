############################################################
# GENERAL DEGREE-DAY PHENOLOGY APP
# Units: °C
# File: inst/app/app.R
############################################################

############################################################
# UI
############################################################
ui <- shiny::fluidPage(
  shiny::titlePanel("Degree-Day Phenology Calculator"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("file", "Upload daily temperature data (CSV)", accept = ".csv"),
      shiny::tags$small("Required: Tmin, Tmax | Optional: Date"),
      
      shiny::hr(),
      
      shiny::selectInput(
        "method", "Degree-day method",
        choices = c(
          "Average" = "average",
          "Average (cut)" = "average_cut",
          "Triangular" = "triangle",
          "Triangular + Tupper" = "triangle_upper",
          "Sine" = "sine",
          "Sine + Tupper" = "sine_upper"
        ),
        selected = "sine"
      ),
      
      shiny::numericInput("tbase", "Base temperature (Tb, °C)", 7.0, 0.1),
      shiny::checkboxInput("use_upper", "Use upper threshold (Tupper)", FALSE),
      shiny::numericInput("tupper", "Upper temperature (Tupper, °C)", 30, 0.5),
      
      shiny::hr(),
      
      shiny::h4("Developmental stage thresholds (cumulative GD)"),
      shiny::tags$small("Defaults are illustrative. Adjust for your species."),
      
      shiny::radioButtons(
        "stage_type",
        "Stage naming",
        choices = c("Larvae" = "larvae", "Nymph" = "nymph"),
        selected = "larvae",
        inline = TRUE
      ),
      
      shiny::uiOutput("stage_threshold_inputs"),
      
      shiny::hr(),
      shiny::downloadButton("download_csv", "Download results (CSV)"),
      shiny::downloadButton("download_png", "Download figure (PNG)")
    ),
    
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Table", DT::DTOutput("table")),
        shiny::tabPanel("Figure", shiny::plotOutput("plot", height = 480))
      )
    )
  )
)

############################################################
# SERVER
############################################################
server <- function(input, output, session) {
  
  # ---- Dynamic stage inputs ----
  output$stage_threshold_inputs <- shiny::renderUI({
    st <- if (identical(input$stage_type, "nymph")) "Nymph" else "Larva"
    
    shiny::tagList(
      shiny::numericInput("dd_egg", "Egg", 40),
      
      shiny::numericInput("dd_l1", paste0(st, " 1"), 80),
      shiny::numericInput("dd_l2", paste0(st, " 2"), 120),
      shiny::numericInput("dd_l3", paste0(st, " 3"), 160),
      shiny::numericInput("dd_l4", paste0(st, " 4"), 200),
      
      shiny::checkboxInput("use_l5", paste0("Include ", st, " 5"), FALSE),
      if (isTRUE(input$use_l5)) shiny::numericInput("dd_l5", paste0(st, " 5"), 240),
      
      shiny::checkboxInput("use_l6", paste0("Include ", st, " 6"), FALSE),
      if (isTRUE(input$use_l6)) shiny::numericInput("dd_l6", paste0(st, " 6"), 280),
      
      shiny::checkboxInput("use_pupa", "Include Pupa stage", TRUE),
      if (isTRUE(input$use_pupa)) shiny::numericInput("dd_pupa", "Pupa", 320),
      
      shiny::checkboxInput("use_adult", "Include Adult threshold", TRUE),
      if (isTRUE(input$use_adult)) shiny::numericInput("dd_adult", "Adult", 360),
      
      shiny::checkboxInput("use_preov", "Include preoviposition threshold", FALSE),
      if (isTRUE(input$use_preov)) shiny::numericInput("dd_preov", "Preoviposition", 400)
    )
  })
  
  # ---- Thresholds ----
  thresholds <- shiny::reactive({
    shiny::req(input$dd_egg, input$dd_l1, input$dd_l2, input$dd_l3, input$dd_l4)
    
    st <- if (identical(input$stage_type, "nymph")) "Nymph" else "Larva"
    
    th <- c(
      "Egg" = input$dd_egg,
      stats::setNames(input$dd_l1, paste0(st, " 1")),
      stats::setNames(input$dd_l2, paste0(st, " 2")),
      stats::setNames(input$dd_l3, paste0(st, " 3")),
      stats::setNames(input$dd_l4, paste0(st, " 4"))
    )
    
    if (isTRUE(input$use_l5)) {
      shiny::req(input$dd_l5)
      th <- c(th, stats::setNames(input$dd_l5, paste0(st, " 5")))
    }
    
    if (isTRUE(input$use_l6)) {
      shiny::req(input$dd_l6)
      th <- c(th, stats::setNames(input$dd_l6, paste0(st, " 6")))
    }
    
    if (isTRUE(input$use_pupa)) {
      shiny::req(input$dd_pupa)
      th <- c(th, "Pupa" = input$dd_pupa)
    }
    
    if (isTRUE(input$use_adult)) {
      shiny::req(input$dd_adult)
      th <- c(th, "Adult" = input$dd_adult)
    }
    
    if (isTRUE(input$use_preov)) {
      shiny::req(input$dd_preov)
      th <- c(th, "Preoviposition" = input$dd_preov)
    }
    
    if (any(diff(as.numeric(th)) <= 0)) {
      stop("Stage thresholds must be strictly increasing", call. = FALSE)
    }
    
    th
  })
  
  # ---- Stage assignment ----
  assign_stage <- function(GD_cum, th) {
    sapply(GD_cum, function(x) {
      i <- which(x < th)[1]
      if (is.na(i)) {
        if ("Preoviposition" %in% names(th)) "Adult (reproductive)" else "Adult"
      } else {
        names(th)[i]
      }
    })
  }
  
  # ---- Data processing ----
  data_proc <- shiny::reactive({
    if (is.null(input$file)) {
      set.seed(1)
      n <- 30
      df <- data.frame(
        Date = seq.Date(Sys.Date() - n + 1, Sys.Date(), by = "day"),
        Tmin = round(stats::rnorm(n, 12, 2), 1),
        Tmax = round(stats::rnorm(n, 28, 3), 1)
      )
      df$Tmax <- pmax(df$Tmax, df$Tmin + 1.5)
    } else {
      df <- utils::read.csv(input$file$datapath, stringsAsFactors = FALSE)
      df$Tmin <- suppressWarnings(as.numeric(df$Tmin))
      df$Tmax <- suppressWarnings(as.numeric(df$Tmax))
      if ("Date" %in% names(df)) {
        df$Date <- tryCatch(as.Date(df$Date), error = function(e) df$Date)
      }
    }
    
    GD <- mapply(
      degree_days,
      df$Tmin, df$Tmax,
      MoreArgs = list(
        Tbase = input$tbase,
        Tupper = if (isTRUE(input$use_upper)) input$tupper else NULL,
        method = input$method
      )
    )
    
    df$GD <- as.numeric(GD)
    df$GD_cum <- cumsum(ifelse(is.na(df$GD), 0, df$GD))
    
    th <- thresholds()
    df$Stage <- assign_stage(df$GD_cum, th)
    
    df
  })
  
  # ---- Stage labels for plot ----
  stage_labels_df <- shiny::reactive({
    df <- data_proc()
    if (!("Date" %in% names(df))) df$Date <- seq_len(nrow(df))
    
    r <- rle(as.character(df$Stage))
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    mids <- floor((starts + ends) / 2)
    
    data.frame(
      Date = df$Date[mids],
      GD_cum = df$GD_cum[mids],
      Stage = r$values,
      stringsAsFactors = FALSE
    )
  })
  
  # ---- Table ----
  output$table <- DT::renderDT({
    DT::datatable(
      data_proc(),
      options = list(
        pageLength = 20,
        lengthMenu = list(
          c(10, 20, 50, 100, 200, -1),
          c("10", "20", "50", "100", "200", "All")
        ),
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ---- Plot ----
  output$plot <- shiny::renderPlot({
    df <- data_proc()
    labs_df <- stage_labels_df()
    
    ggplot2::ggplot(df, ggplot2::aes(x = Date, y = GD_cum)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_text(
        data = labs_df,
        ggplot2::aes(x = Date, y = GD_cum, label = Stage),
        vjust = -0.6,
        size = 3.6,
        inherit.aes = FALSE
      ) +
      ggplot2::labs(
        x = if ("Date" %in% names(df)) "Date" else "Index",
        y = "Cumulative degree-days",
        title = "Thermal phenology based on degree-days",
        subtitle = paste(
          "Method:", input$method,
          "| Tb =", input$tbase,
          if (isTRUE(input$use_upper)) paste("| Tupper =", input$tupper) else ""
        )
      ) +
      ggplot2::theme_minimal(base_size = 13)
  })
  
  # ---- Downloads ----
  output$download_csv <- shiny::downloadHandler(
    filename = function() "degree_day_results.csv",
    content = function(file) {
      utils::write.csv(data_proc(), file, row.names = FALSE)
    }
  )
  
  output$download_png <- shiny::downloadHandler(
    filename = function() "degree_day_phenology.png",
    content = function(file) {
      grDevices::png(file, width = 1800, height = 1200, res = 300)
      df <- data_proc()
      labs_df <- stage_labels_df()
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = GD_cum)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_text(
          data = labs_df,
          ggplot2::aes(x = Date, y = GD_cum, label = Stage),
          vjust = -0.6,
          size = 3.6,
          inherit.aes = FALSE
        ) +
        ggplot2::labs(
          x = if ("Date" %in% names(df)) "Date" else "Index",
          y = "Cumulative degree-days",
          title = "Thermal phenology based on degree-days",
          subtitle = paste(
            "Method:", input$method,
            "| Tb =", input$tbase,
            if (isTRUE(input$use_upper)) paste("| Tupper =", input$tupper) else ""
          )
        ) +
        ggplot2::theme_minimal(base_size = 13)
      
      print(p)
      grDevices::dev.off()
    }
  )
}

############################################################
# Run app
############################################################
shiny::shinyApp(ui, server)
