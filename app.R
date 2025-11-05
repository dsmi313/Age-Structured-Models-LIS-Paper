library(shiny)
library(tidyverse)
library(plotly)

# UI Definition
ui <- fluidPage(
  titlePanel("Crappie Age-Structured Population Model: YPR Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Model Parameters"),

      # Growth Parameters
      h4("Growth Parameters (von Bertalanffy)"),
      selectInput("growth_preset", "Load Preset:",
                  choices = c("Custom" = "custom", "Slow" = "slow", "Moderate" = "moderate", "Fast" = "fast"),
                  selected = "moderate"),
      numericInput("linf", "L∞ (mm):", value = 353, min = 250, max = 450, step = 1),
      numericInput("vbk", "K:", value = 0.374, min = 0.01, max = 1.5, step = 0.001),
      numericInput("t0", "t0:", value = 0.197, min = -1.0, max = 2.0, step = 0.001),

      # Exploitation Parameters
      h4("Exploitation Parameters"),
      sliderInput("exploitation", "Exploitation Rate (U):",
                  min = 0.0, max = 1.0, value = 0.34, step = 0.01),

      # Vulnerability Parameters
      h4("Vulnerability & Selectivity"),
      numericInput("capsize", "Length at 50% Capture (mm):",
                   value = 204, min = 100, max = 300),
      numericInput("harvlim", "Minimum Harvest Size (mm):",
                   value = 254, min = 150, max = 350),

      # Mortality Parameters
      h4("Mortality"),
      numericInput("dismort", "Discard Mortality Rate:",
                   value = 0.09, min = 0.0, max = 1.0, step = 0.01),

      # Simulation Parameters
      h4("Simulation Settings"),
      numericInput("nsim", "Number of Simulations:",
                   value = 1000, min = 100, max = 10000, step = 100),
      numericInput("ymax", "Years to Simulate:",
                   value = 100, min = 50, max = 200),

      actionButton("run_sim", "Run Simulation", class = "btn-primary"),
      br(),
      br(),
      downloadButton("download_results", "Download Results")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results Summary",
                 br(),
                 h4("Simulation Results"),
                 verbatimTextOutput("summary_stats"),
                 br(),
                 plotlyOutput("ypr_plot", height = "300px"),
                 plotlyOutput("spr_plot", height = "300px"),
                 plotlyOutput("prop_plot", height = "300px")
        ),

        tabPanel("Time Series",
                 br(),
                 plotlyOutput("timeseries_plot", height = "600px")
        ),

        tabPanel("Population Structure",
                 br(),
                 plotlyOutput("pop_structure", height = "500px"),
                 plotlyOutput("vulnerability_plot", height = "400px")
        ),

        tabPanel("About",
                 br(),
                 h3("Crappie Age-Structured Model"),
                 p("This Shiny app implements the age-structured population model from:"),
                 p(em("Live-imaging sonar use in Texas crappie fisheries: Assessing population-level
                      responses due to potential increases in exploitation.")),
                 br(),
                 h4("Model Description"),
                 p("This model simulates a crappie population using age-structured dynamics with:"),
                 tags$ul(
                   tags$li("Age classes 1-8 years"),
                   tags$li("Von Bertalanffy growth"),
                   tags$li("Size-dependent vulnerability to capture and harvest"),
                   tags$li("Discard and harvest mortality"),
                   tags$li("Stochastic recruitment (lognormal, CV=0.8)")
                 ),
                 br(),
                 h4("Outputs"),
                 tags$ul(
                   tags$li(strong("YPR:"), "Yield Per Recruit (kg)"),
                   tags$li(strong("SPR:"), "Spawning Potential Ratio (relative to unfished)"),
                   tags$li(strong("Prop Memorable:"), "Proportion of fish ≥12 inches")
                 ),
                 br(),
                 h4("References"),
                 p("Similar models used by Dotson et al. (2009)"),
                 p(a(href = "https://doi.org/10.1577/M08-137.1",
                     "https://doi.org/10.1577/M08-137.1"))
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values to store simulation results
  sim_results <- reactiveVal(NULL)
  time_series_data <- reactiveVal(NULL)
  pop_structure_data <- reactiveVal(NULL)

  # Observer to update growth parameters when preset is selected
  observeEvent(input$growth_preset, {
    if (input$growth_preset == "slow") {
      updateNumericInput(session, "linf", value = 333)
      updateNumericInput(session, "vbk", value = 0.325)
      updateNumericInput(session, "t0", value = 0.174)
    } else if (input$growth_preset == "moderate") {
      updateNumericInput(session, "linf", value = 353)
      updateNumericInput(session, "vbk", value = 0.374)
      updateNumericInput(session, "t0", value = 0.197)
    } else if (input$growth_preset == "fast") {
      updateNumericInput(session, "linf", value = 356)
      updateNumericInput(session, "vbk", value = 0.691)
      updateNumericInput(session, "t0", value = -0.056)
    }
    # If "custom" is selected, don't update anything - user will enter their own values
  })

  # Get growth parameters from inputs
  get_growth_params <- reactive({
    list(Linf = input$linf, vbk = input$vbk, t0 = input$t0)
  })

  # Run simulation when button is clicked
  observeEvent(input$run_sim, {

    # Show progress
    withProgress(message = 'Running simulation...', value = 0, {

      # Get parameters
      growth_params <- get_growth_params()
      Amax <- 8
      Ymax <- input$ymax

      # Weight-length equation
      alfa <- 2.40991e-6
      bet <- 3.38

      # Mortality
      DisMort <- input$dismort

      # Stock-recruit
      Ro <- 10000

      # Vulnerabilities
      Capsize <- input$capsize
      CapsizeSD <- Capsize * 0.01
      Uppercap <- 380
      UppercapSD <- Uppercap * 0.01
      Harvlim <- input$harvlim
      HarvlimSD <- Harvlim * 0.01

      Age <- seq(1, Amax)

      # Run simulations
      nsim <- input$nsim
      results <- data.frame(
        sim = 1:nsim,
        YPR = rep(NA, nsim),
        SPR = rep(NA, nsim),
        Prop = rep(NA, nsim)
      )

      # Store one representative time series
      store_timeseries <- TRUE

      for(k in 1:nsim) {

        incProgress(1/nsim, detail = paste("Simulation", k, "of", nsim))

        N <- matrix(NA, Ymax, Amax)
        Wmat <- (alfa * rnorm(1, 200, 20)^bet) / 1000
        Yield <- rep(NA, Ymax)
        SPRt <- rep(NA, Ymax)
        YPR <- rep(NA, Ymax)
        Prop <- rep(NA, Ymax)

        S <- exp(-growth_params$vbk)^(Age - 1)
        So <- exp(-growth_params$vbk)

        N[1, 1] <- 10000
        N[1, ] <- Ro * S

        Rcapacity <- Ro * rlnorm(Ymax, 0, sd = 0.8)

        U <- input$exploitation
        Uo <- input$exploitation + 0.1

        TL <- growth_params$Linf * (1 - exp(-growth_params$vbk * (Age - growth_params$t0)))
        Wt <- (alfa * TL^bet) / 1000
        Fec <- pmax(Wt - Wmat, 0)

        Vulcap <- 1 / (1 + exp(-(TL - Capsize) / CapsizeSD))
        Vulharv <- 1 / (1 + exp(-(TL - Harvlim) / HarvlimSD))

        for(i in 2:Ymax) {
          N[i, 1] <- Rcapacity[i - 1]
          for(j in 2:Amax) {
            trophyvul <- (1 / (1 + exp(-(TL - 305) / (305 * 0.1)))) * Vulcap[j]

            N[i, j] <- N[i-1, j-1] * So *
              (1 - (Vulcap[j-1] * Uo - Vulharv[j-1] * U) * DisMort) *
              (1 - Vulharv[j-1] * U)

            Yield[i] <- sum(Wt * Vulharv * N[i, ]) * U
            SPRt[i] <- (sum(N[i, ] * Fec)) / (sum(N[1, ] * Fec))
            YPR[i] <- (sum(Wt * Vulharv * N[i, ]) * U) / N[i, 1]
            Prop[i] <- sum(trophyvul * N[i, ]) / sum(N[i, ])
          }
        }

        # Store results (last 50 years)
        SPRout <- SPRt[50:Ymax]
        results$SPR[k] <- mean(SPRout, na.rm = TRUE)

        YPRout <- YPR[50:Ymax]
        results$YPR[k] <- mean(YPRout, na.rm = TRUE)

        Propout <- Prop[50:Ymax]
        results$Prop[k] <- mean(Propout, na.rm = TRUE)

        # Store one representative time series
        if(store_timeseries && k == 1) {
          ts_data <- data.frame(
            Year = 1:Ymax,
            YPR = YPR,
            SPR = SPRt,
            Prop = Prop,
            TotalN = rowSums(N, na.rm = TRUE)
          )
          time_series_data(ts_data)

          # Store population structure
          pop_data <- data.frame(
            Age = Age,
            Length = TL,
            Weight = Wt,
            Abundance = N[Ymax, ],
            VulCapture = Vulcap,
            VulHarvest = Vulharv
          )
          pop_structure_data(pop_data)

          store_timeseries <- FALSE
        }
      }

      sim_results(results)
    })
  })

  # Summary statistics output
  output$summary_stats <- renderPrint({
    req(sim_results())
    results <- sim_results()

    cat("SIMULATION SUMMARY\n")
    cat("==================\n\n")
    cat("Model Parameters:\n")
    cat(sprintf("  Exploitation Rate (U): %.2f%%\n", input$exploitation * 100))
    cat(sprintf("  L∞: %.1f mm\n", input$linf))
    cat(sprintf("  K: %.3f\n", input$vbk))
    cat(sprintf("  t0: %.3f\n", input$t0))
    cat(sprintf("  Number of Simulations: %d\n\n", input$nsim))

    cat("Results (Mean ± SD):\n")
    cat(sprintf("  YPR:              %.4f ± %.4f kg\n",
                mean(results$YPR, na.rm = TRUE),
                sd(results$YPR, na.rm = TRUE)))
    cat(sprintf("  SPR:              %.4f ± %.4f\n",
                mean(results$SPR, na.rm = TRUE),
                sd(results$SPR, na.rm = TRUE)))
    cat(sprintf("  Prop Memorable:   %.4f ± %.4f\n",
                mean(results$Prop, na.rm = TRUE),
                sd(results$Prop, na.rm = TRUE)))
  })

  # YPR distribution plot
  output$ypr_plot <- renderPlotly({
    req(sim_results())
    results <- sim_results()

    p <- ggplot(results, aes(x = YPR)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "black") +
      geom_vline(aes(xintercept = mean(YPR, na.rm = TRUE)),
                 color = "red", linetype = "dashed", size = 1) +
      labs(title = "Yield Per Recruit Distribution",
           x = "YPR (kg)", y = "Frequency") +
      theme_minimal()

    ggplotly(p)
  })

  # SPR distribution plot
  output$spr_plot <- renderPlotly({
    req(sim_results())
    results <- sim_results()

    p <- ggplot(results, aes(x = SPR)) +
      geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7, color = "black") +
      geom_vline(aes(xintercept = mean(SPR, na.rm = TRUE)),
                 color = "red", linetype = "dashed", size = 1) +
      labs(title = "Spawning Potential Ratio Distribution",
           x = "SPR", y = "Frequency") +
      theme_minimal()

    ggplotly(p)
  })

  # Proportion memorable plot
  output$prop_plot <- renderPlotly({
    req(sim_results())
    results <- sim_results()

    p <- ggplot(results, aes(x = Prop)) +
      geom_histogram(bins = 30, fill = "orange", alpha = 0.7, color = "black") +
      geom_vline(aes(xintercept = mean(Prop, na.rm = TRUE)),
                 color = "red", linetype = "dashed", size = 1) +
      labs(title = "Proportion of Memorable-Sized Fish (≥12 inches)",
           x = "Proportion", y = "Frequency") +
      theme_minimal()

    ggplotly(p)
  })

  # Time series plot
  output$timeseries_plot <- renderPlotly({
    req(time_series_data())
    ts_data <- time_series_data()

    ts_long <- ts_data %>%
      select(Year, YPR, SPR, Prop) %>%
      pivot_longer(-Year, names_to = "Metric", values_to = "Value")

    p <- ggplot(ts_long, aes(x = Year, y = Value, color = Metric)) +
      geom_line(size = 0.8) +
      facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
      labs(title = "Population Metrics Over Time (Representative Simulation)",
           x = "Year", y = "Value") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Population structure plot
  output$pop_structure <- renderPlotly({
    req(pop_structure_data())
    pop_data <- pop_structure_data()

    p <- ggplot(pop_data, aes(x = Age)) +
      geom_col(aes(y = Abundance), fill = "steelblue", alpha = 0.7) +
      geom_line(aes(y = Abundance), color = "darkblue", size = 1) +
      geom_point(aes(y = Abundance), color = "darkblue", size = 3) +
      labs(title = "Population Structure by Age (Final Year)",
           x = "Age", y = "Abundance") +
      theme_minimal()

    ggplotly(p)
  })

  # Vulnerability plot
  output$vulnerability_plot <- renderPlotly({
    req(pop_structure_data())
    pop_data <- pop_structure_data()

    vul_long <- pop_data %>%
      select(Length, VulCapture, VulHarvest) %>%
      pivot_longer(-Length, names_to = "Type", values_to = "Vulnerability")

    p <- ggplot(vul_long, aes(x = Length, y = Vulnerability, color = Type)) +
      geom_line(size = 1.2) +
      labs(title = "Vulnerability Curves by Length",
           x = "Total Length (mm)", y = "Vulnerability",
           color = "Type") +
      scale_color_manual(values = c("VulCapture" = "blue", "VulHarvest" = "red"),
                         labels = c("Capture", "Harvest")) +
      theme_minimal()

    ggplotly(p)
  })

  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("crappie_simulation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(sim_results())
      write.csv(sim_results(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
