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

      # Scenario Comparison
      h4("Scenario Comparison"),
      textInput("scenario_name", "Scenario Name:", value = ""),
      actionButton("save_scenario", "Save Scenario for Comparison", class = "btn-success"),
      br(),
      br(),
      actionButton("clear_scenarios", "Clear All Scenarios", class = "btn-warning"),
      br(),
      br(),
      downloadButton("download_results", "Download Current Results"),
      downloadButton("download_comparison", "Download Comparison")
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

        tabPanel("Compare Scenarios",
                 br(),
                 h4("Saved Scenarios"),
                 verbatimTextOutput("scenarios_list"),
                 br(),
                 h4("Comparison Plots"),
                 plotlyOutput("compare_ypr", height = "400px"),
                 plotlyOutput("compare_spr", height = "400px"),
                 plotlyOutput("compare_prop", height = "400px"),
                 br(),
                 h4("Summary Table"),
                 tableOutput("compare_table")
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
  saved_scenarios <- reactiveVal(data.frame())
  detailed_results <- reactiveVal(data.frame())

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

    p <- ggplot(results, aes(x = "", y = YPR)) +
      geom_violin(fill = "steelblue", alpha = 0.7, color = "black") +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = "Yield Per Recruit Distribution",
           x = "", y = "YPR (kg)") +
      theme_minimal() +
      theme(axis.text.x = element_blank())

    ggplotly(p)
  })

  # SPR distribution plot
  output$spr_plot <- renderPlotly({
    req(sim_results())
    results <- sim_results()

    p <- ggplot(results, aes(x = "", y = SPR)) +
      geom_violin(fill = "darkgreen", alpha = 0.7, color = "black") +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = "Spawning Potential Ratio Distribution",
           x = "", y = "SPR") +
      theme_minimal() +
      theme(axis.text.x = element_blank())

    ggplotly(p)
  })

  # Proportion memorable plot
  output$prop_plot <- renderPlotly({
    req(sim_results())
    results <- sim_results()

    p <- ggplot(results, aes(x = "", y = Prop)) +
      geom_violin(fill = "orange", alpha = 0.7, color = "black") +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = "Proportion of Memorable-Sized Fish (≥12 inches)",
           x = "", y = "Proportion") +
      theme_minimal() +
      theme(axis.text.x = element_blank())

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

  # Save scenario for comparison
  observeEvent(input$save_scenario, {
    req(sim_results())
    results <- sim_results()

    # Create scenario name
    scenario_name <- if(input$scenario_name != "") {
      input$scenario_name
    } else {
      paste0("Scenario_", nrow(saved_scenarios()) + 1)
    }

    # Create summary row for this scenario
    new_scenario <- data.frame(
      Scenario = scenario_name,
      Exploitation = input$exploitation,
      Linf = input$linf,
      K = input$vbk,
      t0 = input$t0,
      MLL_mm = input$harvlim,
      MLL_inches = round(input$harvlim / 25.4, 1),
      YPR_mean = mean(results$YPR, na.rm = TRUE),
      YPR_sd = sd(results$YPR, na.rm = TRUE),
      SPR_mean = mean(results$SPR, na.rm = TRUE),
      SPR_sd = sd(results$SPR, na.rm = TRUE),
      Prop_mean = mean(results$Prop, na.rm = TRUE),
      Prop_sd = sd(results$Prop, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    # Add full results for violin plots
    results$Scenario <- scenario_name

    # Update saved scenarios
    current_scenarios <- saved_scenarios()
    if(nrow(current_scenarios) == 0) {
      current_scenarios <- new_scenario
    } else {
      current_scenarios <- rbind(current_scenarios, new_scenario)
    }
    saved_scenarios(current_scenarios)

    # Store detailed results for plotting
    current_detailed <- detailed_results()
    if(nrow(current_detailed) == 0) {
      detailed_results(results)
    } else {
      detailed_results(rbind(current_detailed, results))
    }

    showNotification(paste("Saved:", scenario_name), type = "message")
  })

  # Clear all scenarios
  observeEvent(input$clear_scenarios, {
    saved_scenarios(data.frame())
    detailed_results(data.frame())
    showNotification("All scenarios cleared", type = "warning")
  })

  # Display saved scenarios
  output$scenarios_list <- renderPrint({
    scenarios <- saved_scenarios()
    if(nrow(scenarios) == 0) {
      cat("No scenarios saved yet.\n")
      cat("Run a simulation and click 'Save Scenario for Comparison'")
    } else {
      cat(sprintf("Total Scenarios: %d\n\n", nrow(scenarios)))
      for(i in 1:nrow(scenarios)) {
        cat(sprintf("%d. %s\n", i, scenarios$Scenario[i]))
        cat(sprintf("   U=%.2f%%, MLL=%.1f\", L∞=%.0f, K=%.3f\n",
                    scenarios$Exploitation[i] * 100,
                    scenarios$MLL_inches[i],
                    scenarios$Linf[i],
                    scenarios$K[i]))
        cat(sprintf("   YPR=%.4f, SPR=%.4f, Prop=%.4f\n\n",
                    scenarios$YPR_mean[i],
                    scenarios$SPR_mean[i],
                    scenarios$Prop_mean[i]))
      }
    }
  })

  # Comparison plots
  output$compare_ypr <- renderPlotly({
    details <- detailed_results()
    req(nrow(details) > 0)

    p <- ggplot(details, aes(x = Scenario, y = YPR, fill = Scenario)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = "YPR Comparison Across Scenarios",
           x = "Scenario", y = "YPR (kg)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

    ggplotly(p)
  })

  output$compare_spr <- renderPlotly({
    details <- detailed_results()
    req(nrow(details) > 0)

    p <- ggplot(details, aes(x = Scenario, y = SPR, fill = Scenario)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = "SPR Comparison Across Scenarios",
           x = "Scenario", y = "SPR") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

    ggplotly(p)
  })

  output$compare_prop <- renderPlotly({
    details <- detailed_results()
    req(nrow(details) > 0)

    p <- ggplot(details, aes(x = Scenario, y = Prop, fill = Scenario)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = "Proportion Memorable Fish Comparison",
           x = "Scenario", y = "Proportion") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

    ggplotly(p)
  })

  # Comparison table
  output$compare_table <- renderTable({
    scenarios <- saved_scenarios()
    req(nrow(scenarios) > 0)

    scenarios %>%
      select(Scenario, Exploitation, MLL_inches, Linf, K,
             YPR_mean, SPR_mean, Prop_mean) %>%
      rename(
        `U (%)` = Exploitation,
        `MLL (in)` = MLL_inches,
        `L∞` = Linf,
        `YPR` = YPR_mean,
        `SPR` = SPR_mean,
        `Prop Memorable` = Prop_mean
      ) %>%
      mutate(`U (%)` = round(`U (%)` * 100, 1),
             YPR = round(YPR, 4),
             SPR = round(SPR, 4),
             `Prop Memorable` = round(`Prop Memorable`, 4))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

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

  # Download comparison
  output$download_comparison <- downloadHandler(
    filename = function() {
      paste0("crappie_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      scenarios <- saved_scenarios()
      req(nrow(scenarios) > 0)
      write.csv(scenarios, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
