library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# ============================================================================
# SALMON POPULATION MODEL ENGINE
# ============================================================================
# This model is specifically designed for semelparous salmon populations.
# Key differences from YPR/SPR models:
# - Brood-year dynamics (not equilibrium age structure)
# - Age-at-return schedules (not continuous aging)
# - Terminal spawning (fish spawn once and die)
# - Escapement-at-age outputs (not YPR/SPR metrics)
# ============================================================================

# Stock-Recruitment Functions
# ============================================================================

#' Beverton-Holt stock-recruitment
#' @param S spawning stock biomass (or abundance)
#' @param alpha productivity parameter
#' @param beta density dependence parameter
beverton_holt <- function(S, alpha, beta) {
  (alpha * S) / (1 + beta * S)
}

#' Ricker stock-recruitment
#' @param S spawning stock biomass (or abundance)
#' @param alpha productivity at low density
#' @param beta density dependence parameter
ricker <- function(S, alpha, beta) {
  alpha * S * exp(-beta * S)
}

#' Parameterize Beverton-Holt from biological reference points
#' @param R0 unfished recruitment
#' @param S0 unfished spawners
#' @param h steepness (proportion of R0 produced at 0.2*S0)
bh_from_steepness <- function(R0, S0, h) {
  alpha <- (4 * h * R0) / (S0 * (1 - h))
  beta <- (5 * h - 1) / (S0 * (1 - h))
  list(alpha = alpha, beta = beta)
}

#' Parameterize Ricker from biological reference points
#' @param R0 unfished recruitment
#' @param S0 unfished spawners
#' @param productivity scalar for Ricker curve
ricker_from_productivity <- function(R0, S0, productivity = 2.5) {
  alpha <- productivity / S0
  beta <- log(productivity) / S0
  list(alpha = alpha, beta = beta)
}


# Salmon Life Cycle Model
# ============================================================================

#' Build age-at-return probability distribution
#' @param ages vector of return ages (e.g., 3:5)
#' @param probs vector of probabilities (must sum to 1)
#' @return data frame with age and probability
build_age_schedule <- function(ages, probs) {
  if (abs(sum(probs) - 1.0) > 0.001) {
    probs <- probs / sum(probs)
  }
  data.frame(
    age = ages,
    prob = probs
  )
}

#' Calculate ocean survival to age a
#' @param age return age
#' @param M_ocean annual ocean natural mortality
#' @param age_at_ocean_entry age when entering ocean (typically 1)
ocean_survival <- function(age, M_ocean, age_at_ocean_entry = 1) {
  years_in_ocean <- age - age_at_ocean_entry
  exp(-M_ocean * years_in_ocean)
}

#' Simulate salmon population with brood-year dynamics
#' @param params list of model parameters
#' @param nsim number of Monte Carlo simulations
#' @param progress_cb optional progress callback function
simulate_salmon_population <- function(params, nsim = 1000, progress_cb = NULL) {

  # Extract parameters
  n_brood_years <- params$n_brood_years
  return_ages <- params$return_ages
  age_probs <- params$age_probs
  M_ocean <- params$M_ocean
  F_rate <- params$F_rate
  SR_function <- params$SR_function
  SR_params <- params$SR_params
  S0 <- params$S0
  R0 <- params$R0
  rec_cv <- params$rec_cv
  harvest_rate <- params$harvest_rate

  # Build age-at-return schedule
  age_schedule <- build_age_schedule(return_ages, age_probs)
  n_ages <- length(return_ages)
  min_age <- min(return_ages)
  max_age <- max(return_ages)

  # Calculate derived quantities
  n_return_years <- n_brood_years + max_age
  sigmaR <- sqrt(log(rec_cv^2 + 1))

  # Initialize storage arrays
  all_spawners <- array(NA, dim = c(n_brood_years, nsim))
  all_recruits <- array(NA, dim = c(n_brood_years, nsim))
  all_escapement <- array(NA, dim = c(n_return_years, n_ages, nsim))
  all_harvest <- array(NA, dim = c(n_return_years, n_ages, nsim))
  all_returns <- array(NA, dim = c(n_return_years, n_ages, nsim))

  # Run simulations
  for (sim in 1:nsim) {
    if (!is.null(progress_cb)) {
      progress_cb(1 / nsim, detail = paste("Simulation", sim, "of", nsim))
    }

    # Storage for this simulation
    spawners <- numeric(n_brood_years)
    recruits <- numeric(n_brood_years)
    escapement <- matrix(0, nrow = n_return_years, ncol = n_ages)
    harvest <- matrix(0, nrow = n_return_years, ncol = n_ages)
    returns <- matrix(0, nrow = n_return_years, ncol = n_ages)

    # Initialize first brood year at unfished equilibrium
    spawners[1] <- S0

    # Apply stock-recruitment function
    if (SR_function == "Beverton-Holt") {
      R_mean <- beverton_holt(spawners[1], SR_params$alpha, SR_params$beta)
    } else {
      R_mean <- ricker(spawners[1], SR_params$alpha, SR_params$beta)
    }

    # Add recruitment variability
    if (rec_cv > 0) {
      recruits[1] <- R_mean * rlnorm(1, meanlog = 0, sdlog = sigmaR)
    } else {
      recruits[1] <- R_mean
    }

    # Forward simulation through brood years
    for (b in 1:n_brood_years) {

      # Calculate returns by age for this brood year
      for (a_idx in 1:n_ages) {
        age <- age_schedule$age[a_idx]
        return_year <- b + age

        if (return_year <= n_return_years) {
          # Number of fish returning at this age
          S_ocean <- ocean_survival(age, M_ocean)
          N_return <- recruits[b] * age_schedule$prob[a_idx] * S_ocean

          returns[return_year, a_idx] <- N_return

          # Apply fishing mortality (instantaneous)
          if (harvest_rate > 0) {
            # Convert harvest rate to F
            F_inst <- -log(1 - harvest_rate)
            H <- N_return * (1 - exp(-F_inst))
            E <- N_return * exp(-F_inst)
          } else {
            H <- 0
            E <- N_return
          }

          harvest[return_year, a_idx] <- H
          escapement[return_year, a_idx] <- E
        }
      }

      # Calculate spawners for next brood year
      if (b < n_brood_years) {
        # Spawners are the sum of escapement from all ages in year b + min_age
        # (earliest year that brood b+1 can be produced)
        spawn_year <- b + min_age
        if (spawn_year <= n_return_years) {
          spawners[b + 1] <- sum(escapement[spawn_year, ])
        } else {
          spawners[b + 1] <- S0  # fallback
        }

        # Apply stock-recruitment
        if (SR_function == "Beverton-Holt") {
          R_mean <- beverton_holt(spawners[b + 1], SR_params$alpha, SR_params$beta)
        } else {
          R_mean <- ricker(spawners[b + 1], SR_params$alpha, SR_params$beta)
        }

        # Add recruitment variability
        if (rec_cv > 0) {
          recruits[b + 1] <- R_mean * rlnorm(1, meanlog = 0, sdlog = sigmaR)
        } else {
          recruits[b + 1] <- R_mean
        }
      }
    }

    # Store results
    all_spawners[, sim] <- spawners
    all_recruits[, sim] <- recruits
    all_escapement[, , sim] <- escapement
    all_harvest[, , sim] <- harvest
    all_returns[, , sim] <- returns
  }

  # Calculate summary statistics
  spawners_mean <- rowMeans(all_spawners, na.rm = TRUE)
  recruits_mean <- rowMeans(all_recruits, na.rm = TRUE)

  # Escapement by year and age
  escapement_summary <- array(NA, dim = c(n_return_years, n_ages, 3))
  dimnames(escapement_summary) <- list(
    year = 1:n_return_years,
    age = return_ages,
    stat = c("mean", "lower", "upper")
  )

  for (yr in 1:n_return_years) {
    for (a_idx in 1:n_ages) {
      vals <- all_escapement[yr, a_idx, ]
      escapement_summary[yr, a_idx, "mean"] <- mean(vals, na.rm = TRUE)
      escapement_summary[yr, a_idx, "lower"] <- quantile(vals, 0.025, na.rm = TRUE)
      escapement_summary[yr, a_idx, "upper"] <- quantile(vals, 0.975, na.rm = TRUE)
    }
  }

  # Total escapement by year
  total_escapement <- matrix(NA, nrow = n_return_years, ncol = nsim)
  for (sim in 1:nsim) {
    total_escapement[, sim] <- rowSums(all_escapement[, , sim])
  }

  escapement_total_mean <- rowMeans(total_escapement, na.rm = TRUE)
  escapement_total_sd <- apply(total_escapement, 1, sd, na.rm = TRUE)

  # Spawner-recruit pairs
  SR_pairs <- data.frame(
    spawners = as.vector(all_spawners),
    recruits = as.vector(all_recruits)
  )

  # Return compiled results
  list(
    spawners_mean = spawners_mean,
    recruits_mean = recruits_mean,
    escapement_summary = escapement_summary,
    escapement_total_mean = escapement_total_mean,
    escapement_total_sd = escapement_total_sd,
    total_escapement = total_escapement,
    all_spawners = all_spawners,
    all_recruits = all_recruits,
    all_escapement = all_escapement,
    all_harvest = all_harvest,
    all_returns = all_returns,
    SR_pairs = SR_pairs,
    params = params,
    nsim = nsim
  )
}

#' Calculate equilibrium metrics for salmon population
#' @param params model parameters
calculate_equilibrium_metrics <- function(params) {
  SR_params <- params$SR_params
  S0 <- params$S0
  R0 <- params$R0
  return_ages <- params$return_ages
  age_probs <- params$age_probs
  M_ocean <- params$M_ocean
  harvest_rate <- params$harvest_rate

  # Calculate recruits per spawner at equilibrium
  if (harvest_rate > 0) {
    F_inst <- -log(1 - harvest_rate)
  } else {
    F_inst <- 0
  }

  # Calculate lifetime egg production (LEP)
  # For salmon, this is fecundity * probability of surviving to spawn
  LEP <- 0
  for (i in seq_along(return_ages)) {
    age <- return_ages[i]
    prob <- age_probs[i]
    S_ocean <- ocean_survival(age, M_ocean)
    S_fishing <- exp(-F_inst)
    LEP <- LEP + prob * S_ocean * S_fishing
  }

  # Spawner per recruit (unfished)
  SPR0 <- LEP

  # Spawner per recruit (fished)
  SPR_fished <- LEP

  # SPR ratio
  SPR_ratio <- SPR_fished / SPR0

  list(
    S0 = S0,
    R0 = R0,
    SPR0 = SPR0,
    SPR_fished = SPR_fished,
    SPR_ratio = SPR_ratio,
    LEP = LEP
  )
}


# ============================================================================
# SHINY USER INTERFACE
# ============================================================================

ui <- fluidPage(
  titlePanel("Salmon Population Model: Brood-Year Dynamics"),

  sidebarLayout(
    sidebarPanel(
      h3("Model Parameters"),
      width = 3,

      # Life History Parameters
      h4("Life History"),
      helpText("Salmon-specific biological parameters"),

      numericInput("M_ocean", "Ocean Natural Mortality (M):",
                   value = 0.15, min = 0.05, max = 0.5, step = 0.01),
      helpText(tags$small(tags$em("Annual mortality rate during ocean phase"))),

      # Age-at-return schedule
      h4("Age-at-Return Schedule"),
      helpText("Probability of returning to spawn at each age. Must sum to 1.0"),

      numericInput("prob_age3", "Age-3 Proportion:",
                   value = 0.10, min = 0, max = 1, step = 0.05),
      numericInput("prob_age4", "Age-4 Proportion:",
                   value = 0.60, min = 0, max = 1, step = 0.05),
      numericInput("prob_age5", "Age-5 Proportion:",
                   value = 0.30, min = 0, max = 1, step = 0.05),

      textOutput("age_schedule_check"),
      br(),

      # Harvest Parameters
      h4("Harvest Management"),
      sliderInput("harvest_rate", "Harvest Rate:",
                  min = 0.0, max = 0.9, value = 0.3, step = 0.05),
      helpText(tags$small(tags$em("Proportion of returning fish harvested before spawning"))),

      # Stock-Recruitment Parameters
      h4("Stock-Recruitment"),
      selectInput("SR_function", "Function:",
                  choices = c("Beverton-Holt", "Ricker"),
                  selected = "Beverton-Holt"),

      conditionalPanel(
        condition = "input.SR_function == 'Beverton-Holt'",
        sliderInput("steepness", "Steepness (h):",
                    min = 0.2, max = 1.0, value = 0.7, step = 0.05),
        helpText(tags$small(tags$em("Proportion of R0 at 20% of S0. Higher = stronger compensation")))
      ),

      conditionalPanel(
        condition = "input.SR_function == 'Ricker'",
        numericInput("ricker_productivity", "Productivity:",
                     value = 2.5, min = 1.0, max = 10.0, step = 0.1),
        helpText(tags$small(tags$em("Maximum recruits per spawner at low density")))
      ),

      numericInput("S0", "Unfished Spawners (S0):",
                   value = 50000, min = 1000, max = 1000000, step = 1000),
      numericInput("R0", "Unfished Recruits (R0):",
                   value = 50000, min = 1000, max = 1000000, step = 1000),

      # Recruitment Variability
      h4("Recruitment Variability"),
      numericInput("rec_cv", "Recruitment CV:",
                   value = 0.6, min = 0.0, max = 2.0, step = 0.1),
      helpText(tags$small(tags$em("Coefficient of variation for recruitment. 0 = deterministic"))),

      # Simulation Settings
      h4("Simulation Settings"),
      numericInput("n_brood_years", "Number of Brood Years:",
                   value = 50, min = 20, max = 200, step = 5),
      numericInput("nsim", "Number of Simulations:",
                   value = 1000, min = 100, max = 5000, step = 100),

      actionButton("run_sim", "Run Simulation", class = "btn-primary"),
      br(), br(),

      # Scenario Management
      h4("Scenario Comparison"),
      textInput("scenario_name", "Scenario Name:", value = ""),
      actionButton("save_scenario", "Save Scenario", class = "btn-success"),
      br(), br(),
      actionButton("clear_scenarios", "Clear All Scenarios", class = "btn-warning")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # Results Tab
        tabPanel("Results",
                 br(),
                 h4("Simulation Summary"),
                 verbatimTextOutput("summary_output"),
                 br(),
                 h4("Total Escapement Over Time"),
                 plotlyOutput("escapement_time_series", height = "400px"),
                 br(),
                 h4("Escapement by Age"),
                 plotlyOutput("escapement_by_age", height = "400px")
        ),

        # Stock-Recruitment Tab
        tabPanel("Stock-Recruitment",
                 br(),
                 h4("Spawner-Recruit Relationship"),
                 plotlyOutput("SR_plot", height = "500px"),
                 br(),
                 h4("Time Series"),
                 plotlyOutput("SR_timeseries", height = "400px")
        ),

        # Age Structure Tab
        tabPanel("Age Structure",
                 br(),
                 h4("Age Composition of Escapement"),
                 helpText("Mean proportion of escapement by age across all return years"),
                 plotlyOutput("age_composition", height = "400px"),
                 br(),
                 h4("Age-Specific Escapement Time Series"),
                 plotlyOutput("age_escapement_ts", height = "500px")
        ),

        # Comparison Tab
        tabPanel("Compare Scenarios",
                 br(),
                 h4("Saved Scenarios"),
                 verbatimTextOutput("scenarios_list"),
                 br(),
                 h4("Escapement Comparison"),
                 plotlyOutput("compare_escapement", height = "400px"),
                 br(),
                 h4("Summary Table"),
                 tableOutput("compare_table")
        )
      )
    )
  )
)


# ============================================================================
# SHINY SERVER
# ============================================================================

server <- function(input, output, session) {

  # Reactive values for storing results
  results <- reactiveVal(NULL)
  scenarios <- reactiveVal(list())

  # Check that age schedule sums to 1
  output$age_schedule_check <- renderText({
    age_sum <- input$prob_age3 + input$prob_age4 + input$prob_age5
    if (abs(age_sum - 1.0) > 0.01) {
      paste("WARNING: Probabilities sum to", round(age_sum, 3), "- should be 1.0")
    } else {
      paste("Age schedule OK (sum =", round(age_sum, 3), ")")
    }
  })

  # Build parameter list from inputs
  build_params <- reactive({
    # Age-at-return schedule
    return_ages <- c(3, 4, 5)
    age_probs <- c(input$prob_age3, input$prob_age4, input$prob_age5)
    age_probs <- age_probs / sum(age_probs)  # normalize

    # Stock-recruitment parameters
    if (input$SR_function == "Beverton-Holt") {
      SR_params <- bh_from_steepness(input$R0, input$S0, input$steepness)
    } else {
      SR_params <- ricker_from_productivity(input$R0, input$S0, input$ricker_productivity)
    }

    list(
      n_brood_years = input$n_brood_years,
      return_ages = return_ages,
      age_probs = age_probs,
      M_ocean = input$M_ocean,
      F_rate = -log(1 - input$harvest_rate),
      SR_function = input$SR_function,
      SR_params = SR_params,
      S0 = input$S0,
      R0 = input$R0,
      rec_cv = input$rec_cv,
      harvest_rate = input$harvest_rate
    )
  })

  # Run simulation
  observeEvent(input$run_sim, {
    params <- build_params()

    withProgress(message = "Running simulation...", value = 0, {
      sim_results <- simulate_salmon_population(
        params = params,
        nsim = input$nsim,
        progress_cb = function(value, detail) {
          incProgress(value, detail = detail)
        }
      )
      results(sim_results)
    })
  })

  # Summary output
  output$summary_output <- renderPrint({
    req(results())
    res <- results()

    # Calculate mean escapement across all years
    mean_total_escapement <- mean(res$escapement_total_mean, na.rm = TRUE)
    sd_total_escapement <- mean(res$escapement_total_sd, na.rm = TRUE)

    # Mean by age
    n_ages <- dim(res$escapement_summary)[2]
    age_names <- dimnames(res$escapement_summary)[[2]]

    cat("=== Salmon Population Model Results ===\n\n")
    cat("Simulation Settings:\n")
    cat("  Brood Years:", res$params$n_brood_years, "\n")
    cat("  Simulations:", res$nsim, "\n")
    cat("  Harvest Rate:", round(res$params$harvest_rate, 3), "\n")
    cat("  Ocean Mortality:", round(res$params$M_ocean, 3), "\n\n")

    cat("Mean Total Escapement:\n")
    cat("  Mean:", round(mean_total_escapement, 0), "\n")
    cat("  SD:", round(sd_total_escapement, 0), "\n\n")

    cat("Mean Escapement by Age:\n")
    for (a in 1:n_ages) {
      age_vals <- res$escapement_summary[, a, "mean"]
      cat("  Age", age_names[a], ":", round(mean(age_vals, na.rm = TRUE), 0), "\n")
    }

    cat("\nStock-Recruitment Parameters:\n")
    cat("  Function:", res$params$SR_function, "\n")
    cat("  Alpha:", round(res$params$SR_params$alpha, 6), "\n")
    cat("  Beta:", round(res$params$SR_params$beta, 9), "\n")
  })

  # Total escapement time series
  output$escapement_time_series <- renderPlotly({
    req(results())
    res <- results()

    n_years <- length(res$escapement_total_mean)
    df <- data.frame(
      year = 1:n_years,
      mean = res$escapement_total_mean,
      lower = res$escapement_total_mean - 1.96 * res$escapement_total_sd,
      upper = res$escapement_total_mean + 1.96 * res$escapement_total_sd
    )
    df$lower <- pmax(0, df$lower)

    p <- ggplot(df, aes(x = year, y = mean)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue") +
      geom_line(color = "steelblue", size = 1) +
      labs(x = "Return Year", y = "Total Escapement",
           title = "Total Escapement Over Time") +
      theme_minimal()

    ggplotly(p)
  })

  # Escapement by age
  output$escapement_by_age <- renderPlotly({
    req(results())
    res <- results()

    esc_summary <- res$escapement_summary
    n_years <- dim(esc_summary)[1]
    n_ages <- dim(esc_summary)[2]
    ages <- as.numeric(dimnames(esc_summary)[[2]])

    # Create long-format data
    df_list <- list()
    for (a_idx in 1:n_ages) {
      age <- ages[a_idx]
      df_list[[a_idx]] <- data.frame(
        year = 1:n_years,
        age = paste("Age", age),
        mean = esc_summary[, a_idx, "mean"],
        lower = esc_summary[, a_idx, "lower"],
        upper = esc_summary[, a_idx, "upper"]
      )
    }
    df <- bind_rows(df_list)

    p <- ggplot(df, aes(x = year, y = mean, color = age, fill = age)) +
      geom_line(size = 0.8) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
      labs(x = "Return Year", y = "Escapement",
           title = "Escapement by Age Class",
           color = "Age", fill = "Age") +
      theme_minimal()

    ggplotly(p)
  })

  # Stock-recruitment plot
  output$SR_plot <- renderPlotly({
    req(results())
    res <- results()

    SR_data <- res$SR_pairs
    params <- res$params

    # Generate theoretical SR curve
    S_seq <- seq(0, max(SR_data$spawners, na.rm = TRUE) * 1.2, length.out = 100)
    if (params$SR_function == "Beverton-Holt") {
      R_seq <- beverton_holt(S_seq, params$SR_params$alpha, params$SR_params$beta)
    } else {
      R_seq <- ricker(S_seq, params$SR_params$alpha, params$SR_params$beta)
    }

    theory_df <- data.frame(S = S_seq, R = R_seq)

    p <- ggplot() +
      geom_point(data = SR_data, aes(x = spawners, y = recruits),
                 alpha = 0.3, color = "gray40") +
      geom_line(data = theory_df, aes(x = S, y = R),
                color = "red", size = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      labs(x = "Spawners", y = "Recruits",
           title = paste(params$SR_function, "Stock-Recruitment Relationship")) +
      theme_minimal()

    ggplotly(p)
  })

  # SR time series
  output$SR_timeseries <- renderPlotly({
    req(results())
    res <- results()

    n_brood <- length(res$spawners_mean)
    df <- data.frame(
      brood_year = 1:n_brood,
      spawners = res$spawners_mean,
      recruits = res$recruits_mean
    )

    df_long <- df %>%
      pivot_longer(cols = c(spawners, recruits),
                   names_to = "variable", values_to = "value")

    p <- ggplot(df_long, aes(x = brood_year, y = value, color = variable)) +
      geom_line(size = 1) +
      labs(x = "Brood Year", y = "Abundance",
           title = "Spawners and Recruits Over Time",
           color = "Variable") +
      theme_minimal()

    ggplotly(p)
  })

  # Age composition
  output$age_composition <- renderPlotly({
    req(results())
    res <- results()

    esc_summary <- res$escapement_summary
    n_ages <- dim(esc_summary)[2]
    ages <- as.numeric(dimnames(esc_summary)[[2]])

    # Calculate mean across all years
    age_means <- colMeans(esc_summary[, , "mean"], na.rm = TRUE)
    age_props <- age_means / sum(age_means)

    df <- data.frame(
      age = paste("Age", ages),
      proportion = age_props
    )

    p <- ggplot(df, aes(x = age, y = proportion, fill = age)) +
      geom_col() +
      labs(x = "Age Class", y = "Proportion of Escapement",
           title = "Mean Age Composition of Escapement") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p)
  })

  # Age-specific escapement time series
  output$age_escapement_ts <- renderPlotly({
    req(results())
    res <- results()

    esc_summary <- res$escapement_summary
    n_years <- dim(esc_summary)[1]
    n_ages <- dim(esc_summary)[2]
    ages <- as.numeric(dimnames(esc_summary)[[2]])

    # Create stacked area data
    df_list <- list()
    for (a_idx in 1:n_ages) {
      age <- ages[a_idx]
      df_list[[a_idx]] <- data.frame(
        year = 1:n_years,
        age = paste("Age", age),
        escapement = esc_summary[, a_idx, "mean"]
      )
    }
    df <- bind_rows(df_list)

    p <- ggplot(df, aes(x = year, y = escapement, fill = age)) +
      geom_area(alpha = 0.7) +
      labs(x = "Return Year", y = "Escapement",
           title = "Age-Specific Escapement (Stacked)",
           fill = "Age") +
      theme_minimal()

    ggplotly(p)
  })

  # Save scenario
  observeEvent(input$save_scenario, {
    req(results())
    req(input$scenario_name != "")

    scenario_list <- scenarios()
    scenario_list[[input$scenario_name]] <- list(
      results = results(),
      params = build_params(),
      timestamp = Sys.time()
    )
    scenarios(scenario_list)

    showNotification(paste("Scenario", input$scenario_name, "saved"), type = "message")
  })

  # Clear scenarios
  observeEvent(input$clear_scenarios, {
    scenarios(list())
    showNotification("All scenarios cleared", type = "warning")
  })

  # List scenarios
  output$scenarios_list <- renderPrint({
    scenario_list <- scenarios()
    if (length(scenario_list) == 0) {
      cat("No scenarios saved yet.\n")
    } else {
      cat("Saved Scenarios:\n")
      for (name in names(scenario_list)) {
        cat("  -", name, "\n")
      }
    }
  })

  # Compare scenarios - escapement
  output$compare_escapement <- renderPlotly({
    scenario_list <- scenarios()
    req(length(scenario_list) > 0)

    df_list <- list()
    for (name in names(scenario_list)) {
      res <- scenario_list[[name]]$results
      n_years <- length(res$escapement_total_mean)
      df_list[[name]] <- data.frame(
        year = 1:n_years,
        escapement = res$escapement_total_mean,
        scenario = name
      )
    }
    df <- bind_rows(df_list)

    p <- ggplot(df, aes(x = year, y = escapement, color = scenario)) +
      geom_line(size = 1) +
      labs(x = "Return Year", y = "Mean Total Escapement",
           title = "Scenario Comparison: Total Escapement",
           color = "Scenario") +
      theme_minimal()

    ggplotly(p)
  })

  # Compare scenarios - table
  output$compare_table <- renderTable({
    scenario_list <- scenarios()
    req(length(scenario_list) > 0)

    summary_list <- list()
    for (name in names(scenario_list)) {
      res <- scenario_list[[name]]$results
      params <- scenario_list[[name]]$params

      mean_escapement <- mean(res$escapement_total_mean, na.rm = TRUE)
      mean_spawners <- mean(res$spawners_mean, na.rm = TRUE)

      summary_list[[name]] <- data.frame(
        Scenario = name,
        HarvestRate = round(params$harvest_rate, 3),
        MeanEscapement = round(mean_escapement, 0),
        MeanSpawners = round(mean_spawners, 0),
        SR_Function = params$SR_function
      )
    }

    bind_rows(summary_list)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
