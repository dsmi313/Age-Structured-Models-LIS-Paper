library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# UI Definition
ui <- fluidPage(
  titlePanel("Age-Structured Population Model: YPR Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Model Parameters"),

      # Species Selection
      h4("Species / Biological Parameters"),
      selectInput("species", "Species:",
                  choices = c("Crappie" = "crappie",
                              "Walleye" = "walleye",
                              "Largemouth Bass" = "lmb",
                              "Smallmouth Bass" = "smb",
                              "Channel Catfish" = "channel_catfish",
                              "Blue Catfish" = "blue_catfish",
                              "Custom" = "custom"),
                  selected = "crappie"),

      h5("Weight-Length Relationship: W = a × L^b"),
      helpText(tags$small(tags$em("W in kg, L in mm"))),
      numericInput("wl_a", "a (coefficient):", value = 2.40991e-6, min = 1e-8, max = 1e-3, step = 1e-7),
      numericInput("wl_b", "b (exponent):", value = 3.38, min = 2.5, max = 4.0, step = 0.01),

      numericInput("mat_size", "Maturity Size (mm):", value = 200, min = 50, max = 500, step = 10),
      helpText(tags$small(tags$em("Fish at this size are sexually mature"))),

      numericInput("memorable_size", "Memorable Size (mm):", value = 305, min = 100, max = 700, step = 5),
      helpText(tags$small(tags$em("Trophy/quality fish threshold"))),

      numericInput("nat_mort", "Natural Mortality (M):", value = 0.35, min = 0.05, max = 1.0, step = 0.01),
      helpText(tags$small(tags$em("Annual natural mortality rate"))),

      numericInput("rec_cv", "Recruitment CV:", value = 0.8, min = 0.1, max = 1.5, step = 0.05),
      helpText(tags$small(tags$em("Coefficient of variation for stochastic recruitment (higher = more variable)"))),
      br(),

      # Growth Parameters
      h4("Growth Parameters (von Bertalanffy)"),
      helpText(tags$small(tags$em("L∞ = max length, K = growth rate, t0 = age at length 0"))),
      selectInput("growth_preset", "Load Preset:",
                  choices = c("Custom" = "custom", "Slow" = "slow", "Moderate" = "moderate", "Fast" = "fast"),
                  selected = "moderate"),
      numericInput("linf", "L∞ (mm):", value = 353, min = 250, max = 900, step = 1),
      numericInput("vbk", "K:", value = 0.374, min = 0.01, max = 1.5, step = 0.001),
      numericInput("t0", "t0:", value = 0.197, min = -1.0, max = 2.0, step = 0.001),

      # Exploitation Parameters
      h4("Exploitation Parameters"),
      helpText(tags$small(tags$em("U = proportion of harvestable fish removed annually"))),
      sliderInput("exploitation", "Exploitation Rate (U):",
                  min = 0.0, max = 1.0, value = 0.34, step = 0.01),

      # Vulnerability Parameters
      h4("Vulnerability & Selectivity"),
      helpText(tags$small(tags$em("Size at which fish become vulnerable to gear and regulations"))),
      numericInput("capsize", "Length at 50% Capture (mm):",
                   value = 204, min = 100, max = 300),
      numericInput("harvlim", "Minimum Harvest Size (mm):",
                   value = 254, min = 150, max = 450),

      checkboxInput("enable_slot", "Enable Slot Limit", value = FALSE),
      conditionalPanel(
        condition = "input.enable_slot == true",
        radioButtons("slot_type", "Slot Type:",
                     choices = c("Traditional (keep fish WITHIN slot)" = "traditional",
                                 "Protective (protect fish WITHIN slot)" = "protective"),
                     selected = "traditional"),
        numericInput("slot_upper", "Maximum Size (mm):",
                     value = 406, min = 250, max = 500),
        helpText(tags$small(tags$em("Traditional: harvest ONLY between min-max. Protective: PROTECT between min-max")))
      ),

      # Mortality Parameters
      h4("Mortality"),
      helpText(tags$small(tags$em("Proportion of released fish that die"))),
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
      uiOutput("scenario_delete_ui"),
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

        tabPanel("Yield Curves",
                 br(),
                 h4("Yield Per Recruit vs Exploitation Rate"),
                 helpText("Shows how YPR and SPR respond to different exploitation rates with current growth and selectivity parameters.",
                          tags$br(),
                          tags$strong("Shaded bands show where 95% of population outcomes fall"),
                          "due to stochastic recruitment variability (not uncertainty in the mean estimate).",
                          tags$br(),
                          "Reference lines show common SPR thresholds (40% = sustainable, 30% = overfished)."),
                 br(),
                 sliderInput("yield_curve_nsim", "Number of Simulations per Point:",
                             min = 1, max = 5000, value = 2000, step = 1),
                 actionButton("run_yield_curve", "Generate Yield Curve", class = "btn-primary"),
                 br(),
                 br(),
                 plotlyOutput("yield_curve_plot", height = "400px"),
                 plotlyOutput("spr_curve_plot", height = "400px"),
                 plotlyOutput("prop_curve_plot", height = "400px")
        ),

        tabPanel("About",
                 br(),
                 h3("Age-Structured Population Model"),
                 p("This Shiny app implements a general age-structured population model originally developed for:"),
                 p(em("Live-imaging sonar use in Texas crappie fisheries: Assessing population-level
                      responses due to potential increases in exploitation.")),
                 br(),
                 h4("Multi-Species Capability"),
                 p("The model now includes presets for multiple species:"),
                 tags$ul(
                   tags$li(strong("Crappie:"), "Default parameters from original study"),
                   tags$li(strong("Walleye:"), "Standard walleye life history parameters"),
                   tags$li(strong("Largemouth Bass:"), "Typical warmwater bass parameters"),
                   tags$li(strong("Smallmouth Bass:"), "Smallmouth bass parameters"),
                  tags$li(strong("Channel Catfish:"), "Parameters from FishBase/literature"),
                  tags$li(strong("Blue Catfish:"), "Parameters from FishBase/literature"),
                   tags$li(strong("Custom:"), "Enter your own species-specific parameters")
                 ),
                 br(),
                 h4("Model Description"),
                 p("The model simulates fish populations using age-structured dynamics with:"),
                 tags$ul(
                   tags$li("Age classes 1-8 years"),
                   tags$li("Species-specific von Bertalanffy growth"),
                   tags$li("Customizable weight-length relationships"),
                   tags$li("Size-dependent vulnerability to capture and harvest"),
                   tags$li("Traditional and protective slot limit options"),
                   tags$li("Natural mortality and discard mortality"),
                   tags$li("Stochastic recruitment (lognormal, species-specific CV)")
                 ),
                 br(),
                 h4("Outputs"),
                 tags$ul(
                   tags$li(strong("YPR:"), "Yield Per Recruit (kg)"),
                   tags$li(strong("SPR:"), "Spawning Potential Ratio (relative to unfished)"),
                   tags$li(strong("Prop Memorable:"), "Proportion of trophy/quality fish")
                 ),
                 br(),
                 h4("References"),
                 p("Crappie model based on work similar to Dotson et al. (2009)"),
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
  yield_curve_data <- reactiveVal(NULL)

  # Species parameter presets
  observeEvent(input$species, {
    if (input$species == "crappie") {
      updateNumericInput(session, "wl_a", value = 2.40991e-6)
      updateNumericInput(session, "wl_b", value = 3.38)
      updateNumericInput(session, "mat_size", value = 180)  # ~7 inches (literature: 6-7" typical)
      updateNumericInput(session, "memorable_size", value = 305)  # 12 inches
      updateNumericInput(session, "linf", value = 353)
      updateNumericInput(session, "vbk", value = 0.374)
      updateNumericInput(session, "t0", value = 0.197)
      updateNumericInput(session, "nat_mort", value = 0.374)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.8)  # High recruitment variability
      showNotification("Loaded Crappie parameters", type = "message")

    } else if (input$species == "walleye") {
      # Craig et al. 1995; Weight-length from North American data
      updateNumericInput(session, "wl_a", value = 3.52e-6)  # From log10(W) = -5.453 + 3.180*log10(L)
      updateNumericInput(session, "wl_b", value = 3.18)
      updateNumericInput(session, "mat_size", value = 356)  # 14 inches (female maturity lower range, literature)
      updateNumericInput(session, "memorable_size", value = 635)  # 25 inches (Gabelhouse 1984)
      updateNumericInput(session, "linf", value = 466)  # Craig et al. 1995
      updateNumericInput(session, "vbk", value = 0.215)  # Craig et al. 1995
      updateNumericInput(session, "t0", value = -0.632)  # Craig et al. 1995
      updateNumericInput(session, "nat_mort", value = 0.215)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 1.1)  # Very high recruitment variability (literature: CV=112%)
      showNotification("Loaded Walleye parameters (Craig et al. 1995)", type = "message")

    } else if (input$species == "lmb") {
      # Lake Trasimeno study; averaged male/female parameters
      updateNumericInput(session, "wl_a", value = 9.88e-6)  # From W=0.00988*L^3.15
      updateNumericInput(session, "wl_b", value = 3.15)
      updateNumericInput(session, "mat_size", value = 203)  # 8 inches (female maturity, literature)
      updateNumericInput(session, "memorable_size", value = 508)  # 20 inches (Gabelhouse 1984)
      updateNumericInput(session, "linf", value = 450)  # Average of male/female
      updateNumericInput(session, "vbk", value = 0.35)  # Average of male/female
      updateNumericInput(session, "t0", value = 0.04)
      updateNumericInput(session, "nat_mort", value = 0.35)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.5)  # Moderate-high recruitment variability (literature: CV>0.5)
      showNotification("Loaded Largemouth Bass parameters (literature)", type = "message")

    } else if (input$species == "smb") {
      # Conservative estimates based on typical smallmouth bass populations
      updateNumericInput(session, "wl_a", value = 1.08e-5)
      updateNumericInput(session, "wl_b", value = 3.08)
      updateNumericInput(session, "mat_size", value = 254)  # 10 inches (female first spawn lower range, literature)
      updateNumericInput(session, "memorable_size", value = 432)  # 17 inches (Gabelhouse 1984)
      updateNumericInput(session, "linf", value = 420)
      updateNumericInput(session, "vbk", value = 0.25)
      updateNumericInput(session, "t0", value = -0.3)
      updateNumericInput(session, "nat_mort", value = 0.25)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.7)  # Moderate-high recruitment variability (literature: CV=52-80%)
      showNotification("Loaded Smallmouth Bass parameters (typical values)", type = "message")

    } else if (input$species == "channel_catfish") {
      # Channel catfish parameters (FishBase/literature/Gabelhouse 1984)
      # W-L from NLLS: W(g) = 0.00522 * L(cm)^3.2293, converted to kg and mm
      updateNumericInput(session, "wl_a", value = 3.08e-9)
      updateNumericInput(session, "wl_b", value = 3.23)
      updateNumericInput(session, "mat_size", value = 356)  # 14 inches (literature: female maturity)
      updateNumericInput(session, "memorable_size", value = 711)  # 28 inches (Gabelhouse 1984)
      updateNumericInput(session, "linf", value = 650)  # Moderate growth
      updateNumericInput(session, "vbk", value = 0.18)
      updateNumericInput(session, "t0", value = -1.2)
      updateNumericInput(session, "nat_mort", value = 0.18)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.4)  # Moderate recruitment variability
      showNotification("Loaded Channel Catfish parameters (literature/Gabelhouse 1984)", type = "message")

    } else if (input$species == "blue_catfish") {
      # Blue catfish parameters (FishBase/literature/Gabelhouse 1984)
      # W-L from FishBase Bayesian: W(g) = 0.00525 * L(cm)^3.11, converted to kg and mm
      updateNumericInput(session, "wl_a", value = 4.08e-9)
      updateNumericInput(session, "wl_b", value = 3.11)
      updateNumericInput(session, "mat_size", value = 350)  # ~14 inches (female maturity lower range, literature: 35-50cm)
      updateNumericInput(session, "memorable_size", value = 889)  # 35 inches (Gabelhouse 1984)
      updateNumericInput(session, "linf", value = 900)  # Moderate growth (larger species)
      updateNumericInput(session, "vbk", value = 0.15)
      updateNumericInput(session, "t0", value = -1.2)
      updateNumericInput(session, "nat_mort", value = 0.15)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.5)  # Moderate-high recruitment variability (literature: σR=0.49, Hilling et al. 2025)
      showNotification("Loaded Blue Catfish parameters (literature/Gabelhouse 1984)", type = "message")
    }
    # If custom, don't update anything
  })

  # Dynamic UI for deleting individual scenarios
  output$scenario_delete_ui <- renderUI({
    scenarios <- saved_scenarios()
    if(nrow(scenarios) == 0) return(NULL)

    selectInput("scenario_to_delete", "Delete Scenario:",
                choices = c("Select scenario..." = "", scenarios$Scenario),
                selectize = TRUE)
  })

  # Delete individual scenario
  observeEvent(input$scenario_to_delete, {
    req(input$scenario_to_delete != "")

    scenario_name <- input$scenario_to_delete

    # Remove from saved scenarios
    scenarios <- saved_scenarios()
    scenarios <- scenarios[scenarios$Scenario != scenario_name, ]
    saved_scenarios(scenarios)

    # Remove from detailed results
    details <- detailed_results()
    details <- details[details$Scenario != scenario_name, ]
    detailed_results(details)

    showNotification(paste("Deleted:", scenario_name), type = "warning")

    # Reset the selector
    updateSelectInput(session, "scenario_to_delete", selected = "")
  })

  # Observer to update growth parameters when preset is selected
  # Species-specific growth presets
  observeEvent(input$growth_preset, {
    req(input$species, input$growth_preset)

    # Crappie growth parameters (from original study)
    if (input$species == "crappie") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 333)
        updateNumericInput(session, "vbk", value = 0.325)
        updateNumericInput(session, "t0", value = 0.174)
        updateNumericInput(session, "nat_mort", value = 0.325)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 353)
        updateNumericInput(session, "vbk", value = 0.374)
        updateNumericInput(session, "t0", value = 0.197)
        updateNumericInput(session, "nat_mort", value = 0.374)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 356)
        updateNumericInput(session, "vbk", value = 0.691)
        updateNumericInput(session, "t0", value = -0.056)
        updateNumericInput(session, "nat_mort", value = 0.691)  # M = K
      }
    }
    # Walleye growth parameters (FishBase K range: 0.05-0.45)
    else if (input$species == "walleye") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 500)  # Northern populations
        updateNumericInput(session, "vbk", value = 0.12)
        updateNumericInput(session, "t0", value = -0.5)
        updateNumericInput(session, "nat_mort", value = 0.12)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 466)  # Craig et al. 1995
        updateNumericInput(session, "vbk", value = 0.215)
        updateNumericInput(session, "t0", value = -0.632)
        updateNumericInput(session, "nat_mort", value = 0.215)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 430)  # Southern populations
        updateNumericInput(session, "vbk", value = 0.35)
        updateNumericInput(session, "t0", value = -0.75)
        updateNumericInput(session, "nat_mort", value = 0.35)  # M = K
      }
    }
    # Largemouth Bass growth parameters
    else if (input$species == "lmb") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 400)  # Northern strain
        updateNumericInput(session, "vbk", value = 0.25)
        updateNumericInput(session, "t0", value = 0.1)
        updateNumericInput(session, "nat_mort", value = 0.25)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 450)  # Average
        updateNumericInput(session, "vbk", value = 0.35)
        updateNumericInput(session, "t0", value = 0.04)
        updateNumericInput(session, "nat_mort", value = 0.35)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 550)  # Florida strain
        updateNumericInput(session, "vbk", value = 0.42)
        updateNumericInput(session, "t0", value = -0.1)
        updateNumericInput(session, "nat_mort", value = 0.42)  # M = K
      }
    }
    # Smallmouth Bass growth parameters (FishBase K range: 0.10-0.28)
    else if (input$species == "smb") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 450)  # Oligotrophic systems
        updateNumericInput(session, "vbk", value = 0.18)
        updateNumericInput(session, "t0", value = -0.2)
        updateNumericInput(session, "nat_mort", value = 0.18)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 420)  # Typical
        updateNumericInput(session, "vbk", value = 0.25)
        updateNumericInput(session, "t0", value = -0.3)
        updateNumericInput(session, "nat_mort", value = 0.25)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 380)  # Productive systems
        updateNumericInput(session, "vbk", value = 0.35)
        updateNumericInput(session, "t0", value = -0.4)
        updateNumericInput(session, "nat_mort", value = 0.35)  # M = K
      }
    }
    # Channel Catfish growth parameters (literature-based estimates)
    else if (input$species == "channel_catfish") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 600)  # Slower growing populations
        updateNumericInput(session, "vbk", value = 0.12)
        updateNumericInput(session, "t0", value = -1.0)
        updateNumericInput(session, "nat_mort", value = 0.12)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 650)  # Typical growth
        updateNumericInput(session, "vbk", value = 0.18)
        updateNumericInput(session, "t0", value = -1.2)
        updateNumericInput(session, "nat_mort", value = 0.18)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 700)  # Fast growing populations
        updateNumericInput(session, "vbk", value = 0.24)
        updateNumericInput(session, "t0", value = -1.5)
        updateNumericInput(session, "nat_mort", value = 0.24)  # M = K
      }
    }
    # Blue Catfish growth parameters (literature-based estimates, larger species)
    else if (input$species == "blue_catfish") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 800)  # Slower growing populations
        updateNumericInput(session, "vbk", value = 0.10)
        updateNumericInput(session, "t0", value = -1.0)
        updateNumericInput(session, "nat_mort", value = 0.10)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 900)  # Typical growth
        updateNumericInput(session, "vbk", value = 0.15)
        updateNumericInput(session, "t0", value = -1.2)
        updateNumericInput(session, "nat_mort", value = 0.15)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 1000)  # Fast growing populations
        updateNumericInput(session, "vbk", value = 0.20)
        updateNumericInput(session, "t0", value = -1.5)
        updateNumericInput(session, "nat_mort", value = 0.20)  # M = K
      }
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

      # Weight-length equation (species-specific)
      alfa <- input$wl_a
      bet <- input$wl_b

      # Mortality
      DisMort <- input$dismort
      Nat_mort <- input$nat_mort

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

      # Store ALL time series data from all simulations
      all_YPR <- matrix(NA, Ymax, nsim)
      all_SPR <- matrix(NA, Ymax, nsim)
      all_Prop <- matrix(NA, Ymax, nsim)

      for(k in 1:nsim) {

        incProgress(1/nsim, detail = paste("Simulation", k, "of", nsim))

        N <- matrix(NA, Ymax, Amax)
        Wmat <- (alfa * rnorm(1, input$mat_size, input$mat_size * 0.1)^bet) / 1000
        Yield <- rep(NA, Ymax)
        SPRt <- rep(NA, Ymax)
        YPR <- rep(NA, Ymax)
        Prop <- rep(NA, Ymax)

        S <- exp(-Nat_mort)^(Age - 1)
        So <- exp(-Nat_mort)

        N[1, 1] <- 10000
        N[1, ] <- Ro * S

        Rcapacity <- Ro * rlnorm(Ymax, 0, sd = input$rec_cv)

        U <- input$exploitation
        Uo <- input$exploitation + 0.1

        TL <- growth_params$Linf * (1 - exp(-growth_params$vbk * (Age - growth_params$t0)))
        Wt <- (alfa * TL^bet) / 1000
        Fec <- pmax(Wt - Wmat, 0)

        Vulcap <- 1 / (1 + exp(-(TL - Capsize) / CapsizeSD))

        # Calculate harvest vulnerability with or without slot limit
        if(input$enable_slot) {
          Slot_upper <- input$slot_upper
          # Use extremely small SD for near-step-function slot boundaries
          Slot_upperSD <- 0.01
          HarvlimSD_slot <- 0.01

          # Logistic for minimum size (vulnerable above min)
          Vulharv_above_min <- 1 / (1 + exp(-(TL - Harvlim) / HarvlimSD_slot))
          # Logistic for maximum size (vulnerable below max)
          Vulharv_below_max <- 1 / (1 + exp((TL - Slot_upper) / Slot_upperSD))

          if(input$slot_type == "traditional") {
            # Traditional slot: harvest ONLY within slot (min to max)
            # Zero vulnerability outside slot
            Vulharv <- Vulharv_above_min * Vulharv_below_max
          } else {
            # Protective slot: PROTECT within slot (min to max)
            # Zero vulnerability within slot, full vulnerability outside
            Vulharv <- 1 - (Vulharv_above_min * Vulharv_below_max)
          }
        } else {
          # Standard minimum length limit only
          Vulharv <- 1 / (1 + exp(-(TL - Harvlim) / HarvlimSD))
        }

        for(i in 2:Ymax) {
          N[i, 1] <- Rcapacity[i - 1]
          for(j in 2:Amax) {
            trophyvul <- (1 / (1 + exp(-(TL - input$memorable_size) / (input$memorable_size * 0.1)))) * Vulcap[j]

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

        # Store time series from this simulation
        all_YPR[, k] <- YPR
        all_SPR[, k] <- SPRt
        all_Prop[, k] <- Prop

        # Store population structure from first simulation
        if(k == 1) {
          pop_data <- data.frame(
            Age = Age,
            Length = TL,
            Weight = Wt,
            Abundance = N[Ymax, ],
            VulCapture = Vulcap,
            VulHarvest = Vulharv
          )
          pop_structure_data(pop_data)
        }
      }

      # Calculate mean and SD across all simulations at each year
      ts_data <- data.frame(
        Year = 1:Ymax,
        YPR_mean = rowMeans(all_YPR, na.rm = TRUE),
        YPR_sd = apply(all_YPR, 1, sd, na.rm = TRUE),
        SPR_mean = rowMeans(all_SPR, na.rm = TRUE),
        SPR_sd = apply(all_SPR, 1, sd, na.rm = TRUE),
        Prop_mean = rowMeans(all_Prop, na.rm = TRUE),
        Prop_sd = apply(all_Prop, 1, sd, na.rm = TRUE)
      )

      # Calculate 95% prediction intervals: mean ± 1.96 × SD
      ts_data$YPR_lower <- ts_data$YPR_mean - 1.96 * ts_data$YPR_sd
      ts_data$YPR_upper <- ts_data$YPR_mean + 1.96 * ts_data$YPR_sd
      ts_data$SPR_lower <- ts_data$SPR_mean - 1.96 * ts_data$SPR_sd
      ts_data$SPR_upper <- ts_data$SPR_mean + 1.96 * ts_data$SPR_sd
      ts_data$Prop_lower <- ts_data$Prop_mean - 1.96 * ts_data$Prop_sd
      ts_data$Prop_upper <- ts_data$Prop_mean + 1.96 * ts_data$Prop_sd

      time_series_data(ts_data)

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

    if(input$enable_slot) {
      slot_label <- ifelse(input$slot_type == "traditional",
                           "Traditional Slot (keep",
                           "Protective Slot (protect")
      cat(sprintf("  %s %.1f - %.1f\"): %.0f - %.0f mm\n",
                  slot_label,
                  input$harvlim / 25.4,
                  input$slot_upper / 25.4,
                  input$harvlim,
                  input$slot_upper))
    } else {
      cat(sprintf("  Minimum Length: %.1f\" (%.0f mm)\n",
                  input$harvlim / 25.4,
                  input$harvlim))
    }

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

    # Convert memorable size from mm to inches for display
    memorable_inches <- round(input$memorable_size / 25.4, 1)

    p <- ggplot(results, aes(x = "", y = Prop)) +
      geom_violin(fill = "orange", alpha = 0.7, color = "black") +
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
      labs(title = paste0("Proportion of Memorable-Sized Fish (≥", memorable_inches, " inches)"),
           x = "", y = "Proportion") +
      theme_minimal() +
      theme(axis.text.x = element_blank())

    ggplotly(p)
  })

  # Time series plot
  output$timeseries_plot <- renderPlotly({
    req(time_series_data())
    ts_data <- time_series_data()

    # Create separate plots for each metric with ribbons
    p1 <- ggplot(ts_data, aes(x = Year, y = YPR_mean)) +
      geom_ribbon(aes(ymin = YPR_lower, ymax = YPR_upper),
                  alpha = 0.2, fill = "steelblue") +
      geom_line(color = "steelblue", size = 1) +
      labs(title = "YPR Over Time (Mean ± 95% Prediction Interval)",
           x = "", y = "YPR (kg)") +
      theme_minimal()

    p2 <- ggplot(ts_data, aes(x = Year, y = SPR_mean)) +
      geom_ribbon(aes(ymin = SPR_lower, ymax = SPR_upper),
                  alpha = 0.2, fill = "darkgreen") +
      geom_line(color = "darkgreen", size = 1) +
      geom_hline(yintercept = 0.40, linetype = "dashed", color = "orange", alpha = 0.7) +
      geom_hline(yintercept = 0.30, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = "SPR Over Time (Mean ± 95% Prediction Interval)",
           subtitle = "Dashed lines: 40% (sustainable), 30% (overfished)",
           x = "", y = "SPR") +
      theme_minimal()

    memorable_inches <- round(input$memorable_size / 25.4, 1)
    p3 <- ggplot(ts_data, aes(x = Year, y = Prop_mean)) +
      geom_ribbon(aes(ymin = Prop_lower, ymax = Prop_upper),
                  alpha = 0.2, fill = "darkorange") +
      geom_line(color = "darkorange", size = 1) +
      labs(title = paste0("Proportion Memorable (≥", memorable_inches, "\") Over Time"),
           subtitle = "Mean ± 95% prediction interval",
           x = "Year", y = "Proportion") +
      theme_minimal()

    # Combine plots vertically
    subplot(
      ggplotly(p1),
      ggplotly(p2),
      ggplotly(p3),
      nrows = 3,
      shareX = TRUE,
      titleY = TRUE
    ) %>%
      layout(title = list(
        text = paste0("Population Metrics Over Time<br>",
                     "<sup>Mean across ", input$nsim, " simulations with 95% prediction intervals</sup>"),
        x = 0.5,
        xanchor = "center"
      ))
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
      Slot_enabled = input$enable_slot,
      Slot_type = ifelse(input$enable_slot, input$slot_type, NA),
      Slot_upper_mm = ifelse(input$enable_slot, input$slot_upper, NA),
      Slot_upper_inches = ifelse(input$enable_slot, round(input$slot_upper / 25.4, 1), NA),
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

        if(scenarios$Slot_enabled[i]) {
          slot_label <- ifelse(scenarios$Slot_type[i] == "traditional",
                               "keep", "protect")
          cat(sprintf("   U=%.2f%%, %s %.1f-%.1f\", L∞=%.0f, K=%.3f\n",
                      scenarios$Exploitation[i] * 100,
                      slot_label,
                      scenarios$MLL_inches[i],
                      scenarios$Slot_upper_inches[i],
                      scenarios$Linf[i],
                      scenarios$K[i]))
        } else {
          cat(sprintf("   U=%.2f%%, MLL=%.1f\", L∞=%.0f, K=%.3f\n",
                      scenarios$Exploitation[i] * 100,
                      scenarios$MLL_inches[i],
                      scenarios$Linf[i],
                      scenarios$K[i]))
        }

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
      mutate(Regulation = ifelse(Slot_enabled,
                                  ifelse(Slot_type == "traditional",
                                         paste0("keep ", MLL_inches, "-", Slot_upper_inches, "\""),
                                         paste0("protect ", MLL_inches, "-", Slot_upper_inches, "\"")),
                                  paste0(MLL_inches, "\" min"))) %>%
      select(Scenario, Exploitation, Regulation, Linf, K,
             YPR_mean, SPR_mean, Prop_mean) %>%
      rename(
        `U (%)` = Exploitation,
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

  # Yield Curve Generation
  observeEvent(input$run_yield_curve, {
    withProgress(message = 'Generating yield curve...', value = 0, {

      growth_params <- get_growth_params()
      Amax <- 8
      Ymax <- input$ymax

      # Weight-length equation (species-specific)
      alfa <- input$wl_a
      bet <- input$wl_b

      # Mortality
      DisMort <- input$dismort
      Nat_mort <- input$nat_mort

      # Stock-recruit
      Ro <- 10000

      # Vulnerabilities (use current settings)
      Capsize <- input$capsize
      CapsizeSD <- Capsize * 0.01
      Uppercap <- 380
      UppercapSD <- Uppercap * 0.01
      Harvlim <- input$harvlim
      HarvlimSD <- Harvlim * 0.01

      Age <- seq(1, Amax)

      # Test exploitation rates from 0 to 1
      U_values <- seq(0, 1, by = 0.05)
      nsim <- input$yield_curve_nsim

      curve_results <- data.frame()

      for(u_idx in seq_along(U_values)) {
        incProgress(1/length(U_values), detail = paste("U =", round(U_values[u_idx], 2)))

        U_test <- U_values[u_idx]
        ypr_vals <- numeric(nsim)
        spr_vals <- numeric(nsim)
        prop_vals <- numeric(nsim)

        for(k in 1:nsim) {
          N <- matrix(NA, Ymax, Amax)
          Wmat <- (alfa * rnorm(1, input$mat_size, input$mat_size * 0.1)^bet) / 1000
          YPR <- rep(NA, Ymax)
          SPRt <- rep(NA, Ymax)
          Prop <- rep(NA, Ymax)

          S <- exp(-Nat_mort)^(Age - 1)
          So <- exp(-Nat_mort)

          N[1, 1] <- 10000
          N[1, ] <- Ro * S

          Rcapacity <- Ro * rlnorm(Ymax, 0, sd = input$rec_cv)

          U <- U_test
          Uo <- U_test + 0.1

          TL <- growth_params$Linf * (1 - exp(-growth_params$vbk * (Age - growth_params$t0)))
          Wt <- (alfa * TL^bet) / 1000
          Fec <- pmax(Wt - Wmat, 0)

          Vulcap <- 1 / (1 + exp(-(TL - Capsize) / CapsizeSD))

          # Calculate harvest vulnerability with or without slot limit
          if(input$enable_slot) {
            Slot_upper <- input$slot_upper
            # Use extremely small SD for near-step-function slot boundaries
            Slot_upperSD <- 0.01
            HarvlimSD_slot <- 0.01

            # Logistic for minimum size (vulnerable above min)
            Vulharv_above_min <- 1 / (1 + exp(-(TL - Harvlim) / HarvlimSD_slot))
            # Logistic for maximum size (vulnerable below max)
            Vulharv_below_max <- 1 / (1 + exp((TL - Slot_upper) / Slot_upperSD))

            if(input$slot_type == "traditional") {
              # Traditional slot: harvest ONLY within slot (min to max)
              # Zero vulnerability outside slot
              Vulharv <- Vulharv_above_min * Vulharv_below_max
            } else {
              # Protective slot: PROTECT within slot (min to max)
              # Zero vulnerability within slot, full vulnerability outside
              Vulharv <- 1 - (Vulharv_above_min * Vulharv_below_max)
            }
          } else {
            # Standard minimum length limit only
            Vulharv <- 1 / (1 + exp(-(TL - Harvlim) / HarvlimSD))
          }

          for(i in 2:Ymax) {
            N[i, 1] <- Rcapacity[i - 1]
            for(j in 2:Amax) {
              trophyvul <- (1 / (1 + exp(-(TL - input$memorable_size) / (input$memorable_size * 0.1)))) * Vulcap[j]

              N[i, j] <- N[i-1, j-1] * So *
                (1 - (Vulcap[j-1] * Uo - Vulharv[j-1] * U) * DisMort) *
                (1 - Vulharv[j-1] * U)

              YPR[i] <- (sum(Wt * Vulharv * N[i, ]) * U) / N[i, 1]
              SPRt[i] <- (sum(N[i, ] * Fec)) / (sum(N[1, ] * Fec))
              Prop[i] <- sum(trophyvul * N[i, ]) / sum(N[i, ])
            }
          }

          ypr_vals[k] <- mean(YPR[50:Ymax], na.rm = TRUE)
          spr_vals[k] <- mean(SPRt[50:Ymax], na.rm = TRUE)
          prop_vals[k] <- mean(Prop[50:Ymax], na.rm = TRUE)
        }

        curve_results <- rbind(curve_results, data.frame(
          U = U_test,
          YPR_mean = mean(ypr_vals, na.rm = TRUE),
          YPR_sd = sd(ypr_vals, na.rm = TRUE),
          YPR_n = nsim,
          SPR_mean = mean(spr_vals, na.rm = TRUE),
          SPR_sd = sd(spr_vals, na.rm = TRUE),
          SPR_n = nsim,
          Prop_mean = mean(prop_vals, na.rm = TRUE),
          Prop_sd = sd(prop_vals, na.rm = TRUE),
          Prop_n = nsim
        ))
      }

      yield_curve_data(curve_results)
    })
  })

  # Yield curve plot
  output$yield_curve_plot <- renderPlotly({
    curve_data <- yield_curve_data()
    req(!is.null(curve_data))

    # Calculate 95% prediction interval: mean ± 1.96 × SD
    # Shows where 95% of population outcomes fall due to recruitment variability
    curve_data$YPR_lower <- curve_data$YPR_mean - 1.96 * curve_data$YPR_sd
    curve_data$YPR_upper <- curve_data$YPR_mean + 1.96 * curve_data$YPR_sd

    p <- ggplot(curve_data, aes(x = U * 100, y = YPR_mean)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_ribbon(aes(ymin = YPR_lower, ymax = YPR_upper),
                  alpha = 0.2, fill = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      labs(title = "Yield Per Recruit vs Exploitation Rate",
           subtitle = "Shaded band: 95% of population outcomes due to recruitment variability",
           x = "Exploitation Rate (%)",
           y = "YPR (kg)") +
      theme_minimal()

    ggplotly(p)
  })

  # SPR curve plot
  output$spr_curve_plot <- renderPlotly({
    curve_data <- yield_curve_data()
    req(!is.null(curve_data))

    # Calculate 95% prediction interval: mean ± 1.96 × SD
    # Shows where 95% of population outcomes fall due to recruitment variability
    curve_data$SPR_lower <- curve_data$SPR_mean - 1.96 * curve_data$SPR_sd
    curve_data$SPR_upper <- curve_data$SPR_mean + 1.96 * curve_data$SPR_sd

    p <- ggplot(curve_data, aes(x = U * 100, y = SPR_mean)) +
      geom_line(color = "darkgreen", size = 1.5) +
      geom_ribbon(aes(ymin = SPR_lower, ymax = SPR_upper),
                  alpha = 0.2, fill = "darkgreen") +
      geom_point(color = "darkgreen", size = 2) +
      geom_hline(yintercept = 0.40, linetype = "dashed", color = "orange", size = 1) +
      geom_hline(yintercept = 0.30, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = 90, y = 0.42, label = "SPR = 40% (Sustainable)", color = "orange", size = 3) +
      annotate("text", x = 90, y = 0.32, label = "SPR = 30% (Overfished)", color = "red", size = 3) +
      labs(title = "Spawning Potential Ratio vs Exploitation Rate",
           subtitle = "Shaded band: 95% of population outcomes due to recruitment variability",
           x = "Exploitation Rate (%)",
           y = "SPR") +
      theme_minimal()

    ggplotly(p)
  })

  # Proportion memorable curve plot
  output$prop_curve_plot <- renderPlotly({
    curve_data <- yield_curve_data()
    req(!is.null(curve_data))

    # Calculate 95% prediction interval: mean ± 1.96 × SD
    # Shows where 95% of population outcomes fall due to recruitment variability
    curve_data$Prop_lower <- curve_data$Prop_mean - 1.96 * curve_data$Prop_sd
    curve_data$Prop_upper <- curve_data$Prop_mean + 1.96 * curve_data$Prop_sd

    # Convert memorable size from mm to inches for display
    memorable_inches <- round(input$memorable_size / 25.4, 1)

    p <- ggplot(curve_data, aes(x = U * 100, y = Prop_mean)) +
      geom_line(color = "darkorange", size = 1.5) +
      geom_ribbon(aes(ymin = Prop_lower, ymax = Prop_upper),
                  alpha = 0.2, fill = "darkorange") +
      geom_point(color = "darkorange", size = 2) +
      labs(title = paste0("Proportion of Memorable-Sized Fish (≥", memorable_inches, "\") vs Exploitation Rate"),
           subtitle = "Shaded band: 95% of population outcomes due to recruitment variability",
           x = "Exploitation Rate (%)",
           y = "Proportion Memorable") +
      theme_minimal()

    ggplotly(p)
  })

  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("ypr_simulation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(sim_results())
      write.csv(sim_results(), file, row.names = FALSE)
    }
  )

  # Download comparison
  output$download_comparison <- downloadHandler(
    filename = function() {
      paste0("ypr_comparison_", Sys.Date(), ".csv")
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
