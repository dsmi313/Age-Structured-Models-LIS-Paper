library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Load modular model engine
source("model_engine.R")

# UI Definition
ui <- fluidPage(
  titlePanel("Age-Structured Population Model: YPR Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Parameters"),
      
      # Species Selection
      h4("Species / Biological Parameters"),
      selectInput("species", "Species:",
                  choices = c("White Crappie" = "white_crappie",
                              "Black Crappie" = "black_crappie",
                              "Walleye" = "walleye",
                              "Largemouth Bass" = "lmb",
                              "Smallmouth Bass" = "smb",
                              "Channel Catfish" = "channel_catfish",
                              "Blue Catfish" = "blue_catfish",
                              "Custom" = "custom"),
                  selected = "white_crappie"),
      
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
      
      numericInput("rec_cv", "Recruitment CV:", value = 0.8, min = 0.0, max = 1.5, step = 0.05),
      helpText(tags$small(tags$em("Coefficient of variation for stochastic recruitment (0 = deterministic, higher = more variable)"))),

      numericInput("R0", "Unfished Recruitment (R0):", value = 10000, min = 100, max = 1000000, step = 1000),
      helpText(tags$small(tags$em("Average recruitment in unfished equilibrium (number of age-1 fish)"))),

      numericInput("amax", "Maximum Age (years):", value = 8, min = 5, max = 50, step = 1),
      helpText(tags$small(tags$em("Maximum age class in the model"))),

      # Density-Dependent Recruitment (Experimental)
      h4("Recruitment Dynamics (Experimental)", style = "color: orange;"),
      checkboxInput("enable_ddr", "Enable Density-Dependent Recruitment (Beverton-Holt)", value = FALSE),
      helpText(tags$small(tags$em(tags$strong("⚠️ Experimental:"), " When disabled (default), recruitment = constant R0 with stochastic noise (traditional per-recruit model). When enabled, recruitment depends on spawning stock biomass."))),
      conditionalPanel(
        condition = "input.enable_ddr == true",
        sliderInput("steepness", "Steepness (h):",
                    min = 0.5, max = 0.95, value = 0.7, step = 0.01),
        helpText(tags$small(tags$em("h = 0.5: strong compensation (recruitment proportional to SSB). h = 0.8+: weak compensation (recruitment nearly constant). Typical: 0.7-0.8."))),
        br(),
        checkboxInput("enable_depensation", "Enable Depensation (Allee Effects)", value = FALSE),
        helpText(tags$small(tags$em("When enabled, recruitment crashes when SSB drops below 20% of unfished level. Simulates mate-finding failure, predator swamping failure, and other critical thresholds.")))
      ),
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
      
      numericInput("growth_cv", "Growth CV:", value = 0.15, min = 0.0, max = 0.5, step = 0.05),
      helpText(tags$small(tags$em("Coefficient of variation for length-at-age (0 = deterministic, 0.15 = 15% variation)"))),
      
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
      
      # Regulation type: mutually exclusive options
      h4("Length Regulations (choose one)"),
      helpText(tags$small(tags$em("Select either standard minimum, slot limit, OR maximum limit - not multiple"))),
      
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
      
      conditionalPanel(
        condition = "input.enable_slot == false",
        checkboxInput("enable_max_limit", "Enable Maximum Length Limit", value = FALSE),
        conditionalPanel(
          condition = "input.enable_max_limit == true && input.enable_slot == false",
          numericInput("max_harvest_size", "Maximum Harvest Size (mm):",
                       value = 500, min = 300, max = 800),
          helpText(tags$small(tags$em("Protects all fish above this size. Harvestable window: Capture Size to Maximum Size.")))
        )
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
                   value = 120, min = 50, max = 200),
      
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
                 h4("Age Distribution at Equilibrium"),
                 helpText("Shows the number of fish in each age class at equilibrium. Ages are tracked directly from recruited cohorts through the simulation (no back-calculation).",
                          tags$br(),
                          tags$strong("Bars show median abundance across simulations."), "Shaded area shows a 95% prediction interval (median ± 1.96 × SD) capturing recruitment and growth variability."),
                 plotlyOutput("pop_structure", height = "500px"),
                 br(),
                 h4("Length-Frequency Distribution"),
                 helpText("Shows the distribution of fish lengths in the equilibrium population. Bars show mean abundance, shaded area shows 95% prediction interval (mean ± 1.96 × SD)."),
                 plotlyOutput("length_frequency", height = "400px"),
                 br(),
                 h4("Vulnerability by Length"),
                 helpText("Blue curve: probability a fish is caught (capture vulnerability). Red curve: probability a caught fish is legally harvestable."),
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
                 sliderInput("yield_curve_nsim", "Number of Simulations per Point:",
                             min = 500, max = 10000, value = 1000, step = 500, ticks = FALSE),
                 helpText(tags$small(tags$em("Higher values = smoother curves but slower. 1000-2000 recommended for publication quality."))),
                 actionButton("run_yield_curve", "Generate Yield Curve", class = "btn-primary"),
                 br(),
                 br(),
                 conditionalPanel(
                   condition = "input.enable_ddr == true && output.msy_plot",
                   h4("Maximum Sustainable Yield (MSY) Analysis"),
                   helpText("Shows total yield and equilibrium recruitment across exploitation rates.",
                            tags$br(),
                            tags$strong("Red star marks MSY:"), " the maximum sustainable yield and optimal exploitation rate (U_MSY).",
                            tags$br(),
                            tags$strong("Total Yield (blue):"), " YPR × Recruitment - the actual population-level harvest.",
                            tags$br(),
                            tags$strong("Recruitment (green):"), " Equilibrium recruitment at each exploitation rate (with DDR if enabled).")
                 ),
                 conditionalPanel(
                   condition = "input.enable_ddr == true",
                   plotlyOutput("msy_plot", height = "500px"),
                   br()
                 ),
                 conditionalPanel(
                   condition = "output.yield_curve_plot",
                   h4("Yield Per Recruit vs Exploitation Rate"),
                   helpText("Shows how YPR and SPR respond to different exploitation rates with current growth and selectivity parameters.",
                            tags$br(),
                            tags$strong("Shaded bands show where 95% of population outcomes fall"),
                            "due to stochastic recruitment variability (not uncertainty in the mean estimate).",
                            tags$br(),
                            "Reference lines show common SPR thresholds (40% = sustainable, 30% = overfished).")
                 ),
                 plotlyOutput("yield_curve_plot", height = "400px"),
                 plotlyOutput("spr_curve_plot", height = "400px"),
                 plotlyOutput("prop_curve_plot", height = "400px")
        ),
        
        tabPanel("About",
                 br(),
                 h3("Length-Structured Population Model with Growth Variability"),
                 p("This Shiny app implements a length-structured population model with individual growth variability, originally developed for:"),
                 p(em("Smith, D.R., Bennett, D.L., Norman, J.D., Allen, M.S. 2025. Live-imaging sonar use in Texas crappie fisheries: Examining population-level responses due to potential increases in exploitation. Fisheries, vuae015. ",
                      a(href = "https://doi.org/10.1093/fshmag/vuae015", "https://doi.org/10.1093/fshmag/vuae015"))),
                 br(),
                 h4("Multi-Species Capability"),
                 p("The model includes presets for multiple species with standardized parameters:"),
                 tags$ul(
                   tags$li(strong("White Crappie:"), "Empirical parameters from Smith et al. (2025)"),
                   tags$li(strong("Black Crappie:"), "FishBase median weight-length and growth parameters"),
                   tags$li(strong("Walleye:"), "FishBase median parameters"),
                   tags$li(strong("Largemouth Bass:"), "FishBase median parameters"),
                   tags$li(strong("Smallmouth Bass:"), "FishBase median parameters"),
                   tags$li(strong("Channel Catfish:"), "FishBase median parameters"),
                   tags$li(strong("Blue Catfish:"), "FishBase median parameters"),
                   tags$li(strong("Custom:"), "Enter your own species-specific parameters")
                 ),
                 br(),
                 h4("Model Features"),
                 p("The model simulates fish populations using length-structured dynamics with:"),
                 tags$ul(
                   tags$li(strong("Length bins:"), "10mm bins tracking fish by total length (TL)"),
                   tags$li(strong("Growth variability:"), "Individual variation in growth using mechanistic von Bertalanffy with coefficient of variation (CV)"),
                   tags$li(strong("Growth transition matrix:"), "Probabilistic movement between length bins based on growth increment and CV"),
                   tags$li(strong("Weight-length relationships:"), "Species-specific allometric relationships (W = a × L^b) from FishBase medians"),
                   tags$li(strong("Density-dependent recruitment (DDR):"), "Optional Beverton-Holt stock-recruitment with configurable steepness"),
                   tags$li(strong("Size-based selectivity:"), "Logistic vulnerability curves for capture and harvest"),
                   tags$li(strong("Regulations:"), "Traditional slot limits, protective (reverse) slots, and maximum length limits"),
                   tags$li(strong("Mortality:"), "Natural mortality (M) and configurable discard mortality"),
                   tags$li(strong("Stochastic recruitment:"), "Lognormal variability with species-specific CV"),
                   tags$li(strong("Fecundity:"), "Weight-based egg production scaled by logistic maturity ogive"),
                   tags$li(strong("MSY analysis:"), "Maximum Sustainable Yield calculated from equilibrium yield and recruitment curves")
                 ),
                 br(),
                 h4("Key Outputs"),
                 tags$ul(
                   tags$li(strong("YPR:"), "Yield Per Recruit (kg) - harvest per individual recruit"),
                   tags$li(strong("Total Yield:"), "Population-level harvest (YPR × Recruitment)"),
                   tags$li(strong("MSY:"), "Maximum Sustainable Yield and optimal exploitation rate (U_MSY)"),
                   tags$li(strong("SPR:"), "Spawning Potential Ratio (current SSB / unfished SSB)"),
                   tags$li(strong("Equilibrium Recruitment:"), "Recruits at equilibrium under DDR"),
                   tags$li(strong("Population Structure:"), "Age and length distributions with 95% prediction intervals"),
                   tags$li(strong("Prop Memorable:"), "Proportion of trophy/quality-sized fish in population")
                 ),
                 br(),
                 h4("References"),
                 p(strong("Primary citation:")),
                 p("Smith, D.R., Bennett, D.L., Norman, J.D., Allen, M.S. 2025. Live-imaging sonar use in Texas crappie fisheries: Examining population-level responses due to potential increases in exploitation. Fisheries, vuae015. ",
                   a(href = "https://doi.org/10.1093/fshmag/vuae015", "https://doi.org/10.1093/fshmag/vuae015")),
                 br(),
                 p(strong("Parameter sources:")),
                 p("Froese, R. and D. Pauly. Editors. 2024. FishBase. World Wide Web electronic publication. ",
                   a(href = "https://www.fishbase.org", "www.fishbase.org")),
                 p("Gabelhouse, D.W., Jr. 1984. A length-categorization system to assess fish stocks. North American Journal of Fisheries Management 4:273-285.")
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
    if (input$species == "white_crappie") {
      updateNumericInput(session, "wl_a", value = 2.40991e-6)
      updateNumericInput(session, "wl_b", value = 3.38)
      updateNumericInput(session, "mat_size", value = 180)  # ~7 inches (literature: 6-7" typical)
      updateNumericInput(session, "memorable_size", value = 305)  # 12 inches
      updateNumericInput(session, "linf", value = 353)  # LIS paper
      updateNumericInput(session, "vbk", value = 0.374)  # LIS paper
      updateNumericInput(session, "t0", value = 0.197)  # LIS paper
      updateNumericInput(session, "nat_mort", value = 0.374)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.8)  # High recruitment variability
      updateNumericInput(session, "amax", value = 8)  # Typical crappie maximum age    
      updateNumericInput(session, "ymax", value = 8 + 20 + 100)
      
      showNotification("Loaded White Crappie parameters (Smith et al. 2025)", type = "message")
      
    } else if (input$species == "black_crappie") {
      updateNumericInput(session, "wl_a", value = 1.10e-5)  # FishBase median
      updateNumericInput(session, "wl_b", value = 3.07)
      updateNumericInput(session, "mat_size", value = 180)  # ~7 inches (literature: 6-7" typical)
      updateNumericInput(session, "memorable_size", value = 305)  # 12 inches
      updateNumericInput(session, "linf", value = 381)  # FishBase Q2 (moderate growth)
      updateNumericInput(session, "vbk", value = 0.19)  # FishBase Q2
      updateNumericInput(session, "t0", value = 0.34)  # FishBase (single value)
      updateNumericInput(session, "nat_mort", value = 0.19)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.8)  # High recruitment variability
      updateNumericInput(session, "amax", value = 8)  # Typical crappie maximum age
      updateNumericInput(session, "ymax", value = 8 + 20 + 100)
      
      showNotification("Loaded Black Crappie parameters (FishBase median)", type = "message")
      
    } else if (input$species == "walleye") {
      # Weight-length from FishBase median
      updateNumericInput(session, "wl_a", value = 6.63e-6)  # FishBase median
      updateNumericInput(session, "wl_b", value = 3.10)
      updateNumericInput(session, "mat_size", value = 356)  # 14 inches (female maturity lower range, literature)
      updateNumericInput(session, "memorable_size", value = 635)  # 25 inches (Gabelhouse 1984)
      updateNumericInput(session, "harvlim", value = 356)  # 14 inches minimum harvest size
      updateNumericInput(session, "linf", value = 683)  # Craig et al. 1995
      updateNumericInput(session, "vbk", value = 0.32)  # Craig et al. 1995
      updateNumericInput(session, "t0", value = -0.52)  # Craig et al. 1995
      updateNumericInput(session, "nat_mort", value = 0.32)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 1.1)  # Very high recruitment variability (literature: CV=112%)
      updateNumericInput(session, "amax", value = 15)  # Walleye can live 15-20 years
      updateNumericInput(session, "ymax", value = 15 + 20 + 100)
      showNotification("Loaded Walleye parameters (FishBase median)", type = "message")
      
    } else if (input$species == "lmb") {
      # Weight-length from FishBase median
      updateNumericInput(session, "wl_a", value = 8.16e-6)  # FishBase median
      updateNumericInput(session, "wl_b", value = 3.10)
      updateNumericInput(session, "mat_size", value = 203)  # 8 inches (female maturity, literature)
      updateNumericInput(session, "memorable_size", value = 508)  # 20 inches (Gabelhouse 1984)
      updateNumericInput(session, "harvlim", value = 305)  # 12 inches minimum harvest size
      updateNumericInput(session, "linf", value = 584)  # Average of male/female
      updateNumericInput(session, "vbk", value = 0.22)  # Average of male/female
      updateNumericInput(session, "t0", value = 0)
      updateNumericInput(session, "nat_mort", value = 0.22)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.5)  # Moderate-high recruitment variability (literature: CV>0.5)
      updateNumericInput(session, "amax", value = 12)  # Largemouth bass maximum age
      updateNumericInput(session, "ymax", value = 12 + 20 + 100)
      showNotification("Loaded Largemouth Bass parameters (FishBase median)", type = "message")
      
    } else if (input$species == "smb") {
      # Weight-length from FishBase median
      updateNumericInput(session, "wl_a", value = 1.09e-5)  # FishBase median
      updateNumericInput(session, "wl_b", value = 3.08)
      updateNumericInput(session, "mat_size", value = 254)  # 10 inches (female first spawn lower range, literature)
      updateNumericInput(session, "memorable_size", value = 432)  # 17 inches (Gabelhouse 1984)
      updateNumericInput(session, "harvlim", value = 305)  # 12 inches minimum harvest size
      updateNumericInput(session, "linf", value = 525)
      updateNumericInput(session, "vbk", value = 0.17)
      updateNumericInput(session, "t0", value = -0.33)
      updateNumericInput(session, "nat_mort", value = 0.17)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.7)  # Moderate-high recruitment variability (literature: CV=52-80%)
      updateNumericInput(session, "amax", value = 12)  # Smallmouth bass maximum age
      updateNumericInput(session, "ymax", value = 12 + 20 + 100)
      showNotification("Loaded Smallmouth Bass parameters (FishBase median)", type = "message")
      
    } else if (input$species == "channel_catfish") {
      # Channel catfish parameters (FishBase median / Gabelhouse 1984)
      updateNumericInput(session, "wl_a", value = 1.66e-6)  # FishBase median
      updateNumericInput(session, "wl_b", value = 3.30)
      updateNumericInput(session, "mat_size", value = 356)  # 14 inches (literature: female maturity)
      updateNumericInput(session, "memorable_size", value = 711)  # 28 inches (Gabelhouse 1984)
      updateNumericInput(session, "harvlim", value = 305)  # 12 inches minimum harvest size
      updateNumericInput(session, "linf", value = 592)  # Moderate growth
      updateNumericInput(session, "vbk", value = 0.17)
      updateNumericInput(session, "t0", value = -0.62)
      updateNumericInput(session, "nat_mort", value = 0.17)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.4)  # Moderate recruitment variability
      updateNumericInput(session, "amax", value = 24)  # Channel catfish can live 20-24 years
      updateNumericInput(session, "ymax", value = 24 + 20 + 100)
      showNotification("Loaded Channel Catfish parameters (FishBase median)", type = "message")
      
    } else if (input$species == "blue_catfish") {
      # Blue catfish parameters (FishBase median / Gabelhouse 1984)
      updateNumericInput(session, "wl_a", value = 7.74e-7)  # FishBase median
      updateNumericInput(session, "wl_b", value = 3.41)
      updateNumericInput(session, "mat_size", value = 350)  # ~14 inches (female maturity lower range, literature: 35-50cm)
      updateNumericInput(session, "memorable_size", value = 889)  # 35 inches (Gabelhouse 1984)
      updateNumericInput(session, "harvlim", value = 305)  # 12 inches minimum harvest size
      updateNumericInput(session, "linf", value = 1300)  # Moderate growth (larger species)
      updateNumericInput(session, "vbk", value = 0.079)
      updateNumericInput(session, "t0", value = -1.3)
      updateNumericInput(session, "nat_mort", value = 0.15)  # M = K (default)
      updateNumericInput(session, "rec_cv", value = 0.5)  # Moderate-high recruitment variability (literature: σR=0.49, Hilling et al. 2025)
      updateNumericInput(session, "amax", value = 30)  # Blue catfish can live 25-30 years
      updateNumericInput(session, "ymax", value = 30 + 20 + 100)
      showNotification("Loaded Blue Catfish parameters (FishBase median)", type = "message")
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
    
    # White Crappie growth parameters (from LIS paper)
    if (input$species == "white_crappie") {
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
    # Black Crappie growth parameters (FishBase quartile-based)
    else if (input$species == "black_crappie") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 440)  # Q3 Linf
        updateNumericInput(session, "vbk", value = 0.17)  # Q1 K
        updateNumericInput(session, "t0", value = 0.34)  # Single value (limited data)
        updateNumericInput(session, "nat_mort", value = 0.17)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 381)  # Q2 Linf
        updateNumericInput(session, "vbk", value = 0.19)  # Q2 K
        updateNumericInput(session, "t0", value = 0.34)  # Single value (limited data)
        updateNumericInput(session, "nat_mort", value = 0.19)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 356)  # Q1 Linf
        updateNumericInput(session, "vbk", value = 0.26)  # Q3 K
        updateNumericInput(session, "t0", value = 0.34)  # Single value (limited data)
        updateNumericInput(session, "nat_mort", value = 0.26)  # M = K
      }
    }
    # Walleye growth parameters (FishBase quartile-based)
    else if (input$species == "walleye") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 748)  # Q3 Linf
        updateNumericInput(session, "vbk", value = 0.24)  # Q1 K
        updateNumericInput(session, "t0", value = -0.66)  # Q1 t0
        updateNumericInput(session, "nat_mort", value = 0.24)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 683)  # Q2 Linf
        updateNumericInput(session, "vbk", value = 0.32)  # Q2 K
        updateNumericInput(session, "t0", value = -0.52)  # Q2 t0
        updateNumericInput(session, "nat_mort", value = 0.32)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 615)  # Q1 Linf
        updateNumericInput(session, "vbk", value = 0.43)  # Q3 K
        updateNumericInput(session, "t0", value = -0.20)  # Q3 t0
        updateNumericInput(session, "nat_mort", value = 0.43)  # M = K
      }
    }
    # Largemouth Bass growth parameters (FishBase quartile-based)
    else if (input$species == "lmb") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 638)  # Q3 Linf
        updateNumericInput(session, "vbk", value = 0.17)  # Q1 K
        updateNumericInput(session, "t0", value = -0.21)  # Q1 t0
        updateNumericInput(session, "nat_mort", value = 0.17)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 584)  # Q2 Linf
        updateNumericInput(session, "vbk", value = 0.22)  # Q2 K
        updateNumericInput(session, "t0", value = 0.00)  # Q2 t0
        updateNumericInput(session, "nat_mort", value = 0.22)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 540)  # Q1 Linf
        updateNumericInput(session, "vbk", value = 0.28)  # Q3 K
        updateNumericInput(session, "t0", value = 0.10)  # Q3 t0
        updateNumericInput(session, "nat_mort", value = 0.28)  # M = K
      }
    }
    # Smallmouth Bass growth parameters (FishBase quartile-based)
    else if (input$species == "smb") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 608)  # Q3 Linf
        updateNumericInput(session, "vbk", value = 0.14)  # Q1 K
        updateNumericInput(session, "t0", value = -0.45)  # Q1 t0
        updateNumericInput(session, "nat_mort", value = 0.14)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 525)  # Q2 Linf
        updateNumericInput(session, "vbk", value = 0.17)  # Q2 K
        updateNumericInput(session, "t0", value = -0.33)  # Q2 t0
        updateNumericInput(session, "nat_mort", value = 0.17)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 506)  # Q1 Linf
        updateNumericInput(session, "vbk", value = 0.22)  # Q3 K
        updateNumericInput(session, "t0", value = 0.02)  # Q3 t0
        updateNumericInput(session, "nat_mort", value = 0.22)  # M = K
      }
    }
    # Channel Catfish growth parameters (FishBase quartile-based)
    else if (input$species == "channel_catfish") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 797)  # Q3 Linf
        updateNumericInput(session, "vbk", value = 0.12)  # Q1 K
        updateNumericInput(session, "t0", value = -0.82)  # Q1 t0
        updateNumericInput(session, "nat_mort", value = 0.12)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 592)  # Q2 Linf
        updateNumericInput(session, "vbk", value = 0.17)  # Q2 K
        updateNumericInput(session, "t0", value = -0.62)  # Q2 t0
        updateNumericInput(session, "nat_mort", value = 0.17)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 470)  # Q1 Linf
        updateNumericInput(session, "vbk", value = 0.23)  # Q3 K
        updateNumericInput(session, "t0", value = -0.20)  # Q3 t0
        updateNumericInput(session, "nat_mort", value = 0.23)  # M = K
      }
    }
    # Blue Catfish growth parameters (FishBase quartile-based)
    else if (input$species == "blue_catfish") {
      if (input$growth_preset == "slow") {
        updateNumericInput(session, "linf", value = 1396)  # Q3 Linf
        updateNumericInput(session, "vbk", value = 0.051)  # Q1 K
        updateNumericInput(session, "t0", value = -1.52)  # Q1 t0
        updateNumericInput(session, "nat_mort", value = 0.051)  # M = K
      } else if (input$growth_preset == "moderate") {
        updateNumericInput(session, "linf", value = 1300)  # Q2 Linf
        updateNumericInput(session, "vbk", value = 0.079)  # Q2 K
        updateNumericInput(session, "t0", value = -1.30)  # Q2 t0
        updateNumericInput(session, "nat_mort", value = 0.079)  # M = K
      } else if (input$growth_preset == "fast") {
        updateNumericInput(session, "linf", value = 1060)  # Q1 Linf
        updateNumericInput(session, "vbk", value = 0.095)  # Q3 K
        updateNumericInput(session, "t0", value = -1.01)  # Q3 t0
        updateNumericInput(session, "nat_mort", value = 0.095)  # M = K
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

      # Build static components once (replaces all manual calculations)
      static <- build_static_components_from_input(input)

      # Run population simulation using modular engine
      sim_out <- run_population_simulation(
        U = input$exploitation,
        input = input,
        static = static,
        nsim = input$nsim,
        store_details = TRUE,
        progress_cb = function(p, detail = NULL) incProgress(p, detail = detail)
      )

      # Store results in reactive values
      sim_results(sim_out$results)
      time_series_data(sim_out$time_series)
      pop_structure_data(sim_out$pop_structure)
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
    
    # Convert mean length harvested to inches for display
    mean_length_mm <- mean(results$MeanLengthHarvested, na.rm = TRUE)
    mean_length_inches <- mean_length_mm / 25.4
    sd_length_mm <- sd(results$MeanLengthHarvested, na.rm = TRUE)
    sd_length_inches <- sd_length_mm / 25.4
    cat(sprintf("  Mean Length Harvested: %.1f\" (%.0f mm) ± %.1f\" (%.0f mm)\n",
                mean_length_inches, mean_length_mm,
                sd_length_inches, sd_length_mm))
    
    # Management warning if SPR < 0.3
    mean_spr <- mean(results$SPR, na.rm = TRUE)
    if(mean_spr < 0.3) {
      cat("\n")
      cat("  ⚠️  WARNING: SPR < 0.3 (Overfishing threshold)\n")
      cat("  Population may be experiencing recruitment overfishing.\n")
      cat("  Consider reducing exploitation or implementing protective regulations.\n")
    }
    
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

    # Get burn-in years from attribute (set by modular engine)
    burnin_years_plot <- attr(ts_data, "burn_in_years")
    if(is.null(burnin_years_plot)) burnin_years_plot <- 20
    
    # Create separate plots for each metric with ribbons
    # Add shaded region for unfished burn-in period
    p1 <- ggplot(ts_data, aes(x = Year, y = YPR_mean)) +
      annotate("rect", xmin = 1, xmax = burnin_years_plot, ymin = -Inf, ymax = Inf,
               fill = "gray", alpha = 0.2) +
      geom_ribbon(aes(ymin = YPR_lower, ymax = YPR_upper),
                  alpha = 0.2, fill = "steelblue") +
      geom_line(color = "steelblue", size = 1) +
      geom_vline(xintercept = burnin_years_plot, linetype = "dotted", color = "gray40", alpha = 0.7) +
      labs(title = "YPR Over Time (Mean ± 95% Prediction Interval)",
           subtitle = "Gray shaded area: unfished burn-in period",
           x = "", y = "YPR (kg)") +
      theme_minimal()
    
    p2 <- ggplot(ts_data, aes(x = Year, y = SPR_mean)) +
      annotate("rect", xmin = 1, xmax = burnin_years_plot, ymin = -Inf, ymax = Inf,
               fill = "gray", alpha = 0.2) +
      geom_ribbon(aes(ymin = SPR_lower, ymax = SPR_upper),
                  alpha = 0.2, fill = "darkgreen") +
      geom_line(color = "darkgreen", size = 1) +
      geom_vline(xintercept = burnin_years_plot, linetype = "dotted", color = "gray40", alpha = 0.7) +
      geom_hline(yintercept = 0.40, linetype = "dashed", color = "orange", alpha = 0.7) +
      geom_hline(yintercept = 0.30, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = "SPR Over Time (Mean ± 95% Prediction Interval)",
           subtitle = "Dashed lines: 40% (sustainable), 30% (overfished) | Gray: unfished burn-in",
           x = "", y = "SPR") +
      theme_minimal()
    
    memorable_inches <- round(input$memorable_size / 25.4, 1)
    p3 <- ggplot(ts_data, aes(x = Year, y = Prop_mean)) +
      annotate("rect", xmin = 1, xmax = burnin_years_plot, ymin = -Inf, ymax = Inf,
               fill = "gray", alpha = 0.2) +
      geom_ribbon(aes(ymin = Prop_lower, ymax = Prop_upper),
                  alpha = 0.2, fill = "darkorange") +
      geom_line(color = "darkorange", size = 1) +
      geom_vline(xintercept = burnin_years_plot, linetype = "dotted", color = "gray40", alpha = 0.7) +
      labs(title = paste0("Proportion Memorable (≥", memorable_inches, "\") Over Time"),
           subtitle = "Mean ± 95% prediction interval | Gray: unfished burn-in",
           x = "", y = "Proportion") +
      theme_minimal()
    
    # Calculate 20% SSB threshold (depensation threshold) from unfished SSB
    burnin_index <- min(burnin_years_plot, nrow(ts_data))
    SSB0_approx <- ts_data$SSB_mean[burnin_index]  # Unfished equilibrium at end of burn-in
    depensation_threshold <- SSB0_approx * 0.2
    
    p4 <- ggplot(ts_data, aes(x = Year, y = SSB_mean)) +
      annotate("rect", xmin = 1, xmax = burnin_years_plot, ymin = -Inf, ymax = Inf,
               fill = "gray", alpha = 0.2) +
      geom_ribbon(aes(ymin = SSB_lower, ymax = SSB_upper),
                  alpha = 0.2, fill = "purple") +
      geom_line(color = "purple", size = 1) +
      geom_vline(xintercept = burnin_years_plot, linetype = "dotted", color = "gray40", alpha = 0.7) +
      geom_hline(yintercept = depensation_threshold, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = "Spawning Stock Biomass (SSB) Over Time",
           subtitle = "Dashed red line: 20% SSB₀ (depensation threshold) | Gray: unfished burn-in",
           x = "Year", y = "SSB") +
      theme_minimal()
    
    # Combine plots vertically
    subplot(
      ggplotly(p1),
      ggplotly(p2),
      ggplotly(p3),
      ggplotly(p4),
      nrows = 4,
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
  
  # Population structure plot (by AGE)
  output$pop_structure <- renderPlotly({
    req(pop_structure_data())
    pop_data <- pop_structure_data()
    age_data <- pop_data$age_data
    
    # Determine the max age with meaningful abundance (filter out ages with near-zero abundance)
    max_age <- max(age_data$Age[age_data$Abundance_median > 0.1], na.rm = TRUE)
    
    # Ensure all integer ages from 1 to max are represented (no age-0 fish)
    all_ages <- data.frame(Age = 1:max_age)
    age_data <- all_ages %>%
      left_join(age_data, by = "Age") %>%
      mutate(
        Abundance_median = replace_na(Abundance_median, 0),
        Abundance_lower = replace_na(Abundance_lower, 0),
        Abundance_upper = replace_na(Abundance_upper, 0)
      )
    
    p <- ggplot(age_data, aes(x = Age)) +
      geom_ribbon(aes(ymin = Abundance_lower, ymax = Abundance_upper),
                  fill = "steelblue", alpha = 0.3) +
      geom_col(aes(y = Abundance_median), fill = "steelblue", alpha = 0.7, width = 0.8) +
      geom_line(aes(y = Abundance_median), color = "darkblue", size = 1) +
      scale_x_continuous(breaks = 1:max_age, limits = c(0.5, max_age + 0.5)) +
      labs(title = "Age Distribution at Equilibrium",
           subtitle = "Bars show median abundance. Shaded area shows 95% prediction interval across simulations (median ± 1.96 × SD).",
           x = "Age (years)", y = "Abundance") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Vulnerability plot
  output$vulnerability_plot <- renderPlotly({
    req(pop_structure_data())
    pop_data <- pop_structure_data()
    length_data <- pop_data$length_data
    
    # Create separate data frames for each curve with slight offset for visibility
    capture_data <- data.frame(
      Length = length_data$Length,
      Vulnerability = length_data$VulCapture,
      Type = "VulCapture"
    )
    
    # Offset harvest curve by 2mm so it doesn't completely overlap capture curve
    harvest_data <- data.frame(
      Length = length_data$Length + 2,
      Vulnerability = length_data$VulHarvest,
      Type = "VulHarvest"
    )
    
    # Offset trophy curve by 4mm so all three curves are visible
    trophy_data <- data.frame(
      Length = length_data$Length + 4,
      Vulnerability = length_data$VulTrophy,
      Type = "VulTrophy"
    )
    
    vul_long <- rbind(capture_data, harvest_data, trophy_data)
    
    # Get the full range of length data
    length_range <- range(length_data$Length, na.rm = TRUE)
    x_max <- ceiling(length_range[2] * 1.05 / 100) * 100  # Round up to nearest 100
    
    # Get memorable size for vertical line
    memorable_mm <- input$memorable_size
    
    p <- ggplot(vul_long, aes(x = Length, y = Vulnerability, color = Type)) +
      geom_line(size = 1.2) +
      geom_vline(xintercept = memorable_mm, linetype = "dashed", color = "goldenrod", alpha = 0.6) +
      annotate("text", x = memorable_mm + 15, y = 0.5, label = "Memorable\nthreshold",
               color = "goldenrod", size = 3, hjust = 0) +
      scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max, by = 100), expand = c(0, 0)) +
      labs(title = "Vulnerability Curves by Length",
           subtitle = "Curves slightly offset for visibility. Dashed line: memorable size threshold.",
           x = "Total Length (mm)", y = "Vulnerability",
           color = "Type") +
      scale_color_manual(values = c("VulCapture" = "blue", "VulHarvest" = "red", "VulTrophy" = "goldenrod"),
                         labels = c("Capture", "Harvest", "Trophy/Memorable")) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
  })
  
  # Length-frequency histogram (now directly from length bins!)
  output$length_frequency <- renderPlotly({
    req(pop_structure_data())
    pop_data <- pop_structure_data()
    length_data <- pop_data$length_data
    
    # Get the full range of length data (same as vulnerability plot)
    length_range <- range(length_data$Length, na.rm = TRUE)
    x_max <- ceiling(length_range[2] * 1.05 / 100) * 100  # Round up to nearest 100
    
    growth_cv <- input$growth_cv
    
    # Add line and 95% prediction intervals
    p <- ggplot(length_data, aes(x = Length)) +
      geom_ribbon(aes(ymin = Abundance_lower, ymax = Abundance_upper),
                  fill = "steelblue", alpha = 0.3) +
      geom_col(aes(y = Abundance_mean), fill = "steelblue", alpha = 0.7, color = "black", width = 10) +
      geom_line(aes(y = Abundance_mean), color = "darkblue", size = 1) +
      scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max, by = 100), expand = c(0, 0)) +
      labs(title = paste0("Length-Frequency Distribution (Equilibrium) - Growth CV = ", growth_cv),
           subtitle = "Shaded area shows 95% prediction interval (mean ± 1.96 × SD)",
           x = "Total Length (mm)", y = "Abundance") +
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

      # Build static components once
      static <- build_static_components_from_input(input)

      # Run yield curve simulation using modular engine
      curve_data <- run_yield_curve_simulation(
        input = input,
        static = static,
        U_values = seq(0, 1, by = 0.1),
        nsim = input$yield_curve_nsim,
        progress_cb = function(p, detail = NULL) incProgress(p, detail = detail)
      )

      # Store yield curve results
      yield_curve_data(curve_data)
    })
  })
  
  # MSY plot (Total Yield and Recruitment)
  output$msy_plot <- renderPlotly({
    curve_data <- yield_curve_data()
    req(!is.null(curve_data))
    
    # Find MSY (maximum total yield) and corresponding U
    msy_idx <- which.max(curve_data$TotalYield_mean)
    msy_value <- curve_data$TotalYield_mean[msy_idx]
    u_msy <- curve_data$U[msy_idx] * 100
    
    # Calculate 95% prediction intervals
    curve_data$TotalYield_lower <- curve_data$TotalYield_mean - 1.96 * curve_data$TotalYield_sd
    curve_data$TotalYield_upper <- curve_data$TotalYield_mean + 1.96 * curve_data$TotalYield_sd
    curve_data$Recruit_lower <- curve_data$Recruit_mean - 1.96 * curve_data$Recruit_sd
    curve_data$Recruit_upper <- curve_data$Recruit_mean + 1.96 * curve_data$Recruit_sd
    
    # Calculate y-axis ranges to ensure zeros align
    # Both axes start at 0 and extend to max with 10% padding
    yield_max <- max(curve_data$TotalYield_upper, na.rm = TRUE) * 1.1
    recruit_max <- max(curve_data$Recruit_upper, na.rm = TRUE) * 1.1
    
    # Create dual-axis plot using plotly
    p <- plot_ly(curve_data) %>%
      # Total Yield (left y-axis)
      add_ribbons(x = ~U * 100, ymin = ~TotalYield_lower, ymax = ~TotalYield_upper,
                  fillcolor = "rgba(70, 130, 180, 0.2)", line = list(color = "transparent"),
                  showlegend = FALSE, name = "Total Yield 95% PI") %>%
      add_trace(x = ~U * 100, y = ~TotalYield_mean, type = "scatter", mode = "lines+markers",
                line = list(color = "steelblue", width = 3), marker = list(size = 6),
                name = "Total Yield (kg)", yaxis = "y1") %>%
      # MSY marker
      add_trace(x = u_msy, y = msy_value, type = "scatter", mode = "markers",
                marker = list(color = "red", size = 12, symbol = "star"),
                name = paste0("MSY = ", round(msy_value, 1), " kg at U = ", round(u_msy, 1), "%"),
                yaxis = "y1") %>%
      # Recruitment (right y-axis)
      add_ribbons(x = ~U * 100, ymin = ~Recruit_lower, ymax = ~Recruit_upper,
                  fillcolor = "rgba(34, 139, 34, 0.2)", line = list(color = "transparent"),
                  showlegend = FALSE, name = "Recruitment 95% PI", yaxis = "y2") %>%
      add_trace(x = ~U * 100, y = ~Recruit_mean, type = "scatter", mode = "lines+markers",
                line = list(color = "forestgreen", width = 3, dash = "dash"),
                marker = list(size = 6), name = "Equilibrium Recruitment", yaxis = "y2") %>%
      # Layout with dual y-axes (both starting at 0 to align zeros)
      layout(
        title = "Maximum Sustainable Yield (MSY) Analysis",
        xaxis = list(title = "Exploitation Rate (%)"),
        yaxis = list(title = "Total Yield (kg)", side = "left", showgrid = FALSE,
                     range = c(0, yield_max)),
        yaxis2 = list(title = "Equilibrium Recruitment (number)", side = "right", overlaying = "y", showgrid = FALSE,
                      range = c(0, recruit_max)),
        hovermode = "x unified",
        legend = list(x = 0.7, y = 0.95),
        margin = list(r = 100)  # Add right margin for y-axis2 title visibility
      )
    
    p
  })
  
  # Yield curve plot (YPR)
  output$yield_curve_plot <- renderPlotly({
    curve_data <- yield_curve_data()
    req(!is.null(curve_data))
    
    # Calculate 95% prediction interval: mean ± 1.96 × SD
    # Shows where 95% of population outcomes fall due to recruitment variability
    # Constrain YPR to biologically valid range (cannot be negative)
    curve_data$YPR_lower <- pmax(0, curve_data$YPR_mean - 1.96 * curve_data$YPR_sd)
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
    # Constrain SPR lower bound to 0, allow upper to exceed 1
    curve_data$SPR_lower <- pmax(0, curve_data$SPR_mean - 1.96 * curve_data$SPR_sd)
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
    # Constrain Prop to biologically valid range [0, 1]
    curve_data$Prop_lower <- pmax(0, curve_data$Prop_mean - 1.96 * curve_data$Prop_sd)
    curve_data$Prop_upper <- pmin(1, curve_data$Prop_mean + 1.96 * curve_data$Prop_sd)
    
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
