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
      
      # Get parameters
      growth_params <- get_growth_params()
      Amax <- input$amax
      Ymax <- Amax + 100 + 20
      
      # Weight-length equation (species-specific)
      alfa <- input$wl_a
      bet <- input$wl_b
      
      # Mortality
      DisMort <- input$dismort
      Nat_mort <- input$nat_mort
      
      # Stock-recruit
      Ro <- input$R0
      
      # Vulnerabilities
      Capsize <- input$capsize
      CapsizeSD <- Capsize * 0.01
      Uppercap <- 380
      UppercapSD <- Uppercap * 0.01
      Harvlim <- input$harvlim
      HarvlimSD <- Harvlim * 0.01
      
      # ========================================================================
      # LENGTH-STRUCTURED MODEL WITH GROWTH VARIABILITY
      # ========================================================================
      
      # Define length bins (10mm bins)
      bin_width <- 10
      max_length <- ceiling(growth_params$Linf * 1.2)  # 120% of Linf to be safe
      length_bins <- seq(0, max_length, by = bin_width)
      L_bins <- length(length_bins) - 1  # Number of bins
      bin_midpoints <- (length_bins[-1] + length_bins[-(L_bins+1)]) / 2
      
      # Get growth CV
      growth_cv <- input$growth_cv
      
      # Pre-compute length-specific variables for each bin
      # Weight at length
      Wt_bins <- (alfa * bin_midpoints^bet) / 1000
      
      # Fecundity with logistic maturity ogive
      Wmat <- (alfa * input$mat_size^bet) / 1000
      maturity_ogive_bins <- 1 / (1 + exp(-(Wt_bins - Wmat) / (Wmat * 0.1)))
      # ================================
      # SPECIES-SPECIFIC FECUNDITY
      # ================================
      # Default exponent for ALL species = 1.18 (Barneche et al. 2018)
      fec_exp <- 1.18
      
      # Crappie (white or black) use species-specific exponent = 1.27
      if (input$species %in% c("white_crappie", "black_crappie")) {
        fec_exp <- 1.27
      }
      
      # Apply exponent to weight
      Fec_bins <- (Wt_bins ^ fec_exp) * maturity_ogive_bins      
      # Capture vulnerability by length
      Vulcap_bins <- 1 / (1 + exp(-(bin_midpoints - Capsize) / CapsizeSD))
      
      # Harvest vulnerability by length (depends on regulation type)
      if(input$enable_slot) {
        Slot_upper <- input$slot_upper
        Slot_upperSD <- 0.01
        HarvlimSD_slot <- 0.01
        Effective_min <- max(Harvlim, Capsize)
        
        Vulharv_above_min <- 1 / (1 + exp(-(bin_midpoints - Effective_min) / HarvlimSD_slot))
        Vulharv_below_max <- 1 / (1 + exp((bin_midpoints - Slot_upper) / Slot_upperSD))
        
        if(input$slot_type == "traditional") {
          Vulharv_bins <- Vulharv_above_min * Vulharv_below_max
        } else {
          Vulharv_bins <- (1 - (Vulharv_above_min * Vulharv_below_max)) * Vulcap_bins
        }
      } else if(input$enable_max_limit) {
        Max_harvest_size <- input$max_harvest_size
        Max_harvestSD <- 0.01
        Vulharv_above_capture <- 1 / (1 + exp(-(bin_midpoints - Capsize) / CapsizeSD))
        Vulharv_below_max <- 1 / (1 + exp((bin_midpoints - Max_harvest_size) / Max_harvestSD))
        Vulharv_bins <- Vulharv_above_capture * Vulharv_below_max
      } else {
        Vulharv_bins <- 1 / (1 + exp(-(bin_midpoints - Harvlim) / HarvlimSD))
      }
      
      # Trophy vulnerability by length
      trophyvul_bins <- (1 / (1 + exp(-(bin_midpoints - input$memorable_size) / (input$memorable_size * 0.1)))) * Vulcap_bins
      
      # Get exploitation rate
      U <- input$exploitation
      
      # Size-dependent natural mortality
      M_adult <- Nat_mort
      M_bins <- rep(M_adult, L_bins)
      
      # Juveniles have higher mortality
      juvenile_threshold <- input$mat_size * 0.5
      M_bins[bin_midpoints < juvenile_threshold] <- M_adult * 2.0
      M_bins[bin_midpoints >= juvenile_threshold & bin_midpoints < input$mat_size] <- M_adult * 1.5
      
      # Annual survival by length bin (natural mortality only)
      S_bins <- exp(-M_bins)
      
      # UNFISHED survival by length bin (natural mortality only, NO fishing)
      # This is used for building unfished equilibrium and SPR denominator
      Unfished_survival_bins <- S_bins
      
      # FISHED survival by length bin (includes fishing mortality)
      # Fishing mortality by length bin
      F_bins <- Vulharv_bins * U
      # Release mortality from discarded fish
      Release_mort_bins <- (Vulcap_bins - Vulharv_bins) * U * DisMort
      # Total annual survival by length bin WITH FISHING
      Survival_bins <- S_bins * (1 - F_bins) * (1 - Release_mort_bins)
      
      # ========================================================================
      # GROWTH TRANSITION MATRIX
      # ========================================================================
      # Create matrix: Growth_matrix[i, j] = prob of moving from bin i to bin j in one year
      
      Growth_matrix <- matrix(0, nrow = L_bins, ncol = L_bins)
      
      for(i in 1:L_bins) {
        current_length <- bin_midpoints[i]
        
        K <- growth_params$vbk
        Linf <- growth_params$Linf
        
        # Growth increment (mechanistic von Bertalanffy for discrete annual time step)
        # Proper VB increment: ΔL = (L∞ - L_t) * (1 - exp(-K))
        # This gives: L_{t+1} = L∞ * (1 - exp(-K)) + L_t * exp(-K)
        growth_increment <- (Linf - current_length) * (1 - exp(-K))
        
        # Ensure positive growth, even for fish at/above Linf
        growth_increment <- max(0.1, growth_increment)
        
        expected_length <- current_length + growth_increment
        
        # === deterministic case when growth_cv == 0 (age cohort approach) ===
        if(growth_cv == 0) {
          next_bin <- which.min(abs(bin_midpoints - expected_length))
          
          Growth_matrix[i, ] <- 0
          Growth_matrix[i, next_bin] <- 1
          next  # skip to next i (next length bin)
        }
        
        # === stochastic case (normal distribution) ===
        # Add variability: SD = growth_increment * CV
        # Set minimum SD to avoid pnorm() issues and ensure proper scaling with bin width
        # Ensures variability scales appropriately for both small (crappie) and large (catfish) species
        growth_sd <- max(1, growth_increment * growth_cv, bin_width * 0.15)
        
        # For fish at or above Linf, minimal growth with small SD
        if(current_length >= Linf * 0.99) {
          growth_increment <- 0.1
          growth_sd <- max(1, bin_width * 0.15)
          expected_length <- current_length + growth_increment
        }
        
        # Distribute probability across bins
        for(j in 1:L_bins) {
          bin_lower <- length_bins[j]
          bin_upper <- length_bins[j+1]
          
          # Probability of being in bin j next year
          prob <- pnorm(bin_upper, expected_length, growth_sd) - pnorm(bin_lower, expected_length, growth_sd)
          
          Growth_matrix[i, j] <- prob
        }
        
        # Normalize row to sum to 1 (handle edge effects)
        row_sum <- sum(Growth_matrix[i, ])
        if(row_sum > 0) {
          Growth_matrix[i, ] <- Growth_matrix[i, ] / row_sum
        } else {
          # If no growth possible (at max bin), stay in current bin
          Growth_matrix[i, i] <- 1.0
        }
      }
      
      # Convert CV to lognormal sigma (used in each simulation)
      sigmaR <- sqrt(log(input$rec_cv^2 + 1))
      
      # Run simulations
      nsim <- input$nsim
      results <- data.frame(
        sim = 1:nsim,
        YPR = rep(NA, nsim),
        SPR = rep(NA, nsim),
        Prop = rep(NA, nsim),
        MeanLengthHarvested = rep(NA, nsim)  # Mean length of harvested fish
      )
      
      # Store ALL time series data from all simulations
      all_YPR <- matrix(NA, Ymax, nsim)
      all_SPR <- matrix(NA, Ymax, nsim)
      all_Prop <- matrix(NA, Ymax, nsim)
      all_SSB <- matrix(NA, Ymax, nsim)  # Store SSB time series
      all_Abundance <- matrix(NA, L_bins, nsim)  # Store population length structure from all sims
      all_AgeAbundance <- matrix(NA, Amax, nsim) # Store population age structure from all sims
      
      # Calculate mean recruitment length (age-1) and its distribution
      age1_mean_length <- growth_params$Linf * (1 - exp(-growth_params$vbk * (1 - growth_params$t0)))
      
      # Create recruitment length distribution (which bins do age-1 fish go into?)
      recruit_dist <- rep(0, L_bins)
      
      # === deterministic case when growth_cv == 0 (age cohort approach) ===
      if(growth_cv == 0) {
        closest_bin <- which.min(abs(bin_midpoints - age1_mean_length))
        recruit_dist[closest_bin] <- 1.0
      } else {
        # === stochastic case (normal distribution) ===
        age1_sd_length <- max(0.5, age1_mean_length * growth_cv)  # Minimum SD to avoid issues
        
        for(j in 1:L_bins) {
          bin_lower <- length_bins[j]
          bin_upper <- length_bins[j+1]
          prob <- pnorm(bin_upper, age1_mean_length, age1_sd_length) - pnorm(bin_lower, age1_mean_length, age1_sd_length)
          recruit_dist[j] <- max(0, prob)  # Ensure non-negative
        }
        # Normalize and handle edge case of all zeros
        if(sum(recruit_dist) > 0) {
          recruit_dist <- recruit_dist / sum(recruit_dist)
        } else {
          # Fallback: put all recruitment in the bin closest to age1_mean_length
          closest_bin <- which.min(abs(bin_midpoints - age1_mean_length))
          recruit_dist[closest_bin] <- 1.0
        }
      }
      
      burnin_years <- min(Ymax, input$amax + 20)
      
      for(k in 1:nsim) {
        
        incProgress(1/nsim, detail = paste("Simulation", k, "of", nsim))
        
        # Initialize matrices for this simulation (LENGTH-STRUCTURED)
        N <- matrix(0, Ymax, L_bins)  # Abundance by year and length bin
        age_len <- matrix(0, Amax, L_bins)  # Track cohorts by age across length bins
        Yield <- rep(NA, Ymax)
        SPRt <- rep(NA, Ymax)
        YPR <- rep(NA, Ymax)
        Prop <- rep(NA, Ymax)
        SSBt <- rep(NA, Ymax)  # Spawning stock biomass time series
        
        # Set initial population structure (UNFISHED equilibrium in length bins)
        # Start with recruitment distributed across length bins
        age_len[1, ] <- Ro * recruit_dist
        N[1, ] <- colSums(age_len)
        
        # Track SSB during burn-in for SPR baseline
        SSB_burnin <- rep(NA, burnin_years)
        SSB_burnin[1] <- sum(N[1, ] * Fec_bins)
        
        # Build UNFISHED equilibrium over the burn-in window (establish baseline for SPR)
        for(init_year in 2:burnin_years) {
          # Apply UNFISHED survival (natural mortality only, NO fishing) by age
          age_survive <- age_len * matrix(Unfished_survival_bins, nrow = Amax, ncol = L_bins, byrow = TRUE)
          
          # Apply growth (move to new length bins) for each age, then increment age
          new_age_len <- matrix(0, Amax, L_bins)
          for(a in 1:(Amax - 1)) {
            grown <- as.vector(age_survive[a, ] %*% Growth_matrix)
            new_age_len[a + 1, ] <- grown
          }
          
          # Add stochastic recruitment (unfished populations still have recruitment variability)
          new_age_len[1, ] <- new_age_len[1, ] + (Ro * rlnorm(1, 0, sd = sigmaR)) * recruit_dist
          
          age_len <- new_age_len
          N[init_year, ] <- colSums(age_len)
          
          # Track SSB for this burn-in year
          SSB_burnin[init_year] <- sum(N[init_year, ] * Fec_bins)
        }
        
        burnin_start <- max(1, burnin_years - 9)
        SPR_denom <- mean(SSB_burnin[burnin_start:burnin_years], na.rm = TRUE)
        
        # Compute unfished SSB0 for DDR (from unfished equilibrium)
        SSB0 <- SPR_denom
        
        # Generate stochastic recruitment (or calculate from SSB if DDR enabled)
        if(isTRUE(input$enable_ddr)) {
          # Will be calculated dynamically inside loop based on SSB
          Rcapacity <- rep(NA, Ymax)
        } else {
          # Traditional per-recruit: constant mean recruitment
          if(input$rec_cv == 0) {
            # Deterministic recruitment when CV = 0
            Rcapacity <- rep(Ro, Ymax)
          } else {
            # Stochastic recruitment with lognormal noise
            Rcapacity <- Ro * rlnorm(Ymax, 0, sd = sigmaR)
          }
        }
        
        # Get steepness if DDR is enabled
        h <- ifelse(isTRUE(input$enable_ddr), input$steepness, 0.7)
        
        # Calculate metrics for unfished burn-in period
        for(yr in 1:burnin_years) {
          Yield[yr] <- 0  # No fishing during burn-in
          SSBt[yr] <- sum(N[yr, ] * Fec_bins)  # Track SSB during burn-in
          SPRt[yr] <- SSBt[yr] / SPR_denom  # Build toward equilibrium
          YPR[yr] <- 0
          Prop[yr] <- sum(trophyvul_bins * N[yr, ]) / max(1, sum(N[yr, ]))
        }
        
        # Main simulation loop with FISHING (starts after burn-in)
        start_year <- min(burnin_years + 1, Ymax)
        for(i in start_year:Ymax) {
          # If DDR enabled, calculate recruitment from previous year's SSB
          if(isTRUE(input$enable_ddr)) {
            # Calculate spawning stock biomass from PREVIOUS year
            SSB_t <- sum(N[i-1, ] * Fec_bins)
            
            # Prevent negative SSB
            SSB_t <- max(0, SSB_t)
            
            # Beverton-Holt recruitment with steepness parameterization
            # R = (4*h*R0*SSB) / (SSB0*(1-h) + (5*h-1)*SSB)
            R_BH <- (4 * h * Ro * SSB_t) / (SSB0 * (1 - h) + (5 * h - 1) * SSB_t)
            
            # Ensure positive recruitment, minimum 1 recruit
            R_BH <- max(1, R_BH)
            
            # Apply depensation (Allee effects) if enabled
            if(isTRUE(input$enable_depensation) && SSB_t < 0.2 * SSB0) {
              depensation_factor <- (SSB_t / (0.2 * SSB0))^2  # Quadratic penalty
              R_BH <- R_BH * depensation_factor
            }
            
            # Add stochastic noise (or deterministic if rec_cv = 0)
            if(input$rec_cv == 0) {
              Rcapacity[i] <- max(1, R_BH)
            } else {
              Rcapacity[i] <- max(1, R_BH * rlnorm(1, 0, sd = sigmaR))
            }
          }
          
          # Apply survival to previous year's population by age
          age_survive <- age_len * matrix(Survival_bins, nrow = Amax, ncol = L_bins, byrow = TRUE)
          
          # Apply growth (transition to new length bins) and age the cohorts
          new_age_len <- matrix(0, Amax, L_bins)
          for(a in 1:(Amax - 1)) {
            grown <- as.vector(age_survive[a, ] %*% Growth_matrix)
            new_age_len[a + 1, ] <- grown
          }
          
          # Add stochastic recruitment distributed across length bins to age-1
          new_age_len[1, ] <- new_age_len[1, ] + Rcapacity[i] * recruit_dist
          
          age_len <- new_age_len
          N[i, ] <- colSums(age_len)
          
          # Calculate annual metrics
          total_recruits <- Rcapacity[i]  # Now correctly using actual recruitment
          
          Yield[i] <- sum(Wt_bins * Vulharv_bins * N[i, ]) * U
          SSBt[i] <- sum(N[i, ] * Fec_bins)  # Spawning stock biomass
          SPRt[i] <- SSBt[i] / SPR_denom
          # YPR = Yield / Recruitment. With DDR, this has a less clean interpretation than
          # traditional per-recruit models because: (1) recruitment depends on SSB, and
          # (2) yield depends on length structure. Still valid, but represents equilibrium
          # yield per recruit at the current SSB level, not a constant biological parameter.
          YPR[i] <- Yield[i] / max(1, Rcapacity[i])  # Prevent division by zero
          Prop[i] <- sum(trophyvul_bins * N[i, ]) / max(1, sum(N[i, ]))  # Prevent division by zero
        }
        
        # Store results (last 50 years of fished equilibrium)
        last_50_start <- max(start_year, Ymax - 49)  # Ensure we don't include unfished burn-in
        SPRout <- SPRt[last_50_start:Ymax]
        results$SPR[k] <- mean(SPRout, na.rm = TRUE)
        
        YPRout <- YPR[last_50_start:Ymax]
        results$YPR[k] <- mean(YPRout, na.rm = TRUE)
        
        Propout <- Prop[last_50_start:Ymax]
        results$Prop[k] <- mean(Propout, na.rm = TRUE)
        
        # Calculate mean length of harvested fish (weighted average over last 50 years)
        harvest_lengths <- numeric(length(last_50_start:Ymax))
        for(yr_idx in seq_along(last_50_start:Ymax)) {
          yr <- last_50_start + yr_idx - 1
          harvest_by_bin <- N[yr, ] * Vulharv_bins * U
          total_harvest <- sum(harvest_by_bin)
          if(total_harvest > 0) {
            harvest_lengths[yr_idx] <- sum(harvest_by_bin * bin_midpoints) / total_harvest
          } else {
            harvest_lengths[yr_idx] <- NA
          }
        }
        results$MeanLengthHarvested[k] <- mean(harvest_lengths, na.rm = TRUE)
        
        # Store time series from this simulation
        all_YPR[, k] <- YPR
        all_SPR[, k] <- SPRt
        all_Prop[, k] <- Prop
        all_SSB[, k] <- SSBt
        
        # Store final year abundance from this simulation
        all_Abundance[, k] <- N[Ymax, ]
        all_AgeAbundance[, k] <- rowSums(age_len)
      }
      
      # Calculate mean and SD across all simulations at each year
      ts_data <- data.frame(
        Year = 1:Ymax,
        YPR_mean = rowMeans(all_YPR, na.rm = TRUE),
        YPR_sd = apply(all_YPR, 1, sd, na.rm = TRUE),
        SPR_mean = rowMeans(all_SPR, na.rm = TRUE),
        SPR_sd = apply(all_SPR, 1, sd, na.rm = TRUE),
        Prop_mean = rowMeans(all_Prop, na.rm = TRUE),
        Prop_sd = apply(all_Prop, 1, sd, na.rm = TRUE),
        SSB_mean = rowMeans(all_SSB, na.rm = TRUE),
        SSB_sd = apply(all_SSB, 1, sd, na.rm = TRUE)
      )
      
      ts_data$burnin_years <- burnin_years
      
      # Calculate 95% prediction intervals: mean ± 1.96 × SD
      # Constrain to biologically valid ranges
      ts_data$YPR_lower <- pmax(0, ts_data$YPR_mean - 1.96 * ts_data$YPR_sd)
      ts_data$YPR_upper <- ts_data$YPR_mean + 1.96 * ts_data$YPR_sd
      ts_data$SPR_lower <- pmax(0, ts_data$SPR_mean - 1.96 * ts_data$SPR_sd)
      ts_data$SPR_upper <- ts_data$SPR_mean + 1.96 * ts_data$SPR_sd  # Allow > 1 during burn-in
      ts_data$Prop_lower <- pmax(0, ts_data$Prop_mean - 1.96 * ts_data$Prop_sd)
      ts_data$Prop_upper <- pmin(1, ts_data$Prop_mean + 1.96 * ts_data$Prop_sd)
      ts_data$SSB_lower <- pmax(0, ts_data$SSB_mean - 1.96 * ts_data$SSB_sd)
      ts_data$SSB_upper <- ts_data$SSB_mean + 1.96 * ts_data$SSB_sd
      
      time_series_data(ts_data)
      
      # Calculate mean, median, and 95% prediction intervals for population structure
      # LENGTH distribution
      length_data <- data.frame(
        Length = bin_midpoints,
        Weight = Wt_bins,
        Abundance_mean = rowMeans(all_Abundance, na.rm = TRUE),
        Abundance_median = apply(all_Abundance, 1, median, na.rm = TRUE),
        Abundance_sd = apply(all_Abundance, 1, sd, na.rm = TRUE),
        Abundance_q25 = apply(all_Abundance, 1, quantile, probs = 0.25, na.rm = TRUE),
        Abundance_q75 = apply(all_Abundance, 1, quantile, probs = 0.75, na.rm = TRUE),
        VulCapture = Vulcap_bins,
        VulHarvest = Vulharv_bins,
        VulTrophy = trophyvul_bins  # Trophy/memorable vulnerability
      )
      # Calculate 95% prediction intervals: mean ± 1.96 × SD
      length_data$Abundance_lower <- length_data$Abundance_mean - 1.96 * length_data$Abundance_sd
      length_data$Abundance_upper <- length_data$Abundance_mean + 1.96 * length_data$Abundance_sd
      length_data$Abundance_lower <- pmax(0, length_data$Abundance_lower)  # Can't be negative
      
      # AGE distribution directly from cohort tracking
      age_data <- data.frame(
        Age = 1:Amax,
        Abundance_mean = rowMeans(all_AgeAbundance, na.rm = TRUE),
        Abundance_median = apply(all_AgeAbundance, 1, median, na.rm = TRUE),
        Abundance_sd = apply(all_AgeAbundance, 1, sd, na.rm = TRUE)
      )
      age_data$Abundance_lower <- pmax(0, age_data$Abundance_median - 1.96 * age_data$Abundance_sd)
      age_data$Abundance_upper <- age_data$Abundance_median + 1.96 * age_data$Abundance_sd
      
      pop_structure_data(list(length_data = length_data, age_data = age_data))
      
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
    
    burnin_years_plot <- if("burnin_years" %in% names(ts_data)) ts_data$burnin_years[1] else 20
    
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
      
      growth_params <- get_growth_params()
      Amax <- input$amax
      Ymax <- Amax + 120
      
      # Weight-length equation (species-specific)
      alfa <- input$wl_a
      bet <- input$wl_b
      
      # Mortality
      DisMort <- input$dismort
      Nat_mort <- input$nat_mort
      
      # Stock-recruit
      Ro <- input$R0
      
      # Vulnerabilities (use current settings)
      Capsize <- input$capsize
      CapsizeSD <- Capsize * 0.01
      Uppercap <- 380
      UppercapSD <- Uppercap * 0.01
      Harvlim <- input$harvlim
      HarvlimSD <- Harvlim * 0.01
      
      # ========================================================================
      # LENGTH-STRUCTURED YIELD CURVES (with growth variability and optional DDR)
      # ========================================================================
      
      # Define length bins (10mm bins) - same as main simulation
      bin_width <- 10
      max_length <- ceiling(growth_params$Linf * 1.2)
      length_bins <- seq(0, max_length, by = bin_width)
      L_bins <- length(length_bins) - 1
      bin_midpoints <- (length_bins[-1] + length_bins[-(L_bins+1)]) / 2
      
      # Get growth CV
      growth_cv <- input$growth_cv
      
      # Pre-compute length-specific variables for each bin
      Wt_bins <- (alfa * bin_midpoints^bet) / 1000
      Wmat <- (alfa * input$mat_size^bet) / 1000
      maturity_ogive_bins <- 1 / (1 + exp(-(Wt_bins - Wmat) / (Wmat * 0.1)))
      # ================================
      # SPECIES-SPECIFIC FECUNDITY
      # ================================
      # Default exponent for ALL species = 1.18 (Barneche et al. 2018)
      fec_exp <- 1.18
      
      # Crappie (white or black) use species-specific exponent = 1.27
      if (input$species %in% c("white_crappie", "black_crappie")) {
        fec_exp <- 1.27
      }
      
      # Apply exponent to weight
      Fec_bins <- (Wt_bins ^ fec_exp) * maturity_ogive_bins      
      Vulcap_bins <- 1 / (1 + exp(-(bin_midpoints - Capsize) / CapsizeSD))
      
      # Harvest vulnerability by length (depends on regulation type)
      if(input$enable_slot) {
        Slot_upper <- input$slot_upper
        Slot_upperSD <- 0.01
        HarvlimSD_slot <- 0.01
        Effective_min <- max(Harvlim, Capsize)
        
        Vulharv_above_min <- 1 / (1 + exp(-(bin_midpoints - Effective_min) / HarvlimSD_slot))
        Vulharv_below_max <- 1 / (1 + exp((bin_midpoints - Slot_upper) / Slot_upperSD))
        
        if(input$slot_type == "traditional") {
          Vulharv_bins <- Vulharv_above_min * Vulharv_below_max
        } else {
          Vulharv_bins <- (1 - (Vulharv_above_min * Vulharv_below_max)) * Vulcap_bins
        }
      } else if(input$enable_max_limit) {
        Max_harvest_size <- input$max_harvest_size
        Max_harvestSD <- 0.01
        Vulharv_above_capture <- 1 / (1 + exp(-(bin_midpoints - Capsize) / CapsizeSD))
        Vulharv_below_max <- 1 / (1 + exp((bin_midpoints - Max_harvest_size) / Max_harvestSD))
        Vulharv_bins <- Vulharv_above_capture * Vulharv_below_max
      } else {
        Vulharv_bins <- 1 / (1 + exp(-(bin_midpoints - Harvlim) / HarvlimSD))
      }
      
      # Trophy vulnerability by length
      trophyvul_bins <- (1 / (1 + exp(-(bin_midpoints - input$memorable_size) / (input$memorable_size * 0.1)))) * Vulcap_bins
      
      # Size-dependent natural mortality
      M_adult <- Nat_mort
      M_bins <- rep(M_adult, L_bins)
      
      # Juveniles have higher mortality
      juvenile_threshold <- input$mat_size * 0.5
      M_bins[bin_midpoints < juvenile_threshold] <- M_adult * 2.0
      M_bins[bin_midpoints >= juvenile_threshold & bin_midpoints < input$mat_size] <- M_adult * 1.5
      
      # Annual survival by length bin (natural mortality only)
      S_bins <- exp(-M_bins)
      Unfished_survival_bins <- S_bins
      
      # Convert CV to lognormal sigma
      sigmaR <- sqrt(log(input$rec_cv^2 + 1))
      
      # Build growth transition matrix (same as main simulation)
      Growth_matrix <- matrix(0, nrow = L_bins, ncol = L_bins)
      
      for(i in 1:L_bins) {
        current_length <- bin_midpoints[i]
        K <- growth_params$vbk
        Linf <- growth_params$Linf
        
        # Growth increment (mechanistic von Bertalanffy for discrete annual time step)
        growth_increment <- (Linf - current_length) * (1 - exp(-K))
        growth_increment <- max(0.1, growth_increment)  # Ensure positive
        
        expected_length <- current_length + growth_increment
        
        # === deterministic case when growth_cv == 0 (age cohort approach) ===
        if(growth_cv == 0) {
          next_bin <- which.min(abs(bin_midpoints - expected_length))
          
          Growth_matrix[i, ] <- 0
          Growth_matrix[i, next_bin] <- 1
          next  # skip to next i (next length bin)
        }
        
        # === stochastic case (normal distribution) ===
        # Set minimum SD to ensure proper scaling with bin width
        growth_sd <- max(1, growth_increment * growth_cv, bin_width * 0.15)
        
        # For fish at or above Linf, minimal growth with small SD
        if(current_length >= Linf * 0.99) {
          growth_increment <- 0.1
          growth_sd <- max(1, bin_width * 0.15)
          expected_length <- current_length + growth_increment
        }
        
        # Distribute probability across bins
        for(j in 1:L_bins) {
          bin_lower <- length_bins[j]
          bin_upper <- length_bins[j+1]
          prob <- pnorm(bin_upper, expected_length, growth_sd) - pnorm(bin_lower, expected_length, growth_sd)
          Growth_matrix[i, j] <- prob
        }
        
        # Normalize row to sum to 1
        row_sum <- sum(Growth_matrix[i, ])
        if(row_sum > 0) {
          Growth_matrix[i, ] <- Growth_matrix[i, ] / row_sum
        } else {
          Growth_matrix[i, i] <- 1.0  # Stay in current bin if no growth
        }
      }
      
      # Calculate mean recruitment length (age-1) and its distribution
      age1_mean_length <- growth_params$Linf * (1 - exp(-growth_params$vbk * (1 - growth_params$t0)))
      
      # Create recruitment length distribution (which bins do age-1 fish go into?)
      recruit_dist <- rep(0, L_bins)
      
      # === deterministic case when growth_cv == 0 (age cohort approach) ===
      if(growth_cv == 0) {
        closest_bin <- which.min(abs(bin_midpoints - age1_mean_length))
        recruit_dist[closest_bin] <- 1.0
      } else {
        # === stochastic case (normal distribution) ===
        age1_sd_length <- max(0.5, age1_mean_length * growth_cv)  # Minimum SD to avoid issues
        
        for(j in 1:L_bins) {
          bin_lower <- length_bins[j]
          bin_upper <- length_bins[j+1]
          prob <- pnorm(bin_upper, age1_mean_length, age1_sd_length) - pnorm(bin_lower, age1_mean_length, age1_sd_length)
          recruit_dist[j] <- max(0, prob)  # Ensure non-negative
        }
        # Normalize and handle edge case of all zeros
        if(sum(recruit_dist) > 0) {
          recruit_dist <- recruit_dist / sum(recruit_dist)
        } else {
          # Fallback: put all recruitment in the bin closest to age1_mean_length
          closest_bin <- which.min(abs(bin_midpoints - age1_mean_length))
          recruit_dist[closest_bin] <- 1.0
        }
      }
      
      # Get number of simulations
      nsim <- input$yield_curve_nsim
      
      # Use 100-year simulation for yield curves with longer burn-in to reduce uncertainty
      Ymax_yield <- Amax + 20 + 100
      burnin_yield <- Amax + 20
      
      # Test exploitation rates from 0 to 1
      U_values <- seq(0, 1, by = 0.1)  # Reduced resolution for speed (11 points instead of 21)
      n_points <- length(U_values)
      
      # Pre-allocate results data frame for speed (avoid rbind in loop)
      curve_results <- data.frame(
        U = U_values,
        YPR_mean = numeric(n_points),
        YPR_sd = numeric(n_points),
        YPR_n = integer(n_points),
        SPR_mean = numeric(n_points),
        SPR_sd = numeric(n_points),
        SPR_n = integer(n_points),
        Prop_mean = numeric(n_points),
        Prop_sd = numeric(n_points),
        Prop_n = integer(n_points),
        Recruit_mean = numeric(n_points),
        Recruit_sd = numeric(n_points),
        TotalYield_mean = numeric(n_points),
        TotalYield_sd = numeric(n_points)
      )
      
      for(u_idx in 1:n_points) {
        incProgress(1/length(U_values), detail = paste("U =", round(U_values[u_idx], 2)))
        
        U_test <- U_values[u_idx]
        
        # Fishing mortality and survival by length bin (for this U)
        F_bins <- Vulharv_bins * U_test
        Release_mort_bins <- (Vulcap_bins - Vulharv_bins) * U_test * DisMort
        Survival_bins <- S_bins * (1 - F_bins) * (1 - Release_mort_bins)
        
        # Harvest weight by length bin
        Wt_harvest_bins <- Wt_bins * Vulharv_bins
        
        ypr_vals <- numeric(nsim)
        spr_vals <- numeric(nsim)
        prop_vals <- numeric(nsim)
        recruit_vals <- numeric(nsim)
        
        for(k in 1:nsim) {
          # Initialize length-structured population matrix
          N <- matrix(0, nrow = Ymax_yield, ncol = L_bins)
          YPR <- rep(NA, Ymax_yield)
          SPRt <- rep(NA, Ymax_yield)
          Prop <- rep(NA, Ymax_yield)
          
          # Set initial population structure
          N[1, ] <- Ro * recruit_dist
          
          # Build UNFISHED equilibrium over burn-in window (establish baseline for SPR)
          SSB_burnin <- rep(NA, burnin_yield)
          SSB_burnin[1] <- sum(N[1, ] * Fec_bins)
          for(init_year in 2:burnin_yield) {
            # Apply UNFISHED survival (natural mortality only, NO fishing)
            N_survive <- N[init_year-1, ] * Unfished_survival_bins
            
            # Apply growth (move to new length bins)
            N[init_year, ] <- as.vector(N_survive %*% Growth_matrix)
            
            # Add stochastic recruitment (unfished populations still have recruitment variability)
            N[init_year, ] <- N[init_year, ] + (Ro * rlnorm(1, 0, sd = sigmaR)) * recruit_dist
            
            SSB_burnin[init_year] <- sum(N[init_year, ] * Fec_bins)
          }
          
          burnin_start <- max(1, burnin_yield - 9)
          SPR_denom <- mean(SSB_burnin[burnin_start:burnin_yield], na.rm = TRUE)
          
          # Compute unfished SSB0 for DDR
          SSB0 <- SPR_denom
          
          # Generate stochastic recruitment (or calculate from SSB if DDR enabled)
          if(isTRUE(input$enable_ddr)) {
            # Will be calculated dynamically inside loop based on SSB
            Rcapacity <- rep(NA, Ymax_yield)
          } else {
            # Traditional per-recruit: constant mean recruitment
            if(input$rec_cv == 0) {
              # Deterministic recruitment when CV = 0
              Rcapacity <- rep(Ro, Ymax_yield)
            } else {
              # Stochastic recruitment with lognormal noise
              Rcapacity <- Ro * rlnorm(Ymax_yield, 0, sd = sigmaR)
            }
          }
          
          # Get steepness if DDR is enabled
          h <- ifelse(isTRUE(input$enable_ddr), input$steepness, 0.7)
          
          # Calculate metrics for unfished burn-in period
          for(yr in 1:burnin_yield) {
            YPR[yr] <- 0
            SPRt[yr] <- sum(N[yr, ] * Fec_bins) / SPR_denom  # Build toward equilibrium
            Prop[yr] <- sum(trophyvul_bins * N[yr, ]) / max(1, sum(N[yr, ]))
          }
          
          # Main simulation loop with FISHING (starts after burn-in)
          start_year_yield <- min(burnin_yield + 1, Ymax_yield)
          for(i in start_year_yield:Ymax_yield) {
            # If DDR enabled, calculate recruitment from previous year's SSB
            if(isTRUE(input$enable_ddr)) {
              # Calculate spawning stock biomass from PREVIOUS year
              SSB_t <- sum(N[i-1, ] * Fec_bins)
              
              # Prevent negative SSB
              SSB_t <- max(0, SSB_t)
              
              # Beverton-Holt recruitment with steepness parameterization
              R_BH <- (4 * h * Ro * SSB_t) / (SSB0 * (1 - h) + (5 * h - 1) * SSB_t)
              
              # Ensure positive recruitment, minimum 1 recruit
              R_BH <- max(1, R_BH)
              
              # Apply depensation (Allee effects) if enabled
              if(isTRUE(input$enable_depensation) && SSB_t < 0.2 * SSB0) {
                depensation_factor <- (SSB_t / (0.2 * SSB0))^2  # Quadratic penalty
                R_BH <- R_BH * depensation_factor
              }
              
              # Add stochastic noise (or deterministic if rec_cv = 0)
              if(input$rec_cv == 0) {
                Rcapacity[i] <- max(1, R_BH)
              } else {
                Rcapacity[i] <- max(1, R_BH * rlnorm(1, 0, sd = sigmaR))
              }
            }
            
            # Apply FISHED survival (includes fishing mortality)
            N_survive <- N[i-1, ] * Survival_bins
            
            # Apply growth (move to new length bins)
            N[i, ] <- as.vector(N_survive %*% Growth_matrix)
            
            # Add recruitment
            N[i, ] <- N[i, ] + Rcapacity[i] * recruit_dist
            
            # Calculate metrics
            harvest_weight <- sum(Wt_harvest_bins * N[i, ])
            fecundity_now <- sum(Fec_bins * N[i, ])
            abundance_now <- max(1, sum(N[i, ]))  # Prevent division by zero
            
            YPR[i] <- (harvest_weight * U_test) / max(1, Rcapacity[i])  # Prevent division by zero
            SPRt[i] <- fecundity_now / SPR_denom
            Prop[i] <- sum(trophyvul_bins * N[i, ]) / abundance_now
          }
          
          # Use last 50 years of fished equilibrium for calculations
          last_50_start <- max(start_year_yield, Ymax_yield - 49)  # Ensure we don't include unfished burn-in
          ypr_vals[k] <- mean(YPR[last_50_start:Ymax_yield], na.rm = TRUE)
          spr_vals[k] <- mean(SPRt[last_50_start:Ymax_yield], na.rm = TRUE)
          prop_vals[k] <- mean(Prop[last_50_start:Ymax_yield], na.rm = TRUE)
          recruit_vals[k] <- mean(Rcapacity[last_50_start:Ymax_yield], na.rm = TRUE)
        }
        
        # Store results in pre-allocated data frame (much faster than rbind)
        curve_results$YPR_mean[u_idx] <- mean(ypr_vals, na.rm = TRUE)
        curve_results$YPR_sd[u_idx] <- sd(ypr_vals, na.rm = TRUE)
        curve_results$YPR_n[u_idx] <- nsim
        curve_results$SPR_mean[u_idx] <- mean(spr_vals, na.rm = TRUE)
        curve_results$SPR_sd[u_idx] <- sd(spr_vals, na.rm = TRUE)
        curve_results$SPR_n[u_idx] <- nsim
        curve_results$Prop_mean[u_idx] <- mean(prop_vals, na.rm = TRUE)
        curve_results$Prop_sd[u_idx] <- sd(prop_vals, na.rm = TRUE)
        curve_results$Prop_n[u_idx] <- nsim
        curve_results$Recruit_mean[u_idx] <- mean(recruit_vals, na.rm = TRUE)
        curve_results$Recruit_sd[u_idx] <- sd(recruit_vals, na.rm = TRUE)
        
        # Calculate Total Yield = YPR × Recruitment
        total_yield_vals <- ypr_vals * recruit_vals
        curve_results$TotalYield_mean[u_idx] <- mean(total_yield_vals, na.rm = TRUE)
        curve_results$TotalYield_sd[u_idx] <- sd(total_yield_vals, na.rm = TRUE)
      }
      
      yield_curve_data(curve_results)
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
