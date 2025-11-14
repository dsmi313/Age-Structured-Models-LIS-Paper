# Crappie Age-Structured Model Shiny App

Interactive Shiny application for the age-structured population model from the paper:
**"Live-imaging sonar use in Texas crappie fisheries: Assessing population-level responses due to potential increases in exploitation."**

## Installation

### Required R Packages

```r
install.packages(c("shiny", "tidyverse", "plotly"))
```

## Running the App

### Option 1: Run from R/RStudio

```r
library(shiny)
runApp("path/to/Age-Structured-Models-LIS-Paper")
```

### Option 2: Run directly from the file

Open `app.R` in RStudio and click "Run App" button.

### Option 3: Run from GitHub (once pushed)

```r
library(shiny)
runGitHub("Age-Structured-Models-LIS-Paper", "dsmi313")
```

## Features

### Parameter Controls

**Growth Parameters:**
- Pre-set growth rates: Slow, Moderate, Fast
- Based on von Bertalanffy growth equation (L∞, K, t₀)

**Exploitation:**
- Adjustable exploitation rate (U) from 0-100%
- Represents fishing mortality

**Selectivity:**
- Length at 50% capture vulnerability
- Minimum harvest size limit

**Mortality:**
- Discard mortality rate

**Simulation Settings:**
- Number of simulations (Monte Carlo)
- Years to simulate (with 50-year burn-in)

### Outputs

**Results Summary Tab:**
- Summary statistics for YPR, SPR, and proportion of memorable fish
- Distribution histograms for each metric

**Time Series Tab:**
- Temporal dynamics of population metrics
- Shows equilibrium behavior

**Population Structure Tab:**
- Age distribution at equilibrium
- Vulnerability curves by length (capture vs. harvest)

## Model Details

### Age-Structured Dynamics
- Age classes: 1-8 years
- Stochastic recruitment (lognormal distribution, CV = 0.8)
- Natural mortality incorporated via survival rate

### Growth
Von Bertalanffy equation: L(t) = L∞(1 - e^(-K(t-t₀)))

### Weight-Length Relationship
W = αL^β
- α = 2.40991×10⁻⁶
- β = 3.38

### Selectivity
Logistic functions for:
- Capture vulnerability
- Harvest vulnerability (size limit)

### Key Metrics

**YPR (Yield Per Recruit):**
Total yield (kg) divided by recruitment

**SPR (Spawning Potential Ratio):**
Spawning potential relative to unfished population

**Proportion Memorable:**
Proportion of fish ≥305mm (~12 inches)

## Usage Example

1. Select growth rate (e.g., "Moderate")
2. Set exploitation rate (e.g., 0.34 = 34%)
3. Adjust minimum harvest size if desired (default 254mm = 10 inches)
4. Set number of simulations (more = smoother distributions, but slower)
5. Click "Run Simulation"
6. Explore results in different tabs
7. Download results as CSV if needed

## References

Similar models used in:
- Dotson et al. (2009) https://doi.org/10.1577/M08-137.1

## File Structure

```
.
├── app.R              # Main Shiny application
├── Rcode              # Original simulation code
├── README_SHINY.md    # This file
└── Crappie YPR 092023.xlsx  # Excel version of model
```

## Notes

- Simulations include stochasticity, so results will vary slightly between runs
- Higher numbers of simulations provide more stable estimates but take longer
- The model runs a burn-in period of 50 years before recording results
- Default parameters match those used in the original study
