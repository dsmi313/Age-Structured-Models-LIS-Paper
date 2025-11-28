The R code here is from our paper Live-imaging sonar use in Texas crappie fisheries: Assessing population-level responses due to potential increases in exploitation.

The Excel spreadsheet attached is a simplified version of the model with the point and click capabilities of Excel, similar to those used by Dotson et al. (2009) https://doi.org/10.1577/M08-137.1

## Running the Shiny apps

Two Shiny apps live in the repository root:

* `app.R` — original age-structured model.
* `length.R` — length-structured model that automatically falls back to the age-based path when `growth_cv` is set to `0`.

To launch either app locally:

1. Open an R session in the repository root.
2. Install the dependencies if needed:

   ```r
   install.packages(c("shiny", "dplyr", "tidyr", "ggplot2", "plotly"))
   ```

3. Run the desired app (replace the file name as needed):

   ```r
   shiny::runApp("length.R")
   # or
   shiny::runApp("app.R")
   ```

When `length.R` is run with `growth_cv = 0`, it reverts to the strictly age-based behavior to match `app.R`; positive values use the length-structured workflow.
