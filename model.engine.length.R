# Shared model engine helpers

get_growth_params <- function(input) {
  list(Linf = input$linf, vbk = input$vbk, t0 = input$t0)
}

build_length_bins <- function(input, bin_width = 10, growth_params = get_growth_params(input)) {
  max_length <- ceiling(growth_params$Linf * 1.2)
  length_bins <- seq(0, max_length, by = bin_width)
  L_bins <- length(length_bins) - 1
  bin_lowers <- length_bins[-length(length_bins)]
  bin_uppers <- length_bins[-1]
  bin_midpoints <- (bin_uppers + bin_lowers) / 2
  
  list(
    bin_width = bin_width,
    length_bins = length_bins,
    L_bins = L_bins,
    bin_lowers = bin_lowers,
    bin_uppers = bin_uppers,
    bin_midpoints = bin_midpoints,
    max_length = max_length
  )
}

build_vulnerability_curves <- function(input, bin_midpoints) {
  Capsize <- input$capsize
  CapsizeSD <- Capsize * 0.01
  Harvlim <- input$harvlim
  HarvlimSD <- Harvlim * 0.01
  
  Vulcap_bins <- 1 / (1 + exp(-(bin_midpoints - Capsize) / CapsizeSD))
  
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
  
  trophyvul_bins <- (1 / (1 + exp(-(bin_midpoints - input$memorable_size) / (input$memorable_size * 0.1)))) * Vulcap_bins
  
  list(
    Vulcap_bins = Vulcap_bins,
    Vulharv_bins = Vulharv_bins,
    trophyvul_bins = trophyvul_bins
  )
}

build_mortality <- function(input, length_bins) {
  M_adult <- input$nat_mort
  M_bins <- rep(M_adult, length_bins$L_bins)
  
  list(
    M_bins = M_bins,
    Unfished_survival_bins = exp(-M_bins)
  )
}

build_fecundity <- function(input, Wt_bins, maturity_ogive_bins) {
  fec_exp <- 1.18
  if (input$species %in% c("white_crappie", "black_crappie")) {
    fec_exp <- 1.27
  }
  (Wt_bins ^ fec_exp) * maturity_ogive_bins
}


build_recruit_distribution <- function(input, length_bins, growth_params = get_growth_params(input), growth_cv_effective) {
  age1_mean_length <- growth_params$Linf * (1 - exp(-growth_params$vbk * (1 - growth_params$t0)))

  if (growth_cv_effective == 0) {
    closest_bin <- which.min(abs(length_bins$bin_midpoints - age1_mean_length))
    recruit_dist <- rep(0, length(length_bins$length_bins) - 1)
    recruit_dist[closest_bin] <- 1.0
    return(recruit_dist)
  }

  age1_sd_length <- max(0.5, age1_mean_length * growth_cv_effective)

  L_bins <- length(length_bins$length_bins) - 1
  recruit_dist <- rep(0, L_bins)
  for(j in 1:L_bins) {
    bin_lower <- length_bins$length_bins[j]
    bin_upper <- length_bins$length_bins[j + 1]
    prob <- pnorm(bin_upper, age1_mean_length, age1_sd_length) - pnorm(bin_lower, age1_mean_length, age1_sd_length)
    recruit_dist[j] <- max(0, prob)
  }

  if (sum(recruit_dist) > 0) {
    recruit_dist <- recruit_dist / sum(recruit_dist)
  } else {
    closest_bin <- which.min(abs(length_bins$bin_midpoints - age1_mean_length))
    recruit_dist[closest_bin] <- 1.0
  }

  recruit_dist
}

make_growth_matrix <- function(L_bins, bin_midpoints, bin_lowers, bin_uppers,
                               growth_params, bin_width, growth_cv_input,
                               growth_cv_effective) {
  Growth_matrix <- matrix(0, nrow = L_bins, ncol = L_bins)
  
  for(i in 1:L_bins) {
    current_length <- bin_midpoints[i]
    K <- growth_params$vbk
    Linf <- growth_params$Linf

    # === deterministic case when growth_cv == 0 ===
    if (growth_cv_input == 0) {
      next_length <- Linf * (1 - exp(-K)) + current_length * exp(-K)
      deterministic_bin <- which.min(abs(bin_midpoints - next_length))

      Growth_matrix[i, ] <- 0
      Growth_matrix[i, deterministic_bin] <- 1
      next  # skip stochastic code
    }

    # von Bertalanffy annual increment
    growth_increment <- (Linf - current_length) * (1 - exp(-K))
    growth_increment <- max(0.1, growth_increment)

    expected_length <- current_length + growth_increment

    # === stochastic case (normal distribution) ===
    growth_sd <- max(1, growth_increment * growth_cv_effective, bin_width * 0.15)
    
    # handle fish at/near Linf
    if(current_length >= Linf * 0.99) {
      growth_increment <- 0.1
      growth_sd <- max(1, bin_width * 0.15)
      expected_length <- current_length + growth_increment
    }
    
    probs <- pnorm(bin_uppers, expected_length, growth_sd) -
      pnorm(bin_lowers, expected_length, growth_sd)
    
    probs[probs < 0] <- 0
    
    # normalize row to sum to 1
    row_sum <- sum(probs)
    if (row_sum > 0) {
      Growth_matrix[i, ] <- probs / row_sum
    } else {
      Growth_matrix[i, i] <- 1.0
    }
  }
  
  Growth_matrix
}

build_growth_matrix <- function(input, length_bins, growth_params = get_growth_params(input), growth_cv_effective) {
  make_growth_matrix(
    L_bins = length_bins$L_bins,
    bin_midpoints = length_bins$bin_midpoints,
    bin_lowers = length_bins$bin_lowers,
    bin_uppers = length_bins$bin_uppers,
    growth_params = growth_params,
    bin_width = length_bins$bin_width,
    growth_cv_input = input$growth_cv,
    growth_cv_effective = growth_cv_effective
  )
}

bh_params <- function(h, Ro, SSB0) {
  inv <- 1 / max(1, SSB0 * (1 - h))
  list(alpha = 4 * h * Ro * inv,
       beta  = (5 * h - 1) * inv)
}

build_static_components <- function(input, growth_params = get_growth_params(input)) {
  length_bins <- build_length_bins(input, growth_params = growth_params)
  growth_cv_effective <- if (input$growth_cv == 0) 0 else input$growth_cv
  
  vulnerabilities <- build_vulnerability_curves(input, length_bins$bin_midpoints)
  mortality <- build_mortality(input, length_bins)
  
  alfa <- input$wl_a
  bet <- input$wl_b
  
  Wt_bins <- (alfa * length_bins$bin_midpoints^bet) / 1000
  Wmat <- (alfa * input$mat_size^bet) / 1000
  maturity_ogive_bins <- 1 / (1 + exp(-(Wt_bins - Wmat) / (Wmat * 0.1)))
  
  Fec_bins <- build_fecundity(input, Wt_bins, maturity_ogive_bins)
  
  Growth_matrix <- build_growth_matrix(
    input = input,
    length_bins = length_bins,
    growth_params = growth_params,
    growth_cv_effective = growth_cv_effective
  )
  
  recruit_dist <- build_recruit_distribution(
    input = input,
    length_bins = length_bins,
    growth_params = growth_params,
    growth_cv_effective = growth_cv_effective
  )
  
  list(
    length_bins = length_bins,
    Wt_bins = Wt_bins,
    maturity_ogive_bins = maturity_ogive_bins,
    Fec_bins = Fec_bins,
    Vulcap_bins = vulnerabilities$Vulcap_bins,
    Vulharv_bins = vulnerabilities$Vulharv_bins,
    trophyvul_bins = vulnerabilities$trophyvul_bins,
    M_bins = mortality$M_bins,
    Unfished_survival_bins = mortality$Unfished_survival_bins,
    Growth_matrix = Growth_matrix,
    recruit_dist = recruit_dist,
    growth_cv_effective = growth_cv_effective
  )
}

simulate_population <- function(U, static, params) {
  L_bins <- static$length_bins$L_bins
  bin_midpoints <- static$length_bins$bin_midpoints
  
  Amax <- params$Amax
  burn_in_years <- params$burn_in_years
  Ymax <- params$Ymax
  DisMort <- params$DisMort
  sigmaR <- sqrt(log(params$rec_cv^2 + 1))
  Ro <- params$Ro
  store_details <- isTRUE(params$store_details)
  
  Vulcap_bins <- static$Vulcap_bins
  Vulharv_bins <- static$Vulharv_bins
  trophyvul_bins <- static$trophyvul_bins
  Wt_bins <- static$Wt_bins
  Fec_bins <- static$Fec_bins
  M_bins <- static$M_bins
  Unfished_survival_bins <- static$Unfished_survival_bins
  Growth_matrix <- static$Growth_matrix
  recruit_dist <- static$recruit_dist
  
  F_inst <- -log(1 - U)
  
  YPR <- matrix(0, nrow = Ymax, ncol = params$nsim)
  SPRt <- matrix(0, nrow = Ymax, ncol = params$nsim)
  Prop <- matrix(0, nrow = Ymax, ncol = params$nsim)
  SSBt <- matrix(0, nrow = Ymax, ncol = params$nsim)
  mean_harvest_length <- matrix(NA_real_, nrow = Ymax, ncol = params$nsim)
  N <- matrix(0, nrow = Ymax, ncol = L_bins)
  all_YPR <- all_SPR <- all_Prop <- all_SSB <- matrix(0, nrow = Ymax, ncol = params$nsim)
  all_Abundance <- matrix(0, nrow = L_bins, ncol = params$nsim)
  all_AgeAbund <- matrix(0, nrow = Amax, ncol = params$nsim)
  
  burn_in_span <- min(burn_in_years, Ymax)
  
  for(k in 1:params$nsim) {
    if (!is.null(params$progress)) {
      params$progress(1 / params$nsim,
                      detail = paste("Simulation", k, "of", params$nsim))
    }
    Cohort <- matrix(0, nrow = Amax, ncol = L_bins)
    Cohort[1, ] <- Ro * recruit_dist
    
    N[1, ] <- colSums(Cohort)
    
    SSB_burnin <- rep(NA_real_, burn_in_span)
    SSB_burnin[1] <- compute_ssb(N[1, ], Fec_bins)
    
    if (isTRUE(params$enable_ddr)) {
      alpha_beta <- NULL
      # placeholder to avoid R CMD check notes when unused without DDR
    }
    
    for(t in 2:burn_in_span) {
      newCohort <- matrix(0, nrow = Amax, ncol = L_bins)
      for(a in Amax:2) {
        survivors <- Cohort[a - 1, ] * Unfished_survival_bins
        newCohort[a, ] <- as.vector(survivors %*% Growth_matrix)
      }
      
      R <- Ro * rlnorm(1, 0, sd = sigmaR)
      newCohort[1, ] <- R * recruit_dist
      
      Cohort <- newCohort
      N[t, ] <- colSums(Cohort)
      SSB_burnin[t] <- compute_ssb(N[t, ], Fec_bins)
    }
    
    # Correct unfished reference SSB (U = 0, length-based mortality only)
    SPR_denom <- mean(SSB_burnin[max(1, burn_in_span - 10 + 1):burn_in_span], na.rm = TRUE)
    if (!is.finite(SPR_denom) || SPR_denom <= 0) SPR_denom <- 1
    SSB0 <- SPR_denom
    
    if (isTRUE(params$enable_ddr)) {
      alpha_beta <- bh_params(h = params$steepness, Ro = Ro, SSB0 = SSB0)
    }
    
    if (isTRUE(params$enable_ddr)) {
      Rcapacity <- rep(NA_real_, Ymax)
    } else {
      Rcapacity <- Ro * rlnorm(Ymax, 0, sd = sigmaR)
    }
    
    for(yr in 1:burn_in_span) {
      Yield <- 0
      SSB_now <- compute_ssb(N[yr, ], Fec_bins)
      SSBt[yr, k] <- SSB_now
      SPRt[yr, k] <- if (SPR_denom > 0) SSB_now / SPR_denom else 0
      YPR[yr, k] <- 0
      Prop[yr, k] <- ifelse(sum(N[yr, ]) > 0, sum(trophyvul_bins * N[yr, ]) / sum(N[yr, ]), 0)
    }
    
    start_year <- min(burn_in_span + 1, Ymax)
    for(t in start_year:Ymax) {
      if (isTRUE(params$enable_ddr)) {
        if (t == start_year) {
          Rcapacity[t] <- Rcapacity[t - 1]
        } else {
          SSB_prev <- compute_ssb(N[t - 1, ], Fec_bins)
          if (!is.finite(SSB_prev) || SSB_prev < 0) SSB_prev <- 0
          
          if (is.na(SSB0) || !is.finite(SSB0) || SSB0 <= 0) SSB0 <- 1
          
          R_BH <- alpha_beta$alpha * SSB_prev / (1 + alpha_beta$beta * SSB_prev)
          if (!is.finite(R_BH) || R_BH <= 0) R_BH <- 1
          
          if (isTRUE(params$enable_depensation) && SSB_prev < 0.2 * SSB0) {
            depensation_factor <- (SSB_prev / (0.2 * SSB0))^2
            if (!is.finite(depensation_factor) || depensation_factor < 0) depensation_factor <- 0
            R_BH <- R_BH * depensation_factor
          }
          
          if (!is.finite(R_BH) || R_BH <= 0) R_BH <- 1
          
          Rcapacity[t] <- max(1, R_BH * rlnorm(1, 0, sd = sigmaR))
        }
      }
      
      # Length-based fishing mortality and total mortality
      F_len <- F_inst * Vulharv_bins
      Z_bins <- M_bins + F_len
      S_bins_total <- exp(-Z_bins)
      catch_fraction_bins <- (F_len / Z_bins) * (1 - S_bins_total)
      catch_fraction_bins[!is.finite(catch_fraction_bins)] <- 0
      
      newCohort <- matrix(0, nrow = Amax, ncol = L_bins)
      annual_harvest_bins <- numeric(L_bins)
      for(a in Amax:2) {
        survivors <- Cohort[a - 1, ] * S_bins_total
        newCohort[a, ] <- as.vector(survivors %*% Growth_matrix)
        annual_harvest_bins <- annual_harvest_bins + Cohort[a - 1, ] * catch_fraction_bins
      }
      
      newCohort[1, ] <- Rcapacity[t] * recruit_dist
      
      Cohort <- newCohort
      N[t, ] <- colSums(Cohort)
      
      Yield_weight <- sum(Wt_bins * annual_harvest_bins)
      SSB_now <- compute_ssb(N[t, ], Fec_bins)
      Trophy_prop <- ifelse(sum(N[t, ]) > 0, sum(trophyvul_bins * N[t, ]) / sum(N[t, ]), 0)
      
      YPR[t, k] <- Yield_weight / max(1, Rcapacity[t])
      SSBt[t, k] <- SSB_now
      SPRt[t, k] <- if (SPR_denom > 0) SSB_now / SPR_denom else 0
      Prop[t, k] <- Trophy_prop
      
      harvest_by_bin <- annual_harvest_bins
      harvest_total <- sum(harvest_by_bin)
      mean_harvest_length[t, k] <- if (harvest_total > 0) {
        sum(harvest_by_bin * bin_midpoints) / harvest_total
      } else {
        NA_real_
      }
      
      
    }
    
    last_50_start <- max(start_year, Ymax - 49)
    
    results <- data.frame(
      YPR = mean(YPR[last_50_start:Ymax, k], na.rm = TRUE),
      SPR = mean(SPRt[last_50_start:Ymax, k], na.rm = TRUE),
      Prop = mean(Prop[last_50_start:Ymax, k], na.rm = TRUE),
      Recruit = mean(Rcapacity[last_50_start:Ymax], na.rm = TRUE)
    )
    results$MeanLengthHarvested <- mean(mean_harvest_length[last_50_start:Ymax, k], na.rm = TRUE)
    
    if (k == 1) {
      results_accum <- results
    } else {
      results_accum <- rbind(results_accum, results)
    }
    
    if (store_details) {
      all_YPR[, k] <- YPR[, k]
      all_SPR[, k] <- SPRt[, k]
      all_Prop[, k] <- Prop[, k]
      all_SSB[, k] <- SSBt[, k]
      all_Abundance[, k] <- N[Ymax, ]
      all_AgeAbund[, k] <- rowSums(Cohort)
    }
  }
  
  ts_data <- NULL
  pop_structure <- NULL
  
  if (store_details) {
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
    
    attr(ts_data, "burn_in_years") <- burn_in_span
    
    ts_data$YPR_lower <- pmax(0, ts_data$YPR_mean - 1.96 * ts_data$YPR_sd)
    ts_data$YPR_upper <- ts_data$YPR_mean + 1.96 * ts_data$YPR_sd
    ts_data$SPR_lower <- pmax(0, ts_data$SPR_mean - 1.96 * ts_data$SPR_sd)
    ts_data$SPR_upper <- ts_data$SPR_mean + 1.96 * ts_data$SPR_sd
    ts_data$Prop_lower <- pmax(0, ts_data$Prop_mean - 1.96 * ts_data$Prop_sd)
    ts_data$Prop_upper <- pmin(1, ts_data$Prop_mean + 1.96 * ts_data$Prop_sd)
    ts_data$SSB_lower <- pmax(0, ts_data$SSB_mean - 1.96 * ts_data$SSB_sd)
    ts_data$SSB_upper <- ts_data$SSB_mean + 1.96 * ts_data$SSB_sd
    
    pop_data <- data.frame(
      Length = bin_midpoints,
      Weight = Wt_bins,
      Abundance_mean = rowMeans(all_Abundance, na.rm = TRUE),
      Abundance_median = apply(all_Abundance, 1, median, na.rm = TRUE),
      Abundance_sd = apply(all_Abundance, 1, sd, na.rm = TRUE),
      VulCapture = Vulcap_bins,
      VulHarvest = Vulharv_bins,
      VulTrophy = trophyvul_bins
    )
    pop_data$Abundance_lower <- pop_data$Abundance_mean - 1.96 * pop_data$Abundance_sd
    pop_data$Abundance_upper <- pop_data$Abundance_mean + 1.96 * pop_data$Abundance_sd
    pop_data$Abundance_lower <- pmax(0, pop_data$Abundance_lower)
    
    age_median <- apply(all_AgeAbund, 1, median, na.rm = TRUE)
    age_sd     <- apply(all_AgeAbund, 1, sd, na.rm = TRUE)
    
    age_lower <- pmax(0, age_median - 1.96 * age_sd)
    age_upper <- age_median + 1.96 * age_sd
    
    age_data <- data.frame(
      Age = 1:Amax,
      Abundance_median = age_median,
      Abundance_lower = age_lower,
      Abundance_upper = age_upper
    )
    
    pop_structure <- list(
      length_data = pop_data,
      age_data = age_data
    )
  }
  
  list(
    results = results_accum,
    time_series = ts_data,
    pop_structure = pop_structure
  )
}

simulate_yield_curve <- function(static, params) {
  U_values <- if (!is.null(params$U_values)) params$U_values else seq(0, 1, by = 0.1)
  n_points <- length(U_values)
  
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
  
  for(u_idx in seq_along(U_values)) {
    if (!is.null(params$progress)) {
      params$progress(1/n_points, detail = paste("U =", round(U_values[u_idx], 2)))
    }
    
    sim_out <- simulate_population(
      U = U_values[u_idx],
      static = static,
      params = modifyList(params, list(store_details = FALSE, progress = NULL))
    )
    
    res <- sim_out$results
    
    curve_results$YPR_mean[u_idx] <- mean(res$YPR, na.rm = TRUE)
    curve_results$YPR_sd[u_idx] <- sd(res$YPR, na.rm = TRUE)
    curve_results$YPR_n[u_idx] <- params$nsim
    curve_results$SPR_mean[u_idx] <- mean(res$SPR, na.rm = TRUE)
    curve_results$SPR_sd[u_idx] <- sd(res$SPR, na.rm = TRUE)
    curve_results$SPR_n[u_idx] <- params$nsim
    curve_results$Prop_mean[u_idx] <- mean(res$Prop, na.rm = TRUE)
    curve_results$Prop_sd[u_idx] <- sd(res$Prop, na.rm = TRUE)
    curve_results$Prop_n[u_idx] <- params$nsim
    curve_results$Recruit_mean[u_idx] <- mean(res$Recruit, na.rm = TRUE)
    curve_results$Recruit_sd[u_idx] <- sd(res$Recruit, na.rm = TRUE)
    
    total_yield_vals <- res$YPR * res$Recruit
    curve_results$TotalYield_mean[u_idx] <- mean(total_yield_vals, na.rm = TRUE)
    curve_results$TotalYield_sd[u_idx] <- sd(total_yield_vals, na.rm = TRUE)
  }
  
  curve_results
}

build_static_components_from_input <- function(input) {
  growth_params <- list(
    Linf = input$linf,
    vbk  = input$vbk,
    t0   = input$t0
  )
  build_static_components(input = input, growth_params = growth_params)
}

run_population_simulation <- function(
    U,
    input,
    static,
    nsim,
    store_details = TRUE,
    progress_cb = NULL
) {
  burn_in_years <- min(input$ymax, max(20, input$amax + 20))
  Ymax_val <- input$ymax
  
  params <- list(
    Amax              = input$amax,
    burn_in_years     = burn_in_years,
    Ymax              = Ymax_val,
    nsim              = nsim,
    DisMort           = input$dismort,
    rec_cv            = input$rec_cv,
    enable_ddr        = input$enable_ddr,
    enable_depensation = input$enable_depensation,
    steepness         = input$steepness,
    Ro                = 10000,
    store_details     = store_details,
    progress          = progress_cb
  )
  
  simulate_population(U = U, static = static, params = params)
}

run_yield_curve_simulation <- function(
    input,
    static,
    U_values,
    nsim,
    progress_cb = NULL
) {
  burn_in_years <- min(input$ymax, max(20, input$amax + 20))
  Ymax_val <- input$ymax
  
  params <- list(
    Amax              = input$amax,
    burn_in_years     = burn_in_years,
    Ymax              = Ymax_val,
    nsim              = nsim,
    DisMort           = input$dismort,
    rec_cv            = input$rec_cv,
    enable_ddr        = input$enable_ddr,
    enable_depensation = input$enable_depensation,
    steepness         = input$steepness,
    Ro                = 10000,
    store_details     = FALSE,
    U_values          = U_values,
    progress          = progress_cb
  )
  
  simulate_yield_curve(static = static, params = params)
}
compute_ssb <- function(N_row, Fec_bins) {
  sum(N_row * Fec_bins)
}
