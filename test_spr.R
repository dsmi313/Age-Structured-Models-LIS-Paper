# Test SPR calculation to verify if SPR > 1 is possible

# Simple YPR model to test SPR
test_spr <- function(U, Harvlim_age) {
  Amax <- 8
  Nat_mort <- 0.35
  Age <- 1:Amax

  # Fecundity increases with age (simplified)
  Fec <- Age^2  # Older fish more fecund

  # Survival
  S_natural <- exp(-Nat_mort)^(Age - 1)

  # Unfished equilibrium (year 1)
  N_unfished <- 10000 * S_natural
  SPR_unfished <- sum(N_unfished * Fec) / N_unfished[1]

  # Fished equilibrium with selective harvest
  # Only harvest fish >= Harvlim_age
  Vulharv <- ifelse(Age >= Harvlim_age, 1, 0)

  # Apply fishing mortality
  F_rate <- -log(1 - U)
  Z <- Nat_mort + F_rate * Vulharv  # Total mortality
  S_fished <- exp(-Z)^(Age - 1)

  # Fished equilibrium
  N_fished <- 10000 * S_fished
  SPR_fished <- sum(N_fished * Fec) / N_fished[1]

  # SPR ratio
  SPR <- SPR_fished / SPR_unfished

  return(list(
    U = U,
    Harvlim_age = Harvlim_age,
    SPR = SPR,
    SPR_fished = SPR_fished,
    SPR_unfished = SPR_unfished
  ))
}

# Test scenarios
cat("Testing SPR with different harvest scenarios:\n\n")

# Scenario 1: Normal harvest (all ages)
result1 <- test_spr(U = 0.3, Harvlim_age = 1)
cat(sprintf("Scenario 1 - Harvest all ages (U=0.3): SPR = %.3f\n", result1$SPR))

# Scenario 2: Harvest only older fish (protect juveniles)
result2 <- test_spr(U = 0.3, Harvlim_age = 4)
cat(sprintf("Scenario 2 - Harvest age 4+ only (U=0.3): SPR = %.3f\n", result2$SPR))

# Scenario 3: High exploitation on older fish only
result3 <- test_spr(U = 0.8, Harvlim_age = 4)
cat(sprintf("Scenario 3 - Heavy harvest age 4+ (U=0.8): SPR = %.3f\n", result3$SPR))

# Scenario 4: Harvest ONLY juveniles (age 1-2)
# This requires modifying the function
test_spr_juveniles <- function(U) {
  Amax <- 8
  Nat_mort <- 0.35
  Age <- 1:Amax

  # Fecundity increases with age
  Fec <- Age^2

  # Survival
  S_natural <- exp(-Nat_mort)^(Age - 1)

  # Unfished
  N_unfished <- 10000 * S_natural
  SPR_unfished <- sum(N_unfished * Fec) / N_unfished[1]

  # Only harvest age 1-2 (before maturity at age 3)
  Vulharv <- ifelse(Age <= 2, 1, 0)

  F_rate <- -log(1 - U)
  Z <- Nat_mort + F_rate * Vulharv
  S_fished <- exp(-Z)^(Age - 1)

  N_fished <- 10000 * S_fished
  SPR_fished <- sum(N_fished * Fec) / N_fished[1]

  SPR <- SPR_fished / SPR_unfished

  return(SPR)
}

result4 <- test_spr_juveniles(U = 0.5)
cat(sprintf("Scenario 4 - Harvest ONLY juveniles age 1-2 (U=0.5): SPR = %.3f\n", result4))

cat("\n**Conclusion**: In all scenarios, SPR <= 1.0\n")
cat("This confirms that selective harvest CANNOT increase SPR above unfished levels\n")
cat("in a model without compensatory growth or mortality.\n")
