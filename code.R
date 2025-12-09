set.seed(400)

library(knitr)

paf_single <- function(Pe, RR) {
  PAF <- (Pe * (RR - 1)) / (Pe * (RR - 1) + 1)
  return(PAF)
}

var_beta_from_CI <- function(RR, Lower, Upper) {
  CIR   <- Upper / Lower
  logse <- log(CIR) / (2 * qnorm(0.975))  
  Var_beta <- logse^2                     
  return(Var_beta)
}

var_Pe <- function(Tp, Pe) {
  Var_Pe <- Pe * (1 - Pe) / Tp 
  return(Var_Pe)
}


delta_paf_ci <- function(Tp, Pe, RR, Lower, Upper){
  PAF <- paf_single(Pe, RR)
  Var_Pe<- var_Pe(Tp,Pe)
  Var_beta <- var_beta_from_CI(RR, Lower, Upper) 
  
  Delta_Var_PAF <- (((RR - 1)^2) * Var_Pe + ((Pe * RR)^2) *
                     Var_beta)/((1 + Pe * (RR - 1))^4)
  
  se <- sqrt(Delta_Var_PAF)
  z  <- qnorm(0.975)
  
  low <- PAF- (z*se)
  up  <- PAF+ (z*se)
  
  out <- c(PAF = PAF,
           low = low,
           up  = up,
           var = Delta_Var_PAF)
  return(out)
}

greenland_paf_ci <- function(Tp, Pe, RR, Lower, Upper){
  PAF <- paf_single(Pe, RR)
  Var_Pe<- var_Pe(Tp,Pe)
  Var_beta <- var_beta_from_CI(RR, Lower, Upper) 
  
  O <- Pe / (1 - Pe)
  
  Green_Var_PAF <- ((O / Tp) * ((RR - 1)^2 + Var_beta * RR^2)+ Var_beta * O^2 *
                     RR^2) * (1 - PAF)^2 / (1 +O)^2
  
  se <- sqrt(Green_Var_PAF)
  z  <- qnorm(0.975)
  
  low <- 1-(1-PAF) * exp(z*se)
  up  <- 1-(1-PAF) * exp(-z*se)
  
  out <- c(PAF = PAF,
           low = low,
           up  = up,
           var = Green_Var_PAF)
  return(out)
}

mc_paf_ci <- function(Tp, Pe, RR, Lower, Upper, B= 10000){
  
  Var_beta <- var_beta_from_CI(RR, Lower, Upper) 
  
  beta<- log(RR)
  
  MonteRR <- rlnorm(B, meanlog = beta, sdlog = sqrt(Var_beta))
  
  MontePe <- rbinom(B, size = Tp, prob = Pe) / Tp
  
  MontePAF <- (MontePe * (MonteRR - 1)) / (1 + MontePe * (MonteRR -1))
  
  PAF_est <- median(MontePAF)   
  low <- as.numeric(quantile(MontePAF, 0.025))
  up <- as.numeric(quantile(MontePAF, 0.975))
  
  out <- c(PAF = PAF_est,
           low = low,
           up  = up)
  return(out)
}

Tp_A <- 1000
Pe_A <- 0.10
RR_A <- 1.2
Lower_A <- 1.0524696
Upper_A <- 1.368211

true_PAF_A <- paf_single(Pe_A, RR_A)

delta_A <- delta_paf_ci(Tp_A, Pe_A, RR_A, Lower_A, Upper_A)
green_A<- greenland_paf_ci(Tp_A, Pe_A, RR_A, Lower_A, Upper_A)
mc_A <- mc_paf_ci(Tp_A, Pe_A, RR_A, Lower_A, Upper_A, B = 10000)

scenarioA_results <- data.frame(
  method = c("Delta", "Greenland", "Monte Carlo"),
  PAF = c(delta_A["PAF"], green_A["PAF"], mc_A["PAF"]),
  lower = c(delta_A["low"], green_A["low"], mc_A["low"]),
  upper = c(delta_A["up"], green_A["up"], mc_A["up"])
)

Tp_B <- 100000
Pe_B <- 0.10
RR_B <- 5.0
Lower_B <- 4.3852901
Upper_B <- 5.700877

true_PAF_B <- paf_single(Pe_A, RR_A)

delta_B <- delta_paf_ci(Tp_B, Pe_B, RR_B, Lower_B, Upper_B)
green_B<- greenland_paf_ci(Tp_B, Pe_B, RR_B, Lower_B, Upper_B)
mc_B <- mc_paf_ci(Tp_B, Pe_B, RR_B, Lower_B, Upper_B, B = 10000)

scenarioB_results <- data.frame(
  method = c("Delta", "Greenland", "Monte Carlo"),
  PAF = c(delta_B["PAF"], green_B["PAF"], mc_B["PAF"]),
  lower = c(delta_B["low"], green_B["low"], mc_B["low"]),
  upper = c(delta_B["up"], green_B["up"], mc_B["up"])
)

one_sim <- function() {
  Var_beta_A <- var_beta_from_CI(RR_A, Lower_A, Upper_A)
  beta_A <- log(RR_A)
  RR_star <- rlnorm(1, meanlog = beta_A, sdlog = sqrt(Var_beta_A))
  Pe_star <- rbinom(1, size = Tp_A, prob = Pe_A) / Tp_A
  d <- delta_paf_ci(Tp_A, Pe_star, RR_star, Lower_A, Upper_A)
  g <- greenland_paf_ci(Tp_A, Pe_star, RR_star, Lower_A, Upper_A)
  m <- mc_paf_ci(Tp_A, Pe_star, RR_star, Lower_A, Upper_A, B = 2000)
  
  out <- c(
    delta_low = d["low"],
    delta_up= d["up"],
    green_low = g["low"],
    green_up = g["up"],
    mc_low = m["low"],
    mc_up = m["up"]
  )
  return(out)
}

n_sim <- 500

sims <- matrix(NA, nrow =n_sim, ncol = 6)
colnames(sims) <- c("delta_low", "delta_up",
                    "green_low", "green_up",
                    "mc_low","mc_up")

for (b in 1:n_sim) {
  sims[b, ] <- one_sim()
}
sims<- as.data.frame(sims)

delta_cover <- (sims$delta_low <= true_PAF_A) & (sims$delta_up>= true_PAF_A)
green_cover <- (sims$green_low <= true_PAF_A) & (sims$green_up>= true_PAF_A)
mc_cover<- (sims$mc_low <= true_PAF_A) & (sims$mc_up>= true_PAF_A)

delta_width <- sims$delta_up - sims$delta_low
green_width <- sims$green_up - sims$green_low
mc_width <- sims$mc_up - sims$mc_low

coverage_width_A <- data.frame(
  method = c("Delta", "Greenland", "Monte Carlo"),
  cover  = c(mean(delta_cover),
             mean(green_cover),
             mean(mc_cover)),
  width  = c(mean(delta_width),
             mean(green_width),
             mean(mc_width))
)
sims_B <- matrix(NA, nrow = n_sim, ncol = 6)
colnames(sims_B) <- c("delta_low", "delta_up",
                      "green_low", "green_up",
                      "mc_low", "mc_up")

for (b in 1:n_sim) {
  sims_B[b, ] <- one_sim()
}

sims_B <- as.data.frame(sims_B)

delta_cover_B <- (sims_B$delta_low <= true_PAF_B) & (sims_B$delta_up>= true_PAF_B)
green_cover_B <- (sims_B$green_low <= true_PAF_B) & (sims_B$green_up>= true_PAF_B)
mc_cover_B    <- (sims_B$mc_low <= true_PAF_B) & (sims_B$mc_up>= true_PAF_B)

delta_width_B <- sims_B$delta_up - sims_B$delta_low
green_width_B <- sims_B$green_up - sims_B$green_low
mc_width_B    <- sims_B$mc_up - sims_B$mc_low

coverage_width_B <- data.frame(
  method = c("Delta", "Greenland", "Monte Carlo"),
  cover  = c(mean(delta_cover_B),
             mean(green_cover_B),
             mean(mc_cover_B)),
  width  = c(mean(delta_width_B),
             mean(green_width_B),
             mean(mc_width_B))
)
delta_bad <- (sims$delta_low < 0) | (sims$delta_up > 1)
green_bad <- (sims$green_low < 0) | (sims$green_up > 1)
mc_bad  <- (sims$mc_low < 0) | (sims$mc_up > 1)

boundary_A <- data.frame(
  method  = c("Delta", "Greenland", "Monte Carlo"),
  prob_outside_01 = c(mean(delta_bad),
                      mean(green_bad),
                      mean(mc_bad))
)
delta_bad_B <- (sims_B$delta_low < 0) | (sims_B$delta_up > 1)
green_bad_B <- (sims_B$green_low < 0) | (sims_B$green_up > 1)
mc_bad_B  <- (sims_B$mc_low < 0) | (sims_B$mc_up > 1)

boundary_B <- data.frame(
  method  = c("Delta", "Greenland", "Monte Carlo"),
  prob_outside_01 = c(mean(delta_bad_B),
                      mean(green_bad_B),
                      mean(mc_bad_B))
)
