# Create a matrix of male-male ties
Mask_TB_M = matrix(NA, nrow=nrow(A_Give_TB), ncol=ncol(A_Give_TB))
Mask_TB_F = matrix(NA, nrow=nrow(A_Give_TB), ncol=ncol(A_Give_TB))

rownames(Mask_TB_M) = rownames(Mask_TB_M) = rownames(A_Give_TB)
colnames(Mask_TB_M) = colnames(Mask_TB_M) = colnames(A_Give_TB)

rownames(Mask_TB_F) = rownames(Mask_TB_F) = rownames(A_Give_TB)
colnames(Mask_TB_F) = colnames(Mask_TB_F) = colnames(A_Give_TB)

for( i in 1:nrow(A_Give_TB)){
  for(j in 1:ncol(A_Give_TB)){
    Mask_TB_M[i,j] = ifelse(Gender_TB[i] == "M" & Gender_TB[j]=="M", 0, 1) # Anything except male-male dyads are masked
    Mask_TB_F[i,j] = ifelse(Gender_TB[i] == "F" & Gender_TB[j]=="F", 0, 1) # Anything except female-female dyads are masked
}}

diag(Mask_TB_M) = 1
diag(Mask_TB_F) = 1

# Outcomes stored as a labeled list
outcomes_TB = list(
 Dominant = A_Dominant_TB, 
 Prestigous = A_Respect_TB, 
 Like = A_Like_TB,
 Fear = A_Fear_TB,
 Trust = A_Trusted_TB,
 Distrust = A_Untrusted_TB,
 Friend = A_Friend_TB, 
 Give = ifelse(A_Give_TB>0, 1, 0), 
 Take = ifelse(A_Leave_TB>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_TB>0, 1, 0)
)

# Masks stored as a labeled list
masks_TB_M = list(
 Dominant = Mask_TB_M, 
 Prestigous = Mask_TB_M, 
 Like = Mask_TB_M,
 Fear = Mask_TB_M,
 Trust = Mask_TB_M,
 Distrust = Mask_TB_M,
 Friend = Mask_TB_M, 
 Give = Mask_TB_M, 
 Take = Mask_TB_M,
 Reduce = Mask_TB_M
)

masks_TB_F = list(
 Dominant = Mask_TB_F, 
 Prestigous = Mask_TB_F, 
 Like = Mask_TB_F,
 Fear = Mask_TB_F,
 Trust = Mask_TB_F,
 Distrust = Mask_TB_F,
 Friend = Mask_TB_F, 
 Give = Mask_TB_F, 
 Take = Mask_TB_F,
 Reduce = Mask_TB_F
)

# Merge data
dat_TB_M = make_strand_data(
 outcome = outcomes_TB,
 mask = masks_TB_M,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)

dat_TB_F = make_strand_data(
 outcome = outcomes_TB,
 mask = masks_TB_F,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on SU data
 fit_TB_M = fit_multiplex_model(
  data=dat_TB_M,
  block_regression = ~ 1,
  focal_regression = ~ 1,
  target_regression = ~ 1,
  dyad_regression = ~ 1,
  mode="mcmc",
  stan_mcmc_parameters = list(
    chains = 1,
    parallel_chains = 1,
    refresh = 1,
    iter_warmup = 500,
    iter_sampling = 500,
    max_treedepth = 12,
    adapt_delta = 0.96)
 )

  fit_TB_F = fit_multiplex_model(
  data=dat_TB_F,
  block_regression = ~ 1,
  focal_regression = ~ 1,
  target_regression = ~ 1,
  dyad_regression = ~ 1,
  mode="mcmc",
  stan_mcmc_parameters = list(
    chains = 1,
    parallel_chains = 1,
    refresh = 1,
    iter_warmup = 500,
    iter_sampling = 500,
    max_treedepth = 12,
    adapt_delta = 0.96)
 )


res_TB_M = summarize_strand_results(fit_TB_M)
res_TB_F = summarize_strand_results(fit_TB_F)

