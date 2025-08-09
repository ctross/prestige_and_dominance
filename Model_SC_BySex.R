# Create a matrix of male-male ties
Mask_SC_M = matrix(NA, nrow=nrow(A_Give_SC), ncol=ncol(A_Give_SC))
Mask_SC_F = matrix(NA, nrow=nrow(A_Give_SC), ncol=ncol(A_Give_SC))

rownames(Mask_SC_M) = rownames(Mask_SC_M) = rownames(A_Give_SC)
colnames(Mask_SC_M) = colnames(Mask_SC_M) = colnames(A_Give_SC)

rownames(Mask_SC_F) = rownames(Mask_SC_F) = rownames(A_Give_SC)
colnames(Mask_SC_F) = colnames(Mask_SC_F) = colnames(A_Give_SC)

for( i in 1:nrow(A_Give_SC)){
  for(j in 1:ncol(A_Give_SC)){
    Mask_SC_M[i,j] = ifelse(Gender_SC[i] == "M" & Gender_SC[j]=="M", 0, 1) # Anything except male-male dyads are masked
    Mask_SC_F[i,j] = ifelse(Gender_SC[i] == "F" & Gender_SC[j]=="F", 0, 1) # Anything except female-female dyads are masked
}}

diag(Mask_SC_M) = 1
diag(Mask_SC_F) = 1

# Outcomes stored as a labeled list
outcomes_SC = list(
 Dominant = A_Dominant_SC, 
 Prestigous = A_Respect_SC, 
 Like = A_Like_SC,
 Fear = A_Fear_SC,
 Trust = A_Trusted_SC,
 Distrust = A_Untrusted_SC,
 Friend = A_Friend_SC, 
 Give = ifelse(A_Give_SC>0, 1, 0), 
 Take = ifelse(A_Leave_SC>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_SC>0, 1, 0)
)

# Masks stored as a labeled list
masks_SC_M = list(
 Dominant = Mask_SC_M, 
 Prestigous = Mask_SC_M, 
 Like = Mask_SC_M,
 Fear = Mask_SC_M,
 Trust = Mask_SC_M,
 Distrust = Mask_SC_M,
 Friend = Mask_SC_M, 
 Give = Mask_SC_M, 
 Take = Mask_SC_M,
 Reduce = Mask_SC_M
)

masks_SC_F = list(
 Dominant = Mask_SC_F, 
 Prestigous = Mask_SC_F, 
 Like = Mask_SC_F,
 Fear = Mask_SC_F,
 Trust = Mask_SC_F,
 Distrust = Mask_SC_F,
 Friend = Mask_SC_F, 
 Give = Mask_SC_F, 
 Take = Mask_SC_F,
 Reduce = Mask_SC_F
)

# Merge data
dat_SC_M = make_strand_data(
 outcome = outcomes_SC,
 mask = masks_SC_M,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)

dat_SC_F = make_strand_data(
 outcome = outcomes_SC,
 mask = masks_SC_F,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on SU data
 fit_SC_M = fit_multiplex_model(
  data=dat_SC_M,
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

  fit_SC_F = fit_multiplex_model(
  data=dat_SC_F,
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


res_SC_M = summarize_strand_results(fit_SC_M)
res_SC_F = summarize_strand_results(fit_SC_F)
