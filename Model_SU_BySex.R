# Create a matrix of male-male ties
Mask_SU_M = matrix(NA, nrow=nrow(A_Give_SU), ncol=ncol(A_Give_SU))
Mask_SU_F = matrix(NA, nrow=nrow(A_Give_SU), ncol=ncol(A_Give_SU))

rownames(Mask_SU_M) = rownames(Mask_SU_M) = rownames(A_Give_SU)
colnames(Mask_SU_M) = colnames(Mask_SU_M) = colnames(A_Give_SU)

rownames(Mask_SU_F) = rownames(Mask_SU_F) = rownames(A_Give_SU)
colnames(Mask_SU_F) = colnames(Mask_SU_F) = colnames(A_Give_SU)

for( i in 1:nrow(A_Give_SU)){
  for(j in 1:ncol(A_Give_SU)){
    Mask_SU_M[i,j] = ifelse(Gender_SU[i] == "M" & Gender_SU[j]=="M", 0, 1) # Anything except male-male dyads are masked
    Mask_SU_F[i,j] = ifelse(Gender_SU[i] == "F" & Gender_SU[j]=="F", 0, 1) # Anything except female-female dyads are masked
}}

diag(Mask_SU_M) = 1
diag(Mask_SU_F) = 1

# Outcomes stored as a labeled list
outcomes_SU = list(
 Dominant = A_Dominant_SU, 
 Prestigious = A_Respect_SU, 
 Like = A_Like_SU,
 Fear = A_Fear_SU,
 Trust = A_Trusted_SU,
 Distrust = A_Untrusted_SU,
 Friend = A_Friend_SU, 
 Give = ifelse(A_Give_SU>0, 1, 0), 
 Take = ifelse(A_Leave_SU>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_SU>0, 1, 0)
)

# Masks stored as a labeled list
masks_SU_M = list(
 Dominant = Mask_SU_M, 
 Prestigious = Mask_SU_M, 
 Like = Mask_SU_M,
 Fear = Mask_SU_M,
 Trust = Mask_SU_M,
 Distrust = Mask_SU_M,
 Friend = Mask_SU_M, 
 Give = Mask_SU_M, 
 Take = Mask_SU_M,
 Reduce = Mask_SU_M
)

masks_SU_F = list(
 Dominant = Mask_SU_F, 
 Prestigious = Mask_SU_F, 
 Like = Mask_SU_F,
 Fear = Mask_SU_F,
 Trust = Mask_SU_F,
 Distrust = Mask_SU_F,
 Friend = Mask_SU_F, 
 Give = Mask_SU_F, 
 Take = Mask_SU_F,
 Reduce = Mask_SU_F
)

# Merge data
dat_SU_M = make_strand_data(
 outcome = outcomes_SU,
 mask = masks_SU_M,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)

dat_SU_F = make_strand_data(
 outcome = outcomes_SU,
 mask = masks_SU_F,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on SU data
 fit_SU_M = fit_multiplex_model(
  data=dat_SU_M,
  block_regression = ~ 1,
  focal_regression = ~ 1,
  target_regression = ~ 1,
  dyad_regression = ~ 1,
  mode="mcmc",
  mcmc_parameters = list(
    chains = 1,
    parallel_chains = 1,
    refresh = 1,
    iter_warmup = 1000,
    iter_sampling = 1000,
    max_treedepth = 12,
    adapt_delta = 0.96)
 )

  fit_SU_F = fit_multiplex_model(
  data=dat_SU_F,
  block_regression = ~ 1,
  focal_regression = ~ 1,
  target_regression = ~ 1,
  dyad_regression = ~ 1,
  mode="mcmc",
  mcmc_parameters = list(
    chains = 1,
    parallel_chains = 1,
    refresh = 1,
    iter_warmup = 1000,
    iter_sampling = 1000,
    max_treedepth = 12,
    adapt_delta = 0.96)
 )




res_SU_M = summarize_strand_results(fit_SU_M)
res_SU_F = summarize_strand_results(fit_SU_F)

