# Create a matrix of male-male ties
Mask_BS_M = matrix(NA, nrow=nrow(A_Give_BS), ncol=ncol(A_Give_BS))
Mask_BS_F = matrix(NA, nrow=nrow(A_Give_BS), ncol=ncol(A_Give_BS))

rownames(Mask_BS_M) = rownames(Mask_BS_M) = rownames(A_Give_BS)
colnames(Mask_BS_M) = colnames(Mask_BS_M) = colnames(A_Give_BS)

rownames(Mask_BS_F) = rownames(Mask_BS_F) = rownames(A_Give_BS)
colnames(Mask_BS_F) = colnames(Mask_BS_F) = colnames(A_Give_BS)

for( i in 1:nrow(A_Give_BS)){
  for(j in 1:ncol(A_Give_BS)){
    Mask_BS_M[i,j] = ifelse(Gender_BS[i] == "M" & Gender_BS[j]=="M", 0, 1) # Anything except male-male dyads are masked
    Mask_BS_F[i,j] = ifelse(Gender_BS[i] == "F" & Gender_BS[j]=="F", 0, 1) # Anything except female-female dyads are masked
}}

diag(Mask_BS_M) = 1
diag(Mask_BS_F) = 1

# Outcomes stored as a labeled list
outcomes_BS = list(
 Dominant = A_Dominant_BS, 
 Prestigious = A_Respect_BS, 
 Like = A_Like_BS,
 Fear = A_Fear_BS,
 Trust = A_Trusted_BS,
 Distrust = A_Untrusted_BS,
 Friend = A_Friend_BS, 
 Give = ifelse(A_Give_BS>0, 1, 0), 
 Take = ifelse(A_Leave_BS>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_BS>0, 1, 0)
)

# Masks stored as a labeled list
masks_BS_M = list(
 Dominant = Mask_BS_M, 
 Prestigious = Mask_BS_M, 
 Like = Mask_BS_M,
 Fear = Mask_BS_M,
 Trust = Mask_BS_M,
 Distrust = Mask_BS_M,
 Friend = Mask_BS_M, 
 Give = Mask_BS_M, 
 Take = Mask_BS_M,
 Reduce = Mask_BS_M
)

masks_BS_F = list(
 Dominant = Mask_BS_F, 
 Prestigious = Mask_BS_F, 
 Like = Mask_BS_F,
 Fear = Mask_BS_F,
 Trust = Mask_BS_F,
 Distrust = Mask_BS_F,
 Friend = Mask_BS_F, 
 Give = Mask_BS_F, 
 Take = Mask_BS_F,
 Reduce = Mask_BS_F
)

# Merge data
dat_BS_M = make_strand_data(
 outcome = outcomes_BS,
 mask = masks_BS_M,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)

dat_BS_F = make_strand_data(
 outcome = outcomes_BS,
 mask = masks_BS_F,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on BS data
 fit_BS_M = fit_multiplex_model(
  data=dat_BS_M,
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

  fit_BS_F = fit_multiplex_model(
  data=dat_BS_F,
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


res_BS_M = summarize_strand_results(fit_BS_M)
res_BS_F = summarize_strand_results(fit_BS_F)
