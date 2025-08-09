

# Outcomes stored as a labeled list
outcomes_BS = list(
 Dominant = A_Dominant_BS, 
 Prestigous = A_Respect_BS, 
 Like = A_Like_BS,
 Fear = A_Fear_BS,
 Trust = A_Trusted_BS,
 Distrust = A_Untrusted_BS,
 Friend = A_Friend_BS, 
 Give = ifelse(A_Give_BS>0, 1, 0), 
 Take = ifelse(A_Leave_BS>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_BS>0, 1, 0)
)

# Merge data
dat_BS = make_strand_data(
 outcome = outcomes_BS,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on BS data
 fit_BS = fit_multiplex_model(
  data=dat_BS,
  block_regression = ~ 1,
  focal_regression = ~ 1,
  target_regression = ~ 1,
  dyad_regression = ~ 1,
  mode="mcmc",
  stan_mcmc_parameters = list(
    chains = 1,
    parallel_chains = 1,
    refresh = 1,
    iter_warmup = 1000,
    iter_sampling = 1000,
    max_treedepth = 12,
    adapt_delta = 0.96)
 )

res_BS = summarize_strand_results(fit_BS)


colors = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)
colors = c(colors[1], "grey90", colors[3])

multiplex_plot(fit_BS, type="dyadic", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "BS_dyadic.pdf", height=6, width=7, palette=colors)
multiplex_plot(fit_BS, type="generalized", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "BS_generalized.pdf", height=6, width=7, palette=colors)
