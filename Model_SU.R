

# Outcomes stored as a labeled list
outcomes_SU = list(
 Dominant = A_Dominant_SU, 
 Prestigous = A_Respect_SU, 
 Like = A_Like_SU,
 Fear = A_Fear_SU,
 Trust = A_Trusted_SU,
 Distrust = A_Untrusted_SU,
 Friend = A_Friend_SU, 
 Give = ifelse(A_Give_SU>0, 1, 0), 
 Take = ifelse(A_Leave_SU>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_SU>0, 1, 0)
)

# Merge data
dat_SU = make_strand_data(
 outcome = outcomes_SU,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on SU data
 fit_SU = fit_multiplex_model(
  data=dat_SU,
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

res_SU = summarize_strand_results(fit_SU)


colors = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)
colors = c(colors[1], "grey90", colors[3])

multiplex_plot(fit_SU, type="dyadic", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "SU_dyadic.pdf", height=14, width=22, palette=colors)
multiplex_plot(fit_SU, type="generalized", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "SU_generalized.pdf", height=14, width=22, palette=colors)


