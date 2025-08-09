

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

# Merge data
dat_SC = make_strand_data(
 outcome = outcomes_SC,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on SC data
 fit_SC = fit_multiplex_model(
  data=dat_SC,
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

res_SC = summarize_strand_results(fit_SC)


colors = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)
colors = c(colors[1], "grey90", colors[3])

multiplex_plot(fit_SC, type="dyadic", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "SC_dyadic.pdf", height=6, width=7, palette=colors)
multiplex_plot(fit_SC, type="generalized", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "SC_generalized.pdf", height=6, width=7, palette=colors)
