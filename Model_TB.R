

# Outcomes stored as a labeled list
outcomes_TB = list(
 Dominant = A_Dominant_TB, 
 Prestigious = A_Respect_TB, 
 Like = A_Like_TB,
 Fear = A_Fear_TB,
 Trust = A_Trusted_TB,
 Distrust = A_Untrusted_TB,
 Friend = A_Friend_TB, 
 Give = ifelse(A_Give_TB>0, 1, 0), 
 Take = ifelse(A_Leave_TB>0, 0, 1), # Leaving is reverse-coded to Taking
 Reduce = ifelse(A_Reduce_TB>0, 1, 0)
)

# Merge data
dat_TB = make_strand_data(
 outcome = outcomes_TB,
 block_covariates = NULL, 
 individual_covariates = NULL, 
 dyadic_covariates = NULL,
 outcome_mode="bernoulli",
 link_mode="logit",
 multiplex = TRUE
)


# Run model on TB data
 fit_TB = fit_multiplex_model(
  data=dat_TB,
  block_regression = ~ 1,
  focal_regression = ~ 1,
  target_regression = ~ 1,
  dyad_regression = ~ 1,
  mode="mcmc",
  mcmc_parameters = list(
    chains = 2,
    parallel_chains = 2,
    refresh = 1,
    iter_warmup = 1000,
    iter_sampling = 2000,
    max_treedepth = 12,
    adapt_delta = 0.96)
 )

res_TB = summarize_strand_results(fit_TB)


colors = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)
colors = c(colors[1], "grey90", colors[3])

multiplex_plot(fit_TB, type="dyadic", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "TB_dyadic.pdf", height=14, width=22, palette=colors)
multiplex_plot(fit_TB, type="generalized", HPDI=0.9, plot = TRUE, export_as_table = FALSE, save_plot = "TB_generalized.pdf", height=14, width=22, palette=colors)

