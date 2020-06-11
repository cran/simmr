## ----eval = FALSE-------------------------------------------------------------
#  install.packages('simmr')

## ----eval = FALSE-------------------------------------------------------------
#  library(simmr)

## -----------------------------------------------------------------------------
mix = matrix(c(-10.13, -10.72, -11.39, -11.18, -10.81, -10.7, -10.54, 
               -10.48, -9.93, -9.37, 11.59, 11.01, 10.59, 10.97, 11.52, 11.89, 
               11.73, 10.89, 11.05, 12.3), ncol = 2, nrow = 10)
colnames(mix) = c('d13C','d15N')
s_names = c("Zostera", "Grass", "U.lactuca", "Enteromorpha")
s_means = matrix(c(-14, -15.1, -11.03, -14.44, 3.06, 7.05, 13.72, 5.96), ncol = 2, nrow = 4)
s_sds = matrix(c(0.48, 0.38, 0.48, 0.43, 0.46, 0.39, 0.42, 0.48), ncol = 2, nrow = 4)
c_means = matrix(c(2.63, 1.59, 3.41, 3.04, 3.28, 2.34, 2.14, 2.36), ncol = 2, nrow = 4)
c_sds = matrix(c(0.41, 0.44, 0.34, 0.46, 0.46, 0.48, 0.46, 0.66), ncol = 2, nrow = 4)
conc = matrix(c(0.02, 0.1, 0.12, 0.04, 0.02, 0.1, 0.09, 0.05), ncol = 2, nrow = 4)

## ----include = FALSE----------------------------------------------------------
library(simmr)

## -----------------------------------------------------------------------------
simmr_in = simmr_load(mixtures = mix,
                     source_names = s_names,
                     source_means = s_means,
                     source_sds = s_sds,
                     correction_means = c_means,
                     correction_sds = c_sds,
                     concentration_means = conc)

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
plot(simmr_in)

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
plot(simmr_in, 
     xlab = expression(paste(delta^13, "C (\u2030)",
                             sep = "")), 
     ylab = expression(paste(delta^15, "N (\u2030)",
                             sep = "")), 
     title = 'Isospace plot of example data')

## ----results = 'hide'---------------------------------------------------------
simmr_out = simmr_mcmc(simmr_in)

## -----------------------------------------------------------------------------
summary(simmr_out, type = 'diagnostics')

## ----results = 'hide'---------------------------------------------------------
post_pred = posterior_predictive(simmr_out)
print(post_pred)

## ----results = 'hide'---------------------------------------------------------
prior_viz(simmr_out)

## -----------------------------------------------------------------------------
summary(simmr_out, type = 'statistics')
summary(simmr_out, type = 'quantiles')

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
plot(simmr_out, type = 'density')

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
plot(simmr_out, type = 'matrix')

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
compare_sources(simmr_out, 
                source_names = c('Zostera','U.lactuca'))

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
compare_sources(simmr_out,
                source_names = c('Zostera',
                                 'U.lactuca',
                                 'Enteromorpha'))

## -----------------------------------------------------------------------------
data(geese_data)

## -----------------------------------------------------------------------------
simmr_groups = with(geese_data, 
                    simmr_load(mixtures = mixtures[,1:2],
                               source_names = source_names,
                               source_means = source_means,
                               source_sds = source_sds,
                               correction_means = correction_means,
                               correction_sds = correction_sds,
                               concentration_means = concentration_means,
                               group = as.factor(paste('period', 
                                                     mixtures[,3]))))

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
plot(simmr_groups,
     group = 1:8,
     xlab = expression(paste(delta^13, "C (\u2030)",
                             sep = "")), 
     ylab = expression(paste(delta^15, "N (\u2030)",
                             sep = "")), 
     title = 'Isospace plot of Inger et al Geese data',
     mix_name = 'Geese')

## ----results = 'hide'---------------------------------------------------------
simmr_groups_out = simmr_mcmc(simmr_groups)

## ----reults = 'hide'----------------------------------------------------------
summary(simmr_groups_out, 
        type = 'quantiles', 
        group = 1)
summary(simmr_groups_out, 
        type = 'quantiles',
        group = c(1,3))
summary(simmr_groups_out, 
        type = c('quantiles','statistics'),
        group = c(1,3))

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
plot(simmr_groups_out,
     type = 'boxplot',
     group = 2,
     title = 'simmr output group 2')
plot(simmr_groups_out,
     type = c('density','matrix'),
     group = 6,
     title = 'simmr output group 6')

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
compare_groups(simmr_groups_out,
               source = 'Zostera',
               groups = 1:2)

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
compare_groups(simmr_groups_out,
               source = 'Zostera',
               groups = 1:3)

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
simmr_out_combine = combine_sources(simmr_out,
                                    to_combine = c('U.lactuca',
                                                   'Enteromorpha'),
                                    new_source_name = 'U.lac+Ent')
plot(simmr_out_combine$input)
plot(simmr_out_combine,
     type = 'boxplot',
     title = 'simmr output: combined sources')

## ----fig.align = 'center',fig.width = 7, fig.height = 5-----------------------
simmr_groups_out_combine = combine_sources(simmr_groups_out,
                                           to_combine = c('U.lactuca',
                                                          'Enteromorpha'),
                                           new_source_name = 'U.Lac+Ent')
plot(simmr_groups_out_combine$input, 
     group = 1:8)
plot(simmr_groups_out_combine,
     type = 'boxplot',
     title = 'simmr output: combined sources',
     group = 8)
plot(simmr_groups_out_combine,
     type = 'matrix',
     title = 'simmr output: combined sources',
     group = 8)

# And we can now compare sources across groups on this new data set
compare_groups(simmr_groups_out_combine, 
               source = 'U.Lac+Ent', 
               group = 1:3)

## -----------------------------------------------------------------------------
mix = matrix(c(-10.13, -10.72, -11.39, -11.18, -10.81, -10.7, -10.54, 
               -10.48, -9.93, -9.37), ncol = 1, nrow = 10)
colnames(mix) = c('d13C')
s_names = c("Zostera", "Grass", "U.lactuca", "Enteromorpha")
s_means = matrix(c(-14, -15.1, -11.03, -14.44), ncol = 1, nrow = 4)
s_sds = matrix(c(0.48, 0.38, 0.48, 0.43), ncol = 1, nrow = 4)
c_means = matrix(c(2.63, 1.59, 3.41, 3.04), ncol = 1, nrow = 4)
c_sds = matrix(c(0.41, 0.44, 0.34, 0.46), ncol = 1, nrow = 4)
conc = matrix(c(0.02, 0.1, 0.12, 0.04), ncol = 1, nrow = 4)

## -----------------------------------------------------------------------------
simmr_in_1D = simmr_load(mixtures = mix,
                     source_names = s_names,
                     source_means = s_means,
                     source_sds = s_sds,
                     correction_means = c_means,
                     correction_sds = c_sds,
                     concentration_means =   conc)

## ----results = 'hide'---------------------------------------------------------
plot(simmr_in_1D)

## -----------------------------------------------------------------------------
simmr_run_1D = simmr_mcmc(simmr_in_1D)

## -----------------------------------------------------------------------------
plot(simmr_run_1D, type = 'boxplot')

## -----------------------------------------------------------------------------
summary(simmr_out, type = 'quantiles')

## -----------------------------------------------------------------------------
proportion_means = c(0.4,0.3,0.2,0.1)

## -----------------------------------------------------------------------------
proportion_sds = c(0.08,0.02,0.01,0.02)

## -----------------------------------------------------------------------------
prior = simmr_elicit(4, proportion_means,
                     proportion_sds)

## -----------------------------------------------------------------------------
simmr_out_informative = simmr_mcmc(simmr_in,
                                   prior_control = 
                                     list(means = prior$mean,
                                          sd = prior$sd))

## -----------------------------------------------------------------------------
summary(simmr_out_informative, 
        type = 'quantiles')

## -----------------------------------------------------------------------------
prior_viz(simmr_out_informative)

## -----------------------------------------------------------------------------
simmr_tdf = simmr_load(mixtures = mix,
                       source_names = s_names,
                       source_means = s_means,
                       source_sds = s_sds,
                       concentration_means = conc)

## -----------------------------------------------------------------------------
plot(simmr_tdf)

## -----------------------------------------------------------------------------
p_known = matrix(rep(1/simmr_tdf$n_sources,
                     simmr_tdf$n_sources),
                 ncol = simmr_tdf$n_sources,
                 nrow = simmr_tdf$n_obs, 
                 byrow = TRUE)

## ---- results = 'hide'--------------------------------------------------------
simmr_tdf_out = simmr_mcmc_tdf(simmr_tdf, 
                               p = p_known)

## -----------------------------------------------------------------------------
summary(simmr_tdf_out,type = 'diagnostics')

## -----------------------------------------------------------------------------
summary(simmr_tdf_out,type = 'quantiles')

## -----------------------------------------------------------------------------
simmr_tdf_2 = simmr_load(mixtures = mix,
                         source_names = s_names,
                         source_means = s_means,
                         source_sds = s_sds,
                         correction_means =   simmr_tdf_out$c_mean_est,
                         correction_sds = simmr_tdf_out$c_sd_est,
                         concentration_means = conc)
plot(simmr_tdf_2)

## ----eval = FALSE-------------------------------------------------------------
#  str(simmr_in)

## -----------------------------------------------------------------------------
mean(simmr_out$output$`1`$BUGSoutput$sims.list$p[,'Zostera'])

## -----------------------------------------------------------------------------
mean(simmr_out$output$`1`$BUGSoutput$sims.list$p[,'Zostera']
     > simmr_out$output$`1`$BUGSoutput$sims.list$p[,'Grass'])

