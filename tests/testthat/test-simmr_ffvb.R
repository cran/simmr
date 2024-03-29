set.seed(123)
co <- function(expr) capture.output(expr, file = NULL)

mix <- matrix(c(
  -10.13, -10.72, -11.39, -11.18, -10.81, -10.7, -10.54,
  -10.48, -9.93, -9.37, 11.59, 11.01, 10.59, 10.97, 11.52, 11.89,
  11.73, 10.89, 11.05, 12.3
), ncol = 2, nrow = 10)
colnames(mix) <- c("d13C", "d15N")
s_names <- c("Source A", "Source B", "Source C", "Source D")
s_means <- matrix(c(-14, -15.1, -11.03, -14.44, 3.06, 7.05, 13.72, 5.96), ncol = 2, nrow = 4)
s_sds <- matrix(c(0.48, 0.38, 0.48, 0.43, 0.46, 0.39, 0.42, 0.48), ncol = 2, nrow = 4)
c_means <- matrix(c(2.63, 1.59, 3.41, 3.04, 3.28, 2.34, 2.14, 2.36), ncol = 2, nrow = 4)
c_sds <- matrix(c(0.41, 0.44, 0.34, 0.46, 0.46, 0.48, 0.46, 0.66), ncol = 2, nrow = 4)
conc <- matrix(c(0.02, 0.1, 0.12, 0.04, 0.02, 0.1, 0.09, 0.05), ncol = 2, nrow = 4)


# Taken from the simmr_mcmc example
test_that("simmr_ffvb_simplest", {
  # Load into simmr
  simmr_1 <- simmr_load(
    mixtures = mix,
    source_names = s_names,
    source_means = s_means,
    source_sds = s_sds,
    correction_means = c_means,
    correction_sds = c_sds,
    concentration_means = conc
  )
  # FFVB run
  co(simmr_1_out <- simmr_ffvb(simmr_1))

  expect_s3_class(simmr_1_out, "simmr_output")
})

# One observation
# Taken from the simmr_mcmc example
test_that("simmr_ffvb_1obs", {
  simmr_2 <- simmr_load(
    mixtures = mix[1, , drop = FALSE], # drop required to keep the mixtures as a matrix
    source_names = s_names,
    source_means = s_means,
    source_sds = s_sds,
    correction_means = c_means,
    correction_sds = c_sds,
    concentration_means = conc
  )


  # MCMC run - automatically detects the single observation
  co(simmr_2_out <- simmr_ffvb(simmr_2))
  expect_s3_class(simmr_2_out, "simmr_output")
})

# Test that the VB version is similar (but not identical) to the MCMC version
test_that("simmr_ffvb_compare", {
  # Load into simmr
  simmr_1 <- simmr_load(
    mixtures = mix,
    source_names = s_names,
    source_means = s_means,
    source_sds = s_sds,
    correction_means = c_means,
    correction_sds = c_sds,
    concentration_means = conc
  )
  # MCMC run
  co(simmr_1_vb <- simmr_ffvb(simmr_1))
  co(simmr_1_mcmc <- simmr_mcmc(simmr_1))
  
  expect_s3_class(simmr_1_vb, "simmr_output")
  expect_s3_class(simmr_1_mcmc, "simmr_output")
  
  # Compare the two output p values
  p_mcmc <- simmr_1_mcmc$output[[1]]$BUGSoutput$mean$p
  p_vb <- colMeans(simmr_1_vb$output[[1]]$BUGSoutput$sims.list$p)
  
  # Define the difference between them and some kind of tolerance
  diff_p <- sum((p_mcmc - p_vb)^2)
  assert_true(diff_p < 0.03)
  
  # Get the sigmas
  sig_mcmc <- simmr_1_mcmc$output[[1]]$BUGSoutput$mean$sigma
  sig_vb <- colMeans(simmr_1_vb$output[[1]]$BUGSoutput$sims.list$sigma)
  diff_sig <- sum((sig_mcmc - sig_vb)^2)
  assert_true(diff_sig < 0.01)
  
})
