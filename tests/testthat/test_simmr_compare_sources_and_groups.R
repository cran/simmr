set.seed(123)
library(vdiffr)
co <- function(expr) capture.output(expr, file = NULL)

# Single group version first
data("geese_data_day1")

simmr_1 <- with(
  geese_data_day1,
  simmr_load(
    mixtures = mixtures,
    source_names = source_names,
    source_means = source_means,
    source_sds = source_sds,
    correction_means = correction_means,
    correction_sds = correction_sds,
    concentration_means = concentration_means
  )
)
# MCMC run
co(simmr_1_out <- simmr_mcmc(simmr_1,
  mcmc_control = list(
    iter = 100,
    burn = 10,
    thin = 1,
    n.chain = 2
  )
))

# FFVB run
co(simmr_1_out_ffvb <- simmr_ffvb(simmr_1,
  ffvb_control = list(
    n_output = 3600,
    S = 10,
    P = 1,
    beta_1 = 0.9,
    beta_2 = 0.9,
    tau = 1000,
    eps_0 = 0.1,
    t_W = 1
  )
))

data("geese_data")

# Load into simmr
simmr_2 <- with(
  geese_data,
  simmr_load(
    mixtures = mixtures,
    source_names = source_names,
    source_means = source_means,
    source_sds = source_sds,
    correction_means = correction_means,
    correction_sds = correction_sds,
    concentration_means = concentration_means,
    group = groups
  )
)
# MCMC run
co(simmr_2_out <- simmr_mcmc(simmr_2,
  mcmc_control = list(
    iter = 100,
    burn = 10,
    thin = 1,
    n.chain = 2
  )
))

# FFVB run
co(simmr_2_out_ffvb <- simmr_ffvb(simmr_2,
  ffvb_control = list(
    n_output = 3600,
    S = 10,
    P = 1,
    beta_1 = 0.9,
    beta_2 = 0.9,
    tau = 1000,
    eps_0 = 0.1,
    t_W = 1
  )
))


# Multiple groups, 1 isotope
simmr_3 <- with(
  geese_data,
  simmr_load(
    mixtures = mixtures[, 1, drop = FALSE],
    source_names = source_names,
    source_means = source_means[, 1, drop = FALSE],
    source_sds = source_sds[, 1, drop = FALSE],
    correction_means = correction_means[, 1, drop = FALSE],
    correction_sds = correction_sds[, 1, drop = FALSE],
    concentration_means = concentration_means[, 1, drop = FALSE],
    group = groups
  )
)
# MCMC run
co(simmr_3_out <- simmr_mcmc(simmr_3,
  mcmc_control = list(
    iter = 100,
    burn = 10,
    thin = 1,
    n.chain = 2
  )
))

# FFVB run
co(simmr_3_out_ffvb <- simmr_ffvb(simmr_3,
  ffvb_control = list(
    n_output = 3600,
    S = 10,
    P = 1,
    beta_1 = 0.9,
    beta_2 = 0.9,
    tau = 1000,
    eps_0 = 0.1,
    t_W = 1
  )
))

test_that("Compare sources", {
  # 2 isotopes, 1 group, all sources
  co(cs1 <- compare_sources(simmr_1_out))
  expect_true(is.list(cs1))
  expect_true(is.matrix(cs1$out_all))
  expect_true(is.character(cs1$Ordering))
  # p <- cs1$plot + ylim(-1, 1)
  # expect_doppelganger("iso2_1_group_changed_ylim", p)

  # 2 isotopes, 1 group, two sources - change options
  co(cs2 <- compare_sources(simmr_1_out, source_names = simmr_1_out$input$source_names[1:2], plot = FALSE))
  expect_true(is.list(cs2))
  expect_true(is.vector(cs2[[1]]))
  expect_error(compare_sources(simmr_1_out, group = 2))

  # 2 isotopes, multiple groups, all sources
  co(cs3 <- compare_sources(simmr_2_out, group = 1))
  expect_true(is.list(cs3))
  expect_true(is.matrix(cs3$out_all))
  expect_true(is.character(cs3$Ordering))
  co(cs4 <- compare_sources(simmr_2_out, group = 2))
  expect_false(cs3$out_all[1, 1] == cs4$out_all[1, 1])
  # p <- cs3$plot + ylim(-1, 1)
  # expect_doppelganger("iso2_multi_group_changed_ylim", p)

  # 2 isotopes, multiple groups, 2 sources
  co(cs5 <- compare_sources(simmr_2_out,
    group = 1,
    source_names = simmr_2_out$input$source_names[1:2], plot = FALSE
  ))
  expect_true(is.list(cs5))
  expect_true(is.vector(cs5))
  co(cs6 <- compare_sources(simmr_2_out,
    group = 2,
    source_names = simmr_2_out$input$source_names[1:2], plot = FALSE
  ))
  expect_false(cs5[[1]][1] == cs6[[1]][1])

  # 1 isotope, multiple groups, all sources
  co(cs7 <- compare_sources(simmr_3_out, group = 1))
  expect_true(is.list(cs7))
  expect_true(is.matrix(cs7$out_all))
  expect_true(is.character(cs7$Ordering))
  co(cs8 <- compare_sources(simmr_3_out, group = 2))
  expect_false(cs7$out_all[1, 1] == cs8$out_all[1, 1])
  # p <- cs7$plot + ylim(-1, 1)
  # expect_doppelganger("iso2_multi_group_source_changed_ylim", p)

  # 1 isotopes, multiple groups, 2 sources
  co(cs9 <- compare_sources(simmr_3_out,
    group = 1,
    source_names = simmr_3_out$input$source_names[1:2], plot = FALSE
  ))
  expect_true(is.list(cs9))
  expect_true(is.vector(cs9))
  co(cs10 <- compare_sources(simmr_3_out,
    group = 2,
    source_names = simmr_3_out$input$source_names[1:2], plot = FALSE
  ))
  expect_false(cs9[[1]][1] == cs10[[1]][1])
})

# FFVB
test_that("Compare sources", {
  # 2 isotopes, 1 group, all sources
  co(cs1 <- compare_sources(simmr_1_out_ffvb))
  expect_true(is.list(cs1))
  expect_true(is.matrix(cs1$out_all))
  expect_true(is.character(cs1$Ordering))
  # p <- cs1$plot + ylim(-1, 1)
  # expect_doppelganger("iso2_1_group_changed_ylim_ffvb", p)

  # 2 isotopes, 1 group, two sources - change options
  co(cs2 <- compare_sources(simmr_1_out_ffvb, source_names = simmr_1_out_ffvb$input$source_names[1:2], plot = FALSE))
  expect_true(is.list(cs2))
  expect_true(is.vector(cs2[[1]]))
  expect_error(compare_sources(simmr_1_out, group = 2))

  # 2 isotopes, multiple groups, all sources
  co(cs3 <- compare_sources(simmr_2_out_ffvb, group = 1))
  expect_true(is.list(cs3))
  expect_true(is.matrix(cs3$out_all))
  expect_true(is.character(cs3$Ordering))
  co(cs4 <- compare_sources(simmr_2_out_ffvb, group = 2))
  expect_false(cs3$out_all[1, 1] == cs4$out_all[1, 1])
  # p <- cs3$plot + ylim(-1, 1)
  # expect_doppelganger("iso2_multi_group_changed_ylim_ffvb", p)

  # 2 isotopes, multiple groups, 2 sources
  co(cs5 <- compare_sources(simmr_2_out_ffvb,
    group = 1,
    source_names = simmr_2_out_ffvb$input$source_names[1:2], plot = FALSE
  ))
  expect_true(is.list(cs5))
  expect_true(is.vector(cs5))
  co(cs6 <- compare_sources(simmr_2_out_ffvb,
    group = 2,
    source_names = simmr_2_out_ffvb$input$source_names[1:2], plot = FALSE
  ))
  expect_false(cs5[[1]][1] == cs6[[1]][1])

  # 1 isotope, multiple groups, all sources
  co(cs7 <- compare_sources(simmr_3_out_ffvb, group = 1))
  expect_true(is.list(cs7))
  expect_true(is.matrix(cs7$out_all))
  expect_true(is.character(cs7$Ordering))
  co(cs8 <- compare_sources(simmr_3_out_ffvb, group = 2))
  expect_false(cs7$out_all[1, 1] == cs8$out_all[1, 1])
  # p <- cs7$plot + ylim(-1, 1)
  # expect_doppelganger("iso2_multi_group_source_changed_ylim_ffvb", p)

  # 1 isotopes, multiple groups, 2 sources
  co(cs9 <- compare_sources(simmr_3_out_ffvb,
    group = 1,
    source_names = simmr_3_out_ffvb$input$source_names[1:2], plot = FALSE
  ))
  expect_true(is.list(cs9))
  expect_true(is.vector(cs9))
  co(cs10 <- compare_sources(simmr_3_out_ffvb,
    group = 2,
    source_names = simmr_3_out_ffvb$input$source_names[1:2], plot = FALSE
  ))
  expect_false(cs9[[1]][1] == cs10[[1]][1])
})

test_that("Compare groups", {
  # 1 group version should give an error
  expect_error(compare_groups(simmr_1_out))

  # 2 isotopes, all groups
  co(cg1 <- compare_groups(simmr_2_out, groups = 1:simmr_2$n_groups))
  expect_true(is.list(cg1))
  expect_true(is.matrix(cg1$out_all))
  expect_true(is.character(cg1$Ordering))
  expect_error(compare_groups(simmr_2_out, groups = 1))
  expect_error(compare_groups(simmr_2_out, groups = NA))
  expect_error(compare_groups(simmr_2_out,
    source_name = "Dracula"
  ))

  co(cg2 <- compare_groups(simmr_2_out, groups = 2:4))
  expect_false(cg1$out_all[1, 1] == cg2$out_all[1, 1])
  # p <- cg1$plot + ylim(-1, 1)
  # expect_doppelganger("compare_groups_1", p)

  # 2 isotopes, 2 groups
  co(cg3 <- compare_groups(simmr_2_out,
    groups = c(1, 3),
    source_name = simmr_2_out$input$source_names[2],
    plot = FALSE
  ))
  expect_true(is.list(cg3))
  expect_true(is.vector(cg3))
  co(cg4 <- compare_groups(simmr_2_out,
    groups = c(2, 4),
    source_name = simmr_2_out$input$source_names[2],
    plot = FALSE
  ))
  expect_false(cg3[[1]][1] == cg4[[1]][1])

  # 1 isotope, multiple groups
  co(cg5 <- compare_groups(simmr_3_out, groups = 1:simmr_3$n_groups))
  expect_true(is.list(cg5))
  expect_true(is.matrix(cg5$out_all))
  expect_true(is.character(cg5$Ordering))
  co(cg6 <- compare_groups(simmr_3_out, groups = 2:simmr_3$n_groups))
  expect_false(cg5$out_all[1, 1] == cg6$out_all[1, 1])
  # p <- cg5$plot + ylim(-1, 1)
  # expect_doppelganger("iso1_multi_group", p)

  # 1 isotopes, 2 groups
  co(cg7 <- compare_groups(simmr_3_out,
    groups = 3:4,
    source_name = simmr_3_out$input$source_names[3],
    plot = FALSE
  ))
  expect_true(is.list(cg7))
  expect_true(is.vector(cg7))
  co(cg8 <- compare_groups(simmr_3_out,
    group = 4:3, # Swap the groups around - should be negative
    source_name = simmr_3_out$input$source_names[3],
    plot = FALSE
  ))
  expect_true(cg7[[1]][1] == -cg8[[1]][1])
})


test_that("Compare groups", {
  # 1 group version should give an error
  expect_error(compare_groups(simmr_1_out_ffvb))

  # 2 isotopes, all groups
  co(cg1 <- compare_groups(simmr_2_out_ffvb, groups = 1:simmr_2_out_ffvb$input$n_groups))
  expect_true(is.list(cg1))
  expect_true(is.matrix(cg1$out_all))
  expect_true(is.character(cg1$Ordering))
  expect_error(compare_groups(simmr_2_out_ffvb, groups = 1))
  expect_error(compare_groups(simmr_2_out_ffvb, groups = NA))
  expect_error(compare_groups(simmr_2_out_ffvb,
    source_name = "Dracula"
  ))

  co(cg2 <- compare_groups(simmr_2_out_ffvb, groups = 2:4))
  expect_false(cg1$out_all[1, 1] == cg2$out_all[1, 1])
  # p <- cg1$plot + ylim(-1, 1)
  # expect_doppelganger("compare_groups_1_ffvb", p)

  # 2 isotopes, 2 groups
  co(cg3 <- compare_groups(simmr_2_out_ffvb,
    groups = c(1, 3),
    source_name = simmr_2_out_ffvb$input$source_names[2],
    plot = FALSE
  ))
  expect_true(is.list(cg3))
  expect_true(is.vector(cg3))
  co(cg4 <- compare_groups(simmr_2_out_ffvb,
    groups = c(2, 4),
    source_name = simmr_2_out_ffvb$input$source_names[2],
    plot = FALSE
  ))
  expect_false(cg3[[1]][1] == cg4[[1]][1])

  # 1 isotope, multiple groups
  co(cg5 <- compare_groups(simmr_3_out_ffvb, groups = 1:simmr_3_out_ffvb$input$n_groups))
  expect_true(is.list(cg5))
  expect_true(is.matrix(cg5$out_all))
  expect_true(is.character(cg5$Ordering))
  co(cg6 <- compare_groups(simmr_3_out, groups = 2:simmr_3_out$input$n_groups))
  expect_false(cg5$out_all[1, 1] == cg6$out_all[1, 1])
  # p <- cg5$plot + ylim(-1, 1)
  # expect_doppelganger("iso1_multi_group_ffvb", p)

  # 1 isotopes, 2 groups
  co(cg7 <- compare_groups(simmr_3_out_ffvb,
    groups = 3:4,
    source_name = simmr_3_out_ffvb$input$source_names[3],
    plot = FALSE
  ))
  expect_true(is.list(cg7))
  expect_true(is.vector(cg7))
  co(cg8 <- compare_groups(simmr_3_out_ffvb,
    group = 4:3, # Swap the groups around - should be negative
    source_name = simmr_3_out_ffvb$input$source_names[3],
    plot = FALSE
  ))
  expect_true(cg7[[1]][1] == -cg8[[1]][1])
})
