## ---- eval = FALSE------------------------------------------------------------
#  install.packages("simmr")

## ---- message=FALSE-----------------------------------------------------------
library(simmr)

## ---- eval = FALSE------------------------------------------------------------
#  system.file("extdata", "geese_data.xls", package = "simmr")

## ---- echo = FALSE------------------------------------------------------------
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("readxl needed for this vignette to work. Please install it.",
    call. = FALSE
  )
}

## -----------------------------------------------------------------------------
library(readxl)
path <- system.file("extdata", "geese_data.xls", package = "simmr")
geese_data <- lapply(excel_sheets(path), read_excel, path = path)

## -----------------------------------------------------------------------------
targets <- geese_data[[1]]
sources <- geese_data[[2]]
TEFs <- geese_data[[3]]
concdep <- geese_data[[4]]

## -----------------------------------------------------------------------------
geese_simmr <- simmr_load(
  mixtures = targets[, 1:2],
  source_names = sources$Sources,
  source_means = sources[, 2:3],
  source_sds = sources[, 4:5],
  correction_means = TEFs[, 2:3],
  correction_sds = TEFs[, 4:5],
  concentration_means = concdep[, 2:3],
  group = as.factor(paste("Day", targets$Time))
)

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
plot(geese_simmr, group = 1:8)

## ---- results = 'hide', message = FALSE---------------------------------------
geese_simmr_out <- simmr_mcmc(geese_simmr)
summary(geese_simmr_out,
  type = "diagnostics",
  group = 1
)

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
posterior_predictive(geese_simmr_out, group = 5)

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
prior_viz(geese_simmr_out)

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
plot(geese_simmr_out, type = "histogram")

## ----fig.align = 'center',fig.width = 7,fig.height = 5------------------------
compare_groups(geese_simmr_out,
  groups = 1:4,
  source_name = "Enteromorpha"
)

