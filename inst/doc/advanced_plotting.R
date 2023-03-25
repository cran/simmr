## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center", fig.width = 7, fig.height = 5
)
library(simmr)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("simmr")
#  library(simmr)

## ---- message=FALSE, results='hide'-------------------------------------------
# Load in example data
data(geese_data_day1)
# Load into simmr
simmr_in <- with(
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
simmr_out <- simmr_mcmc(simmr_in)

## -----------------------------------------------------------------------------
p <- plot(simmr_in,
  xlab = expression(paste(delta^13, "C (\u2030)", sep = "")),
  ylab = expression(paste(delta^15, "N (\u2030)", sep = "")),
  title = "Isospace plot of Inger et al Geese data",
  mix_name = "Organism"
)

## -----------------------------------------------------------------------------
p + xlim(-50, 50) + labs(subtitle = "A subtitle goes here")

## -----------------------------------------------------------------------------
p <- plot(simmr_out,
  type = "boxplot",
  title = "simmr"
)

## -----------------------------------------------------------------------------
p + ylim(0, 0.5) +
  labs(subtitle = "Something else") +
  ylab("A new ylab")

## -----------------------------------------------------------------------------
# First extract the dietary proportions
simmr_out2 <- simmr_out$output[[1]]$BUGSoutput$sims.list$p
colnames(simmr_out2) <- simmr_out$input$source_names

# Now turn into a proper data frame
library(reshape2)
df <- reshape2::melt(simmr_out2)
colnames(df) <- c("Num", "Source", "Proportion")

# Finally create the new variable that you want to colour by
df$new_colour <- "Type 2"
df$new_colour[df$Source == "Zostera"] <- "Type 1"

# And create the plot
ggplot(df, aes_string(
  y = "Proportion", x = "Source",
  fill = "new_colour", alpha = 0, 5
)) +
  geom_boxplot(notch = TRUE, outlier.size = 0) +
  theme_bw() +
  ggtitle("simmr output boxplot with changed colours") +
  theme(legend.position = "none") +
  coord_flip()

## -----------------------------------------------------------------------------
p <- plot(simmr_in)

## -----------------------------------------------------------------------------
library(ggnewscale)
new_df <- data.frame(
  x = geese_data_day1$source_means[, "meand13CPl"] + geese_data_day1$correction_means[, "meand13CPl"],
  y = geese_data_day1$source_means[, "meand15NPl"] + geese_data_day1$correction_means[, "meand15NPl"],
  Source = "Mixtures"
)

## -----------------------------------------------------------------------------
p +
  new_scale_color() +
  geom_polygon(data = new_df, aes(x = x, y = y), fill = "orange", alpha = 0.2)

## -----------------------------------------------------------------------------
chull_vals <- chull(new_df[, 1], new_df[, 2])
new_df2 <- new_df[chull_vals, ]
p +
  new_scale_color() +
  geom_polygon(data = new_df2, aes(x = x, y = y), fill = "orange", alpha = 0.2)

## -----------------------------------------------------------------------------
# Create the new data frame
new_mix <- data.frame(
  x = geese_data_day1$mixtures[, "d13C_Pl"],
  y = geese_data_day1$mixtures[, "d15N_Pl"],
  Source = "Mixtures"
)
# Find the convex hull
chull_mix_vals <- chull(new_mix[, 1], new_mix[, 2])
new_mix2 <- new_mix[chull_mix_vals, ]
# Plot using new_scale_color
p +
  new_scale_color() +
  geom_polygon(data = new_mix2, aes(x = x, y = y), fill = "purple", alpha = 0.2)

## -----------------------------------------------------------------------------
source_means_c <- geese_data_day1$source_means + geese_data_day1$correction_means
source_sds_c <- sqrt(geese_data_day1$source_sds^2 + geese_data_day1$correction_sds^2)
mix <- geese_data_day1$mixtures
x <- c(
  source_means_c[, "meand13CPl"] - source_sds_c[, "SDd13C"],
  source_means_c[, "meand13CPl"] - source_sds_c[, "SDd13C"],
  source_means_c[, "meand13CPl"] + source_sds_c[, "SDd13C"],
  source_means_c[, "meand13CPl"] + source_sds_c[, "SDd13C"]
)
y <- c(
  source_means_c[, "meand15NPl"] - source_sds_c[, "SDd15N"],
  source_means_c[, "meand15NPl"] + source_sds_c[, "SDd15N"],
  source_means_c[, "meand15NPl"] - source_sds_c[, "SDd15N"],
  source_means_c[, "meand15NPl"] + source_sds_c[, "SDd15N"]
)
new_df3 <- data.frame(
  x = x,
  y = y,
  Source = "Mixtures"
)
chull_vals <- chull(new_df3[, 1], new_df3[, 2])
new_df4 <- new_df3[chull_vals, ]
p + new_scale_color() +
  geom_polygon(data = new_df2, aes(x = x, y = y), fill = "orange", alpha = 0.3) +
  new_scale_color() +
  geom_polygon(data = new_df4, aes(x = x, y = y), fill = "orange", alpha = 0.1)

