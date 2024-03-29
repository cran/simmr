% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_groups.R
\name{compare_groups}
\alias{compare_groups}
\title{Compare dietary proportions for a single source across different groups}
\usage{
compare_groups(
  simmr_out,
  source_name = simmr_out$input$source_names[1],
  groups = 1:2,
  plot = TRUE
)
}
\arguments{
\item{simmr_out}{An object of class \code{simmr_output} created from
\code{\link{simmr_mcmc}} or \code{\link{simmr_ffvb}}.}

\item{source_name}{The name of a source. This should match the names exactly
given to \code{\link{simmr_load}}.}

\item{groups}{The integer values of the group numbers to be compared. At
least two groups must be specified.}

\item{plot}{A logical value specifying whether plots should be produced or
not.}
}
\value{
If there are two groups, a vector containing the differences between
the two groups proportions for that source. If there are multiple groups, a
list containing the following fields: \item{Ordering }{The different
possible orderings of the dietary proportions across groups} \item{out_all
}{The dietary proportions for this source across the groups specified as
columns in a matrix}
}
\description{
This function takes in an object of class \code{simmr_output} and creates
probabilistic comparisons for a given source and a set of at least two
groups.
}
\details{
When two groups are specified, the function produces a direct calculation of
the probability that one group is bigger than the other. When more than two
groups are given, the function produces a set of most likely probabilistic
orderings for each combination of groups. The function produces boxplots by
default and also allows for the storage of the output for further analysis if
required.
}
\examples{
\dontrun{
data(geese_data)
simmr_in <- with(
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

# Print
simmr_in

# Plot
plot(simmr_in,
  group = 1:8, xlab = expression(paste(delta^13, "C (per mille)", sep = "")),
  ylab = expression(paste(delta^15, "N (per mille)", sep = "")),
  title = "Isospace plot of Inger et al Geese data"
)

# Run MCMC for each group
simmr_out <- simmr_ffvb(simmr_in)

# Print output
simmr_out

# Summarise output
summary(simmr_out, type = "quantiles", group = 1)
summary(simmr_out, type = "quantiles", group = c(1, 3))
summary(simmr_out, type = c("quantiles", "statistics"), group = c(1, 3))

# Plot - only a single group allowed
plot(simmr_out, type = "boxplot", group = 2, title = "simmr output group 2")
plot(simmr_out, type = c("density", "matrix"), grp = 6, title = "simmr output group 6")

# Compare groups
compare_groups(simmr_out, source = "Zostera", groups = 1:2)
compare_groups(simmr_out, source = "Zostera", groups = 1:3)
compare_groups(simmr_out, source = "U.lactuca", groups = c(4:5, 7, 2))
}

}
\seealso{
See \code{\link{simmr_mcmc}} for complete examples.
}
\author{
Andrew Parnell <andrew.parnell@mu.ie>
}
