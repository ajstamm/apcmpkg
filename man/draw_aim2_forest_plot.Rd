% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_aim2_forest_plot.R
\name{draw_aim2_forest_plot}
\alias{draw_aim2_forest_plot}
\title{Draw individual plots to include in the double-plot figure}
\usage{
draw_aim2_forest_plot(data, title)
}
\arguments{
\item{data}{Dataset to plot, with RRs and CIs.}

\item{title}{Title for the individual plot.}
}
\description{
This function draws the risk ratios and confidence intervals by lag
for each air pollutant on the same graph. It returns a ggplot object.
}
