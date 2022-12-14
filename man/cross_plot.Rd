% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_plot.R
\name{cross_plot}
\alias{cross_plot}
\title{Create a series of diagnostic cross-basis plots}
\usage{
cross_plot(
  bd,
  air,
  measure,
  model,
  list,
  centre = NULL,
  atvals = NULL,
  plotvar = NULL
)
}
\arguments{
\item{bd}{String denoting birth defect}

\item{air}{String denoting air pollutant}

\item{measure}{String denoting type of pollutant measure}

\item{model}{String denoting type of cross-basis model}

\item{list}{List object containing the series of models}

\item{centre}{The threshold or centering value}

\item{atvals}{The air pollutant concentrations to predict}

\item{plotvar}{The unit increase of interest to calculate risk ratios}
}
\description{
This function reads in a list produced by \code{cb_signif} or \code{cb_diagnostics}
and returns a series of plots, including:
\itemize{
\item residual plot overlaid best fit line
\item lag CI plot
\item cumulative CI plot
\item plot with discrete lag CIs
\item 3D graph of risk ratio, lags, and pollutant levels
}

The code for these plots is adapted from the code provided in the \code{dlnm}
package vignette.
}
