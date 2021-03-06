% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/woePlot.R
\name{woePlot}
\alias{woePlot}
\title{Title woePlot}
\usage{
woePlot(
  bins,
  etheme = "westeros",
  elabels = TRUE,
  elabelspos = "top",
  stacking = FALSE
)
}
\arguments{
\item{bins}{Variable binning, which can be a list or a data frame, see the example for details.}

\item{etheme}{Please refer to ?echarts4r::e_theme().}

\item{elabels}{Please refer to ?echarts4r::e_labels}

\item{elabelspos}{Please refer to ?echarts4r::e_labels position}

\item{stacking}{Whether stacking, the default is FALSE}
}
\value{
Interactive graphics for binning of variables.
}
\description{
Visualize variable binning via echarts4r
}
\examples{

library(data.table)

data("germancredit", package="scorecard")

data <- germancredit

binsChi <- binningsChimerge(
  data = data
  , y = 'creditability'
)


woePlot(binsChi)

woePlot(binsChi$duration.in.month)

}
