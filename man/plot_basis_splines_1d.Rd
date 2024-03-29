% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_basis_splines_1d.R
\name{plot_basis_splines_1d}
\alias{plot_basis_splines_1d}
\title{Plot basis splines of a smooth parameter estimated from Generalized Additive Models (GAM) based on ggplot2}
\usage{
plot_basis_splines_1d(
  gam_model = NULL,
  data_var = NULL,
  show_points = FALSE,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  line_type = "dashed",
  line_color = "black",
  line_width = 1
)
}
\arguments{
\item{gam_model}{A \code{gam} one dimensional model produced after fitting data from the \code{mgcv::gam()} function.}

\item{data_var}{A string that is the name of one of the data variables in the GAM model.}

\item{show_points}{A logical which if TRUE will plot the observed  points.}

\item{title}{A string that sets the plot title.}

\item{subtitle}{A string that sets the plot subtitle.}

\item{center_titles}{A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.}

\item{x_limits}{Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
and maximum for the x axis.}

\item{x_major_breaks}{A numeric vector that defines the exact major tic locations along the x axis for all
smooth parameters. If not set then the smooth parameter has x axis scaling using ggplot2 defaults.}

\item{y_limits}{A numeric 2 element vector that sets the minimum and  maximum for the y axis for the smooth parameter.}

\item{y_major_breaks}{A numeric vector that defines the exact major tic locations along the y axis for the
smooth parameter. If not set then the smooth parameter has y axis scaling using ggplot2 defaults.}

\item{line_type}{A string that sets the line type for the basis splines. Typical values are \code{twodash, solid, longdash, dotted, dotdash,
dashed, blank}}

\item{line_color}{A string that sets the color of the lines.}

\item{line_width}{A numeric value that sets the width of lines.}
}
\value{
A ggplot2 plot object
}
\description{
From a one dimensional GAM model produced by \code{mgcv::gam()}, display a ggplot2 based plot of
all the basis spline components that add up to a single smooth parameter.
}
\author{
Rick Dean
}
