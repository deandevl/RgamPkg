% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_smooth.R
\name{evaluate_smooth}
\alias{evaluate_smooth}
\title{Evaluate a smooth}
\usage{
evaluate_smooth(
  gam_model = NULL,
  smooth_term = NULL,
  n_points = 100,
  new_data = NULL
)
}
\arguments{
\item{gam_model}{A \code{gam} object model produced after fitting data from the \code{mgcv::gam()} function.}

\item{smooth_term}{A string that identifies the smooth to be evaluated.}

\item{n_points}{An integer that sets the number of points to be evaluated.}

\item{newdata}{A vector or data frame of points at which to evaluate the points.}
}
\value{
A data frame of the evaluated smooth spline values
}
\description{
Evaluate a smooth term and return its corresponding spline values
}
\author{
Rick Dean
}
