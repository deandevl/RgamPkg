% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spline_values.R
\name{spline_values}
\alias{spline_values}
\title{Function computes the spline values for a smooth object computed from a GAM model produced by \code{mgcv::gam()}}
\usage{
spline_values(smooth_obj = NULL, new_data = NULL, gam_model = NULL)
}
\arguments{
\item{smooth_obj}{An object of class \dQuote{mgcv.smooth} produced by \code{mgcv::gam()}.}

\item{new_data}{Either a data frame of observed data or data to be predicted.}

\item{gam_model}{An object of class \dQuote{gam} produced by \code{mgcv::gam()}}
}
\value{
A data frame of fitted spline vales and their associated standard errors.
}
\description{
Function computes the spline values for a smooth object computed from a GAM model produced by \code{mgcv::gam()}
}
\author{
Rick Dean
}
