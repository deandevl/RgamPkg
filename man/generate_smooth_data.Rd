% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_smooth_data.R
\name{generate_smooth_data}
\alias{generate_smooth_data}
\title{Generate data over the range of variable used in smooths}
\usage{
generate_smooth_data(smooth_obj = NULL, n_points = 100, gam_model = NULL)
}
\arguments{
\item{smooth_obj}{A smooth object of class \code{mgcv.smooth} for which new data is required.}

\item{n_points}{An integer that sets the number of point values to be generated.}

\item{gam_model}{A \code{gam} object model produced after fitting data from the \code{mgcv::gam()} function.}
}
\description{
For a smooth object of class \code{mgcv.smooth} from a GAM model, generate new
data over the range of the variables involved in a smooth.
}
\author{
Rick Dean
}
