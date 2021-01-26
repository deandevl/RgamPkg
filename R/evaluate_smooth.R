#' Evaluate a smooth
#'
#' @description   Evaluate a smooth term and return its corresponding spline values
#'
#' @param gam_model A \code{gam} object model produced after fitting data from the \code{mgcv::gam()} function.
#' @param smooth_term A string that identifies the smooth to be evaluated.
#' @param n_points An integer that sets the number of points to be evaluated.
#' @param newdata A vector or data frame of points at which to evaluate the points.
#'
#' @return A data frame of the evaluated smooth spline values
#'
#' @import mgcv
#'
#' @author Rick Dean
#'
#' @export
evaluate_smooth <- function(
  gam_model = NULL,
  smooth_term = NULL,
  n_points = 100,
  new_data = NULL
){
  index <- RgamPkg::term_index(smooth_term = smooth_term, gam_model = gam_model)
  smooth_obj <- gam_model$smooth[[index]]

  if(inherits(smooth_obj, "random.effect")){
    stop("not implemented")
  }else if(inherits(smooth_obj, "fs.interaction")){
    stop("not implemented")
  }else if(smooth_obj$dim == 1L){
    if(is.null(new_data)){
      x_data_df <- RgamPkg::generate_smooth_data(smooth_obj = smooth_obj, n_points = n_points, gam_model = gam_model)
    }
  }else if(smooth_obj$dim == 2L){
    stop("not implemented")
  }
  evaluated <- list(
    term = smooth_term,
    spline_data = RgamPkg::spline_values(
      smooth_obj,
      new_data = x_data_df,
      gam_model = gam_model
    )
  )

  return(evaluated)
}
