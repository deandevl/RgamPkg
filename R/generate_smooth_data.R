#' Generate data over the range of variable used in smooths
#'
#' @description For a smooth object of class \code{mgcv.smooth} from a GAM model, generate new
#' data over the range of the variables involved in a smooth.
#'
#' @param gam_model A \code{gam} object model produced after fitting data from the \code{mgcv::gam()} function.
#' @param smooth_obj A smooth object of class \code{mgcv.smooth} for which new data is required.
#' @param n_points An integer that sets the number of point values to be generated.
#'
#' @import mgcv
#'
#' @author Rick Dean
#'
#' @export
generate_smooth_data <- function(
  smooth_obj = NULL,
  n_points = 100,
  gam_model = NULL
  ){
  smooth_dim <- RgamPkg::dim_smooth(smooth_obj)

  if(smooth_dim == 1L){
    model_fit_data <- gam_model[["model"]]
    smooth_term <- RgamPkg::term_smooth(smooth_obj)
    smooth_label <- RgamPkg::label_smooth(smooth_obj)

    smooth_data <- model_fit_data[smooth_term]

    x_data <- data.frame(
      smooth = rep(smooth_label, n_points)
    )
    x_data[smooth_term] <- seq(min(smooth_data), max(smooth_data), length.out = n_points)

    return(x_data)
  }
}
