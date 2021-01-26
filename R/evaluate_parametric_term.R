#' Evaluate a parametric term
#'
#' @description Evaluate a parametric term and return its values
#'
#' @param gam_model A \code{gam} object model produced after fitting data from the \code{mgcv::gam()} function.
#' @param pterm A sting that names a parametric term to be evaluated
#'
#' @return A data frame of the evaluated parametric term
#'
#' @import mgcv
#'
#' @author Rick Dean
#'
#' @export
evaluate_parametric_term <- function(
  gam_model = NULL,
  pterm = NULL
){
  pterms <- RgamPkg::get_parametric_terms(gam_model = gam_model)
  if(!pterm %in% pterms){
    stop(paste(pterm, "is not in the parametric part of the model."))
  }
  model_data <- stats::model.frame(gam_model)
  if(is.factor(model_data[[pterm]])){

  }
}
