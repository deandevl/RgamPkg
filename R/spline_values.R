#' Function computes the spline values for a smooth object computed from a GAM model produced by \code{mgcv::gam()}
#'
#' @param smooth_obj An object of class \dQuote{mgcv.smooth} produced by \code{mgcv::gam()}.
#' @param new_data Either a data frame of observed data or data to be predicted.
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @import mgcv
#'
#' @return A data frame of fitted spline vales and their associated standard errors.
#'
#' @author Rick Dean
#'
#' @export
spline_values <- function(
  smooth_obj = NULL,
  new_data = NULL,
  gam_model = NULL){

  if(!("mgcv.smooth" %in% class(smooth_obj))){
    stop('spline_values function: smooth_obj argument must be of class "mgcv.smooth".')
  }
  if(!("gam" %in% class(gam_model))){
    stop('spline_values function: gam_model argument must be of class "gam".')
  }

  predict_data_mat <- mgcv::PredictMat(smooth_obj, new_data)

  start <- smooth_obj[["first.para"]]
  end <- smooth_obj[["last.para"]]
  para_seq <- start:end
  coefs <- coef(gam_model)[para_seq]

  est <- drop(predict_data_mat %*% coefs)

  # variables for computing standard errors of fit
  v <- RgamPkg::get_vcov(gam_model)
  column_means <- gam_model$cmX
  meanL1 <- smooth_obj$meanL1

  if(attr(smooth_obj, "nCons") > 0L){
    if(length(column_means) < ncol(v)){
      column_means <- c(column_means, rep(0, ncol(v) - length(column_means)))
    }

    Xcm <- matrix(column_means, nrow = nrow(predict_data_mat), ncol = ncol(v), byrow = TRUE)
    if(!is.null(smooth_obj$meanL1)){
      Xcm <- Xcm / smooth_obj$meanL1
    }

    Xcm[, para_seq] <- predict_data_mat
    rs <- rowSums((Xcm %*% v) * Xcm)
  }else {
    rs <- rowSums((predict_data_mat %*% v[para_seq, para_seq]) * predict_data_mat)
  }

  se_est <- sqrt(pmax(0, rs))
  smooth_label <- RgamPkg::label_smooth(smooth_obj)
  smooth_term <- RgamPkg::term_smooth(smooth_obj)

  if(smooth_obj$dim == 1L){
    crit <- stats::qnorm((1 - 0.95)/2, lower.tail = FALSE)
    values <- data.frame(
      x = new_data[[smooth_term]],
      est = est,
      se = se_est,
      upper = est + (crit * se_est),
      lower = est - (crit * se_est)
    )
  }
  return(values)
}
