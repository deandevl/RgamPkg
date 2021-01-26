# ------------------------------------------------------------------------------------
#' Get the dims of all smooth objects from a GAM model produced by \code{mgcv::gam()}
#'
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
dims_gam <- function(gam_model){
  if("gam" %in% class(gam_model)){
    return(vapply(gam_model[["smooth"]], FUN = `[[`, FUN.VALUE = integer(1), "dim"))
  }else{
    stop('dims_gam function: gam_model argument must be of class "gam".')
  }
}

#' Get the dim of a smooth object
#'
#' @param smooth_obj An object of class \dQuote{mgcv.smooth} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
dim_smooth <- function(smooth_obj){
  if("mgcv.smooth" %in% class(smooth_obj)){
    return(smooth_obj$dim)
  }else{
    stop('dim_smooth function: smooth_obj argument must be of class "mgcv.smooth".')
  }
}

# ------------------------------------------------------------------------------
#' Get the labels of all smooth objects from a GAM model produced by \code{mgcv::gam()}
#'
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
labels_gam <- function(gam_model){
  if("gam" %in% class(gam_model)){
    return(vapply(gam_model[["smooth"]], FUN = `[[`, FUN.VALUE = character(1), "label"))
  }else{
    stop('labels_gam function: gam_model argument must be of class "gam".')
  }
}

#' Get the label of a smooth object
#'
#' @param smooth_obj An object of class \dQuote{mgcv.smooth} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
label_smooth <- function(smooth_obj){
  if("mgcv.smooth" %in% class(smooth_obj)){
    return(smooth_obj$label)
  }else{
    stop('label_smooth function: smooth_obj argument must be of class "mgcv.smooth".')
  }
}

# ---------------------------------------------------------------------------------------
#' Get the terms of all smooth objects from a GAM model produced by \code{mgcv::gam()}
#'
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
terms_gam <- function(gam_model){
  if("gam" %in% class(gam_model)){
    return(vapply(gam_model[["smooth"]], FUN = `[[`, FUN.VALUE = character(1),"term"))
  }else{
    stop('terms_gam function: gam_model argument must be of class "gam".')
  }
}

#' Get the term of a smooth object
#'
#' @param smooth_obj An object of class \dQuote{mgcv.smooth} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
term_smooth <- function(smooth_obj){
  if("mgcv.smooth" %in% class(smooth_obj)){
    return(smooth_obj$term)
  }else {
    stop('term_smooth function: smooth_obj argument must be of class "mgcv.smooth".')
  }
}

# --------------------------------------------------------------------------------
#' Get the indices of a smooth label from a GAM model produced by \code{mgcv::gam()}
#'
#' @param smooth_label A string that is the label of smooth object of class \code{mgcv.smooth}
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @author Rick Dean
#'
#' @export
term_index <- function(smooth_term, gam_model){
  if(!("gam" %in% class(gam_model))){
    stop('term_index function: gam_model argument must be of class "gam".')
  }
  all_smooth_terms <- RgamPkg::terms_gam(gam_model = gam_model)
  index <- unique(grep(smooth_term, all_smooth_terms, fixed = TRUE))

  if(length(index) == 0){
    stop(paste("Could not locate", smooth_term))
  }
  return(index)
}

# -------------------------------------------------------------------------------------
#' Get vcov ?
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @export
get_vcov <- function(gam_model){
  if("gam" %in% class(gam_model)){
    return(gam_model$Vp)
  }else{
    stop('get_vcov function: gam_model argument must be of class "gam".')
  }
}

# --------------------------------------------------------------------------------------
#' get parametric terms of model
#' @param gam_model An object of class \dQuote{gam} produced by \code{mgcv::gam()}
#'
#' @export
get_parametric_terms <- function(gam_model){
  pterms <- gam_model$pterms
  if(length(attr(pterms, "term.labels")) > 0){
    pterms <- stats::delete.response(pterms)
    pterm_names <- labels(pterms)
    names(pterm_names) <- pterm_names
    return(pterm_names)
  }else {
    return(character(0))
  }
}


