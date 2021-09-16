#' Plot basis splines of a smooth parameter estimated from Generalized Additive Models (GAM) based on ggplot2
#'
#' @description From a one dimensional GAM model produced by \code{mgcv::gam()}, display a ggplot2 based plot of
#'  all the basis spline components that add up to a single smooth parameter.
#'
#' @param gam_model A \code{gam} one dimensional model produced after fitting data from the \code{mgcv::gam()} function.
#' @param data_var A string that is the name of one of the data variables in the GAM model.
#' @param show_points A logical which if TRUE will plot the observed  points.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_limits Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis.
#' @param x_major_breaks A numeric vector that defines the exact major tic locations along the x axis for all
#'  smooth parameters. If not set then the smooth parameter has x axis scaling using ggplot2 defaults.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis for the smooth parameter.
#' @param y_major_breaks A numeric vector that defines the exact major tic locations along the y axis for the
#'  smooth parameter. If not set then the smooth parameter has y axis scaling using ggplot2 defaults.
#' @param line_type A string that sets the line type for the basis splines. Typical values are \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank}
#' @param line_width A numeric value that sets the width of lines.
#' @param line_color A string that sets the color of the lines.
#'
#' @import ggplot2
#' @import RplotterPkg
#' @import dplyr
#' @import mgcv
#'
#' @return A ggplot2 plot object
#'
#' @author Rick Dean
#'
#' @export
plot_basis_splines_1d <- function(
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
  line_width = 1.0
){
  # check gam_model
  if(is.null(gam_model) | !inherits(gam_model, "gam")){
    stop("The submitted model is NULL or is not a gam object from mgcv.")
  }

  # check smooth_label
  smooth_label <- paste0("s(" ,data_var, ")")
  all_smooth_labels <- RgamPkg::labels_gam(gam_model = gam_model)
  if(!(smooth_label %in% all_smooth_labels)) {
    stop(paste("Could not locate", smooth_label, "in GAM model."))
  }

  basis_mat <- mgcv::model.matrix.gam(gam_model)

  indexes <- tidyr::starts_with(smooth_label, vars = colnames(basis_mat))
  basis_mat <- basis_mat[,indexes]

  coef_mat <- as.matrix(stats::coef(gam_model))
  intercept <- coef_mat[[1]]

  coef_mat <- coef_mat[indexes,1]

  vals_mat <- matrix(data = NA, nrow = nrow(basis_mat), ncol = ncol(basis_mat))
  for(i in 1:ncol(basis_mat)){
    vals <- basis_mat[,i] * coef_mat[[i]] + intercept
    vals_mat[,i] <- vals
  }

  basis_vals_df <- as.data.frame(vals_mat)
  names(basis_vals_df) <- colnames(basis_mat)

  basis_vals_df <- cbind(basis_vals_df, gam_model$model[data_var])

  basis_vals_long_df <- tidyr::pivot_longer(
    data = basis_vals_df,
    cols = starts_with(smooth_label),
    names_to = "Function",
    values_to = names(gam_model$model)[[1]]
  )
  basis_vals_long_df <- basis_vals_long_df %>%
    mutate(Function = as.factor(Function))

  fit_df <- data.frame(
    x_ob = gam_model$model[[data_var]],
    y_ob = gam_model$y,
    fit = basis_mat %*% coef_mat + intercept
  )

  basis_plot <- RplotterPkg::create_scatter_plot(
    df = basis_vals_long_df,
    aes_x = data_var,
    aes_y = names(gam_model$model)[[1]],
    aes_color = "Function",
    connect = TRUE,
    connect_linetype = line_type,
    line_width = line_width,
    line_color = line_color,
    show_pts = FALSE,
    title = title,
    subtitle = subtitle,
    x_limits = x_limits,
    x_major_breaks = x_major_breaks,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks
  ) +
    ggplot2::geom_line(data = fit_df, aes(x = x_ob, y = fit), color = "red", size = 1.2)

  if(show_points){
    basis_plot <- basis_plot +
      ggplot2::geom_point(data = fit_df, aes(x = x_ob, y = y_ob), size = 2, shape = 1, color = "blue", stroke = 1.5)
  }

  return(basis_plot)
}
