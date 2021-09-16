#' Plot smooth parameters estimated from Generalized Additive Models (GAM) based on ggplot2
#'
#' @description From a one dimensional GAM model produced by \code{mgcv::gam()}, produces a ggplot2 based plot of
#'  selected smooth parameters in a multi-panel display.
#'
#' @param gam_model A \code{gam} one dimensional model produced after fitting data from the \code{mgcv::gam()} function.
#' @param select_smooth_terms A string vector of one or more data variables from the GAM model to be plotted.
#'  If \code{NULL} the all the smooth parameters are plotted.
#' @param columns An integer that sets the number of columns for the multi-panel display.
#' @param col_width An integer that sets the width of each plot column in inches.
#' @param row_height An integer that sets the height of each plot column in inches.
#' @param n_points An integer that sets the number of points used for each 1-d plot.
#' @param se A logical which if TRUE upper and lower lines are plotted at 2 standard errors above and below the
#'  the smooth being plotted.
#' @param se_fill A string that sets the fill color between the standard error lines.
#' @param se_color A string that sets the color of the standard error lines.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_limits Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis.
#' @param x_major_breaks A numeric vector that defines the exact major tic locations along the x axis for all
#'  smooth parameters. If not set then each smooth parameter has their own x axis scaling using ggplot2 defaults. In
#'  addition the argument may be assigned to a named list (e.g. \code{list(x1 = seq(-3, 4, 1.0), x3 = seq(-2, 2, 0.5))}) of
#'  numeric vectors.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis for all smooth parameters.
#' @param y_major_breaks A numeric vector that defines the exact major tic locations along the y axis for all
#'  smooth parameters. If not set then each smooth parameter has their own y axis scaling using ggplot2 defaults. In
#'  addition the argument may be assigned to a named list (e.g. \code{list(x1 = seq(-3, 4, 1.0), x3 = seq(-2, 2, 0.5))}) of
#'  numeric vectors.
#' @param line_width A numeric value that sets the width of lines.
#' @param line_color A string that sets the color of the lines.
#'
#' @import ggplot2
#' @import grid
#' @import gtable
#' @import RplotterPkg
#' @import dplyr
#' @import rlang
#' @import mgcv
#'
#' @return A ggplot2 plot object
#'
#' @author Rick Dean
#'
#' @export
plot_gam_1d <- function(
  gam_model = NULL,
  select_smooth_terms = NULL,
  columns = 2,
  col_width = 4,
  row_height = 4,
  n_points = 100,
  se = TRUE,
  se_fill = NA,
  se_color = "red",
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  line_width = 1,
  line_color = "black"
){
  if(is.null(gam_model) | !inherits(gam_model, "gam")){
    stop("The submitted model is NULL or is not a gam object from mgcv.")
  }

  # get all smooth terms
  all_smooth_terms <- RgamPkg::terms_gam(gam_model = gam_model)

  # check smooth dim-- only 1 dim accepted
  all_smooth_dims <- RgamPkg::dims_gam(gam_model = gam_model)
  is_1_dim <- all_smooth_dims == 1L
  all_smooth_terms <- all_smooth_terms[is_1_dim]
  if(length(all_smooth_terms) == 0){
    stop("Could not locate a 1-dimensional smooth component.")
  }

  if(is.null(select_smooth_terms)){
    smooth_terms <- all_smooth_terms
  }else {
    smooth_terms <- c()
    # select_smooths
    for(smooth_term in select_smooth_terms){
      if(smooth_term %in% all_smooth_terms){
        smooth_terms <- c(smooth_terms, smooth_term)
      }
    }
  }

  n_smooths <- length(smooth_terms)
  if(n_smooths == 0){
    stop("Could not locate a 1-dimensional smooth component.")
  }
  layer_smooth <- vector("list", n_smooths)

  smooth_plots <- list()
  for(smooth_term in smooth_terms){
    smooth_eval <- RgamPkg::evaluate_smooth(
      gam_model = gam_model,
      smooth_term = smooth_term,
      n_points = n_points
    )

    if(is.list(x_major_breaks) & smooth_term %in% names(x_major_breaks)){
      x_breaks <- x_major_breaks[[smooth_term]]
    }else if(is.numeric(y_major_breaks)) {
      x_breaks <- x_major_breaks
    }else {
      x_breaks <- waiver()
    }

    if(is.list(y_major_breaks) & smooth_term %in% names(y_major_breaks)){
      y_breaks <- y_major_breaks[[smooth_term]]
    }else if(is.numeric(y_major_breaks)) {
      y_breaks <- y_major_breaks
    }else {
      y_breaks <- waiver()
    }

    smooth_plots[[smooth_term]] <- RplotterPkg::create_scatter_plot(
      df = smooth_eval$spline_data,
      aes_x = "x",
      aes_y = "est",
      x_title = smooth_term,
      y_title = paste0("s(", smooth_term, ")"),
      x_limits = x_limits,
      x_major_breaks = x_breaks,
      y_limits = y_limits,
      y_major_breaks = y_breaks,
      connect = TRUE,
      show_pts = FALSE,
      line_width = line_width,
      line_color = line_color
    )

    if(se){
      smooth_plots[[smooth_term]] <-  smooth_plots[[smooth_term]] +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, color = se_color, linetype = "dashed", fill = se_fill)
    }
  }
  if(length(smooth_plots) == 1){
    columns <- 1
  }

  cols <- c()
  for(i in seq(1, length(smooth_plots), by = 1)){
    val <- i %% columns
    if(val == 0){
      cols <- c(cols, columns)
    }else {
      cols <- c(cols,val)
    }
  }

  n_rows <- ceiling(length(smooth_plots)/columns)
  rows <- c()
  for(i in seq(1, n_rows, by = 1)){
    for(ii in seq(1, columns, by = 1)){
      rows <- c(rows, i)
    }
  }

  layout <- list(
    plots = smooth_plots,
    rows = rows,
    cols = cols
  )
  multi_plot <- RplotterPkg::multi_panel_grid(
    layout = layout,
    col_widths = rep(col_width, columns),
    row_heights = rep(row_height, n_rows),
    title = title,
    subtitle = subtitle
  )
  return(multi_plot)
}
