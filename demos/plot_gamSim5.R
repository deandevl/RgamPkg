library(mgcv)
library(RgamPkg)
library(RplotterPkg)
library(dplyr)
library(ggplot2)
library(grid)
library(gtable)

# additive model + factor with data from mgcv::gamSim() example 5 ("eg = 5")
data_example_5_df <- mgcv::gamSim(eg = 5, n = 400, dist = "normal", scale = 2)
str(data_example_5_df)

# start with a model with one smoothed term (x1) and one categorical predictor x0 which has 4 levels

# plot the simulated data x0,x1 vs y
RplotterPkg::multi_scatter_plot(
  df = data_example_5_df,
  factor_var = "x0",
  factor_x = "x1",
  aes_y = "y",
  title = "x1 vs y for levels of x0",
  pts_size = 1.4,
  x_limits = c(0.0, 1.0),
  x_major_breaks = seq(0.0, 1.0, 0.25),
  y_limits = c(0, 25),
  y_major_breaks = seq(0, 25, 5)
)

# create the GAM model
model_x0_x1_gam <- mgcv::gam(formula = y ~ x0 + s(x1), data = data_example_5_df)
summary(model_x0_x1_gam)

# edf is the estimated degrees of freedom -- a larger edf value implies more complex wiggly splines
#  1. A value close to 1 tend to be close to a linear term.
#  2. A high value (8-10 or higher)  means that the spline in highly non-linear

# using mgcv::plot.gam() function to plot the model
mgcv::plot.gam(x = model_x0_x1_gam, rug = FALSE)

#using RgamPkg::plot_gam_1d() function to plot the model
RgamPkg::plot_gam_1d(
  gam_model = model_x0_x1_gam,
  select_smooth_terms = "x1",
  col_width = 6,
  row_height = 8,
  title = "s(x1) Plot",
  subtitle = "formula: y ~ x0 + s(x1)",
  y_limits = c(-3.0, 5.0),
  y_major_breaks = seq(-3.0, 5.0, 1.0)
)

model_x0_x1_x2_gam <- mgcv::gam(formula = y ~ x0 + s(x1) + s(x2), data = data_example_5_df)
summary(model_x0_x1_x2_gam)

RgamPkg::plot_gam_1d(
  gam_model = model_x0_x1_x2_gam,
  col_width = 5,
  row_height = 8,
  title = "s(x1), s(x2) Plot",
  subtitle = "formula: y ~ x0 + s(x1) + s(x2)",
  y_limits = c(-3.0, 5.0),
  y_major_breaks = seq(-3.0, 5.0, 1.0)
)
