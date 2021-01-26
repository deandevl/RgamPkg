library(mgcv)
library(RgamPkg)
library(RplotterPkg)
library(RregressPkg)
library(RgamPkg)
library(dplyr)
library(magrittr)
library(rlang)
library(ggplot2)
library(grid)
library(gtable)
library(MASS)

data(mcycle)
str(mcycle)

# plot the data
RplotterPkg::create_scatter_plot(
  df = mcycle,
  aes_x = "times",
  aes_y = "accel",
  title = "Head Acceleration in Simulated Motorcycle Accident",
  subtitle = "Data source: MASS::mcycle",
  connect = TRUE
)

# create the GAM model
mcycle_mod_gam <- mgcv::gam(formula = accel ~ s(times, k = 8), data = mcycle)
summary(mcycle_mod_gam)

# plot the model using mgcv::plot.gam()
mgcv::plot.gam(mcycle_mod_gam, rug = FALSE)

# plot the model using RregressPkg::plot_xy_fit()
mcycle_gam_fit_plot <- RregressPkg::plot_xy_fit(
  x = mcycle_mod_gam$model$times,
  y = mcycle_mod_gam$model$accel,
  fit = mcycle_mod_gam$fitted.values,
  title = "Head Acceleration in Simulated Motorcycle Accident",
  subtitle = "Observations and fitted GAM model",
  x_title = "times",
  y_title = "accel",
  fit_color = "red",
  col_width = 6,
  row_height = 6
)

# plot the model's independent component "times" using RgamPkg::plot_gam_1d()
mcycle_gam_functions_plot <- RgamPkg::plot_gam_1d(
  gam_model = mcycle_mod_gam,
  col_width = 8,
  row_height = 8,
  title = "Head Acceleration in Simulated Motorcycle Accident",
  subtitle = "GAM model: accel ~ s(times)"
)

# plot the basis splines that make up the smoother from the "times" variable
RgamPkg::plot_basis_splines_1d(
  gam_model = mcycle_mod_gam,
  data_var = "times"
)
