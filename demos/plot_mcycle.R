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
predict_gam <- predict(mcycle_mod_gam, mcycle, type = "link",  se.fit = T)

mcycle_gam_df <- data.frame(
  times = mcycle_mod_gam$model$times,
  accel = mcycle_mod_gam$model$accel,
  fit = predict_gam$fit,
  ci_lower = predict_gam$fit - (2 * predict_gam$se.fit),
  ci_upper = predict_gam$fit + (2 * predict_gam$se.fit)
)
RplotterPkg::create_scatter_plot(
  df = mcycle_gam_df,
  aes_x = "times",
  aes_y = "fit",
  connect = T,
  show_pts = F,
  CI_lwr = "ci_lower",
  CI_upr = "ci_upper",
  CI_show_line = T,
  CI_show_ribbon = T,
  title = "Head Acceleration in Simulated Motorcycle Accident",
  subtitle = "Observations and fitted GAM model"
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
