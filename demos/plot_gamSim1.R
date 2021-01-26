library(mgcv)
library(RgamPkg)
library(RplotterPkg)
library(dplyr)
library(ggplot2)
library(grid)
library(gtable)

# additive model with data from mgcv::gamSim() example 1 ("eg = 1")
data_example_1_df <- mgcv::gamSim(eg = 1, n = 400, dist = "normal", scale = 2)

# plot the simulated data
data_plot <- RplotterPkg::multi_scatter_plot(
  df = data_example_1_df,
  variables = c("x0", "x1", "x2", "x3"),
  aes_y = "y",
  title = "Predictor Variables for a GAM Model",
  subtitle = "Simulated data source: mgcv::gamSim() example 1",
  connect = TRUE,
  show_pts = FALSE
)

model_1_gam <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data_example_1_df, method = "REML")
summary(model_1_gam)

# plot the model's independent components using mgcv::plot.gam()
mgcv::plot.gam(x = model_1_gam, pages = 1, rug = FALSE)

# plot the model's independent components using RgamPkg::plot_gam_1d()
y_major_breaks <- list(
  x1 = seq(-3, 4, 1.0),
  x3 = seq(-2, 2, 0.5)
)
plots <- RgamPkg::plot_gam_1d(
  gam_model = model_1_gam,
  select_smooth_terms = c("x0", "x1", "x2", "x3"),
  title = "GAM Plots for mgcv::gamSim, example no. 1",
  subtitle = "y ~ s(x0) + s(x1) + s(x2) + s(x3)",
  y_major_breaks = y_major_breaks
)

# plot the basis splines that make up the smoother from the "x1" variable
RgamPkg::plot_basis_splines_1d(
  gam_model = model_1_gam,
  data_var = "x1",
  title = "Spline functions for smooth component x1",
  subtitle = "GAM model from mgcv::gamSim, example no. 1"
)

# plot the basis splines that make up the smoother from the "x2" variable
RgamPkg::plot_basis_splines_1d(
  gam_model = model_1_gam,
  data_var = "x2",
  title = "Spline functions for smooth component x2",
  subtitle = "GAM model from mgcv::gamSim, example no. 1"
)
# redefine the GAM model with x1 and x3 having a reduced number of spline functions
model_1_gam_simpler <- mgcv::gam(y ~ s(x0) + s(x1, k = 3) + s(x2) + s(x3, k = 3), data = data_example_1_df, method = "REML")

# plot the basis splines for components x1 and x3 from the redefined, simpler GAM model
RgamPkg::plot_basis_splines_1d(
  gam_model = model_1_gam_simpler,
  data_var = "x1",
  title = "Spline functions for simpler smooth component x1",
  subtitle = "GAM model from mgcv::gamSim, example no. 1"
)

RgamPkg::plot_basis_splines_1d(
  gam_model = model_1_gam_simpler,
  data_var = "x3",
  title = "Spline functions for simpler smooth component x3",
  subtitle = "GAM model from mgcv::gamSim, example no. 1"
)

mgcv::plot.gam(x = model_1_gam_simpler, pages = 1, rug = FALSE)
