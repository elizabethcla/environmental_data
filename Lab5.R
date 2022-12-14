ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 25, .2), 
  from = 0, to = 100, add = TRUE, ylim = c(0,100), 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", col = "black")
curve(
  ricker_fun(x, 20, .2), 
  from = 0, to = 100, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty = 2, lwd = 1, col = "black")
curve(
  ricker_fun(x, 10, .2), 
  from = 0, to = 100, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty = 2, lwd = 1, col = "black")
curve(
  ricker_fun(x, 75, .3), 
  from = 0, to = 100, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", col = "red")
curve(
  ricker_fun(x, 50, .3), 
  from = 0, to = 100, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty = 2, lwd = 1, col = "red")
curve(
  ricker_fun(x, 40, .3), 
  from = 0, to = 100, add = TRUE, 
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", lty = 2, lwd = 1, col = "red")

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

error_mean = 0
error_sd = 0.1

y_observed_3 = 
  (y_pred + 
    rexp(n_pts, rate = 1.2))


par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")



fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 1))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)

curve(
  exp_fun(x, 1.9, .1), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "black"); box()
curve(
  exp_fun(x, 1.9, .3), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty = 2, lwd = 1, col = "black"); box()
curve(
  exp_fun(x, 10, .2), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red"); box()
curve(
  exp_fun(x, 1.2, .4), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", lty = 2, lwd = 1, col = "red"); box()

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

require(here)
disp_dat = read.csv(
  here("dispersal (2).csv")
)
read.csv("dispersal (2).csv")

plot(
  disp_dat$dist.class,
  disp_dat$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n linear model")
curve(line_point_slope(x, 800, .3, -0.0005), add = TRUE)

plot(
  disp_dat$dist.class,
  disp_dat$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n linear model")
curve(
  exp_fun(x, .99, .003), add = TRUE, from = 0, to = 1500,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "black"); box()

plot(
  disp_dat$dist.class,
  disp_dat$disp.rate.ftb,
  xlim = c(0, 1500),
  ylim = c(0,2),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders")
curve(
  ricker_fun(x, 0.03, .01), 
  from = 0, to = 1500, ylim = c(0, 1500), add = TRUE, 
  main = "Salamander Ricker function",
  ylab = "f(x)", xlab = "x")
plot(disp_dat$disp.rate.ftb ~ disp_dat$dist.class, data = disp_dat, pch = 8)
curve(line_point_slope(x, 800, .3, -.0005), add = TRUE)

resids_ricker <- ricker_fun(disp_dat$dist.class, .03, .01)
hist(disp_dat$disp.rate.ftb - resids_linear)
hist(disp_dat$disp.rate.ftb - resids_exp)
hist(disp_dat$disp.rate.ftb - resids_ricker)
