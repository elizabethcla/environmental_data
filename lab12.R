require("here")
bird = read.csv(here("data", "bird.sub.csv"))
hab = read.csv(here("data", "hab.sub.csv"))
birdhab=merge(bird, hab, by = c("basin", "sub"))
str(birdhab)
dim(birdhab)

int_obs=.0991
slope_obs=.00584
sd_obs=0.1412668

#observed data

linear = function(x, y_int, slope)
{
  y= y_int+x*slope
  return(y)
}

linear_simulator= function(x, y_int, slope, st_dev)
{
  first=linear(x, y_int, slope)
  stoch=rnorm(n=length(x), mean=0, sd=st_dev)
  
  return(first+stoch)
}

y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

fit_sim = lm(y_sim ~ birdhab$ls)
par(mfrow = c(1, 1))
alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

sample_sizes = seq(2, 20)
sample_size_powers = numeric(length(sample_sizes))

# The maximum x value in the simulation.
# Use the maximum observed x-value in the data
max_x = max(birdhab$ls)

for(j in 1:length(sample_sizes))
{
  # A sequence of equally-spaced x-values:
  x_vals = seq(0, max_x, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sample_size_powers[j] = sum(p_vals < alpha) / n_sims
}


sim_sample_size = 
  data.frame(
    sample_size = sample_sizes,
    power       = sample_size_powers) 
plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'p', xlab = 'Sample size', ylab = 'Power')
plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size), type="l",xlab = 'Sample size', ylab = 'Power',main="Sim and Lowess 30% graph")
points(
  power ~ sample_size, data = sim_sample_size, type="p", main="Sim and Lowess 30% graph")

legend("bottomright", legend = c("Lowess", "Simulation"), lty = c(1,NA),pch=c(NA, 1), col = c("black"))

par(mfrow=c(1,1))
plot(
  power ~ sample_size, data = sim_sample_size,
  type = 'l', xlab = 'Sample size', ylab = 'Power')
points(
  power ~ sample_size, data = sim_sample_size,
  pch=20)
fit_lowess_30 = loess(power ~ sample_size, data = sim_sample_size, pch=20, span = 0.3)

newdata_sample_size = data.frame(sample_size = seq(2, 20, length.out = 100)) 

?plot()

plot(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size),
  type = "l",
  ylab = "Statistical Power", xlab = "Sample Size")

points(
  x = newdata_sample_size$sample_size,
  y = predict(fit_lowess_30, newdata = newdata_sample_size))


ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

dat_dispersal = read.csv(
  here("dispersal (2).csv")
)
read.csv("dispersal (2).csv")

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

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  ricker_fun(x, 25, .2), 
  from = 0, to = 100, add = TRUE, ylim = c(0,100),
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", col = "black")
fit_ricker_nls = nls(
  disp.rate.ftb ~ ricker_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_ricker_nls)

fit_exp_nls = nls(
  disp.rate.ftb ~ exp_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
lines(predict(fit_exp_nls, newdata = dist_newdata))
curve(
  exp_fun(x, 1.55, .0039), add = TRUE, from = 0, to = 1500,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red"); box()
legend("topright", legend = c("nls exp fit", "visual exp fit"), lty = 1, col = c("black", "red"))

curve(
  exp_fun(x, .015, .02), 
  from = 0, to = 100, add = TRUE, ylim = c(0,100),
  main = "Ricker function",
  ylab = "f(x)", xlab = "x", col = "black")
par(mfrow=c(1,1))
plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n NLS & Visual fit lines", ylim=c(0, 2))

curve(
  ricker_fun(x, 0.025, .01), 
  from = 0, to = 1500, ylim = c(0, 1500), add = TRUE, 
  main = "Salamander Ricker function",
  ylab = "f(x)", xlab = "x", col="red")
dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  

lines(predict(fit_ricker_nls, newdata = dist_newdata))
legend("topright", legend = c("nls fit", "guess"), lty = 1, col = c("black", "red"))

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
dat_all$HEWA_pres = dat_all$HEWA > 0

dat_all$GCKI_pres = dat_all$GCKI > 0

GCKI_pres <- as.numeric(dat_all$GCKI >= 1)
plot(x = dat_all$elev, y = GCKI_pres, ylim = c(0,1))

# Create model fits
fit_GCKI_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_GCKI_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

summary(fit_GCKI_both_additive)

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)

ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)

slope_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_ba_tot,
    newdata = ba_newdata,
    type = "response", na.rm=TRUE
  )

AIC(
  fit_GCKI_ba_tot,
  fit_GCKI_slope,
  fit_GCKI_both_additive,
  fit_GCKI_both_interactive)
install.packages("AICcmodavg")
library(AICcmodavg)
models= list(fit_GCKI_ba_tot,
             fit_GCKI_slope,
             fit_GCKI_both_additive,
             fit_GCKI_both_interactive)
model.names= c("fit_GCKI_ba_tot",
               "fit_GCKI_slope",
               "fit_GCKI_both_additive",
               "fit_GCKI_both_interactive")
aictab(cand.set = models, modnames = model.names)
summary(fit_GCKI_both_interactive)
n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)
head(new_dat_all)

tail(new_dat_all)

new_dat_all$pred_add = predict(
  fit_GCKI_both_additive,
  newdata = new_dat_all,
  type = "response")

new_dat_all$pred_int = predict(
  fit_GCKI_both_interactive,
  newdata = new_dat_all,
  type = "response")

z_GCKI_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)
z_GCKI_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

require(rgl)

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_GCKI_add,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_GCKI_int,
  col = "steelblue",
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.4)
rglwidget()


par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_GCKI_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_GCKI_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")
#Create a color ramp function using heat colors
colorfunc = colorRampPalette(
  heat.colors(length(ba.tot)))

# Figure out the indices of the color ramp
col_indices_hewa_add = findInterval(
  new_dat_all$pred_add,
  seq(
    min(new_dat_all$pred_add),
    max(new_dat_all$pred_add),
    length.out = 50))

rgl::persp3d(
  x = ba.tot,
  y = slope,
  z = z_hewa_add,
  xlab = "Basal Area",
  ylab = "Slope",
  zlab = "Pr(present)",
  alpha = 0.9,
  col = colorfunc(50)[col_indices_hewa_add]
)
rglwidget()

n = 500

slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
  
ba_newdata = data.frame(
    ba.tot = seq(
      from = min(dat_all$ba.tot, na.rm = T),
      to = max(dat_all$ba.tot, na.rm = T),
      length.out = n
    )
)
plot(slope_newdata)
dat_all$GCKI_pres <- dat_all$GCKI >0
plot(x= dat_all$slope, y = dat_all$GCKI_pres, data=slope_newdata, na.rm=T, ylab="GCKI presence/absence", xlab="percent slope", main="GCKI pres/abs and percent slope")
curve(logistic_midpoint_slope(x, midpoint = 700, slope = -0.005), add = TRUE)
plot(x= dat_all$ba.tot, y = dat_all$GCKI_pres, data=ba_newdata, na.rm=T, ylab="GCKI presence/absence", xlab="Basal area", main="GCKI pres/abs and basal area")
curve(logistic_midpoint_slope(x, midpoint = 30, slope = 0.15), add = TRUE)
