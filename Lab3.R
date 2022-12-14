install.packages("psych")
install.packages("mnormt")
require("psych")
pairs.panels(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])
install.packages("here")


require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
dat_hab = read.csv(here("data", "hab.sta.csv"))
par(mfrow = c(3, 1))
hist(dat_hab$elev)
hist(dat_hab$aspect)
hist(dat_hab$slope)
head(dat_hab
     )
dat_all = merge(dat_bird, dat_hab, by = c("basin", "sub", "sta"))
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

par(mfrow = c(3, 1))
guess_x = 25
guess_y = 0
guess_slope = 0.05

plot(ba.tot ~ elev, data = dat_hab, cex = .05)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, col = "purple")
guess_x = 25
guess_y = 25
guess_slope = 0
plot(ba.tot ~ aspect, data = dat_hab, cex = .05)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, col = "green")
guess_x = 50
guess_y = 50
guess_slope = -0.1
plot(ba.tot ~ slope, data = dat_hab, cex = .05)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T, col = "blue")

sample(dat_all$CEWA, 100)

my_vec = rep(1:3, 5)
my_vec == 3

my_vec > 1

dat_all$CEWA >= 1

cewa_present_absent <- as.numeric(dat_all$CEWA >= 1)
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}

# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

head(dat_bird)
pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.tot")])


sample(dat_all$CBCH, 100)
dat_all$CBCH >= 1
cbch_present_absent <- as.numeric(dat_all$CBCH >= 1)
plot(x = dat_all$ba.tot, y = cbch_present_absent)

sample(dat_all$WIWA, 100)
dat_all$WIWA >= 1
wiwa_present_absent <- as.numeric(dat_all$WIWA >= 1)
plot(x = dat_all$ba.tot, y = wiwa_present_absent, col=adjustcolor(col = 1, 0.5), pch = 16)
pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.tot")])

plot(x = dat_all$ba.con, y = wiwa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 60, slope = 0.1), add = TRUE)


curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.1), add = TRUE)

plot(x = dat_all$ba.tot, y = wiwa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.005), add = TRUE)

pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.con")])
plot(x = dat_all$ba.con, y = wiwa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 100, slope = 0.1), add = TRUE)


curve(logistic_midpoint_slope(x, midpoint = 60, slope = -0.1), add = TRUE)

plot(x = dat_all$ba.con, y = wiwa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 60, slope = -0.005), add = TRUE)

pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.hard")])
plot(x = dat_all$ba.hard, y = wiwa_present_absent)

curve(logistic_midpoint_slope(x, midpoint = 50, slope = 0.1), add = TRUE)

pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.snag")])
plot(x = dat_all$ba.snag, y = wiwa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 50, slope = 0.1), add = TRUE)
plot(x = dat_all$ba.hard, y = cbch_present_absent)
plot(x = dat_all$ba.tot, y = cbch_present_absent, col=adjustcolor(col = 1, 0.5), pch = 16)
title("Likelihood of occurance for WIWA")
pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.tot")])
plot(x = dat_all$ba.tot, y = cbch_present_absent, xlab = "total basal area", ylab = "likelihood WIWA presents or absence")

title("Likelihood of occurance for WIWA")
pairs.panels(dat_all[, c("elev", "slope", "aspect", "ba.hard")])
plot(x = dat_all$ba.hard, y = wiwa_present_absent, xlab = "total basal area of hardwoods", ylab = "likelihood CBCH presents or absence")
curve(logistic_midpoint_slope(x, midpoint = 50, slope = 0.1), add = TRUE)

sample(dat_all$GRJA, 100)
sum(dat_all$GRJA >= 1)
sum(dat_all$GRJA)
grja_present_absent <- as.numeric(dat_all$grja >= 1)
plot(x = dat_all$ba.tot, y = grja_present_absent)

x_bin = 0:5
y_bin_2 = dbinom(x_bin, size = 5, prob = 0.4) 
y_bin_2

dbinom(x=4, size = 6, prob = 4/6, log = FALSE) 
pbinom(q = 4, size = 6, prob = 2/3, lower.tail = TRUE, log.p = FALSE)

pbinom(q = 4, size = 6, prob = 2/3, lower.tail = FALSE, log.p = FALSE)

cat(letters[sample(26, size = 3, replace = T)], sep = "")
