
install.packages("palmerpenguins")
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

install.packages("simpleboot")
require(simpleboot)
#Parametric Two-Sample Test
#Perform a t-test of the alternative hypothesis that Adelie penguins have shorter flippers than Chinstrap penguins. Is this a one- or two-tailed test?
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
?two.boot()
c_fliplength = droplevels(subset(penguin_dat, species != "Chinstrap", na.rm= TRUE))
c_fliplength= c_fliplength$flipper_length_mm
a_fliplength = droplevels(subset(penguin_dat, species != "Adelie", na.rm= TRUE))
a_fliplength= a_fliplength$flipper_length_mm
pen_boot<- two.boot(c_fliplength, a_fliplength, na.rm = TRUE, FUN=mean, R=10000)
hist(pen_boot)
str(pen_boot)

pen_boot$t
sd(pen_boot$t)
boot.ci(pen_boot)
quantile(pen_boot$t, c(0.025, 0.975))
mean(pen_boot$t)
median(pen_boot$t)
?ecdf
pen_ecdf= ecdf(pen_boot$t)
1-pen_ecdf(-8)
require(here)
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, dat = veg)
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
wilcox.test(pine ~ treatment, data = dat_tree, alternative = "two.sided") 
?wilcox.test()
clipped= droplevels(subset(dat_tree, treatment != "clipped", na.rm= TRUE))
clipped= clipped$pine
control= droplevels(subset(dat_tree, treatment != "control", na.rm= TRUE))
control= control$pine
require(boot)

tree_boot= two.boot(control, clipped, na.rm=TRUE, FUN=mean, R=1000)
str(tree_boot)
boot.ci(tree_boot)
quantile(tree_boot$t, c(0.025, 0.975))
hist(tree_boot)

dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)
# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
#Now, examine the standardized data to see if our standardization worked:
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#simple linear reg
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#slope coefficient
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))
#resampling

#null dist
set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)
?numeric
#monte carlo rando
m = 10000 
result_mc = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  
  result_mc[i] = coef(fit_resampled_i)[2]
} 
head(result_mc)

hist(
  result_mc,
  main = "Liz's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = q, lty = 2, col = "blue", lwd = 2)

q=quantile(result_mc, c(.05))

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)
hist(
  result_boot,
  main = "Mike's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)
set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)

m = 10000 
result_boot = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  head(dat_boot)
  coef(fit_bs1)
  result_boot[i] = coef(fit_bs1)[2]
} 
hist(
  result_boot,
  main = "Liz's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)
par(mfrow = c(1, 1))
hist(
  result_mc,
  main = "Null",
  xlab = "Slope Parameter")
hist(
  result_boot,
  main = "Alternative",
  xlab = "Slope Parameter")

plot(
  density(result_mc),
  col= c("blue"),
  xlab = "Slope Coefficient")
plot(
  density(result_boot),
  main = "Liz's Double Density Plot",
  xlab = "Slope Coefficient",
  col= c("purple"),
  ylab= "Density")
lines(density(result_mc))

legend(-.05, 60, legend=c("Monte Carlo", "Bootstrap"),fill = c("black","purple"))

legend(
  'topleft', border = "black", text.col = "black",
  legend=c('Monte Carlo','Bootstrap'), fill = c("black", "purple"),
  lty=c("black", "purple"),col=c("black", "purple"), inset=c(.1,.1))
