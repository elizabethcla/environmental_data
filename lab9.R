require("here")
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
#Binomial Test for Proportions
#Reproductive Success and Failure
#What is the evidence that reproductive success is more (or less) likely than reproductive failure?
#How likely is a response of 33/61 if the reproductive success and failure are equally likely, i.e., Pr(success)=0.5?
#We can use a binomial test for this, specifying the number of successes (33) and the total sample size (61), as follows:
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)

#Reproductive Catastrophe and Late Filling
#Let’s define variables to hold the late- and normal-filling rates:
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

#What is the evidence that reproductive success is more or less frequent than the normal-filling rate?
#In this scenario, we expect successful reproduction in approximately 5 of every 7 years.
#We can modify the code we used above to test the observed reproduction success rate against the normal-filling rate:
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 

#In addition, note again that the default test is a two-sided alternative
#We might instead prefer the one-sided alternative hypothesis that the observed success rate is less than the pond normal-filling rate. We can perform the test as follows:
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

#Comparison with one-sample tests.
#What do these binomial tests say about the success (or catastrophe) rate?
#Do these results agree with the Student’s t test and/or Wilcoxon’s signed rank test?
  #Which test do think is most appropriate? Why?
t.test(catrate$cat.rate, mu = 2/7)

#Two sample tests
#compare two variences
#The simplest test is called Fisher’s F test, based on the F-statistic.
#The F-statistic represents the ratio between two variances.
#In order to be significantly different, the ratio will need to be significantly smaller or larger than 1, depending on whether the smaller variance is in the numerator or denominator.
veg = read.csv(here("data", "vegdata.csv"))
head(veg)
boxplot(pine ~ treatment, data = veg)

#variance test
veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

# verify that treatment is factorized
veg2$treatment = factor(veg2$treatment)
#Now, perform the test:
var.test(
  pine ~ treatment,
  data = veg2)
#F-tests Assumes Normality
#Note that Fisher’s F test for unequal variances assumes that the data are normally distributed.
shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])
#Note, because the Shapiro-Wilk test is a one-sample test, we had to select the records for each treatment and conduct separate tests.

#Non-parametric Variance Test
#If the results indicate that the data are non-normal, then we should use a non-parametric test of homogeneity of variances, such as the Fligner-Killeen test, as follows:
fligner.test(
  pine ~ treatment,
  data = veg2)

#Tests for multiple variances
#The n-sample parametric test is called Bartlett’s test, which we can use to test for homogeneity of variances among all four treatment levels as follows:
bartlett.test(pine ~ treatment, data = veg)




birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

chisq.test(br_creeper_table)

require("palmerpenguins")
fit_species = 
  lm(
    formula = body_mass_g ~ species,
    data = penguins)
fit_species
fit_fl_sp = 
lm(
  formula = flipper_length_mm ~ species,
  data = penguins)

mass as predicted by sex.
fit_sex = 
  lm(
    formula = body_mass_g ~ sex,
    data = penguins)
fit_sex

fit_both = 
  lm(
    formula = body_mass_g ~ sex:species,
    data = penguins)
fit_both

boxplot(
  formula = body_mass_g ~ sex:species,
  data = penguins, 
  main = "Liz's fit_both conditional boxplot", 
  names = c("F Adelie", "M Adelie", "F Chinstrap", "M Chinstrap", "F Gentoo", "M Gentoo"))
bartlett.test(body_mass_g ~ sex:species, data = penguins)

dat_groups = aggregate(
  body_mass_g ~ sex:species,
  data = penguins,
  FUN = c)
bartlett.test(dat_groups$body_mass_g, data = penguins)
require("here")
dat_fl =  read.csv(here("data", "trees_FL.csv"))
head(dat_fl)

par(mfrow = c(2,2))

barplot(
  table(dat_fl$ProbabilityofFailure), 
  main = "dat_fl\nProbabilityofFailure",
  ylab = "tree count")

barplot(
  table(dat_fl$Failure_Standardized), 
  main = "dat_fl\nFailure_Standardized",
  ylab = "tree count")

hist(dat_fl$DBH_in,
     main = "Hist DBH")

plot(dat_fl$DBH_in, dat_fl$HeighttoTop_ft,
     main = "scatterplot",
     xlab = "DBH",
     ylab = "Height")

head(dat_fl$DBH_in)

whole_dat_fl = droplevels(subset(dat_fl, Failure_Standardized == "whole", na.rm = TRUE))
none_dat_fl = droplevels(subset(dat_fl, Failure_Standardized == "none", na.rm = TRUE))

ks.test(dat_fl$DBH_in, dat_fl$HeighttoTop_ft)
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
cor.test(
  dat_fl$DBH_in, dat_fl$HeighttoTop_ft,
  use='complete.obs')

dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")
fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2

chi_fl=chisq.test(fl_table_2)
chi_fl$expected
round(chi_fl$observed - chi_fl$expected,
      digits = 1)
