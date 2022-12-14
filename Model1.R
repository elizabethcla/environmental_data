require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
#Let’s look at a histogram:
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body"
dat_ade$sex <- as.factor(dat_ade$sex)
levels(dat_ade$sex) <- c("F","M")

boxplot(dat_ade$body_mass_g ~ dat_ade$sex,
        main = "Boxplots of Penguin Body Mass by Sex",
        ylab = "Body mass (g)",
        xlab = "Sex")
femalepen=subset(dat_ade,sex== "F" ) 
malepen=subset(dat_ade,sex== "M" ) 
?t.test
mean(femalepen)
t.test(femalepen$body_mass_g, mu=0, alternative="greater")
t.test(femalepen$body_mass_g, malepen$body_mass_g)
t.test(malepen$body_mass_g, femalepen$body_mass_g, mu=0, alternative="greater", paired="TRUE")

t.test(malepen$body_mass_g, femalepen$body_mass_g, alternative = "greater", paired = TRUE, var.equal = FALSE) 

t.test(malepen$body_mass_g, femalepen$body_mass_g, alternative = "less", paired = TRUE, var.equal = FALSE) 

catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
par(mfrow = c(1,1))
hist(catrate$cat.rate,
     xlab = "Catastrophe Rate",
     main= "Histogram of Salamander Catastrophic Rate"
)
shapiro.test(catrate$cat.rate)

t.test(catrate$cat.rate, mu = 2/7)
wilcox.test(catrate$cat.rate, mu = 2 / 7)
require(datarium)
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chin = subset(penguin_dat, species == "Chinstrap")          
t.test(dat_adelie$flipper_length_mm)
t.test(dat_chin$flipper_length_mm)

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chin$flipper_length_mm)

par(mfrow = c(1,2))
hist(dat_adelie$flipper_length_mm, xlab="flipper length(mm)", main= "Adelie flipper length", ylim= c(0,50), xlim = c(170, 215))
hist(dat_chin$flipper_length_mm, xlab="flipper length(mm)", main= "Chinstrap flipper length",ylim= c(0,20),  xlim = c(170, 220))
t.test(flipper_length_mm ~ species, 
       data = penguin_dat,)

?hist
require(nortest)
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

