data(iris)
fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)
summary(fit_species)
head(fit_species)
boxplot(iris$Sepal.Length ~ iris$Species,
        main = "Boxplots of Sepal Length by Species",
        ylab = "Sepal Length",
        xlab = "Species")
setosa=subset(iris,Species== "setosa" ) 
mean(setosa$Sepal.Length)
vir=subset(iris,Species== "virginica" ) 
mean(vir$Sepal.Length)
5.006+1.5820
shapiro.test(residuals(fit_species))

plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

fit_petals = 
  lm(
    Petal.Width ~ Petal.Length,
    data = iris)
summary(fit_petals)
fit_petals$fitted.values
str(fit_petals)
4/0.415755 
shapiro.test(residuals(fit_petals))
