install.packages("here")
require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
dat_habitat = read.csv(here("data", "hab.sta.csv"))
x_observed = c(2, 6)
print(x_observed)
dpois(x = 2, lambda = 4.5) #probability mass
dpois(x = 6, lambda = 4.5)
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5) #know the likelihood of observing those particular counts together is the product of the individual likelihoods
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5) #I can take advantage of vectorization in R by storing the counts in a single vector
prod(dpois(x = wiwa_counts, lambda = 4.5))
#And the sum of log-likelihoods like:
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
#Now let’s say I want to find the value of that maximizes the likelihood for the counts of Wilson’s Warblers.
dat_all = merge(dat_bird, dat_habitat)
summary(dat_all$WIWA)
hist(dat_all$WIWA)
#Let’s try setting the breaks argument to 7. This will suggest to R that it should create 7 bins (corresponding to observations between 0 and 7 wrens):
hist(dat_all$WIWA, breaks = 7)
# we need to un-group the zero and one counts so that we see them as distinct bars in the histogram. We used a single number for the breaks argument to tell to try to automatically figure out how to divide the counts into 7 bins. You can use a vector for the breaks argument. R attempts to estimate bin breakpoints when breaks is a single number, but it will honor your input if you provide a vector of breakpoints
hist(dat_all$WIWA, breaks = 0:7)
#We can trick R into only counting the lower endpoint if we cleverly manipulate the sequence that we give to bins.
0:7 - 0.5
#If we write code that subtracts a single value (a scalar in linear algebra lingo) from a vector, R will subtract the number from each element in the vector and return the output.
hist(dat_all$WIWA, breaks = 0:7 - .5)
#If we wanted to use code like this with data for which we didn’t know the maximum value ahead of time we could write:
par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")
#I’ll try a Poisson distribution with lambda = 1.0:
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

wiwr_counts = c(2, 6)
dbinom(x = wiwr_counts, size= 6, prob = 0.5)
sum(log(dpois(x = wiwa_counts, lambda = 4)))

sum(log(dpois(x = dat_all$WIWR, lambda = 1.46)))
sum(log(dbinom(x = wiwr_counts, size= 6, prob = 0.5)))
hist(dat_all$WIWR)
set.seed(1)
dnorm(x= wiwr_counts, 4, 1)

set.seed(1) 
vec_rnorm = rnorm(n = 10, mean = .0001, sd = .0001) 
sum(log(dnorm(vec_rnorm, mean=0, sd=1))) 
?set.seed
set.seed(1)  

vec_rnorm = rnorm(n = 10, mean = 0, sd = 0.5) 

sum(log(dnorm(vec_rnorm, mean=0,sd=0.5))) 

-5.087216 
