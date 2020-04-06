#Interest Rate Sampling Model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Interest Rate Monte Carlo Simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})

#Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, color = "black")

#Expected Value and Standard Error of the sum of 1000 loans
n*(p*loss_per_foreclosure + (1-p)*0) #expected value
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p)) #standard error

#Calculating the interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p))) #Algebraic result of determining the z score and solving for x
x
x/180000
loss_per_foreclosure*p + x*(1-p) #expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) #expected value of the profit over n loans

#Checking theoretical approximation with Monte Carlo Simulation
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, prob = c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)
mean(profit<0)

#Expected value with higher default and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#Calcultating the number of loans required to achieve desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2 * (x-l)^2 * p * (1-p)) / (l*p + x * (1-p))^2)
n
n * (loss_per_foreclosure * p +x * (1-p))

#Monte Carlo Simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate (B, {
  draws <- sample(c(x, loss_per_foreclosure), n, prob = c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)

#Monte Carlo simulation with unkown default probability (range within which a shock affects all borrowers)
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1) 
  draws <- sample(c(x, loss_per_foreclosure), n, prob = c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)
mean(profit < 0)
mean(profit < -10000000)

#Profitability of Life Insurance Claims
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

death_prob %>% filter(age == 50 & sex == "Female")
p <- seq(.01, .03, .001)
n <- 1000
cost_death <- -150000
premium <- 1150

ev <- n*(p*cost_death+(1-p)*premium)
se <- sqrt(n)*abs(premium-cost_death)*sqrt(p*(1-p))
plot(p, pnorm(0, ev, se))
plot(p, pnorm(-1000000, ev, se))

p_loss <- .015
set.seed(25, sample.kind = "Rounding")
claim <- sample(c(cost_death, premium), n, prob = c(p_loss, 1-p_loss), replace = TRUE)
sum(claim)/10^6

set.seed(27, sample.kind = "Rounding")
B <- 10000
profit <- replicate(B, {
  claim <- sample(c(cost_death, premium), n, prob = c(p_loss, 1-p_loss), replace = TRUE)
  sum(claim)
})
mean(profit < -1000000)

#Calculating the premium required for a 5% chance of losses
z <- qnorm(0.05)
x <- -cost_death*(n*p_loss - z*sqrt(n*p_loss*(1-p_loss))) / (n*(1-p_loss) + z*sqrt(n*p_loss*(1-p_loss)))
x
cost_death*p_loss + x*(1-p_loss) #expected value of the profit per loan
n*(cost_death*p_loss + x*(1-p_loss)) #expected value of the profit over n loans

set.seed(28, sample.kind = "Rounding")
profit <- replicate(B, {
  claim <- sample(c(cost_death, x), n, prob = c(p_loss, 1-p_loss), replace = TRUE)
  sum(claim)
})
mean(profit < 0)

#Randomly changing probability of death
set.seed(29, sample.kind = "Rounding")
profit <- replicate(B, {
  p <- p_loss + sample(seq(-0.01, 0.01, length = 100), 1)
  claim <- sample(c(cost_death, x), n, prob = c(p, 1-p), replace = TRUE)
  sum(claim)
})
mean(profit)
mean(profit<0)
mean(profit < -1000000)