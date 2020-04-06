#Cumulative Distribution Function
library(tidyverse)
library(dslabs)
data(heights)

x <- heights %>% filter(sex == "Male") %>% pull(height)
F <- function(a) mean(x <= a)
1 - F(70) #Probability of a Male taller than 70 inches

#Estimating the above probability using the normal approximation
1 - pnorm(70, mean(x), sd(x))

#Determining a Percentile
qnorm(.99, mean(x), sd(x))

#Plot Distribution of exact heights
plot(prop.table(table(x)), xlab = "a = Height in Inches", ylab = "Pr(x = a)")

#Plot the Probability Density Function for a Normal Distribution
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#Generate Normally Distributed Random Numbers
x <- heights %>% filter(sex == "Male") %>% pull(height)
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

#Graphical depiction of a randomly generated, normal distribution
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color = "black", binwidth = 2)

#Monte Carlo Simulation within a randomly generated data set of 800 observations
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest >= 7*12) #Probabiliy of being taller than 7 feet

#Generating Random ACT Distributions
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
act_avg <- mean(act_scores)
act_sd <- sd(act_scores)

#Comparing Randomly Generated Values with Theoretical
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()