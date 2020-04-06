#Sampling Models
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

x <- sample(c(-1, 1),n, replace = TRUE, prob = c(9/19, 10/19))
S <- sum(x)
S

#Monte Carlo Simulation of Sampling Model for more robust estimation
n <- 1000 #number of roulette players
B <- 10000 #number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))
  sum(X)
})
mean(S < 0) #Probability the casino loses money

#Plot observed values to gauge distribution's normalcy
library(tidyverse)
s <- seq(min(S), max(S), length = 100)
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S)))
data.frame (S = S) %>% 
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#SAT Guessing Strategy
S <- replicate(B, {
  X <- sample(c(1, -.25), 44, replace = TRUE, prob = c(.2, .8))
  sum(X)
})
mean(S>=8)

#Removing the penalty for guessing and reducing the number of answer choices
S <- replicate(B, {
  X <- sample(c(1, 0), 44, replace = TRUE, prob = c(.25, .75))
  sum(X)
})
mean(S)

#Assessing Scoring Probability across a range of student skills
n <- 44
a <- 1
b <- 0
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)]) #minimum skill level required to score 35 points with 80% probability
