library(tidyverse)
library(dslabs)
take_poll(25)

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt((p*(1-p))/N)
plot(p, se)

# This For-Loop plots various proportions across three different sample sizes
sample_sizes <- c(25, 100, 1000)
for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
}

#Computing the Probability of X_Bar being within 0.01 of p
blue <- 12
red <- 13
x_hat <- blue / (blue + red)
se <- sqrt(x_hat*(1-x_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

#Monte Carlo Simulation using a set value of P
p <- 0.45
N <- 1000
#Simulating one Poll with N Draws
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p)) 
x_hat <- mean(x)
#Simulating B polls each with N Draws
B <- 1000
N <- 1000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

#Construct Histogram snd QQPlot of Monte Carlo Results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")

p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd(x_hat))) +
  geom_abline() +
  ylab("x_hat") +
  xlab("Theoretical normal")

grid.arrange(p1, p2, nrow = 1)

#PLotting Margin of Error in an extremely large poll over a range of values p
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()

#Geom_smooth Confidence Interval Example
library(tidyverse)
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

#Mone Carlo Simulation of Confidence Intervals
p <- 0.45
N <- 1000
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat) #True of p is in interval
})
mean(inside)

c(X_hat - 2*SE_hat, X_hat + 2*SE_hat) #Mathematical Calculation of Interval

#Solving for Z with qnorm
z <- qnorm(0.95) #Stores the previously calculated interval range
pnorm(qnorm(0.95))
pnorm(qnorm(1-0.95))
pnorm(z)-pnorm(-z) #Validates calcultation by printing desired quantile range

#Poll Simulation
d <- 0.039 #Actual Spread
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 32, 1291, 1056, 2172, 516) #Poll Sample Sizes
p <- (d+1)/2 #Formula for calculating observed value from the spread
#Calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1 #generates spread estimate with upper and lower bounds
})

#Generate a data frame storing the results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")

#Calculating the spread of the aggregated polls
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% .$avg
p_hat <- (1+d_hat)/2
moe <- 2*qnorm(.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
round(d_hat*100,1)
round(moe*100,1)

#Generating Simulated Poll Data
library(dslabs)
data("polls_us_election_2016")
names(polls_us_election_2016)
#Filtering polls for those within one week of election and with reliable grades
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
#Add Spread Estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
#Compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
#Compute margin of error
p_hat <- (d_hat + 1)/2
moe <- qnorm(.975)*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
#Histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color = "black", binwidth = .01)
#Investigating Pollster Bias
polls %>% group_by(pollster) %>% summarize(n())
polls %>% group_by(pollster) %>%
  filter(n() >=6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Standard error for each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 *sqrt(p_hat*(1-p_hat)/median(samplesize)))

#Data Driven Model Incorporating Pollster Variability
#Collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
#Histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
#Construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(.975)*se, end = avg + qnorm(.975)*se)
round(results*100, 1)

#Monte Carlo Simulation of Bayes Theorem (Frequentist Statistics)
prev <- 0.00025 #Disease Prevalence
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
N_D <- sum(outcome == "Disease")
N_H <- sum(outcome == "Healthy")
#Randomly determine if the test is possitive or negative
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace = TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("+", "-"), N_H, replace = TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)

#Computing the Posterier Mean and Credible Interval
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
posterior_mean
posterior_se
#95% Credible Interval
posterior_mean + c(-qnorm(.975), qnorm(.975))*posterier_se
#Probability of d>0 (Clinton Wins Popular Vote)
1 - pnorm(0, posterior_mean, posterior_se)
#Adjustment for General Bias Variability
sigma <- sqrt(results$se^2 + .025^2)
B <- sigma^2/(sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
1-pnorm(0, posterior_mean, posterior_se)

#Research Funding Example
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates
#Compute totals that were successful or not
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
#Compare percentage of men and women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men), 
                     percent_women = yes_women/(yes_women + no_women))

#Two by Two Table and P-Value for "Lady Tasting Tea" (Fisher Test)
tab <- matrix(c(3, 1, 1, 3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab
fisher.test(tab, alternative = "greater")

#Two by Two Table and P-Value for Research Problem (Chi Squared)
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two
#Compute the null hyposthesis for the two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men +totals$ye_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))
#Chi Squared Test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value
#Odds Ratio
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) / 
  (two_by_two$men[1] / sum(two_by_two$men))
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) / (two_by_two$women[1] / sum(two_by_two$women))
odds_men/odds_women

