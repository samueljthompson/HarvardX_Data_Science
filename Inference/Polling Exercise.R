# Load the data
data(polls_us_election_2016)
#Filter for polls that ended on or after October 31, 2016 in the United States
polls <- filter(polls_us_election_2016, state == "U.S." & enddate >= "2016-10-31")
# How many rows does `polls` contain? Print this value to the console.
nrow(polls)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# For the first poll in `polls`, assign the estimated percentage of Clinton voters
X_hat <- polls$rawpoll_clinton[1]*.01
X_hat
# Calculate the standard error 
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters
ci <- c(X_hat - qnorm(.975)*se_hat, X_hat + qnorm(.975)*se_hat)

polls <- mutate(polls,
  X_hat = polls$rawpoll_clinton*.01,
  se_hat = sqrt(X_hat*(1-X_hat)/polls$samplesize),
  lower = X_hat - qnorm(0.975)*se_hat,
  upper = X_hat + qnorm(0.975)*se_hat)
pollster_results <- select(polls, pollster, enddate, X_hat, se_hat, lower, upper)

#Determine the proportion of poll intervals that contain the true value of .482
avg_hit <- mutate(pollster_results, hit = if_else(.482>lower & .482<upper, 1, 0)) %>%
summarize(mean = mean(hit))

#summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
# Calculate the p-value
2*(1-pnorm(estimate/se_hat))

#Electoral College Forecasting
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)
#Computing the Average and Standard Deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
            !grepl("CD", "state") &
            enddate >= "2016-10-31" &
            (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
#10 Closest Races
results %>% arrange(abs(avg))
#Joining Electoral College Votes and Results
results <- left_join(results, results_us_election_2016, by = "state")
#Sates with no Polls
results_us_election_2016 %>% filter(!state %in% results$state)
#Assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
#Calculating the Posterior Distribution
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                  B = sigma^2 / (sigma^2 +tau^2),
                  posterior_mean = B*mu + (1-B)*avg,
                  posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
#Monte Carlo Simulation Including General Bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>% #Award Electoral Votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%
    .$clinton + 7 #7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269) #Over 269 votes to win the Electoral College

#Polling Variability across time
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
#The observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se
#The Distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color= "black")
#Spread Prediction Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >+ 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)
#Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

#Calculating 95% Confidence Intervals with the T Distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread) / sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)
#quantile from T Distribution vs Normal Distribution
qt(0.975, 14) #14 Degrees of freedom
qnorm(0.975)

#Numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c("A-", "C-")) %>%
group_by(grade, hit) %>%
summarize(n = n()) %>%
spread(grade, n)
totals
# Print the proportion of hits for grade A- polls to the console
totals$"A-"[2]/(totals$"A-"[1]+totals$"A-"[2])
# Print the proportion of hits for grade C- polls to the console
totals$"C-"[2]/(totals$"C-"[1]+totals$"C-"[2])
#Chi Squared Test
chisq_test <- totals %>% select(-hit) %>% chisq.test()
chisq_test$p.value