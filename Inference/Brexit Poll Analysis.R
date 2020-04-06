library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481 #Final Proportion Voting Remain
d <- 2*p-1
N <- 1500

#Summary Statistics
p*N
sqrt(N*p*(1-p))
x_hat <- (d+1)/2
x_hat
se <- sqrt((x_hat*(1-x_hat))/N)
se
se_d <- 2*se
se_d

#Estimate total remainers given observed spread
brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread+1)/2, se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize))

#Brexit Poll Confidence Interval
brexit_polls <- brexit_polls %>%
mutate(lower = x_hat - qnorm(.975)*se_x_hat, upper = x_hat + qnorm(.975)*se_x_hat)
brexit_polls[1,]

june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_spread = 2*se_x_hat,
         d_hat = 2*x_hat - 1,
         lower_d = d_hat - qnorm(.975)*se_spread,
         upper_d = d_hat + qnorm(.975)*se_spread,
         hit_d = d > lower_d & d < upper_d)
nrow(june_polls)
mean(june_polls$hit_d)
mean(june_polls$lower_d>0)
mean(june_polls$lower_d<0 & june_polls$upper_d >0)

june_polls %>%
  group_by(pollster) %>%
  summarize(avg_hit = mean(hit_d), n = n()) %>%
  arrange(avg_hit)

june_polls %>% ggplot(aes(poll_type, spread)) +
  geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            lower = spread - qnorm(.975)*se_spread,
            upper = spread + qnorm(.975)*se_spread)
combined_by_type

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread +1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
two_by_two <- brexit_hit %>%
  group_by(poll_type, hit) %>%
  summarize(n = n()) %>%
  spread(poll_type, n)
two_by_two
chisq_test <- two_by_two %>% select(-hit) %>% chisq.test()
chisq_test$p.value

odds_online <- (two_by_two$Online[2] / sum(two_by_two$Online)) / 
  (two_by_two$Online[1] / sum(two_by_two$Online))
odds_online
odds_telephone <- (two_by_two$Telephone[2] / sum(two_by_two$Telephone)) / (two_by_two$Telephone[1] / sum(two_by_two$Telephone))
odds_telephone
select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%odds_online/odds_telephone

brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_hline(yintercept = -0.038, color = "black")

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "loess", span = 0.3)
