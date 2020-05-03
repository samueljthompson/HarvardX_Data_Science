library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)

#Compute RSS for any pair of Beta0 and Beta1 in Galton Data
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
  return(sum(resid^2))
}

#plot RSS as a function of beta1 when beta0 = 25 (otherwise this should be a three dimensional plot)
beta1 = seq(0, 1, len = nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) +
  geom_line() +
  geom_line(aes(beta1, rss))

#Fit Regression Line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit
#Summary Statistics
summary(fit)


#Monte Carlo Simulation (LSE)
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

#Plot distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

#Summary Statistics
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#LSE Correlation
lse %>% summarize(cor(beta_0, beta_1))
#Correlation Following Standardization of dependent variable (transformation influences correlation)
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef
})
cor(lse[1,], lse[2,])

#Plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

#Predict Y Directly
fit <- galton_heights %>% lm(son ~ father, data = .)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

#Plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
  ggplot(aes(father, Y_hat)) +
  geom_line()

#Plot With Confidence Interval (Option 2)
model <- lm(son~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_point(data = galton_heights, aes(x = father, y = son))

#Assessment 1
library(Lahman)
data("Teams")
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R/G,
         BB_per_game = BB/G, 
         HR_per_game = HR/G)
fit_1 <-  lm(R_per_game ~ BB_per_game, data = Teams)
summary(fit_1)
fit_2 <- lm(R_per_game ~ HR_per_game, data = Teams) %>% summary
fit_2
fit_12 <- lm(R_per_game ~ BB_per_game + HR_per_game, data = Teams) %>% summary
fit_12

#Assessment 2
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)

female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data = female_heights)
summary(fit)
predict(fit)[1]
female_heights$mother[1]

#Assessment 3
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    summarize(mean_singles = mean(singles), mean_bb = mean(bb))
head(bat_01)

bat_01 %>% filter(mean_singles > 0.2 & duplicated(playerID) == FALSE) %>% nrow()
bat_01 %>% filter(mean_bb > 0.2 & duplicated(playerID) == FALSE) %>% nrow()

bat_12 <-inner_join(bat_01, bat_02, by = "playerID")
head(bat_12)

bat_12 %>% summarize(r1 = cor(singles, mean_singles),
                     r2 = cor(bb, mean_bb))

bat_12 %>%
  ggplot(aes(singles.y, mean_singles)) +
  geom_point()

bat_12 %>%
  ggplot(aes(bb.y, mean_bb)) +
  geom_point()

fit_12 <- lm(singles ~ mean_singles, data = bat_12)
summary(fit_12)
fit_12_bb <- lm(bb ~ mean_bb, data = bat_12)
summary(fit_12_bb)
