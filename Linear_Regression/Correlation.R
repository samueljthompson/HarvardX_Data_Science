library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Summary Statistics
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

#Scattorplot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

#Correlation Coefficient
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

#Compute Sample Correlation (Random Variable)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))

#Monte Carlo Simulation illustrating distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
mean(R)
sd(R)

#QQ Plot to determine if N is sufficiently large
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

#Assessment 1 (Baseball)
library(Lahman)
data(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(r1 = cor(AB/G, R/G),
                    r2 = cor(E/G, W/G), 
                    r3 = cor(X3B/G, X2B/G))

#Stratification
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

#Center each Boxplot for Stratified Heights (Crude Regression Line)
galton_heights %>% mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

#Calculate Values to Plot Regression Line on Original Data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

#Bivariate Normal Distribution confirmed by plotting stratified son heights against standardized father heights
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>% filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap(~ z_father)

#The Regression line is contingent on the dependent and independent variable order
m_1 <- r * s_y / s_x
m_1
b_1 <- mu_y - m_1*mu_x
b_1

m_2 <- r * s_x / s_y
m_2
b_2 <- mu_x - m_2*mu_y
b_2

#Assessment 2
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)

female_heights %>%
  summarize(mu_x = mean(mother),
            s_x = sd(mother),
            mu_y = mean(daughter),
            s_y = sd(daughter),
            r = cor(mother, daughter))

mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * s_y/s_x
m
b <- mu_y - m*mu_x
b
#Percent Variation in y explained by x
(r^2)*100
#Conditional Expected Value of y given x = 60
mu_y + r*((60 - mu_x)/s_x)*s_y