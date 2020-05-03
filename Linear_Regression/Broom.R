library(tidyverse)
library(Lahman)
data("Teams")

#Tibbles
#Inspect Data Frame and Tibble
Teams
as.tibble(Teams)

#Subsetting a Data Frame sometimes generates vectors while subsetting a tibble returns a tibble
class(Teams[,20])
class(as.tibble(Teams[,20]))

#Pulling a Vector from a tibble requires $
class(as.tibble(Teams)$HR)

#Access a Non-Existing Column in a Data Frame or Tibble (Useful Error Message)
as.tibble(Teams)$hr

#Create a Tibble with Complex Objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

#Do
#Use Do to fit a regression line to each HR stratum
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB / G,
         R = R / G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR <= 1.2)

dat %>% 
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

#Using Do without a column name generates an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

#Define a Function to extract slope from lm; Output is a data frame
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
}

dat %>%
  group_by(HR) %>%
  do(get_slope(.))

#Not the Desired Output (A Column containing data frames)
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))

#Data Frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}
dat %>%
  group_by(HR) %>%
  do(get_lse(.))

#Broom
library(broom)

#Use tidy to return lm estimates and related info as a data frame
fit <- lm(R ~ BB, data = dat)
tidy(fit, conf.int = TRUE)

#Pipeline with lm, do, and tidy
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

glance(fit)

#Assessment
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))

galton %>%
  group_by(pair) %>%
  summarize(n = n())

#Determine Max and Min Correlation for Child-Parent Pairings
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))

#Fit Regression Lines to Each Child-Parent Pairing
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))

