options(digits = 3)
library(tidyverse)
library(dslabs)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#Studying Passenger Composition#
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = .2, bw = 1, position = "stack")

titanic %>% filter(Age==40) %>% count(Sex == "male")
titanic %>% count(Sex == "male")
titanic %>% filter(Age >= 18 & Age <= 35) %>% count(Sex == "male")
which.max(titanic$Age)
titanic$Sex[631]

#QQ Plot#
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

#Survival Barplot#
titanic %>%
  ggplot() +
  geom_bar(aes(Sex, fill = Survived), position = position_dodge())

#Survival Density Plot (Age)#
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(bw = 1, alpha = 0.2)

#Survival by Fare#
titanic %>%
  filter(!Fare==0) %>%
  ggplot(aes(Survived, Fare), group_by(Survived)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2) +
  scale_y_continuous(trans = "log2")

#Survival by Class (Proportion)#
titanic %>% count(Pclass)
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_dodge())

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

#Survival Density Plot Grid#
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, bw = 5) +
  facet_grid(Sex ~ Pclass)