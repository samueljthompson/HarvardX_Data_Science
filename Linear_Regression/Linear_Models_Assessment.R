library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G)

#Constructing Linear Models for Various Variable Relationships
Teams_small %>%
  mutate(R = R/G) %>%
  lm(avg_attendance ~ R, data = .) %>%
  tidy()
        
Teams_small %>%
  mutate(HR = HR/G) %>%
  lm(avg_attendance ~ HR, data = .) %>%
  tidy()

 Teams_small %>% 
   lm(avg_attendance ~ W, data = .) %>%
   tidy()

 Teams_small %>%
   lm(avg_attendance ~ yearID, data = .) %>%
   tidy()
 
 #Comparing Correlation
 Teams_small %>%
   summarize(r1 = cor(R/G, W),
             r2 = cor(HR/G, W))
 
 #Constructing a Stratified Data Set
Teams_small_strat <- Teams_small %>%
   mutate(W_round = round(W/10)) %>%
   filter(W_round >= 5 & W_round <= 10)
Teams_small_strat %>% filter(W_round == 8) %>% nrow()
   
#Calculating a regression line for stratified data set
Teams_small_strat %>%
  group_by(W_round) %>%
  mutate(R_per_game = R/G) %>%
  summarize(slope = cor(R_per_game, avg_attendance)*sd(avg_attendance)/sd(R_per_game))

Teams_small_strat %>%
  group_by(W_round) %>%
  mutate(HR_per_game = HR/G) %>%
  summarize(slope = cor(HR_per_game, avg_attendance)*sd(avg_attendance)/sd(HR_per_game))

#Construct a Multivariate regression
multi_fit <- Teams_small %>%
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
multi_coefs <- tidy(multi_fit, conf.int = TRUE)
multi_coefs

#Predict attendance given parameters
df <- data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002)
predict(multi_fit, df)

df <- data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960)
predict(multi_fit, df)

#Compare Estimated and Actual Attendance Correlation
Teams_attend <- Teams %>% filter(yearID == 2002) %>%
  mutate(R_per_game = R/G,
         HR_per_game = HR/G,
         avg_attendance = attendance/G) %>%
  mutate(R_hat_attend = predict(multi_fit, newdata = .))
Teams_attend %>% summarize(r = cor(R_hat_attend, avg_attendance))
