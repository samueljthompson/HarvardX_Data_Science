library(tidyverse)
library(Lahman)
library(broom)
data("Teams")

#Linear Regression with two variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data = .)
  tidy(fit, conf.int = TRUE)
  
#Regression with BB, Singles, Doubles, Triples, and HRs
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB / G,
         singles = (H - X2B - X3B - HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs
        
#Predict the number of runs for each team in 2002 and plot
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) +
  geom_point() +
  geom_text(nudge_x = 0.1, cex = 2) +
  geom_abline()

#AVergae number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean

#Compute per plate appearance rates for players
players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA) / pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

#Plot Player specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

#Add 2002 salary of each player (budget consideration)
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by = "playerID")

#Add Defensive Position
position_names <- c("G_p", "G_c", "G_1b", "G_2b", "G_3b", "G_ss", "G_lf", "G_cf", "G_rf")
tmp_tab <- Appearances %>%
  filter(yearID == 2002) %>%
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max)
players <- tibble(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by = "playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

#Add players first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by = "playerID")

#Top 10 Players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

#Players with higher metrics generally have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#Remake plot without players that debuted after 1998 (rookies skewed by few data points)
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()

#Picking Players via Linear Programming
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut < "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE)

#This algorithm chooses the nine players based off the above constraint definition
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#Measurement Error Models
library(dslabs)
falling_object <- rfalling_object()

#Drawing the Trajectory of the ball
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in Meters") +
  xlab("Time in Seconds")

#Estimating the theoretical coefficients
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance ~ time + time_sq, data = .)

#Check if estimated data fits observations
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

tidy(fit, conf.int = TRUE) #Confidence intervals 

#Assessment Part I
#Estiamte the effect of BB on Runs in 1971
fit <- Teams %>%
  filter(yearID == "1971") %>%
  mutate(BB = BB / G,
         HR = HR / G,
         R = R / G) %>%
  lm(R ~ BB + HR, data = .)
model <- tidy(fit, conf.int = TRUE)
model

#OR

Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "BB") %>%
    pull(estimate)

#Determine the effect of BB and HR on runs from 1961 to 2018
res <- Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() 

#Plot how this relationship changes over time
res %>%
    filter(term == "BB") %>%
    ggplot(aes(yearID, estimate)) +
    geom_point() +
    geom_smooth(method = "lm")

#Quantify the changing effect of BB over timee
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy()    

#Assessment Part II