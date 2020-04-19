#Import US Murders Data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

#Import US Election Results Data
data("polls_us_election_2016")
head(polls_us_election_2016)
identical(results_us_election_2016$state, murders$state) #This establishes that the two sets are not perfect matches

#Join the murders tabe and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

#Experiment with two smaller tables
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

#Binding
bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

#Set Operators
#Intersect Vectors or Data Frames
intersect(1:10, 6:15)
intersect(c("a", "b", "c"), c("b", "c", "d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

#Perform Union of Vectors or Data Frames
union(1:10, 6:15)
union(c("a", "b", "c"), c("b", "c", "d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

#Set Difference
setdiff(1:10, 6:15)
setdiff(c("a", "b", "c"), c("b", "c", "d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

#Set Equal to determine whether sets have the same elements
setequal(1:10, 6:15)
setequal(c("a", "b", "c"), c("b", "c", "d"))
setequal(tab1, tab2)

#Assessmentlibrary("Lahman")
library(Lahman)
top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>% 
  slice(1:10)
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

AwardsPlayers %>% filter(yearID == 2016) %>%
  inner_join(top_names) %>%
  select(nameFirst, nameLast) #How many top ten home run hitters won an award?

AwardsPlayers %>% filter(yearID == 2016) %>%
  anti_join(top_names) %>%
  select(playerID) %>%
  unique() %>%
  nrow() #How many award recipients weren't in the top ten?
