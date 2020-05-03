#Spearman Correlation to combat outliers
#simulate independent X, Y and standardize all except entry 23 (thus creating an outlier)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

qplot(x, y, alpha = 0.5) #Plot illustrating the outlier

#Outlier creates spurious correlation
cor(x,y)
cor(x[-23], y[-23])

#The Spearman correlation avoids this by sorting according to rank
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

cor(x, y, method = "spearman")

#Assessment
library(dslabs)
library(tidyverse)
data("research_funding_rates")
research_funding_rates

#Create a Table illustrating award recipients by sex
two_by_two <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men, 
              no_men = applications_men - awards_men, 
              yes_women = awards_women, 
              no_women = applications_women - awards_women) %>%
    gather %>%
    separate(key, c("awarded", "gender")) %>%
    spread(gender, value)
  two_by_two
  
  #Calculate Percentages
  two_by_two %>% 
    mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
    filter(awarded == "yes") %>%
    pull(women)
  
  #Chi Squared Test to determine significance
  library(broom)
  two_by_two %>%
    select(-awarded) %>%
    chisq.test() %>%
    tidy()
  
  #Testing for Simpson's Paradox
  dat <- research_funding_rates %>% 
      mutate(discipline = reorder(discipline, success_rates_total)) %>%
      rename(success_total = success_rates_total,
             success_men = success_rates_men,
             success_women = success_rates_women) %>%
      gather(key, value, -discipline) %>%
      separate(key, c("type", "gender")) %>%
      spread(type, value) %>%
      filter(gender != "total")
dat
  
dat %>% 
      ggplot(aes(discipline, success, size = applications, color = gender)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_point()
