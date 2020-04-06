#Load the Relevant Data and Packages#
library(dslabs)
data(heights)

#Create a Summary Table#
s <- heights %>%
  filter(sex=="Male") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

#Generate alternate Summary Statistics#
heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))

#Using the Dot Placeholder in Dplyr#
data(murders)
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate

#Grouping Summary Statistics#
data(heights)
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

data(murders)
murders <- murders %>% 
  mutate(murder_rate = total / population * 100000)
murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))

#Sorting Data Tables#
data(murders)
murders <- murders %>% 
  mutate(murder_rate = total / population * 100000)

#Depicts first five data points in ascending order by population#
murders %>% arrange(population) %>% head()

#Depicts first five data points in descending order by population#
murders %>% arrange(desc(population)) %>% head()

#Depicts the regions alphabetically then by murder rate within each region #
murders %>% arrange(region, murder_rate) % head()

#Depicts the top ten states with the highest murder rate, ordered by rate#
murders %>% arrange(desc(murder_rate)) %>% top_n(10)

#Ignoring Missing Values in Summary Statistics#
library(NHANES
data(NHANES)
NHANES %>%
  filter(Gender == "male", AgeDecade == " 40-49") %>%
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>%
  arrange(average)