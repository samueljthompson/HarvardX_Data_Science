library(tidyverse)
library(dslabs)
library(lubridate)
data("polls_us_election_2016")

#Inspect Start Date Column
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head #Days from Epoch

#GGplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

#Extract Month, Day, and Year from random dates
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates
data.frame(date = dates,
           month = month(dates),
           day = day(dates),
           year = year(dates))
month(dates, label = TRUE) #Extract Month Label

#YMD works on mixed data styles
x <- c(200090101, "2009-01-02", "2009 01 03", "2009-1-4", "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

now() #Current time in your time zone
now("GMT") #Current time in GMT
now() %>% hour()
now() %>% minute()
now() %>% second()

#Parse Time
x <- c("12:34:56")
hms(x)

#Parse Date and Time
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

#Assessment
options(digits = 3)
data("brexit_polls")
head(brexit_polls)
#Count the Polls Occuring in April
nrow(brexit_polls %>%
  filter(as.numeric(month(startdate))==4))
#Count the Polls ending the week of 2016-06-12
brexit_polls %>%
  mutate(revised_enddate = round_date(brexit_polls$enddate, "week", week_start = 7)) %>%
  filter(revised_enddate == "2016-06-12")
#Determine the Mode Day of the Week for Polling
brexit_polls %>%
  mutate(day = weekdays(enddate)) %>%
  group_by(day) %>%
  summarize(n = n())
#Frequency of Movie Reviews
data(movielens)
movielens %>%
  mutate(date_time = as_datetime(timestamp)) %>%
  group_by(year(date_time)) %>%
  summarize(n = n())
movielens %>%
  mutate(date_time = as_datetime(timestamp)) %>%
  group_by(hour(date_time)) %>%
  summarize(n = n()) %>%
  print(n = 24)