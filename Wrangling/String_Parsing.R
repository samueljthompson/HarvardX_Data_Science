library(tidyverse)
library(readxl)
library(rvest)

#Case Study 1 - Murders Data
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

murders_raw$population[1:3]
as.numeric(murders$population[1:3]) #Unable due to commas in data

#Detect Presence of Commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
#Replace Commas with empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
#Parse_Number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murder_new <- murders_raw %>% mutate_at(2:3, parse_number)
murder_new %>% head

#Case Study 2 - Reported Heights
library(dslabs)
data(reported_heights)
class(reported_heights$height) #Character as opposed to Numeric

x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x)) #Illustrate the extent of formatting discrepancies producing NAs

#Keep only entries that result in NAs in order to isolate patterns
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=10)
#Calculate cutoffs that cover 99.999% of the human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)
#Keep only entries that result in NAs or are outside plausible range
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
#Number of Problematic Entries
problems <- reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)

#10 Examples of x'y"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
#10 Examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
#10 Examples of cm rather than in
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81))
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#Regular Expressions (Regex)
#Show the subset including commas
pattern <- ","
str_detect(murders_raw$total, pattern)
#Show the Subset Including "cm"
str_subset(reported_heights$height, "cm")
#Use | inside a Regex
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")
#Highlight the First Occurance of a Pattern
str_view(s, pattern)
#Highlight all instances of a Pattern
str_view_all(s, pattern)

#Number of Entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
#Inspect Examples of Entries with Problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")
#Replace feed/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>%
  str_replace("feet|ft\foot", "'") %>% #replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% #remove all inches symbols
  str_detect(pattern) %>%
  sum()
#Update pattern by adding optional spaces before ad after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft\foot", "'") %>% #replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% #remove all inches symbols
  str_detect(pattern) %>%
  sum()

#Define Regex with groups
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

#Testing and Improving
#Function to test problematic entries
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
#Identify entries with problems
problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>%
  str_replace("feet|foot|ft", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_replace("^([4,7])\\s*[,\\.\\s+](\\d*)$", "\\1'\\2")

#Find Proportion that fit pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index] #Remaining Problems

#University Formatting Example
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "U California", "California State University")
schools %>%
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>%
  str_replace("^University of |^University ", "University of ")

#Putting it into a Function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% 
    str_replace_all("inches|in|''|\"|cm|and","") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>%
    str_replace("^([4-7])'?$", "\\1'0") %>% 
    str_replace("^([4-7])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
    str_trim()
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#Putting it all Together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
  mutate(original = height,
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height,
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54,
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54,
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess,
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>%
  arrange(height) %>%
  View()
    
