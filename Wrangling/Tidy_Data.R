library(tidyverse)
library(dslabs)
data(gapminder)

#Create and Inspect a Tidy Data Frame
tidy_data <- gapminder %>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#Import and Inspect example of Gapminder Data in Wide Format
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, '1960':'1967')

#Gather Wide Data to make Tidy Data
new_tidy_data <- wide_data %>%
  gather(year, fertility, '1960':'2015', -country, convert=TRUE)
head(new_tidy_data)

new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#Spread Tidy Data to generate Wide Data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, '1960':'1967')

#Prepare Data for Separate Example
filename_separate <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename_separate)
select(raw_dat, 1:5)

dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

#Split on All Underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right")
#Split on First Underscore but keep Life_Expectancy Merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

#Full Code for Tidying Data
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep = "_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

#Assessment Part 1
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>%
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide, month, co2, -year)
head(co2_tidy)

co2_tidy %>%
  ggplot(aes(as.numeric(month), co2, color = year)) +
  geom_line()

#Assessment Part 2
data(admissions)
dat <- admissions %>% select(-applicants)

dat_tidy <- spread(dat, gender, admitted)
dat_tidy

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2
tmp2_wide <- spread(tmp2, column_name, value)
tmp2_wide