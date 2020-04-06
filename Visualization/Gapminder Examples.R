#Load and Orient Oneself to Applicable Data#
library(dslabs)
data(gapminder)
head(gapminder)

#Compare Infant Mortality Rates in Sri Lanka and Turkey#
gapminder %>%
  filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

#Compare Life Expectancy and Fertility in a Scatterplot colored by continent#
ds_theme_set()
filter(gapminder, year == 1962) %>%
         ggplot(aes(fertility, life_expectancy, color = continent)) +
         geom_point()

#Faceting By Continent and Year#
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

#Faceting a Single Variable Across Multiple Years without Compression#
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
filter(gapminder, year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(. ~ year)

#Time-Series Line-Plot of U.S. Fertility# 
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

#Time-Series Line-Plot of Life Expectancy#
countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

#Histogram with Log Scaled Data#
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp / population / 365)
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

#Histogram with Log Scaled Axis#
gapminder <- gapminder %>% 
  mutate(dollars_per_day = gdp / population / 365)
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

#Boxplot Comparing the GDP of World Regions by Median Income#
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp / population / 365)
past_year <- 1970
gapminder %>%
  filter(year==past_year &!is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE)

# Define Variables for Distribution Comparison
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp / population / 365)
past_year <- 1970
present_year <- 2010
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
country_list_1 <- gapminder %>%
  filter(year==past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year==present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

#Faceting the income disparity between region and year#
gapminder %>% 
  filter(year %in% c(past_year, present_year) &!is.na(gdp))  %>%
  mutate(group = ifelse(region %in% west, "West",  "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Include only uniformly available data and compare regional income distribution#
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2") +
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

#Creating sub-groups using case_when#
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" &.$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#Creating a Stacked Density Plot#
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

#Jitter and Alpha Blending as tools for depicting congested data in Boxplots#
heights %>% ggplot(aes(sex, height)) + geom_boxplot() + geom_jitter(width = 0.1, alpha = 0.2)

#Slope Charts (Illustrate Differences in Small Data Sets)#
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"), location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")

#Bland-Altman Plot#
library(ggrepel)
dat %>%
  mutate(year = paste("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference of 2015 and 2010")