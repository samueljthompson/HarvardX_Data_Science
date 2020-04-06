library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#Equivalent Syntax for determining Last Year of Reported Carbon Emissions#
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
 .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

#Time Series Plot Modeling Temperature Anomaly#
p <- temp_carbon %>%
  ggplot() +
  geom_line(aes(year, temp_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  geom_line(aes(year, land_anomaly), col = "green") +
  geom_hline(aes(yintercept = 0), col = "blue") +
  xlim(1850, 2018) +
  ylab("Temperature Anomaly (degrees C)") +
  ggtitle("Temperature Anomaly Relative to 20th Century Mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th Century Mean"), col = "blue")
p

#Plot of Greenhouse Gas Concentrations#
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850, col = "blue") +
  ylab("Concentration (ch4/n2o, ppb, co2 ppm)") +
  ggtitle("Atmospheric Greenhouse Gas Concentration by Year, 0-2000")

#Time Series Line Plot of Carbon Emissions#
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

#Historic CO2 Trends#
data("historic_co2")
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, group = source)) +
  geom_line(aes(color = source))
co2_time
co2_time + xlim(-3000, 2018)