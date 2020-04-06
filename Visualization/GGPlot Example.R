#Load Relevant Libraries#
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(dslabs)
data(murders)

#Define the Intercept#
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

#Construct the Plot#
murders %>% 
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in Millions (Log Scale)") +
  ylab("Total Number of Murders (Log Scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()