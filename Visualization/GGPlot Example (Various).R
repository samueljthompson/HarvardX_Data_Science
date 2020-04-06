#Load Relevant Data#
library(tidyverse)
library(dslabs)
data(heights)

#Define P#
p <- heights %>%
 ggplot(aes(x = height, group = sex))

#Construct Histogram#
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Heights in Inches") +
  ggtitle("Histogram")

#Construct Smooth Density Plot#
p <- heights %>%
 ggplot(aes(x = height, color = sex)) +
 geom_density(alpha = 0.2)

#Construct Quantile-Quantile Plot (It is necessary to use "sample" instead of "x" in the aes argument)#
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

#QQ-Plot against a Normal Distribution with same mean and sd as data (otherwise qqplot defaults to mean = 0; sd = 1)#
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

#Illustrate Different Plots Arrayed Next to One Another#
#Define the Plots#
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

#Arrange the Plots#
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
