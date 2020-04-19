library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
txt

#Extract Page 9 and split into rows
x <- str_split(txt[9], "\n")
x
class(x)

s <- x[[1]]
s <- str_trim(s)
s[1]

header_index <- str_which(s, "2015")[1]
header_index

#Divide Header Information
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]

#Identify Summation Row
tail_index <- str_which(s, "Total")[1]
tail_index

n <- str_count(s, "\\d+")
sum(n==1)

#Remove anything beyond digits and spaces and irrelevant rows
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)
s <- str_remove_all(s, "[^\\d\\s]")
s

#Convert data into a matrix with day and death count statistics
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

#Add Column Names and Convert to Numeric
colNames <- c("day", header)
tab <- s %>%
  as_data_frame() %>%
  setNames(colNames) %>%
  mutate_all(as.numeric)

#Summary Statistics
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])

#Convert to Tidy Format
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

#Plot deaths versus day with color denoting year
tab %>% filter(year < 2018) %>%
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) + 
  geom_point()