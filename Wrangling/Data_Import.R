getwd()

#Set a path to a raw data file and list the files contained therein
path <- system.file("extdata", package = "dslabs")
list.files(path)

#Generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

#Copy a file from dslabs to the working directory
file.copy(fullpath, getwd())
file.exists(filename) #Confirm File

library(dslabs)
library(tidyverse)
library(readxl)

read_lines("murders.csv", n_max = 3)

dat <- read_csv(filename)
dat <- read_csv(fullpath)
head(dat)

path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

#Example Pathway Definition
filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat = read.csv(file.path(path, filename))
dat1 = read.csv(file.path(path, filename1))
dat2 = read.csv(file.path(path, filename2))

#Download files from the internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)

download.file(url, "murders.csv")
read_csv("murders.csv")

tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(temp_filename)
file.remove(tmp_filename)

url_2 <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_csv(url_2, col_names = FALSE)
