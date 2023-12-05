#Clear
cat("\014")  
rm(list=ls())

#Set Directory
rm(list=ls())
setwd("C:/Users/usuario/Downloads/EC349")

#Load Libraries
install.packages(c("tidytext", "dplyr", "tm", "stringr"))
library(tidytext)
library(dplyr)
library(tm)
library(stringr)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)
library(jsonlite)
library(tidyr)
library(caret)
library(purrr)
library(tidytext)
library(glue)
