#loading packages
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

#loading the data as patients
path_name<-file.path("./raw","patients.csv")
patients<-read_csv(path_name)
view(patients)
