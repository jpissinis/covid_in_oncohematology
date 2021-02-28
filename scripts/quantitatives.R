#loading packages
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(writexl)


#loading the data as patients
path_name<-file.path("./raw","patients.csv")
patients<-read_csv(path_name)
str(patients)

quant_patients<-patients%>%select( 
  `Edad al momento de la Infeccion por SARS-CoV2`, COMORBILIDADES)

#obtaining the median
median_quant_patients<-quant_patients%>%summarise_all(median)
median_quant_patients

mean(quant_patients$COMORBILIDADES)
