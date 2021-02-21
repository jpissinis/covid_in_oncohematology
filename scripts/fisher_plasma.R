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

#selecting qualitative variables and wrangling into booleans
qual_patients<-patients%>%
  select(PLASMA, Sexo, Quimioterapia,
         `Estado de la Enfermedad al Momento de la Infeccion por SARS-CoV2`,
         Neumonia, `Antecedente de Trasplante de CPH`, Quimioterapia,
         EPOC, Obesidad, HTA, DIABETES, UTI, ARM)

#defining the events
events<-c("SI","Mujer","En Remisión")

#wrangling the columns and translating the events
is_event<-function(x){ifelse(is.na(x),x,x%in%events)}
qual_patients<-qual_patients%>%
  mutate_if(is.numeric,as.logical)%>%
  mutate_if(is.character,is_event)%>%
  mutate_if(is.character,as.logical)

#calculating the proportions for each variable by PLASMA
mean_na_rm<-function(x){mean(x,na.rm=TRUE)}
prop_qual_patients<-qual_patients%>%
  group_by(PLASMA)%>%summarise_all(mean_na_rm)

#calculating the events for each variable by PLASMA
sum_na_rm<-function(x){sum(x,na.rm=T)}
events_qual_patients<-qual_patients%>%
  group_by(PLASMA)%>%summarise_all(sum_na_rm)

#calculating the length for each variable by PLASMA
length_na_rm<-function(x){sum(!is.na(x))}
length_qual_patients<-qual_patients%>%
  group_by(PLASMA)%>%summarise_all(length_na_rm)

#calculating the length for each variable by PLASMA
not_event<-function(x){sum(!x,na.rm=T)}
not_qual_patients<-qual_patients%>%
  group_by(PLASMA)%>%summarise_all(not_event)

#constructing the table with the events and not events by PLASMA
events_plus_not_qual<-bind_rows(
  events_qual_patients, not_qual_patients)

#building the function for fisher test to use in summerise_all
fisher_test_for_columns<-function(x){
  table2by2<-matrix(x,2,2)
  test<-fisher.test(table2by2)
  test$p.value
}

#applying fisher to every variable by PLASMA
fisher_qual_patients<-events_plus_not_qual%>%
  select(-PLASMA)%>%
  summarise_all(fisher_test_for_columns)

#exporting the results
fisher_path<-file.path("./exports","fisher_plasma.xlsx")
write_xlsx(fisher_qual_patients,fisher_path)

