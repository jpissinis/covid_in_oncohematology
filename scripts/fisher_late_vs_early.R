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

#selecting qualitative variables and wrangling into booleans
qual_patients<-patients%>%
  select(PLASMA, `Infusion temprana 1 tardia 0`, Sexo, Quimioterapia,
         `Estado de la Enfermedad al Momento de la Infeccion por SARS-CoV2`,
         Neumonia, `Antecedente de Trasplante de CPH`, 
         Quimioterapia, EPOC, Obesidad, HTA, DIABETES, UTI, ARM)


#defining the events
events<-c("SI","Mujer","En Remisión")

#wrangling the columns and translating the events
is_event<-function(x){ifelse(is.na(x),x,x%in%events)}
qual_patients<-qual_patients%>%
  mutate_if(is.numeric,as.logical)%>%
  mutate_if(is.character,is_event)%>%
  mutate_if(is.character,as.logical)

#filtering the patients that received `Infusion temprana 1 tardia 0`
qual_patients<-qual_patients%>%
  filter(PLASMA==T)

#grouping by the comparison variable
qual_patients<-qual_patients%>%
  select(-PLASMA)%>%
  group_by(`Infusion temprana 1 tardia 0`)

#calculating the proportions for each variable by `Infusion temprana 1 tardia 0`
mean_na_rm<-function(x){mean(x,na.rm=T)}
prop_qual_patients<-qual_patients%>%
  summarise_all(mean_na_rm)

#calculating the events for each variable by `Infusion temprana 1 tardia 0`
sum_na_rm<-function(x){sum(x,na.rm=T)}
events_qual_patients<-qual_patients%>%
  summarise_all(sum_na_rm)

#calculating the length for each variable by `Infusion temprana 1 tardia 0`
length_na_rm<-function(x){sum(!is.na(x))}
length_qual_patients<-qual_patients%>%
  summarise_all(length_na_rm)

#calculating the length for each variable by `Infusion temprana 1 tardia 0`
not_event<-function(x){sum(!x,na.rm=T)}
not_qual_patients<-qual_patients%>%
  summarise_all(not_event)

#constructing the table with the events and not events by `Infusion temprana 1 tardia 0`
events_plus_not_qual<-bind_rows(
  events_qual_patients, not_qual_patients)


#building the function for fisher test to use in summerise_all
fisher_test_for_columns_p<-function(x){
  table2by2<-matrix(x,2,2)
  test<-fisher.test(table2by2)
  test$p.value
}

#building the function for fisher test to use in summerise_all
odds_ratio_for_columns<-function(x){
  table2by2<-matrix(x,2,2)
  test<-fisher.test(table2by2)
  test$estimate
  }

#obtaining the p value of the fisher test for every variable by `Infusion temprana 1 tardia 0`
fisher_late_vs_early<-events_plus_not_qual%>%
  select(-`Infusion temprana 1 tardia 0`)%>%
  summarise_all(fisher_test_for_columns_p)

#obtaining the odds ratio for every variable by `Infusion temprana 1 tardia 0`
odds_late_vs_early<-events_plus_not_qual%>%
  select(-`Infusion temprana 1 tardia 0`)%>%
  summarise_all(odds_ratio_for_columns)

#binding the odd ratios and the p values
final_late_vs_early<-bind_rows(
  odds_late_vs_early,fisher_late_vs_early)

#adding the row names
row_names<-data.frame(late_vs_early)
final_late_vs_early<-final_late_vs_early%>%
  mutate(late_vs_early=c("OR","p"))

#exporting the results
fisher_path<-file.path("./exports","fisher_late_vs_early.xlsx")
write_xlsx(final_late_vs_early,fisher_path)