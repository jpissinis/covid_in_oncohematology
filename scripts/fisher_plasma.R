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
  select(PLASMA, Sexo,`Estado de la Enfermedad al Momento de la Infeccion por SARS-CoV2`,
         Neumonia, `Antecedente de Trasplante de CPH`,
         `TIPO TRASPLANTE EST AUTOLOGO 1 ALOGENICO 2`,
         Quimioterapia, EPOC, Obesidad, HTA, DIABETES, 
         UTI, ARM, Evolucion)

#defining the events
events<-c("SI","Mujer","En Remisión","Fallecido")

#wrangling the columns and translating the events
is_event<-function(x){ifelse(is.na(x),x,x%in%events)}
qual_patients<-qual_patients%>%
  mutate(`TIPO TRASPLANTE EST AUTOLOGO 1 ALOGENICO 2`=
           `TIPO TRASPLANTE EST AUTOLOGO 1 ALOGENICO 2`-1)%>%
  mutate_if(is.numeric,as.logical)%>%
  mutate_if(is.character,is_event)%>%
  mutate_if(is.character,as.logical)

#grouping by the comparison variable
qual_patients<-qual_patients%>%
  group_by(PLASMA)

#calculating the proportions for each variable by PLASMA
mean_na_rm<-function(x){mean(x,na.rm=T)}
prop_qual_patients<-qual_patients%>%
  summarise_all(mean_na_rm)

#calculating the events for each variable by PLASMA
sum_na_rm<-function(x){sum(x,na.rm=T)}
events_qual_patients<-qual_patients%>%
  summarise_all(sum_na_rm)

#calculating the length for each variable by PLASMA
length_na_rm<-function(x){sum(!is.na(x))}
length_qual_patients<-qual_patients%>%
  summarise_all(length_na_rm)

#calculating the length for each variable by PLASMA
not_event<-function(x){sum(!x,na.rm=T)}
not_qual_patients<-qual_patients%>%
  summarise_all(not_event)

#constructing the table with the events and not events by PLASMA
events_plus_not_qual<-bind_rows(
  events_qual_patients, not_qual_patients)

#building the function for the odds ratio to use in summerise_all
odds_ratio_for_columns<-function(x){
  table2by2<-matrix(x,2,2)
  test<-fisher.test(table2by2)
  test$estimate
}

#obtaining the p value of the fisher test for every variable by PLASMA
fisher_PLASMA<-events_plus_not_qual%>%
  select(-PLASMA)%>%
  summarise_all(fisher_test_for_columns_p)

#obtaining the odds ratio for every variable by PLASMA
odds_PLASMA<-events_plus_not_qual%>%
  select(-PLASMA)%>%
  summarise_all(odds_ratio_for_columns)

#binding the odd ratios and the p values
final_PLASMA<-bind_rows(
  odds_PLASMA,fisher_PLASMA)

#adding the row names
final_PLASMA<-final_PLASMA%>%
  mutate(late_vs_early=c("OR","p"))
view(final_PLASMA)

#exporting the results
fisher_path<-file.path("./exports","fisher_PLASMA.xlsx")
write_xlsx(final_PLASMA,fisher_path)

