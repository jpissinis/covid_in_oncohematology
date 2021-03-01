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

#selecting the relevant variables for the analysis
quant_patients<-patients%>%select( 
  PLASMA, `Infusion temprana 1 tardia 0`,
  `Edad al momento de la Infeccion por SARS-CoV2`, COMORBILIDADES,
  EPOC, `Tumor Solido`, `ICC/CI`, Obesidad, HTA, DIABETES,
  `TABAQ/EXTABAQ`, `Enfermedad Autoinmune`, ERC)

#defining the events
events<-c("SI")

#wrangling the columns and translating the events
is_event<-function(x){ifelse(is.na(x),x,x%in%events)}
quant_patients<-quant_patients%>%
  mutate_if(is.numeric,as.logical)%>%
  mutate_if(is.character,is_event)%>%
  mutate_if(is.character,as.logical)

#counting the comorbidities
quant_patients<-quant_patients%>%
  mutate(N_comorbilidades=
    EPOC+`Tumor Solido`+`ICC/CI`+Obesidad+HTA+DIABETES+
    `TABAQ/EXTABAQ`+`Enfermedad Autoinmune`)%>%
  mutate(N_comorbilidades=
           ifelse(is.na(ERC),N_comorbilidades,
                  N_comorbilidades+ERC))

#obtaining the median
median_quant_patients<-quant_patients%>%summarise_all(median)

#plotting the number of comorbidities
quant_patients%>%select(N_comorbilidades,PLASMA)%>%
  ggplot(aes(N_comorbilidades,fill=PLASMA,alpha=0.5))+
  geom_density()

