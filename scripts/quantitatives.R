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

#selecting the comorbidities variables
comorbidities_patients<-patients%>%select( 
  PLASMA, `Infusion temprana 1 tardia 0`,EPOC, `Tumor Solido`,
  `ICC/CI`, Obesidad, HTA, DIABETES,`TABAQ/EXTABAQ`,
  `Enfermedad Autoinmune`, ERC)

#defining the events
events<-c("SI")

#wrangling the columns and translating the events
is_event<-function(x){ifelse(is.na(x),x,x%in%events)}
comorbidities_patients<-comorbidities_patients%>%
  mutate_if(is.numeric,as.logical)%>%
  mutate_if(is.character,is_event)%>%
  mutate_if(is.character,as.logical)

#counting the comorbidities
comorbidities_patients<-comorbidities_patients%>%
  mutate(N_comorbilidades=
    EPOC+`Tumor Solido`+`ICC/CI`+Obesidad+HTA+DIABETES+
    `TABAQ/EXTABAQ`+`Enfermedad Autoinmune`)%>%
  mutate(N_comorbilidades=
           ifelse(is.na(ERC),N_comorbilidades,
                  N_comorbilidades+ERC))

#grouping by the comparison variable
comorbidities_patients<-comorbidities_patients%>%
  group_by(PLASMA)

#obtaining the median number of comorbidities
median_na_rm<-function(x){median(x,na.rm=T)}
median_comorbidities_patients<-comorbidities_patients%>%
  select(PLASMA,N_comorbilidades)%>%summarise_all(median_na_rm)
median_comorbidities_patients

#plotting the number of comorbidities as histogram
comorbidities_patients%>%select(N_comorbilidades,PLASMA)%>%
  ggplot(aes(N_comorbilidades,fill=PLASMA),alpha=0.5)+
  geom_histogram(binwidth = 1)

#plotting the number of comorbidities as a boxplot
comorbidities_patients%>%select(N_comorbilidades,PLASMA)%>%
  ggplot(aes(N_comorbilidades,fill=PLASMA),alpha=0.5)+
  geom_boxplot()

#conducting the Mann Whitney test
N_comorb_PLASMA_YES<-comorbidities_patients%>%filter(PLASMA==T)
N_comorb_PLASMA_YES<-N_comorb_PLASMA_YES$N_comorbilidades
N_comorb_PLASMA_NO<-comorbidities_patients%>%filter(PLASMA==F)
N_comorb_PLASMA_NO<-N_comorb_PLASMA_NO$N_comorbilidades
N_comorb_PLASMA_NO
wilcox.test(N_comorb_PLASMA_YES,N_comorb_PLASMA_NO)
