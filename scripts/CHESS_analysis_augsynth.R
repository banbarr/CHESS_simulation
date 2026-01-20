

rm(list=ls())
library(did2s)
library(did)

library(WeightIt)
library(fixest)
# library(gsynth)
# install.packages("microsynth")
library(microsynth)
library(synthdid)



library(data.table)
library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)
library(boot)
library(augsynth)

study_pop_ag<-fread("./data/chess_simulated_data2018.csv")
# ############ analysis 

#  make ids characters
# study_pop_ag[, pseudo_nhs_num:=paste0("nhs_", pseudo_nhs_num)]
# study_pop_ag[, pseudo_uprn:=paste0("uprn_", pseudo_uprn)]
# study_pop_ag[date>=date_treated & treated==1, gname:=min(time), by=.(pseudo_nhs_num)]
# with(study_pop_ag, table(time, gname))
# study_pop_ag[ , gname:=min(gname, na.rm = T),by=.(pseudo_nhs_num)]
# study_pop_ag[is.na(gname)==T, gname:=0]
# with(study_pop_ag, table(time, gname))
 # study_pop_ag[, outcome:=outcome1]

study_pop_ag[, did:=as.numeric(date>date_treated & treated==1)]

study_pop_ag[date>date_treated, num_post:=.N, by=.( pseudo_nhs_num)]
study_pop_ag[date<date_treated, num_pre:=.N, by=.( pseudo_nhs_num)]

table(study_pop_ag$num_post)
table(study_pop_ag$num_pre)
ppool_syn_time <- multisynth(outcome1 ~ did, pseudo_nhs_num, time,
                             study_pop_ag, n_leads = 5, n_lags = 5)

dt1<-dt[, list(samhi_index=mean(samhi_index, na.rm=T), did=max(did, na.rm = T), tyear=min(tyear, na.rm = T), treated=max(treated)), by=.(oslauanm, year)]
dt1[, relyear:=year-tyear]
table(dt1$relyear, useNA = "ifany")
dt1<-dt1[is.na(treated)==F]
dt1[oslauanm=="",oslauanm:="City of London" ]
dt1[, maxfu:=max(relyear, na.rm = T), by=.(oslauanm)]
table(dt1$maxfu)
dt1<-dt1[maxfu>3|treated==0]


ppool_syn_summ<-summary(ppool_syn_time)
summary(ppool_syn_time)
plot(ppool_syn_summ)

