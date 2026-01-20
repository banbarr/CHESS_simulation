

rm(list=ls())

library(WeightIt)
library(fixest)
library(fixest)
# library(gsynth)
# install.packages("microsynth")
library(microsynth)
library(synthdid)
library(data.table)
library(readxl)
library(janitor)
library(ggplot2)
#  get imd data
imd<-as.data.table(read_excel("./data/2019_Scores.xlsx", sheet = 1))
imd<-clean_names(imd)
las=c("Knowsley","Liverpool","Halton","Sefton","Wirral","St. Helens")
imd<-imd[local_authority_district_name_2019 %in% las]

#  simulate data as in the SDE  population of 200 households
set.seed(1235)
cohort_num<-4000
study_pop<- data.table(pseudo_uprn=1:cohort_num)
#  each household is in an lsoa
study_pop[, lsoa11:=sample(unique(imd$lsoa_code_2011), 1), by=.(pseudo_uprn)]
#  simulate number in household random between 1 - 3
study_pop[, hh_num:=sample(c(1:3), 1), by=.(pseudo_uprn)]
# each house has a EPC code - random
study_pop<-as.data.table(study_pop[rep(seq_len(.N),hh_num),])
#  each has an id
study_pop[, pseudo_nhs_num:=1:.N]

#  add imd
study_pop<-merge(study_pop, imd[, .(lsoa11=lsoa_code_2011,imd= index_of_multiple_deprivation_imd_score)], by="lsoa11")

#  each has an age, sex]
study_pop[, `:=` (age=round(rgamma(.N, shape = 10, scale = 4)), 
                  sex=rbinom(.N,size=1, prob=0.5))]

study_pop[, pr:=exp(0.009*age+0.009*imd-3)/(1+exp(0.009*age+0.009*imd-3))]
study_pop[, pr2:=exp(0.005*age-0.00005*age^2+0.005*imd-2)/(1+exp(0.005*age-0.00005*age^2+0.005*imd-2))]


# each has a probability of having a health condition depending on age and deprivation
study_pop[, `:=` (
  dep=rbinom(.N,size=1, prob=pr2),
  cvd=rbinom(.N,size=1, prob=pr))]
feglm(dep~age+age^2+imd, family = binomial(), data=study_pop)
feglm(cvd~imd+age, family = binomial(), data=study_pop)
plot(study_pop$age, study_pop$pr)
plot(study_pop$age, study_pop$pr2)

study_pop$pr<-NULL
study_pop$pr2<-NULL
study_pop$hh_num<-NULL

#  probability of experiencing outcome
#  
study_pop[, intercept:=log(runif(1)), by=.(pseudo_nhs_num)]
# study_pop[, intercept:=log(rnorm(1,50,50)/100), by=.(pseudo_nhs_num)]



study_pop$pr_treated<-NULL
dates<-seq(as.IDate('2018/01/01'), as.IDate('2026/12/31'), by="day")
numdays<-length(dates)
study_pop<-as.data.table(study_pop[rep(seq_len(.N),numdays),])
study_pop[, date:=dates, by=.(pseudo_nhs_num)]

study_pop[, age_b:=age]
study_pop[, age:=age+(as.numeric(date-as.IDate('2018/01/01'))/365)]
hist(study_pop$age)

#  each day each person has a probability of the outcome that is a function of age, imd, condition and random slope

study_pop[, random_slope:=sample((-10:10)/1000, 1), by=.(pseudo_nhs_num) ]

study_pop[, random_slope_sq:=runif(1)/100000, by=.(pseudo_nhs_num) ]

study_pop[, time:=date-as.IDate('2018/01/01')]
time_select<-unique(study_pop$time)

study_pop[, s_time:=sample(time_select, 1), by=.(pseudo_nhs_num)]
study_pop[, c_time:=time-s_time, by=.(pseudo_nhs_num)]
study_pop[, time_sq:=c_time^2]
study_pop[, pr:=
            exp(0.005*age_b+0.005*imd+0.005*cvd+0.005*dep+random_slope*c_time-random_slope_sq*time_sq+intercept)
          /(1+
              exp(0.005*age_b+0.005*imd+0.005*cvd+0.005*dep+random_slope*c_time-random_slope_sq*time_sq+intercept)
          )]


study_pop[, `:=` (
  outcome1=rbinom(.N,size=1, prob=pr),
  outcome2=rbinom(.N,size=1, prob=pr), 
  year=data.table::year(date), 
  month=data.table::month(date), 
  quarter=data.table::quarter(date))]



study_pop_ag<-study_pop[, list(outcome1=sum(outcome1), 
                               date=min(date)), by=.(intercept, pseudo_nhs_num, year, quarter, dep, cvd, imd, age_b, lsoa11, pseudo_uprn)]


study_pop_ag[order(date), time:=1:.N, by=.(pseudo_nhs_num)]


#  probability of receiving the intervention at all is function of fixed charecteristics and cumulative outcome

study_pop_ag[order(date), cum_out:=cumsum(outcome1), by=.(pseudo_nhs_num)]

study_pop_ag[order(date), roll_out:=frollmean(outcome1, 1, align = "right"), by=.(pseudo_nhs_num)]

study_pop_ag[is.na(roll_out)==T, roll_out:=outcome1, by=.(pseudo_nhs_num)]

for (ints in c("w2w","lcss", "cap", "hiw", "lr")) {

start_dates<- unique(study_pop_ag[time>8 & time<max(time-8)]$date)
study_pop_ag[, plac_date:=sample(start_dates, 1), by=.(pseudo_nhs_num)]

table(study_pop_ag$plac_date)

study_pop_ag[, pr_treated:=NULL]
study_pop_ag[, pr_treated:=0]
study_pop_ag[, first_treated:=0]


 
  
for (start in start_dates){
study_pop_ag[date==start , pr_treated:= exp(0.009*age_b+0.009*imd+0.009*cvd+0.009*dep+0.005*cum_out*0.1+0.09*outcome1-6)
          /(1+
              exp(0.009*age_b+0.009*imd+0.009*cvd+0.009*dep+0.005*cum_out*0.1+0.09*outcome1-6)
          ), by=.(plac_date)]
}
study_pop_ag[date==plac_date, first_treated:=rbinom(.N,size=1, prob=pr_treated),by=.(plac_date)]

table(study_pop_ag$first_treated, useNA = "ifany")
study_pop_ag[order(date), after:=as.numeric(cumsum(first_treated)>0),by=.(pseudo_nhs_num)]
study_pop_ag[is.na(after)==T, after:=0]
study_pop_ag[, treated:=max(after, na.rm = T), by=.(pseudo_nhs_num)]
study_pop_ag[after==1, date_treated:=min(date),by=.(pseudo_nhs_num) ]
study_pop_ag[, date_treated:=min(date_treated, na.rm = T),by=.(pseudo_nhs_num) ]
study_pop_ag[treated==0, date_treated:=0]
setnames(study_pop_ag, old=c("first_treated", "treated", "after", "date_treated"),new=paste0(ints,"_", c("first_treated", "treated", "after", "date_treated")) )
}

select<-sample(unique(study_pop_ag$pseudo_nhs_num), 1000)
ggplot(study_pop_ag[pseudo_nhs_num %in% select], aes(x=date, y=outcome1, group=pseudo_nhs_num, colour = as.factor(lcss_after)))+geom_line(linewidth = 0.05)

study_pop_ag[, outcome1:=rpois(1,round(outcome1)), by=.(pseudo_nhs_num, date) ]

ag2<-study_pop_ag[, list(outcome1=mean(outcome1)), by=.( quarter, date,  lcss_treated, lcss_after, lcss_date_treated)]

ggplot(ag2, aes(x=date, y=outcome1, group=lcss_date_treated, colour = as.factor(lcss_after)))+geom_line(linewidth = 1)+facet_wrap(lcss_date_treated~.)+geom_line(data=study_pop_ag, aes(group=pseudo_nhs_num), alpha = 0.1)

table(study_pop_ag$cap_treated, study_pop_ag$lcss_treated)


study_pop_ag<-study_pop_ag[, .(pseudo_nhs_num, year, quarter, dep,date,  cvd, imd, age_b, lsoa11, pseudo_uprn, outcome1, cum_out,time,
                               lcss_treated, cap_treated, w2w_treated, hiw_treated, lr_treated,
                               lcss_date_treated, cap_date_treated, w2w_date_treated, hiw_date_treated, lr_date_treated,
                               lcss_after, cap_after, w2w_after, hiw_after, lr_after)]
fwrite(study_pop_ag, "./data/chess_simulated_data2018_multiple_interventions_vs1.csv")


