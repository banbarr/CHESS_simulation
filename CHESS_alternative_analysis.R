

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

study_pop_ag<-fread("./chess_simulated_data.csv")
# ############ analysis 

#  make ids characters
study_pop_ag[, pseudo_nhs_num:=paste0("nhs_", pseudo_nhs_num)]
study_pop_ag[, pseudo_uprn:=paste0("uprn_", pseudo_uprn)]
study_pop_ag[date>=date_treated & treated==1, gname:=min(time), by=.(pseudo_nhs_num)]
with(study_pop_ag, table(time, gname))
study_pop_ag[ , gname:=min(gname, na.rm = T),by=.(pseudo_nhs_num)]
study_pop_ag[is.na(gname)==T, gname:=0]
with(study_pop_ag, table(time, gname))
study_pop_ag[, outcome:=outcome1]

lsdts<-list()
for (i in unique((study_pop_ag[is.na(date_treated)==F]$date_treated))) {
   # i<-unique((study_pop_ag[is.na(date_treated)==F]$date_treated))[4]
   dt2 <- study_pop_ag[(date_treated==i|treated==0),  .(pseudo_nhs_num,pseudo_uprn,  date, outcome=outcome1, date_treated, treated)]
   dt2<-dt2[is.na(outcome)==F]
   dt2<-dt2[is.infinite(outcome)==F]
   dt2[, did:=as.numeric(date>=i & treated==1)]
   dt2[, numdate:=.N, by=.(pseudo_nhs_num)]
   #  force balanced panel
   dt2<-dt2[numdate==max(dt2$numdate)]
   setup = panel.matrices(dt2, unit = 'pseudo_nhs_num', time = 'date', 
                          outcome = 'outcome', treatment = 'did')
   estimate = synthdid_estimate(setup$Y, setup$N0, setup$T0)
   controls = as.data.table(synthdid_controls(estimate, mass = 1), keep.rownames=T)
   controls_l = as.data.table(synthdid_controls(estimate, mass = 1, weight.type = "lambda") , keep.rownames=T)
   dt2<-merge(dt2, controls[, .(rn, wt=`estimate 1`)], by.x="pseudo_nhs_num", by.y = "rn", all.x=T)
   controls_l[, rn:=as.IDate(rn)]
   dt2<-merge(dt2, controls_l[, .(rn=as.numeric(rn), lwt=`estimate 1`)], by.x="date", by.y = "rn", all.x=T)
   dt2[, after:=as.numeric(date>=i)]
   dt2[treated==1, wt:=1]
   dt2[ is.na(wt)==T, wt:=0]
   dt2[after==1, lwt:=1]
   dt2[ is.na(lwt)==T, lwt:=0]
   dt2<-dt2[date>=min(dt2[treated==1]$date)]
   dt2[, date_treated_dt:=i]
   dt2[, tau_wt:=sum(did), by=.(date_treated)]
   ag_data<-dt2[, list(outcome=weighted.mean(outcome, w=wt), num=.N), by=.(date, treated, after, did, lwt, date_treated_dt, tau_wt)]
   lsdts[[i]]<-ag_data
}
pooled_data<-rbindlist(lsdts)
# 
# pooled_data<-dt2

lm(outcome~after*treated, data = pooled_data)
pooled_data[, estimate:=coef(lm(outcome~after*treated, weights = lwt))[4], by=.(date_treated_dt)]
est_table<-unique(pooled_data[treated==1, .(date_treated_dt, estimate, tau_wt)])
est_table[order(date_treated_dt), overall:=weighted.mean(estimate, tau_wt)]

#  just shows below ssynth_estimate gives identical results (and can use for CIs)
# if (out1=="samhi_index"){
# dt3<-dt[!(date_treated==2012 & treated==1) & is.na(get(out1))==F]
# }
# if (out1!="samhi_index"){
#   dt3<-dt[is.na(get(out1))==F]
# }
# estimate2 <- ssynth_estimate(dt3, unit = "pseudo_nhs_num", time = "date", treated = "did", outcome = out1)
# est_table[, sssynthdid_est:=estimate2$att_table$tau]
# est_table[, sssynthdid_tau_wt:=estimate2$att_table$tau_wt]
# est_table[, sssynthdid_overall:=estimate2$att_estimate]


plot1<-ggplot(data=pooled_data, aes(x=date, y=outcome, group = as.factor(treated), color=as.factor(treated)))+geom_line()+geom_vline(aes(xintercept =date_treated_dt))+facet_wrap(date_treated_dt~.)+theme_minimal() +theme(strip.text.y = element_text(angle = 0, size=7))+scale_color_brewer("",  type = "qual", palette = 2, labels=c("Control", "Intervention"))
plot1


study_pop_ag[, did:=as.numeric(date>=date_treated & treated==1)]
study_pop_ag[, pid:=as.numeric(as.factor(pseudo_nhs_num))]


study_pop_ag[is.na(gname)==T, gname:=0]

multiple_ests = did2s::event_study(
   data = as.data.frame(study_pop_ag),
   gname = "gname",
   idname = "pid",
   tname = "time",
   yname = "outcome1",
   estimator = "all",
   xformla =  ~ age_2022+imd+cvd+cum_out
)

plot<-did2s::plot_event_study(multiple_ests)
plot
sum(study_pop_ag$gname)

study_pop_ag[, dup:=.N, by=.(pid, tname)]
table(study_pop_ag$dup)
table(study_pop_ag$tname)
att_byyear <- att_gt(yname = "outcome1",
                     tname = "time",
                     gname = "gname",
                     idname = "pid",
                     panel = TRUE,
                     bstrap =F, 
                     xformla =  ~ age_2022+imd+cvd, 
                     data = study_pop_ag)

att_byyear 
aggte(att_byyear, type = "simple" )


att_dyn<-  aggte(att_byyear, type = "dynamic" )
att_dyn$



graph_data <- tibble(est = att_dyn$att,se = att_dyn$se, time=-16:16)

graph_data <- graph_data %>%
   mutate(upp = est + 1.96*se,
          low = est - 1.96*se)
graph_data<-as.data.table(graph_data)
graph_data[, inter:=as.numeric(time>=0)]

plot1<-ggplot(data=graph_data, aes(time, est, colour = as.factor(inter))) +
   geom_point() +
   geom_errorbar(aes(ymin = low, ymax = upp), width=0.3,
                 linewidth=0.5) +
   theme_minimal() +labs( y = "Outcome", x = "Time from intervention", title = " Difference between intervention and control") +
   scale_colour_manual(  values= c("darkslategray4","deeppink4"), labels=c("Before", "After")) +
   geom_hline(yintercept = 0, linetype = 2) +
   theme(text = element_text(size = 16), legend.title = element_blank())

plot1

rs1 <- as.data.table(tibble( result="Edoxaban", est =  round(m1$ATT*100,1),lcl=round(m1$lci*100,1),ucl=round(m1$uci*100,1)))


#  Trying entropy balancing - split by cohort then agregate across cohorts
lsdts<-list()
for (i in unique((study_pop_ag[is.na(date_treated)==F]$date_treated))) {
    # i<-unique((study_pop_ag[is.na(date_treated)==F]$date_treated))[4]
   dt2 <- study_pop_ag[(date_treated==i|treated==0),  .(pseudo_nhs_num,pseudo_uprn, cvd, dep,age_2022, imd,  date, cum_out, outcome, date_treated, treated, time)]
   dt2<-dt2[is.na(outcome)==F]
   dt2<-dt2[is.infinite(outcome)==F]
   
   temp<-dcast(dt2[date<=i ], pseudo_nhs_num+
                   cvd+dep+age_2022+imd+
                   treated~time, value.var = c("outcome1"))
   temp<-clean_names(temp)
   form1<-as.formula(paste0("treated~", paste0(names(temp[, -c("pseudo_nhs_num","treated")]), collapse = "+")))
   temp<-na.omit(temp)

   dt2<-merge( dt2, 
               temp[, wt:=weightit(form1,temp, estimand = "ATT", method = "ebal")$weights]
               [, .(pseudo_nhs_num,wt)]
               , by="pseudo_nhs_num", all.x=T)
   
   dt2[, did:=as.numeric(date>=i & treated==1)]
   dt2[, after:=as.numeric(date>=i )]
   dt2[, date_treated_dt:=as.IDate(i)]
   dt2[, tau_wt:=sum(did), by=.(date_treated)]
   ag_data<-dt2[, list(outcome=weighted.mean(outcome, w=wt), num=.N), by=.(date, treated, after, did, date_treated_dt,  tau_wt)]
   lsdts[[i]]<-ag_data
}
pooled_data<-rbindlist(lsdts)

pooled_data[, estimate:=coef(lm(outcome~after*treated))[4], by=.(date_treated_dt)]
est_table<-unique(pooled_data[treated==1, .(date_treated_dt, estimate, tau_wt)])
est_table[order(date_treated_dt), overall:=weighted.mean(estimate, tau_wt)]


plot1<-ggplot(data=pooled_data, aes(x=date, y=outcome, group = as.factor(treated), color=as.factor(treated)))+geom_line()+geom_vline(aes(xintercept =date_treated_dt))+facet_wrap(date_treated_dt~.)+theme_minimal() +theme(strip.text.y = element_text(angle = 0, size=7))+scale_color_brewer("",  type = "qual", palette = 2, labels=c("Control", "Intervention"))
plot1



start.pre<-min(temp$time)
end.pre<-max(temp[after==0]$time)
end.post<-max(temp$time)
match.out<-c("outcome1", "cumout")
cov.var<-c("cvd", "dep", "age_2022", "imd")
sea2 <- microsynth(as.data.frame(temp), 
                   idvar="pseudo_nhs_num", timevar="time", intvar="treated", 
                   start.pre= start.pre, end.pre= end.pre, end.post= end.post, 
                   match.out=match.out, match.covar=cov.var, 
                   result.var="outcome1", 
                   omnibus.var=F,
                   test="twosided",
                   # use.backup = TRUE,
                   confidence = 0.95,
                   # perm=100, 
                   jack=F, 
                   n.cores = 3)

sea2
par(mar=c(1,1,1,1))
plot_microsynth(sea2)


ag2<-temp[, list(outcome1=weighted.mean(outcome1, w=wt),
                 outcome_ssid=weighted.mean(outcome1, w=ssdid_wt, na.rm=T) 
                 ), by=.( quarter, date,  treated, after, date_treated, ssdid_lwt, did)]



feols(outcome_ssid~treated*after, data=ag2, weights = ag2$ssdid_lwt)
estimate
feols(outcome1~treated*after, data=ag2)

ggplot(ag2, aes(x=date, y=outcome1, group=treated, colour = as.factor(after)))+geom_line(linewidth = 1)

ggplot(ag2, aes(x=date, y=outcome_ssid, group=treated, colour = as.factor(after)))+geom_line(linewidth = 1)

plot(estimate)
###Economic Evaluation  starts from here. Only the path may need to be changed

 EE <- read.csv("chess_simulated_data.csv")
 
 library(data.table)
 
 setDT(EE)   
 
 
 # Baseline utility is computed based on age, having any morbidity at the begining (cvd and Dep) and imd because these factors likely affect the quality of life  
 # These coefficients used can be adjusted however I derived these numbers for dep and cvd from previous study (from Ara & Brazier 2011).
 EE[, b_utility := 0.92 -
      0.0005 * age_2022 -     
      0.13 * dep -            
      0.10 * cvd -            
      0.005 * imd]           
 
 # Extra decrement if someone is having both
 EE[dep == 1 & cvd == 1, b_utility := b_utility - 0.01]
 #Optional 
 # Cap utility values to valid range [0, 1]
 EE[b_utility < 0, b_utility := 0]
 EE[b_utility > 1, b_utility := 1]
 
 #Temporary utility decrements likely due to emergency admission  based on the below study but this can be treated optional
 #Health utility after emergency medical admission is poor compared to population norms (Goodacre, Steve W., et al 2012)
 
 # Decrease utility by 0.05 for any quarter with ≥1 emergency  (0.05 maybe too high)
 EE[, utility := b_utility]
 EE[outcome1 > 0, utility := pmax(0, utility - 0.05)]
 
 # Compute Quarterly QALYs
 #QALYs per quarter based on my understanding that data is quarterly (I  may be wrong here)
 
 EE[, qrt_qaly := utility * 0.25]
 
 # compute Emergency Costs (adjustable for inflation)
 
 emerg_costs <- data.table(
   date = c(2022, 2023, 2024, 2025, 2026),
   cost_perevent = c(220, 280, 370, 420, 430)
 )
 EE <- merge(EE, emerg_costs, by = "date", all.x = TRUE, allow.cartesian = FALSE)
 #Per-quarter emergency cost
 EE[, cost_emerg := outcome1 * cost_perevent]
 
 # Compute Intervention Costs
 #  Adjust t_costs as per  total annual intervention budgets
 int_costs <- data.table(
   date = c(2022, 2023, 2024, 2025, 2026),
   t_costs = c(100000, 200000, 150000, 120000, 80000)
 )
 #############################################################
 # Adjusting inflation (Optional)
 #inflation_factors <- data.table( date = c(2022, 2023, 2024, 2025, 2026),
 #multiplier = c(1.10, 1.06, 1.03, 1.00, 1.01)  )
 # Merge inflation by date
 #EE <- merge(EE, inflation_factors, by = "date", all.x = TRUE)
 # Adjust both emergency and intervention costs
 #EE[, int_costsadj := int_costs * multiplier]
 #EE[, int_costsadj := int_costs * multiplier]
 #EE[, multiplier := NULL]
 ###################################################################################
 
 #  how many people were treated each date and then merge
 treated_counts <- unique(
   EE[!is.na(date_treated), .(pseudo_nhs_num, date_treated = date(date_treated))]
 )[, .N, by = date_treated]
 
 int_costs <- merge(int_costs, treated_counts,
                    by.x = "date", by.y = "date_treated", all.x = TRUE)
 
 # Per-person intervention cost for each date
 int_costs[, pp_cost := ifelse(!is.na(N) & N > 0, t_costs / N, 0)]
 
 # Intervention Costs to Patients
 EE[, Int_costpat := 0]
 EE[, date_treated := date(date_treated)]
 
 EE <- merge(EE,
             int_costs[, .(date, pp_cost)],
             by.x = "date_treated", by.y = "date",
             all.x = TRUE, allow.cartesian = FALSE)
 
 # Assign per-person cost in the treatment-start quarter
 EE[!is.na(date_treated) & date == date_treated, Int_costpat := pp_cost]
 EE[, pp_cost := NULL]
 
 #Person level summary
 EE[, treated_flag := max(treated, na.rm = TRUE), by = pseudo_nhs_num]
 
 summary_cca <- EE[, .(
   tot_qalys = sum(qrt_qaly, na.rm = TRUE),
   totemg = sum(outcome1, na.rm = TRUE),
   totemg_cost = sum(cost_emerg, na.rm = TRUE),
   tot_int_cost = sum(Int_costpat, na.rm = TRUE),
   tot_cost = sum(cost_emerg + Int_costpat, na.rm = TRUE),
   treated = max(treated_flag)
 ), by = pseudo_nhs_num]
 
 #Final Cost Consequences Analysis 
 cca_table <- summary_cca[, .(
   Avg_qalys = mean(tot_qalys, na.rm = TRUE),
   Avg_emgadms = mean(totemg, na.rm = TRUE),
   Avg_emgcost = mean(totemg_cost, na.rm = TRUE),
   Avg_intcost = mean(tot_int_cost, na.rm = TRUE),
   Avg_totcost = mean(tot_cost, na.rm = TRUE)
 ), by = treated]
 
 #Final Table (Not necessarily making any sense)
 print(cca_table)
 ##Other checks 
 # Cost effectiveness analysis 
 cea_summary <- cca_table[, .(
   diff_cost = diff(Avg_totcost),   #difference in average cost
   diff_qaly = diff(Avg_qalys),    #difference in mean QALYs 
   ICER = diff(Avg_totcost) / diff(Avg_qalys)   #Incremental Cost-Effectiveness Ratio
 )]
 
 cea_summary
 
 
 # cost benefit analysis
 valpqaly <- 22000  # Pound per QALY (NICE threshold)
 
 cba_sum <- cca_table[, .(
   treated,
   Avg_totcost,
   Avg_qalys,
   m_benefit = Avg_qalys * valpqaly,                 # monetary benefit
   net_mbenefit = Avg_qalys * valpqaly - Avg_totcost # net monetary benefit
 )]
 
 incremental_cba <- cba_sum[, .(
   diff_cost = diff(Avg_totcost),
   diff_benefit = diff(m_benefit),
   diff_net_benefit = diff(net_mbenefit)
 )]
 
 ##Results in Excel
 if (!require("writexl")) install.packages("writexl")
 library(writexl)
 write_xlsx(
   list(
     CCA = cca_table,
     CEA = cea_summary,
     CBA_sum = cba_sum,
     CBA_Inc = incremental_cba
   ),
   path = "economic_evaluation.xlsx"
 )

####END####
