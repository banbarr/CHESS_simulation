

rm(list=ls())

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

study_pop_ag<-fread("./chess_simulated_data.csv")
# ############ analysis 
start_dates<- unique(study_pop_ag[treated==1]$date_treated)
st_date<-start_dates[4]
temp<-study_pop_ag[date_treated==st_date|treated==0]
temp[order(date), time:=1:.N, by=.(pseudo_nhs_num)]
temp2<-dcast(temp[date<=st_date ], pseudo_nhs_num+
               cvd+dep+age_2022+imd+
               treated~time, value.var = c("cum_out"))
temp2<-clean_names(temp2)
form1<-as.formula(paste0("treated~", paste0(names(temp2[, -c("pseudo_nhs_num","treated")]), collapse = "+")))
temp2<-na.omit(temp2)
# out<-weightit(form1,temp2, estimand = "ATT", method = "ebal")
table(temp2$treated)
temp<-merge(temp, 
            temp2[, wt:=weightit(form1,temp2, estimand = "ATT", method = "ebal")$weights]
            [, .(pseudo_nhs_num,wt)]
            , by="pseudo_nhs_num", all.x=T)

temp[, did:=as.numeric(date>=st_date & treated==1)]
setup = panel.matrices(temp, unit = 'pseudo_nhs_num', time = 'time', 
                       outcome = 'outcome1', treatment = 'did')
estimate = synthdid_estimate(setup$Y, setup$N0, setup$T0)
controls = as.data.table(synthdid_controls(estimate, mass = 1), keep.rownames=T)
controls_l = as.data.table(synthdid_controls(estimate, mass = 1, weight.type = "lambda") , keep.rownames=T)
temp<-merge(temp, controls[, .(rn=as.numeric(rn), ssdid_wt=`estimate 1`)], by.x="pseudo_nhs_num", by.y = "rn", all.x=T)

temp<-merge(temp, controls_l[, .(rn=as.numeric(rn), ssdid_lwt=`estimate 1`)], by.x="time", by.y = "rn", all.x=T)
temp[treated==1, ssdid_wt:=1]
temp[ is.na(ssdid_wt)==T, ssdid_wt:=0]


temp[, after:=as.numeric(date>=st_date)]
temp[after==1, ssdid_lwt:=1]
temp[ is.na(ssdid_lwt)==T, ssdid_lwt:=0]

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
   year = c(2022, 2023, 2024, 2025, 2026),
   cost_perevent = c(220, 280, 370, 420, 430)
 )
 EE <- merge(EE, emerg_costs, by = "year", all.x = TRUE, allow.cartesian = FALSE)
 #Per-quarter emergency cost
 EE[, cost_emerg := outcome1 * cost_perevent]
 
 # Compute Intervention Costs
 #  Adjust t_costs as per  total annual intervention budgets
 int_costs <- data.table(
   year = c(2022, 2023, 2024, 2025, 2026),
   t_costs = c(100000, 200000, 150000, 120000, 80000)
 )
 #############################################################
 # Adjusting inflation (Optional)
 #inflation_factors <- data.table( year = c(2022, 2023, 2024, 2025, 2026),
 #multiplier = c(1.10, 1.06, 1.03, 1.00, 1.01)  )
 # Merge inflation by year
 #EE <- merge(EE, inflation_factors, by = "year", all.x = TRUE)
 # Adjust both emergency and intervention costs
 #EE[, int_costsadj := int_costs * multiplier]
 #EE[, int_costsadj := int_costs * multiplier]
 #EE[, multiplier := NULL]
 ###################################################################################
 
 #  how many people were treated each year and then merge
 treated_counts <- unique(
   EE[!is.na(date_treated), .(pseudo_nhs_num, year_treated = year(date_treated))]
 )[, .N, by = year_treated]
 
 int_costs <- merge(int_costs, treated_counts,
                    by.x = "year", by.y = "year_treated", all.x = TRUE)
 
 # Per-person intervention cost for each year
 int_costs[, pp_cost := ifelse(!is.na(N) & N > 0, t_costs / N, 0)]
 
 # Intervention Costs to Patients
 EE[, Int_costpat := 0]
 EE[, year_treated := year(date_treated)]
 
 EE <- merge(EE,
             int_costs[, .(year, pp_cost)],
             by.x = "year_treated", by.y = "year",
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
