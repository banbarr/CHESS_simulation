

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


###Economic Evaluation  starts from here. Only the path may need to be changed

 EE <- read.csv("./data/chess_simulated_data.csv")
 
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
