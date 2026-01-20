

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

#  this uses a different simulated dataset - where the intervention distribution across age / sex / lsoa is taken from aggregate data from the services.  But there is something not quite right 
study_pop_ag<-fread("./data/chess_simulated_data2018_multiple_interventions_vs2.csv")
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
 # study_pop_ag[, age_2022:=age_2018]

library(data.table)
library(WeightIt)



source("estimate_pooled_did.R")

# # Basic usage with default variable names
#  study_pop_ag[order(date),cum_out:=frollsum(outcome1,n=4, align = "right"), by=.(pseudo_nhs_num) ]
#  study_pop_ag<- study_pop_ag[is.na(cum_out)==F]
#  
# Custom variable names

 system.time(result <- estimate_pooled_did(
   data = study_pop_ag,
   id_var = "pseudo_nhs_num",
   outcome_var = "outcome1",
   cum_out_var = "cum_out",
   time_var = "date",
   treated_var = "lcss_treated",
   date_treated_var = "lcss_date_treated",
   control_vars = c("age_b", "imd", "cvd", "dep"),
   estimand = "ATT",
   balancing_method = "glm",
   lead=450, 
   followup=450
))

# Access results
pooled_data <- result$aggregated_data
est_table <- result$estimates

# View overall estimate
est_table$overall[1]


plot1<-ggplot(data=pooled_data, aes(x=date, y=outcome1, group = as.factor(lcss_treated), color=as.factor(lcss_treated)))+geom_line()+geom_point()+geom_vline(aes(xintercept =date_treated_dt))+facet_wrap(date_treated_dt~.)+theme_minimal() +theme(strip.text.y = element_text(angle = 0, size=7))+scale_color_brewer("",  type = "qual", palette = 2, labels=c("Control", "Intervention"))
plot1

#  this bootstrapping of the results will take a long time to run. 

library(parallel)
library(bigmemory)

R <- 100
clusters <- as.numeric(unique(study_pop_ag[["pseudo_nhs_num"]]))
n_clusters <- length(clusters)
setkey(study_pop_ag, pseudo_nhs_num)


# OPTIMIZATION: Create cluster index for O(1) lookup
# This creates a list mapping each cluster to its row indices
cluster_indices <- split(1:nrow(study_pop_ag), 
                         study_pop_ag$pseudo_nhs_num)

# Precompute arguments
control_vars_vec <- c("age_2022", "imd", "cvd", "dep")

# Function that uses precomputed indices (FASTEST)
run_fast_bootstrap <- function(rep_num) {
   # Sample clusters
   sampled_clusters <- sample(clusters, n_clusters, replace = TRUE)
   
   # Get all row indices for sampled clusters
   boot_indices <- unlist(cluster_indices[sampled_clusters], 
                          use.names = FALSE, 
                          recursive = FALSE)
   
   # OPTIMIZATION: Use .SD with indices instead of join
   # This avoids any join overhead
   boot_data <- study_pop_ag[boot_indices, ]
   
   boot_data[, pid:=paste0(pseudo_nhs_num, 1:.N), by=.(date, pseudo_nhs_num)]
   # plot<-boot_data[, .(outcome1=mean(outcome1)), by=.(date, treated)]
   # ggplot(plot, aes(x=date, y=outcome1, colour = as.factor(treated), group=as.factor(treated)))+geom_line()
   # 
   # plot<-study_pop_ag[, .(outcome1=mean(outcome1)), by=.(date, treated)]
   # ggplot(plot, aes(x=date, y=outcome1, colour = as.factor(treated), group=as.factor(treated)))+geom_line()
   # 
          result <- estimate_pooled_did(
      data = boot_data,
      id_var = "pid",
      outcome_var = "outcome1",
      cum_out_var = "cum_out",
      time_var = "date",
      treated_var = "treated",
      date_treated_var = "date_treated",
      control_vars = control_vars_vec,
      estimand = "ATT",
      balancing_method = "ps", 
      lead=450, 
      followup=450
   )
   
   # result$estimates$overall[1]
   # 
   # pooled_data <- result$aggregated_data
   # est_table <- result$estimates
   # 
   # # View overall estimate
   # est_table$overall[1]
   # 
   # 
   # plot1<-ggplot(data=pooled_data, aes(x=date, y=outcome1, group = as.factor(treated), color=as.factor(treated)))+geom_line()+geom_vline(aes(xintercept =date_treated_dt))+facet_wrap(date_treated_dt~.)+theme_minimal() +theme(strip.text.y = element_text(angle = 0, size=7))+scale_color_brewer("",  type = "qual", palette = 2, labels=c("Control", "Intervention"))
   # plot1
   
   return(result$estimates$overall[1])
}

# Use mclapply with forking (Linux/Mac only)
# For Windows, use parLapply as shown above
estimates <- mclapply(1:R, run_fast_bootstrap, 
                      mc.cores = detectCores() - 1,
                      mc.preschedule = FALSE)  # Better for varying compute times

estimates <- unlist(estimates)
ci_95 <- quantile(as.numeric(estimates), probs = c(0.025, 0.5, 0.975), na.rm = T)
ci_95



#  this then runs the same analysis using an IV to instrument the did  as a robustess test, 

study_pop_ag<-fread("./data/chess_simulated_data2018_multiple_interventions_vs2.csv")
# ############ analysis 



library(data.table)
library(WeightIt)

iv<-fread("./data/chess_ivs.csv")

study_pop_ag<-merge(study_pop_ag, iv, by="lsoa21")
study_pop_ag[, prop_lcss_20172022:=(lcss_2017+
                                       lcss_2018+
                                       lcss_2019+ 
                                       lcss_2020+ 
                                       lcss_2021+ 
                                       lcss_2022)/pop]

study_pop_ag[,lcss_sc_iv:=prop_lcss_20172022*sh]

source("./scripts/estimate_pooled_did_iv.R")

# # Basic usage with default variable names
#  study_pop_ag[order(date),cum_out:=frollsum(outcome1,n=4, align = "right"), by=.(pseudo_nhs_num) ]
#  study_pop_ag<- study_pop_ag[is.na(cum_out)==F]
#  
# Custom variable names

result <- estimate_pooled_did_iv(
   data = study_pop_ag,
   id_var = "pseudo_nhs_num",
   outcome_var = "outcome1",
   cum_out_var = "cum_out",
   time_var = "date",
   treated_var = "lcss_treated",
   date_treated_var = "lcss_date_treated",
   control_vars = c("age_b", "imd", "cvd", "dep"),
   iv="lcss_sc_iv",
   estimand = "ATT",
   balancing_method = "glm",
   lead=450, 
   followup=450
)

# Access results
pooled_data <- result$aggregated_data
est_table <- result$estimates

# View overall estimate
est_table$overall[1]


plot1<-ggplot(data=pooled_data, aes(x=date, y=outcome1, group = as.factor(lcss_treated), color=as.factor(lcss_treated)))+geom_line()+geom_point()+geom_vline(aes(xintercept =date_treated_dt))+facet_wrap(date_treated_dt~.)+theme_minimal() +theme(strip.text.y = element_text(angle = 0, size=7))+scale_color_brewer("",  type = "qual", palette = 2, labels=c("Control", "Intervention"))
plot1



library(parallel)
library(bigmemory)

R <- 100
clusters <- as.numeric(unique(study_pop_ag[["pseudo_nhs_num"]]))
n_clusters <- length(clusters)
setkey(study_pop_ag, pseudo_nhs_num)


# OPTIMIZATION: Create cluster index for O(1) lookup
# This creates a list mapping each cluster to its row indices
cluster_indices <- split(1:nrow(study_pop_ag), 
                         study_pop_ag$pseudo_nhs_num)

# Precompute arguments
control_vars_vec <- c("age_b", "imd", "cvd", "dep")

# Function that uses precomputed indices (FASTEST)
run_fast_bootstrap <- function(rep_num) {
   # Sample clusters
   sampled_clusters <- sample(clusters, n_clusters, replace = TRUE)
   
   # Get all row indices for sampled clusters
   boot_indices <- unlist(cluster_indices[sampled_clusters], 
                          use.names = FALSE, 
                          recursive = FALSE)
   
   # OPTIMIZATION: Use .SD with indices instead of join
   # This avoids any join overhead
   boot_data <- study_pop_ag[boot_indices, ]
   
   boot_data[, pid:=paste0(pseudo_nhs_num, 1:.N), by=.(date, pseudo_nhs_num)]
   # plot<-boot_data[, .(outcome1=mean(outcome1)), by=.(date, treated)]
   # ggplot(plot, aes(x=date, y=outcome1, colour = as.factor(treated), group=as.factor(treated)))+geom_line()
   # 
   # plot<-study_pop_ag[, .(outcome1=mean(outcome1)), by=.(date, treated)]
   # ggplot(plot, aes(x=date, y=outcome1, colour = as.factor(treated), group=as.factor(treated)))+geom_line()
   # 
   result <- estimate_pooled_did_iv(
      data = boot_data,
      id_var = "pseudo_nhs_num",
      outcome_var = "outcome1",
      cum_out_var = "cum_out",
      time_var = "date",
      treated_var = "lcss_treated",
      date_treated_var = "lcss_date_treated",
      control_vars = control_vars_vec,
      iv="lcss_sc_iv",
      estimand = "ATT",
      balancing_method = "glm", 
      lead=450, 
      followup=450
   )
   
   # result$estimates$overall[1]
   # 
   # pooled_data <- result$aggregated_data
   # est_table <- result$estimates
   # 
   # # View overall estimate
   # est_table$overall[1]
   # 
   # 
   # plot1<-ggplot(data=pooled_data, aes(x=date, y=outcome1, group = as.factor(treated), color=as.factor(treated)))+geom_line()+geom_vline(aes(xintercept =date_treated_dt))+facet_wrap(date_treated_dt~.)+theme_minimal() +theme(strip.text.y = element_text(angle = 0, size=7))+scale_color_brewer("",  type = "qual", palette = 2, labels=c("Control", "Intervention"))
   # plot1
   
   return(result$estimates$overall[1])
}

# Use mclapply with forking (Linux/Mac only)
# For Windows, use parLapply
estimates <- mclapply(1:R, run_fast_bootstrap, 
                      mc.cores = detectCores() - 1,
                      mc.preschedule = FALSE)  # Better for varying compute times

estimates <- unlist(estimates)
ci_95 <- quantile(as.numeric(estimates), probs = c(0.025, 0.5, 0.975), na.rm = T)
ci_95







