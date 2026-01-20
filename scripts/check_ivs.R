


library(data.table)
library(fixest)
dt<-fread("./data/chess_ivs.csv")




#  check IVs LCSS use interaction between pre 2023 uptake and social housing % in lsoa , controlling for pre 2023 uptake and % social housing - or use interaction in panel data analysis

dt[, prop_lcss2023:=lcss_2023/pop]
dt[, prop_lcss_20172022:=(lcss_2017+
                            lcss_2018+
                            lcss_2019+ 
                            lcss_2020+ 
                            lcss_2021+ 
                            lcss_2022)/pop]

dt[,lcss_sc_iv:=prop_lcss_20172022*sh]


feols(prop_lcss2023~lcss_sc_iv+prop_lcss_20172022+sh+income_score_rate+
        employment_score_rate+
        barriers_to_housing_and_services_score+
        crime_score+
        living_environment_score, data=dt[lad_2021_name %in% c("Liverpool")])

feglm(lcss_2023~lcss_sc_iv+prop_lcss_20172022+sh+income_score_rate+
        employment_score_rate+
        barriers_to_housing_and_services_score+
        crime_score+
        living_environment_score+offset(log(pop)), family = poisson(),  data=dt[lad_2021_name %in% c("Liverpool")])


 #  lcss_sc_iv  - good predictor of (negative)  uptake

#  citizens advice
dt[, prop_cap:=cap/pop]

dt[, prop_cap2023:=cap_2023/pop]


# or use lsoa uptake in 2018 to intrument 2023 and after? 
dt[, prop_cap2018:=cap_2018/pop]

#  ut_201719 - uptake in 2017-19 in nearest GP practice
with(dt, plot( ut_201719, prop_cap))


feglm(cap~ut_201719+gp_dist+income_score_rate+
        employment_score_rate+
        barriers_to_housing_and_services_score+
        crime_score+
        living_environment_score
      +offset(log(pop)), family = poisson(), data=dt[lad_2021_name %in% c("Liverpool")])

feglm(cap_2022~ut_201719+gp_dist+income_score_rate+
        employment_score_rate+
        barriers_to_housing_and_services_score+
        crime_score+
        living_environment_score
      +offset(log(pop)), family = poisson(), data=dt[lad_2021_name %in% c("Liverpool")])

feglm(cap_2023~ut_201719+prop_cap2018+gp_dist+income_score_rate+
        employment_score_rate+health_deprivation_and_disability_score+
        barriers_to_housing_and_services_score+
        crime_score+
        living_environment_score
      +offset(log(pop)), family = poisson(), data=dt)



#  life rooms - use distance to life roms locations 

dt[, prop_lr:=lr/pop]
with(dt[lad_2021_name %in% c("Knowsley", "Liverpool","Sefton")], plot(log(min_lr), prop_lr))


feglm(lr~log(min_lr)+income_score_rate+
        employment_score_rate+
        barriers_to_housing_and_services_score+
        crime_score+
        living_environment_score
      +offset(log(pop)), family = poisson(), data=dt[lad_2021_name %in% c("Knowsley", "Liverpool","Sefton")])


 #  greater distance form life rooms reduces uptake

#  Households into woek
dt[, prop_hiw:=hiw/pop]
# min_hiw_w2w1 is distance to nearest adult learning centre
# min_jc is is distance to nearest job centre
with(dt, plot( log(min_hiw_w2w1), prop_hiw))


with(dt, plot(log(min_jc), prop_hiw))


m1<-feglm(hiw~log(min_hiw_w2w1)+ log(min_jc)+
            income_score_rate+
            employment_score_rate+
            barriers_to_housing_and_services_score+
            crime_score+
            living_environment_score
          +offset(log(pop)), family = poisson(), data=dt)
summary(m1)

# distance to job centres and adult learing centres reduce uptake


dt[, prop_hiw:=hiw/pop]

dt[, prop_hiw2020:=hiw_2020/pop]



m1<-feglm(hiw_2022~log(min_hiw_w2w1)+ log(min_jc)+prop_hiw2020+
            income_score_rate+
            employment_score_rate+
            barriers_to_housing_and_services_score+
            crime_score+
            living_environment_score
          +offset(log(pop)), family = poisson(), data=dt)
summary(m1)


# ways to work
dt[, prop_w2w:=w2w/pop]
with(dt, plot( log(min_hiw_w2w1), prop_w2w))

dt[, prop_:=w2w/pop]
with(dt, plot(log(min_jc), prop_w2w))

m1<-feglm(w2w~log(min_hiw_w2w1)+ log(min_jc)+
            income_score_rate+
            employment_score_rate+
            barriers_to_housing_and_services_score+
            crime_score+
            living_environment_score
          +offset(log(pop)), family = poisson(), data=dt)
summary(m1)

# distance to job centers and adult learning centers reduce uptake

#  also using with previous uptake 
dt[, prop_w2w2017:=w2w_2017/pop]
m1<-feglm(w2w~log(min_hiw_w2w1)+ log(min_jc)+log(prop_w2w2017)+
            income_score_rate+
            employment_score_rate+
            barriers_to_housing_and_services_score+
            crime_score+
            living_environment_score
          +offset(log(pop)), family = poisson(), data=dt)
summary(m1)








