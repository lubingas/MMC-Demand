rm(list=ls())
library(tidyverse)
library(lubridate)

setwd("./Data")

# get functions we need
source("clean_data_functions.R")

# Get the data
data = d = read_csv("maindata_full3.csv", col_names = TRUE)    # use for csv files

# first calculate ages for people who knew their exact date of birth

df = data %>%
  # fix age issues
  mutate(last_age = if_else(known_dob==1, time_length(interval(mdy(dob), mdy(today)), "years"), last_age))  %>%
  mutate(last_age = if_else(last_age < 18, mean(last_age[which(last_age >= 18)]), last_age), age_30_more = last_age > 30) %>%
  # now recode respondent's tribe
  mutate(resp_tribe = if_else(resp_tribe==1, "Muganda", 
                              if_else(resp_tribe %in% c(2,4), "Kiga_Nkole", 
                                      if_else(resp_tribe==3, "Musoga", "Other")))) %>%
  select(last_age, resp_tribe) %>%
  factor(resp_tribe, levels = c("Muganda", "Kiga_Nkole", "Musoga", "Other"))
  


mean(d$last_age)

d$last_age <- ifelse(d$known_dob==1, (as.Date(d$today, "%m/%d/%y") - as.Date(d$dob, "%m/%d/%y"))/365, d$last_age)
d$last_age <- ifelse(d$last_age<18, mean(d$last_age[which(d$last_age>=18)]), d$last_age)
d$age_30_more    = as.numeric(d$last_age > 30)

mean(d$age_30_more)

# tribe manipulation
#table(df$resp_tribe)
d$muganda=as.numeric(d$resp_tribe==1)
d$kiga_nkole=as.numeric(d$resp_tribe %in% c(2,4))
d$musoga=as.numeric(d$resp_tribe==3)
d$other_tribe=as.numeric(d$resp_tribe!=1)

# religion manipulation
#table(d$resp_religion)
d$catholic=as.numeric(d$resp_religion==1)
d$protestant=as.numeric(d$resp_religion %in% c(2,4))
d$other_relig=as.numeric(d$resp_religion %in% c(3,4,5))

# schooling dummy variables
#table(d$school_level,exclude = NULL)
table(d$attended_school,exclude = NULL)
#cbind(d$attended_school,d$school_level)
d$noeduc=as.numeric(d$attended_school==2|d$school_level==0)
d$pri=as.numeric(d$attended_school==1&d$school_level==1)
d$olev=as.numeric(d$attended_school==1&d$school_level==2)
d$alev=as.numeric(d$attended_school==1&d$school_level>2)
table(d$school_years, exclude=NULL)

d$school_years <- ifelse(d$school_level==0, 0, ifelse(d$school_level==1, d$school_class_year, ifelse(d$school_level==2, d$school_class_year+7, ifelse(d$school_level==3, d$school_class_year+3, ifelse(d$school_level==4, d$school_class_year+13, ifelse(d$school_level==5, d$school_class_year+13, 99))))))
d$educ_pri_more  = abs(as.numeric(d$noeduc==1 | d$pri==1)-1)


# MARITAL STATUS DUMMIES
table(d$marital_status,exclude = NULL)
d$married=as.numeric(d$marital_status==1)
d$nevermarried=as.numeric(d$marital_status==2)
d$other_marital=as.numeric(d$marital_status %in% c(3,4))

## children
d$nochild=as.numeric(d$children_fathered==0)
#table(d$children_fathered)

# where you live, i.e. urban or rural
d$urban=as.numeric(d$living_area_n==1)

# OCCUPATION
table(d$occupation_n)
d$fisher=as.numeric(d$occupation_n==1)
d$trader=as.numeric(d$occupation_n==2)
d$farmer=as.numeric(d$occupation_n==4)
d$occup_other=as.numeric(d$occupation_n==5)
d$nooccup=as.numeric(d$occupation_n==6)
d$boda=as.numeric(str_detect(d$occupation_other, "Boda"))

# INCOME/EXPENDITURE
#cbind(d$monthly_expenditure,d$monthly_income,d$occupation_n)[d$occupation_n %in% c(5),]
#cbind(d$monthly_expenditure,d$monthly_income,d$occupation_n)[d$monthly_expenditure>d$monthly_income,]

d$monthly_income <- ifelse(d$occupation_n==6, 0, d$monthly_income)  # if not employed, then no income (assumption)
d$monthly_expenditure <- ifelse(d$occupation_n==6, 0, d$monthly_expenditure)
d$abs_income <- d$monthly_income-d$monthly_expenditure

d$ispoor=as.numeric(d$monthly_income < 60*3750)
d$age25=as.numeric(d$last_age < 25)
d$age25_35=as.numeric(d$last_age >= 25 & d$last_age < 35 )
d$age35=as.numeric(d$last_age >= 35)

#hist(d$abs_income)
#hist(scale(d$monthly_income))

# wealth index items
# SOURCE OF DRINKING WATER
# well or borehole
d$wellwater <- as.numeric(d$drinking_water_source==2 | d$drinking_water_source_other %in% c("Bore hole","Borehole"))
#pipedwater
d$pipedwater <- as.numeric(d$drinking_water_source==1 | d$drinking_water_source_other %in% c("Buy piped water from next village"))
# Spring
d$springwater <- as.numeric(d$drinking_water_source==3)
#Surface
d$surfacewater <- as.numeric(d$drinking_water_source==4 | d$drinking_water_source_other %in% c("Rain water"))
#Bottled
d$bottledwater <- as.numeric(d$drinking_water_source_other %in% c("Bottled",
                                                                  "Bottled  water",
                                                                  "Bottled water",
                                                                  "Buy boiled water",
                                                                  "Buy bottled",
                                                                  "Buy bottled rwenzori water",
                                                                  "Mineral water",
                                                                  "Packed bottled water",
                                                                  "Packed Bottled water",
                                                                  "Packed Mineral water",
                                                                  "Packed water, Rwenzori",
                                                                  "Rwenzori"))
# do we have everyone--all 406 respondents? TES
#sum(d$wellwater,d$pipedwater,d$springwater,d$surfacewater,d$bottledwater)

# ROOF MATERIAL
#table(d$roof)
#table(d$roof_material_other)
d$fin_roof <- as.numeric(d$roof==3)
d$rud_roof <- as.numeric(d$roof==2)
d$nat_roof <- as.numeric(d$roof==1)

# toilet
#table(d$toilet_facility)
d$toilet <- as.numeric(d$toilet_facility==1)
d$pit_slab <- as.numeric(d$toilet_facility==2)
d$pit_noslab <- as.numeric(d$toilet_facility==3)

#table(d$toilet_share)
d$toilet_share <- as.numeric(d$toilet_share==1)

## OWNERSHIP 1
# see below

## sexual history
#cbind(d$sex_first, d$sex_first_age, d$sex_last_time_period, d$sex_last_time, d$last_time_condom, d$sex_partner_type, d$sex_partner_type_other)

d$injunctive7 <- ifelse(is.na(d$injunctive7), 50, d$injunctive7)
#mean(d$injunctive7,na.rm=TRUE)
#hist(d$injunctive7)
#cbind(d$school_level,d$school_years)

#d$scaled_income=d$abs_income/3750


#mean(d$scaled_income<1.99*28)

#quantile(d$scaled_income, c(0.3333, 0.6667))
#hist(d$scaled_income)

# replicate mkspline command
#d$scaled_income_0_12=pmin(d$scaled_income,12) # set to d$scaled income, else 12 if d$scaled income is more than 12
#d$scaled_income_12_40=pmax(pmin(d$scaled_income,40),12)-12 # set to d$scaled income - 12, else 0 if d$scaled income is less than 12
#d$scaled_income_40_60=pmax(pmin(d$scaled_income,60),40)-40
#d$scaled_income_60_more=pmax(d$scaled_income,60)-60

#income <- cbind.data.frame(d$abs_income,d$scaled_income,d$scaled_income_0_12,d$scaled_income_12_40,d$scaled_income_40_60,d$scaled_income_60_more)
#income <- income[order(income[,2]),]

#d$scaled_income_0_60=pmin(d$scaled_income,60) # set to d$scaled income, else 12 if d$scaled income is more than 12
#d$scaled_income_60_more=pmax(d$scaled_income,60)-60

#income=cbind.data.frame(d$abs_income,d$scaled_income,d$scaled_income_0_12,d$scaled_income_60_more)
#income=income[order(income[,2]),]

#d$ispoor=d$scaled_income < 60

#table(d$ispoor)/nrow(d)

# now process numbers on risk
d$trans_no_cd <- ifelse((d$ext2==50 & d$ext3==-1), d$ext3_other, d$ext2)
d$trans_wi_cd <- ifelse((d$ext4==50 & d$ext5==-1), d$ext5_other, d$ext4)

# process numbers on prevalence
d$prev_all <- ifelse((d$ext6==50 & d$ext7==-1), d$ext7_other, d$ext6)
d$prev_att <- ifelse((d$ext8==50 & d$ext9==-1), d$ext9_other, d$ext8)

# process numbers on mortality
d$mort_HIV_neg_1 <- ifelse((d$m1==50 & d$m2==-1), d$m2_other, d$m1)
d$mort_HIV_neg_5 <- ifelse((d$m3==50 & d$m4==-1), d$m4_other, d$m3)
d$mort_HIV_neg_10 <- ifelse((d$m5==50 & d$m6==-1), d$m6_other, d$m5)

d$mort_HIV_pos_1 <- ifelse((d$m21==50 & d$m22==-1), d$m22_other, d$m21)
d$mort_HIV_pos_5 <- ifelse((d$m23==50 & d$m24==-1), d$m24_other, d$m23)
d$mort_HIV_pos_10 <- ifelse((d$m25==50 & d$m25_1==-1), d$m25_1_other, d$m25)

# process numbers on HIV treatment expectations
d$HIV_tx <- ifelse((d$m26==50 & d$m27==-1), d$m27_other, d$m26)

# process numbers on mortality conditional on receiving treatment
d$mort_HIV_pos_tx_1 <- ifelse((d$mh21==50 & d$mh22==-1), d$mh22_other, d$mh21)
d$mort_HIV_pos_tx_5 <- ifelse((d$mh3==50 & d$mh4==-1), d$mh4_other, d$mh3)
d$mort_HIV_pos_tx_10 <- ifelse((d$mh5==50 & d$mh6==-1), d$m6_other, d$mh5)

# d$circum_intent <- factor(d$circum_intension,levels = c("-2","-1","0","1","2"))

d$riskpref_15_5 <- d$r1
d$riskpref_5_0 <- d$r2
d$riskpref_10_10 <- d$r3
d$timepref_5_1_4 <- d$r3.1
d$timepref_5_4_10 <- d$r4
d$timepref_30_1_4 <- d$r5
d$timepref_30_4_10 <- d$r6

d$ID <- seq(1,nrow(d))

# gather colums of choices
d$choice1 <- rowSums(cbind(d$block_1_scenario_qn_1, d$block_2_scenario_qn_1, d$block_3_scenario_qn_1),na.rm = TRUE)
d$choice2 <- rowSums(cbind(d$block_1_scenario_qn_2, d$block_2_scenario_qn_2, d$block_3_scenario_qn_2),na.rm = TRUE)
d$choice3 <- rowSums(cbind(d$block_1_scenario_qn_3, d$block_2_scenario_qn_3, d$block_3_scenario_qn_3),na.rm = TRUE)
d$choice4 <- rowSums(cbind(d$block_1_scenario_qn_4, d$block_2_scenario_qn_4, d$block_3_scenario_qn_4),na.rm = TRUE)
d$choice5 <- rowSums(cbind(d$block_1_scenario_qn_5, d$block_2_scenario_qn_5, d$block_3_scenario_qn_5),na.rm = TRUE)
d$choice6 <- rowSums(cbind(d$block_1_scenario_qn_6, d$block_2_scenario_qn_6, d$block_3_scenario_qn_6),na.rm = TRUE)
d$choice7 <- rowSums(cbind(d$block_1_scenario_qn_7, d$block_2_scenario_qn_7, d$block_3_scenario_qn_7),na.rm = TRUE)
d$choice8 <- rowSums(cbind(d$block_1_scenario_qn_8, d$block_2_scenario_qn_8, d$block_3_scenario_qn_8),na.rm = TRUE)
d$choice9 <- rowSums(cbind(d$block_1_scenario_qn_9, d$block_2_scenario_qn_9, d$block_3_scenario_qn_9),na.rm = TRUE)
d$choice10 <- rowSums(cbind(d$block_1_scenario_qn_10, d$block_2_scenario_qn_10, d$block_3_scenario_qn_10),na.rm = TRUE)
d$choice11 <- rowSums(cbind(d$block_1_scenario_qn_11, d$block_2_scenario_qn_11, d$block_3_scenario_qn_11),na.rm = TRUE)
d$choice12 <- rowSums(cbind(d$block_1_scenario_qn_12, d$block_2_scenario_qn_12, d$block_3_scenario_qn_12),na.rm = TRUE)


#d$block <- d$choiceblock
#d$occupation <- d$occupation_n
# d[, substr(names(d), 1, 6) == "choice"]

# str(d,list.len = ncol(d))

d$discountrate_5_1_4 <- discount(Xv=d$timepref_5_1_4,Xs=5,v=4,s=1)
d$discountrate_5_4_10 <- discount(Xv=d$timepref_5_4_10,Xs=5,v=10,s=4)
d$discountrate_30_1_4 <- discount(Xv=d$timepref_30_1_4,Xs=30,v=4,s=1)
d$discountrate_30_4_10 <- discount(Xv=d$timepref_30_4_10,Xs=30,v=10,s=4)

## calculate social values orientation figures
# sv1
self1 <- rep(8500,9)
other1 <- c(8500,7600,6800,5900,5000,4100,3300,2400,1500)

# sv2
self2 <- c(8500,8700,8900,9100,9300,9400,9600,9800,10000)
other2 <- c(1500,1900,2400,2800,3300,3700,4100,4600,5000)

# sv3
self3 <- c(5000,5400,5900,6300,6800,7200,7600,8100,8500)
other3 <- c(10000,9800,9600,9400,9300,9100,8900,8700,8500)

# sv4
self4 <- c(5000,5400,5900,6300,6800,7200,7600,8100,8500)
other4 <- c(10000,8900,7900,6800,5800,4700,3600,2600,1500)

# sv5
self5 <- c(10000,9400,8800,8100,7500,6900,6300,5600,5000)
other5 <- c(5000,5600,6300,6900,7500,8100,8800,9400,10000)

# sv6
self6 <- c(10000,9800,9600,9400,9300,9100,8900,8700,8500)
other6 <- c(5000,5400,5900,6300,6800,7200,7600,8100,8500)

socval <- cbind(getsocvals(self = self1, other = other1, socval = d$sv1),
                getsocvals(self = self2, other = other2, socval = d$sv2),
                getsocvals(self = self3, other = other3, socval = d$sv3),
                getsocvals(self = self4, other = other4, socval = d$sv4),
                getsocvals(self = self5, other = other5, socval = d$sv5),
                getsocvals(self = self6, other = other6, socval = d$sv6))

colnames(socval) <- c("payself1","payother1",
                      "payself2","payother2",
                      "payself3","payother3",
                      "payself4","payother4",
                      "payself5","payother5",
                      "payself6","payother6")

svscore <- atan(apply(socval[,c(1,3,5,7,9,11)],1,mean)/apply(socval[,c(2,4,6,8,10,12)],1,mean))*180/pi
d$svscore <- svscore
svcat <- ifelse(svscore > 57.15, 1, ifelse((svscore <= 57.15 & svscore > 22.45), 2, ifelse((svscore <= 22.45 & svscore > -12.04), 3, ifelse(svscore <=-12.04, 4, 0))))
d$svcat <- svcat

# rename, force to use old plyr
d <- plyr::rename(d, replace=c("current_health_status"="health_status",
                 "item_own_electricity"="electricity","item_own_radio"="radio","item_own_bicycle"="bicycle","item_own_car_truck"="car_truck","item_own_boat_engine"="boat_engine","item_own_boat_manual"="boat_manual",
                 "item_own_land"="land","item_own_cattle"="cattle","item_own_sheep_goats"="sheep_goats","item_own_business_shop"="business_shop","item_own_house"="house",
                 "q21"="friend_circum","q22"="family_circum","q23"="promot_materials","q24"="discuss_partner","q25"="partner_rec","q26"="hw_rec","q27"="friend_rec",
                 "q28"="family_rec","q29"="religlead_rec","q30"="villagehead_rec",
                 #"sub1"="prob_self_HIV","sub2"="prob_STDs","sub3"="prob_partner_HIV","sub4"="prob_partner_CC","sub5"="prob_ImproveHygiene",
                 #"sub6"="prob_ImproveSexLife","sub7"="prob_LongerSex","sub8"="prob_PleasePartner","sub9"="prob_SexPartner","sub10"="prob_ReduceSexPleasure",
                 #"sub11"="prob_ReduceLibido","sub12"="prob_Infertile",
                 "circum_intension"="circum_intent","choiceblock"="block","occupation_n"="occupation",
                 "self1"="read_news_eng", "self2"="read_news_lug",
                 "self12"="read_letter_eng", "self22"="read_letter_lug",
                 "self13"="read_phone_eng", "self23"="read_phone_lug",
                 "self120"="write_letter_eng", "self220"="write_letter_lug",
                 "self1201"="write_phone_eng", "self2201"="write_phone_lug",
                 "l1"="numeracy1","l2"="numeracy2","l3"="numeracy3","l4"="numeracy4",
                 "s13"="nopain_proc_device","s14"="somepain_proc_device","s15"="extpain_proc_device",
                 "s16"="nopain_wear_device","s17"="somepain_wear_device","s18"="extpain_wear_device",
                 "s19"="nopain_remove_device","s20"="somepain_remove_device","s21"="extpain_remove_device",
                 "s22"="nopain_heal_device","s23"="somepain_heal_device","s24"="extpain_heal_device",
                 "s25"="bleed_device","s26"="heal_device","s27"="infection_device","s28"="looknice_device",
                 "min100"="time_heal_device_min","max100"="time_heal_device_max","avg_min100_max100"="time_heal_device_avg","s29"="prob_time_heal_device",
                 "min200"="time_work_device_min","max200"="time_work_device_max","avg_min200_max200"="time_work_device_avg","s30"="prob_time_work_device",
                 "min300"="time_sex_device_min","max300"="time_sex_device_max","avg_min300_max300"="time_sex_device_avg","s301"="prob_time_sex_device",
                 "s29_1"="nopain_proc_surgery","s30_1"="somepain_proc_surgery","s31_1"="extpain_proc_surgery",
                 "s32"="nopain_heal_surgery","s33"="somepain_heal_surgery","s34"="extpain_heal_surgery",
                 "s41"="bleed_surgery","s42"="heal_surgery","s43"="infection_surgery","s44"="looknice_surgery",
                 "min1_"="time_heal_surgery_min","max1_"="time_heal_surgery_max","avg_min1_max1"="time_heal_surgery_avg","s45"="prob_time_heal_surgery",
                 "min400"="time_work_surgery_min","max400"="time_work_surgery_max","avg_min400_max400"="time_work_surgery_avg","s46"="prob_time_work_surgery",
                 "min500"="time_sex_surgery_min","max500"="time_sex_surgery_max","avg_min500_max500"="time_sex_surgery_avg","S47"="prob_time_sex_surgery",
                 "phy1"="selfconsciousness1","phy2"="selfconsciousness2","phy3"="selfconsciousness3","phy4"="selfconsciousness4","phy5"="selfconsciousness5","phy6"="selfconsciousness6",
                 "phy7"="conscientiousness1","phy8"="conscientiousness2","phy9"="conscientiousness3","phy10"="conscientiousness4","phy11"="conscientiousness5","phy12"="conscientiousness6","phy13"="conscientiousness7","phy14"="conscientiousness8","phy15"="conscientiousness9",
                 "phy16"="extraversion1","phy17"="extraversion2","phy18"="extraversion3","phy19"="extraversion4","phy20"="extraversion5","phy21"="extraversion6","phy22"="extraversion7","phy23"="extraversion8",
                 "phy24"="loc1","phy25"="loc2","phy26"="loc3","phy27"="loc4","phy28"="loc5","phy29"="loc6"))

d <-  subset(d, select=c(ID,last_age,age25,age25_35,age35,age_30_more
                         resp_tribe,muganda,kiga_nkole,musoga,other_tribe,
                         resp_religion,catholic,protestant,other_relig,
                         school_level,school_years,noeduc,pri,olev,alev,educ_pri_more
                         marital_status,married,nevermarried,other_marital,
                         nochild,children_fathered,
                         urban,
                         occupation,fisher,trader,farmer,boda,occup_other,nooccup,
                         monthly_income,monthly_expenditure,abs_income,ispoor,
                         drinking_water_source,wellwater,pipedwater,springwater,surfacewater,bottledwater,
                         roof,fin_roof,rud_roof,nat_roof,
                         toilet_facility,toilet,pit_slab,pit_noslab,toilet_share,
                         electricity,radio,bicycle,car_truck,boat_engine,boat_manual,
                         land,cattle,sheep_goats,business_shop,house,
                         health_status,
                         sex_first,sex_first_age,sex_last_time_period,sex_last_time,last_time_condom,sex_partner_type,sex_partner_type_other,
                         friend_circum,family_circum,promot_materials,discuss_partner,partner_rec,hw_rec,friend_rec,family_rec,religlead_rec,villagehead_rec,
                         trans_no_cd,trans_wi_cd,prev_all,prev_att,
                         mort_HIV_neg_1,mort_HIV_neg_5,mort_HIV_neg_10,mort_HIV_pos_1,mort_HIV_pos_5,mort_HIV_pos_10,
                         HIV_tx,mort_HIV_pos_tx_1,mort_HIV_pos_tx_5,mort_HIV_pos_tx_10,
                         riskpref_15_5,riskpref_5_0,riskpref_10_10,discountrate_5_1_4,discountrate_5_4_10,discountrate_30_1_4,discountrate_30_4_10,
                         #prob_self_HIV,prob_STDs,prob_partner_HIV,prob_partner_CC,prob_ImproveHygiene,prob_ImproveSexLife,
                         #prob_LongerSex,prob_PleasePartner,prob_SexPartner,prob_ReduceSexPleasure,prob_ReduceLibido,prob_Infertile,
                         sub1,sub2,sub3,sub4,sub5,sub6,sub7,sub8,sub9,sub10,sub11,sub12,
                         nopain_proc_device,somepain_proc_device,extpain_proc_device,
                         nopain_wear_device,somepain_wear_device,extpain_wear_device,
                         nopain_remove_device,somepain_remove_device,extpain_remove_device,
                         nopain_heal_device,somepain_heal_device,extpain_heal_device,
                         bleed_device,heal_device,infection_device,looknice_device,
                         time_heal_device_min,time_heal_device_max,time_heal_device_avg,prob_time_heal_device,
                         time_work_device_min,time_work_device_max,time_work_device_avg,prob_time_work_device,
                         time_sex_device_min,time_sex_device_max,time_sex_device_avg,prob_time_sex_device,
                         nopain_proc_surgery,somepain_proc_surgery,extpain_proc_surgery,
                         nopain_heal_surgery,somepain_heal_surgery,extpain_heal_surgery,
                         bleed_surgery,heal_surgery,infection_surgery,looknice_surgery,
                         time_heal_surgery_min,time_heal_surgery_max,time_heal_surgery_avg,prob_time_heal_surgery,
                         time_work_surgery_min,time_work_surgery_max,time_work_surgery_avg,prob_time_work_surgery,
                         time_sex_surgery_min,time_sex_surgery_max,time_sex_surgery_avg,prob_time_sex_surgery,
                         block,
                         choice1,choice2,choice3,choice4,choice5,choice6,choice7,choice8,choice9,choice10,choice11,choice12,
                         circum_intent,
                         injunctive1,injunctive2,injunctive3,injunctive4,injunctive5,injunctive6,injunctive7,
                         descriptive1,descriptive2,descriptive3,descriptive4,
                         cb1,cb2,cb3,cb4,cb5,cb6,cb7,cb8,cb9,cb10,cb11,cb12,cb13,cb14,cb15,cb16,cb17,cb18,cb19,cb20,cb21,
                         e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,
                         selfconsciousness1,selfconsciousness2,selfconsciousness3,selfconsciousness4,selfconsciousness5,selfconsciousness6,
                         conscientiousness1,conscientiousness2,conscientiousness3,conscientiousness4,conscientiousness5,conscientiousness6,conscientiousness7,conscientiousness8,conscientiousness9,
                         extraversion1,extraversion2,extraversion3,extraversion4,extraversion5,extraversion6,extraversion7,extraversion8,
                         loc1,loc2,loc3,loc4,loc5,loc6,
                         svscore,
                         read_news_eng,read_news_lug,
                         read_letter_eng,read_letter_lug,
                         read_phone_eng,read_phone_lug,
                         write_letter_eng,write_letter_lug,
                         write_phone_eng,write_phone_lug,
                         numeracy1,numeracy2,numeracy3,numeracy4))


# d[which(d$monthly_income<d$monthly_expenditure),]

riskcols <- c("trans_no_cd","trans_wi_cd","prev_all","prev_att",
              "mort_HIV_neg_1","mort_HIV_neg_5","mort_HIV_neg_10","mort_HIV_pos_1","mort_HIV_pos_5","mort_HIV_pos_10",
              "HIV_tx","mort_HIV_pos_tx_1","mort_HIV_pos_tx_5","mort_HIV_pos_tx_10")

subcols<- c("sub1","sub2","sub3","sub4","sub5","sub6",
            "sub7","sub8","sub9","sub10","sub11","sub12")


socvals <- c("injunctive1","injunctive2","injunctive3","injunctive4","injunctive5","injunctive6","injunctive7","descriptive1","descriptive2","descriptive3","descriptive4")

socvars <- c("friend_circum","family_circum","promot_materials","discuss_partner","partner_rec","hw_rec","friend_rec","family_rec","religlead_rec","villagehead_rec",
             "electricity","radio","bicycle","car_truck","boat_engine","boat_manual",
             "land","cattle","sheep_goats","business_shop","house")

# first calculate probability of pain
painvars <- c("nopain_proc_device","nopain_wear_device","nopain_remove_device","nopain_heal_device",
              "nopain_proc_surgery","nopain_heal_surgery")
painvars2 <- c("pain_proc_device","pain_wear_device","pain_remove_device","pain_heal_device",
              "pain_proc_surgery","pain_heal_surgery")
d[painvars2] <- apply(d[painvars], 2, FUN=function(x) (100-x))

d$pain_proc = (d$pain_proc_device + d$pain_proc_surgery)/2
d$pain_heal = (d$pain_heal_device + d$pain_heal_surgery)/2
d$time_heal = (d$time_heal_device_avg + d$time_heal_surgery_avg)/2
d$time_work = (d$time_work_device_avg + d$time_work_surgery_avg)/2
d$time_sex = (d$time_sex_device_avg + d$time_sex_surgery_avg)/2
d$bleed = (d$bleed_device + d$bleed_surgery)/2
d$heal = (d$heal_device + d$heal_surgery)/2
d$infection = (d$infection_device + d$infection_surgery)/2

circumprobbeliefs <- c("bleed_device","infection_device",
                       "pain_proc_device",
                       #"nopain_wear_device",
                       #"nopain_remove_device",
                       "pain_heal_device",
                       "bleed_surgery","infection_surgery",
                       "pain_proc_surgery",
                       "pain_heal_surgery",
                       "bleed","infection",
                       "pain_proc","pain_heal")

circumcontbeliefs <- c("time_heal_device_avg","time_work_device_avg","time_sex_device_avg",
                       "time_heal_surgery_avg","time_work_surgery_avg","time_sex_surgery_avg",
                       "time_heal","time_work","time_sex")

d[socvars] <- apply(d[socvars], 2, FUN=function(x) (abs(x-2)))
d[socvals] <- apply(d[socvals], 2, FUN=function(x) (x/100))
d[riskcols] <- apply(d[riskcols], 2, FUN=function(x) (x/100))
d[subcols] <- apply(d[subcols], 2, FUN=function(x) (x/100))
d[circumprobbeliefs] <- apply(d[circumprobbeliefs], 2, FUN=function(x) (x/100))

apply(d[subcols], 2, mean)

apply(d[socvars], 2, mean)

apply(d[socvals], 2, mean)

## calculate benefit of circumcision
d$pHIV_notC=d$prev_att * d$trans_no_cd               #/* perceived probability of HIV if not circumcised */
d$pHIV_C=d$prev_att*d$trans_no_cd*d$sub1             #/* perceived probability of HIV if circumcised (lower) */
d$benefitHIV=d$pHIV_C - d$pHIV_notC                  #/* reduction in HIV risk confered by circumcision, should be positively valued */

mean(d$benefitHIV)



### Summarize demographics
contvars <- c("last_age","monthly_income","monthly_expenditure","abs_income","school_years","children_fathered")
catvars <- c("age25","age25_35","age35","ispoor",
             "married","nevermarried","other_marital",
             "noeduc","pri","olev","alev",
             "nochild",
             "muganda","kiga_nkole","musoga","other_tribe",
             "catholic","protestant","other_relig",
             "fisher","trader","farmer","boda","occup_other","nooccup",
             "urban",
             "wellwater","pipedwater","springwater","surfacewater","bottledwater",
             "fin_roof","rud_roof","nat_roof",
             "toilet","pit_slab","pit_noslab","toilet_share",
             "electricity","radio","bicycle","car_truck","boat_engine","boat_manual",
             "land","cattle","sheep_goats","business_shop","house",
             "friend_circum","family_circum",
             "promot_materials",
             "discuss_partner","partner_rec","hw_rec","friend_rec","family_rec","religlead_rec","villagehead_rec")

sumtab <- rbind(matrix(sapply(d[contvars],subdescfx),nrow=6,byrow=TRUE),matrix(sapply(d[catvars],subdescfx2),nrow=59,byrow=TRUE))

sumtab <- data.frame(sumtab, c(sapply(d[contvars],descrfx), sapply(d[catvars],descrfx2)))

colnames(sumtab) <- c(paste0("Block 1 (n = ",nrow(d[d$block==1,]),")"),
                      paste0("Block 2 (n = ",nrow(d[d$block==2,]),")"),
                      paste0("Block 3 (n = ", nrow(d[d$block==3,]),")"),
                      paste0("Overall (n = ", nrow(d),")"))
names <- c("Age, mean (sd)","Monthly Income in USD, mean (sd)", "Monthly Expenditure in USD, mean (sd)","Absolute Income in USD, mean (sd)","Years of schooling, mean (sd)","No of Children, mean (sd)",
           "Age < 25","Age 25 - 35","Age 35+","Is poor (Income < $60)",
           "Married","Never married","Other status",
           "No Education/Pre-primary","Primary","O'Levels (Sec)","A'Levels (Sec)",
           "No Children",
           "Muganda","Mukiga/Munyankole","Musoga","Other tribe",
           "Catholic","Protestant","Other religion",
           "Fisherman","Trading","Farming","Boda boda cyclist","Other occupation","Unemployed",
           "Lives in urban area",
           "Well water","Piped water","Spring water","Surface water","Bottled water",
           "Finished roof","Rudimentary roof","Natural roof",
           "Flushing toilet","Pit latrine with slab","Pit latrine without slab","Toilet is shared",
           "Has electricity","Has radio","Has bicycle","Has car/truck","Has boat with engine","Has manual boat",
           "Has land","Has cattle","Has sheep/goats","Has business/shop","Has house",
           "Friend circumcised","Family member circumcised",
           "Seen promotional materials",
           "Discussed with partner","Partner recommended","Health worker recommended","Friend recommended",
           "Family member recommended","Religious leader recommended",
           "village head recommended")

rownames(sumtab) <- names

# Now summarize beliefs
riskvars <- c("trans_no_cd","trans_wi_cd","prev_all","prev_att",
              "mort_HIV_neg_10","mort_HIV_pos_10",
              "mort_HIV_pos_tx_10","sub1","descriptive3","benefitHIV")

beliefs <- data.frame(t(sapply(d[riskvars],subdescfx)),sapply(d[riskvars],descrfx))
colnames(beliefs) <- c(paste0("Block 1 (n = ",nrow(d[d$block==1,]),")"),
                      paste0("Block 2 (n = ",nrow(d[d$block==2,]),")"),
                      paste0("Block 3 (n = ", nrow(d[d$block==3,]),")"),
                      paste0("Overall (n = ", nrow(d),")"))
names <- c("HIV transmission probability without condom use",
           "HIV transmission probability with condom use",
           "Overall prevalence of HIV",
           "Prevalence of HIV in those respondent is attracted to",
           "10-year mortality if HIV negative",
           "10-year mortality if HIV positive, and not on treatment",
           "10-year mortality if HIV positive, and on treatment",
           "Likelihood that circumcision will protect you from HIV",
           "Probability that your friends would get circumcised",
           "Net reduction in HIV risk")

rownames(beliefs) <- names

circumbeliefs <- c(circumprobbeliefs,circumcontbeliefs)

circumbeliefs <- data.frame(t(sapply(d[circumbeliefs],subdescfx)),sapply(d[circumbeliefs],descrfx))
colnames(circumbeliefs) <- c(paste0("Block 1 (n = ",nrow(d[d$block==1,]),")"),
                             paste0("Block 2 (n = ",nrow(d[d$block==2,]),")"),
                             paste0("Block 3 (n = ", nrow(d[d$block==3,]),")"),
                             paste0("Overall (n = ", nrow(d),")"))

names <- c("Risk of bleeding if device","Risk of infection if device",
           "Probability of pain during procedure with device",
           "Probability of pain during healing with device",
           "Risk of bleeding if surgery","Risk of infection if surgery",
           "Probability of pain during procedure with surgery",
           "Probability of pain during healing with surgery",
           "Risk of bleeding","Risk of infection",
           "Probability of pain during procedure",
           "Probability of pain during healing",
           "Days to healing if device","Days away from work if device","Days away from sex if device",
           "Days to healing if surgery","Days away from work if surgery","Days away from sex if surgery",
           "Days to healing","Days away from work","Days away from sex")

rownames(circumbeliefs) <- names

save.xlsx("demographics.xlsx",sumtab,beliefs,circumbeliefs)

# table(cut_interval(d$injunctive1, n=5, right=FALSE, labels=c(-2,-1,0,1,2)))

attcols <- c("bleed_device","infection_device", "pain_proc_device", "pain_heal_device",
             "bleed_surgery","infection_surgery", "pain_proc_surgery", "pain_heal_surgery",
              "bleed","infection", "pain_proc","pain_heal")

circumcontbeliefs <- c("time_heal_device_avg","time_work_device_avg","time_sex_device_avg",
                       "time_heal_surgery_avg","time_work_surgery_avg","time_sex_surgery_avg",
                       "time_heal","time_work","time_sex")

d[socvals] <- apply(d[socvals], 2, FUN=Grouper)
d[subcols] <- apply(d[subcols], 2, FUN=Grouper)
d[attcols] <- apply(d[attcols], 2, FUN=Grouper)
d$time_work_device_avg[d$time_work_device_avg==0] = 0.01
d[circumcontbeliefs] <- apply(d[circumcontbeliefs], 2, FUN=log)

apply(d[circumcontbeliefs], 2, FUN=mean)
# if circumciont beliefs == Inf, replace with log

#table(d$bleed)

#d[socvals] <- apply(d[socvals], 2, FUN=function(x) cut(x,breaks=c(0,0.2,0.4,0.6,0.8,1.0),right=TRUE, include.lowest = TRUE, labels=FALSE))
#d[subcols] <- apply(d[subcols], 2, FUN=function(x) cut(x,breaks=c(0,0.2,0.4,0.6,0.8,1.0),right=TRUE, include.lowest = TRUE, labels=FALSE))

N <- nrow(d)           # number of decision makers
alts <- 3              # number of alternatives
tasks <- 12            # choice scenarios per individual

longd <- reshape(d, varying=list(c("choice1","choice2","choice3","choice4","choice5","choice6","choice7","choice8","choice9","choice10","choice11","choice12")),
                 direction="long",idvar="ID",timevar="task",v.names="choice",sep = "")
longd <- longd[order(longd["ID"],longd["task"]),]


# Get the experimental design matrix and replicate it N times (i.e., each person is faced with the same 12 choice scenarios)
desmat <- read.csv("desmat.csv", header=T)    # use for csv files

## --- create dummy variables
desmat <- dummy_cols(select_columns = c("distance1","privacy1","incentive1","distance2","privacy2","incentive2"), desmat, remove_selected_columns=TRUE)

desmat <- plyr::rename(desmat, c("privacy1_0"="veryprivate1", "privacy1_1"="someprivate1", "privacy1_2"="notprivate1",
                           "privacy2_0"="veryprivate2", "privacy2_1"="someprivate2", "privacy2_2"="notprivate2",
                           "incentive1_0"="none1", "incentive1_1"="voucher1", "incentive1_2"="cash1",
                           "incentive2_0"="none2", "incentive2_1"="voucher2", "incentive2_2"="cash2",
                           "method1"="device1","method2"="device2","method3"="device3",
                           "facility1"="permhc1","facility2"="permhc2","facility3"="permhc3",
                           "distance1_1"="distance11","distance1_5"="distance51","distance1_15"="distance151",
                           "distance2_1"="distance12","distance2_5"="distance52","distance2_15"="distance152"))
                      
# remove base category for the categorical variables from the data.frame
# desmat <- subset(desmat, select = -c(veryprivate1,veryprivate2,none1,none2))

m <- merge(desmat, longd, by=c("task", "block"))

m <- m[order(m["ID"],m["task"]),]
m <- arrange.vars(m, c("ID"=1, "block"=2, "task"=3, "choice"=4))

## for stata create stacked dataframe with each line representing a choice (i.e. make dataset even longer)
longm <- reshape(m, varying=list(c("permhc1","permhc2","permhc3"),
                                 c("distance11","distance12","distance13"),
                                 c("distance51","distance52","distance53"),
                                 c("distance151","distance152","distance153"),
                                 c("available1","available2","available3"),
                                 c("veryprivate1","veryprivate2","veryprivate3"),
                                 c("someprivate1","someprivate2","someprivate3"),
                                 c("notprivate1","notprivate2","notprivate3"),
                                 c("device1","device2","device3"),
                                 c("none1","none2","none3"),
                                 c("voucher1","voucher2","voucher3"),
                                 c("cash1","cash2","cash3"),
                                 c("price1","price2","price3")),
                 v.names=c("permhc","distance1","distance5","distance15","available","veryprivate",
                           "someprivate","notprivate","device","none","voucher","cash","price"),
                 timevar="alt",
                 times=c(1,2,3),
                 direction="long")

longm <- longm[order(longm["ID"],longm["task"],longm["alt"]),]

# now identify chosen alternative
longm$chosen <- as.numeric(longm$choice==longm$alt)
longm <- arrange.vars(longm, c("ID"=1, "block"=2, "task"=3, "alt"=4, "choice"=5, "chosen"=6,
                               "permhc"=7,"distance1"=8,"distance5"=9,"distance15"=10,"available"=11,"veryprivate"=12,
                               "someprivate"=13,"notprivate"=14,"device"=15,"none"=16,"voucher"=17,"cash"=18,"price"=19))


# set working directory, allowing me to work on both my windows and mac machines
if (.Platform$OS.type=="windows") {
  write.csv(m,"C:/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mmnl/cleanerdata.csv")
  write.csv(m,"C:/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mnl/cleanerdata.csv")
  write.table(m, file="C:/Users/slubinga/HybridChoice/cleanerdata.dat", row.names=FALSE, sep="\t", quote=FALSE)
  write.csv(longm,"C:/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mnl/cleanerdatamlogit.csv")
  #write.dta(longm, file="C:/Users/slubinga/Documents/StataDissertation/cleanerdata.dta")
} else {
  write.csv(m,"/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mmnl/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mmnl/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/Documents/Dissertation/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/Documents/Dissertation/papers/cmccode/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/Documents/Dissertation/papers/gmnl/cleanerdatamlogit.csv")
  write.csv(m,"/Users/slubinga/Documents/Dissertation/papers/paper3/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/Documents/Dissertation/papers/cmccode/ICLV/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/myPythonFiles/PRJ1/cleanerdata.csv")
  write.csv(m,"/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mnl/cleanerdata.csv")
  write.csv(longm,"/Users/slubinga/Dropbox/Dissertation_A/MMC-Dissertation/Analyses/DCE Designs/mainsurvey/mnl/cleanerdatamlogit.csv")
  write.csv(longm,"/Users/slubinga/Documents/StataDissertation/cleanerdatamlogit.csv")
  write.table(m, file="/Users/slubinga/HybridChoice/cleandata.dat", row.names=FALSE, sep="\t", quote=FALSE)
  #write.dta(longm, file="/Users/slubinga/Documents/StataDissertation/cleanerdata.dta")
}
