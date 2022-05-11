rm(list=ls())

library(apollo)
library(tidyverse)

setwd("/Users/slubinga/Documents/GitHub/MMC-Demand/SEM_Model")

apollo_initialise()

apollo_control = list(modelName ="tbp_mimic_intentions",
                      modelDescr ="TPB MIMIC Model",
                      indivID ="ID",
                      mixing=TRUE,
                      nCores=4)

database = read.csv("../cleanerdata.csv",header=TRUE)

database=subset(database,task==1,select=c(ID,task,last_age,
                                          resp_tribe,muganda,kiga_nkole,musoga,other_tribe,
                                          resp_religion,catholic,protestant,other_relig,
                                          school_level,school_years,noeduc,pri,olev,alev,
                                          marital_status,married,nevermarried,other_marital,
                                          nochild,children_fathered,
                                          urban,
                                          occupation,fisher,trader,farmer,occup_other,
                                          monthly_income,monthly_expenditure,abs_income,
                                          drinking_water_source,wellwater,pipedwater,springwater,surfacewater,bottledwater,
                                          roof,fin_roof,rud_roof,nat_roof,
                                          toilet_facility,toilet,pit_slab,pit_noslab,toilet_share,
                                          electricity,radio,bicycle,car_truck,boat_engine,boat_manual,
                                          land,cattle,sheep_goats,business_shop,house,
                                          health_status,
                                          circum_intent,
                                          sex_first,sex_first_age,sex_last_time_period,sex_last_time,last_time_condom,sex_partner_type,sex_partner_type_other,
                                          friend_circum,family_circum,promot_materials,discuss_partner,partner_rec,hw_rec,friend_rec,family_rec,religlead_rec,villagehead_rec,
                                          trans_no_cd,trans_wi_cd,prev_all,prev_att,
                                          mort_HIV_neg_1,mort_HIV_neg_5,mort_HIV_neg_10,mort_HIV_pos_1,mort_HIV_pos_5,mort_HIV_pos_10,
                                          HIV_tx,mort_HIV_pos_tx_1,mort_HIV_pos_tx_5,mort_HIV_pos_tx_10,
                                          riskpref_15_5,riskpref_5_0,riskpref_10_10,discountrate_5_1_4,discountrate_5_4_10,discountrate_30_1_4,discountrate_30_4_10,
                                          prob_self_HIV,prob_STDs,prob_partner_HIV,prob_partner_CC,prob_ImproveHygiene,prob_ImproveSexLife,
                                          prob_LongerSex,prob_PleasePartner,prob_SexPartner,prob_ReduceSexPleasure,prob_ReduceLibido,prob_Infertile,
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
                                          injunctive1,injunctive2,injunctive3,injunctive4,injunctive5,injunctive6,injunctive7,
                                          descriptive1,descriptive2,descriptive3,descriptive4,
                                          cb1,cb2,cb3,cb4,cb5,cb6,cb7,cb8,cb9,cb10,cb11,cb12,cb13,cb14,cb15,cb16,cb17,cb18,cb19,cb20,cb21,
                                          e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,
                                          conscientiousness1,conscientiousness2,conscientiousness3,conscientiousness4,conscientiousness5,conscientiousness6,conscientiousness7,conscientiousness8,conscientiousness9,
                                          selfconsciousness1,selfconsciousness2,selfconsciousness3,selfconsciousness4,selfconsciousness5,selfconsciousness6,
                                          extraversion1,extraversion2,extraversion3,extraversion4,extraversion5,extraversion6,extraversion7,extraversion8,
                                          loc1,loc2,loc3,loc4,loc5,loc6,
                                          read_news_eng,read_news_lug,
                                          read_letter_eng,read_letter_lug,
                                          read_phone_eng,read_phone_lug,
                                          write_letter_eng,write_letter_lug,
                                          write_phone_eng,write_phone_lug,
                                          numeracy1,numeracy2,numeracy3,numeracy4))


#####################################################################
#####################################################################
# DEFINE MODEL PARAMETERS                                           #
#####################################################################
#####################################################################

# set names for parameters
parnames=c("beta_CONTROL","beta_NORMS","beta_ATTITUDES",
           
           "SOCIALBARS_lambda01","SOCIALBARS_lambda02","SOCIALBARS_lambda03","SOCIALBARS_lambda04",
           "FEARSURGERY_lambda01","FEARSURGERY_lambda02","FEARSURGERY_lambda03","FEARSURGERY_lambda04",
           "PRIVACY_lambda01","PRIVACY_lambda02","PRIVACY_lambda03","PRIVACY_lambda04",
           "TRAVELCOST_lambda01","TRAVELCOST_lambda02","TRAVELCOST_lambda03",
           "REASSURANCE_lambda01","REASSURANCE_lambda02","REASSURANCE_lambda03","REASSURANCE_lambda04","REASSURANCE_lambda05",
           
           "INJUNCTIVE_lambda01","INJUNCTIVE_lambda02","INJUNCTIVE_lambda03","INJUNCTIVE_lambda04","INJUNCTIVE_lambda05","INJUNCTIVE_lambda06","INJUNCTIVE_lambda07",
           "DESCRIPTIVE_lambda01","DESCRIPTIVE_lambda02","DESCRIPTIVE_lambda03","DESCRIPTIVE_lambda04",
           
           "SEXBENEFITS_lambda01","SEXBENEFITS_lambda02","SEXBENEFITS_lambda03","SEXBENEFITS_lambda04",
           "STDPREVENTION_lambda01","STDPREVENTION_lambda02","STDPREVENTION_lambda03","STDPREVENTION_lambda04",
           "NEGATIVE_lambda01","NEGATIVE_lambda02","NEGATIVE_lambda03",
           
           "CONTROL_SOCIALBARS_lambda","CONTROL_FEARSURGERY_lambda","CONTROL_PRIVACY_lambda","CONTROL_TRAVELCOST_lambda","CONTROL_REASSURANCE_lambda",
           "NORMS_INJUNCTIVE_lambda","NORMS_DESCRIPTIVE_lambda",
           "ATTITUDES_SEXBENEFITS_lambda","ATTITUDE_STDPREVENTION_lambda","ATTITUDES_NEGATIVE_lambda",

           "sig_SOCIALBARS",
           "sig_FEARSURGERY",
           "sig_PRIVACY",
           "sig_TRAVELCOST",
           "sig_REASSURANCE",
           "sig_INJUNCTIVE",
           "sig_DESCRIPTIVE",
           "sig_SEXBENEFITS",
           "sig_STDPREVENTION",
           "sig_NEGATIVE",
           "sig_CONTROL",
           "sig_NORMS_CONTROL","sig_NORMS",
           "sig_ATTITUDES_CONTROL","sig_ATTITUDES_NORMS","sig_ATTITUDES",

           # parameters to compute thresholds for the indicators #
           "SOCIALBARS_tau01_1","SOCIALBARS_tau01_2","SOCIALBARS_tau01_3","SOCIALBARS_tau01_4",
           "SOCIALBARS_tau02_1","SOCIALBARS_tau02_2","SOCIALBARS_tau02_3","SOCIALBARS_tau02_4",
           "SOCIALBARS_tau03_1","SOCIALBARS_tau03_2","SOCIALBARS_tau03_3","SOCIALBARS_tau03_4",
           "SOCIALBARS_tau04_1","SOCIALBARS_tau04_2","SOCIALBARS_tau04_3","SOCIALBARS_tau04_4",

           "FEARSURGERY_tau01_1","FEARSURGERY_tau01_2","FEARSURGERY_tau01_3","FEARSURGERY_tau01_4",
           "FEARSURGERY_tau02_1","FEARSURGERY_tau02_2","FEARSURGERY_tau02_3","FEARSURGERY_tau02_4",
           "FEARSURGERY_tau03_1","FEARSURGERY_tau03_2","FEARSURGERY_tau03_3","FEARSURGERY_tau03_4",
           "FEARSURGERY_tau04_1","FEARSURGERY_tau04_2","FEARSURGERY_tau04_3","FEARSURGERY_tau04_4",

           "PRIVACY_tau01_1","PRIVACY_tau01_2","PRIVACY_tau01_3","PRIVACY_tau01_4",
           "PRIVACY_tau02_1","PRIVACY_tau02_2","PRIVACY_tau02_3","PRIVACY_tau02_4",
           "PRIVACY_tau03_1","PRIVACY_tau03_2","PRIVACY_tau03_3","PRIVACY_tau03_4",
           "PRIVACY_tau04_1","PRIVACY_tau04_2","PRIVACY_tau04_3",
           
           "TRAVELCOST_tau01_1","TRAVELCOST_tau01_2","TRAVELCOST_tau01_3","TRAVELCOST_tau01_4",
           "TRAVELCOST_tau02_1","TRAVELCOST_tau02_2","TRAVELCOST_tau02_3","TRAVELCOST_tau02_4",
           "TRAVELCOST_tau03_1","TRAVELCOST_tau03_2","TRAVELCOST_tau03_3","TRAVELCOST_tau03_4",
           
           "REASSURANCE_tau01_1","REASSURANCE_tau01_2","REASSURANCE_tau01_3","REASSURANCE_tau01_4",
           "REASSURANCE_tau02_1","REASSURANCE_tau02_2","REASSURANCE_tau02_3","REASSURANCE_tau02_4",
           "REASSURANCE_tau03_1","REASSURANCE_tau03_2","REASSURANCE_tau03_3","REASSURANCE_tau03_4",
           "REASSURANCE_tau04_1","REASSURANCE_tau04_2","REASSURANCE_tau04_3","REASSURANCE_tau04_4",
           "REASSURANCE_tau05_1","REASSURANCE_tau05_2","REASSURANCE_tau05_3","REASSURANCE_tau05_4",
           
           "INJUNCTIVE_tau01_1","INJUNCTIVE_tau01_2","INJUNCTIVE_tau01_3","INJUNCTIVE_tau01_4",
           "INJUNCTIVE_tau02_1","INJUNCTIVE_tau02_2","INJUNCTIVE_tau02_3","INJUNCTIVE_tau02_4",
           "INJUNCTIVE_tau03_1","INJUNCTIVE_tau03_2","INJUNCTIVE_tau03_3","INJUNCTIVE_tau03_4",
           "INJUNCTIVE_tau04_1","INJUNCTIVE_tau04_2","INJUNCTIVE_tau04_3","INJUNCTIVE_tau04_4",
           "INJUNCTIVE_tau05_1","INJUNCTIVE_tau05_2","INJUNCTIVE_tau05_3","INJUNCTIVE_tau05_4",
           "INJUNCTIVE_tau06_1","INJUNCTIVE_tau06_2","INJUNCTIVE_tau06_3","INJUNCTIVE_tau06_4",
           "INJUNCTIVE_tau07_1","INJUNCTIVE_tau07_2","INJUNCTIVE_tau07_3","INJUNCTIVE_tau07_4",

           "DESCRIPTIVE_tau01_1","DESCRIPTIVE_tau01_2","DESCRIPTIVE_tau01_3","DESCRIPTIVE_tau01_4",
           "DESCRIPTIVE_tau02_1","DESCRIPTIVE_tau02_2","DESCRIPTIVE_tau02_3","DESCRIPTIVE_tau02_4",
           "DESCRIPTIVE_tau03_1","DESCRIPTIVE_tau03_2","DESCRIPTIVE_tau03_3","DESCRIPTIVE_tau03_4",
           "DESCRIPTIVE_tau04_1","DESCRIPTIVE_tau04_2","DESCRIPTIVE_tau04_3","DESCRIPTIVE_tau04_4",
           
           "SEXBENEFITS_tau01_1","SEXBENEFITS_tau01_2","SEXBENEFITS_tau01_3","SEXBENEFITS_tau01_4",
           "SEXBENEFITS_tau02_1","SEXBENEFITS_tau02_2","SEXBENEFITS_tau02_3","SEXBENEFITS_tau02_4",
           "SEXBENEFITS_tau03_1","SEXBENEFITS_tau03_2","SEXBENEFITS_tau03_3","SEXBENEFITS_tau03_4",
           "SEXBENEFITS_tau04_1","SEXBENEFITS_tau04_2","SEXBENEFITS_tau04_3","SEXBENEFITS_tau04_4",
           
           "STDPREVENTION_tau01_1","STDPREVENTION_tau01_2","STDPREVENTION_tau01_3","STDPREVENTION_tau01_4",
           "STDPREVENTION_tau02_1","STDPREVENTION_tau02_2","STDPREVENTION_tau02_3","STDPREVENTION_tau02_4",
           "STDPREVENTION_tau03_1","STDPREVENTION_tau03_2","STDPREVENTION_tau03_3","STDPREVENTION_tau03_4",
           "STDPREVENTION_tau04_1","STDPREVENTION_tau04_2","STDPREVENTION_tau04_3","STDPREVENTION_tau04_4",

           "NEGATIVE_tau01_1","NEGATIVE_tau01_2","NEGATIVE_tau01_3","NEGATIVE_tau01_4",
           "NEGATIVE_tau02_1","NEGATIVE_tau02_2","NEGATIVE_tau02_3","NEGATIVE_tau02_4",
           "NEGATIVE_tau03_1","NEGATIVE_tau03_2","NEGATIVE_tau03_3","NEGATIVE_tau03_4",
           
           "INTENTION_tau01","INTENTION_tau02","INTENTION_tau03","INTENTION_tau04")

# set starting values
startvalues=c(rep(0,3),                      # betas in structural model of utility (of ordered intention variable)
              rep(1,4+4+4+3+5+7+4+4+4+3),    # lambdas in measurement model of indicators (FIRST ORDER EQUATIONS)
              rep(1,5+2+3),                  # lambdas in measurement model of indicators (SECOND ORDER EQUATIONS)
              
              ## ONLY DIAGONALS FOR NOW, CAN ASSIGN WHATEVER CORRELATION STRUCTURE YOU WANT
              0,0,0,0,0,0,0,0,0,1,           # SOCIALBARS, FEARSURGERY, PRIVACY, REASSURANCE, TRAVELCOST, INJUNCTIVE, DESCRIPTIVE, SEXBENEFITS, STDPREVENTION, NEGATIVE
              1,                             # CONTROL
              0,1,                           # NORMS_CONTROL, NORMS
              0,0,1,                         # ATTITUDES_CONTROL,ATTITUDES_NORMS,ATTITUDES
              
              
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of SOCIALBARS
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of FEARSURGERY
              rep(c(-2,-1,1,2),3),            # taus (thresholds) in measurement model of PRIVACY (first 3 indicators)
              rep(c(-2,1,2),1),               # taus (thresholds) in measurement model of PRIVACY (last indicators)
              rep(c(-2,-1,1,2),3),            # taus (thresholds) in measurement model of TRAVELCOST
              rep(c(-2,-1,1,2),5),            # taus (thresholds) in measurement model of REASSURANCE
              rep(c(-2,-1,1,2),7),            # taus (thresholds) in measurement model of INJUNCTIVE
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of DESCRIPTIVE
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of SEXBENEFITS
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of STDPREVENTION
              rep(c(-2,-1,1,2),3),            # taus (thresholds) in measurement model of NEGATIVE

              rep(c(-2,-1,1,2),1)             # taus (thresholds) in measurement model of INTENTION
)

length(startvalues)
length(parnames)

#startvalues = unlist(read.csv("apollo_iclv2_iterations.csv")[54,1:length(startvalues)])

# convert the above into a beta vector with names
apollo_beta=startvalues
names(apollo_beta)=parnames

apollo_fixed = c(
  # fist first loading since we are eatimating variances (for identification)
  "SOCIALBARS_lambda01",
  "FEARSURGERY_lambda01",
  "PRIVACY_lambda01",
  "TRAVELCOST_lambda01",
  "REASSURANCE_lambda01",
  "INJUNCTIVE_lambda01",
  "DESCRIPTIVE_lambda01",
  "SEXBENEFITS_lambda01",
  "STDPREVENTION_lambda01",
  "NEGATIVE_lambda01")

apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 10,
  interUnifDraws = c(),
  interNormDraws = c("eta_c01","eta_c02","eta_c03","eta_c04","eta_c05",
                     "eta_c06","eta_c07",
                     "eta_c08","eta_c09","eta_c10",
                     "eta_c11","eta_c12","eta_c13"),
  intraDrawsType = "mlhs",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  
  # latent variables - random component (with structural equations)
  randcoeff[['SOCIALBARS']]      =      sig_SOCIALBARS        * eta_c01
  randcoeff[['FEARSURGERY']]     =      sig_FEARSURGERY       * eta_c02
  randcoeff[['PRIVACY']]         =      sig_PRIVACY           * eta_c03
  randcoeff[['TRAVELCOST']]      =      sig_TRAVELCOST        * eta_c04
  randcoeff[['REASSURANCE']]     =      sig_REASSURANCE       * eta_c05
  
  randcoeff[['INJUNCTIVE']]      =      sig_INJUNCTIVE        * eta_c06
  randcoeff[['DESCRIPTIVE']]     =      sig_DESCRIPTIVE       * eta_c07

  randcoeff[['SEXBENEFITS']]     =      sig_SEXBENEFITS       * eta_c08
  randcoeff[['STDPREVENTION']]   =      sig_STDPREVENTION     * eta_c09
  randcoeff[['NEGATIVE']]        =      sig_NEGATIVE          * eta_c10
  
  randcoeff[['CONTROL']]         =      sig_CONTROL           * eta_c11
  randcoeff[['NORMS']]           =      sig_NORMS_CONTROL     * eta_c11 + sig_CONTROL         * eta_c12
  randcoeff[['ATTITUDES']]        =     sig_ATTITUDES_CONTROL * eta_c11 + sig_ATTITUDES_NORMS * eta_c12 + sig_ATTITUDES * eta_c13
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach (apollo_beta, apollo_inputs)
  on.exit(apollo_detach (apollo_beta,apollo_inputs))
  
  ### Create list of probabilities P
  P=list()
  
  ## Initialize list for the second order LVs
  LV2 = list()
  
  # FIRST ORDER EQUATIONS

  ### MEASUREMENT MODELS
  #############################
  ## CONTROL BELIEFS
  #############################
  # SOCIALBARS
  ol_settings_SOCIALBARS01 = list(outcomeOrdered=cb17,V=SOCIALBARS_lambda01*SOCIALBARS,tau=c(SOCIALBARS_tau01_1,SOCIALBARS_tau01_2,SOCIALBARS_tau01_3,SOCIALBARS_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_SOCIALBARS02 = list(outcomeOrdered=cb18,V=SOCIALBARS_lambda02*SOCIALBARS,tau=c(SOCIALBARS_tau02_1,SOCIALBARS_tau02_2,SOCIALBARS_tau02_3,SOCIALBARS_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_SOCIALBARS03 = list(outcomeOrdered=cb19,V=SOCIALBARS_lambda03*SOCIALBARS,tau=c(SOCIALBARS_tau03_1,SOCIALBARS_tau03_2,SOCIALBARS_tau03_3,SOCIALBARS_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_SOCIALBARS04 = list(outcomeOrdered=cb21,V=SOCIALBARS_lambda04*SOCIALBARS,tau=c(SOCIALBARS_tau04_1,SOCIALBARS_tau04_2,SOCIALBARS_tau04_3,SOCIALBARS_tau04_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_SOCIALBARS01"]]     = apollo_ol(ol_settings_SOCIALBARS01, functionality)
  P[["indic_SOCIALBARS02"]]     = apollo_ol(ol_settings_SOCIALBARS02, functionality)
  P[["indic_SOCIALBARS03"]]     = apollo_ol(ol_settings_SOCIALBARS03, functionality)
  P[["indic_SOCIALBARS04"]]     = apollo_ol(ol_settings_SOCIALBARS04, functionality)
  
  # FEARSURGERY
  ol_settings_FEARSURGERY01 = list(outcomeOrdered=cb13,V=FEARSURGERY_lambda01*FEARSURGERY,tau=c(FEARSURGERY_tau01_1,FEARSURGERY_tau01_2,FEARSURGERY_tau01_3,FEARSURGERY_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_FEARSURGERY02 = list(outcomeOrdered=cb14,V=FEARSURGERY_lambda02*FEARSURGERY,tau=c(FEARSURGERY_tau02_1,FEARSURGERY_tau02_2,FEARSURGERY_tau02_3,FEARSURGERY_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_FEARSURGERY03 = list(outcomeOrdered=cb15,V=FEARSURGERY_lambda03*FEARSURGERY,tau=c(FEARSURGERY_tau03_1,FEARSURGERY_tau03_2,FEARSURGERY_tau03_3,FEARSURGERY_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_FEARSURGERY04 = list(outcomeOrdered=cb16,V=FEARSURGERY_lambda04*FEARSURGERY,tau=c(FEARSURGERY_tau04_1,FEARSURGERY_tau04_2,FEARSURGERY_tau04_3,FEARSURGERY_tau04_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_FEARSURGERY01"]]     = apollo_ol(ol_settings_FEARSURGERY01, functionality)
  P[["indic_FEARSURGERY02"]]     = apollo_ol(ol_settings_FEARSURGERY02, functionality)
  P[["indic_FEARSURGERY03"]]     = apollo_ol(ol_settings_FEARSURGERY03, functionality)
  P[["indic_FEARSURGERY04"]]     = apollo_ol(ol_settings_FEARSURGERY04, functionality)
  
  # PRIVACY
  ol_settings_PRIVACY01 = list(outcomeOrdered=cb9, V=PRIVACY_lambda01*PRIVACY,tau=c(PRIVACY_tau01_1,PRIVACY_tau01_2,PRIVACY_tau01_3,PRIVACY_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_PRIVACY02 = list(outcomeOrdered=cb11,V=PRIVACY_lambda02*PRIVACY,tau=c(PRIVACY_tau02_1,PRIVACY_tau02_2,PRIVACY_tau02_3,PRIVACY_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_PRIVACY03 = list(outcomeOrdered=cb12,V=PRIVACY_lambda03*PRIVACY,tau=c(PRIVACY_tau03_1,PRIVACY_tau03_2,PRIVACY_tau03_3,PRIVACY_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_PRIVACY04 = list(outcomeOrdered=cb20,V=PRIVACY_lambda04*PRIVACY,tau=c(PRIVACY_tau04_1,PRIVACY_tau04_2,PRIVACY_tau04_3),coding=c(-2,0,1,2))

  P[["indic_PRIVACY01"]]     = apollo_ol(ol_settings_PRIVACY01, functionality)
  P[["indic_PRIVACY02"]]     = apollo_ol(ol_settings_PRIVACY02, functionality)
  P[["indic_PRIVACY03"]]     = apollo_ol(ol_settings_PRIVACY03, functionality)
  P[["indic_PRIVACY04"]]     = apollo_ol(ol_settings_PRIVACY04, functionality)
  
  # REASSURANCE
  ol_settings_REASSURANCE01 = list(outcomeOrdered=cb2, V=REASSURANCE_lambda01*REASSURANCE,tau=c(REASSURANCE_tau01_1,REASSURANCE_tau01_2,REASSURANCE_tau01_3,REASSURANCE_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_REASSURANCE02 = list(outcomeOrdered=cb3, V=REASSURANCE_lambda02*REASSURANCE,tau=c(REASSURANCE_tau02_1,REASSURANCE_tau02_2,REASSURANCE_tau02_3,REASSURANCE_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_REASSURANCE03 = list(outcomeOrdered=cb5, V=REASSURANCE_lambda03*REASSURANCE,tau=c(REASSURANCE_tau03_1,REASSURANCE_tau03_2,REASSURANCE_tau03_3,REASSURANCE_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_REASSURANCE04 = list(outcomeOrdered=cb7, V=REASSURANCE_lambda04*REASSURANCE,tau=c(REASSURANCE_tau04_1,REASSURANCE_tau04_2,REASSURANCE_tau04_3,REASSURANCE_tau04_4),coding=c(-2,-1,0,1,2))
  ol_settings_REASSURANCE05 = list(outcomeOrdered=cb10,V=REASSURANCE_lambda05*REASSURANCE,tau=c(REASSURANCE_tau05_1,REASSURANCE_tau05_2,REASSURANCE_tau05_3,REASSURANCE_tau05_4),coding=c(-2,-1,0,1,2))

  P[["indic_REASSURANCE01"]]     = apollo_ol(ol_settings_REASSURANCE01, functionality)
  P[["indic_REASSURANCE02"]]     = apollo_ol(ol_settings_REASSURANCE02, functionality)
  P[["indic_REASSURANCE03"]]     = apollo_ol(ol_settings_REASSURANCE03, functionality)
  P[["indic_REASSURANCE04"]]     = apollo_ol(ol_settings_REASSURANCE04, functionality)
  P[["indic_REASSURANCE05"]]     = apollo_ol(ol_settings_REASSURANCE05, functionality)
  
  # TRAVELCOST
  ol_settings_TRAVELCOST01 = list(outcomeOrdered=cb1,V=TRAVELCOST_lambda01*TRAVELCOST,tau=c(TRAVELCOST_tau01_1,TRAVELCOST_tau01_2,TRAVELCOST_tau01_3,TRAVELCOST_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_TRAVELCOST02 = list(outcomeOrdered=cb4,V=TRAVELCOST_lambda02*TRAVELCOST,tau=c(TRAVELCOST_tau02_1,TRAVELCOST_tau02_2,TRAVELCOST_tau02_3,TRAVELCOST_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_TRAVELCOST03 = list(outcomeOrdered=cb6,V=TRAVELCOST_lambda03*TRAVELCOST,tau=c(TRAVELCOST_tau03_1,TRAVELCOST_tau03_2,TRAVELCOST_tau03_3,TRAVELCOST_tau03_4),coding=c(-2,-1,0,1,2))

  P[["indic_TRAVELCOST01"]]     = apollo_ol(ol_settings_TRAVELCOST01, functionality)
  P[["indic_TRAVELCOST02"]]     = apollo_ol(ol_settings_TRAVELCOST02, functionality)
  P[["indic_TRAVELCOST03"]]     = apollo_ol(ol_settings_TRAVELCOST03, functionality)

  #############################
  ### SOCIAL NORMS/EXPECTATIONS
  #############################
  # INJUNCTIVE
  ol_settings_INJUNCTIVE01 = list(outcomeOrdered=injunctive1,V=INJUNCTIVE_lambda01*INJUNCTIVE,tau=c(INJUNCTIVE_tau01_1,INJUNCTIVE_tau01_2,INJUNCTIVE_tau01_3,INJUNCTIVE_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_INJUNCTIVE02 = list(outcomeOrdered=injunctive2,V=INJUNCTIVE_lambda02*INJUNCTIVE,tau=c(INJUNCTIVE_tau02_1,INJUNCTIVE_tau02_2,INJUNCTIVE_tau02_3,INJUNCTIVE_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_INJUNCTIVE03 = list(outcomeOrdered=injunctive3,V=INJUNCTIVE_lambda03*INJUNCTIVE,tau=c(INJUNCTIVE_tau03_1,INJUNCTIVE_tau03_2,INJUNCTIVE_tau03_3,INJUNCTIVE_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_INJUNCTIVE04 = list(outcomeOrdered=injunctive4,V=INJUNCTIVE_lambda04*INJUNCTIVE,tau=c(INJUNCTIVE_tau04_1,INJUNCTIVE_tau04_2,INJUNCTIVE_tau04_3,INJUNCTIVE_tau04_4),coding=c(-2,-1,0,1,2))
  ol_settings_INJUNCTIVE05 = list(outcomeOrdered=injunctive5,V=INJUNCTIVE_lambda05*INJUNCTIVE,tau=c(INJUNCTIVE_tau05_1,INJUNCTIVE_tau05_2,INJUNCTIVE_tau05_3,INJUNCTIVE_tau05_4),coding=c(-2,-1,0,1,2))
  ol_settings_INJUNCTIVE06 = list(outcomeOrdered=injunctive6,V=INJUNCTIVE_lambda06*INJUNCTIVE,tau=c(INJUNCTIVE_tau06_1,INJUNCTIVE_tau06_2,INJUNCTIVE_tau06_3,INJUNCTIVE_tau06_4),coding=c(-2,-1,0,1,2))
  ol_settings_INJUNCTIVE07 = list(outcomeOrdered=injunctive7,V=INJUNCTIVE_lambda07*INJUNCTIVE,tau=c(INJUNCTIVE_tau07_1,INJUNCTIVE_tau07_2,INJUNCTIVE_tau07_3,INJUNCTIVE_tau07_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_INJUNCTIVE1"]]    = apollo_ol(ol_settings_INJUNCTIVE01, functionality)
  P[["indic_INJUNCTIVE2"]]    = apollo_ol(ol_settings_INJUNCTIVE02, functionality)
  P[["indic_INJUNCTIVE3"]]    = apollo_ol(ol_settings_INJUNCTIVE03, functionality)
  P[["indic_INJUNCTIVE4"]]    = apollo_ol(ol_settings_INJUNCTIVE04, functionality)
  P[["indic_INJUNCTIVE5"]]    = apollo_ol(ol_settings_INJUNCTIVE05, functionality)
  P[["indic_INJUNCTIVE6"]]    = apollo_ol(ol_settings_INJUNCTIVE06, functionality)
  P[["indic_INJUNCTIVE7"]]    = apollo_ol(ol_settings_INJUNCTIVE07, functionality)
  
  # DESCRIPTIVE
  ol_settings_DESCRIPTIVE01 = list(outcomeOrdered=descriptive1,V=DESCRIPTIVE_lambda01*DESCRIPTIVE,tau=c(DESCRIPTIVE_tau01_1,DESCRIPTIVE_tau01_2,DESCRIPTIVE_tau01_3,DESCRIPTIVE_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_DESCRIPTIVE02 = list(outcomeOrdered=descriptive2,V=DESCRIPTIVE_lambda02*DESCRIPTIVE,tau=c(DESCRIPTIVE_tau02_1,DESCRIPTIVE_tau02_2,DESCRIPTIVE_tau02_3,DESCRIPTIVE_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_DESCRIPTIVE03 = list(outcomeOrdered=descriptive3,V=DESCRIPTIVE_lambda03*DESCRIPTIVE,tau=c(DESCRIPTIVE_tau03_1,DESCRIPTIVE_tau03_2,DESCRIPTIVE_tau03_3,DESCRIPTIVE_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_DESCRIPTIVE04 = list(outcomeOrdered=descriptive4,V=DESCRIPTIVE_lambda04*DESCRIPTIVE,tau=c(DESCRIPTIVE_tau04_1,DESCRIPTIVE_tau04_2,DESCRIPTIVE_tau04_3,DESCRIPTIVE_tau04_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_DESCRIPTIVE1"]]     = apollo_ol(ol_settings_DESCRIPTIVE01, functionality)
  P[["indic_DESCRIPTIVE2"]]     = apollo_ol(ol_settings_DESCRIPTIVE02, functionality)
  P[["indic_DESCRIPTIVE3"]]     = apollo_ol(ol_settings_DESCRIPTIVE03, functionality)
  P[["indic_DESCRIPTIVE4"]]     = apollo_ol(ol_settings_DESCRIPTIVE04, functionality)
  
  #############################
  ### ATTITUDES
  #############################
  # SEX BENEFITS
  ol_settings_SEXBENEFITS01 = list(outcomeOrdered=sub6,V=SEXBENEFITS_lambda01*SEXBENEFITS,tau=c(SEXBENEFITS_tau01_1,SEXBENEFITS_tau01_2,SEXBENEFITS_tau01_3,SEXBENEFITS_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_SEXBENEFITS02 = list(outcomeOrdered=sub7,V=SEXBENEFITS_lambda02*SEXBENEFITS,tau=c(SEXBENEFITS_tau02_1,SEXBENEFITS_tau02_2,SEXBENEFITS_tau02_3,SEXBENEFITS_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_SEXBENEFITS03 = list(outcomeOrdered=sub8,V=SEXBENEFITS_lambda03*SEXBENEFITS,tau=c(SEXBENEFITS_tau03_1,SEXBENEFITS_tau03_2,SEXBENEFITS_tau03_3,SEXBENEFITS_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_SEXBENEFITS04 = list(outcomeOrdered=sub9,V=SEXBENEFITS_lambda04*SEXBENEFITS,tau=c(SEXBENEFITS_tau04_1,SEXBENEFITS_tau04_2,SEXBENEFITS_tau04_3,SEXBENEFITS_tau04_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_SEXBENEFITS1"]]     = apollo_ol(ol_settings_SEXBENEFITS01, functionality)
  P[["indic_SEXBENEFITS2"]]     = apollo_ol(ol_settings_SEXBENEFITS02, functionality)
  P[["indic_SEXBENEFITS3"]]     = apollo_ol(ol_settings_SEXBENEFITS03, functionality)
  P[["indic_SEXBENEFITS4"]]     = apollo_ol(ol_settings_SEXBENEFITS04, functionality)
  
  # STD PREVENTION
  ol_settings_STDPREVENTION01 = list(outcomeOrdered=sub1,V=STDPREVENTION_lambda01*STDPREVENTION,tau=c(STDPREVENTION_tau01_1,STDPREVENTION_tau01_2,STDPREVENTION_tau01_3,STDPREVENTION_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_STDPREVENTION02 = list(outcomeOrdered=sub2,V=STDPREVENTION_lambda02*STDPREVENTION,tau=c(STDPREVENTION_tau02_1,STDPREVENTION_tau02_2,STDPREVENTION_tau02_3,STDPREVENTION_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_STDPREVENTION03 = list(outcomeOrdered=sub3,V=STDPREVENTION_lambda03*STDPREVENTION,tau=c(STDPREVENTION_tau03_1,STDPREVENTION_tau03_2,STDPREVENTION_tau03_3,STDPREVENTION_tau03_4),coding=c(-2,-1,0,1,2))
  ol_settings_STDPREVENTION04 = list(outcomeOrdered=sub5,V=STDPREVENTION_lambda04*STDPREVENTION,tau=c(STDPREVENTION_tau04_1,STDPREVENTION_tau04_2,STDPREVENTION_tau04_3,STDPREVENTION_tau04_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_STDPREVENTION1"]]     = apollo_ol(ol_settings_STDPREVENTION01, functionality)
  P[["indic_STDPREVENTION2"]]     = apollo_ol(ol_settings_STDPREVENTION02, functionality)
  P[["indic_STDPREVENTION3"]]     = apollo_ol(ol_settings_STDPREVENTION03, functionality)
  P[["indic_STDPREVENTION4"]]     = apollo_ol(ol_settings_STDPREVENTION04, functionality)
  
  # NEGATIVE
  ol_settings_NEGATIVE01 = list(outcomeOrdered=sub10,V=NEGATIVE_lambda01*NEGATIVE,tau=c(NEGATIVE_tau01_1,NEGATIVE_tau01_2,NEGATIVE_tau01_3,NEGATIVE_tau01_4),coding=c(-2,-1,0,1,2))
  ol_settings_NEGATIVE02 = list(outcomeOrdered=sub11,V=NEGATIVE_lambda02*NEGATIVE,tau=c(NEGATIVE_tau02_1,NEGATIVE_tau02_2,NEGATIVE_tau02_3,NEGATIVE_tau02_4),coding=c(-2,-1,0,1,2))
  ol_settings_NEGATIVE03 = list(outcomeOrdered=sub12,V=NEGATIVE_lambda03*NEGATIVE,tau=c(NEGATIVE_tau03_1,NEGATIVE_tau03_2,NEGATIVE_tau03_3,NEGATIVE_tau03_4),coding=c(-2,-1,0,1,2))
  
  P[["indic_NEGATIVE1"]]     = apollo_ol(ol_settings_NEGATIVE01, functionality)
  P[["indic_NEGATIVE2"]]     = apollo_ol(ol_settings_NEGATIVE02, functionality)
  P[["indic_NEGATIVE3"]]     = apollo_ol(ol_settings_NEGATIVE03, functionality)
  
  ##########################################################
  # SECOND ORDER FACTOR EQUATIONS
  ##########################################################
  
  # STRUCTURAL EQUATIONS
  
  
  # MEASUREMENT EQUATIONS
  # CONTROL
  normalDensity_settings_CONTROL_SOCIALBARS = list(outcomeNormal = SOCIALBARS, 
                                                   xNormal       = CONTROL_SOCIALBARS_lambda*CONTROL, 
                                                   mu            = 0, 
                                                   sigma         = sig_SOCIALBARS, 
                                                   componentName = "indic_CONTROL_SOCIALBARS")
  normalDensity_settings_CONTROL_FEARSURGERY = list(outcomeNormal = FEARSURGERY, 
                                                    xNormal       = CONTROL_FEARSURGERY_lambda*CONTROL, 
                                                    mu            = 0, 
                                                    sigma         = sig_FEARSURGERY, 
                                                    componentName = "indic_CONTROL_FEARSURGERY")
  normalDensity_settings_CONTROL_PRIVACY = list(outcomeNormal = PRIVACY, 
                                                xNormal       = CONTROL_PRIVACY_lambda*CONTROL, 
                                                mu            = 0, 
                                                sigma         = sig_PRIVACY, 
                                                componentName = "indic_CONTROL_PRIVACY")
  normalDensity_settings_CONTROL_TRAVELCOST = list(outcomeNormal = TRAVELCOST, 
                                                   xNormal       = CONTROL_TRAVELCOST_lambda*CONTROL, 
                                                   mu            = 0, 
                                                   sigma         = sig_TRAVELCOST, 
                                                   componentName = "indic_CONTROL_TRAVELCOST")
  normalDensity_settings_CONTROL_REASSURANCE = list(outcomeNormal = REASSURANCE, 
                                                    xNormal       = CONTROL_REASSURANCE_lambda*CONTROL, 
                                                    mu            = 0, 
                                                    sigma         = sig_REASSURANCE, 
                                                    componentName = "indic_CONTROL_REASSURANCE")
  
  
  P[["indic_CONTROL_SOCIALBARS"]]   = apollo_normalDensity(normalDensity_settings_CONTROL_SOCIALBARS, functionality)
  P[["indic_CONTROL_FEARSURGERY"]]  = apollo_normalDensity(normalDensity_settings_CONTROL_FEARSURGERY, functionality)
  P[["indic_CONTROL_PRIVACY"]]      = apollo_normalDensity(normalDensity_settings_CONTROL_PRIVACY, functionality)
  P[["indic_CONTROL_TRAVELCOST"]]   = apollo_normalDensity(normalDensity_settings_CONTROL_TRAVELCOST, functionality)
  P[["indic_CONTROL_REASSURANCE"]]  = apollo_normalDensity(normalDensity_settings_CONTROL_REASSURANCE, functionality)
  
  # NORMS
  normalDensity_settings_NORMS_INJUNCTIVE = list(outcomeNormal = INJUNCTIVE-mean(INJUNCTIVE), 
                                                   xNormal       = NORMS_INJUNCTIVE_lambda*NORMS, 
                                                   mu            = 0, 
                                                   sigma         = sig_INJUNCTIVE, 
                                                   componentName = "indic_NORMS_INJUNCTIVE")
  normalDensity_settings_NORMS_DESCRIPTIVE = list(outcomeNormal = DESCRIPTIVE-mean(DESCRIPTIVE), 
                                                    xNormal       = NORMS_DESCRIPTIVE_lambda*NORMS, 
                                                    mu            = 0, 
                                                    sigma         = sig_DESCRIPTIVE, 
                                                    componentName = "indic_NORMS_DESCRIPTIVE")
  
  P[["indic_NORMS_INJUNCTIVE"]]   = apollo_normalDensity(normalDensity_settings_NORMS_INJUNCTIVE, functionality)
  P[["indic_NORMS_DESCRIPTIVE"]]  = apollo_normalDensity(normalDensity_settings_NORMS_DESCRIPTIVE, functionality)

  # ATTITUDES
  normalDensity_settings_ATTITUDES_SEXBENEFITS = list(outcomeNormal = SEXBENEFITS-mean(SEXBENEFITS), 
                                                   xNormal       = ATTITUDES_SEXBENEFITS_lambda*ATTITUDES, 
                                                   mu            = 0, 
                                                   sigma         = sig_SEXBENEFITS, 
                                                   componentName = "indic_ATTITUDES_SEXBENEFITS")
  normalDensity_settings_ATTITUDES_STDPREVENTION = list(outcomeNormal = STDPREVENTION-mean(STDPREVENTION), 
                                                    xNormal       = ATTITUDES_STDPREVENTION_lambda*ATTITUDES, 
                                                    mu            = 0, 
                                                    sigma         = sig_STDPREVENTION, 
                                                    componentName = "indic_ATTITUDES_STDPREVENTION")
  normalDensity_settings_ATTITUDES_NEGATIVE = list(outcomeNormal = NEGATIVE-mean(NEGATIVE), 
                                                xNormal       = ATTITUDES_NEGATIVE_lambda*ATTITUDES, 
                                                mu            = 0, 
                                                sigma         = sig_NEGATIVE, 
                                                componentName = "indic_ATTITUDES_NEGATIVE")

  P[["indic_ATTITUDES_SEXBENEFITS"]]    = apollo_normalDensity(normalDensity_settings_ATTITUDES_SEXBENEFITS, functionality)
  P[["indic_ATTITUDES_STDPREVENTION"]]  = apollo_normalDensity(normalDensity_settings_ATTITUDES_STDPREVENTION, functionality)
  P[["indic_ATTITUDES_NEGATIVE"]]       = apollo_normalDensity(normalDensity_settings_ATTITUDES_NEGATIVE, functionality)

  
  ####################################################################################################################
  # UTILITY EQUATIONS
  ####################################################################################################################
  
  ## STRUCTURAL EQUATION FOR UTILITY/INTENTION
  INTENTION = beta_CONTROL*CONTROL + beta_NORMS*NORMS + beta_ATTITUDES*ATTITUDES
  
  # MEASUREMENT MODEL FOR INTENTION
  ol_settings_INTENTION = list(outcomeOrdered=circum_intent,V=INTENTION,tau=c(INTENTION_tau01,INTENTION_tau02,INTENTION_tau03,INTENTION_tau04),coding=c(-2,-1,0,1,2))
  P[["choice"]] = apollo_ol(ol_settings_INTENTION,functionality)

  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs , functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional: calculate LL before model estimation
#apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

#x=apollo_probabilities(apollo_beta, apollo_inputs, functionality="estimate")

### increase iterations, complex model
estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       #hessianRoutine="numDeriv",
                       hessianRoutine="maxLik")

### Search for good starting
#apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings)

modelOutput_settings = list(printPVal=TRUE)
saveOutput_settings = list(printPVal=TRUE)

apollo_modelOutput(model, modelOutput_settings)
apollo_saveOutput(model, saveOutput_settings)
