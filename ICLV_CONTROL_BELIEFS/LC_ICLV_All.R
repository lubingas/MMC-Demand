rm(list=ls())

library(apollo)

setwd("/Users/slubinga/Documents/Dissertation/papers/paper3")

apollo_initialise ()

apollo_control = list(modelName ="apollo_All_ICLV_LC_3_Classes",
                      modelDescr ="ICLV All LV in LC Model with 3 classes",
                      indivID ="ID",
                      mixing =TRUE,
                      nCores =4)

database = read.csv("cleandata.csv",header=TRUE)

# interaction terms
database$pricenone1=database$price1*(database$none1==1)
database$pricenone2=database$price2*(database$none2==1)

database$pricevoucher1=database$price1*(database$voucher1==1)
database$pricevoucher2=database$price2*(database$voucher2==1)

database$pricecash1=database$price1*(database$cash1==1)
database$pricecash2=database$price2*(database$cash2==1)
#database$cb20=factor(database$cb20,levels=c(-2,-1,0,1,2))

## set poverty line in $USD
m0 = 60
database$ispoor         = as.numeric(database$scaled_income < m0)
database$educ_primary  = (database$school_years > 7)
database$age_35_more    = (database$last_age > 35)


#####################################################################
#####################################################################
# DEFINE MODEL PARAMETERS                                           #
#####################################################################
#####################################################################

# set names for parameters
parnames=c("neither_C1","permhc_C1","dist5_C1","dist15_C1","avail_C1","somepriv_C1","notpriv_C1","device_C1","voucher_C1","cash_C1","pricenone_C1","pricevoucher_C1","pricecash_C1",
           "neither_C2","permhc_C2","dist5_C2","dist15_C2","avail_C2","somepriv_C2","notpriv_C2","device_C2","voucher_C2","cash_C2","pricenone_C2","pricevoucher_C2","pricecash_C2",
           "neither_C3","permhc_C3","dist5_C3","dist15_C3","avail_C3","somepriv_C3","notpriv_C3","device_C3","voucher_C3","cash_C3","pricenone_C3","pricevoucher_C3","pricecash_C3",
           "int_C2","int_C3",
           "age_C2","age_C3",
           "educ_C2","educ_C3",
           "ispoor_C2","ispoor_C3",
           "urban_C2","urban_C3",
           "tau_factor1_C2","tau_factor1_C3",
           "tau_factor2_C2","tau_factor2_C3",
           "tau_factor3_C2","tau_factor3_C3",
           "tau_factor4_C2","tau_factor4_C3",
           "tau_factor5_C2","tau_factor5_C3",
           "factor1_age","factor1_educ","factor1_ispoor","factor1_urban",
           "factor2_age","factor2_educ","factor2_ispoor","factor2_urban",
           "factor3_age","factor3_educ","factor3_ispoor","factor3_urban",
           "factor4_age","factor4_educ","factor4_ispoor","factor4_urban",
           "factor5_age","factor5_educ","factor5_ispoor","factor5_urban",
           "factor1_lambda01","factor1_lambda02","factor1_lambda03","factor1_lambda04",
           "factor2_lambda01","factor2_lambda02","factor2_lambda03","factor2_lambda04",
           "factor3_lambda01","factor3_lambda02","factor3_lambda03",
           "factor4_lambda01","factor4_lambda02","factor4_lambda03","factor4_lambda04",
           "factor5_lambda01","factor5_lambda02","factor5_lambda03",
           # parameters to compute thresholds for the indicators # LV1
           "factor1_tau01_1","factor1_tau01_2","factor1_tau01_3","factor1_tau01_4",
           "factor1_tau02_1","factor1_tau02_2","factor1_tau02_3","factor1_tau02_4",
           "factor1_tau03_1","factor1_tau03_2","factor1_tau03_3","factor1_tau03_4",
           "factor1_tau04_1","factor1_tau04_2","factor1_tau04_3","factor1_tau04_4",
           # parameters to compute thresholds for the indicators # LV2
           "factor2_tau01_1","factor2_tau01_2","factor2_tau01_3","factor2_tau01_4",
           "factor2_tau02_1","factor2_tau02_2","factor2_tau02_3","factor2_tau02_4",
           "factor2_tau03_1","factor2_tau03_2","factor2_tau03_3","factor2_tau03_4",
           "factor2_tau04_1","factor2_tau04_2","factor2_tau04_3","factor2_tau04_4",
           # parameters to compute thresholds for the indicators # LV3
           "factor3_tau01_1","factor3_tau01_2","factor3_tau01_3","factor3_tau01_4",
           "factor3_tau02_1","factor3_tau02_2","factor3_tau02_3","factor3_tau02_4",
           "factor3_tau03_1","factor3_tau03_2","factor3_tau03_3",
           # parameters to compute thresholds for the indicators # LV4
           "factor4_tau01_1","factor4_tau01_2","factor4_tau01_3","factor4_tau01_4",
           "factor4_tau02_1","factor4_tau02_2","factor4_tau02_3","factor4_tau02_4",
           "factor4_tau03_1","factor4_tau03_2","factor4_tau03_3","factor4_tau03_4",
           "factor4_tau04_1","factor4_tau04_2","factor4_tau04_3","factor4_tau04_4",
           # parameters to compute thresholds for the indicators # LV5
           "factor5_tau01_1","factor5_tau01_2","factor5_tau01_3","factor5_tau01_4",
           "factor5_tau02_1","factor5_tau02_2","factor5_tau02_3","factor5_tau02_4",
           "factor5_tau03_1","factor5_tau03_2","factor5_tau03_3","factor5_tau03_4")

#startvalues=rep(0,length(parnames))

# set starting values
startvalues=read.csv("lc_3_classes_estimates.csv")[,5]
startvalues=c(startvalues,
              rep(0,10),
              rep(1,20),
              rep(1,18),
              rep(c(-2,-1,1,2),4),
              rep(c(-2,-1,1,2),4),
              rep(c(-2,-1,1,2),2),
              c(-1,0,1),
              rep(c(-2,-1,1,2),4),
              rep(c(-2,-1,1,2),3))

# convert the above into a beta vector with names
apollo_beta=startvalues
names(apollo_beta)=parnames

length(parnames)
length(apollo_beta)

apollo_fixed = c()


#### DEFINE RANDOM COMPONENTS for LV
### set parameters for generating drawss 
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 50,
  interUnifDraws = c(),
  interNormDraws = c("eta01","eta02","eta03","eta04","eta05"), # latent variable disturbances ~N(0,1), assume uncorrelated
  intraDrawsType = "mlhs",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  
  # latent variables - random
  randcoeff[['LV1']]       =      factor1_age*age_35_more+factor1_educ*educ_primary+factor1_urban*urban+factor1_ispoor*ispoor+eta01
  randcoeff[['LV2']]       =      factor2_age*age_35_more+factor2_educ*educ_primary+factor2_urban*urban+factor2_ispoor*ispoor+eta02
  randcoeff[['LV3']]       =      factor3_age*age_35_more+factor3_educ*educ_primary+factor3_urban*urban+factor3_ispoor*ispoor+eta03
  randcoeff[['LV4']]       =      factor4_age*age_35_more+factor4_educ*educ_primary+factor4_urban*urban+factor4_ispoor*ispoor+eta04
  randcoeff[['LV5']]       =      factor5_age*age_35_more+factor5_educ*educ_primary+factor5_urban*urban+factor5_ispoor*ispoor+eta05

  return(randcoeff)
}


#### DEFINE LATENT CLASS COMPONENTS                              ####

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["neither"]]         = list(neither_C1, neither_C2, neither_C3)
  lcpars[["permhc"]]          = list(permhc_C1, permhc_C2, permhc_C3)
  lcpars[["dist5"]]           = list(dist5_C1, dist5_C2, dist5_C3)
  lcpars[["dist15"]]          = list(dist15_C1, dist15_C2, dist15_C3)
  lcpars[["avail"]]           = list(avail_C1, avail_C2, avail_C3)
  lcpars[["somepriv"]]        = list(somepriv_C1, somepriv_C2, somepriv_C3)
  lcpars[["notpriv"]]         = list(notpriv_C1, notpriv_C2, notpriv_C3)
  lcpars[["device"]]          = list(device_C1, device_C2, device_C3)
  lcpars[["voucher"]]         = list(voucher_C1, voucher_C2, voucher_C3)
  lcpars[["cash"]]            = list(cash_C1, cash_C2, cash_C3)
  lcpars[["pricenone"]]       = list(pricenone_C1, pricenone_C2, pricenone_C3)
  lcpars[["pricevoucher"]]    = list(pricevoucher_C1, pricevoucher_C2, pricevoucher_C3)
  lcpars[["pricecash"]]       = list(pricecash_C1, pricecash_C2, pricecash_C3)
  
  V=list()
  V[["class_C1"]] = 0
  V[["class_C2"]] = int_C2+age_C2*age_35_more+educ_C2*educ_primary+urban_C2*urban+ispoor_C2*ispoor+tau_factor1_C2*LV1+tau_factor2_C2*LV2+tau_factor3_C2*LV3+tau_factor4_C2*LV4+tau_factor5_C2*LV5
  V[["class_C3"]] = int_C3+age_C3*age_35_more+educ_C3*educ_primary+urban_C3*urban+ispoor_C3*ispoor+tau_factor1_C3*LV1+tau_factor2_C3*LV2+tau_factor3_C3*LV3+tau_factor4_C3*LV4+tau_factor5_C3*LV5
  
  mnl_settings = list(
    alternatives = c(class_C1=1, class_C2=2, class_C3=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

#### GROUP AND VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()


#### DEFINE MODEL AND LIKELIHOOD FUNCTION
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach (apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta,apollo_inputs))
  
  ### Create list of probabilities P
  # 1. choice and indicators
  P=list()
  # class allocation
  Pinclass=list()
  
  ### Likelihood of indicators
  # settings for latent variable 1: SOCIAL BARRIERS
  ol_settings01_1 = list(outcomeOrdered=cb17,V=factor1_lambda01*LV1,tau=c(factor1_tau01_1,factor1_tau01_2,factor1_tau01_3,factor1_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_1 = list(outcomeOrdered=cb18,V=factor1_lambda02*LV1,tau=c(factor1_tau02_1,factor1_tau02_2,factor1_tau02_3,factor1_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_1 = list(outcomeOrdered=cb19,V=factor1_lambda03*LV1,tau=c(factor1_tau03_1,factor1_tau03_2,factor1_tau03_3,factor1_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_1 = list(outcomeOrdered=cb21,V=factor1_lambda04*LV1,tau=c(factor1_tau04_1,factor1_tau04_2,factor1_tau04_3,factor1_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))

  P[["indic_cb17"]]     = apollo_ol(ol_settings01_1, functionality)
  P[["indic_cb18"]]     = apollo_ol(ol_settings02_1, functionality)
  P[["indic_cb19"]]     = apollo_ol(ol_settings03_1, functionality)
  P[["indic_cb21"]]     = apollo_ol(ol_settings04_1, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb17"]] = apollo_panelProd(P[["indic_cb17"]], apollo_inputs ,functionality)
  P[["indic_cb18"]] = apollo_panelProd(P[["indic_cb18"]], apollo_inputs ,functionality)
  P[["indic_cb19"]] = apollo_panelProd(P[["indic_cb19"]], apollo_inputs ,functionality)
  P[["indic_cb21"]] = apollo_panelProd(P[["indic_cb21"]], apollo_inputs ,functionality)
  
  # settings for latent variable 2: FEAR OF SURGICAL PROCEDURES
  ol_settings01_2 = list(outcomeOrdered=cb13,V=factor2_lambda01*LV2,tau=c(factor2_tau01_1,factor2_tau01_2,factor2_tau01_3,factor2_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_2 = list(outcomeOrdered=cb14,V=factor2_lambda02*LV2,tau=c(factor2_tau02_1,factor2_tau02_2,factor2_tau02_3,factor2_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_2 = list(outcomeOrdered=cb15,V=factor2_lambda03*LV2,tau=c(factor2_tau03_1,factor2_tau03_2,factor2_tau03_3,factor2_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_2 = list(outcomeOrdered=cb16,V=factor2_lambda04*LV2,tau=c(factor2_tau04_1,factor2_tau04_2,factor2_tau04_3,factor2_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb13"]]     = apollo_ol(ol_settings01_2, functionality)
  P[["indic_cb14"]]     = apollo_ol(ol_settings02_2, functionality)
  P[["indic_cb15"]]     = apollo_ol(ol_settings03_2, functionality)
  P[["indic_cb16"]]     = apollo_ol(ol_settings04_2, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb13"]] = apollo_panelProd(P[["indic_cb13"]], apollo_inputs ,functionality)
  P[["indic_cb14"]] = apollo_panelProd(P[["indic_cb14"]], apollo_inputs ,functionality)
  P[["indic_cb15"]] = apollo_panelProd(P[["indic_cb15"]], apollo_inputs ,functionality)
  P[["indic_cb16"]] = apollo_panelProd(P[["indic_cb16"]], apollo_inputs ,functionality)
  
  # settings for latent variable 3: INCENTIVE RESPONSIVE
  ol_settings01_3 = list(outcomeOrdered=cb9,V=factor3_lambda01*LV3,tau=c(factor3_tau01_1,factor3_tau01_2,factor3_tau01_3,factor3_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_3 = list(outcomeOrdered=cb12,V=factor3_lambda02*LV3,tau=c(factor3_tau02_1,factor3_tau02_2,factor3_tau02_3,factor3_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_3 = list(outcomeOrdered=cb20,V=factor3_lambda03*LV3,tau=c(factor3_tau03_1,factor3_tau03_2,factor3_tau03_3),coding=c(-2,0,1,2),rows=(task==1))

  P[["indic_cb9"]]     = apollo_ol(ol_settings01_3, functionality)
  P[["indic_cb12"]]     = apollo_ol(ol_settings02_3, functionality)
  P[["indic_cb20"]]     = apollo_ol(ol_settings03_3, functionality)

  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb9"]] = apollo_panelProd(P[["indic_cb9"]], apollo_inputs ,functionality)
  P[["indic_cb12"]] = apollo_panelProd(P[["indic_cb12"]], apollo_inputs ,functionality)
  P[["indic_cb20"]] = apollo_panelProd(P[["indic_cb20"]], apollo_inputs ,functionality)
  
  # settings for latent variable 4: PRIVACY CONCERN
  ol_settings01_4 = list(outcomeOrdered=cb3,V=factor4_lambda01*LV4,tau=c(factor4_tau01_1,factor4_tau01_2,factor4_tau01_3,factor4_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_4 = list(outcomeOrdered=cb5,V=factor4_lambda02*LV4,tau=c(factor4_tau02_1,factor4_tau02_2,factor4_tau02_3,factor4_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_4 = list(outcomeOrdered=cb7,V=factor4_lambda03*LV4,tau=c(factor4_tau03_1,factor4_tau03_2,factor4_tau03_3,factor4_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_4 = list(outcomeOrdered=cb10,V=factor4_lambda04*LV4,tau=c(factor4_tau04_1,factor4_tau04_2,factor4_tau04_3,factor4_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb3"]]     = apollo_ol(ol_settings01_4, functionality)
  P[["indic_cb5"]]     = apollo_ol(ol_settings02_4, functionality)
  P[["indic_cb7"]]     = apollo_ol(ol_settings03_4, functionality)
  P[["indic_cb10"]]     = apollo_ol(ol_settings04_4, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb3"]] = apollo_panelProd(P[["indic_cb3"]], apollo_inputs ,functionality)
  P[["indic_cb5"]] = apollo_panelProd(P[["indic_cb5"]], apollo_inputs ,functionality)
  P[["indic_cb7"]] = apollo_panelProd(P[["indic_cb7"]], apollo_inputs ,functionality)
  P[["indic_cb10"]] = apollo_panelProd(P[["indic_cb10"]], apollo_inputs ,functionality)
  
  # settings for latent variable 5: INCENTIVE RESPONSIVE
  ol_settings01_5 = list(outcomeOrdered=cb1,V=factor5_lambda01*LV5,tau=c(factor5_tau01_1,factor5_tau01_2,factor5_tau01_3,factor5_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_5 = list(outcomeOrdered=cb4,V=factor5_lambda02*LV5,tau=c(factor5_tau02_1,factor5_tau02_2,factor5_tau02_3,factor5_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_5 = list(outcomeOrdered=cb6,V=factor5_lambda03*LV5,tau=c(factor5_tau03_1,factor5_tau03_2,factor5_tau03_3,factor5_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb1"]]     = apollo_ol(ol_settings01_5, functionality)
  P[["indic_cb4"]]     = apollo_ol(ol_settings02_5, functionality)
  P[["indic_cb6"]]     = apollo_ol(ol_settings03_5, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb1"]] = apollo_panelProd(P[["indic_cb1"]], apollo_inputs ,functionality)
  P[["indic_cb4"]] = apollo_panelProd(P[["indic_cb4"]], apollo_inputs ,functionality)
  P[["indic_cb6"]] = apollo_panelProd(P[["indic_cb6"]], apollo_inputs ,functionality)
  
  

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(opt1=1, opt2=2, neither=3),
    avail = list(opt1=1, opt2=1, neither=1),
    choiceVar = choice)
  
  ### Loop over classes
  s=1
  while(s<=3){
    ### Compute class-specific utilities
    V=list()
    V[['opt1']] = permhc[[s]]*permhc1+dist5[[s]]*distance51+dist15[[s]]*distance151+avail[[s]]*available1+device[[s]]*device1+somepriv[[s]]*someprivate1+notpriv[[s]]*notprivate1+voucher[[s]]*voucher1+cash[[s]]*cash1+pricenone[[s]]*pricenone1+pricevoucher[[s]]*pricevoucher1+pricecash[[s]]*pricecash1
    V[['opt2']] = permhc[[s]]*permhc2+dist5[[s]]*distance52+dist15[[s]]*distance152+avail[[s]]*available2+device[[s]]*device2+somepriv[[s]]*someprivate2+notpriv[[s]]*notprivate2+voucher[[s]]*voucher2+cash[[s]]*cash2+pricenone[[s]]*pricenone2+pricevoucher[[s]]*pricevoucher2+pricecash[[s]]*pricecash2
    V[['neither']] = neither[[s]]
    
    mnl_settings$V = V
    
    ### Compute within-class choice probabilities using MNL model
    Pinclass[[s]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    Pinclass[[s]] = apollo_panelProd(Pinclass[[s]], apollo_inputs, functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = Pinclass, classProb=pi_values)
  P[["choice"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws in class allocation probabilities
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### CHECK FOR COMPUTATIONAL REQUIREMENTS                         ####
# ################################################################# #

speedTest_settings=list(
  nDrawsTry = c(250, 500, 1000),
  nCoresTry = 1:4,
  nRep      = 10
)

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
# search speedtest
apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

# search starting values
apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

# estimate settings
estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       hessianRoutine="numDeriv")

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings)

apollo_modelOutput(model)
