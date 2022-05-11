rm(list=ls())

library(apollo)

setwd("/Users/slubinga/MMC-Demand/ICLV_CONTROL_BELIEFS/")

apollo_initialise()

apollo_control = list(modelName ="ICLV_MNL_CONTROL_BELIEFS",
                      modelDescr ="ICLV MNL WITH CONTROL BELIEFS",
                      indivID ="ID",
                      mixing =TRUE,
                      nCores =4)

database = read.csv("../cleanerdata.csv",header=TRUE)

# interaction terms
database$pricenone1=database$price1*(database$none1==1)
database$pricenone2=database$price2*(database$none2==1)

database$pricevoucher1=database$price1*(database$voucher1==1)
database$pricevoucher2=database$price2*(database$voucher2==1)

database$pricecash1=database$price1*(database$cash1==1)
database$pricecash2=database$price2*(database$cash2==1)

#####################################################################
#####################################################################
# DEFINE MODEL PARAMETERS                                           #
#####################################################################
#####################################################################

# set names for parameters
parnames=c("neither","permhc","dist5","dist15","avail","somepriv","notpriv","device","voucher","cash","pricenone","pricevoucher","pricecash",
           
           # interactions with MNL model parameters
           "neither_SOCIALBARS","permhc_SOCIALBARS","dist5_SOCIALBARS","dist15_SOCIALBARS",
           "neither_FEARSURGERY","permhc_FEARSURGERY","dist5_FEARSURGERY","dist15_FEARSURGERY",
           "neither_PRIVACY","permhc_PRIVACY","dist5_PRIVACY","dist15_PRIVACY",
           "neither_TRAVELCOST","permhc_TRAVELCOST","dist5_TRAVELCOST","dist15_TRAVELCOST",
           "neither_REASSURANCE","permhc_REASSURANCE","dist5_REASSURANCE","dist15_REASSURANCE",

           # lambdas for indicators
           "SOCIALBARS_lambda01","SOCIALBARS_lambda02","SOCIALBARS_lambda03","SOCIALBARS_lambda04",
           "FEARSURGERY_lambda01","FEARSURGERY_lambda02","FEARSURGERY_lambda03","FEARSURGERY_lambda04",
           "PRIVACY_lambda01","PRIVACY_lambda02","PRIVACY_lambda03","PRIVACY_lambda04",
           "TRAVELCOST_lambda01","TRAVELCOST_lambda02","TRAVELCOST_lambda03",
           "REASSURANCE_lambda01","REASSURANCE_lambda02","REASSURANCE_lambda03","REASSURANCE_lambda04","REASSURANCE_lambda05",
           
           # SIGMAS
           "sig_SOCIALBARS","sig_FEARSURGERY","sig_PRIVACY","sig_TRAVELCOST","sig_REASSURANCE",
           
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
           "REASSURANCE_tau05_1","REASSURANCE_tau05_2","REASSURANCE_tau05_3","REASSURANCE_tau05_4")

#startvalues=rep(0,length(parnames))

# set starting values
startvalues=c(rep(0,13),                      # betas for MNL model
              rep(0,5*4),                     # betas for interaction terms of LVs and MNL attributes
              rep(1,4+4+4+3+5),               # lambdas in measurement model of indicators

              ## STANDARD DEVIATION FOR LATENT VARIABLES, ONLY DIAGONALS FOR NOW, CAN ASSIGN WHATEVER CORRELATION STRUCTURE YOU WANT
              1,1,1,1,1,                      # SOCIALBARS, FEARSURGERY, PRIVACY, REASSURANCE, TRAVELCOST
              
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of SOCIALBARS
              rep(c(-2,-1,1,2),4),            # taus (thresholds) in measurement model of FEARSURGERY
              rep(c(-2,-1,1,2),3),            # taus (thresholds) in measurement model of PRIVACY (first 3 indicators)
              rep(c(-2,1,2),1),               # taus (thresholds) in measurement model of PRIVACY (last indicators)
              rep(c(-2,-1,1,2),3),            # taus (thresholds) in measurement model of TRAVELCOST
              rep(c(-2,-1,1,2),5))            # taus (thresholds) in measurement model of REASSURANCE

# convert the above into a beta vector with names
apollo_beta=startvalues
names(apollo_beta)=parnames

length(parnames)
length(apollo_beta)

apollo_fixed = c("sig_SOCIALBARS","sig_FEARSURGERY","sig_PRIVACY","sig_TRAVELCOST","sig_REASSURANCE")


#### DEFINE RANDOM COMPONENTS for LV
### set parameters for generating drawss 
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 20,
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
  
  # latent variables - random, allow for correlation - no for now?
  # latent variables - random component (with structural equations)
  randcoeff[['SOCIALBARS']]      =      sig_SOCIALBARS        * eta01
  randcoeff[['FEARSURGERY']]     =      sig_FEARSURGERY       * eta02
  randcoeff[['PRIVACY']]         =      sig_PRIVACY           * eta03
  randcoeff[['TRAVELCOST']]      =      sig_TRAVELCOST        * eta04
  randcoeff[['REASSURANCE']]     =      sig_REASSURANCE       * eta05
  
  return(randcoeff)
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

  ### Likelihood of indicators
  # settings for latent variable 1: SOCIAL BARRIERS
  ol_settings01_1 = list(outcomeOrdered=cb17,V=SOCIALBARS_lambda01*SOCIALBARS,tau=c(SOCIALBARS_tau01_1,SOCIALBARS_tau01_2,SOCIALBARS_tau01_3,SOCIALBARS_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_1 = list(outcomeOrdered=cb18,V=SOCIALBARS_lambda02*SOCIALBARS,tau=c(SOCIALBARS_tau02_1,SOCIALBARS_tau02_2,SOCIALBARS_tau02_3,SOCIALBARS_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_1 = list(outcomeOrdered=cb19,V=SOCIALBARS_lambda03*SOCIALBARS,tau=c(SOCIALBARS_tau03_1,SOCIALBARS_tau03_2,SOCIALBARS_tau03_3,SOCIALBARS_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_1 = list(outcomeOrdered=cb21,V=SOCIALBARS_lambda04*SOCIALBARS,tau=c(SOCIALBARS_tau04_1,SOCIALBARS_tau04_2,SOCIALBARS_tau04_3,SOCIALBARS_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))

  P[["indic_cb17"]]     = apollo_ol(ol_settings01_1, functionality)
  P[["indic_cb18"]]     = apollo_ol(ol_settings02_1, functionality)
  P[["indic_cb19"]]     = apollo_ol(ol_settings03_1, functionality)
  P[["indic_cb21"]]     = apollo_ol(ol_settings04_1, functionality)
  
  # settings for latent variable 2: FEAR OF SURGICAL PROCEDURES
  ol_settings01_2 = list(outcomeOrdered=cb13,V=FEARSURGERY_lambda01*FEARSURGERY,tau=c(FEARSURGERY_tau01_1,FEARSURGERY_tau01_2,FEARSURGERY_tau01_3,FEARSURGERY_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_2 = list(outcomeOrdered=cb14,V=FEARSURGERY_lambda02*FEARSURGERY,tau=c(FEARSURGERY_tau02_1,FEARSURGERY_tau02_2,FEARSURGERY_tau02_3,FEARSURGERY_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_2 = list(outcomeOrdered=cb15,V=FEARSURGERY_lambda03*FEARSURGERY,tau=c(FEARSURGERY_tau03_1,FEARSURGERY_tau03_2,FEARSURGERY_tau03_3,FEARSURGERY_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_2 = list(outcomeOrdered=cb16,V=FEARSURGERY_lambda04*FEARSURGERY,tau=c(FEARSURGERY_tau04_1,FEARSURGERY_tau04_2,FEARSURGERY_tau04_3,FEARSURGERY_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb13"]]     = apollo_ol(ol_settings01_2, functionality)
  P[["indic_cb14"]]     = apollo_ol(ol_settings02_2, functionality)
  P[["indic_cb15"]]     = apollo_ol(ol_settings03_2, functionality)
  P[["indic_cb16"]]     = apollo_ol(ol_settings04_2, functionality)
  
  # settings for latent variable 3: PRIVACY
  ol_settings01_3 = list(outcomeOrdered=cb9,V=PRIVACY_lambda01*PRIVACY,tau=c(PRIVACY_tau01_1,PRIVACY_tau01_2,PRIVACY_tau01_3,PRIVACY_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_3 = list(outcomeOrdered=cb11,V=PRIVACY_lambda02*PRIVACY,tau=c(PRIVACY_tau02_1,PRIVACY_tau02_2,PRIVACY_tau02_3,PRIVACY_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_3 = list(outcomeOrdered=cb12,V=PRIVACY_lambda03*PRIVACY,tau=c(PRIVACY_tau03_1,PRIVACY_tau03_2,PRIVACY_tau03_3,PRIVACY_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_3 = list(outcomeOrdered=cb20,V=PRIVACY_lambda04*PRIVACY,tau=c(PRIVACY_tau04_1,PRIVACY_tau04_2,PRIVACY_tau04_3),coding=c(-2,0,1,2),rows=(task==1))

  P[["indic_cb9"]]     = apollo_ol(ol_settings01_3, functionality)
  P[["indic_cb11"]]     = apollo_ol(ol_settings02_3, functionality)
  P[["indic_cb12"]]     = apollo_ol(ol_settings03_3, functionality)
  P[["indic_cb20"]]     = apollo_ol(ol_settings04_3, functionality)
  
  # settings for latent variable 4: TRAVEL COST
  ol_settings01_4 = list(outcomeOrdered=cb1,V=TRAVELCOST_lambda01*TRAVELCOST,tau=c(TRAVELCOST_tau01_1,TRAVELCOST_tau01_2,TRAVELCOST_tau01_3,TRAVELCOST_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_4 = list(outcomeOrdered=cb4,V=TRAVELCOST_lambda02*TRAVELCOST,tau=c(TRAVELCOST_tau02_1,TRAVELCOST_tau02_2,TRAVELCOST_tau02_3,TRAVELCOST_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_4 = list(outcomeOrdered=cb6,V=TRAVELCOST_lambda03*TRAVELCOST,tau=c(TRAVELCOST_tau03_1,TRAVELCOST_tau03_2,TRAVELCOST_tau03_3,TRAVELCOST_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb1"]]     = apollo_ol(ol_settings01_4, functionality)
  P[["indic_cb4"]]     = apollo_ol(ol_settings02_4, functionality)
  P[["indic_cb6"]]     = apollo_ol(ol_settings03_4, functionality)
  
  # settings for latent variable 5: REASSURANCE
  ol_settings01_5 = list(outcomeOrdered=cb2,V=REASSURANCE_lambda01*REASSURANCE,tau=c(REASSURANCE_tau01_1,REASSURANCE_tau01_2,REASSURANCE_tau01_3,REASSURANCE_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_5 = list(outcomeOrdered=cb3,V=REASSURANCE_lambda02*REASSURANCE,tau=c(REASSURANCE_tau02_1,REASSURANCE_tau02_2,REASSURANCE_tau02_3,REASSURANCE_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_5 = list(outcomeOrdered=cb5,V=REASSURANCE_lambda03*REASSURANCE,tau=c(REASSURANCE_tau03_1,REASSURANCE_tau03_2,REASSURANCE_tau03_3,REASSURANCE_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_5 = list(outcomeOrdered=cb7,V=REASSURANCE_lambda04*REASSURANCE,tau=c(REASSURANCE_tau04_1,REASSURANCE_tau04_2,REASSURANCE_tau04_3,REASSURANCE_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings05_5 = list(outcomeOrdered=cb10,V=REASSURANCE_lambda05*REASSURANCE,tau=c(REASSURANCE_tau05_1,REASSURANCE_tau05_2,REASSURANCE_tau05_3,REASSURANCE_tau05_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb2"]]     = apollo_ol(ol_settings01_5, functionality)
  P[["indic_cb3"]]     = apollo_ol(ol_settings02_5, functionality)
  P[["indic_cb5"]]     = apollo_ol(ol_settings03_5, functionality)
  P[["indic_cb7"]]     = apollo_ol(ol_settings04_5, functionality)
  P[["indic_cb10"]]     = apollo_ol(ol_settings05_5, functionality)
  
  ### List of utilities : these must use the same names as in mnl_settings , order is irrelevant
  V=list()
  
  neither0  = neither + neither_SOCIALBARS*SOCIALBARS + neither_FEARSURGERY*FEARSURGERY + neither_PRIVACY*PRIVACY + neither_TRAVELCOST*TRAVELCOST + neither_REASSURANCE*REASSURANCE
  permhc0   = permhc + permhc_SOCIALBARS*SOCIALBARS + permhc_FEARSURGERY*FEARSURGERY + permhc_PRIVACY*PRIVACY + permhc_TRAVELCOST*TRAVELCOST + permhc_REASSURANCE*REASSURANCE
  dist50    = dist5 + dist5_SOCIALBARS*SOCIALBARS + dist5_FEARSURGERY*FEARSURGERY + dist5_PRIVACY*PRIVACY + dist5_TRAVELCOST*TRAVELCOST + dist5_REASSURANCE*REASSURANCE
  dist150   = dist15 + dist15_SOCIALBARS*SOCIALBARS + dist15_FEARSURGERY*FEARSURGERY + dist15_PRIVACY*PRIVACY + dist15_TRAVELCOST*TRAVELCOST + dist15_REASSURANCE*REASSURANCE

  
  V[['opt1']] = permhc0*permhc1+dist50*distance51+dist150*distance151+avail*available1+device*device1+somepriv*someprivate1+notpriv*notprivate1+voucher*voucher1+cash*cash1+pricenone*pricenone1+pricevoucher*pricevoucher1+pricecash*pricecash1
  V[['opt2']] = permhc0*permhc2+dist50*distance52+dist150*distance152+avail*available2+device*device2+somepriv*someprivate2+notpriv*notprivate2+voucher*voucher2+cash*cash2+pricenone*pricenone2+pricevoucher*pricevoucher2+pricecash*pricecash2
  V[['neither']] = neither0
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(opt1=1, opt2=2, neither=3),
    avail = list(opt1=1, opt2=1, neither=1),
    choiceVar = choice,
    V=V)
  
  ### Compute probabilities using MNL model
  P[['choice']] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
# estimate settings
estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       hessianRoutine="numDeriv")

# model estimation
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings)

apollo_modelOutput(model)
