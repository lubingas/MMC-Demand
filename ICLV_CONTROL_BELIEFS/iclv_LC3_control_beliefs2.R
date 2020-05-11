rm(list=ls())

library(apollo)

setwd("/Users/slubinga/Documents/GitHub/MMC-Demand/ICLV_CONTROL_BELIEFS")

apollo_initialise()

apollo_control = list(modelName ="ICLV_3_LC_CONTROL_BELIEFS_Rand_Intercept",
                      modelDescr ="ICLV 3 LC WITH CONTROL BELIEFS RANDOM INTERCEPT IN LC FUNCTION",
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
parnames=c("neither_C1","permhc_C1","dist5_C1","dist15_C1","avail_C1","somepriv_C1","notpriv_C1","device_C1","voucher_C1","cash_C1","pricenone_C1","pricevoucher_C1","pricecash_C1",
           "neither_C2","permhc_C2","dist5_C2","dist15_C2","avail_C2","somepriv_C2","notpriv_C2","device_C2","voucher_C2","cash_C2","pricenone_C2","pricevoucher_C2","pricecash_C2",
           "neither_C3","permhc_C3","dist5_C3","dist15_C3","avail_C3","somepriv_C3","notpriv_C3","device_C3","voucher_C3","cash_C3","pricenone_C3","pricevoucher_C3","pricecash_C3",
           
           # Latent class model parameters
           "INTERCEPT_C2_mu","INTERCEPT_C3_mu",
           "INTERCEPT_C2_sig","INTERCEPT_C3_sig",
           
           "tau_SOCIALBARS_C2","tau_SOCIALBARS_C3",
           "tau_FEARSURGERY_C2","tau_FEARSURGERY_C3",
           "tau_PRIVACY_C2","tau_PRIVACY_C3",
           "tau_TRAVELCOST_C2","tau_TRAVELCOST_C3",
           "tau_REASSURANCE_C2","tau_REASSURANCE_C3",

           # lambdas for indicators
           "SOCIALBARS_lambda01","SOCIALBARS_lambda02","SOCIALBARS_lambda03","SOCIALBARS_lambda04",
           "FEARSURGERY_lambda01","FEARSURGERY_lambda02","FEARSURGERY_lambda03","FEARSURGERY_lambda04",
           "PRIVACY_lambda01","PRIVACY_lambda02","PRIVACY_lambda03","PRIVACY_lambda04",
           "TRAVELCOST_lambda01","TRAVELCOST_lambda02","TRAVELCOST_lambda03",
           "REASSURANCE_lambda01","REASSURANCE_lambda02","REASSURANCE_lambda03","REASSURANCE_lambda04","REASSURANCE_lambda05",
           
           # SIGMAS
           "SOCIALBARS_sig","FEARSURGERY_sig","PRIVACY_sig","TRAVELCOST_sig","REASSURANCE_sig",
           
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
startvalues=read.csv("lc_3_classes_estimates.csv")[1:39,2]
startvalues=c(startvalues,
              0.1,0.1,                        # mu and sigma for INTERCEPT for LC parameters, class 2
              0.1,0.1,                        # mu and sigma for INTERCEPT for LC parameters, class 3
              rep(0,10),                      # taus for the latent class model
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

apollo_fixed = c("SOCIALBARS_sig","FEARSURGERY_sig","PRIVACY_sig","TRAVELCOST_sig","REASSURANCE_sig")


#### DEFINE RANDOM COMPONENTS for LV
### set parameters for generating drawss 
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 200,
  interUnifDraws = c(),
  interNormDraws = c("eta01","eta02","eta03","eta04","eta05","eta06","eta07"), # latent variable disturbances ~N(0,1), assume uncorrelated
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
  randcoeff[['SOCIALBARS']]      =      SOCIALBARS_sig        * eta01
  randcoeff[['FEARSURGERY']]     =      FEARSURGERY_sig       * eta02
  randcoeff[['PRIVACY']]         =      PRIVACY_sig           * eta03
  randcoeff[['TRAVELCOST']]      =      TRAVELCOST_sig        * eta04
  randcoeff[['REASSURANCE']]     =      REASSURANCE_sig       * eta05
  
  randcoeff[['INTERCEPT_C2']]    =      INTERCEPT_C2_mu + INTERCEPT_C2_sig      * eta06
  randcoeff[['INTERCEPT_C3']]    =      INTERCEPT_C3_mu + INTERCEPT_C3_sig      * eta07
  
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
  V[["class_C2"]] = INTERCEPT_C2 + tau_SOCIALBARS_C2*SOCIALBARS+tau_FEARSURGERY_C2*FEARSURGERY+tau_PRIVACY_C2*PRIVACY+tau_TRAVELCOST_C2*TRAVELCOST+tau_REASSURANCE_C2*REASSURANCE
  V[["class_C3"]] = INTERCEPT_C3 + tau_SOCIALBARS_C3*SOCIALBARS+tau_FEARSURGERY_C3*FEARSURGERY+tau_PRIVACY_C3*PRIVACY+tau_TRAVELCOST_C3*TRAVELCOST+tau_REASSURANCE_C3*REASSURANCE
  
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
  ol_settings01_1 = list(outcomeOrdered=cb17,V=SOCIALBARS_lambda01*SOCIALBARS,tau=c(SOCIALBARS_tau01_1,SOCIALBARS_tau01_2,SOCIALBARS_tau01_3,SOCIALBARS_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_1 = list(outcomeOrdered=cb18,V=SOCIALBARS_lambda02*SOCIALBARS,tau=c(SOCIALBARS_tau02_1,SOCIALBARS_tau02_2,SOCIALBARS_tau02_3,SOCIALBARS_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_1 = list(outcomeOrdered=cb19,V=SOCIALBARS_lambda03*SOCIALBARS,tau=c(SOCIALBARS_tau03_1,SOCIALBARS_tau03_2,SOCIALBARS_tau03_3,SOCIALBARS_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_1 = list(outcomeOrdered=cb21,V=SOCIALBARS_lambda04*SOCIALBARS,tau=c(SOCIALBARS_tau04_1,SOCIALBARS_tau04_2,SOCIALBARS_tau04_3,SOCIALBARS_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))

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
  ol_settings01_2 = list(outcomeOrdered=cb13,V=FEARSURGERY_lambda01*FEARSURGERY,tau=c(FEARSURGERY_tau01_1,FEARSURGERY_tau01_2,FEARSURGERY_tau01_3,FEARSURGERY_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_2 = list(outcomeOrdered=cb14,V=FEARSURGERY_lambda02*FEARSURGERY,tau=c(FEARSURGERY_tau02_1,FEARSURGERY_tau02_2,FEARSURGERY_tau02_3,FEARSURGERY_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_2 = list(outcomeOrdered=cb15,V=FEARSURGERY_lambda03*FEARSURGERY,tau=c(FEARSURGERY_tau03_1,FEARSURGERY_tau03_2,FEARSURGERY_tau03_3,FEARSURGERY_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_2 = list(outcomeOrdered=cb16,V=FEARSURGERY_lambda04*FEARSURGERY,tau=c(FEARSURGERY_tau04_1,FEARSURGERY_tau04_2,FEARSURGERY_tau04_3,FEARSURGERY_tau04_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb13"]]     = apollo_ol(ol_settings01_2, functionality)
  P[["indic_cb14"]]     = apollo_ol(ol_settings02_2, functionality)
  P[["indic_cb15"]]     = apollo_ol(ol_settings03_2, functionality)
  P[["indic_cb16"]]     = apollo_ol(ol_settings04_2, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb13"]] = apollo_panelProd(P[["indic_cb13"]], apollo_inputs ,functionality)
  P[["indic_cb14"]] = apollo_panelProd(P[["indic_cb14"]], apollo_inputs ,functionality)
  P[["indic_cb15"]] = apollo_panelProd(P[["indic_cb15"]], apollo_inputs ,functionality)
  P[["indic_cb16"]] = apollo_panelProd(P[["indic_cb16"]], apollo_inputs ,functionality)
  
  # settings for latent variable 3: PRIVACY
  ol_settings01_3 = list(outcomeOrdered=cb9,V=PRIVACY_lambda01*PRIVACY,tau=c(PRIVACY_tau01_1,PRIVACY_tau01_2,PRIVACY_tau01_3,PRIVACY_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_3 = list(outcomeOrdered=cb11,V=PRIVACY_lambda02*PRIVACY,tau=c(PRIVACY_tau02_1,PRIVACY_tau02_2,PRIVACY_tau02_3,PRIVACY_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_3 = list(outcomeOrdered=cb12,V=PRIVACY_lambda03*PRIVACY,tau=c(PRIVACY_tau03_1,PRIVACY_tau03_2,PRIVACY_tau03_3,PRIVACY_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings04_3 = list(outcomeOrdered=cb20,V=PRIVACY_lambda04*PRIVACY,tau=c(PRIVACY_tau04_1,PRIVACY_tau04_2,PRIVACY_tau04_3),coding=c(-2,0,1,2),rows=(task==1))

  P[["indic_cb9"]]     = apollo_ol(ol_settings01_3, functionality)
  P[["indic_cb11"]]     = apollo_ol(ol_settings02_3, functionality)
  P[["indic_cb12"]]     = apollo_ol(ol_settings03_3, functionality)
  P[["indic_cb20"]]     = apollo_ol(ol_settings04_3, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb9"]] = apollo_panelProd(P[["indic_cb9"]], apollo_inputs ,functionality)
  P[["indic_cb11"]] = apollo_panelProd(P[["indic_cb11"]], apollo_inputs ,functionality)
  P[["indic_cb12"]] = apollo_panelProd(P[["indic_cb12"]], apollo_inputs ,functionality)
  P[["indic_cb20"]] = apollo_panelProd(P[["indic_cb20"]], apollo_inputs ,functionality)
  
  # settings for latent variable 4: TRAVEL COST
  ol_settings01_4 = list(outcomeOrdered=cb1,V=TRAVELCOST_lambda01*TRAVELCOST,tau=c(TRAVELCOST_tau01_1,TRAVELCOST_tau01_2,TRAVELCOST_tau01_3,TRAVELCOST_tau01_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings02_4 = list(outcomeOrdered=cb4,V=TRAVELCOST_lambda02*TRAVELCOST,tau=c(TRAVELCOST_tau02_1,TRAVELCOST_tau02_2,TRAVELCOST_tau02_3,TRAVELCOST_tau02_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  ol_settings03_4 = list(outcomeOrdered=cb6,V=TRAVELCOST_lambda03*TRAVELCOST,tau=c(TRAVELCOST_tau03_1,TRAVELCOST_tau03_2,TRAVELCOST_tau03_3,TRAVELCOST_tau03_4),coding=c(-2,-1,0,1,2),rows=(task==1))
  
  P[["indic_cb1"]]     = apollo_ol(ol_settings01_4, functionality)
  P[["indic_cb4"]]     = apollo_ol(ol_settings02_4, functionality)
  P[["indic_cb6"]]     = apollo_ol(ol_settings03_4, functionality)
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb1"]] = apollo_panelProd(P[["indic_cb1"]], apollo_inputs ,functionality)
  P[["indic_cb4"]] = apollo_panelProd(P[["indic_cb4"]], apollo_inputs ,functionality)
  P[["indic_cb6"]] = apollo_panelProd(P[["indic_cb6"]], apollo_inputs ,functionality)
  
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
  
  # product across choices for the same individual - to match LC probabilities
  P[["indic_cb2"]] = apollo_panelProd(P[["indic_cb2"]], apollo_inputs ,functionality)
  P[["indic_cb3"]] = apollo_panelProd(P[["indic_cb3"]], apollo_inputs ,functionality)
  P[["indic_cb5"]] = apollo_panelProd(P[["indic_cb5"]], apollo_inputs ,functionality)
  P[["indic_cb7"]] = apollo_panelProd(P[["indic_cb7"]], apollo_inputs ,functionality)
  P[["indic_cb10"]] = apollo_panelProd(P[["indic_cb10"]], apollo_inputs ,functionality)

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
  nDrawsTry = c(250, 500, 1000, 2000),
  nCoresTry = c(5,10,15,20,30),
  nRep      = 10
)

apollo:::apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional: calculate LL before model estimation
apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### increase iterations, complex model
estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       hessianRoutine="numDeriv")

# model estimation
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings)

apollo_modelOutput(model)
