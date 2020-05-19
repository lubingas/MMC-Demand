rm(list=ls())

library(apollo)
library(car)
library(data.table)
library(ggplot2)
library(ggpubr)
library(plotly)
library(scales)
library(ggthemes)
library(tidyverse)

setwd("/Users/slubinga/MMC-Demand/MIXL_Model/")

set.seed(01980)
apollo_initialise()

apollo_control = list(modelName ="mmc_mmnl_normal_all_corr",
                      modelDescr ="Correlated MMNL Model",
                      indivID ="ID",
                      mixing=TRUE,
                      nCores=4)

database=read.csv("../cleanerdata.csv",header=TRUE)

# create these data.tables for the prediction module
database=subset(database, select=c(ID,task,choice,
                                   permhc1,distance11,distance51,distance151,available1,device1,veryprivate1,someprivate1,notprivate1,none1,voucher1,cash1,price1,
                                   permhc2,distance12,distance52,distance152,available2,device2,veryprivate2,someprivate2,notprivate2,none2,voucher2,cash2,price2))

# interaction terms
database$pricenone1=database$price1*(database$none1==1)
database$pricenone2=database$price2*(database$none2==1)

database$pricevoucher1=database$price1*(database$voucher1==1)
database$pricevoucher2=database$price2*(database$voucher2==1)

database$pricecash1=database$price1*(database$cash1==1)
database$pricecash2=database$price2*(database$cash2==1)

startvalues=unlist(read.csv("apollo_mmnl_normal_corr3_iterations.csv")[215,-105])


apollo_beta=c(neither_mu=0,
              permhc_mu=0,
              dist5_mu=0,
              dist15_mu=0,
              avail_mu=0,
              somepriv_mu=0, 
              notpriv_mu=0, 
              device_mu=0,
              voucher_mu=0,
              cash_mu=0, 
              pricenone_mu=0,
              pricevoucher_mu=0,
              pricecash_mu=0,
              s0101=0,
              s0201=0,s0202=0,
              s0301=0,s0302=0,s0303=0,
              s0401=0,s0402=0,s0403=0,s0404=0,
              s0501=0,s0502=0,s0503=0,s0504=0,s0505=0,
              s0601=0,s0602=0,s0603=0,s0604=0,s0605=0,s0606=0,
              s0701=0,s0702=0,s0703=0,s0704=0,s0705=0,s0706=0,s0707=0,
              s0801=0,s0802=0,s0803=0,s0804=0,s0805=0,s0806=0,s0807=0,s0808=0,
              s0901=0,s0902=0,s0903=0,s0904=0,s0905=0,s0906=0,s0907=0,s0908=0,s0909=0,
              s1001=0,s1002=0,s1003=0,s1004=0,s1005=0,s1006=0,s1007=0,s1008=0,s1009=0,s1010=0,
              s1101=0,s1102=0,s1103=0,s1104=0,s1105=0,s1106=0,s1107=0,s1108=0,s1109=0,s1110=0,s1111=0,
              s1201=0,s1202=0,s1203=0,s1204=0,s1205=0,s1206=0,s1207=0,s1208=0,s1209=0,s1210=0,s1211=0,s1212=0,
              s1301=0,s1302=0,s1303=0,s1304=0,s1305=0,s1306=0,s1307=0,s1308=0,s1309=0,s1310=0,s1311=0,s1312=0,s1313=0)


apollo_beta=startvalues

apollo_fixed = c()

apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 2000,
  interUnifDraws = c(),
  interNormDraws = c("eta01","eta02","eta03","eta04",
                     "eta05","eta06","eta07","eta08",
                     "eta09","eta10","eta11","eta12","eta13"),
  intraDrawsType = "mlhs",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  
  randcoeff[['neither']]      =      neither_mu             + s0101*eta01
  randcoeff[['permhc']]       =      permhc_mu              + s0201*eta01 + s0202*eta02
  randcoeff[['dist5']]        =      dist5_mu               + s0301*eta01 + s0302*eta02 + s0303*eta03
  randcoeff[['dist15']]       =      dist15_mu              + s0401*eta01 + s0402*eta02 + s0403*eta03 + s0404*eta04
  randcoeff[['avail']]        =      avail_mu               + s0501*eta01 + s0502*eta02 + s0503*eta03 + s0504*eta04 + s0505*eta05
  randcoeff[['somepriv']]     =      somepriv_mu            + s0601*eta01 + s0602*eta02 + s0603*eta03 + s0604*eta04 + s0605*eta05 + s0606*eta06
  randcoeff[['notpriv']]      =      notpriv_mu             + s0701*eta01 + s0702*eta02 + s0703*eta03 + s0704*eta04 + s0705*eta05 + s0706*eta06 + s0707*eta07
  randcoeff[['device']]       =      device_mu              + s0801*eta01 + s0802*eta02 + s0803*eta03 + s0804*eta04 + s0805*eta05 + s0806*eta06 + s0807*eta07 + s0808*eta08
  randcoeff[['voucher']]      =      voucher_mu             + s0901*eta01 + s0902*eta02 + s0903*eta03 + s0904*eta04 + s0905*eta05 + s0906*eta06 + s0907*eta07 + s0908*eta08 + s0909*eta09
  randcoeff[['cash']]         =      cash_mu                + s1001*eta01 + s1002*eta02 + s1003*eta03 + s1004*eta04 + s1005*eta05 + s1006*eta06 + s1007*eta07 + s1008*eta08 + s1009*eta09 + s1010*eta10
  randcoeff[['pricenone']]    =      pricenone_mu           + s1101*eta01 + s1102*eta02 + s1103*eta03 + s1104*eta04 + s1105*eta05 + s1106*eta06 + s1107*eta07 + s1108*eta08 + s1109*eta09 + s1110*eta10 + s1111*eta11
  randcoeff[['pricevoucher']] =      pricevoucher_mu        + s1201*eta01 + s1202*eta02 + s1203*eta03 + s1204*eta04 + s1205*eta05 + s1206*eta06 + s1207*eta07 + s1208*eta08 + s1209*eta09 + s1210*eta10 + s1211*eta11 + s1212*eta12
  randcoeff[['pricecash']]    =      pricecash_mu           + s1301*eta01 + s1302*eta02 + s1303*eta03 + s1304*eta04 + s1305*eta05 + s1306*eta06 + s1307*eta07 + s1308*eta08 + s1309*eta09 + s1310*eta10 + s1311*eta11 + s1312*eta12 + s1313*eta13
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach (apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta,apollo_inputs))
  
  ### Create list of probabilities P
  P=list()
  
  ### List of utilities : these must use the same names as in mnl_settings , order is irrelevant
  V=list()
  
  V[['opt1']] = permhc*permhc1+dist5*distance51+dist15*distance151+avail*available1+device*device1+somepriv*someprivate1+notpriv*notprivate1+voucher*voucher1+cash*cash1+pricenone*pricenone1+pricevoucher*pricevoucher1+pricecash*pricecash1
  V[['opt2']] = permhc*permhc2+dist5*distance52+dist15*distance152+avail*available2+device*device2+somepriv*someprivate2+notpriv*notprivate2+voucher*voucher2+cash*cash2+pricenone*pricenone2+pricevoucher*pricevoucher2+pricecash*pricecash2
  V[['neither']] = neither
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(opt1=1, opt2=2, neither=3),
    avail = list(opt1=1, opt2=1, neither=1),
    choiceVar = choice,
    V=V)
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Estimate model
estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       hessianRoutine="numDeriv")

# apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings)

modelOutput_settings = list(printPVal=TRUE)
saveOutput_settings = list(printPVal=TRUE)

apollo_modelOutput(model, modelOutput_settings)
apollo_saveOutput(model, saveOutput_settings)

# ################################################################# #
#### POST ESTIMATION                                            ####
# ################################################################# #

model=apollo_loadModel("mmc_mmnl_normal_all_corr")

##################################################################################################################################################################
# PROCESS OUTPUT FOR STANDARD DEVIATION AND STANDARD ERRORS OF STANDARD DEVIATION
##################################################################################################################################################################

est=model$estimate
varnames=names(est)[1:13]

varcov=model$robvarcov


trimatnames = c("s0101",
                "s0201","s0202",
                "s0301","s0302","s0303",
                "s0401","s0402","s0403","s0404",
                "s0501","s0502","s0503","s0504","s0505",
                "s0601","s0602","s0603","s0604","s0605","s0606",
                "s0701","s0702","s0703","s0704","s0705","s0706","s0707",
                "s0801","s0802","s0803","s0804","s0805","s0806","s0807","s0808",
                "s0901","s0902","s0903","s0904","s0905","s0906","s0907","s0908","s0909",
                "s1001","s1002","s1003","s1004","s1005","s1006","s1007","s1008","s1009","s1010",
                "s1101","s1102","s1103","s1104","s1105","s1106","s1107","s1108","s1109","s1110","s1111",
                "s1201","s1202","s1203","s1204","s1205","s1206","s1207","s1208","s1209","s1210","s1211","s1212",
                "s1301","s1302","s1303","s1304","s1305","s1306","s1307","s1308","s1309","s1310","s1311","s1312","s1313")

trimat = est[match(trimatnames, names(est))]

tri=matrix(0,13,13)
tri[upper.tri(tri,diag=TRUE)]=trimat
tri=t(tri)

# standard deviations for random parameters 
sqrt(diag(tri%*%t(tri)))

#names(est)=estvar
#est=est[match(order, names(est))]

#get standard errors for utility parameters
est_ser=sqrt(diag(varcov))[1:13]
est_p=2*pnorm(abs(est[1:13]/est_ser),lower.tail=FALSE)
# ‘***’ p < 0.001, ‘**’ p < 0.01, ‘*’ p < 0.05, ‘†’ p < 0.1
sigp_est=ifelse(est_p < 0.001, "***", ifelse(est_p < 0.01, "**", ifelse(est_p < 0.05, "*", ifelse(est_p < 0.1, "†", ""))))


## calculate standard deviations and standard errors of standard deviations
library(car)
applist=c(c("sqrt(s0101**2)"),
          c("sqrt(s0201**2+s0202**2)"),
          c("sqrt(s0301**2+s0302**2+s0303^2)"),
          c("sqrt(s0401**2+s0402**2+s0403^2+s0404**2)"),
          c("sqrt(s0501**2+s0502**2+s0503^2+s0504**2+s0505**2)"),
          c("sqrt(s0601**2+s0602**2+s0603^2+s0604**2+s0605**2+s0606**2)"),
          c("sqrt(s0701**2+s0702**2+s0703^2+s0704**2+s0705**2+s0706**2+s0707**2)"),
          c("sqrt(s0801**2+s0802**2+s0803^2+s0804**2+s0805**2+s0806**2+s0807**2+s0808**2)"),
          c("sqrt(s0901**2+s0902**2+s0903^2+s0904**2+s0905**2+s0906**2+s0907**2+s0908**2+s0909**2)"),
          c("sqrt(s1001**2+s1002**2+s1003^2+s1004**2+s1005**2+s1006**2+s1007**2+s1008**2+s1009**2+s1010**2)"),
          c("sqrt(s1101**2+s1102**2+s1103^2+s1104**2+s1105**2+s1106**2+s1107**2+s1108**2+s1109**2+s1110**2+s1111**2)"),
          c("sqrt(s1201**2+s1202**2+s1203^2+s1204**2+s1205**2+s1206**2+s1207**2+s1208**2+s1209**2+s1210**2+s1211**2+s1212**2)"),
          c("sqrt(s1301**2+s1302**2+s1303^2+s1304**2+s1305**2+s1306**2+s1307**2+s1308**2+s1309**2+s1310**2+s1311**2+s1312**2+s1313**2)"))
sds_out = data.frame(t(sapply(applist, function(x) deltaMethod(est, x, vcov.=varcov))))

sds=unname(unlist(sds_out$Estimate))
sds_ser=unname(unlist(sds_out$SE))
sds_p=2*pnorm(abs(sds/sds_ser),lower.tail=FALSE)
sigp_sds=ifelse(sds_p < 0.001, "***", ifelse(sds_p < 0.01, "**", ifelse(sds_p < 0.05, "*", ifelse(sds_p < 0.1, "†", ""))))

cbind(est[1:13], est_ser, est_p, sds, sds_ser, sds_p)
cbind(sigp_est, sigp_sds)

cbind(est[1:13], sigp_est, est_ser, est_p, sds, sds_ser, sds_p)

restable = cbind(names(est[1:13]), paste0("'",round(est[1:13],3), sigp_est, " (",round(est_ser,3),")"), paste0("'",round(sds,3), sigp_sds, " (",round(sds_ser,3),")"))
write.csv(restable,"restable.csv")

##################################################################################################################################################################
# POLICY ANALYSIS
##################################################################################################################################################################

beta=model$estimate

### Create random parameters
#####################################################################
# code for MLHS							    #
#####################################################################

shuffle=function(inv){
  out=inv[rank(runif(length(inv)))];
  out}

mlhs=function(N,d,i){
  temp=seq(0,N-1)/N;
  out=matrix(0,N*i,d);
  j=1;
  k=1;
  while(j<i+1){
    k=1;
    while(k<d+1){
      out[(1+N*(j-1)):(N*j),k]=shuffle(temp+runif(1)/N);
      k=k+1}
    j=j+1}
  out}

###GENERATE DRAWS
Ndraws=2000      # set number of draws to use per person and per parameter
N=1000
#N=length(unique(database$ID))
dimensions=13    # define number of random terms in the model

# generate draws (using Halton, MLHS or PMC draws - uncomment appropriate line)
#draws=as.matrix(halton(Ndraws*N,dimensions))
draws=as.matrix(mlhs(Ndraws,dimensions,N))
#draws=matrix(runif(N*Ndraws*dimensions),nrow=N*Ndraws,byrow=T)
#draws=data.frame(matrix(rnorm(N*Ndraws*dimensions),nrow=N*Ndraws,byrow=T))
#draws=apollo:::apollo_makeDraws(apollo_inputs, silent = FALSE)

# assign names to individual sets of draws - need one entry per dimension
colnames(draws)=c("eta01","eta02","eta03","eta04",
                  "eta05","eta06","eta07","eta08",
                  "eta09","eta10","eta11","eta12","eta13")

draws=data.frame(draws)

randCoeff = function(beta, draws) {
  # needed to be able to refer to parameters by name
  beta1=as.list(beta)
  draws1=draws
  attach(beta1)
  attach(draws1)
  
  randcoeff = list()
  
  randcoeff[['neither']]      =      neither_mu             + s0101*eta01
  randcoeff[['permhc']]       =      permhc_mu              + s0201*eta01 + s0202*eta02
  randcoeff[['dist5']]        =      dist5_mu               + s0301*eta01 + s0302*eta02 + s0303*eta03
  randcoeff[['dist15']]       =      dist15_mu              + s0401*eta01 + s0402*eta02 + s0403*eta03 + s0404*eta04
  randcoeff[['avail']]        =      avail_mu               + s0501*eta01 + s0502*eta02 + s0503*eta03 + s0504*eta04 + s0505*eta05
  randcoeff[['somepriv']]     =      somepriv_mu            + s0601*eta01 + s0602*eta02 + s0603*eta03 + s0604*eta04 + s0605*eta05 + s0606*eta06
  randcoeff[['notpriv']]      =      notpriv_mu             + s0701*eta01 + s0702*eta02 + s0703*eta03 + s0704*eta04 + s0705*eta05 + s0706*eta06 + s0707*eta07
  randcoeff[['device']]       =      device_mu              + s0801*eta01 + s0802*eta02 + s0803*eta03 + s0804*eta04 + s0805*eta05 + s0806*eta06 + s0807*eta07 + s0808*eta08
  randcoeff[['voucher']]      =      voucher_mu             + s0901*eta01 + s0902*eta02 + s0903*eta03 + s0904*eta04 + s0905*eta05 + s0906*eta06 + s0907*eta07 + s0908*eta08 + s0909*eta09
  randcoeff[['cash']]         =      cash_mu                + s1001*eta01 + s1002*eta02 + s1003*eta03 + s1004*eta04 + s1005*eta05 + s1006*eta06 + s1007*eta07 + s1008*eta08 + s1009*eta09 + s1010*eta10
  randcoeff[['pricenone']]    =      pricenone_mu           + s1101*eta01 + s1102*eta02 + s1103*eta03 + s1104*eta04 + s1105*eta05 + s1106*eta06 + s1107*eta07 + s1108*eta08 + s1109*eta09 + s1110*eta10 + s1111*eta11
  randcoeff[['pricevoucher']] =      pricevoucher_mu        + s1201*eta01 + s1202*eta02 + s1203*eta03 + s1204*eta04 + s1205*eta05 + s1206*eta06 + s1207*eta07 + s1208*eta08 + s1209*eta09 + s1210*eta10 + s1211*eta11 + s1212*eta12
  randcoeff[['pricecash']]    =      pricecash_mu           + s1301*eta01 + s1302*eta02 + s1303*eta03 + s1304*eta04 + s1305*eta05 + s1306*eta06 + s1307*eta07 + s1308*eta08 + s1309*eta09 + s1310*eta10 + s1311*eta11 + s1312*eta12 + s1313*eta13
  detach(beta1)
  detach(draws1)
  return(randcoeff)
}

simdraws = data.frame(randCoeff(beta,draws))

stackeddraws=stack(simdraws)
d = ggplot(stackeddraws, aes(x=values, color=ind))  +
  geom_density()

ggplotly(d)

# set up policy matrix
# order: "neither","permhc","distance5","distance15","available","someprivate","notprivate","device","voucher","cash","pricenone","pricevoucher","pricecash"
nocircumcision= c(1,0,0,0,0,0,0,0,0,0,0,0,0)
surgery=        c(0,1,0,1,2,0,1,0,0,0,0,0,0)       # 1) Introduction of circumcision services at permanent facilities
outreach=       c(0,0,0,0,5,1,0,0,0,0,0,0,0)       # 2) Addition of out-reach services within 1 km distance, surgeons are available 5 days a week, some privacy
devices=        c(0,1,0,1,2,0,1,1,0,0,0,0,0)       # 3) Introduction of Circumcision Devices at Permanent Health Facilities
incent_device=  c(0,1,0,1,2,0,1,1,0,1,0,0,-12)     # 4) Incentive payment of US$12 at permanent facilities with devices
incent_surgery= c(0,1,0,1,2,0,1,0,0,1,0,0,-8)      # 5) Incentive payment of US$8 at permanent facilities with surgery

policymat=cbind(nocircumcision,surgery,outreach,devices,incent_device,incent_surgery)
rownames(policymat)=c("neither","permhc","dist5","dist15","avail","somepriv","notpriv","device","voucher","cash","pricenone","pricevoucher","pricecash")
###################################################################################################################################################################


####################################################
# adjust model to replicate revealed shares
# what ASC will give a 30.8% circumcision rate, if only opt-out and surgical circumcision options are available?
# actual shares
S=sum(c(1250,466,334))/sum(c(3943,1093,1627))


# function to calculate a constant we add to the ASC such that model predicted MMC uptake matches RW MMC uptake
S_hat=function(b) {
  # calculate predicted shares
  simdraws["neither"]=simdraws["neither"] + b
  
  # utilities
  expU=exp(data.matrix(simdraws) %*% policymat[,c(1,2)])
  P=expU/rowSums(expU)
  
  # create grouping variable
  group=rep(1:(nrow(expU)/Ndraws), each = Ndraws)
  
  # average across draws for each individual, then use sample function to determine selected option
  aP=data.frame(cbind(P, group))
  aP1 = aP %>%
    group_by(group) %>%
    summarise_all(mean) %>%
    mutate(choice=apply(select(., 2,3), 1, FUN=sample, x=c(0,1), size=1, replace=FALSE))
  
  #C=apply(P, 1, sample, x=c(0,1), size=1, replace=FALSE)
  (mean(aP1$choice))
}
g <- function(x) abs(S - S_hat(x))
a = optimize(g, interval=c(-15, 15), maximum=FALSE)

b=a$minimum

simbeta = simdraws
simbeta["neither"] = simbeta["neither"] + b

# how do they compare?
neitherdata=data.frame(draws=simdraws[,1],beta=simbeta[,1])
neitherdata=stack(neitherdata)
mean.data=plyr::ddply(neitherdata, "ind", summarise, values.mean=mean(values))

ggplot(neitherdata, aes(x=values, color=ind))  +
  geom_density() +
  geom_vline(data=mean.data, aes(xintercept=values.mean,  colour=ind),
             linetype="dashed", size=1)

# CHECK THAT WE GOT THE CORRECT MARKET SHARES AFTER AVERAGING OVER DRAWS
# utilities
expU_try=exp(data.matrix(simbeta) %*% policymat[,c(1,2)])
P_try=expU_try/rowSums(expU_try)

# create grouping variable
group_try=rep(1:(nrow(expU_try)/Ndraws), each = Ndraws)

# average across draws for each individual, then use sample to determine selected option
aP_try=data.frame(cbind(P_try, group_try))
aP1_try = aP_try %>%
  group_by(group_try) %>%
  summarise_all(mean) %>%
  mutate(choice=apply(select(., 2,3), 1, FUN=sample, x=c(0,1), size=1, replace=FALSE))

(mean(aP1_try$choice))

# Now conduct policy analysis i.e., predicted probabilities for different policy scenarios
# calculate exponentiated utilities again and delete INF rows
expU=exp(data.matrix(simbeta) %*% policymat)

# set up grouping variable for Ndraws
group=rep(1:(nrow(expU)/Ndraws), each = Ndraws)

# use simulation
# for each draw from joint distribution for each individual, calcuate choice probabilities, get choice and average across Ndraws

# INITIAL STATES
# calculate choice probabilities
P0=expU[,c(1,2)]/rowSums(expU[,c(1,2)])
# average across draws for each individual, then use sample to determine selected option
aP0=data.frame(cbind(P0, group))
aP01 = aP0 %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  mutate(selected=apply(select(., 2,3), 1, FUN=sample, x=c(1,2), size=1, replace=FALSE))

T0=table(aP01$selected)/nrow(aP01)

# FUTURE STATES
# outreach
# calculate choice probabilities
P1=expU[,c(1,2,3)]/rowSums(expU[,c(1,2,3)])
# average across draws for each individual, then use sample to determine selected option
aP1=data.frame(cbind(P1, group))
aP11 = aP1 %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  mutate(selected=apply(select(., 2,3,4), 1, FUN=sample, x=c(1,2,3), size=1, replace=FALSE))

T1=table(aP11$selected)/nrow(aP11)


# devices
# calculate choice probabilities
P2=expU[,c(1,2,4)]/rowSums(expU[,c(1,2,4)])
# average across draws for each individual, then use sample to determine selected option
aP2=data.frame(cbind(P2, group))
aP21 = aP2 %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  mutate(selected=apply(select(., 2,3,4), 1, FUN=sample, x=c(1,2,4), size=1, replace=FALSE))

T2=table(aP21$selected)/nrow(aP21)

# devices + outreach
# calculate choice probabilities
P3=expU[,c(1,2,3,4)]/rowSums(expU[,c(1,2,3,4)])
# average across draws for each individual, then use sample to determine selected option
aP3=data.frame(cbind(P3, group))
aP31 = aP3 %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  mutate(selected=apply(select(., 2,3,4,5), 1, FUN=sample, x=c(1,2,3,4), size=1, replace=FALSE))

T3=table(aP31$selected)/nrow(aP31)


# incentives
# calculate choice probabilities
P4=expU[,c(1,5,6)]/rowSums(expU[,c(1,5,6)])   # choice probabilities
# average across draws for each individual, then use sample to determine selected option
aP4=data.frame(cbind(P4, group))
aP41 = aP4 %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  mutate(selected=apply(select(., 2,3,4), 1, FUN=sample, x=c(1,5,6), size=1, replace=FALSE))

T4=table(aP41$selected)/nrow(aP41)

# incentives + outreach
# calculate choice probabilities
P5=expU[,c(1,3,5,6)]/rowSums(expU[,c(1,3,5,6)])   # choice probabilities
# average across draws for each individual, then use sample to determine selected option
aP5=data.frame(cbind(P5, group))
aP51 = aP5 %>%
  group_by(group) %>%
  summarise_all(mean) %>%
  mutate(selected=apply(select(., 2,3,4,5), 1, FUN=sample, x=c(1,3,5,6), size=1, replace=FALSE))

T5=table(aP51$selected)/nrow(aP51)

# some data manupilations so it is easy to plot the data
l = list(T0, T1, T2, T3, T4, T5)

c_names <- unique(unlist(sapply(l, names)))
res=t(do.call(rbind, lapply(l, `[`, c_names)))

res[is.na(res)]=0

# small function to combine rows
combine_rows <- function(data, row1, row2) {
  data[row2, ] <- data[row1, ] + data[row2, ]
  data[-row1, ]
}

#res=combine_rows(res, 5, 6)
# res=combine_rows(res, 5, 4)
colnames(res) <- c("Initial state",
                   "Policy 1",
                   "Policy 2",
                   "Policy 3",
                   "Policy 4",
                   "Policy 5")
rownames(res) <- c("Opt-out",
                   "Surgery (Hospital)",
                   "Outreach (Surgery)",
                   "Device (Hospital)",
                   "Incentive (Device)",
                   "Incentive (Surgery)")

res_vec=as.vector(t(res))

pref=factor(rep(c("nocirc","surgery","outreach","device","incentive-device","incentive-surgery"),each=6),
            levels=c("incentive-surgery","incentive-device","device","outreach","surgery","nocirc"),
            labels = c("Incentive (Surgery)",
                       "Incentive (Device)",
                       "Device (Hospital)",
                       "Outreach (Surgery)",
                       "Surgery (Hospital)",
                       "Opt-out"))
policy=rep(c("Initial state","Policy 1","Policy 2","Policy 3","Policy 4","Policy 5"),times=6)

plotdata=data.frame(policy,pref,res=res_vec)

q = ggplot(plotdata, aes(fill=pref, y=res, x=policy)) + 
  geom_bar(stat="identity", position="fill", color="white") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(position = "top") +
  labs(y = "", 
       fill = "Choice",
       x = "",
       title = "") +
  theme_minimal() + 
  geom_text(aes(label = ifelse((res > 0.03), percent(res), "")), 
            size = 6, 
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x=element_text(size=18),
        axis.text.y=element_text(size = 18),
        legend.position="none",
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

postscript(file="mmnl_predictions.eps", horizontal=FALSE, width=9, height=6)
q
dev.off()

p = ggplot(plotdata, aes(fill=pref, y=res, x=policy)) + 
  geom_bar(stat="identity", position="fill", color="white") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(position = "top") +
  theme_minimal() + 
  geom_text(aes(label = ifelse((res > 0.03), percent(res), "")), 
            size = 3, 
            position = position_stack(vjust = 0.5)) + 
  #geom_label(x=0, y=1.1, label="B", fill = "white", fontface = "bold") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        legend.position="none",
        legend.text=element_text(size=9),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pdf(file="mmnl_predictions.pdf", width=4.2, height=2.6, paper="special")
par(mar=c(0,0,0,0))
p
grid.circle(x = unit(0.06, "npc"), y = unit(0.93, "npc"), r = 0.04, gp = gpar(lwd=2))
grid.text("B", x = unit(0.06, "npc"), y = unit(0.93, "npc"), gp = gpar(fontface = "bold", fontsize=10))
dev.off()


