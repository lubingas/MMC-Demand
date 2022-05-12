rm(list=ls())

# load libraries
library(apollo)
library(car)
library(tidyverse)
library(grid)
library(svglite)
library(scales)
library(paletteer)
library(ggpubr)

# my helper functions
source("../../myFunctions/helper_functions.R")

# alot of output, so I set my default directory to where i can find my code to run the model 
#and output results in the same directory
setwd("../../Analytical Models/MIXL_Model")

#update to tidyverse grammar (read data in as tibble)
# relative paths since I am working on my GIT project
database = read_csv("../../Data/cleanerdata_original.csv", col_names = TRUE)


database = database %>%
  select(ID,task,choice, # I am only working with a few variables that define the design matrix
         permhc1,distance11,distance51,distance151,available1,device1,veryprivate1,someprivate1,notprivate1,none1,voucher1,cash1,price1,
         permhc2,distance12,distance52,distance152,available2,device2,veryprivate2,someprivate2,notprivate2,none2,voucher2,cash2,price2) %>%
  mutate(pricenone1 = price1*(none1 == 1), pricenone2 = price2*(none2 == 1),
         pricevoucher1 = price1*(voucher1 == 1), pricevoucher2 = price2*(voucher2 == 1),
         pricecash1 = price1*(cash1 == 1), pricecash2 = price2*(cash2 == 1))


# initialize a few things we need
set.seed(01980)
apollo_initialise()

apollo_control = list(modelName ="mmc_mmnl_normal_all_corr",
                      modelDescr ="Correlated MMNL Model",
                      indivID ="ID",
                      mixing=TRUE,
                      nCores=4)


startvalues=unlist(read_csv("apollo_mmnl_normal_corr_iterations.csv")[215,-105])

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


#apollo_beta=startvalues

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
beta=model$estimate
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

#broom::tidy(model)

##################################################################################################################################################################
# POLICY ANALYSIS
##################################################################################################################################################################
# set up policy matrix
# order: "neither","permhc","distance5","distance15","available","someprivate","notprivate","device","voucher","cash","pricenone","pricevoucher","pricecash"
CHOICE = c("Opt-out", "Surgery (Hospital)", "Outreach (Surgery)", "Device (Hospital)", "Incentive (Device)", "Incentive (Surgery)")
CHOICE_reorder = c("Incentive (Surgery)", "Incentive (Device)", "Device (Hospital)", "Outreach (Surgery)", "Surgery (Hospital)", "Opt-out")
PARAMS = c("neither","permhc","dist5","dist15","avail","somepriv","notpriv","device","voucher","cash","pricenone","pricevoucher","pricecash")
POLICY = c("Initial state", "Policy 1", "Policy 2", "Policy 3", "Policy 4", "Policy 5")

policymat = tibble(nocircumcision = c(1,0,0,0,0,0,0,0,0,0,0,0,0),
                   surgery        = c(0,1,0,1,2,0,1,0,0,0,0,0,0),       # 1) Introduction of circumcision services at permanent facilities
                   outreach       = c(0,0,0,0,5,1,0,0,0,0,0,0,0),       # 2) Addition of out-reach services within 1 km distance, surgeons are available 5 days a week, some privacy
                   devices        = c(0,1,0,1,2,0,1,1,0,0,0,0,0),       # 3) Introduction of Circumcision Devices at Permanent Health Facilities
                   incent_device  = c(0,1,0,1,2,0,1,1,0,1,0,0,-12),     # 4) Incentive payment of US$12 at permanent facilities with devices
                   incent_surgery = c(0,1,0,1,2,0,1,0,0,1,0,0,-8)) %>%  # 5) Incentive payment of US$8 at permanent facilities with surgery
  as.matrix() %>% 
  magrittr::set_rownames(PARAMS) %>%
  magrittr::set_colnames(CHOICE)

init_policy = policymat[,c(1,2)]
policy1 = policymat[,c(1,2,3)]
policy2 = policymat[,c(1,2,4)]
policy3 = policymat[,c(1,2,3,4)]
policy4 = policymat[,c(1,5,6)]
policy5 = policymat[,c(1,3,5,6)]

policies = list(init_policy, policy1, policy2, policy3, policy4, policy5)

###################################################################################################################################################################
###################################################################################################################################################################
# adjust model to replicate revealed shares
# what ASC will give a 30.8% circumcision probability (or 69.2% no circumcision probability), if only opt-out and surgical circumcision options are available?
# actual shares (Ref:  )
S = 1-sum(c(1250,466,334))/sum(c(3943,1093,1627))

## FIRST USE UNCONDITIONAL DRAWS FROM THE MODEL OBJECT FOR 406 INDIVIDUALS
#generate simdraws outside function for speed and reproducibility:
set.seed(01980)
simdraws=apollo_unconditionals(model, apollo_probabilities, apollo_inputs)
Ndraws=2000      # set number of draws to use per person and per parameter
N=406
dimensions=13    # define number of random terms in the model


## wrap everything in a function
S_hat = function(a_hat, draws, policy_matrix, est_b = FALSE) {
  
  set.seed(01980)
  
  # add a_hat to neither from simdraws
  draws$neither = draws$neither + a_hat
  
  if (is.null(dim(draws))) { # if working with unconditional draws from apollo
    
    # create a 3D array, rows = individuals 2d = interindividual draws 3d = intraindividual draws i.e., sS[1:N, 1:Ndraws, 1:dimensions]
    sS = simplify2array(draws)
    
    # for neither add the a_hat
    #sS[1:N, 1:Ndraws, 1:dimensions] = sS[1:N, 1:Ndraws, 1] + a_hat
    
    # calculate choice probabilities along the N dimension (i.e., over the 2000 rand parms for each individual)
    pChosen = lapply(1:N, function(i) exp(sS[i, , ] %*% policy_matrix)/rowSums(exp(sS[i, , ] %*% policy_matrix)))
    
    # average (integrate) choice probabilities over the draws (for each individual)
    pChosen = t(mapply(pChosen, FUN = function(x) apply(x, 2, mean))) %>%
      as_tibble() %>% # combine in tibble
      # probabilistically assign chosen alternative
      mutate(chosen = apply(select(., 1:ncol(policy_matrix)), FUN=sample, MARGIN = 1, x=colnames(policy_matrix), size=1, replace=FALSE))

  } else { # working with our own 2D matrix of simulated draws
    # utilities
    expU = exp(data.matrix(draws) %*% policy_matrix)
    pChosen = expU/rowSums(expU)
    
    # create grouping variable
    pChosen = as_tibble(pChosen) %>% # convert to tibble
      # add ID grouping variable (i.e., 2000 draws for each individual)
      mutate(ID = rep(1:(nrow(draws)/Ndraws), each = Ndraws)) %>% # add individual ID (N = 1000) 2000 draws for each individual
      # group by ID and calculate the average choice probability for each individual (integrate over the 2000 draws)
      group_by(ID) %>%
      summarise_all(mean) %>%
      # now probabilistically assign chosen alternative
      mutate(chosen=apply(select(., 2:(ncol(policy_matrix)+1)), FUN=sample, MARGIN = 1, x=colnames(policy_matrix), size=1, replace=FALSE))
      
  }
  
  pChosen = pChosen %>% 
    select(chosen) %>%
    group_by(chosen) %>%
    summarize(cnt=n()) %>% 
    mutate(freq=cnt/sum(cnt)) %>%
    select(chosen, freq) %>%
    {'names<-'(.$freq, .$chosen)}
  
  
  if(est_b == TRUE)
  {
    return(pChosen[1])
  } 
  else
  {
    return(pChosen) # return named vector
  }
  
}

# calculate a_hat
a_hat = optimize(function(x) abs(S - S_hat(x, draws = simdraws, policy_matrix = init_policy, est_b = TRUE)), interval=c(-15, 15), maximum=FALSE)
#a_hat = 5.503315

# calculate uptake probabilities for policy scenarios and create dataset for plotting
res = bind_rows(lapply(policies, FUN = S_hat, a_hat = a_hat$minimum, draws = simdraws)) %>% # uptake probabilities & bind into tibble
  bind_cols(policy = POLICY) %>% # add policy variable
  pivot_longer(!policy, values_drop_na = TRUE) %>% #pivot longer to prepare data for ggplot
  mutate(name = factor(name, levels = CHOICE_reorder), policy = factor(policy, levels=POLICY))

mCols = c("#91ac9a", "#a9c3b6", "#cedfdf", "#b7d1d3", "#a6c3ce", "#8fb8ca")

mmnl_choices = ggplot(res, aes(fill=name, y=value, x=policy)) +
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent,
                     expand = expansion(mult = 0.0)) +
  scale_fill_manual(values = mCols) +
  scale_x_discrete(position = "top",
                   expand = expansion(mult = 0.1)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(fill = CHOICE_reorder) +
  theme_minimal() + 
  geom_text(aes(label = ifelse((value > 0.03), percent(value, accuracy = 1), "")), 
            size = 3,
            fontface = "bold",
            family = "Lato", 
            position = position_stack(vjust = 0.5)) + 
  theme(plot.title = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, color="black"),
        #plot.background = element_rect(fill = "green"),
        #panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size=9, face="bold", family="Lato"),
        axis.text.y=element_text(size=9, face="bold", family="Lato"),
        legend.position="none",
        legend.text=element_text(size=9),
        #axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.6, 0, 0.3, 0.1), "cm"))

print(mmnl_choices)
grid.circle(x = unit(0.05, "npc"), y = unit(0.96, "npc"), r = 0.03, gp = gpar(lwd=2))
grid.text("B", x = unit(0.05, "npc"), y = unit(0.96, "npc"), gp = gpar(fontface = "bold", fontsize=11))

mmnl_choices_plot = grid.grab()

ggsave(file="../../Manuscripts/mmnl_predictions.svg", device = "svg", 
       plot=mmnl_choices_plot, width=5, height=3, bg = "transparent")

################################################################################################################
##ALRETNATIVE APPROACH
###GENERATE MY OWN DRAWS, artificially increase sample size to allow for better convergence of simulation
Ndraws=2000      # set number of draws to use per person and per parameter
N=1000
dimensions=13    # define number of random terms in the model

# generate draws (MLHS)
set.seed(01980)
#draws2 = apollo_mlhs(N=Ndraws, d=dimensions, i=N)
draws2 = mlhs(N=Ndraws, d=dimensions, i=N)
colnames(draws2) = c("eta01","eta02","eta03","eta04",
                     "eta05","eta06","eta07","eta08",
                     "eta09","eta10","eta11","eta12","eta13")

# calculate random coefficients
simdraws2 = data.frame(t(beta[1:13] + tri %*% t(draws2)))
colnames(simdraws2) = PARAMS

# send directly to S_hat function
a_hat1 = optimize(function(x) abs(S - S_hat(x, draws = simdraws2, policy_matrix = init_policy, est_b = TRUE)), interval=c(-15, 15), maximum=FALSE)
#a_hat1 = 5.071097

#simdraws3=randCoeff(beta=beta, draws=data.frame(draws2))
#apply(simdraws2, 2, mean) == apply(data.frame(simdraws3), 2, mean) # approaches yield same result, use simdraws2

#rm(simdraws2, simdraws3)

# mimic set of of draws in APOLLO so I use the same function I wrote
split_sims=list()
# for each element of simdraws2, create a N=1000 individuals * Ndraws=2000 matrix of dimensions  = 13 random parameters
split_sims = lapply(simdraws2, FUN=matrix, nrow=N, ncol=Ndraws)

#is.null(dim(split_sims))

# calculate a_hat
a_hat2 = optimize(function(x) abs(S - S_hat(x, draws = split_sims, policy_matrix = init_policy, est_b = TRUE)), interval=c(-15, 15), maximum=FALSE)
# a_hat2 = 5.070037 don't know why they are slightly different

# calculate uptake probabilities for policy scenarios and create dataset for plotting
res2 = bind_rows(lapply(policies, FUN = S_hat, a_hat = a_hat2$minimum, draws = split_sims)) %>% # uptake probabilities & bind into tibble
  bind_cols(policy = POLICY) %>% # add policy variable
  pivot_longer(!policy, values_drop_na = TRUE) %>% #pivot longer to prepare data for ggplot
  mutate(name = factor(name, levels = CHOICE_reorder), policy = factor(policy, levels=POLICY))

mCols = c("#91ac9a", "#a9c3b6", "#cedfdf", "#b7d1d3", "#a6c3ce", "#8fb8ca")

mmnl_choices2 = ggplot(res2, aes(fill=name, y=value, x=policy)) +
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent,
                     expand = expansion(mult = 0.0)) +
  scale_fill_manual(values = mCols) +
  scale_x_discrete(position = "top",
                   expand = expansion(mult = 0.1)) +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(fill = CHOICE_reorder) +
  theme_minimal() + 
  geom_text(aes(label = ifelse((value > 0.03), percent(value, accuracy = 1), "")), 
            size = 3,
            fontface = "bold",
            family = "Lato", 
            position = position_stack(vjust = 0.5)) + 
  theme(plot.title = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, color="black"),
        #plot.background = element_rect(fill = "green"),
        #panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size=9, face="bold", family="Lato"),
        axis.text.y=element_text(size=9, face="bold", family="Helvetica Neue"),
        legend.position="none",
        legend.text=element_text(size=9),
        #axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.6, 0, 0.3, 0.1), "cm"))

print(mmnl_choices2)
grid.circle(x = unit(0.05, "npc"), y = unit(0.96, "npc"), r = 0.025, gp = gpar(lwd=2))
grid.text("B", x = unit(0.05, "npc"), y = unit(0.96, "npc"), gp = gpar(fontface = "bold", family = "Lato", fontsize=11))

mmnl_choices_plot2 = grid.grab()

ggsave(file="../../Manuscripts/mmnl_predictions2.svg", device = "svg", 
       plot=mmnl_choices_plot2, width=5, height=3, bg = "transparent")
