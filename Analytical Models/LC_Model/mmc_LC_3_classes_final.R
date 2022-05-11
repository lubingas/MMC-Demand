rm(list=ls())

# load libraries
library(apollo)
library(car)
library(tidyverse)
library(grid)
library(svglite)
library(scales)
library(ggsci)

# alot of output, so I set my default directory to where i can find my code to run the model 
#and output results in the same directory
setwd("../../Analytical Models/LC_Model")

#update to tidyverse grammar (read data in as tibble)
# relative paths since I am working on my GIT project
database = read_csv("../../Data/cleanerdata_original.csv", col_names = TRUE)

database = database %>%
  mutate(pricenone1 = price1*(none1 == 1), pricenone2 = price2*(none2 == 1), # interaction terms
         pricevoucher1 = price1*(voucher1 == 1), pricevoucher2 = price2*(voucher2 == 1),
         pricecash1 = price1*(cash1 == 1), pricecash2 = price2*(cash2 == 1),
         ispoor = as.numeric((monthly_income-monthly_expenditure) < 60*3750), # # income above poverty line
         educ_pri_more  = abs(as.numeric(noeduc==1 | pri==1)-1),
         age_30_more    = as.numeric(last_age > 30)) %>%
  select(ID,task,choice, # I am only working with a few variables that define the design matrix
         permhc1,distance11,distance51,distance151,available1,device1,veryprivate1,someprivate1,notprivate1,none1,voucher1,cash1,price1,pricenone1,pricevoucher1,pricecash1,
         permhc2,distance12,distance52,distance152,available2,device2,veryprivate2,someprivate2,notprivate2,none2,voucher2,cash2,price2,pricenone2,pricevoucher2,pricecash2,
         age_30_more,educ_pri_more,ispoor,urban)


# initialize a few things we need in appollo
set.seed(01980)
apollo_initialise()

apollo_control = list(modelName ="mmc_LC_3_Classes",
                      modelDescr ="LC Model with 3 classes",
                      indivID ="ID",
                      nCores=4)

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
           "urban_C2","urban_C3")

#startvalues=rep(0,length(parnames))

# set starting values
startvalues=read.csv("lc_3_classes_estimates.csv")[,5]

# convert the above into a beta vector with names
apollo_beta=startvalues
names(apollo_beta)=parnames

apollo_fixed = c()

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

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
  V[["class_C2"]] = int_C2+age_C2*age_30_more+educ_C2*educ_pri_more+urban_C2*urban+ispoor_C2*ispoor
  V[["class_C3"]] = int_C3+age_C3*age_30_more+educ_C3*educ_pri_more+urban_C3*urban+ispoor_C3*ispoor
  
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

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #
apollo_inputs = apollo_validateInputs()


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach (apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta,apollo_inputs))
  
  ### Create list of probabilities P
  P=list()
  
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
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs, functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

estimate_settings=list(estimationRoutine="BFGS", maxIterations=1000, hessianRoutine="numDeriv")

#search_settings=list(nCandidates =25, smartStart=FALSE)

# identify best starting values for LC model :)
# apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, searchStart_settings = search_settings)

# estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings)

modelOutput_settings = list(printPVal=TRUE)
saveOutput_settings = list(printPVal=TRUE)

apollo_modelOutput(model, modelOutput_settings)
apollo_saveOutput(model, saveOutput_settings)

# ################################################################# #
#### POST ESTIMATION                                            ####
# ################################################################# #

model=apollo_loadModel("apollo_LC_3_Classes")


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

# some pre-processing for speedier calculations
# make list of class specific utilities for function
# class 1
class1=c("neither_C1","permhc_C1","dist5_C1","dist15_C1","avail_C1","somepriv_C1","notpriv_C1","device_C1","voucher_C1","cash_C1","pricenone_C1","pricevoucher_C1","pricecash_C1")

# class 2
class2=c("neither_C2","permhc_C2","dist5_C2","dist15_C2","avail_C2","somepriv_C2","notpriv_C2","device_C2","voucher_C2","cash_C2","pricenone_C2","pricevoucher_C2","pricecash_C2")

# class 3
class3=c("neither_C3","permhc_C3","dist5_C3","dist15_C3","avail_C3","somepriv_C3","notpriv_C3","device_C3","voucher_C3","cash_C3","pricenone_C3","pricevoucher_C3","pricecash_C3")

# put in list
classUtils_vec = list(class1, class2, class3)

# posterior class allocation probabilities (outside function so no need to calculate every time)
pi = apply(apollo_lcConditionals(model, apollo_probabilities, apollo_inputs), 2, mean)[-1] #-1 to remove first element of vector
pi[4] = sum(pi) # add total probability

# function to calculate a constant we add to the ASC such that model predicted MMC uptake matches RW MMC uptake
# I account for class allocation weights
S_hat = function(a_hat, model, classUtils, policy_matrix, classProb, est_b = FALSE) {
  
  # initialize P
  P = list()
  Q = list()

  for (i in 1:length(classUtils)) {
    
    # calculate predicted shares assuming surgical circumcision is only option available
    betas = model$estimate[classUtils[[i]]]
    betas[1] = betas[1] + a_hat
    expU = exp(betas %*% policy_matrix)
    P[[i]] = expU/rowSums(expU)*classProb[i]
    P[[i]] = setNames(P[[i]][1, ], colnames(P[[i]]))
    Q[[i]] = expU/rowSums(expU)
    Q[[i]] = setNames(Q[[i]][1, ], colnames(Q[[i]]))
    
  }
  
  P_avg = Reduce(`+`, P) # sum elements of list
  
  if(est_b == TRUE)
  {
    return(P_avg[1]) # return average probability of opt-out option
  } 
  else
  {
    Q[[length(classUtils) + 1]] = P_avg
    return(Q) # return list with choice probabilities for each class and averaged across classes
  }
  
}

# calculate a_hat
a_hat = optimize(function(x) abs(S - S_hat(x, model = model, classUtils = classUtils_vec, policy_matrix = init_policy, classProb = pi, est_b = TRUE)), interval=c(-15, 15), maximum=FALSE)
#a_hat = 2.948194

# calculate probabilities & last element of list is average predicted probability (probably more efficient ways to do this using lapply)
policy0_res = S_hat(a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec, policy_matrix = init_policy, classProb = pi)
policy1_res = S_hat(a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec,policy_matrix = policy1, classProb = pi)
policy2_res = S_hat(a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec,policy_matrix = policy2, classProb = pi)
policy3_res = S_hat(a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec,policy_matrix = policy3, classProb = pi)
policy4_res = S_hat(a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec,policy_matrix = policy4, classProb = pi)
policy5_res = S_hat(a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec,policy_matrix = policy5, classProb = pi)

# calculate uptake probabilities for policy scenarios and create nested list with data for plotting
res = lapply(policies, FUN = S_hat, a_hat = a_hat$minimum, model = model, classUtils = classUtils_vec, classProb = pi)


# now take the nested list and cycle through each class (including overall), bind those rows together
res = lapply(1:4, function(x) bind_rows(lapply(res, "[[", x)) %>%
               ## add policy label (it is all part of lapply to cycle through each element of sub list)
               bind_cols(policy = POLICY) %>%
               ## pivot longer to prepare dataset for ggplot
               pivot_longer(!policy, values_drop_na = TRUE) %>%
               mutate(name = factor(name, levels = CHOICE_reorder), policy = factor(policy, levels=POLICY)) %>%
               add_column(classProb = pi[x], class = ifelse(x < 4, paste0("class",x), "all")))


mCols = c("#91ac9a", "#a9c3b6", "#cedfdf", "#b7d1d3", "#a6c3ce", "#8fb8ca")

lc_all_classes = ggplot(res[[4]], aes(fill=name, y=value, x=policy)) +
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
            family = "Helvetica Neue", 
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
        axis.text.x=element_text(size=9, face="bold", family="Helvetica Neue"),
        axis.text.y=element_text(size=9, face="bold", family="Helvetica Neue"),
        legend.position="none",
        legend.text=element_text(size=9),
        #axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.6, 0, 0.3, 0.1), "cm"))

print(lc_all_classes)
grid.circle(x = unit(0.05, "npc"), y = unit(0.97, "npc"), r = 0.03, gp = gpar(lwd=2))
grid.text("C", x = unit(0.05, "npc"), y = unit(0.97, "npc"), gp = gpar(fontface = "bold", fontsize=11))

lc_all_classes_plot = grid.grab()

ggsave(file="../../Manuscripts/lc_all_classes_predictions.svg", device = "svg", 
       plot=lc_all_classes_plot, width=4.2, height=2.6, bg = "transparent")


## trickier, now to process data from the other each class and make complicated plot
plotdata = bind_rows(res[c(1:3)]) %>% # extract data for the classes
  # solve for width issues
  arrange(name, policy) %>% # arrange as class1, class2, class3 repetitively
  group_by(name, policy) %>%
  mutate(w = cumsum(classProb), wm = w - classProb, wt = wm + (w - wm)/2, mid = (w + wm)/2) %>%
  # solve for height issues
  arrange(policy, class) %>% # arrange by policy then preference i.e., group policy together then sort by name
  group_by(policy, class) %>%
  mutate(wx = cumsum(value), wxm = wx - value, wtx = wxm + (wx - wxm)/2, midy = 1-wtx)

lc_sep_classes = ggplot(plotdata, aes(ymin = 1-wxm)) + 
  scale_x_continuous(breaks = sort(unique(plotdata$mid)), 
                     labels = NULL, 
                     expand = expansion(mult = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent,
                     expand = expansion(mult = 0.01)) +
  geom_rect(stat="identity", aes(xmin = wm, xmax = w, fill=name, ymax=1-wx), color="white", size=0.3) +
  scale_fill_manual(values = mCols) +
  #labels=c("LC 1","LC 2","LC 3")) + 
  theme_minimal() + 
  geom_text(aes(x=mid, y=midy, label = ifelse((value > 0.03), percent(value, accuracy=1), "")), 
            size = 2, 
            fontface = "bold",
            family = "Helvetica Neue") +
  facet_grid(~policy) +
  theme(plot.title = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(size=9, face="bold", family="Helvetica Neue"),
        #axis.text.x=element_text(size=9),
        legend.position="none",
        strip.text.x = element_text(size = 9, face="bold", family="Helvetica Neue"),
        #axis.text.y=element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'transparent', color = NA), #transparent panel bg
        plot.background = element_rect(fill = 'transparent', color = NA), #transparent plot bg
        plot.margin = unit(c(0.6, 0, 0.3, 0.1), "cm"))

print(lc_sep_classes)
grid.circle(x = unit(0.05, "npc"), y = unit(0.97, "npc"), r = 0.03, gp = gpar(lwd=2))
grid.text("D", x = unit(0.05, "npc"), y = unit(0.97, "npc"), gp = gpar(fontface = "bold", fontsize=11))

lc_sep_classes_plot = grid.grab()

ggsave(file="../../Manuscripts/lc_class_predictions.svg", plot=lc_sep_classes_plot, width=5, height=3, bg = "transparent")
