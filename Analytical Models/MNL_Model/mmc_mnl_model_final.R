rm(list=ls())

# load the libraries I need
library(apollo)
library(tidyverse)
library(grid)
library(svglite)
library(paletteer)
library(ggpubr)
library(scales)

# alot of output, so I set my default directory to where i can find my code to run the model 
#and output results in the same directory
setwd("../../Analytical Models/MNL_Model")

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

# some Apollo settings I need
apollo_initialise()
apollo_control = list(modelName ="mmc_mnl",
                      modelDescr ="MNL model",
                      indivID ="ID",
                      nCores=1)

# initial values
apollo_beta=c(neither = 0,
              permhc = 0,
              dist5 = 0,
              dist15 = 0,
              avail = 0,
              somepriv = 0, 
              notpriv = 0, 
              device = 0,
              voucher = 0,
              cash = 0, 
              pricenone = 0,
              pricevoucher = 0,
              pricecash = 0)

# which variables are fixed?
apollo_fixed = c()

#validate inputs
apollo_inputs = apollo_validateInputs()

# log likelihood function
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
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       hessianRoutine="numDeriv")

#apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings)

modelOutput_settings = list(printPVal=TRUE)
saveOutput_settings = list(printPVal=TRUE)

apollo_modelOutput(model, modelOutput_settings)
apollo_saveOutput(model, saveOutput_settings)

# ################################################################# #
#### POST ESTIMATION                                            ####
# ################################################################# #

model=apollo_loadModel("apollo_mnl")

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
  magrittr::set_rownames(names(apollo_beta)) %>%
  magrittr::set_colnames(CHOICE)

init_policy = policymat[,c(1,2)]
policy1 = policymat[,c(1,2,3)]
policy2 = policymat[,c(1,2,4)]
policy3 = policymat[,c(1,2,3,4)]
policy4 = policymat[,c(1,5,6)]
policy5 = policymat[,c(1,3,5,6)]

policies = list(init_policy, policy1, policy2, policy3, policy4, policy5)

###################################################################################################################################################################

# adjust model to replicate revealed shares
# what ASC will give a 30.8% circumcision probability (or 69.2% no circumcision probability), if only opt-out and surgical circumcision options are available?
# actual shares (Ref:  )
S = 1-sum(c(1250,466,334))/sum(c(3943,1093,1627))

# function to calculate a constant (a_hat) we add to the ASC such that model predicted MMC uptake matches RW MMC uptake
# function calculates expected MMC uptake, given the model (model) and the initial policies available, and _a_hat (set to TRUE to calculate a_hat)
S_hat = function(a_hat, model, policy_matrix, est_b = FALSE) {
  # calculate predicted shares assuming surgical circumcision is only option available
  betas = model$estimate
  betas[1] = betas[1] + a_hat
  expU = exp(betas %*% policy_matrix)
  P = expU/rowSums(expU)
  
  if(est_b == TRUE)
  {
    return(P[1])
  } 
  else
    {
      return(setNames(P[1, ], colnames(P))) # return named vector
    }
}

# calculate a_hat
a_hat = optimize(function(x) abs(S - S_hat(x, model = model, policy_matrix = init_policy, est_b = TRUE)), interval=c(-15, 15), maximum=FALSE)


# calculate uptake probabilities for policy scenarios and create dataset for plotting
res = bind_rows(lapply(policies, FUN = S_hat, a_hat = a_hat$minimum, model = model))  %>% # uptake probabilities & bind into tibble
  magrittr::set_colnames(CHOICE) %>% # add full column names
  bind_cols(policy = POLICY) %>% # add policy variable
  pivot_longer(!policy, values_drop_na = TRUE) %>% #pivot longer to prepare data for ggplot
  mutate(name = factor(name, levels = CHOICE_reorder), policy = factor(policy, levels=POLICY))

mCols = c("#91ac9a", "#a9c3b6", "#cedfdf", "#b7d1d3", "#a6c3ce", "#8fb8ca")

mnl_choices = ggplot(res, aes(fill=name, y=value, x=policy)) +
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

print(mnl_choices)
grid.circle(x = unit(0.05, "npc"), y = unit(0.96, "npc"), r = 0.025, gp = gpar(lwd=2))
grid.text("A", x = unit(0.05, "npc"), y = unit(0.96, "npc"), gp = gpar(fontface = "bold", fontsize=11, family="Lato"))

mnl_choices_plot = grid.grab()

ggsave(file="../../Manuscripts/mnl_predictions.svg", device = "svg", 
       plot=mnl_choices_plot, width=5, height=3, bg = "transparent")

# extract legend
plots_title = get_legend(mnl_choices + 
                           theme(legend.position="bottom",
                                 legend.text=element_text(size=9, face="bold", family="Lato"),
                                 legend.title=element_blank(),
                                 plot.margin=grid::unit(c(0,0,0,0), "mm"),
                                 #plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                 panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
                                 plot.background = element_rect(fill='transparent', color=NA)))

#leg = get_legend(mnl_choices)

as_ggplot(plots_title)
ggsave(file="../../Manuscripts/legend.svg", plot=as_ggplot(plots_title), width=5, height=0.5)
