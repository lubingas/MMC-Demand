rm(list=ls())

library(apollo)
library(data.table)
library(ggplot2)
library(ggpubr)
library(plotly)
library(scales)
library(ggthemes)
library(tidyverse)
library(grid)

setwd("/Users/slubinga/MMC-Demand/MNL_Model/")

apollo_initialise()

apollo_control = list(modelName ="mmc_mnl",
                      modelDescr ="MNL model",
                      indivID ="ID",
                      nCores=1)

# up one directory to get dataset
database = read.csv("../cleanerdata.csv",header=TRUE)

# interaction terms
#database$price1=-1*database$price1
#database$price2=-1*database$price2

database$pricenone1=database$price1*(database$none1==1)
database$pricenone2=database$price2*(database$none2==1)

database$pricevoucher1=database$price1*(database$voucher1==1)
database$pricevoucher2=database$price2*(database$voucher2==1)

database$pricecash1=database$price1*(database$cash1==1)
database$pricecash2=database$price2*(database$cash2==1)

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

apollo_fixed = c()

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
nocircumcision = c(1,0,0,0,0,0,0,0,0,0,0,0,0)
surgery        = c(0,1,0,1,2,0,1,0,0,0,0,0,0)       # 1) Introduction of circumcision services at permanent facilities
outreach       = c(0,0,0,0,5,1,0,0,0,0,0,0,0)       # 2) Addition of out-reach services within 1 km distance, surgeons are available 5 days a week, some privacy
devices        = c(0,1,0,1,2,0,1,1,0,0,0,0,0)       # 3) Introduction of Circumcision Devices at Permanent Health Facilities
incent_device  = c(0,1,0,1,2,0,1,1,0,1,0,0,-12)     # 4) Incentive payment of US$12 at permanent facilities with devices
incent_surgery = c(0,1,0,1,2,0,1,0,0,1,0,0,-8)      # 5) Incentive payment of US$8 at permanent facilities with surgery

policymat=cbind(nocircumcision,surgery,outreach,devices,incent_device,incent_surgery)
rownames(policymat)=c("neither","permhc","dist5","dist15","avail","somepriv","notpriv","device","voucher","cash","pricenone","pricevoucher","pricecash")
###################################################################################################################################################################


####################################################
# adjust model to replicate revealed shares
# what ASC will give a 30.8% circumcision rate, if only opt-out and surgical circumcision options are available?
# actual shares
S=sum(c(1250,466,334))/sum(c(3943,1093,1627))

# starting beta vector
betas=model$estimate

# function to calculate a constant we add to the ASC such that model predicted MMC uptake matches RW MMC uptake
S_hat=function(b) {
  # calculate predicted shares
  betas["neither"]=betas["neither"] + b
  expU=exp(betas %*% policymat[,c(1,2)])
  (expU[2]/rowSums(expU))
}
g <- function(x) abs(S - S_hat(x))
a = optimize(g, interval=c(-15, 15), maximum=FALSE)


newbetas = betas
newbetas["neither"] = newbetas["neither"] + a$minimum

# CHECK THAT WE GOT THE CORRECT MARKET SHARES AFTER AVERAGING OVER DRAWS
# utilities
expU_try=exp(newbetas %*% policymat[,c(1,2)])
expU_try/rowSums(expU_try)

# Now conduct policy analysis i.e., predicted probabilities for different policy scenarios
# calculate exponentiated utilities again and delete INF rows
expU=exp(newbetas %*% policymat)

# INITIAL STATES
P1=expU[,c(1,2)]/sum(expU[,c(1,2)])   # choice probabilities

# FUTURE STATES
# outreach
P2=expU[,c(1,2,3)]/sum(expU[,c(1,2,3)])   # choice probabilities

# devices
P3=expU[,c(1,2,4)]/sum(expU[,c(1,2,4)])   # choice probabilities

# devices + outreach
P4=expU[,c(1,2,3,4)]/sum(expU[,c(1,2,3,4)])   # choice probabilities

# incentives
P5=expU[,c(1,5,6)]/sum(expU[,c(1,5,6)])   # choice probabilities

# incentives + outreach
P6=expU[,c(1,3,5,6)]/sum(expU[,c(1,3,5,6)])   # choice probabilities

# some data manupilations so it is easy to plot the data
l = list(P1, P2, P3, P4, P5, P6)
do.call(rbind, lapply(l, function(x) x[match(c("nocircumcision","surgery","outreach","devices","incent_device","incent_surgery"), names(x))]))

res=t(do.call(rbind, lapply(l, function(x) x[match(c("nocircumcision","surgery","outreach","devices","incent_device","incent_surgery"), names(x))])))
res[is.na(res)] = 0

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
       fill = "",
       x = "",
       title = "") +
  theme_minimal() + 
  geom_text(aes(label = ifelse((res > 0.03), percent(res), "")), 
            size = 6, 
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x=element_text(size=18),
        axis.text.y=element_text(size = 18),
        legend.position="none",
        legend.text=element_text(size=18),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

postscript(file="mnl_predictions.eps", horizontal=FALSE, width=4.5, height=2.5, paper="special")
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
  #geom_label(x=0, y=1.1, label="A", fill = "white", fontface = "bold") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        legend.position="none",
        legend.text=element_text(size=9),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pdf(file="mnl_predictions.pdf", width=4.2, height=2.6, paper="special")
par(mar=c(0,0,0,0))
p
grid.circle(x = unit(0.06, "npc"), y = unit(0.93, "npc"), r = 0.04, gp = gpar(lwd=2))
grid.text("A", x = unit(0.06, "npc"), y = unit(0.93, "npc"), gp = gpar(fontface = "bold", fontsize=10))
dev.off()

tiff(file="mnl_predictions.tiff", width=4.2, height=2.6, units="in", res=1200)
par(mar=c(0,0,0,0))
p
grid.circle(x = unit(0.06, "npc"), y = unit(0.93, "npc"), r = 0.04, gp = gpar(lwd=2))
grid.text("A", x = unit(0.06, "npc"), y = unit(0.93, "npc"), gp = gpar(fontface = "bold", fontsize=10))
dev.off()



# Extract the legend. Returns a gtable
p = ggplot(plotdata, aes(fill=pref, y=res, x=policy)) + 
  geom_bar(stat="identity", position="fill", color="white") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(position = "top") + 
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(fill = "CHOICE") +
  theme_minimal() + 
  geom_text(aes(label = ifelse((res > 0.03), percent(res), "")), 
            size = 3, 
            position = position_stack(vjust = 0.5)) + 
  #geom_label(x=0, y=1.1, label="A", fill = "white", fontface = "bold") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        legend.position="bottom",
        legend.text=element_text(size=9),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


leg=get_legend(p)

# Convert to a ggplot and print
pdf(file="legend.pdf", width=5, height=0.5, paper="special")
par(mar=c(0,0,0,0))
as_ggplot(leg)
dev.off()





