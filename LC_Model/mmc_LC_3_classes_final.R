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


setwd("/Users/slubinga/MMC-Demand/LC_Model/")

apollo_initialise()

apollo_control = list(modelName ="mmc_LC_3_Classes",
                      modelDescr ="LC Model with 3 classes",
                      indivID ="ID",
                      nCores=4)

# up one directory to get dataset
database = read.csv("../cleanerdata.csv",header=TRUE)

# interaction terms
database$pricenone1=database$price1*(database$none1==1)
database$pricenone2=database$price2*(database$none2==1)

database$pricevoucher1=database$price1*(database$voucher1==1)
database$pricevoucher2=database$price2*(database$voucher2==1)

database$pricecash1=database$price1*(database$cash1==1)
database$pricecash2=database$price2*(database$cash2==1)

## set poverty line in $USD
m0 = 60*3750
database$ispoor         = as.numeric((database$monthly_income-database$monthly_expenditure) < m0)
database$educ_pri_more  = abs(as.numeric(database$noeduc==1 | database$pri==1)-1)
database$age_30_more    = as.numeric(database$last_age > 30)

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

estimate_settings=list(estimationRoutine="BFGS",
                       maxIterations=1000,
                       hessianRoutine="numDeriv")

search_settings=list(nCandidates =25,
                       smartStart=FALSE)

# identify best starting values for LC model :)
apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed, 
                                 apollo_probabilities, apollo_inputs,
                                 searchStart_settings = search_settings)

# estimate model
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings)

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
nocircumcision= c(1,0,0,0,0,0,0,0,0,0,0,0,0)
surgery=   c(0,1,0,1,2,0,1,0,0,0,0,0,0)       # 1) Introduction of circumcision services at permanent facilities
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

# starting beta vector
coefs=model$estimate

# extract class specific utilities from betas
# class 1
class1=c("neither_C1","permhc_C1","dist5_C1","dist15_C1","avail_C1","somepriv_C1","notpriv_C1","device_C1","voucher_C1","cash_C1","pricenone_C1","pricevoucher_C1","pricecash_C1")
coefs_C1=coefs[class1]

# class 2
class2=c("neither_C2","permhc_C2","dist5_C2","dist15_C2","avail_C2","somepriv_C2","notpriv_C2","device_C2","voucher_C2","cash_C2","pricenone_C2","pricevoucher_C2","pricecash_C2")
coefs_C2=coefs[class2]

# class 3
class3=c("neither_C3","permhc_C3","dist5_C3","dist15_C3","avail_C3","somepriv_C3","notpriv_C3","device_C3","voucher_C3","cash_C3","pricenone_C3","pricevoucher_C3","pricecash_C3")
coefs_C3=coefs[class3]

# posterior class allocation probabilities
pi = apply(apollo_lcConditionals(model,apollo_probabilities, apollo_inputs),2,mean)
all_pi=apollo_lcConditionals(model,apollo_probabilities, apollo_inputs)


# function to calculate a constant we add to the ASC such that model predicted MMC uptake matches RW MMC uptake
# I account for class allocation weights

S_hat=function(b) {
  # calculate predicted shares for class 1
  coefs_C1["neither_C1"]=coefs_C1["neither_C1"] + b
  expU_C1=exp(coefs_C1 %*% policymat[,c(1,2)])
  
  # calculate predicted shares for class 2
  coefs_C2["neither_C2"]=coefs_C2["neither_C2"] + b
  expU_C2=exp(coefs_C2 %*% policymat[,c(1,2)])
  
  # calculate predicted shares for class 3
  coefs_C3["neither_C3"]=coefs_C3["neither_C3"] + b
  expU_C3=exp(coefs_C3 %*% policymat[,c(1,2)])
  
  #return weighted probability of choosing circumcision
  ((expU_C1[2]/rowSums(expU_C1))*pi[1] + (expU_C2[2]/rowSums(expU_C2))*pi[2] + (expU_C3[2]/rowSums(expU_C3))*pi[3])
}
g <- function(x) abs(S - S_hat(x))
a = optimize(g, interval=c(-15, 15), maximum=FALSE)
b=a$minimum


# create a function that returns a list with matrix and vector of class specific choice probabilities
# small function to combine rows used in the larger function below
combine_rows <- function(data, row1, row2) {
  data[row2, ] <- data[row1, ] + data[row2, ]
  data[-row1, ]
}


Pfunc=function(beta, b){
  # get class probability
  classprop=ifelse(grepl("_C1", names(beta)[1]), pi[1],ifelse(grepl("_C2", names(beta)[1]), pi[2], pi[3]))
  
  # adjust b0 to reflect current situation
  beta[grep("neither", names(beta))]=beta[grep("neither", names(beta))] + b
  
  # Now conduct policy analysis i.e., predicted probabilities for different policy scenarios
  # calculate exponentiated utilities again and delete INF rows
  expU=exp(beta %*% policymat)
  
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
  res=t(do.call(rbind, lapply(l, function(x) x[match(c("nocircumcision","surgery","outreach","devices","incent_device","incent_surgery"), names(x))])))
  res[is.na(res)] = 0
  
  #res=combine_rows(res, 5, 6)
  #res=combine_rows(res, 5, 4)
  
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
  #anno=c("C",rep("",35))
  
  plotdata=data.frame(policy,pref,classprop=classprop,res=res_vec)
  
  return(list(resmat=res,res=res_vec,plotdata=plotdata))
}

class1=Pfunc(coefs_C1,b)
class2=Pfunc(coefs_C2,b)
class3=Pfunc(coefs_C3,b)

pooledplotdata=data.frame(policy=class1$plotdata$policy,
                          pref=class1$plotdata$pref,
                          res=class1$plotdata$classprop*class1$plotdata$res+class2$plotdata$classprop*class2$plotdata$res+class3$plotdata$classprop*class3$plotdata$res)

pi[1]*class1$resmat+pi[2]*class2$resmat+pi[3]*class3$resmat

q = ggplot(pooledplotdata, aes(fill=pref, y=res, x=policy)) + 
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
  geom_text(aes(label = ifelse((res > 0.05), percent(res), "")), 
            size = 6, 
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x=element_text(size=18),
        axis.text.y=element_text(size = 18),
        legend.position="none",
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

postscript(file="lc_predictions.eps", horizontal=FALSE, width=9, height=6)
q
dev.off()

q2 = ggplot(pooledplotdata, aes(fill=pref, y=res, x=policy)) + 
  geom_bar(stat="identity", position="fill", color="white") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(position = "top") +
  theme_minimal() + 
  geom_text(aes(label = ifelse((res > 0.05), percent(res), "")), 
            size = 3, 
            position = position_stack(vjust = 0.5)) +
  #geom_label(x=0, y=1.1, label="D", fill = "white", fontface = "bold") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        legend.position="none",
        legend.text=element_text(size=9),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(file="lc_predictions.pdf", width=4.2, height=2.6, paper="special")
par(mar=c(0,0,0,0))
q2
grid.circle(x = unit(0.06, "npc"), y = unit(0.93, "npc"), r = 0.04, gp = gpar(lwd=2))
grid.text("D", x = unit(0.06, "npc"), y = unit(0.93, "npc"), gp = gpar(fontface = "bold", fontsize=10))
dev.off()


### pooled analysis
plotdata=rbind(class1$plotdata,class2$plotdata,class3$plotdata)

#plotdata$cumsum <- do.call(c, tapply(plotdata$classprop, plotdata$group1, FUN=cumsum))

# order by class
plotdata2=plotdata[order(plotdata$pref, plotdata$policy),]

plotdata2$w=do.call(c, tapply(plotdata2$classprop, 
                              paste0(plotdata2$pref, plotdata2$policy), 
                              FUN=cumsum))

plotdata2$wm=plotdata2$w - plotdata2$classprop

plotdata2$wt=with(plotdata2, wm + (w - wm)/2)
plotdata2$mid=with(plotdata2, (w + wm)/2)

# order again by policy then pref
plotdata3=plotdata2[order(plotdata2$policy, plotdata2$class),]

plotdata3$wx=do.call(c, tapply(plotdata3$res, 
                               paste0(plotdata3$policy, plotdata3$class), 
                               FUN=cumsum))

plotdata3$wxm=plotdata3$wx - plotdata3$res

plotdata3$wtx <- with(plotdata3, wxm + (wx - wxm)/2)



p = ggplot(plotdata3, aes(ymin = 1-wxm, fill=pref)) + 
  geom_rect(stat="identity", aes(xmin = wm, xmax = w, fill=pref, ymax=1-wx),color="white") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = sort(unique(plotdata3$mid)), 
                     labels = NULL) +
  #labels=c("LC 1","LC 2","LC 3")) + 
  labs(y = "", 
       fill = "Choice",
       x = "",
       title = "") +
  theme_minimal() + 
  geom_text(aes(x=mid, y=res, label = ifelse((res > 0.05), percent(res), "")), 
            size = 4.5, 
            position = position_stack(vjust = 0.5)) +
  facet_grid(~policy) +
  theme(strip.text.x = element_text(size = 14),
        axis.text.y=element_text(size = 18),
        legend.position="none",
        axis.text.x=element_text(color = "black", size=9, angle=90, vjust=0.5, hjust=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

postscript(file="lc_predictions_classes.eps", horizontal=FALSE, width=9, height=6)
p
dev.off()


p2 = ggplot(plotdata3, aes(ymin = 1-wxm, fill=pref)) + 
  geom_rect(stat="identity", aes(xmin = wm, xmax = w, fill=pref, ymax=1-wx),color="white") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     labels = percent) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = sort(unique(plotdata3$mid)), 
                     labels = NULL) +
  #labels=c("LC 1","LC 2","LC 3")) + 
  theme_minimal() + 
  geom_text(aes(x=mid, y=res, label = ifelse((res > 0.05), percent(res), "")), 
            size = 2, 
            position = position_stack(vjust = 0.5)) +
  facet_grid(~policy) +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x=element_text(size=9),
        axis.text.y=element_text(size=9),
        legend.position="none",
        strip.text.x = element_text(size = 8),
        #axis.text.y=element_text(size = 18),
        axis.text.x=element_text(color = "black", size=9, angle=90, vjust=0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

pdf(file="lc_predictions_classes.pdf", width=4.2, height=2.6, paper="special")
par(mar=c(0,0,0,0))
p2
grid.circle(x = unit(0.06, "npc"), y = unit(0.93, "npc"), r = 0.04, gp = gpar(lwd=2))
grid.text("C", x = unit(0.06, "npc"), y = unit(0.93, "npc"), gp = gpar(fontface = "bold", fontsize=10))
dev.off()



