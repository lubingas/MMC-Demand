rm(list=ls())

# set the working directory
setwd("/Users/slubinga/Documents/GitHub/MMC-Demand/SEM_Model")

library(tidyverse)
library(polycor)
library(mvtnorm)
library(psych)
library(GPArotation)
library(psychTools)
library(corrplot)
library(scales)
library(ggthemes)
library(grid)
library(lattice)
library(latticeExtra)
library(HH)
library(lavaan)
library(semPlot)

#####################################################################
#####################################################################
# DATA INPUT                                                        #
#####################################################################
#####################################################################

# define name of data file - note that observations for the  same individual should be grouped together
datafilename="../cleanerdata.csv"

# read data file (use appropriate line depending on file type)
data=read.csv(datafilename)    # use for csv files

factdata=subset(data,task==1,select=c(ID,task,last_age,age25,age25_35,age35,
                                      resp_tribe,muganda,kiga_nkole,musoga,other_tribe,
                                      resp_religion,catholic,protestant,other_relig,
                                      school_level,school_years,noeduc,pri,olev,alev,
                                      marital_status,married,nevermarried,other_marital,
                                      nochild,children_fathered,
                                      urban,
                                      occupation,fisher,trader,farmer,boda,occup_other,nooccup,
                                      monthly_income,monthly_expenditure,abs_income,ispoor,
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
                                      sub1,sub2,sub3,sub4,sub5,sub6,sub7,sub8,sub9,sub10,sub11,sub12,
                                      bleed_device,infection_device,pain_proc_device,pain_heal_device, 
                                      bleed_surgery,infection_surgery,pain_proc_surgery,pain_heal_surgery,
                                      bleed,infection,pain_proc,pain_heal,
                                      time_heal_device_avg,time_work_device_avg,time_sex_device_avg,
                                      time_heal_surgery_avg,time_work_surgery_avg,time_sex_surgery_avg,
                                      time_heal,time_work,time_sex,
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
                                      numeracy1,numeracy2,numeracy3,numeracy4,
                                      svscore))


# first some factor analysis 
CONTROL=subset(factdata,select=c(cb1,cb2,cb3,cb4,cb5,cb6,cb7,cb8,cb9,cb10,cb11,
                                 cb12,cb13,cb14,cb15,cb16,cb17,cb18,cb19,cb20,cb21))

EFFICACY=subset(factdata,select=c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,
                                 e12,e13,e14,e15,e16,e17,e18,e19,e20,e21))

NORMS=subset(factdata,select=c(injunctive1,injunctive2,injunctive3,injunctive4,injunctive5,injunctive6,injunctive7,
                              descriptive1,descriptive2,descriptive3,descriptive4))

LITERACY=subset(factdata,select=c(read_news_eng,read_news_lug,read_letter_eng,read_letter_lug,read_phone_eng,read_phone_lug,
                                  write_letter_eng,write_letter_lug,write_phone_eng,write_phone_lug,
                                  numeracy1,numeracy2,numeracy3,numeracy4))

TRAITS=subset(factdata,select=c(conscientiousness1,conscientiousness2,conscientiousness3,conscientiousness4,conscientiousness5,conscientiousness6,conscientiousness7,conscientiousness8,conscientiousness9,
                                 selfconsciousness1,selfconsciousness2,selfconsciousness3,selfconsciousness4,selfconsciousness5,selfconsciousness6,
                                 extraversion1,extraversion2,extraversion3,extraversion4,extraversion5,extraversion6,extraversion7,extraversion8))

LOC=subset(factdata,select=c(loc1,loc2,loc3,loc4,loc5,loc6))

ATTITUDE=subset(factdata,select=c(sub1,sub2,sub3,sub4,sub5,sub6,sub7,sub8,sub9,sub10,sub11,sub12))

OUTCOMES=subset(factdata,select=c(bleed_surgery,infection_surgery,pain_proc_surgery,pain_heal_surgery,
                                  time_heal_surgery_avg,time_work_surgery_avg,time_sex_surgery_avg,
                                  bleed_device,infection_device,pain_proc_device,pain_heal_device,
                                  time_heal_device_avg,time_work_device_avg,time_sex_device_avg))



outcomes_n=fa.parallel(OUTCOMES, fm = "pa")
threefactor_o=fa(OUTCOMES, nfactors = 5, rotate = "oblimin", fm="pa")
print(threefactor_o)
print(threefactor_o$loadings)
fa.diagram(threefactor_o)
#hist(ATTITUDE$time_heal_surgery_avg)
# ################################################################# #
#### EXPLORAROTY FACTOR ANALYSIS USING PSYCH PACKAGE             ####
# ################################################################# #
## WEALTH INDEX
WEALTH_FACTOR=fa.parallel(WEALTH, fm = "pa")

# try 5 factor solution
WEALTH_FACTOR_a=principal(WEALTH, nfactors = 8, rotate = "varimax")
print(WEALTH_FACTOR_a)
print(WEALTH_FACTOR_a$loadings, cutoff = 0.4)
fa.diagram(WEALTH_FACTOR_a, simple=FALSE, cut= 0.4)

# ################################################################# #
### 1. INSTRUMENTAL ATTITUDE
# ################################################################# #
## PLOT
a=sapply(ATTITUDE,table)
#a <- plyr::ldply(l, rbind)
a[is.na(a)]=0
#names(a) <- c("Item","Extremely difficult","Quite difficult","Neither/Not sure","Quite easy","Extremely easy")

#a$Item=factor(a$Item,levels=c("cb1","cb2","cb3","cb4","cb5","cb6","cb7","cb8","cb9","cb10","cb11","cb12","cb13","cb14","cb15","cb16","cb17","cb18","cb19","cb20","cb21"))

ax=reshape2::melt(a)
names(ax)=c("Response","Item","Count")
ax$Response=factor(ax$Response)
#ax$Response=factor(ax$Response,levels=c("Extremely easy","Quite easy","Neither/Not sure","Quite difficult","Extremely difficult"))

ggplot(ax, aes(x = Item, y = Count, fill = Response)) + 
  theme_bw() + 
  #scale_fill_economist() +
  #geom_bar(stat = "identity") +
  # or:
  geom_bar(position = position_fill(), stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  #scale_x_discrete(labels=idnames) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired",
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom",
        legend.title=element_blank())

### on polychoic covariance matrix! for non-normal data
attitude_n=fa.parallel(ATTITUDE, fm = "pa")
# suggests 4 factors

# ended up with 3 factors for better interpretation
## best solution, as all factors make sense
## 4 FACTOR SOLUTION
fourfactor_a=fa(ATTITUDE, nfactors = 4, rotate = "oblimin", fm="pa")
print(fourfactor_a)
print(fourfactor_a$loadings)
fa.diagram(fourfactor_a)

# factor 1: SEX-RELATED BENEFITS
#           sub6 - improve your sexual life
#           sub7 - enable you to last longer during sex 
#           sub8 - enhance sexual pleasure for your partner 

# factor 2: SEXUALITY CONCERNS
#           sub10 - reduce your sexual pleasure 
#           sub11 - reduce your libido
#           sub12 - cause you to become infertile

# factor 3: STD PREVENTION BENEFITS
#           sub1 - protect you from HIV
#           sub2 - protect you from other STDs 
#           sub3 - protect your partner from HIV 

# factor 4: NON-SEX BENEFITS
#           sub4 - protect your partner from cervical cancer 
#           sub5 - improve your personal hygiene 
#           sub9 - increase your chances of getting a sexual partner

# ################################################################# #
# STRUCTURAL MODEL 1: ONLY ATTITUDES
# ################################################################# #
M_ATTITUDES1 <-  '# MEASUREMENT EQUATIONS
                  # INSTRUMENTAL ATTITIDES
                    SEXBENEFITS     =~ sub6 + sub7 + sub8 + sub9
                    STDPREVENTION   =~ sub1 + sub2 + sub3 + sub5
                    #OTHERBENEFITS   =~ sub4 + sub5 + sub9

                    #POSITIVE        =~ SEXBENEFITS + STDPREVENTION

                    NEGATIVE        =~ sub10 + sub11 + sub12

                    #ATTITUDE        =~ SEXBENEFITS + STDPREVENTION + NEGATIVE

                    # STRUCTURAL EQUATIONS
                    SEXBENEFITS   ~ age35 + promot_materials + friend_rec + family_rec 
                    STDPREVENTION ~ age35 + noeduc + pri + olev 
                    #DISADVANTAGES ~ age35 + urban + noeduc + pri + olev
                    #OTHERBENEFITS ~ age35 + fisher + urban + family_circum + promot_materials + friend_rec + family_rec
                
                    #POSITIVE      ~ age35 + urban + family_circum + promot_materials + friend_rec + family_rec + noeduc + pri + olev 
                    NEGATIVE      ~ age35 + urban + family_circum + promot_materials + friend_rec + family_rec + noeduc + pri + olev 

                    #circum_intent  ~ ATTITUDE
                    circum_intent  ~ SEXBENEFITS + STDPREVENTION + NEGATIVE
          
                    #POSITIVE  ~~ NEGATIVE
SEXBENEFITS ~~ STDPREVENTION + NEGATIVE
STDPREVENTION ~~ NEGATIVE

                    '

RX_ATTITUDES1 <- cfa(M_ATTITUDES1, data=factdata,
                    ordered=c("circum_intent",
                              "sub1","sub2","sub3","sub4","sub5","sub6",
                              "sub7","sub8","sub9","sub10","sub11","sub12",
                              "bleed","infection","pain_proc","pain_heal"))

summary(RX_ATTITUDES1, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)
semPaths(RX_ATTITUDES1, what = "col", whatLabels = "par", style = "mx",
         intercepts=FALSE, thresholds=FALSE, residuals=TRUE,
         rotation = 2, layout = "tree", mar = c(3, 4, 3, 4), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

lavInspect(RX_ATTITUDES1, what = "implied")
lavInspect(RX_ATTITUDES1, what = "sampstat")
lavInspect(RX_ATTITUDES1, what = "resid")
semTools::reliabilityL2(RX_ATTITUDES1, secondFactor = "ATTITUDE")

fitted(RX_ATTITUDES)

# ################################################################# #
### 2. CONTROL BELIEFS
# ################################################################# #
## PLOT
l=sapply(CONTROL,table)
a=t(plyr::ldply(l, rbind,.id=NULL))
a[is.na(a)]=0
#rownames(a)=c("Item","Extremely difficult","Quite difficult","Neither/Not sure","Quite easy","Extremely easy")
colnames(a)=c("cb1","cb2","cb3","cb4","cb5","cb6","cb7","cb8","cb9","cb10","cb11","cb12","cb13","cb14","cb15","cb16","cb17","cb18","cb19","cb20","cb21")

ax=reshape2::melt(a)
names(ax)=c("Response","Item","Count")
ax$Response=factor(ax$Response)

ggplot(ax, aes(x = Item, y = Count, fill = Response)) + 
  theme_bw() + 
  #scale_fill_economist() +
  #geom_bar(stat = "identity") +
  # or:
  geom_bar(position = position_fill(), stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  #scale_x_discrete(labels=idnames) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired",
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom",
        legend.title=element_blank())


### on polychoic covariance matrix! for non-normal data
parallel=fa.parallel(CONTROL, fm = "pa")
# suggests 2-5 factors

## best solution, as all factors make sense
# try 5 factor solution
CONTROL_a=fa(CONTROL, nfactors = 5, rotate = "oblimin", fm="pa")
print(CONTROL_a)
print(CONTROL_a$loadings)
fa.diagram(CONTROL_a)

## 5 FACTOR MODEL
## control beliefs are based on 6 important factors

# factor 1: SOCIAL BARRIERS
#           c17 - it was against your culture
#           c18 - it was against your religion
#           c19 - your partner was against it
#           c21 - your partner may think you will seek pleasure elsewhere

# factor 2: FEAR OF SURGICAL PROCEDURES
#           c13 - circumcision was described as painful
#           c14 - there were reported cases of complications
#           c15 - surgery makes you nervous 
#           c16 - your are afraid of needles

# factor 3: PRIVACY CONCERNS
#           c2  - Only offered at outreach clinics
#           c3  - only available in facilities that are far away from where you live **
#           c5  - cannot have it done privately
#           c7  - did not know what happens to the foreskin after circumcision **
#           c10 - Not sure that you will find the doctor when you go for your circumcision

# factor 4: INCENTIVE COMPATIBILITY/REASSURANCE
#           c9  - A transportation refund was provided
#           c11 - you first had to be tested for HIV ** leave out
#           c12 - clinic staff explained how circumcision is done and pain is prevented
#           c20 - you know people who are circumcised

# factor 5: TRVEL AND COST CONCERNED *** CAN LEAVE OUT
#           c1 - Only offered in health center IV facilities * leaveout if combine with 2
#           c4 - if you had to visit the clinic more than once
#           c6 - not free to you

###############################################################
### CFA
###############################################################
# with correlation
M_CONTROL <-  ' # MEASUREMENT EQUATIONS
                # CONTROL BELIEFS
                SOCIALBARS     =~ cb17+cb18+cb19+cb21
                FEARSURGERY    =~ cb13+cb14+cb15+cb16
                PRIVACY        =~ cb9+cb11+cb12+cb20
                TRAVELCOST     =~ cb1+cb4+cb6
                CONCERN        =~ SOCIALBARS + FEARSURGERY + PRIVACY + TRAVELCOST

                REASSURANCE    =~ cb2+cb3+cb5+cb7+cb10

                # STRUCTURAL EQUATIONS
                #SOCIALBARS    ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                #FEARSURGERY   ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec 
                #PRIVACY       ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                REASSURANCE    ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                #TRAVELCOST    ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                CONCERN        ~ age35 + fisher + promot_materials + discuss_partner + partner_rec + villagehead_rec
                
                #circum_intent  ~ SOCIALBARS + FEARSURGERY + PRIVACY + REASSURANCE + TRAVELCOST
                circum_intent  ~ CONCERN + REASSURANCE

                CONCERN ~~ REASSURANCE

                '


RX_CONTROL <- cfa(M_CONTROL,data=factdata,
                     ordered=c("circum_intent",
                               "cb1","cb2","cb3","cb4","cb5","cb6","cb7","cb8","cb9","cb10","cb11","cb12",
                               "cb13","cb14","cb15","cb16","cb17","cb18","cb19","cb20","cb21"))

summary(RX_CONTROL, fit.measures=TRUE, standardized=TRUE)
semPaths(RX_CONTROL, what = "col", whatLabels = "par", style = "mx",
         intercepts=FALSE, thresholds=FALSE, residuals=TRUE,
         rotation = 2, layout = "tree", mar = c(3, 4, 3, 4), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

lavInspect(RX_CONTROL, what = "implied")
lavInspect(RX_CONTROL, what = "sampstat")
lavInspect(RX_CONTROL, what = "resid")
semTools::reliabilityL2(RX_CONTROL, secondFactor = "CONTROL")


### 
# ################################################################# #
### 4. NORMATIVE BELIEFS
# ################################################################# #
## PLOT
a=sapply(NORMS,table)
#a <- plyr::ldply(l, rbind)
a[is.na(a)]=0
#names(a) <- c("Item","Extremely difficult","Quite difficult","Neither/Not sure","Quite easy","Extremely easy")

#a$Item=factor(a$Item,levels=c("cb1","cb2","cb3","cb4","cb5","cb6","cb7","cb8","cb9","cb10","cb11","cb12","cb13","cb14","cb15","cb16","cb17","cb18","cb19","cb20","cb21"))

ax=reshape2::melt(a)
names(ax)=c("Response","Item","Count")
ax$Response=factor(ax$Response)
#ax$Response=factor(ax$Response,levels=c("Extremely easy","Quite easy","Neither/Not sure","Quite difficult","Extremely difficult"))

ggplot(ax, aes(x = Item, y = Count, fill = Response)) + 
  theme_bw() + 
  #scale_fill_economist() +
  #geom_bar(stat = "identity") +
  # or:
  geom_bar(position = position_fill(), stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  #scale_x_discrete(labels=idnames) +
  coord_flip() +
  scale_fill_brewer(palette = "Paired",
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom",
        legend.title=element_blank())


norm_p=fa.parallel(NORMS, fm = "pa")
# suggests 2 factors

# try 2 factor solution
two_norms=fa(NORMS, nfactors = 2, rotate = "oblimin", fm="pa") # settle for orthogonal rotation
print(two_norms)
print(two_norms$loadings)
fa.diagram(two_norms)

## 2 FACTOR MODEL
## control beliefs are based on 6 important factors

# factor 1: INJUNCTIVE NORMS
#           injunctive1 - Partner (wife or girlfriend) 
#           injunctive2 - Closest friend 
#           injunctive3 - Religious leader 
#           injunctive4 - Parents (or other relatives) 
#           injunctive5 - Tribal/cultural leader
#           injunctive6 - Village head/LC1 chairperson
#           injunctive7 - Community health worker

# factor 2: DESCRIPTIVE NORMS
#           descriptive1 - Father
#           descriptive2 - Brothers
#           descriptive3 - Close male friends
#           descriptive4 - Other male relatives 

###############################################################
### STRUCTURAL EQUATION MODELS WITHOUT HIGHER ORDER TRAITS
###############################################################

M_NORMS <-  '#  MEASUREMENT EQUATIONS
                  # SOCIAL NORMS
                  INJUNCTIVE     =~ injunctive1+injunctive2+injunctive3+injunctive4+injunctive5+injunctive6+injunctive7
                  DESCRIPTIVE    =~ descriptive1+descriptive2+descriptive3+descriptive4

                  NORMS          =~ INJUNCTIVE + DESCRIPTIVE

                  
                  # STRUCTURAL EQUATIONS
                  #INJUNCTIVE    ~ age35 + ispoor + fisher + urban + muganda + family_circum + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #DESCRIPTIVE   ~ age35 + ispoor + fisher + urban + muganda + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  NORMS          ~ age35 + urban + muganda + promot_materials + friend_rec + religlead_rec + villagehead_rec
                  
                  #circum_intent  ~ INJUNCTIVE + DESCRIPTIVE
                  circum_intent  ~ NORMS
                  
                  '


RX_NORMS <- sem(M_NORMS,data=factdata,
                ordered=c("circum_intent",
                          "injunctive1","injunctive2","injunctive3","injunctive4","injunctive5","injunctive6","injunctive7",
                          "descriptive1","descriptive2","descriptive3","descriptive4"))

summary(RX_NORMS, fit.measures=TRUE, standardized=TRUE)

semPaths(RX_NORMS, what = "col", whatLabels = "par", style = "mx",
         intercepts=FALSE, thresholds=FALSE, residuals=TRUE,
         rotation = 2, layout = "tree", mar = c(3, 4, 3, 4), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

lavInspect(RX_NORMS, what = "implied")
lavInspect(RX_NORMS, what = "sampstat")
lavInspect(RX_NORMS, what = "resid")
semTools::reliabilityL2(RX_NORMS, secondFactor = "NORMS")



###############################################################
### FULL STRUCTURAL EQUATION MODELS
###############################################################

M_FULL_FINAL <-  '#  MEASUREMENT EQUATIONS
                  # CONTROL BELIEFS
                  SOCIALBARS     =~ cb17+cb18+cb19+cb21
                  FEARSURGERY    =~ cb13+cb14+cb15+cb16
                  PRIVACY        =~ cb9+cb11+cb12+cb20
                  TRAVELCOST     =~ cb1+cb4+cb6
                  REASSURANCE    =~ cb2+cb3+cb5+cb7+cb10
                  CONTROL        =~ SOCIALBARS + FEARSURGERY + PRIVACY + TRAVELCOST + REASSURANCE
                  
                  # SOCIAL NORMS
                  INJUNCTIVE      =~ injunctive1+injunctive2+injunctive3+injunctive4+injunctive5+injunctive6+injunctive7
                  DESCRIPTIVE     =~ descriptive1+descriptive2+descriptive3+descriptive4
                  NORMS           =~ INJUNCTIVE + DESCRIPTIVE

                  # INSTRUMENTAL ATTITIDES
                  SEXBENEFITS     =~ sub6 + sub7 + sub8 + sub9
                  STDPREVENTION   =~ sub1 + sub2 + sub3 + sub5
                  #POSITIVE        =~ SEXBENEFITS + STDPREVENTION

                  NEGATIVE        =~ sub10 + sub11 + sub12
                  ATTITUDES       =~ SEXBENEFITS + STDPREVENTION + NEGATIVE

                  
                  # STRUCTURAL EQUATIONS
                  #CONCERN        ~ age35 + fisher + promot_materials + discuss_partner + partner_rec + villagehead_rec
                  #CONTROL        ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  CONTROL         ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  NORMS           ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  ATTITUDES       ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #TRAVELCOST    ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #REASSURANCE   ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #INJUNCTIVE    ~ age35 + urban + muganda + promot_materials + friend_rec + religlead_rec + villagehead_rec
                  #DESCRIPTIVE   ~ age35 + urban + muganda + promot_materials + friend_rec + religlead_rec + villagehead_rec
                  #SEXBENEFITS   ~ age35 + fisher + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec
                  #STDPREVENTION ~ age35 + fisher + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec
                  #NEGATIVE      ~ age35 + fisher + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec

                  UTILITY   ~ ATTITUDES + NORMS + CONTROL + age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  UTILITY   =~ 1*circum_intent

                  #circum_intent   ~ ATTITUDES + INJUNCTIVE + DESCRIPTIVE + CONCERN + REASSURANCE
                  UTILITY   ~~ 0*UTILITY
                  #ATTITUDES ~~ 1*ATTITUDES
                  #NORMS     ~~ 1*NORMS
                  #CONTROL   ~~ 1*CONTROL

                  # covariances
                  CONTROL ~~    NORMS + ATTITUDES
                  NORMS   ~~            ATTITUDES
                  
                  '

RX_FULL_FINAL <- sem(M_FULL_FINAL,data=factdata,
                     ordered=c("circum_intent",
                               "cb1","cb2","cb3","cb4","cb5","cb6","cb7","cb8","cb9","cb10","cb11","cb12","cb13","cb14","cb15","cb16","cb17","cb18","cb19","cb20","cb21",
                               "injunctive1","injunctive2","injunctive3","injunctive4","injunctive5","injunctive6","injunctive7","descriptive1","descriptive2","descriptive3","descriptive4",
                               "sub1","sub2","sub3","sub4","sub5","sub6","sub7","sub8","sub9","sub10","sub11","sub12"))

summary(RX_FULL_FINAL, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)

semPaths(RX_FULL_FINAL, what = "col", whatLabels = "par", style = "mx",
         intercepts=FALSE, thresholds=FALSE, residuals=TRUE,
         rotation = 2, layout = "tree", mar = c(3, 4, 3, 4), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

lavInspect(RX_FULL_FINAL, what = "implied")
lavInspect(RX_FULL_FINAL, what = "sampstat")
lavInspect(RX_FULL_FINAL, what = "resid")



## NO 2ND ORDER, ALLOW CORRELATIONS BETWEEN LVS
###############################################################

M_FULL_FINAL <-  '#  MEASUREMENT EQUATIONS
                  # CONTROL BELIEFS
                  SOCIALBARS     =~ cb17+cb18+cb19+cb21
                  FEARSURGERY    =~ cb13+cb14+cb15+cb16
                  PRIVACY        =~ cb9+cb11+cb12+cb20
                  TRAVELCOST     =~ cb1+cb4+cb6
                  REASSURANCE    =~ cb2+cb3+cb5+cb7+cb10
                  #CONTROL        =~ SOCIALBARS + FEARSURGERY + PRIVACY + TRAVELCOST + REASSURANCE
                  
                  # SOCIAL NORMS
                  INJUNCTIVE      =~ injunctive1+injunctive2+injunctive3+injunctive4+injunctive5+injunctive6+injunctive7
                  DESCRIPTIVE     =~ descriptive1+descriptive2+descriptive3+descriptive4
                  #NORMS           =~ INJUNCTIVE + DESCRIPTIVE
                  
                  # INSTRUMENTAL ATTITIDES
                  SEXBENEFITS     =~ sub6 + sub7 + sub8 + sub9
                  STDPREVENTION   =~ sub1 + sub2 + sub3 + sub5
                  #POSITIVE        =~ SEXBENEFITS + STDPREVENTION
                  
                  NEGATIVE        =~ sub10 + sub11 + sub12
                  #ATTITUDES       =~ SEXBENEFITS + STDPREVENTION + NEGATIVE
                  
                  
                  # STRUCTURAL EQUATIONS
                  #CONCERN        ~ age35 + fisher + promot_materials + discuss_partner + partner_rec + villagehead_rec
                  #CONTROL        ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #CONTROL         ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #NORMS           ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #ATTITUDES       ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #TRAVELCOST    ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #REASSURANCE   ~ age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  #INJUNCTIVE    ~ age35 + urban + muganda + promot_materials + friend_rec + religlead_rec + villagehead_rec
                  #DESCRIPTIVE   ~ age35 + urban + muganda + promot_materials + friend_rec + religlead_rec + villagehead_rec
                  #SEXBENEFITS   ~ age35 + fisher + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec
                  #STDPREVENTION ~ age35 + fisher + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec
                  #NEGATIVE      ~ age35 + fisher + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec
                  
                  #UTILITY   ~ ATTITUDES + NORMS + CONTROL + age35 + ispoor + fisher + urban + family_circum + promot_materials + friend_rec + family_rec + discuss_partner + partner_rec + religlead_rec + villagehead_rec + hw_rec
                  UTILITY    ~ SOCIALBARS + FEARSURGERY + PRIVACY + TRAVELCOST + REASSURANCE + INJUNCTIVE + DESCRIPTIVE + SEXBENEFITS + STDPREVENTION + NEGATIVE
                  
                  UTILITY   =~ 1*circum_intent
                  
                  #circum_intent   ~ ATTITUDES + INJUNCTIVE + DESCRIPTIVE + CONCERN + REASSURANCE + INJUNCTIVE + DESCRIPTIVE + SEXBENEFITS + STDPREVENTION + NEGATIVE
                  UTILITY   ~~ 0*UTILITY
                  #ATTITUDES ~~ 1*ATTITUDES
                  #NORMS     ~~ 1*NORMS
                  #CONTROL   ~~ 1*CONTROL
                  
                  # covariances
                  #CONTROL ~~    NORMS + ATTITUDES
                  #NORMS   ~~            ATTITUDES
                  
                  '

RX_FULL_FINAL <- sem(M_FULL_FINAL,data=factdata,
                     ordered=c("circum_intent",
                               "cb1","cb2","cb3","cb4","cb5","cb6","cb7","cb8","cb9","cb10","cb11","cb12","cb13","cb14","cb15","cb16","cb17","cb18","cb19","cb20","cb21",
                               "injunctive1","injunctive2","injunctive3","injunctive4","injunctive5","injunctive6","injunctive7","descriptive1","descriptive2","descriptive3","descriptive4",
                               "sub1","sub2","sub3","sub4","sub5","sub6","sub7","sub8","sub9","sub10","sub11","sub12"))

summary(RX_FULL_FINAL, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)

semPaths(RX_FULL_FINAL, what = "col", whatLabels = "par", style = "mx",
         intercepts=FALSE, thresholds=FALSE, residuals=TRUE,
         rotation = 2, layout = "tree", mar = c(3, 4, 3, 4), nCharNodes = 10,
         shapeMan = "rectangle", sizeMan = 8, sizeMan2 = 5)

lavInspect(RX_FULL_FINAL, what = "implied")
lavInspect(RX_FULL_FINAL, what = "sampstat")
lavInspect(RX_FULL_FINAL, what = "resid")
