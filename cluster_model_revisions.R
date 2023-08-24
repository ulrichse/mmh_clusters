

#Models-------------------------------------------------------------------------
library(sjPlot)

indiv <- read.csv("indiv_w_clu_zcta_cat_data_2016_2019.csv")

PMAD <- subset(indiv, PMAD=='1')
SMI <- subset(indiv, SMI=='1')
MDP <- subset(indiv, MDP=='1')

#PMAD

PMAD_com <- glm(PMAD_CLU ~ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level), family=binomial(link='logit'),data=PMAD)
PMAD_indiv <- glm(PMAD_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat) ,family=binomial(link='logit'),data=PMAD)
PMAD_full <-  glm(PMAD_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat)+ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level) ,family=binomial(link='logit'),data=PMAD)

tab_model(PMAD_full, file="data/PMAD_2016_2019_tabmodel.doc") #,  file = "PMAD_post_tabmodel.doc")
vif(PMAD_full)

#SMI

SMI_com <- glm(SMI_CLU ~ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level), family=binomial(link='logit'),data=SMI)
SMI_indiv <- glm(SMI_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat) ,family=binomial(link='logit'),data=SMI)
SMI_full <-  glm(SMI_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat)+ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level) ,family=binomial(link='logit'),data=SMI)

tab_model(SMI_full,  file = "SMI_2016_2019_tabmodel.doc")
vif(SMI_full)

#MDP
MDP_com <- glm(MDP_CLU ~ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level), family=binomial(link='logit'),data=MDP)
MDP_indiv <- glm(MDP_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat) ,family=binomial(link='logit'),data=MDP)
MDP_full <-  glm(MDP_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat)+ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level) ,family=binomial(link='logit'),data=MDP)

tab_model(MDP_full,  file = "MDP_2016_2019_tabmodel.doc")
vif(MDP_full)


#Make MLM to show it didn't work (but maybe it did)------------------------------------------------

library(questionr)
library(psych)               #use for describe function to get descriptives
library(ggplot2)             #use for plots
library(lme4)                #fits mixed models
library(lmerTest)            #provides t-tests for fixed effects and tests of linear combinations
library(performance)         #computes ICC
library(emmeans)
library(data.table)
library(sjPlot) #for tab_model
library(car) #for vif

PMAD_mlm <- glmer(PMAD_CLU ~ factor(race_cat) + factor(age_cat) + factor(new_ins) + factor(eth_cat) + factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level) + (1|ZCTA), control=glmerControl(optimizer='bobyqa'), nAGQ = 0, data=PMAD, family=binomial(link="logit"))
icc(PMAD_mlm)
summary(PMAD_mlm)
tab_model(PMAD_mlm)
vif(PMAD_mlm)

SMI_mlm <- glmer(SMI_CLU ~ factor(race_cat) + factor(age_cat) + factor(new_ins) + factor(eth_cat) + factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level) + (1|ZCTA), control=glmerControl(optimizer='bobyqa'), nAGQ = 0, data=SMI, family=binomial(link="logit"))
icc(SMI_mlm)
summary(SMI_mlm)
tab_model(SMI_mlm)
rm(SMI_mlm)

MDP_mlm <- glmer(MDP_CLU ~ factor(race_cat) + factor(age_cat) + factor(new_ins) + factor(eth_cat) + factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich) + factor(RUCA_level) + (1|ZCTA), control=glmerControl(optimizer='bobyqa'), nAGQ = 0, data=MDP, family=binomial(link="logit"))
icc(MDP_mlm)
summary(MDP_mlm)
tab_model(MDP_mlm)
rm(MDP_mlm)
