library(data.table)
library(dplyr)
library(ggplot2)

setwd('')

#2016 to 2019

data <- fread("data\\2016_2019_sheps.csv") #Read in Sheps file for 2016 to 2019
pop <- fread("data\\ACS_2018_female_pop.csv")
coord <- fread("data\\XYCenPoints_NC.csv")

#data <- data %>% #might have to do this 
#rename("ZCTA"="ptzip")

data[is.na(data)]<-0

#Summarize cases by ZCTA 
data_zip <- data %>%
  group_by( ZCTA)%>%
  summarize(SMI_Cases_ZIP = sum(SMI)/4) #Divide by number of years (2016-2019), fixes a population count issue
hist(data_zip$SMI_Cases_ZIP)

data_pop <- pop %>% 
  left_join (data_zip, by=c('ZCTA'))
head(data_pop)

data_zip$SMI_Cases_ZIP <- as.numeric(data_zip$SMI_Cases_ZIP)

#Calculate expected cases based on statewide rate
data_pop_expect <- data_pop %>%
  mutate(expect = TotalPop_F * (SMI_Cases_ZIP/5299356)) #5299356 is total female population of North Carolina (2018 ACS)
head(data_pop_expect)

#Join summarized data back to line-level data
data2 <- data %>% 
  left_join (data_pop_expect, by=c('ZCTA'))
head(data2)

hist(data2$expect)
summary(data2)

data2$race <- as.factor(data2$race)

data2[is.na(data2)]<-0

#Calculate SMRs using expected case value from above, adjust for line-level variables
model <-glm(formula = SMI ~ offset(expect) + factor(race) + agey,
            family = quasipoisson, data = data2)
summary(model)
data2$SMR_POP <- predict(model, type="response")
hist(model$fitted.values)
hist(predict(model, type="response"))

data2_sub <- subset(data2, select=c('ZCTA','SMI','expect','SMR_POP'))

data2_grp <- data2_sub %>%
  group_by(ZCTA)%>%
  summarize(SMI_Cases_Zip=(sum(SMI)/4),
            expect=sum(expect),
            SMR_POP=sum(SMR_POP),
  )%>%
  as.data.frame(data2_grp)

data2_grp$SMI <- ceiling(data2_grp$SMI)

#Merge with coordinates to make easier in SaTScan
#coord <- coord %>%
#rename(ZCTA=Zip)

coord <- coord %>%
  left_join(data2_grp, by=c('ZCTA'))

coord[is.na(coord)]<-0

#write off new file - this file likely includes cases and population; can be used to populate both case field and population field in SaTScan
fwrite(coord,"2016_2019_SMI_SMR.csv")

#Do this for all outcomes of interest (SMI, MDP, PMAD)

indiv <- read.csv("data/2016_2019_sheps.csv") #read in individual-level data
indiv <- read.csv("data/2020_2021_sheps.csv")

#Create race categories
indiv$race_cat <- fifelse(indiv$race == "5", "White",
                          fifelse(indiv$race == "3", "Black",
                                  fifelse(indiv$race == "2", "Asian",
                                          fifelse(indiv$race=="1","Indigenous American",
                                                  fifelse(indiv$race=="4","Native Hawaiian/PI",
                                                          fifelse(indiv$race=="6", "Other",
                                                                  "Unknown"))))))
indiv$race_cat=factor(indiv$race_cat)

#Create ethnicity categories
indiv$eth_cat <- fifelse(indiv$ethnicity == "1", "Not Hispanic",
                         fifelse(indiv$ethnicity == "2", "Hispanic",
                                 "Unknown"))
indiv$eth_cat= factor(indiv$eth_cat)

#Create age categories
indiv$age_cat <- as.factor(ifelse(indiv$agey<30, '18-29',
                                  ifelse(indiv$agey<40,'30-39',
                                         ifelse(indiv$agey <= 44, '40+',0))))
indiv$age_cat= factor(indiv$age_cat)

#Create insurance categories
indiv <- indiv %>%
  mutate(new_ins=case_when(self_pay=='1'~'self pay',
                           gov_pay=='1'~'gov pay',
                           com_pay=='1'~'commercial',
                           medicaid=='1'~'medicaid'))

indiv$new_ins <- as.factor(indiv$new_ins)

#Narrow down indiv data
indiv <- indiv%>%
  select(shepsid,fyear,ptstate,ptcnty,ZCTA,agem,agey,sex,race,age_cat,race_cat,new_ins,eth_cat, PMAD, SMI, MDP)

#Create column indicating if each NC ZCTA is in a cluster--easier to create in ArcGIS
#Read in ZCTA cluster files
#Do pre and then do post

clusters <- read.csv("data/zcta_clusters_10_2020_2021.csv") 

#Merge with individual data

indiv <- indiv %>%
  left_join(clusters, by=c('ZCTA'))
fwrite(indiv, 'data/indiv_cluster_10_2016_2019.csv') #write off

library(forecast)
library(ggplot2)
library(gridExtra)
library(xts)
library(dplyr)
library(lubridate)
library(astsa)
library(reshape2)
library(padr)
library(zoo)
library(tfarima)
library(lmtest)
library(gridExtra)
library(data.table)
library(tableone)

data <- read.csv("data/indiv_cluster_10_2020_2021.csv") #read in the .csv that we just made above

data[is.na(data)]<-0

PMADVars <- c("PMAD_CLU","ZCTA","age_cat","race_cat","eth_cat","new_ins")
SMIVars <- c("SMI_CLU","ZCTA","age_cat","race_cat","eth_cat","new_ins")
MDPVars <- c("MDP_CLU","ZCTA","age_cat","race_cat","eth_cat","new_ins")
catVars <- c("age_cat","race_cat","eth_cat")

PMADstrata<-c("PMAD_CLU") #Basically our table is going to show differences between those in and outside of a cluster *among those who have PMAD/SMI/MDP* not the entire pop
SMIstrata <- c("SMI_CLU")
MDPstrata <- c("MDP_CLU")

PMAD <- subset(data, PMAD=='1') #We are only interested in women who have PMAD/SMI/MDP
SMI <- subset(data, SMI=='1')
MDP <- subset(data, MDP=='1')

pmad_tab <- CreateTableOne(vars = PMADVars, strata=PMADstrata, data = PMAD, factorVars = catVars)
smi_tab <- CreateTableOne(vars = SMIVars, strata=SMIstrata, data = SMI, factorVars = catVars)
mdp_tab <- CreateTableOne(vars = MDPVars, strata=MDPstrata, data = MDP, factorVars = catVars)

summary(pmad_tab)
summary(smi_tab)
summary(mdp_tab)

pmad_tabmat <- print(pmad_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd=FALSE)
write.csv(pmad_tabmat, file = "PMAD_2016_2019_10_tab1.csv")

smi_tabmat <- print(smi_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd=FALSE)
write.csv(smi_tabmat, file = "SMI_2020_2021_10_tab1.csv")

mdp_tabmat <- print(mdp_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd=FALSE)
write.csv(mdp_tabmat, file = "MDP_2016_2019_10_tab1.csv")


#Import community-level vars and merge with individual data---------------------

cat_data <- read.csv("data/zcta_cat_data.csv")

cat_data[is.na(cat_data)]<-0

#Create GS categories

cat_data <- cat_data %>%
  mutate(GS = ntile(gs_area_pop, 3)) %>%
  mutate(GS = if_else(GS == 1, 'Low', if_else(GS == 2, 'Medium', 'High'))) %>%
  arrange(GS)
table(cat_data$GS)##QC

cat_data <- cat_data %>%
  mutate(GS_dich = ntile(gs_area_pop, 4)) %>%
  mutate(GS_dich = if_else(GS_dich == 1, 'Low', if_else(GS_dich == 2, 'Low', if_else(GS_dich == 3, 'Low', 'High')))) %>%
  arrange(GS_dich)
table(cat_data$GS_dich)

#Create ICE categories

cat_data$ice_income<-as.numeric(cat_data$AB_ICE)
cat_data$ice_race<-as.numeric(cat_data$CD_ICE)
cat_data$ice_income_race<-as.numeric(cat_data$EF_ICE)

cat_data<-cat_data %>%
  mutate(ICE_Race_tertiles = ntile(ice_race, 3)) %>%
  mutate(ICE_Race_tertiles = if_else(ICE_Race_tertiles == 1, 'Low', if_else(ICE_Race_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(ICE_Race_tertiles)
table(cat_data$ICE_Race_tertiles)##QC

cat_data<-cat_data %>%
  mutate(ICE_Income_tertiles = ntile(ice_income, 3)) %>%
  mutate(ICE_Income_tertiles = if_else(ICE_Income_tertiles == 1, 'Low', if_else(ICE_Income_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(ICE_Income_tertiles)
table(cat_data$ICE_Income_tertiles)##QC

cat_data<-cat_data %>%
  mutate(ICE_Income_Race_tertiles = ntile(ice_income_race, 3)) %>%
  mutate(ICE_Income_Race_tertiles = if_else(ICE_Income_Race_tertiles == 1, 'Low', if_else(ICE_Income_Race_tertiles == 2, 'Medium', 'High'))) %>%
  arrange(ICE_Income_Race_tertiles)
table(cat_data$ICE_Income_Race_tertiles)

#Create RUCA categories

cat_data$RUCA_level <- as.factor(ifelse(cat_data$RUCA1<4, 'Urban',
                                        ifelse(cat_data$RUCA1<7, 'Metro',
                                               ifelse(cat_data$RUCA1<=10,'Rural',0))))
cat_data$RUCA_level= factor(cat_data$RUCA_level)
cat_data$RUCA_level <- factor(cat_data$RUCA_level, ordered=FALSE)
cat_data$RUCA_level <- relevel(cat_data$RUCA_level, ref = "Rural")

#Merge with individual data 

indiv_cat <- data %>%
  left_join(cat_data, by=c('ZCTA'))

write.csv(indiv_cat, "data/indiv_w_clu_zcta_cat_data_2016_2019.csv")

#Models-------------------------------------------------------------------------
library(sjPlot)

indiv <- read.csv("data/indiv_w_clu_zcta_cat_data_2016_2019.csv")

indiv[is.na(indiv)] <- 0

#Set reference categories
indiv$race_cat= factor(indiv$race_cat)
indiv$race_cat <- factor(indiv$race_cat, ordered=FALSE)
indiv$race_cat <- relevel(indiv$race_cat, ref = "White")

indiv$eth_cat= factor(indiv$eth_cat)
indiv$eth_cat <- factor(indiv$eth_cat, ordered=FALSE)
indiv$eth_cat <- relevel(indiv$eth_cat, ref = "Not Hispanic")

indiv$age_cat= factor(indiv$age_cat)
indiv$age_cat <- factor(indiv$age_cat, ordered=FALSE)
indiv$age_cat <- relevel(indiv$age_cat, ref = "18-29")

indiv$new_ins <- as.factor(indiv$new_ins)
indiv$new_ins <- factor(indiv$new_ins, ordered=FALSE)
indiv$new_ins <- relevel(indiv$new_ins, ref = "commercial")

indiv$RUCA_level= factor(indiv$RUCA_level)
indiv$RUCA_level <- factor(indiv$RUCA_level, ordered=FALSE)
indiv$RUCA_level <- relevel(indiv$RUCA_level, ref = "Rural")

indiv$GS_dich= factor(indiv$GS_dich)
indiv$GS_dich <- factor(indiv$GS_dich, ordered=FALSE)
indiv$GS_dich <- relevel(indiv$GS_dich, ref = "High")

indiv$GS= factor(indiv$GS)
indiv$GS <- factor(indiv$GS, ordered=FALSE)
indiv$GS <- relevel(indiv$GS, ref = "Low")

indiv$ICE_Income_tertiles= factor(indiv$ICE_Income_tertiles)
indiv$ICE_Income_tertiles <- factor(indiv$ICE_Income_tertiles, ordered=FALSE)
indiv$ICE_Income_tertiles <- relevel(indiv$ICE_Income_tertiles, ref = "High")

indiv$ICE_Race_tertiles= factor(indiv$ICE_Race_tertiles)
indiv$ICE_Race_tertiles <- factor(indiv$ICE_Race_tertiles, ordered=FALSE)
indiv$ICE_Race_tertiles <- relevel(indiv$ICE_Race_tertiles, ref = "High")

indiv$ICE_Income_Race_tertiles= factor(indiv$ICE_Income_Race_tertiles)
indiv$ICE_Income_Race_tertiles <- factor(indiv$ICE_Income_Race_tertiles, ordered=FALSE)
indiv$ICE_Income_Race_tertiles <- relevel(indiv$ICE_Income_Race_tertiles, ref = "High")

indiv$RUCA1=factor(indiv$RUCA1)
indiv$RUCA1=factor(indiv$RUCA1, ordered=FALSE)
indiv$RUCA1 <- relevel(indiv$RUCA1, ref="10")

PMAD <- subset(indiv, PMAD=='1')
SMI <- subset(indiv, SMI=='1')
MDP <- subset(indiv, MDP=='1')

#PMAD

PMAD_com <- glm(PMAD_CLU ~ factor(RUCA1) + factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich), family=binomial(link='logit'),data=PMAD)
PMAD_indiv <- glm(PMAD_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat) ,family=binomial(link='logit'),data=PMAD)
PMAD_full <-  glm(PMAD_CLU ~ factor(RUCA_level) + factor(new_ins) + factor(eth_cat)+ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich),family=binomial(link='logit'),data=PMAD)


tab_model(PMAD_full, file="PMAD_2016_2019_tabmodel.doc")
vif(PMAD_full)

#SMI

SMI$race_adj <- fifelse(SMI$race_cat == "White", "White",
                        fifelse(SMI$race_cat == "Black","Black",
                                fifelse(SMI$race_cat == "Indigenous American", "Indigenous American",
                                        "Other/Unknown")))

SMI_com <- glm(SMI_CLU ~ factor(RUCA1) + factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich), family=binomial(link='logit'),data=SMI)
SMI_indiv <- glm(SMI_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat) ,family=binomial(link='logit'),data=SMI)
SMI_full <-  glm(SMI_CLU ~ factor(RUCA_level) + factor(new_ins) + factor(eth_cat)+ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles) + factor(GS_dich),family=binomial(link='logit'),data=SMI)

tab_model(SMI_full,  file = "SMI_2016_2019_tabmodel.doc")
vif(SMI_full)

#MDP
MDP_com <- glm(MDP_CLU ~ factor(RUCA1) + factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich), family=binomial(link='logit'),data=MDP)
MDP_indiv <- glm(MDP_CLU ~ factor(age_cat) + factor(race_cat) + factor(new_ins) + factor(eth_cat) ,family=binomial(link='logit'),data=MDP)
MDP_full <-  glm(MDP_CLU ~ factor(RUCA_level) + factor(new_ins) + factor(eth_cat)+ factor(ICE_Income_tertiles) + factor(ICE_Race_tertiles)+ factor(GS_dich),family=binomial(link='logit'),data=MDP)

tab_model(MDP_full,  file = "MDP_2016_2019_tabmodel.doc")
vif(MDP_full)

#Correlation matrix
indiv_cat$ins1 <- fifelse(indiv_cat$new_ins == "self pay", "1",
                          fifelse(indiv_cat$race == "medicaid", "2",
                                  fifelse(indiv_cat$race == "gov pay", "3",
                                          fifelse(indiv_cat$race=="commercial","4",
                                                  "0"))))

library(GGally)

indiv_cor <- indiv_cat %>%
  dplyr::select(
    c(
      Insurance,
      Ethnicity,
      ICE_Income,
      ICE_Race,
      Greenspace,
      RUCA
    )
  )

indiv_cor %>%
  ggcorr(nbreaks = 7, palette = "RdBu")
rcorr(x, type = c("pearson","spearman"))



indiv_cat$Insurance <- as.numeric(indiv_cat$ins1)
indiv_cat$ICE_Income <- as.numeric(indiv_cat$AB_ICE)
indiv_cat$ICE_Race <- as.numeric(indiv_cat$CD_ICE)
indiv_cat$RUCA <- as.numeric(indiv_cat$RUCA1)
indiv_cat$Greenspace <- as.numeric(indiv_cat$gs_area_pop)
indiv_cat$Ethnicity <- as.numeric(indiv_cat$eth_cat)

indiv_cor %>%
  ggcorr(nbreaks = 7, palette = "RdBu")


rcorr(x, type = c("pearson","spearman"))

#Create forest plots

install.packages("gridExtra")
install.packages("ggpubr")
install.packages("cowplot")


library(cowplot)

library(gridExtra)
library(ggpubr)

#Rural-urban diffs---------------------------------------------------------------

post_clu_pmad <- post_clu %>%
  filter(PMAD_CLU==1)

pre_clu_pmad <- pre_clu %>%
  filter(PMAD_CLU==1)

table(post_clu_pmad$RUCA_level)
table(pre_clu_pmad$RUCA_level)

post_clu_smi <- post_clu %>%
  filter(SMI_CLU==1)

pre_clu_smi <- pre_clu %>%
  filter(SMI_CLU==1)

table(post_clu_smi$RUCA_level)
table(pre_clu_smi$RUCA_level)


post_clu_mdp <- post_clu %>%
  filter(MDP_CLU==1)

pre_clu_mdp <- pre_clu %>%
  filter(MDP_CLU==1)

table(post_clu_mdp$RUCA_level)
table(pre_clu_mdp$RUCA_level)

