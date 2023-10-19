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

#Convert file format from sas to .csv --------------------------------------------------------------------------------------
library(haven)
library(sas7bdat)
library(data.table)
library(dplyr)

setwd()

#sheps <- read_sas("data/nc_pregnancyed13_nofmt.sas7bdat") #Load in sas Sheps file
sheps <- read_sas("nc_pregnancyed14_lb.sas7bdat")
sheps <- as.data.frame(sheps)
head(sheps)

sheps <- sheps %>% #Filter for just NC
  filter(ptstate == 'NC',preg_comp==1)

sheps <- sheps%>%
  select(shepsid,fyear,admitdt,ptstate,ptcnty,ptzip,agem,agey,sex,race,ethnicity,self_pay, gov_pay,com_pay,medicaid,PMAD, SMI, MDP, suicide_attempt, suicide_thought, preg_comp)

sheps <- sheps %>% #Create insurance variable
  mutate(new_ins=case_when(self_pay=='1'~'self pay',
                           gov_pay=='1'~'gov pay',
                           com_pay=='1'~'commercial',
                           medicaid=='1'~'medicaid'))

#Crosswalk the data file and add coordinates-------------------------------------

cw <- read.csv("data\\Crosswalk_NC.csv")
coord <- fread("data\\XYCenPoints_NC.csv")

cw$zip5 <- as.integer(cw$ZIP) #convert zip5
sheps$zip5 <- as.integer(sheps$ptzip) #make sure zip5 is integer in both dataframes
coord$zip5 <- as.integer(coord$Zip)

cw <- cw %>%
  left_join(coord,by=c('zip5'))

sheps <- sheps %>%
  left_join(cw, by=c('zip5')) #join crosswalk to data

sheps <- sheps %>%
  select(-zip5, -ZIP_TYPE, -ZIP, -ptzip) #remove all zip code columns because we ONLY NEED ZCTA as spatial identifier

#write off files-----------------------------------------------------------------

fwrite(sheps, 'data/sheps_cw_2016_2021_month.csv') 

sheps_pre <- sheps %>% #Split into pre- and post-COVID for calculating SMR
  filter(fyear < 2020)
fwrite(sheps_pre, '2016_2019_cw_month_sheps.csv')
sheps_post <- sheps %>% 
  filter(fyear > 2019)
fwrite(sheps_post, '2020_2021_cw_month_sheps.csv')

#CODE TO GENERATE EXPECTED CASES BASED ON TIME (year or month & year) & location (ZCTA)

#pop <- fread("data\\ACS_2018_female_pop.csv")
pop <- fread("data\\ACS2020_pop_age_F_NCZCTA.csv")
pop$ZCTA <- as.numeric(pop$ZCTA)

data <- sheps

data <- data %>% #Filter to study period of interest
  filter(fyear>2019)

data.table::setDT(data)[, Month_Yr := format(as.Date(admitdt), "%Y-%m") ] #create Month_Yr column if needed

data_month <- data %>%
  group_by(Month_Yr, ZCTA)%>%
  summarize(pmad_zcta=sum(PMAD), 
            mdp_zcta=sum(MDP), 
            smi_zcta=sum(SMI), 
            suicide_thoughts_zcta=sum(suicide_thought), 
            suicide_attempt_zcta=sum(suicide_attempt)) #Calculate total cases per ZCTA per time period

data_pop <- pop %>%
  left_join(data_month, by=c('ZCTA')) #Join to population

data_pop_expect <- data_pop %>% #Calculate expected number of cases based on ZCTA pop and state-wide female pop
  mutate(expect_pmad = ((TotalPop_F * (pmad_zcta/5333560))/2), 
         expect_mdp = ((TotalPop_F * (mdp_zcta/5333560))/2),
         expect_smi = ((TotalPop_F * (smi_zcta/5333560))/2),
         expect_suicide_thoughts = ((TotalPop_F * (suicide_thoughts_zcta/5333560))/2),
         expect_suicide_attempts = ((TotalPop_F * (suicide_attempt_zcta/5333560))/2))
head(data_pop_expect)

data_SMR <- data %>% 
  left_join(data_pop_expect, by=c('Month_Yr', 'ZCTA')) #join expected cases to original data
head(data_SMR)

data_SMR <- data_SMR[complete.cases(data_SMR), ] #deals with NAs

fwrite(data_SMR, 'post_covid_monthly_smr.csv') #write off file for future use

#####################REGRESSION CODE FOR PREDICTED VALUES - this is where we generate SMRs###########################

data <- fread('post_covid_monthly_smr.csv') #read in file from above

model <-glm(formula = PMAD ~ offset((expect_pmad)) +  factor(agey) + factor(new_ins),
            family = quasipoisson, data = data) #Generate SMRs using expected cases & demographic info
summary(model)
data$pmad_pop <- predict(model, type="response") #add SMR to dataframe as population
summary(model$fitted.values) #get a sense for SMR distribution (SHOULD NOT HAVE ANY NEGATIVES!!!!!!!!!!!)
hist(predict(model, type="response")) #visual display of SMRs

model <-glm(formula = MDP ~ offset((expect_mdp)) +  factor(agey) + factor(new_ins),
            family = quasipoisson, data = data)
summary(model)
data$mdp_pop <- predict(model, type="response") 
summary(model$fitted.values) 
hist(predict(model, type="response")) 

model <-glm(formula = SMI ~ offset((expect_smi)) +  factor(agey) + factor(new_ins),
            family = quasipoisson, data = data)
summary(model)
data$smi_pop <- predict(model, type="response") 
summary(model$fitted.values) 
hist(predict(model, type="response")) 

model <-glm(formula = suicide_thought ~ offset((expect_suicide_thoughts)) +  factor(agey) + factor(new_ins),
            family = quasipoisson, data = data)
summary(model)
data$suicide_thoughts_pop <- predict(model, type="response") 
summary(model$fitted.values) 
hist(predict(model, type="response")) 

model <-glm(formula = suicide_attempt ~ offset((expect_suicide_attempts)) +  factor(agey) + factor(new_ins),
            family = quasipoisson, data = data)
summary(model)
data$suicide_attempt_pop <- predict(model, type="response") 
summary(model$fitted.values) 
hist(predict(model, type="response")) 

summary(data)

data <- data %>%
  select(ZCTA, XCenPoint, YCenPoint, Month_Yr, fyear, ptcnty, agey, sex, admitdt, TotalPop_F, PMAD, MDP, SMI, suicide_thought, suicide_attempt, pmad_pop, mdp_pop, smi_pop, suicide_thoughts_pop, suicide_attempt_pop)

fwrite(data, 'post_covid_monthly_smr_forsatscan.csv') #write off file -- USE IN SATSCAN









#My attempt at writing a for loop------------------------------------------------

outcome <- c("PMAD","SMI","MDP","suicide_attempt","suicide_thought")

data <- sheps_pre

for (i in outcome) {
  var_name <- paste0(i, "_cases_zip_year")
  
  data_sum <- data %>%
    group_by(ZCTA, fyear) %>%
    summarize(!!var_name := sum(!!sym(i)))
  
  data_pop <- pop %>% 
    left_join(data_sum, by = c('ZCTA'))
  
  # You need to reference the actual column name represented by var_name
  data_pop_expect <- data_pop %>%
    mutate(expect = (TotalPop_F * (!!sym(var_name) / 5299356))) # Use sym(var_name)
           
           data2 <- data %>% 
             left_join(data_pop_expect, by = c('ZCTA'))
           data2[is.na(data2)] <- 0
           
           model <- glm(formula = as.formula(paste0(i, " ~ offset(expect) + factor(race) + agey + factor(new_ins)")),
                        family = quasipoisson, data = data2)
           
           data2$SMR_POP <- predict(model, type = "response")
           
           data2_sub <- subset(data2, select = c('ZCTA', !!sym(i), expect, SMR_POP, fyear, admitdt))
           
           data2_grp <- data2_sub %>%
             group_by(ZCTA, fyear) %>%
             summarize(!!var_name := sum(!!sym(i)),
                       expect = sum(expect),
                       SMR_POP = sum(SMR_POP)
             ) %>%
             as.data.frame(data2_grp)
           
           data2_grp[[i]] <- ceiling(data2_grp[[i]])
           
           coord <- coord %>%
             rename(ZCTA = Zip)
           
           coord <- coord %>%
             left_join(data2_grp, by = c('ZCTA'))
           
           coord[is.na(coord)] <- 0
           
           fwrite(coord, paste0(i, "_SMR_pre.csv"))
}











