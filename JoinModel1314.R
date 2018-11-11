#Clean and organize data

library(dplyr)
library(tidyverse)
library(stringr)
library(GGally)
library(jtools)
library(ggstance)
library(geofacet)

#Add school year
#Add NCES_COMBO variable
JoinModel1314 <- smalljoined1314
JoinModel1314 <- mutate(JoinModel1314, SCHOOL_YEAR = "2013-14")
JoinModel1314$SCHID <- str_pad(JoinModel1314$SCHID, width = 5, side = c("left"), pad = "0")
JoinModel1314$LEAID <- str_pad(JoinModel1314$LEAID, width = 7, side = c("left"), pad = "0")
JoinModel1314 <- mutate(JoinModel1314, NCES_COMBO = paste0(LEAID, SCHID, sep = ""))

#Add Chronic Absenteeism Rate
JoinModel1314 <- mutate(JoinModel1314, ChrAbsentRate = (TOT_ABSENT_M + TOT_ABSENT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Graduation Rate
JoinModel1314 <- JoinModel1314 %>% filter(ALL_RATE_1314!="PS")
JoinModel1314$ALL_RATE <- chartr(old="LT", new="0-", x=JoinModel1314$ALL_RATE_1314)
JoinModel1314$ALL_RATE <- gsub(pattern="GE", replacement="100-", x=JoinModel1314$ALL_RATE)
JoinModel1314 <- JoinModel1314 %>% separate(ALL_RATE_1314, into=c("GRAD_RATE_A", "GRAD_RATE_B"), sep="-", remove=F)
JoinModel1314 <- JoinModel1314 %>% mutate(GRAD_RATE_B = ifelse(is.na(JoinModel1314$GRAD_RATE_B), GRAD_RATE_A, GRAD_RATE_B))
JoinModel1314$GRAD_RATE_A <- as.numeric(JoinModel1314$GRAD_RATE_A)
JoinModel1314$GRAD_RATE_B <- as.numeric(JoinModel1314$GRAD_RATE_B)
JoinModel1314 <- JoinModel1314 %>% mutate(GRAD_RATE = ((GRAD_RATE_A + GRAD_RATE_B)*.5)) %>% filter(GRAD_RATE > 0)

#Add Geometry rate
JoinModel1314 <- mutate(JoinModel1314, GeomRate = (TOT_GEOMENR_GS0712_M + TOT_GEOMENR_GS0712_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Biology rate
JoinModel1314 <- mutate(JoinModel1314, BioRate = (TOT_SCIENR_BIOL_M + TOT_SCIENR_BIOL_F)/(TOT_ENR_M + TOT_ENR_F))

#Add AP class rate
JoinModel1314 <- mutate(JoinModel1314, APRate = (TOT_APENR_M + TOT_APENR_F)/(TOT_ENR_M + TOT_ENR_F))

#Add SAT/ACT rate
JoinModel1314 <- mutate(JoinModel1314, SATRate = (TOT_SATACT_M + TOT_SATACT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add In-School Suspension rate
JoinModel1314 <- mutate(JoinModel1314, ISSRate = (TOT_DISCWODIS_ISS_M + TOT_DISCWODIS_ISS_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Sport Participation rate
JoinModel1314 <- mutate(JoinModel1314, SportRate = (TOT_SSPART)/(TOT_ENR_M + TOT_ENR_F))

#Add Harassment and Bullying Allegations (sex, race, and disabilites)
JoinModel1314 <- mutate(JoinModel1314, BullyRate = (SCH_HBALLEGATIONS_SEX + SCH_HBALLEGATIONS_RAC + SCH_HBALLEGATIONS_DIS)/(TOT_ENR_M + TOT_ENR_F))

#Add money spent on activities
JoinModel1314 <- mutate(JoinModel1314, ActivitiesRate = (SCH_NPE_WOFED)/(TOT_ENR_M + TOT_ENR_F))

#Add number of teachers
JoinModel1314 <- mutate(JoinModel1314, TeacherRate = (SCH_FTETEACH_TOT)/(TOT_ENR_M + TOT_ENR_F))

#Add teacher absenteeism rate
JoinModel1314 <- mutate(JoinModel1314, TeacherAbsentRate = SCH_FTETEACH_ABSENT/SCH_FTETEACH_TOT)

#Add non-certified teacher rate
JoinModel1314 <- mutate(JoinModel1314, TeacherNonCertRate = SCH_FTETEACH_NOTCERT/SCH_FTETEACH_TOT)

head(JoinModel1314)
posterModel1 <- lm(GRAD_RATE ~ ChrAbsentRate + GeomRate + BioRate + APRate + SATRate, data = JoinModel1314)
posterModel1

posterModel3 <- lm(GRAD_RATE ~ SportRate + APRate, data = JoinModel1314)
posterModel3

posterModel4 <- lm(GRAD_RATE ~ ChrAbsentRate, data = JoinModel1314)
posterModel4

posterModel5 <- lm(GRAD_RATE ~ GeomRate, data = JoinModel1314)
posterModel5

posterModel6 <- lm(GRAD_RATE ~ BioRate, data = JoinModel1314)
posterModel6

posterModel7 <- lm(GRAD_RATE ~ APRate, data = JoinModel1314)
posterModel7

posterModel8 <- lm(GRAD_RATE ~ SATRate, data = JoinModel1314)
posterModel8

posterModel9 <- lm(GRAD_RATE ~ ISSRate, data = JoinModel1314)
posterModel9

posterModel10 <- lm(GRAD_RATE ~ SportRate, data = JoinModel1314)
posterModel10

posterModel11 <- lm(GRAD_RATE ~ BullyRate, data = JoinModel1314)
posterModel11

posterModel12 <- lm(GRAD_RATE ~ ActivitiesRate, data = JoinModel1314)
posterModel12

posterModel13 <- lm(GRAD_RATE ~ TeacherRate, data = JoinModel1314)
posterModel13

posterModel14 <- lm(GRAD_RATE ~ TeacherAbsentRate, data = JoinModel1314)
posterModel14

posterModel15 <- lm(GRAD_RATE ~ TeacherNonCertRate, data = JoinModel1314)
posterModel15

posterModel11 <- lm(GRAD_RATE ~ ChrAbsentRate + ISSRate, data = JoinModel1314)
posterModel11

posterModel16 <- lm(GRAD_RATE ~ ChrAbsentRate + APRate + SportRate, data = JoinModel1314)
posterModel16

modelData1314 <- JoinModel1314 %>% select(GRAD_RATE, 
                                          ChrAbsentRate, GeomRate, BioRate, APRate, SATRate,
                                          ISSRate, SportRate, SCH_HBALLEGATIONS_RAC, 
                                          SCH_FTETEACH_ABSENT, SCH_FTETEACH_NOTCERT, 
                                          SCH_FTETEACH_TOT, SCH_FTETEACH_ABSENT, 
                                          SCH_NPE_WOFED)

modelData1314

# Theory: get the best prediction all the variables (not the best model) 
fullModel <- lm(GRAD_RATE ~ ., data = modelData1314)
summary(fullModel)

selectModel <- lm(GRAD_RATE ~ (ChrAbsentRate + GeomRate + APRate + 
                              SATRate + SportRate + TeacherNonCertRate + 
                              TeacherRate + ActivitiesRate), data=JoinModel1314)
selectModel


# Backward variable selection
step(fullModel) 

cor(modelData1314)
ggpairs(modelData1314)

plot(fullModel)

allVariableModel <- JoinModel1314 %>% select(GRAD_RATE, ChrAbsentRate, GeomRate, BioRate, APRate, SATRate,
                                             ISSRate, SportRate, BullyRate, ActivitiesRate, 
                                             TeacherRate, TeacherAbsentRate, TeacherNonCertRate)
GradModel <- lm(GRAD_RATE ~ APRate +SportRate + SATRate + TeacherAbsentRate + ISSRate + TeacherRate + BullyRate + 
               BioRate + ActivitiesRate + TeacherNonCertRate + GeomRate + ChrAbsentRate, data = allVariableModel)
summ(GradModel)

plot_summs(GradModel, scale = TRUE, inner_ci_level = .95)

GradPredictionModel <- lm(GRAD_RATE ~ (ChrAbsentRate + APRate + SportRate), data = JoinModel1314)
GradPredictionModel
summary(GradPredictionModel)

modelData1314 <- JoinModel1314 %>% select(GRAD_RATE, 
                                          ChrAbsentRate, GeomRate, BioRate, APRate, SATRate,
                                          ISSRate, SportRate, SCH_HBALLEGATIONS_RAC, 
                                          SCH_FTETEACH_ABSENT, SCH_FTETEACH_NOTCERT, 
                                          SCH_FTETEACH_TOT, SCH_FTETEACH_ABSENT, 
                                          SCH_NPE_WOFED)

model3 <- lm(GRAD_RATE ~ ChrAbsentRate + APRate + SportRate, data = JoinModel1314)
model3
summary(model3)

model4 <- plot_coefs(fullModel, ci_level = .95)
model4


#prediction model
posterModel16 <- lm(GRAD_RATE ~ ChrAbsentRate + APRate + SportRate, data = JoinModel1314)
posterModel16


predict(GradPredictionModel, newdata = SLHSData, interval = "prediction")

GradPredictModel1 <- predict(posterModel16, level = .95)
GradPredictModel1



