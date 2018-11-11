#graduation prediction
#load actual data for 3 schools, two years
#add variables, needed to predict
#run model

newData1314 <- read.csv("newjoined1314.csv", header = TRUE)
newData1516 <- read.csv("newjoined1516.csv", header = TRUE)

#2013-14 School Year
#Add Chronic Absenteeism Rate
newData1314 <- mutate(newData1314, ChrAbsentRate = (TOT_ABSENT_M + TOT_ABSENT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add AP class rate
newData1314 <- mutate(newData1314, APRate = (TOT_APENR_M + TOT_APENR_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Sport Participation rate
newData1314 <- mutate(newData1314, SportRate = (TOT_SSPART)/(TOT_ENR_M + TOT_ENR_F))

#Add SAT/ACT rate
JoinModel1314 <- mutate(JoinModel1314, SATRate = (TOT_SATACT_M + TOT_SATACT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add teacher absenteeism rate
JoinModel1314 <- mutate(JoinModel1314, TeacherAbsentRate = SCH_FTETEACH_ABSENT/SCH_FTETEACH_TOT)

#Add non-certified teacher rate
JoinModel1314 <- mutate(JoinModel1314, TeacherNonCertRate = SCH_FTETEACH_NOTCERT/SCH_FTETEACH_TOT)

#Add Geometry rate
JoinModel1314 <- mutate(JoinModel1314, GeomRate = (TOT_GEOMENR_GS0712_M + TOT_GEOMENR_GS0712_F)/(TOT_ENR_M + TOT_ENR_F))

#2015-16 Schcool Year
#Add Chronic Absenteeism Rate
newData1516 <- mutate(newData1516, ChrAbsentRate = (TOT_ABSENT_M + TOT_ABSENT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add AP class rate
newData1516 <- mutate(newData1516, APRate = (TOT_APENR_M + TOT_APENR_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Sport Participation rate
newData1516 <- mutate(newData1516, SportRate = (TOT_SSPART)/(TOT_ENR_M + TOT_ENR_F))

#Predictions for 2013-14 School Year
GradPredictionModel <- lm(GRAD_RATE ~ (ChrAbsentRate + APRate + SportRate), data = JoinModel1314)
GradPredictionModel

GRAD_RATE <- newData1314$ALL_RATE_1314
grad1314predict1 <- predict(GradPredictionModel, newdata = newData1314, interval = "confidence")
grad1314predict1

GradPredictionModel3 <- lm(GRAD_RATE ~., data = JoinModel1314, interval = "confidence")
GradPredictionModel3

GradPredictionModel4 <- lm(GRAD_RATE ~ ChrAbsentRate, data = JoinModel1314, interval = "confidence")
GradPredictionModel4

#Predictions for 2015-16 School Year
GradPredictionModel2 <- lm(GRAD_RATE ~ (ChrAbsentRate + APRate + SportRate), data = JoinModel1516)
GradPredictionModel2

GRAD_RATE <- newData1516$ALL_RATE_1516
grad1516predict2 <- predict(GradPredictionModel2, newdata = newData1516, interval = "confidence")
grad1516predict2

#Predictions to get to 90% Graduation Rate
SLHSPredict <- predict(GradPredictionModel, newdata = c(""))

#--------------------------------------------------------
#Change variables to predict 90% graduation
#--------------------------------------------------------
#Add Chronic Absenteeism Rate
newData1516a <- newData1516
newData1516a <- mutate(newData1516a, ChrAbsentRate = (TOT_ABSENT_M + TOT_ABSENT_F - 150)/(TOT_ENR_M + TOT_ENR_F))

#Add AP class rate
newData1516a <- mutate(newData1516a, APRate = (TOT_APENR_M + TOT_APENR_F + 125)/(TOT_ENR_M + TOT_ENR_F))

#Add Sport Participation rate
newData1516a <- mutate(newData1516a, SportRate = (TOT_SSPART + 250)/(TOT_ENR_M + TOT_ENR_F))
GRAD_RATE <- newData1516a$ALL_RATE_1516

GradPredictionModel2 <- lm(GRAD_RATE ~ (ChrAbsentRate + APRate + SportRate), data = JoinModel1516)
grad1516predict2 <- predict(GradPredictionModel2, newdata = newData1516a, interval = "confidence")

grad1516predict2

#-----------------------------------------------------
#Prediction model graphically
#-----------------------------------------------------
prediction <- read.csv("PredictionMetrics2.csv", header = TRUE)
predictChart <- ggplot(data = prediction, aes(x = School_Year, y = Actual_Graduation, group = 1)) +
        facet_wrap(~ SCH_NAME, nrow = 3, dir = "h", strip.position = "top") +
        geom_line() +
        theme_minimal() +
        
predictChart



