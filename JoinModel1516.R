##Clean and organize data

library(dplyr)
library(tidyverse)
library(stringr)
library(GGally)
library(jtools)
library(ggstance)
library(geofacet)

#Add school year
#Add NCES_COMBO variable
JoinModel1516 <- smalljoined1516
JoinModel1516 <- mutate(JoinModel1516, SCHOOL_YEAR = "2015-16")
JoinModel1516$SCHID <- str_pad(JoinModel1516$SCHID, width = 5, side = c("left"), pad = "0")
JoinModel1516$LEAID <- str_pad(JoinModel1516$LEAID.x, width = 7, side = c("left"), pad = "0")
JoinModel1516 <- mutate(JoinModel1516, NCES_COMBO = paste0(LEAID, SCHID, sep = ""))
JoinModel1516$NCES_COMBO <- as.character(JoinModel1516$NCES_COMBO)

#Add Chronic Absenteeism Rate
JoinModel1516 <- mutate(JoinModel1516, ChrAbsentRate = (TOT_ABSENT_M + TOT_ABSENT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Graduation Rate
JoinModel1516 <- JoinModel1516 %>% filter(ALL_RATE_1516!="PS")
JoinModel1516$ALL_RATE <- chartr(old="LT", new="0-", x=JoinModel1516$ALL_RATE_1516)
JoinModel1516$ALL_RATE <- gsub(pattern="GE", replacement="100-", x=JoinModel1516$ALL_RATE_1516)
JoinModel1516 <- JoinModel1516 %>% separate(ALL_RATE_1516, into=c("GRAD_RATE_A", "GRAD_RATE_B"), sep="-", remove=F)
JoinModel1516 <- JoinModel1516 %>% mutate(GRAD_RATE_B = ifelse(is.na(JoinModel1516$GRAD_RATE_B), GRAD_RATE_A, GRAD_RATE_B))
JoinModel1516$GRAD_RATE_A <- as.numeric(JoinModel1516$GRAD_RATE_A)
JoinModel1516$GRAD_RATE_B <- as.numeric(JoinModel1516$GRAD_RATE_B)
JoinModel1516 <- JoinModel1516 %>% mutate(GRAD_RATE = ((GRAD_RATE_A + GRAD_RATE_B)*.5)) %>% filter(GRAD_RATE > 0)

#Add Geometry rate
JoinModel1516 <- mutate(JoinModel1516, GeomRate = (TOT_GEOM_M + TOT_GEOM_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Biology rate
JoinModel1516 <- mutate(JoinModel1516, BioRate = (TOT_SCIENR_BIOL_M + TOT_SCIENR_BIOL_F)/(TOT_ENR_M + TOT_ENR_F))

#Add AP class rate
JoinModel1516 <- mutate(JoinModel1516, APRate = (TOT_APENR_M + TOT_APENR_F)/(TOT_ENR_M + TOT_ENR_F))

#Add SAT/ACT rate
JoinModel1516 <- mutate(JoinModel1516, SATRate = (TOT_SATACT_M + TOT_SATACT_F)/(TOT_ENR_M + TOT_ENR_F))

#Add In-School Suspension rate
JoinModel1516 <- mutate(JoinModel1516, ISSRate = (TOT_DISCWODIS_ISS_M + TOT_DISCWODIS_ISS_F)/(TOT_ENR_M + TOT_ENR_F))

#Add Sport Participation rate
JoinModel1516 <- mutate(JoinModel1516, SportRate = (TOT_SSPART)/(TOT_ENR_M + TOT_ENR_F))

#Add Harassment and Bullying Allegations (sex, race, and disabilites)
JoinModel1516 <- mutate(JoinModel1516, BullyRate = (SCH_HBALLEGATIONS_SEX + SCH_HBALLEGATIONS_RAC + SCH_HBALLEGATIONS_DIS)/(TOT_ENR_M + TOT_ENR_F))

#Add money spent on activities
JoinModel1516 <- mutate(JoinModel1516, ActivitiesRate = (SCH_NPE_WOFED)/(TOT_ENR_M + TOT_ENR_F))

#Add number of teachers
JoinModel1516 <- mutate(JoinModel1516, TeacherRate = (SCH_FTETEACH_TOT)/(TOT_ENR_M + TOT_ENR_F))

#Add teacher absenteeism rate
JoinModel1516 <- mutate(JoinModel1516, TeacherAbsentRate = SCH_FTETEACH_ABSENT/SCH_FTETEACH_TOT)

#Add non-certified teacher rate
JoinModel1516 <- mutate(JoinModel1516, TeacherNonCertRate = SCH_FTETEACH_NOTCERT/SCH_FTETEACH_TOT)

head(JoinModel1516)
tail(JoinModel1516)
str(JoinModel1516)

#States plot

statesModel <- ggplot(data = JoinModel1516, aes(x = ChrAbsentRate, y = GRAD_RATE)) +
        facet_geo(~ "LEA_STATE", grid = "us_state_grid1") +
        geom_point(aes(alpha = .050, color = "red")) +
        geom_smooth(method ="lm", color = "red", na.rm = TRUE, se = FALSE) +
        theme_bw(base_size = 8) +
        theme(axis.text.x = element_text(angle=45)) +
        theme(strip.text.x = element_text(size = 10)) +
        theme(title = element_text(face = "bold")) +
        xlim (0,1) +
        labs(title = "2015-16 Chronic Absenteeism And Graduation Rates by State", x = "Chronic Absenteeism Rate", y = "Graduation Rate")
statesModel

SportModel <- ggplot(data = JoinModel1516, aes(x = SportRate, y = GRAD_RATE)) +
        facet_geo(~ LEA_STATE,  grid = "us_state_grid1") +
        geom_point(alpha = .050, color = "steelblue") +
        geom_smooth(method ="lm", color = "purple", na.rm = TRUE, se = FALSE) +
        theme_bw(base_size = 8) +
        theme(axis.text.x = element_text(angle= 45)) +
        theme(strip.text.x = element_text(size = 10)) +
        theme(title = element_text(face = "bold")) +
        xlim(0,1) +
        theme(axis.text.x=element_blank())
SportModel

APModel <- ggplot(data = JoinModel1516, aes(x = APRate, y = GRAD_RATE)) +
        facet_geo(~ LEA_STATE, grid = "us_state_grid1") +
        geom_point(alpha = .050, color = "steelblue") +
        geom_smooth(method ="lm", color = "dark green", na.rm = TRUE, se = FALSE) +
        theme_bw(base_size = 8) +
        theme(axis.text.x = element_text(angle= 45)) +
        theme(strip.text.x = element_text(size = 10)) +
        theme(axis.text.x=element_blank()) +
        xlim(0,1)
        

#--------------------------------------------------------------
#Introduction to data plots
#--------------------------------------------------------------
JoinModel1516 <- mutate(JoinModel1516, Total_Enrollment = TOT_ENR_F + TOT_ENR_M)
EnrModel <- ggplot(data = JoinModel1516, aes(x = Total_Enrollment)) +
        geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = .5) +
        geom_vline(aes(xintercept=mean(Total_Enrollment)),
                   color="blue", linetype="dashed", size=1) +
        theme_minimal() +
        xlim(0,5000) +
        labs(title = "2015-16 School Enrollment", x = "Total Enrollment, (Mean = 972.28)", y = "Number of Schools, (100 students per bin)")
EnrModel

APModelHis <- ggplot(data = JoinModel1516, aes(x = APRate)) + 
        geom_histogram(binwidth = .03, fill = "dark green", color = "black", alpha = .5) + 
        geom_vline(aes(xintercept = .0909),
                   color="dark green", linetype="dashed", size=1) +
        xlim(0,1) +
        theme_minimal() +
        labs(title = "2015-16 AP Participation Rate", x = "AP Participation Rate, (Mean = .0909)", y = "Number of Schools")
APModelHis

SportModelHis <- ggplot(data = JoinModel1516, aes(x = SportRate)) + 
        geom_histogram(binwidth = .03, fill = "purple", color = "black", alpha = .5) + 
        geom_vline(aes(xintercept=mean(SportRate)),
                   color="purple", linetype="dashed", size=1) +
        xlim(0,1) +
        theme_minimal() +
        labs(title = "2015-16 Sport Participation Rate", x = "Sport Participation Rate,(Mean = .3205)", y = "Number of Schools")
SportModelHis

posterModel20 <- lm(GRAD_RATE ~ SportRate, data = JoinModel1516)
summary(posterModel20)

posterModel21 <- lm(GRAD_RATE ~ APRate, data = JoinModel1516)
summary(posterModel21)

posterModel22 <- lm(GRAD_RATE ~ ChrAbsentRate, data = JoinModel1516)
summary(posterModel22)
