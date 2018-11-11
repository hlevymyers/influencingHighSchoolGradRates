#clean up high school graduation numbers
#mutate graduation ranges to calculate a rate when a range is given
#all exact graduation ranges are whole numbers
#save back to .rda file to use in Tableau

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(stringr)

#School Year 2013-14
cleanHSGrad1314 <- ACGR1314
head(cleanHSGrad1314)

#Calculates grad rates for ranges and includes all specific grad rates
cleanHSGrad1314$NCESSCH <- as.character(cleanHSGrad1314$NCESSCH)
cleanHSGrad1314$NCESSCH <- str_pad(cleanHSGrad1314$NCESSCH, width = 12, side = c("left"), pad = "0")
cleanHSGrad1314 <- dplyr::rename(cleanHSGrad1314, NCES_COMBO = NCESSCH)
Graddata1314 <- cleanHSGrad1314 %>% filter(ALL_RATE!="PS")
Graddata1314$ALL_RATE <- chartr(old="LT", new="0-", x=Graddata1314$ALL_RATE)
Graddata1314$ALL_RATE <- gsub(pattern="GE", replacement="100-", x=Graddata1314$ALL_RATE)
Graddata1314 <- Graddata1314 %>% separate(ALL_RATE, into=c("GRAD_RATE_A", "GRAD_RATE_B"), sep="-", remove=F)
Graddata1314 <- Graddata1314 %>% mutate(GRAD_RATE_B = ifelse(is.na(Graddata$GRAD_RATE_B), GRAD_RATE_A, GRAD_RATE_B))
Graddata1314$GRAD_RATE_A <- as.numeric(Graddata1314$GRAD_RATE_A)
Graddata1314$GRAD_RATE_B <- as.numeric(Graddata1314$GRAD_RATE_B)
Graddata1314 <- Graddata1314 %>% mutate(GRAD_RATE= ((GRAD_RATE_A + GRAD_RATE_B)*.5)) %>% filter(GRAD_RATE > 0)
head(Graddata1314)
tail(Graddata1314)
str(Graddata1314)

#School Year 2015-16
cleanHSGrad1516 <- ACGR1516
head(cleanHSGrad1516)

#Calculates grad rates for ranges and includes all specific grad rates
cleanHSGrad1516$NCESSCH <- as.character(cleanHSGrad1516$NCESSCH)
cleanHSGrad1516$NCESSCH <- str_pad(cleanHSGrad1516$NCESSCH, width = 12, side = c("left"), pad = "0")
cleanHSGrad1516 <- dplyr::rename(cleanHSGrad1516, NCES_COMBO = NCESSCH)
Graddata1516 <- cleanHSGrad1516 %>% filter(ALL_RATE != "PS")
Graddata1516$ALL_RATE_1516 <- chartr(old="LT", new="0-", x=Graddata1516$ALL_RATE)
Graddata1516$ALL_RATE_1516 <- gsub(pattern="GE", replacement="100-", x=Graddata1516$ALL_RATE)
Graddata1516 <- Graddata1516 %>% separate(ALL_RATE, into=c("GRAD_RATE_A", "GRAD_RATE_B"), sep="-", remove=F)
Graddata1516 <-Graddata1516 %>% mutate(GRAD_RATE_B=ifelse(is.na(Graddata1516$GRAD_RATE_B), GRAD_RATE_A, GRAD_RATE_B))
Graddata1516$GRAD_RATE_A <- as.numeric(Graddata1516$GRAD_RATE_A)
Graddata1516$GRAD_RATE_B <- as.numeric(Graddata1516$GRAD_RATE_B)
Graddata1516 <- Graddata1516 %>% mutate(GRAD_RATE = ((GRAD_RATE_A + GRAD_RATE_B)*.5)) %>% filter(GRAD_RATE > 0)
head(Graddata1516)
tail(Graddata1516)
str(Graddata1516)

unique(Graddata1314$NCES_COMBO)
unique(Graddata1516$NCES_COMBO)

Graddata1516 %>% 
        select(GRAD_RATE) %>% 
        is.na() %>% 
        sum() 


