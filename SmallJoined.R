#read in joined data, file from Nat

library(data.table)
library(utils)

#2013-14 School year
smalljoined1314 <- fread(file.path("data", "joined1314.csv"), select = c(1:6, 13:14, 37:38, 45:46, 53:54, 66:67, 76:77, 106, 115:116, 178:179, 200:206))
head(smalljoined1314)

#2015-16 School year
setwd("C:/Users/hlevy/Documents/My Documents/Blog Posts/AbsenteeismGraduationTwoYears/AbsentGraduate/data")
smalljoined1516 <- read.csv("joined1516a.csv", header = TRUE)
head(smalljoined1516)
tail(smalljoined1516)
str(smalljoined1516)