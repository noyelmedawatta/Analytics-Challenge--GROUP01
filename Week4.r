#=========================================================
# Week 4 - Analytics Challange
#=========================================================
setwd("E:/WS_R/DPA")
#=========================================================
# Data Loading
#=========================================================

#install.packages('psidR')
psid <- read.csv("PSID.csv", header = TRUE)
str(psid )
#plot(psid )
nrow(psid)
ncol(psid)
summary(psid )
#fix(psid)

psid = distinct(psid)

nrow(psid)
ncol(psid)


#=========================================================
# Data Pre Processing
#=========================================================
library(dplyr)
psid <- psid %>% select(c(-1))
psid <- subset(psid, psid$educatn<20)
psid <- subset(psid, psid$kids<20)
psid <- subset(psid, psid$hours!=0)
#psid <- subset(psid, psid$earnings!=0)

psid_dt <- data.frame(psid$intnum, psid$persnum, psid$age, psid$educatn, psid$hours, psid$earnings, psid$married, psid$kids)
psid_dt <- psid_dt[order(psid$persnum),]
psid_dt <- psid_dt[order(psid$intnum),]

psid_dt
fix(psid_dt)

nrow(psid)
ncol(psid)
summary(psid)

psid_age <- c(psid$age)
psid_earning <- c(psid$earnings)
psid_hours <- c(psid$hours)
psid_education <- c(psid$educatn)
psid_kids <- c(psid$kids)
psid_marid <- c(psid$married)

psid_edu_earn_matrix <- cbind(psid_education, psid_earning)
plot(psid_edu_earn_matrix)

psid_hours_earn_matrix <- cbind(psid_hours,psid_earning)
plot(psid_hours_earn_matrix)

psid_kids_hours_matrix <- cbind(psid_kids, psid_hours)
plot(psid_kids_hours_matrix)

psid_kids_earnings_hours_matrix <- cbind(psid_education, psid_earning, psid_kids)
plot(psid_kids_earnings_hours_matrix)

psid_marrid_earn_matrix <- cbind(psid_marid,psid_earning)
plot(psid_marrid_earn_matrix)

psid_marrid_kids_matrix <- cbind(psid_marid,psid_kids)
plot(psid_marrid_kids_matrix)

psid_marrid_age_matrix <- cbind(psid_marid, psid_age)
plot(psid_marrid_age_matrix)


kc = kmeans(psid[, 3:7],3)
kc

par(mfrow=c(1,2))
plot(psid[,3:7], col=kc$cluster)

