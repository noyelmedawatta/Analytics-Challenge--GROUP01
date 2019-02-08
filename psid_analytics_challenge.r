# PSID - Analytics Challenge
#==========================================
setwd("E:/WS_R/DPA")
#install.packages("dplyr")
#install.packages("animation")
#install.packages("tidyverse")
library(dplyr)
library(animation)
library(tidyverse)
#==========================================
# Load PSID dataset
# Remove duplocate records if any
# Preview dataset
# Check Summary of dataset
psid<-read.csv("PSID.csv")%>%
  distinct() %>%
  glimpse()
summary(psid)

psid <- as_tibble(psid)
psid

#==========================================
# Remove outliers
# Max Kids = 99 and Max Education = 99
# These could be outliers
psid<-filter(psid, psid$educatn<=20 & psid$kids<=20)
summary(psid)
colnames(psid)

psid<-psid %>% 
  rename(
    sequence_number = Seq.No,
    interview_number = intnum,
    person_number = persnum,
    age = age,
    education = educatn,
    earnings = earnings,
    number_of_hours = hours,
    number_of_kids = kids,
    maritrial_status = married
  )

kc = kmeans(psid[, 4:9],3)
kc

par(mfrow=c(1,2))
plot(psid[,4:9], col=kc$cluster)

education_histogram<-hist(psid$education, xlab="Education Level", ylab="Frequency", main="PSID Education Levels", col="blue", border="red")

set.seed(12345)
kmeans.ani(psid[1:2], 3)

set.seed(12345)
kmeans.ani(psid[2:3], 3)

psid_sample<-data.frame(psid$age, psid$education, psid$earnings, psid$number_of_hours, psid$number_of_kids, psid$maritrial_status)
glimpse(psid_sample)

hist(psid$education, xlab="Education", ylab="Frequency", main="Histogram of Education", col="blue", border = "red")
hist(psid$earnings, xlab="Earnings", ylab="Frequency", main="Histogram of Education", col="blue", border = "red")

par(mfrow=c(1,2))
boxplot(psid$earnings~psid$education, data = psid)
boxplot(psid$number_of_kids~psid$education, data = psid)

par(mfrow=c(1,2))
boxplot(psid$number_of_hours~psid$number_of_kids, data = psid)
boxplot(psid$earnings~psid$number_of_kids, data = psid)

boxplot(psid$earnings~psid$number_of_kids, data = psid)
boxplot(psid$education~psid$maritrial_status, data = psid)
boxplot(psid$number_of_kids~psid$maritrial_status, data = psid)


scaled_psid <- psid %>%
  mutate(age = scale(age),
         education = scale(education),
         earnings = scale(earnings),
         number_of_hours = scale(number_of_hours),
         number_of_kids = scale(number_of_kids)) %>%
  select(-c(sequence_number, interview_number, person_number, maritrial_status))
glimpse(scaled_psid)
summary(scaled_psid)

set.seed(12345)
kmeans.ani(scaled_psid[1:2], 3)

set.seed(12345)
kmeans.ani(scaled_psid[2:3], 3)

set.seed(12345)
kmeans.ani(scaled_psid[4:3], 3)

set.seed(12345)
kmeans.ani(scaled_psid[4:5], 3)