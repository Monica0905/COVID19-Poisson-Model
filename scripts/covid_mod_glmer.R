rm(list=ls())
library(lme4)
library(dplyr)
library(base)

source("covid_mod_data2.R")

data0<-read.csv("../data/processed/daily_new_cases_long_usa_facts.csv")
today <- max(as.Date(data0$date, format="%m/%d/%y"))
predict_from_whichday<-as.Date("4/1/20", format="%m/%d/%y")
data.processed<-covid.mod.data.usa(data0, method="cases", atleast_cum_cases=5, 
                             predict_from_whichday=predict_from_whichday,
                             today=today,
                            days_to_predict=today-predict_from_whichday+3, norm.day=TRUE)

data <- data.processed$data
data$zday3 <- data$zday^3
#data$zday3 <- data$zday3-mean(data$zday3)
data$logpop <- log(data$county_pop)
data2<-data[!is.na(data$new_cases),]
res <- glmer(new_cases~1
             +(1|county_fips)+offset(logpop), data=data2, 
             family=poisson(link="log"))
ff2 <-formula("new_cases~zday+zday2+(1|county_fips)+offset(logpop)")
res2 <- update(res, ff2)
ff3 <-formula("new_cases~zday+zday2+(zday|county_fips)+offset(logpop)")
res3 <-update(res2,ff3)

### currently the best mode, future prediction is a bit too low 
ff4<-formula("new_cases~zday+zday2+(zday+zday2|county_fips)+offset(logpop)")
res4<-update(res3, ff4)

### cubic doesn't run
ff5<-formula("new_cases~zday+zday2+zday3+(zday+zday2|county_fips)+offset(logpop)")
res5<-update(res4, ff5)


data2$fit5 <- exp(predict(res5))

data$fit5 <- exp(predict(res5, newdata=data))
## in-sample fit
data2$cum_fit2 <- ave(data2$fit5, data2$county_fips, FUN=cumsum)
plot(data2$cum_cases, data2$cum_fit2, pch=19, cex=0.1)
abline(0,1)

## red are future predicted points
data$cum_fit2 <- ave(data$fit5, data$county_fips, FUN=cumsum)
plot(data$cum_cases, data$cum_fit2, pch=19, cex=0.1)
points(data$cum_cases[is.na(data$new_cases)], data$cum_fit2[is.na(data$new_cases)], col="red", cex=0.1)
abline(0,1)

