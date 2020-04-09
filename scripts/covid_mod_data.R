rm(list=ls())
data<-data2<-read.csv("../data/processed/daily_new_cases_long_usa_facts.csv")
library(lme4)
library(dplyr)
library(base)

covid.data.summary.county <-function(data, atleast_cum_cases=5) {
    if (class(data$date)!="Date") {
      data$date <-  as.Date(data$date, format="%m/%d/%y")
    }
    county.summary <- data %>% group_by(county_fips) %>% 
      summarise(current.date=min(date), total.day=as.numeric(max(date)-min(date)),
                total.days.atleast=sum(cum_cases>=atleast_cum_cases))
  return(county.summary)
}
covid.mod.data.usa<-function(data, method="day", atleast_cum_cases=5, days_passed=5, 
                             which_day_start=NULL, days_to_predict=today-predict_from_whichday+3,
                             predict_from_whichday=max(as.Date("4/6/20", format="%m/%d/%y")),
                             today=max(as.Date(data$date, format="%m/%d/%y")), norm.day=TRUE,
                             data.summary=TRUE) {
  
  
  
  # check data
  # make sure the first 8 items are as follows
  # if (any(!(names(data)[1:8]) %in% c("state_fips","county_fips","state_name","county_name",
  #                             "date","day","new_cases","cum_cases","ttl_pop"))) stop("variables names are incorrect")
  
  # if cumulative cases are missing, generate cumulative cases
  # check if the data is up-to-date
  
  if (class(data$date)!="Date") {
    data$date <-  as.Date(data$date, format="%m/%d/%y")
  }
  
  if (today!=max(data$date)) stop("not all counties have up-to-date data")
  
  data <- data[data$county_name!="6000" & data$county_name!="2270",]
  data$new_cases[data$new_cases<0 ]<-0
  data<-data[!is.na(data$county_pop) & !is.na(data$new_cases),]
  
  ## filter counties by days or by cum_cases 
  #current.date=min(date), total.day=max(date),
  #total.days.atleast=sum(cum_cases>=atleast_cum_cases))
  if (method=="cases") {
      if (is.null(atleast_cum_cases))  atleast_cum_cases <- 0
      county.summary <-  covid.data.summary.county(data, atleast_cum_cases=atleast_cum_cases)
      county.summary$county.keep <- county.summary$total.days.atleast>=5+days_to_predict
      data <- data[data$county_fips %in% county.summary$county_fips[county.summary$county.keep==1],]
      data <- data[data$cum_cases>=atleast_cum_cases,]     
      # now data contains only counties starts from the day with at least "atleast_cum_cases" and 5 days of obs
      }
  if (method=="day") { # not tested
      atleast_cum_cases <- 0
      county.summary <-  covid.data.summary.county(data, atleast_cum_cases=atleast_cum_cases)
      county.summary$county.keep <- county.summary$total.date1>=5+days_passed
      data <- data[data$county_fips %in% county.summary$county_fips[county.summary$county.keep==1],]
      data <- data[data$day>=days_passed,]     
      
      }

  
  # realign the day to start from 0
  data <- data %>% group_by(county_fips) %>% mutate(new.day=as.numeric(date-min(date))) 
  
  # save a copy of true Y so we can compare later
  data$new_cases_true <- data$new_cases  
  
  # prepare data for prediction
  data$tx <- data$new.day
  
  if (predict_from_whichday < today)  # back-prediction is required 
  { 
    data$new_cases[data$date > predict_from_whichday] <-NA
    data$tx[data$date > predict_from_whichday] <-NA
  }
  
  # the time variable used to standardize data 
  # (only use the time period that is in the training data)
  
  
  ## generate rows for predictions
  if ((today - predict_from_whichday) < days_to_predict) # need to predict to future
  {
    onedaycopy <- data[data$date==today,]  ##take the last row of each county  
    onedaycopy$new.day<-onedaycopy$new.day+1
    onedaycopy$date<-onedaycopy$date+1
    onedaycopy$new_cases<-NA  
    onedaycopy$new_cases_true<-NA
    onedaycopy$tx<-NA
    pred.data<-onedaycopy
    tt <-1
    while (tt<=days_to_predict-(today-predict_from_whichday)) {
      pred.data <- rbind(pred.data, onedaycopy)
      onedaycopy$new.day<-onedaycopy$new.day+1
      onedaycopy$date<-onedaycopy$date+1
      tt <- tt+1
    }  
    data <- rbind(data, pred.data)
  }

  if (norm.day) {
    data <- data%>% group_by(county_fips) %>% mutate(zday=(new.day-mean(tx, na.rm=TRUE))/(var(tx, na.rm=TRUE))^0.5,
                                                     cday=new.day-mean(tx, na.rm=TRUE))
  }
  data <- data%>% arrange(new.day)
  data$zday2 <- data$zday^2
  
 # data2$logpop <- log(data2$ttl_pop)
  data <- data %>% ungroup()
  
  return(data=data)
  
}


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


data2$fit4 <- exp(predict(res4))

data$fit4 <- exp(predict(res4, newdata=data))
## in-sample fit
data2$cum_fit2 <- ave(data2$fit4, data2$county_fips, FUN=cumsum)
plot(data2$cum_cases, data2$cum_fit2, pch=19, cex=0.1)
abline(0,1)

## red are future predicted points
data$cum_fit2 <- ave(data$fit4, data$county_fips, FUN=cumsum)
plot(data$cum_cases, data$cum_fit2, pch=19, cex=0.1)
points(data$cum_cases[is.na(data$new_cases)], data$cum_fit2[is.na(data$new_cases)], col="red", cex=0.1)
abline(0,1)

