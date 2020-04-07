rm(list=ls())
load("daily cases in long format.RDATA")
data <- daily_cases
library(lme4)
library(dplyr)
library(base)
covid.mod.data.usa<-function(data, method="day", atleast_cases=NULL, days_passed=5, 
                             which_day_start=NULL, days_to_predict=3,
                             predict_from_whichday=max(as.Date("3/26/20", format="%m/%d/%y")),
                             today=max(as.Date(data$date, format="%m/%d/%y")), norm.day=TRUE,
                             data.summary=TRUE) {
  
  
  
  # check data
  # make sure the first 8 items are as follows
  # if (any(!(names(data)[1:8]) %in% c("state_fips","county_fips","state_name","county_name",
  #                             "date","day","new_cases","cum_cases","ttl_pop"))) stop("variables names are incorrect")
  
  # if cumulative cases are missing, generate cumulative cases
  
  data <- data[data$county_name!="6000" & data$county_name!="2270",]
  data$new_cases[data$new_cases<0 ]<-0
  
  if (!("cum_cases"%in% names(data))) 
  {
    data$cum_cases <- ave(data$new_cases, data$county_fips, FUN=cumsum)
  }
  
  # convert date into R date variable
  data$date <- as.Date(data$date, format="%m/%d/%y")
  
  # check if the data is up-to-date
  if (today!=max(data$date)) stop("not all counties have up-to-date data")
  
  #notdo generate a data dictionary but may not needed
  notdo <- TRUE
  if (!notdo) {
    # create a first day dirctionary
    county.1stday<-data %>% group_by(county_fips) %>% summarise(date1=date[day==1],dateN=date[day==max(day)])
  }
  
  # methods of data inclusion
  if (method=="case") # based on at least # of cases 
  { 
    data <- data[data$cum_cases>=atleast_cases, ] 
  }
  else if (method=="day") # based on #days past day of 1st case
  {
    data <- data[data$day>=days_passed,]
  }
  else if (method=="whichday") # specify a minimal starting date
  {
    data <- data[data$date>=which_day_start,]
  }
  
  ## notdo, still date dictionary
  if (!notdo) {
    # let the day starts from 1
    current.1stday <- data %>% group_by(county_fips) %>% summarise(current.date1=min(day))
    current.1stday <- merge(county.1stday, current.1stday, by="county_fips")
  }
  
  # realign the day to start from 0
  data <- data %>% group_by(county_fips) %>% mutate(new.day=as.numeric(date-min(date))) 
  
  # save a copy of true Y so we can compare later
  data$new_cases_true <- data$new_cases  
  
  # prepare data for prediction
  
  if (predict_from_whichday < today)  # back-prediction is required 
  { 
    data$new_cases[data$date > predict_from_whichday] <-NA
  }
  
  ## generate rows for predictions
  if ((today - predict_from_whichday) < days_to_predict) # need to predict to future
  {
    onedaycopy <- data[data$date==today,]  ##take the last row of each county  
    onedaycopy$new.day<-onedaycopy$new.day+1
    onedaycopy$date<-onedaycopy$date+1
    onedaycopy$new_cases<-NA  
    onedaycopy$new_cases_true<-NA
    pred.data<-onedaycopy
    tt <-1
    while (tt<=days_to_predict-(today-predict_from_whichday)) {
      pred.data <- rbind(pred.data, onedaycopy)
      onedaycopy$new.day<-onedaycopy$new.day+1
      onedaycopy$date<-onedaycopy$date+1
      tt <- tt+1
    }  
  }
  data <- rbind(data, pred.data)
  if (norm.day) {
    data <- data%>% group_by(county_fips) %>% mutate(zday=(new.day-mean(new.day))/(var(new.day))^0.5,
                                                     cday=new.day-mean(new.day))
  }
  data <- data%>% arrange(new.day)
  data2$zday2 <- data2$zday^2
  data2$logpop <- log(data2$ttl_pop)
  data <- data %>% ungroup()
  
  return(data=data)
  
}


today <- max(as.Date(data$date, format="%m/%d/%y"))
data2 <- covid.mod.data.usa(data, method="day", atleast_cases=NULL, days_passed=5, 
                            which_day_start=NULL, days_to_predict=0,
                            predict_from_whichday=today,
                            today=today, norm.day=TRUE)

