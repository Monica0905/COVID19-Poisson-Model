#rm(list=ls())
#data<-data2<-read.csv("../data/processed/daily_new_cases_long_usa_facts.csv")
library(lme4)
library(dplyr)
library(base)

covid.data.summary.county <-function(data, atleast_cum_cases=5) {
    if (class(data$date)!="Date") {
      data$date <-  as.Date(data$date, format="%m/%d/%y")
    }
    county.summary <- data %>% group_by(county_fips) %>% 
      summarise(state=state_name[1], county=county_name[1], current.date=min(date), 
                total.day=as.numeric(max(date)-min(date)), current_total=max(cum_cases),
                total.days.atleast=sum(cum_cases>=atleast_cum_cases))
  return(county.summary)
}
covid.mod.data.usa<-function(data, method="day", atleast_cum_cases=5, days_since_1stcase=5, 
                             predict_from_whichday=as.Date("4/6/20", format="%m/%d/%y"),
                             today=as.Date(Sys.Date(), format="%m/%d/%y"), 
                             days_to_predict=3,norm.day=TRUE) {
  

  
  # check data
  # make sure the first 8 items are as follows
  # if (any(!(names(data)[1:8]) %in% c("state_fips","county_fips","state_name","county_name",
  #                             "date","day","new_cases","cum_cases","ttl_pop"))) stop("variables names are incorrect")
  
  # if cumulative cases are missing, generate cumulative cases
  # check if the data is up-to-date
  
  if (class(data$date)!="Date") {
    data$date <-  as.Date(data$date, format="%m/%d/%y")
  }
  
  if (today!=max(data$date)) 
    {
      warnings(paste("current date in data is", max(data$date),"\n"))
      today <- max(data$date)
      
      if (predict_from_whichday > today) { 
        predict_from_whichday <- today
      }
    }
      
  
  data <- data[data$county_name!="6000" & data$county_name!="2270",]
  data$new_cases[data$new_cases<0 ]<-0
  ## need to check data
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
      county.summary$county.keep <- county.summary$total.date1>=5+days_since_1stcase
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

 
  return(list(data=data, county.summary=county.summary))
  
}
