load("daily cases in long format.RDATA")
library(lme4)
daily_cases$new_cases[daily_cases$new_cases<0]<-0
sum(cc<=10)  # currently 335 counties with fewer than 5 days of entries
data$cum_cases <- ave(data$new_cases, data$county_fips, FUN=cumsum)

cc2<-table(data$county_fips[data$cum_cases>10])

# drop county with fewer than 10 cumulative cases.
data <- data[data$cum_cases>10,]
ss <- table(data$state_fips) # entries per state
cc <- table(data$county_fips) # entries per county, 522 county



## county_fips=6000 is Grand Princess Cruise Ship
data <- data[data$county_fips!="6000",]
#tt<-table(daily_cases$county_fips)
#data$NYC <- data$county_name %in% c("Queens County",
 #                               "New York County","Kings County","Bronx County")
data$logpop <- log(data$ttl_pop)

## standardize predictors is imporant to get this kind of model to converge
## (and will probably need to orthogonalize them as well)

#data$day2 <- data$day^2
data$eday <- exp(-data$day)
data$cday<-data$day-mean(data$day)
data$zday<-data$cday/diff(range(data$day))
#demean <-function(x) return(x-mean(x))
#zmean <- function(x) return((x-mean(x))/var(x)^0.5)
for (i in 1:length(cc2)) {
  data$day[data$county_fips==names(cc2)[i]] <- data$day[data$county_fips==names(cc2)[i]]-min(data$day[data$county_fips==names(cc2)[i]])+1 
 # data$cday[data$county_fips==names(cc2)[i]]<-demean(data$day[data$county_fips==names(cc2)[i]])
#  data$zday[data$county_fips==names(cc2)[i]]<-zmean(data$day[data$county_fips==names(cc2)[i]])
  }
data$cday2<-data$cday^2
data$zday2<-data$zday^2



data$state_name<-factor(data$state_name)
data$state_name<-relevel(data$state_name, ref="NY")
ss.name <- names(table(data$state_name))
state.dummies<-model.matrix(~data$state_name)
state.zdummies<- apply(state.dummies, 2, zmean)
state.zdummies<-as.data.frame(state.zdummies)
names(state.zdummies)<-ss.name
data <- cbind(data, state.zdummies)

res <- glmer(new_cases~zday+zday2
             +(zday|county_fips)+offset(logpop), data=data, 
             family=poisson(link="log"))

ff2<-formula("new_cases~zday+zday2+(zday+zday2|county_fips)+offset(logpop)")

res2 <- update(res, ff2)

ff3 <- "new_cases~zday+zday2"
for (k in 1:50) {
  ff3 <- paste(ff3, "+",ss.name[k+1], sep="")
}
ff3<-paste(ff3, "++(zday+zday2|county_fips)+offset(logpop)", sep="")
## this model still doesn't converge yet
res3 <- update(res2, formula(ff3))


summary(res)
summary(res2)
summary(res3) 
# NY is the reference group
# NJ fares out much worse than NY after accounting for tot pop

pp.c <- exp(predict(res))
pp.c2<-exp(predict(res2))
pp.c3<-exp(predict(res3))
#pull out county level random effect
#rr.c<- ranef(res)
## pull out prediction interval of these estiamtes will be time consuming
#https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html

## assess cumulative fitted values
data$new_fit<-pp.c
data$new_fit2<-pp.c2
data$new_fit3<-pp.c3

data$cum_fit <- ave(data$new_fit, data$county_fips, FUN=cumsum)
data$cum_fit2 <- ave(data$new_fit2, data$county_fips, FUN=cumsum)
data$cum_fit3 <- ave(data$new_fit2, data$county_fips, FUN=cumsum)

# the predicted values with or without state dummies are the same
# but with state_dummies the prediction intervals will be narrower
plot(data$cum_cases, data$cum_fit, pch=19, cex=0.1)
points(data$cum_cases, data$cum_fit2, cex=0.1, col="red")
points(data$cum_cases, data$cum_fit3, cex=0.1, col="green")
abline(0,1)

#
AIC(res)
AIC(res2)
AIC(res3)
anova(res, res2, res3)



## not run
#res4<-glmer.nb(new_cases~zday
#         +(zday|county_fips)+offset(log(ttl_pop/1000)), data=data, 
#         tol = 5e-3, verbose = TRUE, nb.control = NULL)

# some interesting reading
#https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/