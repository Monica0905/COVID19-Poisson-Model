library(lme4)
library(dplyr)
library(ggplot2)
library("numDeriv")
library("RCurl") 
source("covid_mod_data2.R")
data.case <- read.csv("../data/processed/daily_new_cases_long_usa_facts.csv")
census.ses <- read.csv("../data/processed/census data/socioeconomics.csv")
census.pop <- read.csv("../data/processed/census data/population.csv")
census.metro <- read.csv("../data/processed/census data/metropolitan.csv")
ny.case <- data.case[data.case$state_name=="NY",]
ny.data <- merge(ny.case, census.pop, by.x="county_fips", by.y="county_fips", all.x=TRUE, all.y=FALSE)
ny.data <- merge(ny.data, census.ses, by.x="county_fips", by.y="county_fips", all.x=TRUE, all.y=FALSE)
ny.data <- merge(ny.data, census.metro, by.x="county_fips", by.y="county_fips", all.x=TRUE, all.y=FALSE)
ny.data$county_name<-droplevels(ny.data$county_name)
ny.data<-ny.data[ny.data$cum_cases>1, ]

ny.county <- names(table(ny.data$county_fips))
ny.data$new.day<-rep(0, nrow(ny.data))
for (i in 1:length(ny.county)) {
  ny.data$new.day[ny.data$county_fips==ny.county[i]]<-ny.data$day[ny.data$county_fips==ny.county[i]]-min(ny.data$day[ny.data$county_fips==ny.county[i]])
}

ny.data$day2 <- ny.data$day^2
ny.data$new.day2 <- ny.data$new.day^2

ny.data$logpop <- log(ny.data$county_pop)
ny.data <- ny.data[!is.na(ny.data$logpop),]
ny.data$new_cases[ny.data$new_cases<0]<-0
ny.data$mday <- (ny.data$new.day-10)/10
ny.data$mday2 <- ny.data$mday^2
ny.data$county_name<-droplevels(ny.data$county_name) 
ny.data$nyc_vincinity <- ny.data$county_fips %in% c(36005, 36047,36081,36059,36061,36103,36119)
ny.data$logdensity <- log(ny.data$pop_dens)
ny.metro <- ny.data[ny.data$metro==1,]
ny.metro$county_name<-droplevels(ny.metro$county_name) 
#Bronx, Brooklyn, Manhattan, Queens, Nassau, Suffolk, Westchester, Rockland, Orange Richmond counties
ny.metro$highrisk <- ny.metro$county_fips %in% c(36005, 36047,36061,36081,36103,36119, 36085, 36087,36071,36059)
write.table(ny.metro, file="../data/processed/ny_metro.csv", sep=",", row.names=FALSE)

# Trend over visit (ordinal)
ny.data[ny.data$metro==1,] %>%
  ggplot(aes(x = day, y = new_cases)) +
  facet_wrap(~county_name) +geom_line()

res <- glmer(new_cases~highrisk+(1|county_fips)+offset(logpop), data=ny.metro, 
             family=poisson(link="log"))
summary(res)
ff2 <-formula("new_cases~(mday+mday2)+(mday|county_fips)+offset(logpop)")
res2 <- update(res, ff2)
ff3 <-formula("new_cases~(mday+mday2)+logdensity+(mday|county_fips)+offset(logpop)")
res3 <- update(res2, ff3)
ff4 <-formula("new_cases~(mday+mday2)+pct_pop_poverty+logdensity+(mday|county_fips)+offset(logpop)")
res4 <- update(res3, ff4)

ff5 <-formula("new_cases~(mday+mday2)+(mday+mday2|county_fips)+offset(logpop)")
res5 <- update(res4, ff5)

ff6 <-formula("new_cases~(mday+mday2)+logdensity+pct_pop_poverty+(mday+mday2|county_fips)+offset(logpop)")
res6 <- update(res5, ff6)

ss <- getME(res6,c("theta","fixef"))

res6.b <- update(res6,start=ss,control=glmerControl(optimizer="bobyqa",
                                                         optCtrl=list(maxfun=2e5)))

ff6 <-formula("new_cases~nyc_vincinity*(mday+mday2)+(mday+mday2|county_fips)+offset(logpop)")
res6 <- update(res5, ff6)

ny.metro$unemploy_rate <- ny.metro$unemploy_rate-mean(ny.metro$unemploy_rate)
ny.metro$pct_pop_poverty <- ny.metro$pct_pop_poverty-mean(ny.metro$pct_pop_poverty)
ny.metro$pct_pop_elder <- ny.metro$pct_pop_elder-mean(ny.metro$pct_pop_elder)

ff7 <- formula("new_cases~nyc_vincinity*(mday+mday2)+(mday+mday2|county_fips)+offset(logpop)")
res7 <- update(res6, ff7)
ss <- getME(res7,c("theta","fixef"))
res7.b<-update(res7, start=ss)

ff8 <- formula("new_cases~(mday+mday2)*(nyc_vincinity+pct_pop_elder)+(mday+mday2|county_fips)+offset(logpop)")
res8 <- update(res7, ff8)


ff8 <- formula("new_cases~(mday+mday2)*(nyc_vincinity+pct_pop_elder)+(mday+mday2|county_fips)+offset(logpop)")
res8 <- update(res7, ff8)

#random intercept
ny.metro$fit <- predict(res, type="response")
ny.metro$cum_fit <- ave(ny.metro$fit, ny.metro$county_fips, FUN=cumsum)
# random slope
ny.metro$fit2 <- predict(res2, type="response")
ny.metro$cum_fit2 <- ave(ny.metro$fit2, ny.metro$county_fips, FUN=cumsum)
# random quadratic term
ny.metro$fit3 <- predict(res5, type="response")
ny.metro$cum_fit3 <- ave(ny.metro$fit3, ny.metro$county_fips, FUN=cumsum)

ny.metro$fit4 <- predict(res7, type="response")
ny.metro$cum_fit4 <- ave(ny.metro$fit4, ny.metro$county_fips, FUN=cumsum)

plot(ny.metro$fit, ny.metro$new_cases, cex=0.2, pch=19)
abline(0,1)
points(ny.metro$fit2, ny.metro$new_cases, cex=0.4, pch=19, col="blue")
plot(ny.metro$fit3, ny.metro$new_cases, cex=0.4, pch=19, col="red")
points(ny.metro$fit4, ny.metro$new_cases, cex=0.4, pch=19, col="yellow")



points(ny.metro$fit4, ny.metro$new_cases, cex=0.2, pch=19, col="red")

abline(0,1, col="red")
plot(ny.metro$cum_fit, ny.metro$cum_cases, pch=19, cex=0.1)
points(ny.metro$cum_fit4, ny.metro$cum_cases, pch=19, cex=0.1, col="red")
abline(0,1, col="red")    


## standardize predictors
numcols <- grep("^c\\.",names(ny.metro))
ny.metro.s <- ny.metro
ny.metro.s[,numcols] <- scale(ny.metro.s[,numcols])
res5.sc <- update(res5,data=ny.metro.s)
#check singularity
tt <- getME(res5.sc,"theta")
ll <- getME(res5.sc,"lower")
min(tt[ll==0])
#check convergence
derivs1 <- res5.sc@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

max(pmin(abs(sc_grad1),abs(derivs1$gradient)))

dd <- update(res5.sc,devFunOnly=TRUE)
pars <- unlist(getME(res5.sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))

ss <- getME(res5.sc,c("theta","fixef"))
res5.sc2 <- update(res5.sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
                                                 optCtrl=list(maxfun=2e5)))

