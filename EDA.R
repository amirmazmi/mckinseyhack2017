#---------------------------------------------------------
# Analytics Vidhya and McKinsey Analytics 
#  Hackathon
#   
#   18 Nov 2017
# 
# 
#   Analysis.R - Exploratory Data Analysis
# 
# 
#   rm(list=ls())
# Load libraries
#---------------------------------------------------------
library(pacman)
p_load(ggplot2)
p_load(lubridate)
p_load(timetk)
p_load(gridExtra)
p_load(dplyr)

#---------------------------------------------------------
pathhome <- 'C:/Users/Speedy/Desktop/mckinsey'
setwd(pathhome)
getwd()

img_path <- "./pictures/"


trainfile <- "./train_aWnotuB.csv"  
testfile <- "./test_BdBKkAj.csv"    
#---------------------------------------------------------    

traindata <- read.csv(trainfile )
testdata <- read.csv(testfile)

str(traindata)
summary(traindata)
dim(traindata)
head(traindata)

traindata$Junction <- as.factor(traindata$Junction)
testdata$Junction <- as.factor(testdata$Junction)
levels(traindata$Junction)
levels(testdata$Junction)

# separate data into junctions
dfjunc <- lapply( levels(traindata$Junction), function(k){
                  traindata[ which(traindata$Junction == k ) ,]} )
dfjunc <- setNames(dfjunc, seq(1,4) )

dftestjunc <- lapply( levels(testdata$Junction), function(k){
                testdata[ which(testdata$Junction == k ) ,]} )
dftestjunc <- setNames(dftestjunc, seq(1,4) )


# extract time-series
dfjunc$`1`$DateTime <- ymd_hms(dfjunc$`1`$DateTime)
dfjunc$`1`[,"date"] <- date(dfjunc$`1`$DateTime)
dfjunc1 <- tk_augment_timeseries_signature(dfjunc$`1`)

dftestjunc$`1`$DateTime <- ymd_hms( dftestjunc$`1`$DateTime)
dftestjunc$`1`[,"date"] <- date(dftestjunc$`1`$DateTime)
dftestjunc1 <- tk_augment_timeseries_signature(dftestjunc$`1`)




###########################################################
# consider first junction
###########################################################

# inspect first junction
str(dfjunc1)
summary(dfjunc1)  
names(dfjunc1)

names(dfjunc1)

min( dfjunc1$DateTime)
# "2015-11-01 UTC"
max( dfjunc1$DateTime)
# "2017-06-30 23:00:00 UTC"

###########################################################
defmargin <- par("mar")
par( mar=c(5.1, 5.1, 4.1, 1.1))
###########################################################


# look at time series
plot( dfjunc1$DateTime, dfjunc1$Vehicles, type='l', xaxt='n',
      col="plum",
      main="Vehicles over Time",
      xlab="Time",
      ylab="Vehicles")
axis.POSIXct(side=1, at=seq(min(dfjunc1$DateTime), max(dfjunc1$DateTime),
                            by="1 month"),
             format="%b%y")

# zoom in
plot( dfjunc1$DateTime[1:1288], dfjunc1$Vehicles[1:1288], type='l',
      xaxt='n', col="sienna",
      main="Vehicles over Time",
      xlab="Time",
      ylab="Vehicles")
axis.POSIXct(side=1, 
             at=seq(min(dfjunc1$DateTime[1:1288]), max(dfjunc1$DateTime[1:1288]),
                    by="1 week"),
             format="%e %b'%y")


#----------------------------------------------------------
# look at hourly average
hourbox <- boxplot( dfjunc1$Vehicles ~ dfjunc1$hour,
                    col="lightsalmon", outcol="red",
                    main="Junction 1 - Hour of day",
                    xlab="hour of day",
                    ylab="vehicles")

hourinterval <- aggregate( x=list( vehicles= dfjunc1$Vehicles),
                           by=list( hour= dfjunc1$hour),
                           mean)
hourbar <- ggplot(hourinterval, aes( x=hour, y=vehicles)) +
                geom_bar(stat="identity") +
                coord_cartesian( ylim=c(0,70))

#----------------------------------------------------------
# look at weekly average
weekbox <- boxplot( dfjunc1$Vehicles ~ dfjunc1$wday.lbl,
                    col="powderblue",  outcol="red",
                    main="Junction 1 - Day of week",
                    xlab="day of week",
                    ylab="vehicles")

weekinterval <- aggregate( x=list( vehicles=dfjunc1$Vehicles),
                           by=list( dayofweek=dfjunc1$wday),
                           mean)
weekbar <- ggplot(weekinterval, aes( x=dayofweek, y=vehicles)) +
            geom_bar(stat="identity") +
            coord_cartesian( ylim=c(0,70))

#----------------------------------------------------------
# look at day of month average
monthbox <- boxplot( dfjunc1$Vehicles ~ dfjunc1$day,
                     col="lightgreen", outcol="red",
                     main="Junction 1 - Day of month",
                     xlab="day of month",
                     ylab="vehicles")

dayinterval <- aggregate( x=list( vehicles=dfjunc1$Vehicles),
                          by=list( dayofmon=dfjunc1$day),
                          mean)
monthbar <- ggplot(dayinterval, aes( x=dayofmon, y=vehicles)) +
                geom_bar(stat="identity", fill="green") +
                coord_cartesian( ylim=c(40,50))

#----------------------------------------------------------
# look at monthly 
yearbox <- boxplot( dfjunc1$Vehicles ~ dfjunc1$month.lbl,
                    col="lightskyblue", outcol="red",
                    main="Junction 1 - Month of year",
                    xlab="Month",
                    ylab="vehicles")

# look at sum of cars per day 
carsperday <- tapply( dfjunc1$Vehicles, date(dfjunc1$DateTime), sum)
plot(carsperday, type="l")

cpd <- aggregate( x=list( vehicles=dfjunc1$Vehicles), 
                  by=list( date=dfjunc1$date),
                  sum)
ggplot(cpd[1:100,], aes( x=date, y=vehicles)) +
    geom_bar(stat="identity", fill="green") +
    coord_cartesian( ylim=c(280,2200)) +
    scale_x_date(name="dates", date_breaks ="2 month",
                 date_labels="%b")


#---------------------------------------------------------
# created post hackathon - for github pages
dfplot <- dfjunc1
dfplot[,"weekyear"] <- paste( dfplot$week, dfplot$year)
weekly <- lapply( levels( as.factor(dfplot$weekyear)), function(k){
                        dfplot[ which(dfplot$weekyear==k), ]} )

pal <- colorRampPalette( c("black", "green"))
cols <- pal(10)
# c( "darkblue", "darkgreen", "purple", "lightgreen", "lightblue")
plot(weekly[[1]]$DateTime, weekly[[1]]$Vehicles, type='l', col=cols[10],
     ylim=c(0,160), lwd=3, xaxt='n', ylab="Vehicles",xlab="Day of Week",
     main="Vehicles over Day of week")
mtext(side=3, line=0.5, text="Changes over week - light to dark")

lines(weekly[[1]]$DateTime, weekly[[5]]$Vehicles, col=cols[8], type='l', lwd=3)
lines(weekly[[1]]$DateTime, weekly[[40]]$Vehicles, col=cols[7], type='l', lwd=3)
lines(weekly[[1]]$DateTime, weekly[[22]]$Vehicles, col=cols[5], type='l', lwd=3)
lines(weekly[[1]]$DateTime, weekly[[32]]$Vehicles, col=cols[1], type='l', lwd=3)
axis(side=1, at=weekly[[1]][which(weekly[[1]]$hour == 0),"DateTime"],
     labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))



#----------------------------------------------------------
# post event - github pages
#----------------------------------------------------------
png(filename = "./pictures/2017-11-20-boxplots.png",
    width = 960, height = 720, res=96)
par( mar=c(5.1, 5.1, 4.1, 1.1))


layout( matrix( c(1,2,3,4),2,2, byrow=T))

boxplot( dfjunc1$Vehicles ~ dfjunc1$hour,
         col="lightsalmon", outcol="red",
         main="Junction 1 - Hour of day",
         xlab="hour of day",
         ylab="vehicles")
boxplot( dfjunc1$Vehicles ~ dfjunc1$wday.lbl,
         col="powderblue",  outcol="red",
         main="Junction 1 - Day of week",
         xlab="day of week",
         ylab="vehicles")
boxplot( dfjunc1$Vehicles ~ dfjunc1$day,
         col="lightgreen", outcol="red",
         main="Junction 1 - Day of month",
         xlab="day of month",
         ylab="vehicles")
boxplot( dfjunc1$Vehicles ~ dfjunc1$month.lbl,
         col="lightskyblue", outcol="red",
         main="Junction 1 - Month of year",
         xlab="Month",
         ylab="vehicles")
par( mfrow=c(1,1))


dev.off()



###########################################################
# look at hour of day and day of week
cond_hour <- dfjunc1$hour == 17
cond_day <- dfjunc1$wday == 2
cond <- cond_day & cond_hour

dfstep <- dfjunc1[ which( cond),]

cond1_hour <- dftestjunc1$hour == 17
cond1_day <- dftestjunc1$wday == 2
cond1 <- cond1_day & cond1_hour

dfteststep <- dftestjunc1[ which( cond1), ]
        

###########################################################





plot( dfstep$DateTime, dfstep$Vehicles, type='c',
      main="Vehicles over time", 
      ylab="vehicles",
      xlab="time", xaxt='n')
points(dfstep$DateTime, dfstep$Vehicle, col="red")
mtext( text="5pm of every Monday", side=3, line=0.5)
axis.POSIXct(side=1, 
             at=seq( min(dfstep$DateTime), max(dfstep$DateTime),
                    by="1 month"),
             format="%e %b'%y")




###########################################################
# prepare to model
###########################################################

# exclude factors
excls <- c( grep(".lbl", names(dfjunc1), value=T), 
            grep(".xts", names(dfjunc1), value=T),
            grep(".iso", names(dfjunc1), value=T),
            "ID", "index.num", "diff", "hour12", "date",
            "Junction", "minute", "second", "mday")

dfstep1 <- dfstep[, which( !( names(dfstep) %in% excls))]
dfstep1[1:5,]

dfteststep1 <- dfteststep[, which( !(names(dfteststep) %in% excls))]
dfteststep1[1:5,]

#---------------------------------------------------------
# post event
# look at correlation
res <- cor(dfstep1[,2:ncol(dfstep1)],dfstep1$Vehicles )
res[ which( abs(res) > 0.3),]


###########################################################
# initial fit
fit1 <- lm(Vehicles~. , data=dfstep1)

# predict
dfteststep1[,"Vehicles"] <- as.integer(predict(fit1, dfteststep1))

# combine train and test
dfcomb1 <- do.call( "rbind", list( dfstep1, dfteststep1))

###########################################################

png(filename = "./pictures/2017-11-20-actualvsfittedvalues.png",
    width = 960, height = 720, res=96)
par( mar=c(5.1, 5.1, 4.1, 1.1))

# plot fit
plot( dfstep1$DateTime, dfstep1$Vehicles, type='p',
      main="Vehicles over time - actual vs fitted values", 
      ylab="vehicles",
      xlab="time", xaxt='n')
lines(dfstep1$DateTime, fit1$fitted.values, col="red")
mtext( text="5pm of every Monday", side=3, line=0.5)
axis.POSIXct(side=1, 
             at=seq( min(dfstep1$DateTime), max(dfstep1$DateTime),
                     by="1 month"),
             format="%e %b'%y")
dev.off()


#---------------------------------------------------------
#compare residuals
png(filename = "./pictures/2017-11-20-residuals1.png",
    width = 960, height = 720, res=96)
par( mar=c(5.1, 5.1, 4.1, 1.1))

layout( matrix( c(1,2,3,2),2,2, byrow=T))

plot( fit1$residuals, col="red", ylab="residuals",
      main="Residual of fit")
abline(h=0)

boxplot(fit1$residuals, col="skyblue", outcol="red",
        main="Boxplot of residuals",
        ylab="residuals")
abline(h=0, col="green")

qqnorm(fit1$residuals)
qqline(fit1$residuals)

dev.off()
par( mfrow=c(1,1))




###########################################################
# filter outliers
qqval <- quantile(fit1$residuals)
qrange <- 1.5*IQR(fit1$residuals)
cond1 <-  fit1$residuals <= qqval[2]-qrange
cond2 <-  fit1$residuals >= qqval[4]+qrange
cond_out <- cond1 | cond2

dfstep2 <- dfstep1[ which( !cond_out),]
dfteststep2 <- dfteststep1

###########################################################
# fit again
fit2 <- lm(Vehicles~. , data=dfstep2)

# predict
dfteststep2[,"Vehicles"] <- as.integer(predict(fit2, dfteststep2))

# combine train and test
dfcomb2 <- do.call( "rbind", list( dfstep2, dfteststep2))

###########################################################

png(filename = "./pictures/2017-11-20-actualvsfittedvalues2.png",
    width = 960, height = 720, res=96)
par( mar=c(5.1, 5.1, 4.1, 1.1))

par(mfrow=c(1,1))
# plot fit
plot( dfstep2$DateTime, dfstep2$Vehicles, type='p',
      main="Vehicles over time - actual vs fitted values", 
      ylab="vehicles",
      xlab="time", xaxt='n')
lines(dfstep2$DateTime, fit2$fitted.values, col="red")
mtext( text="5pm of every Monday", side=3, line=0.5)
axis.POSIXct(side=1, 
             at=seq( min(dfstep2$DateTime), max(dfstep2$DateTime),
                     by="1 month"),
             format="%e %b'%y")
dev.off()


#compare residuals
png(filename = "./pictures/2017-11-20-residuals2.png",
    width = 960, height = 720, res=96)
par( mar=c(5.1, 5.1, 4.1, 1.1))

layout( matrix( c(1,2,3,2),2,2, byrow=T))

plot( fit2$residuals, col="red", ylab="residuals",
      main="Residual of fit")
abline(h=0)

boxplot(fit2$residuals, col="skyblue", outcol="red",
        main="Boxplot of residuals",
        ylab="residuals")
abline(h=0, col="green")

qqnorm(fit2$residuals)
qqline(fit2$residuals)

dev.off()
par( mfrow=c(1,1))


###########################################################
# compare both fit
###########################################################
png(filename = "./pictures/2017-11-20-fitcompare.png",
    width = 960, height = 720, res=96)

par( mar=c(5.1, 5.1, 4.1, 1.1))
layout( matrix( c(1,2),2,1, byrow=T))


# plot fit - with outliers
plot( dfcomb1$DateTime, dfcomb1$Vehicles, type='p',
      main="Vehicles over time - with outliers", 
      ylab="vehicles",
      xlab="time", xaxt='n')
lines(dfstep1$DateTime, fit1$fitted.values, col="blue")
lines(dfteststep1$DateTime, dfteststep1$Vehicles, col="red")
mtext( text="5pm of every Monday", side=3, line=0.5)
axis.POSIXct(side=1, 
             at=seq( min(dfcomb1$DateTime), max(dfcomb1$DateTime),
                     by="1 month"),
             format="%e %b'%y")
legend( "bottomright", c("fitted", "predicted"), col=c("blue","red"), lty=1)


# plot fit - without outliers
plot( dfcomb2$DateTime, dfcomb2$Vehicles, type='p',
      main="Vehicles over time - without outliers", 
      ylab="vehicles",
      xlab="time", xaxt='n')
lines(dfstep2$DateTime, fit2$fitted.values, col="blue")
lines(dfteststep2$DateTime, dfteststep2$Vehicles, col="red")
mtext( text="5pm of every Monday", side=3, line=0.5)
axis.POSIXct(side=1, 
             at=seq( min(dfcomb2$DateTime), max(dfcomb2$DateTime),
                     by="1 month"),
             format="%e %b'%y")
legend( "bottomright", c("fitted", "predicted"), col=c("blue","red"), lty=1)

dev.off()
par( mfrow=c(1,1))






#---------------------------------------------------------    
#---------------------------------------------------------    
par( mar=defmargin)
#---------------------------------------------------------    
#---------------------------------------------------------    





