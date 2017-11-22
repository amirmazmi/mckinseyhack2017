#---------------------------------------------------------
# Analytics Vidhya and McKinsey Analytics 
#  Hackathon
#   
#   18 Nov 2017
# 
# 
#   Pipeline.R - prediction pipeline
# 
# 
#   rm(list=ls())
#---------------------------------------------------------
# Load libraries
#---------------------------------------------------------
# library(pacman)
# p_load(dplyr)
# p_load(timetk)
# p_load(lubridate)
# 

#---------------------------------------------------------
pathhome <- 'C:/Users/Speedy/Desktop/mckinsey'
setwd(pathhome)
getwd()

relib <- function(){
    source("./functions.R")
    return(TRUE)
}
relib()



trainfile <- "./train_aWnotuB.csv"  
testfile <- "./test_BdBKkAj.csv"    



#---------------------------------------------------------    

# read in data
dftrainraw <- read.csv( trainfile )
dftestraw <- read.csv( testfile )


# prepare data
# 1. make junctions into factors for splitting
# 2. format datetime and extract information
# 3. split into list of junctions
dftrain <- dftrainraw %>% makeFactor()  %>% 
                            dtaugment() %>%
                            exchols()%>% 
                            splitjunc()

dftest <- dftestraw %>% makeFactor()    %>%
                        dtaugment()     %>% 
                        exchols()       %>%
                        splitjunc()



#---------------------------------------------------------    
# fit and predict

junclist <- seq_along(dftrain)
wdaylist <- seq_along(unique(dftrain$junc1[,"wday"]))
hourlist <- seq(0,max(dftrain$junc1[,"hour"]))   #list starts from 0

# debugging values
# junc <- 5
# day <- 4
# hourlist <- 7

juncoutput=list()

# iterate over list of junctions
for( junc in junclist){
    
    train <- dftrain[[junc]]
    test <- dftest[[junc]]
    
    dayoutput=list()
    
    # iterate over each day of week
    for(day in wdaylist ){
        
        result = list()
        
        # iterate over each hour of day
        for(hour in hourlist ){
            
            # filter training data based on hour and day
            cond_hour <- train[,"hour"] == hour
            cond_day <- train[,"wday"] == day
            condtrain <- cond_day & cond_hour
            
            chunktrain <- train[ which(condtrain),] %>% rmcolm()
            
            # filter test data based on hour and data
            cond_hour <- test[,"hour"] == hour
            cond_day <- test[,"wday"] == day
            condtest <- cond_day & cond_hour
            
            chunktest <- test[ which(condtest),] %>% rmcolm()
            
            if( dim(chunktrain)[1] == 0 | dim(chunktest)[1] == 0){
                cat( "[+] junc:", junc, "  day:", day, "  hour:", hour, "\t SKIPPED\n")
            #     cat( "\t\t\t\t", dim(chunktrain), "\n")
            #     cat( "\t\t\t\t", dim(chunktest), "\n")
                next
            }
            
            
            # fit and predict
            fit <- lm(Vehicles~. , data=chunktrain)
            # fit <- ranger( Vehicles~., data=chunktrain, 
            #                num.trees=200, 
            #                splitrule="extratrees",
            #                importance="impurity"  )
            
            chunktrain[,"residuals"] <- fit$residuals
            
            # remove outliers
            qqval <- quantile(chunktrain$residuals)
            qrange <- 1.5*IQR(chunktrain$residuals)
            cond1 <- chunktrain$residuals <= qqval[2]-qrange
            cond2 <- chunktrain$residuals >= qqval[4]+qrange
            cond_out <- cond1 | cond2
            if( length( which( cond_out)) > 0) {
                
                chunktrain <- chunktrain[ which( !cond1 & !cond2),] %>% select( -residuals)
            
                fit <- lm(Vehicles~. , data=chunktrain)
            }
                
            chunktest[,"Vehicles"] <- as.integer(predict( fit, chunktest))
            # chunktest[,"Vehicles"] <- as.integer(predict( fit, chunktest)$predictions)
            
            
            # compile output
            output1 <- chunktest[, c( "year", "month", "day","hour")]
            output2 <- as.data.frame( list( Vehicles=chunktest[, "Vehicles"]) )
            dummy <- as.data.frame(list( Junction=rep( junc, dim(output1)[1] )))
            result[[hour+1]] <- cbind( output1, dummy, output2)
            
            
        }
        
        dayoutput[[day]] <- do.call( "rbind", result)
        
        
    }
    
    juncoutput[[junc]] <- do.call( "rbind", dayoutput)
    
}

# merge result
final <- do.call( "rbind", juncoutput)
# create ID column
final[,"ID"] <- paste( final$year, 
                       formatC( final$month, width=2, flag=0),
                       formatC( final$day, width=2, flag=0),
                       formatC( final$hour, width=2, flag=0),
                       final$Junction,
                       sep="")

# fail-safe to prevent negatives
final[which( final$Vehicles <0),"Vehicles"] <- 0

# any duplicates?
dupl <- dim(final[ which(duplicated(final$ID)),])[1] != 0
cat("\n\n[+] Any duplicates?   ->", dupl,"\n")

# predicted rows are the same as test df
eqrows <- nrow(dftestraw) == nrow(final)
cat("\n[+] Equal rows?         ->", eqrows, "\n\n")


#---------------------------------------------------------    
# write out 
outputfinal <- final[, c("ID", "Vehicles")]

fileout <- paste("./result/result H", strftime( now(),"%H"),
                 "M", strftime( now(),"%M"),
                 "S", strftime( now(),"%S"),
                 ".csv", sep="")

write.csv( outputfinal, fileout, quote=F, row.names=F)





###########################################################
# junc1 final result - for github pages
###########################################################

# wrangle

# train data
junc1train <- dftrain[[1]]
# predicted output 
junc1 <- juncoutput[[1]]

junc1$DateTime <-  ymd_hms( paste( paste( junc1$year, junc1$month, junc1$day, sep="-"),
                            paste(junc1$hour, ":00:00", sep=""),
                            sep=" ") )
junc2 <- junc1[order(junc1$DateTime),]


# combine
junc1out <- rbind( junc1train[,c("DateTime", "Vehicles")],
                   junc2[,c("DateTime", "Vehicles")])

#---------------------------------------------------------    
# plot

png(filename = "./pictures/2017-11-20-results.png",
    width = 960, height = 720, res=96)

par( mar=c(5.1, 5.1, 4.1, 1.1))
layout( matrix( c(1,2),2,1, byrow=T))

# plot whole series
plot( junc1out$DateTime, junc1out$Vehicles, type="l",
      main="Predictions - Vehicles over time", 
      ylab="vehicles",
      xlab="time", xaxt='n')
lines( junc1train$DateTime, junc1train$Vehicles, type="l", col="blue")
lines( junc2$DateTime, junc2$Vehicles, type="l", col="red")
axis.POSIXct(side=1, 
             at=seq( min(junc1out$DateTime), max(junc1out$DateTime),
                     by="1 month"),
             format="%e %b'%y")
legend( "topleft", c("training", "predicted"), col=c("blue","red"), 
        lty=1, cex=0.8, pt.cex=5)

# plot zoomed in 
start <- 12500
end <- nrow(junc1out)

plot( junc1out$DateTime[start:end], junc1out$Vehicles[start:end], type="l",
      main="Predictions - Vehicles over time", 
      ylab="vehicles",
      xlab="time", xaxt='n')
lines( junc1train$DateTime[start:14448], junc1train$Vehicles[start:14448],
       type="l", col="blue")
lines( junc2$DateTime, junc2$Vehicles, type="l", col="red")
axis.POSIXct(side=1, 
             at=seq( min(junc1out$DateTime[start:end]),
                     max(junc1out$DateTime[start:end]),
                     by="1 month"),
             format="%e %b'%y")
mtext( text="zoomed in", side=3, line=0.5)
legend( "topleft", c("training", "predicted"), col=c("blue","red"),
        lty=1, cex=0.8, pt.cex=5)

dev.off()





par( mfrow=c(1,1))









