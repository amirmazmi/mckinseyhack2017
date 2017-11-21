#---------------------------------------------------------
# Analytics Vidhya and McKinsey Analytics 
#  Hackathon
#   
#   18 Nov 2017
# 
# 
#   functions.R - Exploratory Data Analysis
# 
# 
#---------------------------------------------------------
# Load libraries
#---------------------------------------------------------
library(pacman)
p_load(dplyr)
p_load(ggplot2)
p_load(timetk)
p_load(lubridate)



#---------------------------------------------------------
# Functions
#---------------------------------------------------------
# change junctions to factor for splitting
makeFactor <- function(df){
    df[,"Junction"] <- as.factor(df[,"Junction"])
    return(df)
}


# extract information from datetime
dtaugment <- function(df){
    df[,"DateTime"] <- ymd_hms(df[,"DateTime"])
    df <- tk_augment_timeseries_signature(df)
    df[,"date"] <- as.character(date(df[,"DateTime"]))
    return(df)
}


# convert data into list based on junction
splitjunc <- function(df){
    df <- lapply( levels( df[,"Junction"]), function(k){
        df[ which( df[,"Junction"] == k),] })
    df <- setNames(df, paste("junc", seq(1,4), sep="") )
    return(df)
}


# remove unnecessary columns
rmcolm <- function(df){
    xcolm <- c( grep(".lbl", names(df), value=T), 
                grep(".xts", names(df), value=T),
                grep(".iso", names(df), value=T),
                "ID", "index.num", "diff", "hour12", "date",
                "Junction", "minute", "second", "mday")
    df <- df[, which( !(names(df) %in% xcolm))]
    return(df)
}



# filter out expected holidays
exchols <- function(df){
    holiday <- c( "2015-12-25",
                  "2015-12-26",
                  "2016-01-01",
                  "2016-12-25",
                  "2016-12-26",
                  "2017-01-01")
    
    dfout <- df[ which( !(df[,"date"] %in% holiday)),]
    return(dfout)
}








