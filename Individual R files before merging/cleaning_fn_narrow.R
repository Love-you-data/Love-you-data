 setwd("F:/desktop/Online learning course/Coursera_Google_Analytics/Bellabeat/bellabeat1")
 library(gridExtra)
 library(viridis)
 library(factoextra)
 library(mclust)
 library(ggpubr)
 library(ggplot2)
 library(dplyr)
 library(stringr)
 library(ggplot2)
 library(tidyverse)
 library(lubridate)
 library(skimr)
 library(SimDesign)
 library(chron)
 library(ggcorrplot)
  # library(summarytools)
 # library(explore)
 # library(dataMaid)
 library(sqldf)
library(hms)
 
daily_activity <- read.csv("dailyActivity_merged.csv",header = TRUE)
heartrate_seconds <- read.csv("heartrate_seconds_merged.csv",header = TRUE)
hourly_calories <- read.csv("hourlyCalories_merged.csv",header=TRUE)
hourly_intensities <- read.csv("hourlyIntensities_merged.csv",header = TRUE)
hourly_steps <- read.csv("hourlySteps_merged.csv",header=TRUE)
minuteMET_narrow <- read.csv("minuteMETsNarrow_merged.csv",header=TRUE)
minuteSleep <- read.csv("minuteSleep_merged.csv",header=TRUE)
day_sleep <- read.csv("sleepDay_merged.csv",header=TRUE)
weight_log_info <- read.csv("weightLogInfo_merged.csv",header=TRUE)


print(dfSummary(daily_activity))
explore(daily_activity)
makeDataReport(da_sleep,render=FALSE)
length(unique(weight_log_info[,1]))
str(daily_activity)
glimpse(daily_activity)
summary.data.frame(daily_activity)
# hms(minuteSleep$date)

clean_narrow_data <- function(file,st){
    
    
    if( st == "daily_activity"){
        file[,2] <- mdy(file[,2])
        file[4:10] <- round(file[4:10],2)
        # file$date <- as.Date(file[,2])
        # file$time <- format(as.POSIXct(file[,2]),"%H:%M:%S")
        file$day <- weekdays(file[,2])
    file$day <- factor(file$day, levels= c("Sunday", "Monday", 
                                                 "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        file$week <- week(file[,2])-min(week(file[,2])) + 1
    }
    else if(st!=day_sleep && st!="day_sleep_cp" && st!="mcis"){
        file[,2] <- mdy_hms(file[,2])
        file$day <- weekdays(file[,2])
        file$day <- factor(file$day, levels= c("Sunday", "Monday", 
                                               "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        file$week <- week(file[,2])-min(week(file[,2])) + 1
        # if(st!="day_sleep"){
        # file$time <- hms(file[,2])
        # }
    }
    else{
        file[,2] <- ymd(file[,2])
        file$day <- weekdays(file[,2])
        file$day <- factor(file$day, levels= c("Sunday", "Monday", 
                                               "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        file$week <- week(file[,2])-min(week(file[,2])) + 1
    }
    paste("checking for NAs")
    # which(is.na(file))
    
    if(st == "weight_log_info"){
        file <- file %>%   select(-Fat)
    }
    #Retaining the distinct file
    file <- file %>% distinct(file[,])
    
    #rounding off to two decial places 
    if(st=="hourly_intensities"){
    file[,4] <- round(file[,4],2)
    }
    if(st=="weight_log_info"){
    file[,c(3:5)] <- round(file[,3:5],2)
    }
    # if(st=="heartrate_seconds" ||
    #    st=="minuteMET_narrow" ){
    #     file$Date <- as.Date(file[,2]) # already got this one from the answers above
    #     file$minute <-format(as.POSIXct(file[,2]), format = "%H:%M:%OS") 
    #     file$minute <- as.POSIXct(file$minute,format="%H:%M:%S")
    # }
    if( st != "daily_activity" && st != "day_sleep_cp" && st!="day_sleep") {
        file$Date <- as.Date(file[,2])
        file$time <- format(as.POSIXct(file[,2]), format = "%H:%M:%S") 
        file$time <- as.POSIXct(file$time,format="%H:%M:%S")
    }
    #Removing the redundant column time/activity date/hour etc(column 2)
    if( st !=  "daily_activity" && st != "day_sleep_cp"&& st!="day_sleep"){
    file <- file %>% select(-2)
    }
    return(file)
}

daily_activity <- clean_narrow_data(daily_activity,deparse(substitute(daily_activity)))
day_sleep <- clean_narrow_data(day_sleep,deparse(substitute(day_sleep)))
heartrate_seconds <- clean_narrow_data(heartrate_seconds,deparse(substitute(heartrate_seconds)))
hourly_intensities <- clean_narrow_data(hourly_intensities,deparse(substitute(hourly_intensities)))
hourly_calories <- clean_narrow_data(hourly_calories,deparse(substitute(hourly_calories)))
hourly_steps <- clean_narrow_data(hourly_steps,deparse(substitute(hourly_stepss)))
minuteMET_narrow<- clean_narrow_data(minuteMET_narrow,deparse(substitute(minuteMET_narrow)))
minuteSleep <- clean_narrow_data(minuteSleep,deparse(substitute(minuteSleep)))
weight_log_info <- clean_narrow_data(weight_log_info,deparse(substitute(weight_log_info)))

hourly_cal_int_step <- Reduce(function(x,y) inner_join(x=x,y=y,by=c("Id","Date","time","day","week"),all=T),
                                                  list(hourly_calories,
                                                       hourly_intensities,
                                                       hourly_steps))

head(which(is.na(hourly_cal_int_step), arr.ind=TRUE))
colnames(hourly_cal_int_step)[5] <- "ActivityDate"
rm("hourly_calories","hourly_intensities","hourly_steps")
min(hourly_cal_int_step$Date)
max(hourly_cal_int_step$Date)
min(daily_activity$ActivityDate)
max(daily_activity$ActivityDate)
# rm(hourly_steps)
# my_list <- list(daily_activity=daily_activity,
#                 day_sleep=day_sleep,
#                 heartrate_seconds=heartrate_seconds,
#                 hourly_calories=hourly_calories,
#                 hourly_intensities=hourly_intensities,
#                 hourly_steps=hourly_steps,
#                 minuteMET_narrow=minuteMET_narrow,
#                 minuteSleep=minuteSleep,
#                 weight_log_info=weight_log_info)


#setwd("F:/desktop/Online learning course/Coursera_Google_Analytics/Bellabeat/bellabeat1/new")
#mapply(write.csv, my_list, file=paste0(names(my_list), '.csv'),MoreArgs=list(row.names=FALSE, sep=","))




# install.packages("RMySQL")
 # install.packages("RMariaDB")
library(DBI)
# 
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname="google",
                 host="localhost",
                 port=3306,
                 user="root",

                 password="PriyaDar@123")

# library(RMySQL)
# con <- dbConnect(MySQL(),
#                  dbname="google_courseera",
#                  host="localhost",
#                  port=3306,
#                  user="root",
#                  password="PriyaDar@123")

dbWriteTable(con,name = "daily_activity",value=daily_activity, overwrite = T)
dbWriteTable(con,name = "day_sleep",value=day_sleep, overwrite = T)
dbWriteTable(con,name="heartrate_seconds",value=heartrate_seconds,overwrite=T)
dbWriteTable(con,name="hourly_calories",value=hourly_calories,overwrite=T)
dbWriteTable(con,name="hourly_intesities",value=hourly_intensities,overwrite=T)
dbWriteTable(con,name="hourly_steps",value=hourly_steps,overwrite=T)
dbWriteTable(con,name="minuteMET_narrow",value=minuteMET_narrow,overwrite=T)
dbWriteTable(con,name="minuteSleep",value=minuteSleep,overwrite=T)
dbWriteTable(con,name="weight_log_info",value=weight_log_info,overwrite=T)


# dbWriteTable(con,name = "ab",value=ab, overwrite = T)
# dbWriteTable(con,name = "minuteCalories_narrow",value=minuteCalories_narrow, overwrite = T)
#                         
# dbWriteTable(con,name = "minutecalories_wide",value=minuteCalories_wide, overwrite = T)
# dbWriteTable(con,name = "minuteIntensities_narrow",value=minuteIntensities_narrow, overwrite = T)
# dbWriteTable(con,name = "minuteIntensities_wide",value=minuteIntensities_wide, overwrite = T)
# dbWriteTable(con,name = "minuteSteps_narrow",value=minuteSteps_narrow, overwrite = T)
# dbWriteTable(con,name = "minuteSteps_wide",value=minuteSteps_wide, overwrite = T)
