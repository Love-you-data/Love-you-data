setwd("F:/desktop/Online learning course/Coursera_Google_Analytics/Bellabeat/bellabeat1")

library(gridExtra)
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

colnames(minuteSleep)[6]<- "ActivityDate"
# colnames(day_sleep)[7]<- "ActivityDate"
# day_sleep_cp <- day_sleep
minuteSleep <- minuteSleep %>% subset(ActivityDate >= "2016-04-12" &
                                          ActivityDate <="2016-05-12")
# 
# min(day_sleep$Date)
# max(day_sleep$Date)
# min(daily_activity$ActivityDate)
# max(daily_activity$ActivityDate)
# min(day_sleep_cp$ActivityDate)
# max(day_sleep_cp$ActivityDate)
# min(minuteSleep$ActivityDate)
# max(minuteSleep$ActivityDate)
# 

day_sleep_cp <-
    merge(
        
        minuteSleep %>%
            group_by(Id,ActivityDate) %>%
            mutate(TotalTimeInBed=as.numeric(difftime(time,lag(time),units="mins"))) %>%
            replace(is.na(.),0) %>%
            mutate(TotalTimeInBed=ifelse(TotalTimeInBed>1,0,1)) %>%
            summarise(TotalTimeInBed=sum(TotalTimeInBed)),
        
        minuteSleep %>%
            group_by(Id,ActivityDate) %>%
            mutate(TotalSleepRecords=n_distinct(logId)) %>%
            replace(is.na(.),0) %>%
            summarise(TotalSleepRecords=max(TotalSleepRecords)),

        
        by = c("Id","ActivityDate"))
anti_join(day_sleep[,],day_sleep_cp[,])
day_sleep_cp <- left_join(day_sleep_cp,
                         minuteSleep %>%
                          group_by(Id,ActivityDate) %>%
                          filter(value==1) %>%
                          mutate(TotalMinutesAsleep=n()) %>%
                          replace(is.na(.),0) %>%
                          summarise(TotalMinutesAsleep=mean(TotalMinutesAsleep))) 
   
day_sleep_cp <- clean_narrow_data(day_sleep_cp,deparse(substitute(day_sleep_cp)))
day_sleep_cp[,5] <- day_sleep_cp[,5] %>% replace_na(0)

 t1 <- day_sleep[,c(1,5:7)] %>% group_by(Id,ActivityDate) %>% summarise(n())
 t2 <- day_sleep_cp[,c(1,2,6,7)] %>%  group_by(Id,ActivityDate) %>% summarise(n())
 anti_join(t1,t2)
 rm(t1,t2)
 # ,day_sleep,minuteSleep)
#############################################################################
# install.packages("summarytools")
# install.packages("dataMaid")

print(dfSummary(minuteSleep),file="abc.html")
makeDataReport(da_sleep,render=FALSE)
explore(da_sleep)

da_sleep <- inner_join(daily_activity,day_sleep_cp)
head(which(is.na(da_sleep), arr.ind=TRUE))
# 1844505072

quantile(ceiling(day_sleep_cp$TotalMinutesAsleep/60))
da_sleep <- da_sleep %>%
    mutate(scluster =case_when(
        TotalMinutesAsleep< quantile(ceiling(TotalMinutesAsleep),0.25) ~"Short Sleeper",
        TotalMinutesAsleep>=quantile(ceiling(TotalMinutesAsleep),0.25)&
            TotalMinutesAsleep<=quantile(ceiling(TotalMinutesAsleep),0.75) ~"Perfect Sleepers",    
        TotalMinutesAsleep>quantile(ceiling(TotalMinutesAsleep),0.75) ~"Long Sleepers"
    ),awake_min = TotalTimeInBed - TotalMinutesAsleep)

quantile(da_sleep$TotalMinutesAsleep/60)
# LEFTOUT
da_sleep %>% ggplot(aes(x=TotalTimeInBed/60,y=round(TotalMinutesAsleep/60,2)))+
    geom_jitter()+
    facet_grid(~TotalSleepRecords)+
    scale_y_continuous(breaks = seq(0, 13, len = 13))+
    scale_x_continuous(breaks = seq(0, 16, by = 1))
x11()

da_sleep[which(is.na(da_sleep)),]
da_sleep %>%  ggplot(aes(x=TotalTimeInBed/60,y=round(TotalMinutesAsleep/60,2)))+
    geom_jitter()+
    facet_grid(~mcluster)+
    scale_y_continuous(breaks = seq(0, 13, len = 13))+
    scale_x_continuous(breaks = seq(0, 16, by = 1))+
    geom_hline(data = da_sleep %>%
                   group_by(mcluster) %>%
                   summarise(MN=mean(TotalMinutesAsleep/60)),
               aes(yintercept = MN), col = "purple", linetype = 2,show.legend = TRUE)+
    geom_text(data = da_sleep %>%
                  group_by(mcluster) %>%
                  summarise(MN=mean(TotalMinutesAsleep/60)),
              aes(0,MN,label=round(MN,2),vjust=0,hjust=0))+
    geom_hline(data = da_sleep ,
               aes(yintercept = mean(TotalMinutesAsleep/60)),
               col = "red", linetype = 1,show.legend = TRUE)+
    
    geom_text(data = da_sleep %>%
                  summarise(MN=mean(TotalMinutesAsleep/60)),
              aes(0,MN,label=round(MN,2),vjust=0,hjust=-1))

da_sleep %>%
    ggplot(aes(x=(TotalTimeInBed/60),y=round((TotalMinutesAsleep/60),2)))+
    geom_jitter()+
    facet_grid(day~mcluster)+
    scale_y_continuous(breaks = seq(0, 13, len = 8))+
    geom_hline(data = (da_sleep %>%
                           group_by(mcluster) %>%
                           summarise(MN=mean(TotalMinutesAsleep/60))),
               aes(yintercept = MN), col = "red", linetype = 1,show.legend = TRUE)+
    geom_vline(data = da_sleep %>%
                   group_by(mcluster) %>%
                   summarise(MN=mean(TotalTimeInBed/60)),
               aes(xintercept = MN), col = "blue", linetype = 1,show.legend = TRUE)

# da_sleep  %>% select(19:22) %>% summary()
# da_sleep %>% filter(scluster==1) %>% select(19:22) %>% summary()
# da_sleep %>% filter(scluster==2) %>% select(19:22) %>% summary()
# da_sleep %>% filter(scluster==3) %>% select(19:22) %>% summary()

# library(plotly)

x11()
ggplotly(
    da_sleep %>% 
        ggplot(aes(x=TotalTimeInBed/60,y=TotalMinutesAsleep/60)) +
        # ggplot(aes(y=mean(TotalTimeInBed/60-TotalMinutesAsleep/60),x=scluster,fill=scluster))+
        geom_jitter()+
        facet_grid(mcluster~scluster)+
        geom_hline(data = da_sleep %>%
                       group_by(scluster,mcluster) %>% 
                       summarise(MN=mean(TotalMinutesAsleep/60)),
                   aes(yintercept = MN), col = "red", linetype = 1,show.legend = TRUE)+
        geom_vline(data = da_sleep %>%
                       group_by(scluster,mcluster) %>% 
                       summarise(MN=mean(TotalTimeInBed/60)),
                   aes(xintercept = MN), col = "blue", linetype = 1,show.legend = TRUE)+
        geom_vline(data = da_sleep %>%
                       summarise(MN=mean(TotalTimeInBed/60)),
                   aes(xintercept = MN), col = "black", linetype = 2,show.legend = TRUE)+
        geom_hline(data = da_sleep %>%
                       summarise(MN=mean(TotalMinutesAsleep/60)),
                   aes(yintercept = MN), col = "black", linetype = 2,show.legend = TRUE)+
        
        scale_y_continuous(breaks = seq(0, 13, len = 13))+
        scale_x_continuous(breaks = seq(0, 16, len = 6))
    
)

x11()
library(ggalt) 
# install.packages("ggalt")
library(dplyr)
da_sleep  %>% 
    group_by(mcluster,st,scluster) %>% distinct() %>% 
    summarise(total_calories= mean(Calories)) %>% 
    mutate(total_calories=round(total_calories/1000,2)) %>% 
    ggplot(aes(x=st,y=total_calories))+#,fill=factor(scluster)) +
    geom_lollipop(horizontal =FALSE,
                  point.colour = "blue"
                  )+
    # geom_col(aes(fill=factor(scluster)))+
    facet_grid(scluster~factor(mcluster))+
    geom_text(aes(label =(total_calories)), vjust = -0.2, size = 3,
     position = position_dodge(0.9))+
    theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5))+
    labs(y="Avg CAlories Burnt(000)")



da_sleep$sleep_efficiency <- da_sleep$TotalMinutesAsleep / da_sleep$TotalTimeInBed
#######################################
ggarrange(
    da_sleep  %>%
        filter(mcluster=="Very Active") %>% 
        group_by(scluster,st) %>% 
        summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
        ggplot(aes(x=reorder(st,total_efficiency*100),y=total_efficiency*100)) +
        geom_col(aes(fill=st))+
        facet_grid(~factor(scluster))+
        geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
        labs(y="Sleep Efficiency")+
        theme(axis.text.x = element_blank())+
        ggtitle("Long Sleepers")+
        coord_polar(theta = "y",start=0)+
        theme(axis.text.y = element_blank()),
    da_sleep  %>%
        filter(scluster=="Short Sleeper") %>% 
        group_by(mcluster,st) %>% 
        summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
        ggplot(aes(x=st,y=total_efficiency*100)) +
        geom_col(aes(fill=st))+
        facet_grid(~factor(mcluster))+
        geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
        labs(y="Sleep Efficiency")+
        theme(axis.text.x = element_blank())+
        ggtitle("Short Sleepers")+
        coord_polar(theta = "y",start=0)+
        theme(axis.text.y = element_blank()),
    da_sleep  %>%
        filter(scluster=="Perfect Sleepers") %>% 
        group_by(mcluster,st) %>% 
        summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
        ggplot(aes(x=st,y=total_efficiency*100)) +
        geom_col(aes(fill=st))+
        facet_grid(~factor(mcluster))+
        geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
        labs(y="Sleep Efficiency")+
        theme(axis.text.x = element_blank())+
        ggtitle("Perfect Sleepers")+
        coord_polar(theta = "y",start=0)+
        theme(axis.text.y = element_blank()),ncol = 1,nrow = 3)

ggarrange(
  da_sleep  %>%
    filter(mcluster=="Very Active") %>% 
    group_by(scluster,st) %>% 
    summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
    ggplot(aes(x=reorder(st,total_efficiency*100),y=total_efficiency*100)) +
    geom_col(aes(fill=st))+
    facet_grid(~factor(scluster))+
    geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
              position = position_dodge(0.9))+
    theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
    labs(y="Sleep Efficiency")+
    theme(axis.text.x = element_blank())+
    ggtitle("Very Active USer")+
    coord_polar(theta = "y",start=0)+
    theme(axis.text.y = element_blank()),
  da_sleep  %>%
    filter(mcluster=="Fairly Active") %>% 
    group_by(scluster,st) %>% 
    summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
    ggplot(aes(x=reorder(st,total_efficiency*100),y=total_efficiency*100)) +
    geom_col(aes(fill=st))+
    facet_grid(~factor(scluster))+
    geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
              position = position_dodge(0.9))+
    theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
    labs(y="Sleep Efficiency")+
    theme(axis.text.x = element_blank())+
    ggtitle("Fairly Active User")+
    coord_polar(theta = "y",start=0)+
    theme(axis.text.y = element_blank()),
  da_sleep  %>%
    filter(mcluster=="Lightly Active") %>% 
    group_by(scluster,st) %>% 
    summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
    ggplot(aes(x=reorder(st,total_efficiency*100),y=total_efficiency*100)) +
    geom_col(aes(fill=st))+
    facet_grid(~factor(scluster))+
    geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
              position = position_dodge(0.9))+
    theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
    labs(y="Sleep Efficiency")+
    theme(axis.text.x = element_blank())+
    ggtitle("Lightly Active USer")+
    coord_polar(theta = "y",start=0)+
    theme(axis.text.y = element_blank()),
  da_sleep  %>%
    filter(mcluster=="Sedentary") %>% 
    group_by(scluster,st) %>% 
    summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
    ggplot(aes(x=reorder(st,total_efficiency*100),y=total_efficiency*100)) +
    geom_col(aes(fill=st))+
    facet_grid(~factor(scluster))+
    geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
              position = position_dodge(0.9))+
    theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
    labs(y="Sleep Efficiency")+
    theme(axis.text.x = element_blank())+
    ggtitle("Sedentary User")+
    coord_polar(theta = "y",start=0)+
    theme(axis.text.y = element_blank()),
  
  ncol = 2,nrow = 2)

#################################################
x11()
ggarrange(
    ggarrange(
        da_sleep  %>%
            filter(scluster=="Long Sleepers") %>% 
            group_by(mcluster,st) %>% 
            summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
            ggplot(aes(x=st,y=total_efficiency*100)) +
            geom_col(aes(fill=st))+
            facet_grid(~factor(mcluster))+
            geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
                      position = position_dodge(0.9))+
            theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
            labs(y="Sleep Efficiency")+
            theme(axis.text.x = element_blank())+
            ggtitle("Long Sleepers")+
            coord_polar(theta = "y",start=0)+
            theme(axis.text.y = element_blank()),
        da_sleep  %>%
            filter(scluster=="Short Sleeper") %>% 
            group_by(mcluster,st) %>% 
            summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
            ggplot(aes(x=st,y=total_efficiency*100)) +
            geom_col(aes(fill=st))+
            facet_grid(~factor(mcluster))+
            geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
                      position = position_dodge(0.9))+
            theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
            labs(y="Sleep Efficiency")+
            theme(axis.text.x = element_blank())+
            ggtitle("Short Sleepers")+
            coord_polar(theta = "y",start=0)+
            theme(axis.text.y = element_blank()),
        da_sleep  %>%
            filter(scluster=="Perfect Sleepers") %>% 
            group_by(mcluster,st) %>% 
            summarise(total_efficiency= round(mean(sleep_efficiency),2)) %>% 
            ggplot(aes(x=st,y=total_efficiency*100)) +
            geom_col(aes(fill=st))+
            facet_grid(~factor(mcluster))+
            geom_text(aes(label =(total_efficiency*100)), vjust = -0.2, size = 3,
                      position = position_dodge(0.9))+
            theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
            labs(y="Sleep Efficiency")+
            theme(axis.text.x = element_blank())+
            ggtitle("Perfect Sleepers")+
            coord_polar(theta = "y",start=0)+
            theme(axis.text.y = element_blank()),ncol = 1,nrow = 3,align = "v"),
    ggarrange(
        
        da_sleep  %>%
            filter(scluster=="Long Sleepers") %>% 
            group_by(mcluster,st) %>% 
            summarise(total_awake= round(mean(TotalTimeInBed - TotalMinutesAsleep),2)) %>% 
            ggplot(aes(x=st,y=total_awake)) +
            geom_col(aes(fill=st))+
            facet_grid(~factor(mcluster))+
            geom_text(aes(label =(total_awake)), vjust = -0.2, size = 3,
                      position = position_dodge(0.9))+
            theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
            labs(y="Total_Awake")+
            theme(axis.text.x = element_blank())+
            ggtitle("Long Sleepers")+
            theme(axis.text.y = element_blank()),
        
        da_sleep  %>%
            filter(scluster=="Short Sleeper") %>% 
            group_by(mcluster,st) %>% 
            summarise(total_awake= round(mean(TotalTimeInBed - TotalMinutesAsleep),2)) %>% 
            ggplot(aes(x=st,y=total_awake)) +
            geom_col(aes(fill=st))+
            facet_grid(~factor(mcluster))+
            geom_text(aes(label =(total_awake)), vjust = -0.2, size = 3,
                      position = position_dodge(0.9))+
            theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
            labs(y="Total_Awake")+
            theme(axis.text.x = element_blank())+
            ggtitle("Short Sleepers")+
            theme(axis.text.y = element_blank()),
        
        da_sleep  %>%
            filter(scluster=="Perfect Sleepers") %>% 
            group_by(mcluster,st) %>% 
            summarise(total_awake= round(mean(TotalTimeInBed - TotalMinutesAsleep),2)) %>% 
            ggplot(aes(x=st,y=total_awake)) +
            geom_col(aes(fill=st))+
            facet_grid(~factor(mcluster))+
            geom_text(aes(label =(total_awake)), vjust = -0.2, size = 3,
                      position = position_dodge(0.9))+
            theme(axis.text.x = element_text(angle =90,vjust=1,hjust=0.5))+
            labs(y="Total_Awake")+
            theme(axis.text.x = element_blank())+
            ggtitle("Perfect Sleepers")+
            theme(axis.text.y = element_blank())
        ,ncol = 1,nrow = 3,align = "v")) 

##################################################################################

# daily_acti_min_sleep <- merge(daily_activity,minuteSleep,all.y=TRUE )


da_mi_sleep <- right_join(da_sleep,minuteSleep) 
head(which(is.na(da_mi_sleep), arr.ind=TRUE))

da_mi_sleep <- da_mi_sleep %>% 
    mutate(status=case_when(
        value==1 ~"Asleep",
        value==2 ~"Restless",
        value==3 ~"Awake"
    )) %>% select(-c("value"))

     
 da_mi_sleep <- left_join(da_mi_sleep, da_mi_sleep %>%
                          group_by(Id,ActivityDate,status) %>% count()
                                         )
colnames(da_mi_sleep)[29] <- "Minutes"  
# da_mi_sleep <- da_mi_sleep %>%  select(-c(27)) %>% distinct()
# da_mi_sleep %>% spread(key = "status",value = "n")                                         
    head(which(is.na(da_mi_sleep), arr.ind=TRUE))


    
    
    
#  t <- da_mi_sleep %>% 
#    group_by(mcluster,scluster,st,status) %>% 
#    summarise(mt=mean(Minutes)) %>% distinct()
#  t1 <- da_mi_sleep %>% 
#    group_by(mcluster,scluster,st) %>% 
#    summarise(mt=mean(TotalTimeInBed)) %>% distinct()
#  
 
 x11()
     ggarrange(    
       # x11()  
    da_mi_sleep %>% 
      select(-c(27)) %>% 
      distinct() %>% 
     group_by(mcluster,scluster,st,status) %>%
     summarise(ttb=round(mean(Minutes/60),3)) %>% 

     # filter(status=="Awake" || status=="Restless") %>%
    ggplot(aes(x=scluster,y=ttb)) +
    # geom_boxplot()+
    geom_bar(stat="identity",aes(fill=status),position = "dodge")+
    facet_grid(st~mcluster)+
    geom_text(aes(label = sprintf("%2.1f", ttb),fill=status), 
              size = 3,
              position = position_dodge(width = 1),
              hjust=-0.23)+coord_flip()+
        ylab("MINS Sleep Status mins")+
        xlab("Sleeper Type"),
  # x11()
   da_mi_sleep %>% 
    select(-c(27)) %>% 
    distinct() %>% 
    group_by(mcluster,scluster,st) %>% 
    # 
    summarise(ttb=round(mean(TotalTimeInBed/60),3)) %>%
    # 
    # filter(status=="Awake" || status=="Restless") %>%
    ggplot(aes(x=scluster,y=ttb)) +
    # geom_boxplot()+
    geom_bar(stat="identity",position = "dodge")+
    facet_grid(st~mcluster)+
    geom_text(aes(label = sprintf("%2.1f", ttb)),
              size = 3,
              position = position_dodge(width = 1),
              hjust=-0.23)+coord_flip()+
    ylab("total time in bed hours")+
    xlab("Sleeper Type")
)
  da_mi_sleep %>% group_by(scluster,st) %>%  distinct () %>% summarise(ct=n())
  

              
    

 head(which(is.na(da_mi_sleep), arr.ind=TRUE))

 x11()
 da_mi_sleep %>%
   select(-c(27)) %>%
   distinct() %>% 
        group_by(scluster,st) %>% 
        summarise(mt=mean(TotalTimeInBed/60)) %>% 
        ggplot(aes(x=scluster,y=mt))+
     geom_col()+
     facet_grid(~st)+
     geom_text(aes(label = sprintf("%2.1f", mt)), 
               size = 3,
               position = position_dodge(width = 1),
               hjust=1,vjust=-1)+
     scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
   ylab("Total time in bed")
 
   
#################################################
# REMAINING TO ADD IN RMD
 
 ggarrange(
da_mi_sleep %>%  
  # mutate(hr=hour(time)) %>% 
  group_by(day,mcluster,scluster) %>% distinct() %>% 
  # filter(str_detect(st,"Average")) %>% 
  summarise(dist=round(mean(TotalDistance),2)) %>% 
  ggplot(aes(x=scluster,y=dist)) +
  geom_col(position="dodge")+
  facet_grid(day~mcluster)+
  # facet_grid(mcluster~scluster)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
   coord_flip()+
  scale_fill_viridis_c()+
  geom_text(aes(label = sprintf("%2.1f", dist)),
            size = 3,
            position = position_dodge(width = 1),
            hjust=-0.23),

da_mi_sleep %>%  
  # mutate(hr=hour(time)) %>% 
  group_by(day,mcluster,scluster) %>% distinct() %>% 
  # filter(str_detect(st,"Average")) %>% 
  summarise(cal=round(mean(Calories/1000),2)) %>% 
  ggplot(aes(x=scluster,y=cal)) +
  geom_col(position="dodge")+
  facet_grid(day~mcluster)+
  scale_fill_viridis(discrete=TRUE)+
  # facet_grid(mcluster~scluster)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  geom_text(aes(label = sprintf("%2.1f", cal)),
            size = 3,
            position = position_dodge(width = 1),
            hjust=-0.23)+
  coord_flip()+
  ylab("Calories Burnt")+
  theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5))
)
  
