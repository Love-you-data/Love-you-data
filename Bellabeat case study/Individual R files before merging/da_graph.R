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


###########################################################

fit <- Mclust(daily_activity,G=4)
fviz_mclust_bic(fit,legend = NULL,shape="model")
summary(fit) 

daily_activity$mcluster <- fit$classification
options(scipen=10000)

cat_assign <- daily_activity %>%
        group_by(mcluster) %>% 
        summarise(ts=median(TotalSteps),
                  td=median(TotalDistance),
                  vad=median(VeryActiveDistance),
                  mad=median(ModeratelyActiveDistance),
                  lad=median(LightActiveDistance),
                  sd=median(SedentaryActiveDistance),
                  vam=median(VeryActiveMinutes),
                  fam=median(FairlyActiveMinutes),
                  lam=median(LightlyActiveMinutes),
                  sm=median(SedentaryMinutes),
                  ca=median(Calories)) 

da_overall <- daily_activity %>%
        summarise(
                mcluster = seq(4),
                ts=median(TotalSteps),
                td=median(TotalDistance),
                vad=median(VeryActiveDistance),
                mad=median(ModeratelyActiveDistance),
                lad=median(LightActiveDistance),
                sd=median(SedentaryActiveDistance),
                vam=median(VeryActiveMinutes),
                fam=median(FairlyActiveMinutes),
                lam=median(LightlyActiveMinutes),
                sm=median(SedentaryMinutes),
                ca=median(Calories)) 
#########################################################################
x11()
ggarrange(
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$vam), group = 1, fill = "Clustered Median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = vam, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Very Active Minutes")+
                geom_text(aes(label = round(cat_assign$vam,2),y=cat_assign$vam),hjust=-0.3, vjust =-0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$vam,2),y=da_overall$vam),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9)),
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$fam), group = 1, fill = "Clustered Median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = fam, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Fairly Active Minutes")+
                geom_text(aes(label = round(cat_assign$fam,2),y=cat_assign$fam),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$fam,2),y=da_overall$fam),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9)),
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$lam), group = 1, fill = "Clustered Median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = lam, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Lightly Active Minutes")+
                geom_text(aes(label = round(cat_assign$lam,2),y=cat_assign$lam),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$lam,2),y=da_overall$lam),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9)),
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$sm), group = 1, fill = "Clustered Median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = sm, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Sedentary Minutes")+
                geom_text(aes(label = round(cat_assign$sm,2),y=cat_assign$sm),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$sm,2),y=da_overall$sm),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9))
)
x11()
ggarrange(
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$vad), group = 1, fill = "Clustered median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = vad, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Very Active Distance")+
                geom_text(aes(label = round(cat_assign$vad,2),y=cat_assign$vad),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$vad,2),y=da_overall$vad),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9)),
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$mad), group = 1, fill = "Clustered median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = mad, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Fairly Active Distance")+
                geom_text(aes(label = round(cat_assign$mad,2),y=cat_assign$mad),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$mad,2),y=da_overall$mad),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9)),
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$lad), group = 1, fill = "Clustered median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = lad, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Lightly Active Distance")+
                geom_text(aes(label = round(cat_assign$lad,2),y=cat_assign$lad),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$lad,2),y=da_overall$lad),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9)),
        cat_assign %>% ggplot( aes(mcluster)) +
                geom_bar(aes(y = after_stat(cat_assign$sd), group = 1, fill = "Clustered median"),
                         width = 0.4, position = position_nudge(0.22)) +
                geom_col(aes(y = sd, fill = "Overall median"), data = da_overall,
                         width = 0.4, position = position_nudge(-0.22))+
                labs(y="Sedentary Distance")+
                geom_text(aes(label = round(cat_assign$sd,5),y=cat_assign$sd),hjust=-0.3, vjust = -0.5, size = 3,
                          position = position_dodge(0.9))+
                geom_text(aes(label = round(da_overall$sd,5),y=da_overall$sd),hjust=1, vjust = -0.4, size = 3,
                          position = position_dodge(0.9))
)

daily_activity$mcluster[daily_activity$mcluster %in% 1] <-"Lightly Active"
daily_activity$mcluster[daily_activity$mcluster %in% 2] <-"Fairly Active"
daily_activity$mcluster[daily_activity$mcluster %in% 4] <-"Very Active"
daily_activity$mcluster[daily_activity$mcluster %in% 3] <-"Sedentary"


#############################################################################
daily_activity %>% 
        group_by(mcluster) %>% 
        summarise(total=n()) %>% 
        mutate(totals = sum(total)) %>%
        group_by(mcluster) %>%
        summarise(total_percent = total / totals) %>%
        ggplot(aes(mcluster,y=total_percent, fill=factor(mcluster))) +
        geom_col()+
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position="none") +
        labs(title="User type distridution", x=NULL) +
        theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))+
        geom_text(aes(label = total_percent*100), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))


daily_activity %>%
        group_by(mcluster) %>% 
        summarise(total_calories = round((Calories)/1000,2)) %>% 
        
        ggplot(aes(x=mcluster,y=total_calories,fill=factor(mcluster))) +
        # geom_violin(draw_quantiles = c(0.25,0.5,0.75))
        # geom_bar(stat="identity")
        geom_boxplot()
# geom_text(aes(label = total_calories), vjust = -0.2, size = 3,
# position = position_dodge(0.9))


# scale_y_continuous(labels = comma)

daily_activity %>%
        group_by(mcluster) %>% 
        summarise(total_steps = round((TotalSteps),2)) %>% 
        ggplot(aes(x=mcluster,y=total_steps,fill=factor(mcluster))) +
        # geom_bar(stat="identity")+
        # geom_violin(draw_quantiles = c(0.25,0.5,0.75))
        geom_boxplot()
# geom_text(aes(label = total_steps), vjust = -0.2, size = 3,
#            position = position_dodge(0.9))
cat_steps


 daily_activity %>%
        group_by(mcluster) %>% 
        summarise(total_distance = round((TotalDistance),2)) %>% 
        
        ggplot(aes(x=mcluster,y=total_distance,fill=factor(mcluster))) +
        # geom_bar(stat="identity")+
        # geom_violin(draw_quantiles = c(0.25,0.5,0.75))
        geom_boxplot()
# geom_text(aes(label = total_distance), vjust = -0.2, size = 3,
#           position = position_dodge(0.9))
cat_distance

grid.arrange(cat_calories,cat_distance,cat_steps)

summary(daily_activity)

#CHECK BELOW LATER
##########################################################
ggarrange(
        daily_activity  %>%   ggplot(aes(x=SedentaryMinutes,y=VeryActiveMinutes)) +
                geom_line()+
                facet_grid(~factor(mcluster)),
        daily_activity  %>%   ggplot(aes(x=SedentaryMinutes,y=FairlyActiveMinutes)) +
                geom_line()+
                facet_grid(~factor(mcluster)),
        daily_activity  %>%   ggplot(aes(x=SedentaryMinutes,y=LightlyActiveMinutes)) +
                geom_line()+
                facet_grid(~factor(mcluster)))

#CHECK BACK LATER        
ggarrange(
        daily_activity %>% 
                ggplot(aes(y=(SedentaryMinutes),x=ActivityDate))+
                geom_col()+
                facet_grid(~mcluster)+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
        
        daily_activity %>% 
                ggplot(aes(y=(VeryActiveMinutes),x=ActivityDate))+
                geom_col()+
                facet_grid(~mcluster)+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
        daily_activity %>% 
                ggplot(aes(y=(FairlyActiveMinutes),x=ActivityDate))+
                geom_col()+
                facet_grid(~mcluster)+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
        daily_activity %>% 
                ggplot(aes(y=(LightlyActiveMinutes),x=ActivityDate))+
                geom_col()+
                facet_grid(~mcluster)+
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))


#########################################################################
#PERFECT

day_calories <- daily_activity  %>% 
        group_by(mcluster,day) %>% 
        summarise(total_calories= mean(Calories)) %>% 
        mutate(total_calories=round(total_calories/1000,2)) %>% 
        ggplot(aes(x=day,y=total_calories),fill=factor(mcluster)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_wrap(~factor(mcluster))+
        geom_text(aes(label =(total_calories)), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Avg CAlories Burnt(000)")

day_calories        

day_distance <- daily_activity  %>% 
        group_by(mcluster,day) %>% 
        summarise(total_distance= round(mean(TotalDistance),2)) %>% 
        ggplot(aes(x=day,y=total_distance)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_wrap(~factor(mcluster))+
        geom_text(aes(label =total_distance), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Total Distance(in km)")

day_distance 
day_steps <- daily_activity  %>% 
        group_by(mcluster,day) %>% 
        summarise(total_steps= mean(TotalSteps)) %>% 
        mutate(total_steps=round(total_steps/1000,2)) %>% 
        ggplot(aes(x=day,y=total_steps)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_wrap(~factor(mcluster))+
        geom_text(aes(label =total_steps),hjust=
                          0.4, vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Total Steps( in 000)")



n_distinct(da_mi_sleep$Id)
n_distinct(daily_activity$Id)
ggarrange(day_calories,day_distance,day_steps,nrow=2,ncol=2)
# ggexport(mp,filename = "mp.pdf",width=11,height=8.5)
###############################################################################        

week_steps <- daily_activity  %>% 
        group_by(mcluster,week) %>% 
        summarise(total_steps= mean(TotalSteps)) %>% 
        mutate(total_steps=round(total_steps/1000,2)) %>% 
        ggplot(aes(x=week,y=total_steps)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_wrap(~factor(mcluster))+
        geom_text(aes(label =total_steps),hjust=
                          0.4, vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Total Steps( in 000)")
week_steps

week_distance <- daily_activity  %>% 
        group_by(mcluster,week) %>% 
        summarise(total_distance= mean(TotalDistance)) %>% 
        ggplot(aes(x=week,y=total_distance)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_wrap(~factor(mcluster))+
        geom_text(aes(label =total_distance),hjust=
                          0.4, vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Total distance")
week_distance

week_calories <- daily_activity  %>% 
        group_by(mcluster,week) %>% 
        summarise(total_calories= mean(Calories)) %>% 
        mutate(total_calories=round(total_calories/1000,2)) %>% 
        ggplot(aes(x=week,y=total_calories)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_wrap(~factor(mcluster))+
        geom_text(aes(label =total_calories),hjust=
                          0.4, vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Total Calories( in 000)")
week_calories

ggarrange(week_calories,week_distance,week_steps,nrow=2,ncol=2)

summary(daily_activity)
daily_activity <- daily_activity  %>% 
        mutate(st=case_when(
                TotalSteps < quantile(TotalSteps,0.25) ~"Noob(<3790)",
                TotalSteps >=quantile(TotalSteps,0.25) &
                        TotalSteps<quantile(TotalSteps,0.50) ~"Average\n(>=3790 & <7405)",
                TotalSteps >=quantile(TotalSteps,0.50) &
                        TotalSteps<quantile(TotalSteps,0.75) ~"Above\nAVerage\n(>=7405& <10727)",
                TotalSteps >= quantile(TotalSteps,0.75) ~"Advanced(>=10727)"
        )) 

x11() 
daily_activity  %>% 
        group_by(mcluster,day,st) %>% 
        summarise(total_calories= mean(Calories)) %>% 
        mutate(total_calories=round(total_calories/1000,2)) %>% 
        ggplot(aes(x=day,y=total_calories),fill=factor(mcluster)) +
        geom_col(aes(fill=factor(mcluster)))+
        scale_fill_viridis_d(option="turbo")+
        facet_grid(factor(mcluster)~st)+
        geom_text(aes(label =(total_calories)), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Avg CAlories Burnt(000)")+
        theme(legend.title = element_blank(),legend.position = "none")
x11()
daily_activity  %>% 
        group_by(mcluster,day,st) %>% 
        summarise(total_sed= mean(SedentaryMinutes)) %>% 
        # mutate(total_calories=round(total_calories/1000,2)) %>% 
        ggplot(aes(x=day,y=total_sed),fill=factor(mcluster)) +
        geom_col(aes(fill=factor(mcluster)))+
        facet_grid(factor(mcluster)~st)+
        geom_text(aes(label =round(total_sed,2)), vjust = -0.2, size = 3,
                  position = position_dodge(0.9))+
        theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
        labs(y="Sed min")

daily_activity %>% select(Id,mcluster) %>% distinct()



#####################################

# hourly_cal_int_step <- left_join(hourly_cal_int_step,daily_activity[,c(1,2,16:19)],by=c("Date"="ActivityDate",
                                                                      # "Id"="Id",
                                                                      # "day"="day",
                                                                      # "week"="week"))
# hourly_cal_int_step <- left_join(hourly_cal_int_step,da_sleep[,c(1,2,16:19,23,25)],by=c("Date"="ActivityDate",
                                                                                      # "Id"="Id",
                                                                                      #   "day"="day",
                                                                                      #   "week"="week"))
hourly_cal_int_step <- left_join(hourly_cal_int_step,daily_activity[,c(1,2,16:19)],by=c("ActivityDate","Id",
                                                                                        "day",
                                                                                        "week"))
# anti_join(hourly_cal_int_step,da_sleep[,c(1,2,16:19,23,25)])
head(which(is.na(hourly_cal_int_step), arr.ind=TRUE))

 hourly_cal_int_step  %>% 
         mutate(hr=hour(time)) %>% 
         group_by(mcluster,day,hr,st) %>% 
         summarise(total_calories= mean(Calories),
                   total_steps=mean(StepTotal),
                   total_intensity=mean(TotalIntensity)) %>% 
         mutate(total_calories=round(total_calories/1000,2)) %>% 
         ggplot(aes(x=hr,y=total_calories)) +
         geom_col(position="dodge")+
         facet_grid(day~factor(mcluster))+
         geom_text(aes(label =mean(total_calories)), vjust = -0.2, size = 3,
                   position = position_dodge(0.9))+
         theme(axis.text.x = element_text(angle =45,vjust=1,hjust=0.5))+
         labs(y="Avg CAlories Burnt(000)")
 
         hourly_cal_int_step  %>% 
                 mutate(hr=hour(time)) %>% 
                 group_by(mcluster,day,hr,st) %>% 
                 summarise(total_calories= mean(Calories),
                           total_steps=mean(StepTotal),
                           total_intensity=mean(TotalIntensity)) %>% 
                 # mutate(total_calories=round(total_calories/1000,2)) %>% 
                 ggplot(aes(x=hr,y=total_steps)) +
                 geom_col(position="dodge")+
                 facet_grid(day~factor(mcluster))
         hourly_cal_int_step  %>% 
                 mutate(hr=hour(time)) %>% 
                 group_by(mcluster,day,hr,st) %>% 
                 summarise(total_calories= mean(Calories),
                           total_steps=mean(StepTotal),
                           total_intensity=mean(TotalIntensity)) %>% 
                 # mutate(total_calories=round(total_calories/1000,2)) %>% 
                 ggplot(aes(x=hr,y=total_intensity)) +
                 geom_col(position="dodge")+
                 # geom_line()+
                 facet_grid(day~factor(mcluster))
         
         
                hourly_cal_int_step  %>% 
                         mutate(hr=hour(time)) %>% 
                         group_by(mcluster,day,hr,st) %>% 
                         summarise(total_calories= mean(Calories),
                                   total_steps=mean(StepTotal),
                                   total_intensity=mean(TotalIntensity)) %>% 
                         # mutate(total_calories=round(total_calories/1000,2)) %>% 
                         ggplot(aes(y=total_calories,x=total_intensity)) +
                         geom_line()+
                         facet_grid(day~factor(mcluster))
                 
                hourly_cal_int_step  %>% 
                        mutate(hr=hour(time)) %>% 
                        group_by(mcluster,day,hr,st) %>% 
                        summarise(total_calories= mean(Calories),
                                  total_steps=mean(StepTotal),
                                  total_intensity=mean(TotalIntensity)) %>% 
                        # mutate(total_calories=round(total_calories/1000,2)) %>% 
                        ggplot(aes(y=total_steps,x=total_intensity)) +
                        geom_line()+
                        facet_grid(day~factor(mcluster))
                        
   tt <-    left_join(hourly_cal_int_step,da_sleep[,c(1,2,16:19,23)],by=c("Id"="Id",
                                                                             "ActivityDate"="ActivityDate",
                                                                          "day"="day",
                                                                          "mcluster"="mcluster",
                                                                          "week"="week",
                                                                          "st"="st")) %>% drop_na()
                
 quantile(hourly_cal_int_step$TotalIntensity)   



 
 
 
 tt %>%  
        mutate(hr=hour(time)) %>% 
        group_by(day,hr,scluster,st) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(int=round(mean(TotalIntensity),2)) %>% 
        ggplot(aes(x=hr,y=int)) +
        geom_col(aes(fill=st))+
        # geom_line(aes(color=scluster,linetype=scluster))+
        facet_grid(day~scluster)+
        scale_fill_brewer(palette="Dark2")+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Intensity")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))
#################################################################


x11()
ggarrange(
tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active") %>% 
        group_by(day,hr,scluster,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(int=round(mean(TotalIntensity),2)) %>% 
        ggplot(aes(x=hr,y=int)) +
        geom_col(position="dodge",size=1,fill="#E69F00")+
        # geom_line(aes(color=st),size=1)+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Intensity")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Avg Intensities Per hour",
                subtitle = "Sleepers vs Steps taken for LIGHTLY ACTIVE users"),

tt %>% 
mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active") %>% 
        group_by(day,hr,scluster,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(int=round(mean(TotalIntensity),2)) %>% 
        ggplot(aes(x=hr,y=int)) +
        geom_col(position="dodge",size=1,fill="#CC79A7")+
        # geom_line(aes(color=st),size=1)+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Intensity")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Avg Intensities Per hour",
                subtitle = "Sleepers vs Steps taken for FAIRLY ACTIVE users"),

tt %>% 
mutate(hr=hour(time)) %>% filter(mcluster=="Sedentary") %>% 
        group_by(day,hr,scluster,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(int=round(mean(TotalIntensity),2)) %>% 
        ggplot(aes(x=hr,y=int)) +
        geom_col(position="dodge",size=1,fill="steelblue")+
        # geom_line(aes(color=st),size=1)+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Intensity")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Avg Intensities Per hour",
                subtitle = "Sleepers vs Steps taken for Sedentary users"),

tt %>% 
mutate(hr=hour(time)) %>% filter(mcluster=="Very Active") %>% 
        group_by(day,hr,scluster,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(int=round(mean(TotalIntensity),2)) %>% 
        ggplot(aes(x=hr,y=int)) +
        geom_col(position="dodge",size=1,fill="#009E73")+
        # geom_line(aes(color=st),size=1)+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Intensity")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Avg Intensities Per hour",
                subtitle = "Sleepers vs Steps taken for VERY ACTIVE users")


)
x11()
ggarrange(
tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active") %>% 
        group_by(day,hr,scluster,st,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(cal=round(mean(Calories),2)) %>% 
        ggplot(aes(x=hr,y=cal)) +
        geom_col(position="dodge",size=1,fill="#E69F00")+
        # geom_line(aes(color=st),size=1)+
        # geom_line(aes(color=scluster,linetype=scluster))+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Calories Burnt")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Calories Burnt Per hour",
                subtitle = "Sleepers vs Steps taken for LIGHTLY ACTIVE users"),

tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active") %>% 
        group_by(day,hr,scluster,st,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(cal=round(mean(Calories),2)) %>% 
        ggplot(aes(x=hr,y=cal)) +
        geom_col(position="dodge",size=1,fill="deeppink4")+ #CC79A7
        # geom_line(aes(color=st),size=1)+
        # geom_line(aes(color=scluster,linetype=scluster))+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Calories Burnt")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Calories Burnt Per hour",
                subtitle = "Sleepers vs Steps taken for FAIRLY ACTIVE users"),
tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Sedentary") %>% 
        group_by(day,hr,scluster,st,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(cal=round(mean(Calories),2)) %>% 
        ggplot(aes(x=hr,y=cal)) +
        geom_col(position="dodge",size=1,fill="steelblue")+
        # geom_line(aes(color=st),size=1)+
        # geom_line(aes(color=scluster,linetype=scluster))+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Calories Burnt")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Calories Burnt Per hour",
                subtitle = "Sleepers vs Steps taken for Sedentary users"),
tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Very Active") %>% 
        group_by(day,hr,scluster,st,mcluster) %>% distinct() %>% 
        # filter(str_detect(st,"Average")) %>% 
        summarise(cal=round(mean(Calories),2)) %>% 
        ggplot(aes(x=hr,y=cal)) +
        geom_col(position="dodge",size=1,fill="#009E73")+
        # geom_line(aes(color=st),size=1)+
        # geom_line(aes(color=scluster,linetype=scluster))+
        facet_grid(day~scluster)+
        # scale_fill_distiller()+
        scale_fill_brewer(palette="Dark2")+
        # facet_grid(mcluster~scluster)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Calories Burnt")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("Steps taken" ))+
        ggtitle("Calories Burnt Per hour",
                subtitle = "Sleepers vs Steps taken for VERY ACTIVE users")
)
################################################################################
##################################### LIGHTLY ACTIVE SHORT SLEEPER

ggarrange(
 tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active",
                                         scluster=="Short Sleeper") %>% 
        group_by(day,hr,st) %>% distinct() %>% 
        summarise(int=round(mean(TotalIntensity),2)) %>% 
        ggplot(aes(x=hr,y=int)) +
        # geom_col(position="stack",size=1)+#009E73
        geom_line(size=1,aes(color="#009E73"))+
        facet_grid(st~day)+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Intensities")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("blank"),
              aspect.ratio = 2)+
        ggtitle("Average Intensities Per hour",
                subtitle = "Steps taken vs Day for LIGHTLY ACTIVE SHORT SLEEPERS"),
 
tt %>%  
        mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active",
                                         scluster=="Short Sleeper") %>% 
        group_by(day,hr,st) %>% distinct() %>% 
        summarise(cal=round(mean(Calories),2)) %>% 
        ggplot(aes(x=hr,y=cal,color="Deeppink4")) +
        # geom_col(position="stack",size=1)+#009E73
        geom_line(size=1,aes(color="Deeppink4"))+
        facet_grid(st~day)+
        scale_color_manual(values="Deeppink4")+
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
        ylab("Avg Calories")+
        xlab("hour")+
        theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
              legend.title = element_text("blank"),
              aspect.ratio = 2)+
        ggtitle("Average Calories Burnt Per hour",
                subtitle = "Steps taken vs Day for LIGHTLY ACTIVE SHORT SLEEPERS")
        
)

##################################### 
# LIGHTLY ACTIVE PERFECT SLEEPER

ggarrange(
        tt %>%  
                mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active",
                                                 scluster=="Perfect Sleepers") %>% 
                group_by(day,hr,st) %>% distinct() %>% 
                summarise(int=round(mean(TotalIntensity),2)) %>% 
                ggplot(aes(x=hr,y=int)) +
                # geom_col(position="stack",size=1)+#009E73
                geom_line(size=1,aes(color="#009E73"))+
                facet_grid(st~day)+
                scale_fill_brewer(palette="Dark2")+
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                ylab("Avg Intensities")+
                xlab("hour")+
                theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
                      legend.title = element_text("blank"),
                      aspect.ratio = 2)+
                ggtitle("Average Intensities Per hour",
                        subtitle = "Steps taken vs Day for LIGHTLY ACTIVE PERFECT SLEEPERS"),
        
        tt %>%  
                mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active",
                                                 scluster=="Perfect Sleepers") %>% 
                group_by(day,hr,st) %>% distinct() %>% 
                summarise(cal=round(mean(Calories),2)) %>% 
                ggplot(aes(x=hr,y=cal,color="Deeppink4")) +
                # geom_col(position="stack",size=1)+#009E73
                geom_line(size=1,aes(color="Deeppink4"))+
                facet_grid(st~day)+
                scale_color_manual(values="Deeppink4")+
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                ylab("Avg Calories")+
                xlab("hour")+
                theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
                      legend.title = element_text("blank"),
                      aspect.ratio = 2)+
                ggtitle("Average Calories Burnt Per hour",
                        subtitle = "Steps taken vs Day for LIGHTLY ACTIVE PERFECT SLEEPERS")
        
)

############################################# 
# LIGHTLY ACTIVE LONG SLEEPER

ggarrange(
        tt %>%  
                mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active",
                                                 scluster=="Long Sleepers") %>% 
                group_by(day,hr,st) %>% 
                summarise(int=round(mean(TotalIntensity),2)) %>% 
                ggplot(aes(x=hr,y=int)) +
                # geom_col(position="stack",size=1)+#009E73
                geom_line(size=1,aes(color="#009E73"))+
                facet_grid(st~day)+
                scale_fill_brewer(palette="Dark2")+
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                ylab("Avg Intensities")+
                xlab("hour")+
                theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
                      legend.title = element_text("blank"),
                      aspect.ratio = 2)+
                ggtitle("Average Intensities Per hour",
                        subtitle = "Steps taken vs Day for LIGHTLY ACTIVE LONG SLEEPERS"),
        
        tt %>%  
                mutate(hr=hour(time)) %>% filter(mcluster=="Lightly Active",
                                                 scluster=="Long Sleepers") %>% 
                group_by(day,hr,st) %>% 
                summarise(cal=round(mean(Calories),2)) %>% 
                ggplot(aes(x=hr,y=cal)) +
                # geom_col(position="stack",size=1)+#009E73
                geom_line(size=1,aes(color="Deeppink4"))+
                facet_grid(st~day)+
                scale_color_manual(values="Deeppink4")+
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                ylab("Avg Calories")+
                xlab("hour")+
                theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
                      legend.title = element_text("blank"),
                      aspect.ratio = 2)+
                ggtitle("Average Calories Burnt Per hour",
                        subtitle = "Steps taken vs Day for LIGHTLY ACTIVE LONG SLEEPERS")
        
)
# #############################################
# # FAIRLY ACTIVE SHORT SLEEPER
# 
# ggarrange(
#         tt %>%  
#                 mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active",
#                                                  scluster=="Short Sleeper") %>% 
#                 group_by(day,hr,st) %>% distinct() %>% 
#                 summarise(int=round(mean(TotalIntensity),2)) %>% 
#                 ggplot(aes(x=hr,y=int)) +
#                 # geom_col(position="stack",size=1)+#009E73
#                 geom_line(size=1,color="#009E73")+
#                 facet_grid(st~day)+
#                 scale_fill_manual(values="#009E73")+
#                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
#                 ylab("Avg Intensities")+
#                 xlab("hour")+
#                 theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
#                       legend.title = element_text("blank"),
#                       aspect.ratio = 2)+
#                 ggtitle("Average Intensities Per hour",
#                         subtitle = "Steps taken vs Day for FAIRLY ACTIVE SHORT SLEEPERS"),
#         
#         tt %>%  
#                 mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active",
#                                                  scluster=="Short Sleeper") %>% 
#                 group_by(day,hr,st) %>% distinct() %>% 
#                 summarise(cal=round(mean(Calories),2)) %>% 
#                 ggplot(aes(x=hr,y=cal)) +
#                 # geom_col(position="stack",size=1)+#009E73
#                 geom_line(size=1,color="#0072b2")+
#                 facet_grid(st~day)+
#                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
#                 ylab("Avg Calories")+
#                 xlab("hour")+
#                 theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
#                       legend.title = element_text("blank"),
#                       aspect.ratio = 2)+
#                 ggtitle("Average Calories Burnt Per hour",
#                         subtitle = "Steps taken vs Day for FAIRLY ACTIVE SHORT SLEEPERS")
#         
# )
# 
# # FAIRLY ACTIVE PERFECT SLEEPER
# 
# ggarrange(
#         tt %>%  
#                 mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active",
#                                                  scluster=="Perfect Sleepers") %>% 
#                 group_by(day,hr,st) %>% distinct() %>% 
#                 summarise(int=round(mean(TotalIntensity),2)) %>% 
#                 ggplot(aes(x=hr,y=int)) +
#                 # geom_col(position="stack",size=1)+#009E73
#                 geom_line(size=1,color="#009E73")+
#                 facet_grid(st~day)+
#                 scale_fill_manual(values="#009E73")+
#                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
#                 ylab("Avg Intensities")+
#                 xlab("hour")+
#                 theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
#                       legend.title = element_text("blank"),
#                       aspect.ratio = 2)+
#                 ggtitle("Average Intensities Per hour",
#                         subtitle = "Steps taken vs Day for FAIRLY ACTIVE PERFECT SLEEPERS"),
#         
#         tt %>%  
#                 mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active",
#                                                  scluster=="Perfect Sleepers") %>% 
#                 group_by(day,hr,st) %>% distinct() %>% 
#                 summarise(cal=round(mean(Calories),2)) %>% 
#                 ggplot(aes(x=hr,y=cal)) +
#                 # geom_col(position="stack",size=1)+#009E73
#                 geom_line(size=1,color="#0072b2")+
#                 facet_grid(st~day)+
#                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
#                 ylab("Avg Calories")+
#                 xlab("hour")+
#                 theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
#                       legend.title = element_text("blank"),
#                       aspect.ratio = 2)+
#                 ggtitle("Average Calories Burnt Per hour",
#                         subtitle = "Steps taken vs Day for FAIRLY ACTIVE PERFECT SLEEPERS")
#         
# )
# 
# # FAIRLY ACTIVE LONG SLEEPER
# 
# ggarrange(
#         tt %>%  
#                 mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active",
#                                                  scluster=="Long Sleepers") %>% 
#                 group_by(day,hr,st) %>% distinct() %>% 
#                 summarise(int=round(mean(TotalIntensity),2)) %>% 
#                 ggplot(aes(x=hr,y=int)) +
#                 # geom_col(position="stack",size=1)+#009E73
#                 geom_line(size=1,color="#009E73")+
#                 facet_grid(st~day)+
#                 scale_fill_manual(values="#009E73")+
#                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
#                 ylab("Avg Intensities")+
#                 xlab("hour")+
#                 theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
#                       legend.title = element_text("blank"),
#                       aspect.ratio = 2)+
#                 ggtitle("Average Intensities Per hour",
#                         subtitle = "Steps taken vs Day for FAIRLY ACTIVE LONG SLEEPERS"),
#         
#         tt %>%  
#                 mutate(hr=hour(time)) %>% filter(mcluster=="Fairly Active",
#                                                  scluster=="Long Sleepers") %>% 
#                 group_by(day,hr,st) %>% distinct() %>% 
#                 summarise(cal=round(mean(Calories),2)) %>% 
#                 ggplot(aes(x=hr,y=cal)) +
#                 # geom_col(position="stack",size=1)+#009E73
#                 geom_line(size=1,color="#0072b2")+
#                 facet_grid(st~day)+
#                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
#                 ylab("Avg Calories")+
#                 xlab("hour")+
#                 theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
#                       legend.title = element_text("blank"),
#                       aspect.ratio = 2)+
#                 ggtitle("Average Calories Burnt Per hour",
#                         subtitle = "Steps taken vs Day for FAIRLY ACTIVE LONG SLEEPERS")
#         
# )

###########################

int_cal <- function(tt,mclust,sclust,color1,color2){
ggarrange(
        tt %>%  
                mutate(hr=hour(time)) %>% filter(mcluster==mclust,
                                                 scluster==sclust) %>% 
                group_by(day,hr,st) %>% distinct() %>% 
                summarise(int=round(mean(TotalIntensity),2)) %>% 
                ggplot(aes(x=hr,y=int)) +
                # geom_col(position="stack",size=1,fill=color1)+#009E73
                geom_line(size=1,color=color1)+
                facet_grid(st~day)+
                # scale_fill_manual(values="#009E73")+
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                ylab("Avg Intensities")+
                xlab("hour")+
                theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
                      legend.title = element_text("blank"),
                      aspect.ratio = 2)+
                ggtitle("Average Intensities Per hour",
                        subtitle = paste("Steps taken vs Day for ",mclust,sclust,sep=" ")),
        
        tt %>%  
                mutate(hr=hour(time)) %>% filter(mcluster==mclust,
                                                 scluster==sclust) %>% 
                group_by(day,hr,st) %>% distinct() %>% 
                summarise(cal=round(mean(Calories),2)) %>% 
                ggplot(aes(x=hr,y=cal)) +
                # geom_col(position="stack",size=1,fill=color2)+#009E73
                geom_line(size=1,color=color2)+
                facet_grid(st~day)+
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                ylab("Avg Calories")+
                xlab("hour")+
                theme(axis.text.x = element_text(angle =90,vjust=1.0,hjust=0.5),
                      legend.title = element_text("blank"),
                      aspect.ratio = 2)+
                ggtitle("Average Calories Burnt Per hour",
                         subtitle = paste("Steps taken vs Day for ",mclust,sclust,sep=" "))
        
)
}

int_cal(tt,"Lightly Active","Short Sleeper","#db6d00","#490092")
int_cal(tt,"Lightly Active","Perfect Sleepers","#db6d00","#490092")
int_cal(tt,"Lightly Active","Long Sleepers","#db6d00","#490092")

int_cal(tt,"Fairly Active","Short Sleeper","#009E73","#0072b2")
int_cal(tt,"Fairly Active","Perfect Sleepers","#009E73","#0072b2")
int_cal(tt,"Fairly Active","Long Sleepers","#009E73","#0072b2")

int_cal(tt,"Sedentary","Short Sleeper","#ff6db6","#920000")
int_cal(tt,"Sedentary","Perfect Sleepers","#ff6db6","#920000")
int_cal(tt,"Sedentary","Long Sleepers","#ff6db6","#920000")

int_cal(tt,"Very Active","Short Sleeper","#006ddb","#ff6db6")
int_cal(tt,"Very Active","Perfect Sleepers","#006ddb","#ff6db6")
int_cal(tt,"Very Active","Long Sleepers","#006ddb","#ff6db6")










