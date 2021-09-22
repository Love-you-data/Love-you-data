    ht<- heartrate_seconds %>%
        mutate(hr=hour(time)) %>% 
    group_by(Id,Date,hr) %>% 
    summarise(val=mean(Value))

colnames(ht)[2]<-"ActivityDate"
ht<- left_join(ht,tt[,c(1:3,5,7,9:12)] %>% distinct()) %>%  drop_na()
x11()
ht %>% 
    ggplot(aes(x=hr,y=val)) +
    geom_line()+
    facet_grid(day~Id)+
    theme(axis.text.x = element_text(angle =45,vjust=1.0,hjust=0.5))
# rm(ht)

library(sqldf)
test <- sqldf("SELECT * 
    FROM hourly_cal_int_step hcis
WHERE hcis.ID IN(
    SELECT Id
    FROM ht
)")
test$hr <- hour(test$time)

x11()
test %>% 
    mutate(hr=hour(time)) %>% 
    group_by(Id,ActivityDate,hr,day) %>% 
    summarise(cal=mean(Calories),
              int=mean(TotalIntensity),
              steps=mean(StepTotal)) %>% 
    ggplot(aes(x=hr,y=int)) +
    geom_line()+
    facet_grid(day~Id)

met_hr <- minuteMET_narrow %>% 
    mutate(hr=hour(time)) %>% 
    group_by(Id,Date,day,hr) %>% 
    summarise(met=sum(METs/10)) 

test2 <- sqldf("SELECT met_hr.*,test.mcluster,test.st
    FROM met_hr
    LEFT JOIN test
    ON met_hr.Id=test.Id AND 
        met_hr.Date=test.ActivityDate AND
        met_hr.day=test.day AND
        met_hr.hr=test.hr
    WHERE met_hr.Id IN(
    SELECT Id
    FROM test
)")



  x11() 
  test2 %>% 
    group_by(Id,mcluster,day,hr) %>% 
    summarise(met=mean(met)) %>% 
    ggplot(aes(x=hr,y=met/60),alpha=0.1)+
    geom_line(aes(color=mcluster))+
    facet_grid(day~Id)+
    ylab("Average Metabolic rate per minute")
# x11()

 


















