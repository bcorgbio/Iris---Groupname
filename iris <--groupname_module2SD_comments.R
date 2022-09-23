library(tidyverse)
library(features)

#1- setting up data tibble 
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#CPK: This ^ could have been much more concise. a la ....

pseed.wide <-pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl) %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl)) %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)


#2- mean maximum 
find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}
pseed.sum.max <- pseed.wide%>%
  group_by(fish, bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.sum.max<- pseed.max %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum)) 

#CPK: ^ Why not just keep th pipe rolling, ie. no need to redefine pseed.sum.max


#3- standard error of the mean custom function 
SE <- function(x){
  sd(x)/ sqrt(length(x))
}
pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = SE(amp.sum))
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("speed","fish"))

#CPK, no need to compute sem separately and rejoin to the mean data. [-1]. Note also that cycle would apply to each experiment (date), not the more broad fish and speed only so we needed to group by fish, bl.s, and date. You could've . . . 

pseed.sum.max <- pseed.wide%>%
  group_by(fish, bl.s,date)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum), amp.sum.se=SE(amp.sum))



#4- mean v. specifc swimming speed 
pseed.sum.max %>%
  ggplot(aes(x=bl.s, y=amp.sum.mean, col = fish)) + geom_point() + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width = 0.03, size = 0.3)+geom_smooth(method="lm")

#CPK: Nice work here ^. 

#5- merge with new tibble 
pseed.met.sum <- pseed.met.rate%>%
  group_by(fish,bl.s)%>%
  summarize(met.mean=mean(met.rate),
            met.se=standard_error(met.rate))

#CPK: You didn't ever read in the met.rate data so this ^ doesn't work for me [-1]

#6- metabloic power output v. mean max 
pseed.sum.max %>%
  ggplot(aes(x=amp.sum.mean,y=met.mean,col=fish))+geom_point()+geom_errorbar(aes(ymin=met.mean-met.se,ymax= met.mean+met.se), width = 0.01, size = 0.3)+geom_smooth(method="lm")

#CPK: Nice work over all. Getting there!!!
