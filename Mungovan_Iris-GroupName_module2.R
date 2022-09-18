library(tidyverse)
library(ggplot2)
library(readr)
pseed <- read_csv("C:/Users/mungo/Downloads/Kenaley/pseed.fin.amps.csv")
View(pseed)
pseed.b1 <- read_csv("C:/Users/mungo/Downloads/Kenaley/pseed.lengths.csv")
view(pseed.b1)
speeds <- read_csv("C:/Users/mungo/Downloads/Kenaley/pseed.calibration.csv")
view(speeds)
# Work with fins (for reference)
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
pseed.b1%>%
  print()
pseed2%>%
  select(fish)%>%
  unique()
pseed2 <- pseed2%>%
  left_join(pseed.b1,by="fish")%>%
  print()
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()
library(features)
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
fget(f1)
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()
pseed2%>%
  summarize(n=length(unique(date)))
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>%  
    as_tibble()%>%    
    filter(curvature<0)%>%     
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)}
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T)
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")
pseed2
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
pseed2 %>%
  filter(fin=="R")
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()
#2
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.max.sum=mean(amp.sum))
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x=x,y=y*mult))[2:3] %>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}
pseed.sum.max <- pseed.wide %>%
  group_by(fish,speed) %>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>% 
  filter(peak=T)
pseed.sum.max$peak <- NULL
pseed.sum.max <- pseed.wide %>%
  group_by(fish,speed)%>%
  mutate(amp.sum=L+R)%>%
  mutate(amp.sum.mean=mean(amp.sum))%>%
  print()
#3
SE <- function(x){
  sd(x)/sqrt(length(x))
}
pseed.sum.max <- pseed.sum.max %>%
  group_by(fish,speed)%>%
  select(-m.s)%>%
  select(-L)%>%
  select(-R)%>%
  mutate(amp.sum.se=sd(amp.sum)/sqrt(length(amp.sum)))%>%
  print()
#4
pd <-position_dodge(0.1)
pseed.sum.max%>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), color="black", width=.5, position=pd)+theme_classic()
#5
pseed.met.rate <- read.csv("C:/Users/mungo/Downloads/pseed.met.rate.csv")
View(pseed.met.rate)
pseed.met.rate%>%
  print()
pseed.sum.max <- pseed.sum.max%>%
  merge(pseed.met.rate, by=c("fish","date","bl.s"))%>%
  group_by(fish,speed)%>%
  mutate(amp.met.rate=mean(met.rate))%>%
  print()
pseed.sum.max <- pseed.sum.max%>%
  group_by(fish,speed)%>%
  summarize(met.rate,amp.met.rate,amp.max.sum,amp.sum.mean,amp.sum,amp.sum.se)%>%
  print()
#6
pseed.sum.max%>%
  ggplot(aes(x=amp.met.rate,y=amp.sum.mean, color=fish, group=fish))+geom_point()+geom_smooth(method="lm")+theme_classic()

