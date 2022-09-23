library(tidyverse)
library(ggplot2)
library(readr)
pseed <- read_csv("C:/Users/mungo/Downloads/Kenaley/pseed.fin.amps.csv")
View(pseed)
pseed.b1 <- read_csv("C:/Users/mungo/Downloads/Kenaley/pseed.lengths.csv")
view(pseed.b1)
speeds <- read_csv("C:/Users/mungo/Downloads/Kenaley/pseed.calibration.csv")
view(speeds)

#CPK: ^^^ I can't read in data stored on your machine. [-2] These files are all in your repo and local directory where the R project is stored, so you just need this . . 
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

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

#CPK: This ^ is an example from the project description and unneeded [-1].
library(features)
#CPK: library calls should go at the top of the script

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

#CPK: This ^^^^ is all examples from the project description and unneeded [-1].

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

#CPK: This ^^^^ again is from the project description and unneeded.


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

#CPK: This function ^^^^ was already defined [-1].

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

#CPK: Now this ^^ is what we're after. Note that we want to find cycles in each experiment (data) so have to group by that column, too. And it could all be done in one series of pipes, e.g. . . . 
pseed.sum.max <- pseed.wide %>%
  group_by(fish,speed,date) %>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>% 
  filter(peak=T) %>%
  group_by(fish,speed)%>%
  mutate(amp.sum=L+R)%>%
  mutate(amp.sum.mean=mean(amp.sum))

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

#CPK: no need to compute sem separately. [-1]. You could've . . . 

pseed.sum.max <- pseed.wide%>%
  group_by(fish, bl.s,date)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum), amp.sum.se=SE(amp.sum))

#4
pd <-position_dodge(0.1)
pseed.sum.max%>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), color="black", width=.5, position=pd)+theme_classic()

#CPK: Nice work here ^^

#5
pseed.met.rate <- read.csv("C:/Users/mungo/Downloads/pseed.met.rate.csv")

#CPK: You're telling me to load data from your computer, which I can't do of course. This is what we're after:

pseed.met.rate <- read_csv("pseed.met.rate.csv")

View(pseed.met.rate)

#CPK: Unneeded ^

pseed.met.rate%>%
  print()

#CPK: Unneeded ^

pseed.sum.max <- pseed.sum.max%>%
  merge(pseed.met.rate, by=c("fish","date","bl.s"))%>%
  group_by(fish,speed)%>%
  mutate(amp.met.rate=mean(met.rate))%>%
  print()
pseed.sum.max <- pseed.sum.max%>%
  group_by(fish,speed)%>%
  summarize(met.rate,amp.met.rate,amp.max.sum,amp.sum.mean,amp.sum,amp.sum.se)%>%
  print()

pseed.sum.max <- 
  left_join(pseed.met.rate%>%
              group_by(fish,speed)%>%
              mutate(amp.met.rate=mean(met.rate))
            
  )
  

#CPK: This ^^ will work, but this is more concise and logical . . .




#6
pseed.sum.max%>%
  ggplot(aes(x=amp.met.rate,y=amp.sum.mean, color=fish, group=fish))+geom_point()+geom_smooth(method="lm")+theme_classic()

#CPK: Getting there, but we're still missing some important pieces, e.g., how to use the pipe to keep operations concise, loading data from our cloned repository, and knowing what to include (and what not to) to address the project prompts. 
