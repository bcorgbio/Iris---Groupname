
#Project 2 Vidal


library(tidyverse)
library(features)


#1 Establishing pseed2 data table and pseed.wide

#load data
pseed <- read_csv("pseed.fin.amps.csv")

pseed.bl <- read_csv("pseed.lengths.csv")

speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)

# Create a wider tibble by deleting the amp column (just cuz we don't care about
# it anymore) and using the names from fin to create a column with those names 
# and containing the values from amp.bl
# Additionally, create a new column containing the sum of the L and R amplitudes
# by adding the values from the L and R columns for each row
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)

# 2 Compute the Mean amp.sums 

# Create a function to create a vector of all the peaks from all of the 
# experiments (peaks = critical point where curvature is negative)

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.sum.max <- pseed.wide%>%
  group_by(date,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

pseed.sum.max <- pseed.sum.max%>%
  group_by(fish, speed) %>%
  summarize(amp.sum,amp.sum.mean=mean(amp.sum))

# 3

find.se <- function(x){
  samp_size <- length(x)
    sd(x)/(sqrt(samp_size))
}

pseed.sum.max <- pseed.sum.max%>%
  group_by(fish, speed) %>%
  summarize(amp.sum, amp.sum.mean, amp.sum.se=find.se(amp.sum))

# 4

pseed.sum.max %>%  
ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+geom_point() + 
  scale_color_manual(values=c('purple','#E69F00'))+
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), 
                colour = 'black', width=1)+
  geom_point()

# 5

pseed.met.rate <- read_csv("pseed.met.rate.csv")

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate,by=c('fish'='fish'))

# 6

pseed.sum.max %>%
  ggplot(aes(x=amp.sum.mean,y=met.rate,col=fish))+geom_point()+
  scale_color_manual(values=c('purple','#E69F00'))


