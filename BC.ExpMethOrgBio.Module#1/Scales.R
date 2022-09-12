library(ggplot2)
library(tidyverse)
setwd("~/Desktop/BC.ExpMethOrgBio.Module#1")
dat <- read.csv("scales.csv") # a variable containing the scales data set #1
sapply(dat,class) #reports class of each column of data set #2
dim(dat) #reports dimensions(rows x  columns) of data set #3

#changes species from character to factor, global species variable
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species

#Reports number of observations/punctures per species in column labelled n #4
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

#Reports number of speciments per species #5
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#produces box plots of mean puncture force/quadrant in each species, makes pdf file of them saved in wd
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

  pdf("species.quadrant.pdf")
  for(i in species){
    p <- dat %>%
      filter(species==i)%>%
      ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
    print(p)
  }
  dev.off()
  list.files(pattern=".pdf")
  