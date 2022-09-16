library(ggplot2)
library(tidyverse)

#CPK: I assume this is Brianna's script based on the repo history. You were tasked with naming the script with your First and Last name. [-1] You were also tasked with committing and pushing your script/figures to the repo through git, but this directory was uploaded through the github site.[-1]
setwd("~/Desktop/BC.ExpMethOrgBio.Module#1")
#CPK: No need to set the wd if you use and R project.

dat <- read.csv("scales.csv") # a variable containing the scales data set #1
sapply(dat,class) #reports class of each column of data set #2
dim(dat) #reports dimensions(rows x  columns) of data set #3

#changes species from character to factor, global species variable
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
#CPK: Don't need this next line [-1]
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

#CPK: This is repeated below [-1]
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
  
  #CPK: You were tasked with creating a pdf with your name as the file name. [-1]
  
  
  #CPK: Overall, a solid effort. However, it seems there are some missing pieces like gitting and understanding what's needed (no more or less) to address the prompts.