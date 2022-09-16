library(ggplot2)
library(tidyverse)
library(ggplot2)
setwd("C:/Users/mungo/Downloads/Kenaley")
#CPK: No need set the working directory when working in an R project.

dat <- read_csv("Mungovan Project 1/scales.csv")
#CPK: Just put your data in the same directory (i.e., repo) as your R project and you can spare yourself the long string to get to this file. I put the scales data in the folder that has your script in it. Notice how just have to include the folder name to read the data.


dim(dat)

#CPK: Don't need this next line and several others [-4]
head(dat)
class(dat$N)
class(dat$quadrant)
class(dat$species)
class(dat$specimen)

#CPK: Isn't it more concise to use a function to find the class of the columns like you did below, seems you might just be copying the code but perhaps don't understand how modifications address the prompts. [-1]

#CPK: dont' need this next line
mean(dat$N)
sapply(dat,class)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
#CPK: dont' need these next 13 line
species
length(species)
dat$species==species[1]
dat$species[dat$species==species[1]]
A.rup<-length(dat$species[dat$species==species[1]])
L.gib<-length(dat$species[dat$species==species[2]])
L.mac<-length(dat$species[dat$species==species[3]])
M.sal<-length(dat$species[dat$species==species[4]])
M.sax<-length(dat$species[dat$species==species[5]])
P.fla<-length(dat$species[dat$species==species[6]])
species.obs <- data.frame(sp=species,n=c(A.rup,L.gib,L.mac,M.sal,M.sax,P.fla))
species.obs

dat %>%
  group_by(species) %>%  
  summarise(n = n())  
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#CPK: dont' need this loop, it's repeated below
for(i in 1:10) print(i)
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

#CPK: dont' need this next line
list.files(pattern=".pdf")

#CPK: We should talk, Clare. Doesn't seem you understood the thrust of the project and how to address the prompts, i.e., you just paste a bunch of code and modified a few things.