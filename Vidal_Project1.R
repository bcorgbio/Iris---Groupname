#Project 1 Script Vidal

library(ggplot2)
library(tidyverse)

setwd("~/Desktop/education/senior year/BIOL3140/Projects/scales")

dat <- read.csv("scales.csv")

# 1 
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
length(species) 

# 2 
sapply(dat,class)

# 3 
dim(dat)

# 4
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

# 5
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

# 6 
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
