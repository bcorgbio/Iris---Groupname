#Trying to push new R scriot to GitHub

library(ggplot2)
library(tidyverse)

setwd("~/Desktop/education/senior year/BIOL3140/Projects/Vidal_Project1")
#CPK: No need set the working directory when working in an R project.

# 1
dat <- read.csv("scales.csv")

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
pdf("species.quadrant.vidal.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: Excellent work!
