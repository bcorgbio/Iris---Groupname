library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)
#setwd("~/Desktop/BC.ExpMethOrgBio.Module#3")

#CPK: Don't sent the wd![-2]. I don't have this directory on my machine! Commented out.


#load data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

#1) establishment of anole2 tibble by first combining and filtering anole.csv and anole.eco
anole2 <- anole%>%
left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()#%>%
  
  #CPK:^ notice the errant pipe. threw an error [-1]. commented out
  
#1) creating anole.log tibble from anole2
anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#2) creation of linear model for PH with + notation
anole.log.PH.lm <- lm(HTotal~SVL+PH,anole.log)

#2) creation of  linear model for PD with + notation
anole.log.PD.lm <- lm(HTotal~SVL+ArbPD,anole.log)

#3) compute residuals for both PH and PD
anole.log <- anole.log %>%
  mutate(res1=residuals(anole.log.PH.lm))
anole.log <- anole.log%>%
  mutate(res2=residuals(anole.log.PD.lm))

#CPK: No need to put in seperate mutate operations, could compute both res1 and res2 at same time.

#plot for PH residual
anole.log%>%
  ggplot(aes(Ecomorph2,res1))+geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)
#plot for PD residual
anole.log%>%
  ggplot(aes(Ecomorph2,res2))+geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

#4) create phylogenetic tree using BM and create least squares models
#creates tree
anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)
#creates pgls BM models
pgls.BM1 <- gls(HTotal ~SVL +PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM2 <- gls(HTotal ~SVL + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM3 <- gls(HTotal ~SVL +PH + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#5) perform AICc and AICw tests
anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.BM3)
aicw(anole.phylo.aic$AICc)
#RESULTS:
#pglsBM1 fit= -64.8, pglsBM2 fit= -73.8, pglsBM3 fit= -75.5
#pglsBM1 w = 0.003, pglsBM2 w= 0.297, pglsBM3 w= 0.699
#delta 1= 10.75, delta2= 1.71, delta3= 0.0000
# based on deltaAIC #1 and its fit, pglsBM1 is a poor fit, 
#while pglsBM2 and pglsBM3 are roughly equivalent in both,
#indicating that perch diameter is a significant predictor
#of hindlimb length while perch height is not as much.
#pglsBM3 with both though has the smaller fit and greater w, so it is the slightly better fit.

#CPK: These results don't indicate that PH is a significant predictory, just that a model that include PH fits the data better than one that doesn't/

#6 create plot of best fit model
anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM3))
anole.log %>%
  dplyr::select(SVL,res1,res2)%>%
  pivot_longer(cols=c("res1", "res2"))

p.BM3.phylo <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=phylo.res)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)
  print(p.BM3.phylo)

  
#CPK: really well done. Just a few lingering hiccups.
