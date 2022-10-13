# Iris <- Groupname Offical Problem Set 3


# 1

library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

#load data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")
anole.tree <- read.tree("anole.tre")

anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

anole.log.lm  <- lm(HTotal~SVL,anole.log)

anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

pgls.BM2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM2))

anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))

# 2 


anole.log.PH.lm <- lm(HTotal~SVL+PH,anole.log)

anole.log.ArbPD.lm <- lm(HTotal~SVL+ArbPD,anole.log)


# 3

anole.log <- anole.log %>%
  mutate(resPH=residuals(anole.log.PH.lm))%>%
  mutate(resPD=residuals(anole.log.ArbPD.lm))

p.PH <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=resPH)) + geom_boxplot()
p.PH+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

p.PD <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=resPD)) +geom_boxplot()
p.PD+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)


# 4

#PGLS under BM, with PH
pgls.BMPH <- gls(HTotal ~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, with PD
pgls.BMPD <- gls(HTotal ~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, with both
pgls.BMBOTHPHPD <- gls(HTotal ~SVL+PH+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

# 5

anole.phylo.aic <- AICc(pgls.BMPH,pgls.BMPD,pgls.BMBOTHPHPD)
aicw(anole.phylo.aic$AICc)


#         fit     delta           w
# 1 -64.77956 10.746149 0.003247185
# 2 -73.81081  1.714901 0.296905077
# 3 -75.52571  0.000000 0.699847738

# both have an effect - the pgls considering both is best fit


# num 6 focus on predictions of the models 

anole.log <- anole.log %>%
  mutate(resBOTHPHPD=residuals(pgls.BMBOTHPHPD))

p.BOTH <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=resBOTHPHPD)) +geom_boxplot()
p.BOTH+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=2)

#CPK: Excellent work. Some slick analysis and coding! 15/15