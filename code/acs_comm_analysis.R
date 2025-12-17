library(tidyverse)
library(vegan)


x=read.csv("data/spp_mat_fam_all.csv")
ttt<-x$Treatment
crab<-x$Crab
nutrients<-x$Nutrients

x_orig<-x[,-c(1:7)]

abun=rowSums(x_orig)
richness=specnumber(x_orig)
rar=rarefy(x_orig,min(rowSums(x_orig)))

m1<-glm(rar~crab*nutrients,family="quasipoisson")
summary(m1)

rardf<-data.frame(rar,ttt,nutrients,crab)

rardf %>% 
  group_by(ttt) %>% 
  summarise(ttt = mean(rar)) -> rar3

#maybe some eividence that crabs affect rarefied richness

ggplot(rardf,aes(x=crab,y=rar,colour=nutrients))+
  geom_point()
 

#beta jac
beta_j=vegdist(x_orig,method="jaccard",binary=T)

#composition
adonis2(x_orig~crab+nutrients,method="jaccard")

#beta diversity
betamod<-betadisper(beta_j,ttt,type="median")
permutest(betamod, permutations = 999)


###nmds on bray curtis dissimilarity matrix
mds_out<-metaMDS(x_orig,distance="jaccard",binary=T)
data.scores <- as.data.frame(scores(mds_out)$sites)  

df<-data.frame(data.scores,crab,nutrients,ttt)

hull_ttt <- df %>%
  group_by(nutrients) %>%
  slice(chull(NMDS1,NMDS2))


ggplot()+
  geom_point(data=df,aes(x=NMDS1,y=NMDS2,group=nutrients,colour=nutrients,pch=nutrients))+
  geom_polygon(data=hull_ttt,aes(x=NMDS1,y=NMDS2,fill=nutrients,colour=nutrients,group=nutrients),alpha=0.30) + # add the convex hulls
  theme_bw()

x_orig2<-data.frame(x_orig,ttt)

comp_tile<-x_orig2%>%
  group_by(ttt)%>%
  summarise(across(everything(), list(mean)))

comp_tile2 <- gather(comp_tile, Species, Abundance, 2:59)

comp_tile2$pres_abs<-as.factor(ifelse(comp_tile2$Abundance>0,1,0))

comp_tile2$ttt<-factor(comp_tile2$ttt,levels=c("Control","Crab","Nutrient","Both")) #reorder levels


ggplot(data=comp_tile2,aes(x=ttt,y=Species,fill=pres_abs))+
  geom_tile()
