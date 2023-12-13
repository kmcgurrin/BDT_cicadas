#######################################################################################
##  compiling_data.R: 
##    cleans raw cicada oviposition data, adds clean trophic tree size data and clean trophic tree canopy cover data
##
##  Author: Kelsey McGurrin
##
#######################################################################################

####setup####
library(tidyverse)
library(janitor)

# working directory path (add yours if different)
setwd("G:/Shared drives/BiodiversiTREE/manuscripts/cicada ms 2024")

#### cicada oviposition ####

#Import Cicada Oviposition data set
ovip <- read_csv("input/ovip_simple_sept2022.csv",col_types = cols(Date = col_date(format = "%m/%d/%Y")))
ovip$Tree<-as.factor(ovip$Tree)
ovip$Treatment<-as.factor(ovip$Treatment)
ovip$Direction<-as.factor(ovip$Direction)
ovip$Flagging<-as.factor(ovip$Flagging)
ovip$Flagging.Binary<-as.numeric(ovip$Flagging.Binary)
ovip$Location.of.Damage<-as.factor(ovip$Location.of.Damage)
ovip$Girdled_Factor<-as.factor(ovip$Girdled_Factor)
ovip$Apical.Damage<-as.factor(ovip$Apical.Damage)
ovip$Canopy_OLD<-as.factor(ovip$Canopy_OLD)
#unique ID column may be named differently
names(ovip)[1]= "indiv"
ovip<-ovip%>%
  filter(indiv!="2973B", indiv!="8832B")
summary(ovip)

#### tree ID info ####
#import TREEs data set USE COLUMNS FOR BDT TREATMENT DATA
trees <- read_csv("input/all_trees_list_6_29_2022.csv")
cols(
  .default = col_double(),
  sppInt = col_factor(),
  sci_spp = col_factor(),
  common_spp = col_factor(),
  field = col_factor(),
  myco = col_factor(),
  rep = col_factor())
trees$plot<- as.factor(trees$plot)
trees$indiv<-as.factor(trees$indiv)
trees$div<-as.factor(trees$div)
trees$field<-as.factor(trees$field)
trees$rep<-as.factor(trees$rep)
summary(trees)

#### tree size ####
#USE "all years long_trees_102022" for tree size measurements as "size"
size  <- read_csv("input/all_years_long_tree102022.csv")
size <- filter(size,year == 2022)
size$months<-as.factor(size$months)
size$year<-as.factor(size$year)
size$indiv<-as.factor(size$indiv)
size$plot<- as.factor(size$plot)
size$div<-as.factor(size$div)
size$sppInt<-as.factor(size$sppInt)
size$sppAct<-as.factor(size$sppInt)
size$rep<-as.factor(size$rep)
size$ht<-as.numeric(size$ht)
size$radAvg<-as.numeric(size$radAvg)
summary(size)

#### join ovip with size ####
#merge with ovip and size 
ovip22 = merge(x=ovip,y=size[,c(3,26:29)],by="indiv", all.x=TRUE)
ovip22 = merge(x=ovip22,y=trees[1:15],by="indiv",all.x=TRUE)
# EXCLUDE MAINs
B_ovip22<-ovip22 %>%
  filter(Direction!="MAIN", sppAct!= "CAGL8", indiv!="17678")
#B_ovip22 SHOULD BE BRANCH LEVEL DATA

# Tree level data: Tovip has 1 row per tree;
T_ovip22<-B_ovip22 %>%
  group_by(indiv) %>%
  summarise(mScars = mean(Scars, na.rm = TRUE),
            t.flag=mean(Total.Flagging, na.rm = TRUE),
            b.flag=mean(Flagging.Binary, na.rm = TRUE),
            b.flag.n = sum(!is.na(Flagging.Binary)),
            mProx.Diam=mean(Prox.Diam, na.rm = TRUE),
            mDistal.Diam=mean(Distal.Diam,na.rm = TRUE),
            mLength=mean(Length, na.rm = TRUE))

T_ovip22 = merge(x=T_ovip22,y=size[,c(3,26:29)] ,by="indiv",all.x=TRUE)
T_ovip22 = merge(x=T_ovip22,y=trees[1:15] ,by="indiv",all.x=TRUE)
T_ovip22$ScarDen=T_ovip22$mScars/T_ovip22$radAvg
T_ovip22<-T_ovip22%>%
  filter(sppAct!= "CAGL8")
#Tovip$pair= Tovip$sppAct:rep 
summary(T_ovip22)
tabyl(T_ovip22, div, rep, sppAct)
write.csv(T_ovip22, "output/T_ovip22.csv")

#### join with canopy cover ####
#2021 and 2023


#### calc tree growth ####
#years 2020-21, 21-22, 22-23 (most important)
#don't drop trees if missing some data/some years

#growth in RCD, height, canopy radius most important
#also trunk volume and canopy volume?



#### check for known tree # issues ####
