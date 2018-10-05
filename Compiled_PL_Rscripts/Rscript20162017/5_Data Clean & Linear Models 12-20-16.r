############################ 
#PURPOSE: Plotting results from Blue Ridge Patch Stats Analysis
#INPUT: Tables on patch stats for expansion and new patches "PLv2_Exp_Table_ppgap", "PLv2_New_Table_ppgap"
#OUTPUT: pdf of plots 
#DEVELOPED: 10/31/16, 11/8/16 - Iara Lacher
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
# Need to run the multiplot function. This is in the one note notes.
##### NEXT STEPS #####
# Total area of ecoregions calculated in arc. #* can add this code in later.

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 


# ----------------------------------------------
################################################


# PACKAGES NEEDED

# Plotting
 library(dplyr)
 library(reshape)
 library(stringr)
 library(MASS) # linear regression

# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")




# ----------------------------------------------
# READ INPUT FILES:

# Output_Folder <- "C:/Users/LacherL/Documents/AA_Smithsonian/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" #V Drive
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"

iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)

 
############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# DATA CLEANING
# ----------------------------------------------

pl_un <-filter(iStats_Join, Transition<9999)

# Remove buffer areas
pl_foc <- filter(pl_un, er_maj==1|er_maj==2|er_maj==3)

# from 1800 onward (since there are not patches with core area before or on 1800)
pl_1800 <-filter(pl_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
# pl_new <-filter(pl_foc, estYr>=1985)
pl_new <-filter(pl_foc, estYr>1985)# 1526



#Split by ecoregion
pl_foc_e1 <- filter(pl_foc, er_maj==1)#385
pl_foc_e23 <- filter(pl_foc, er_maj>=2)#1151

# ----------------------------------------------
# UNKNOWNS ONLY:
{# pl_unknowns <-filter(iStats_Join, Transition==9999)
 
# str(pl_unknowns)
# 'data.frame':	595 obs. of  44 variables:
 
# > sum(pl_unknowns$area.ha)
# [1] 440008.2
}


# ----------------------------------------------
# ----------------------------------------------
# OVERALL (end of line)  TOTALS
# ----------------------------------------------
# ----------------------------------------------
{

# ----------------------------------------------
# OVERALL - NUMBER OF PATCHES
# ----------------------------------------------

COUNT <- group_by(iStats_Join) %>%
group_by( estYr) %>%
summarize(total = n())

# ----------------------------------------------
# OVERALL - AREA IN ECOREGION
# ----------------------------------------------

# ----------------------------------------------
#Area
TOTcum_Ar1 <- group_by(pl_foc_e1) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_Ar1$er_maj <- 1

TOTcum_Ar23 <- group_by(pl_foc_e23) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_Ar23$er_maj <- 23

TOTcum_ArER <- bind_rows(TOTcum_Ar1, TOTcum_Ar23)
TOTcum_ArER$Type <- "area"

# ----------------------------------------------
#Core Area
TOTcum_CAr1 <- group_by(pl_foc_e1) %>%
group_by(er_maj) %>%
summarize(sum = sum(core.area.ha))
TOTcum_CAr1$er_maj <- 1

TOTcum_CAr23 <- group_by(pl_foc_e23) %>%
group_by(er_maj) %>%
summarize(sum = sum(core.area.ha))
TOTcum_CAr23$er_maj <- 23

TOTcum_CArER <- bind_rows(TOTcum_CAr1, TOTcum_CAr23)
TOTcum_CArER$Type <- "core"

TOTcum_TArER <- bind_rows(TOTcum_ArER, TOTcum_CArER)

# > TOTcum_TArER
# Source: local data frame [6 x 3]

  # er_maj        sum Type
# 1      1 1744830.72 area
# 2     23  121600.44 area
# 3     23  241211.52 area
# 4      1 1331241.48 core
# 5     23   36391.68 core
# 6     23  150080.04 core


# ----------------------------------------------
# TOTAL FINAL AREA IN OWNERSHIP

TOTcum_ArOwn1 <- group_by(pl_foc_e1) %>%
group_by(own_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_ArOwn1$er_maj <- 1

TOTcum_ArOwn23 <- group_by(pl_foc_e23) %>%
group_by(own_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_ArOwn23$er_maj <- 23

TOTcum_ArOwnER <- bind_rows(TOTcum_ArOwn1, TOTcum_ArOwn23)

# > TOTcum_ArOwnER
# Source: local data frame [8 x 3]

  # own_maj        sum er_maj
# 1       1 1581667.56      1
# 2       2   65473.92      1
# 3       3    2190.24      1
# 4       4   95499.00      1
# 5       1  183665.88     23
# 6       2  153786.60     23
# 7       3    1412.64     23
# 8       4   23946.84     23


# ----------------------------------------------
# TOTAL FINAL LAND COVER BY TYPE - Not using majority

NLCD1 <- group_by(nlcd_e1) %>%
group_by(Class) %>%
summarize(sum = sum(nlcd_area.ha))
NLCD1$er_maj <- 1

NLCD23 <- group_by(nlcd_e23) %>%
group_by(Class) %>%
summarize(sum = sum(nlcd_area.ha))
NLCD23$er_maj <- 23

NLCD <- bind_rows(NLCD1, NLCD23)

# > NLCD
# Source: local data frame [6 x 3]

  # Class       sum er_maj
# 1     5 607503.24      1
# 2     6  11524.68      1
# 3     7    223.56      1
# 4     5 148641.48     23
# 5     6  21565.44     23
# 6     7   3794.04     23


# ----------------------------------------------
# TOTAL AREA BY MANAGEMENT STATUS- NOT USING MAJ, FROM 1800
 
GAP1 <- group_by(gap_foc_e1) %>%
group_by(Class) %>%
summarize(sum = sum(gap_area.ha))
GAP1$er_maj <- 1

GAP23 <- group_by(gap_foc_e23) %>%
group_by(Class) %>%
summarize(sum = sum(gap_area.ha))
GAP23$er_maj <- 23

GAP <- bind_rows(GAP1, GAP23)

# > GAP
# Source: local data frame [8 x 3]

  # Class        sum er_maj
# 1     1  169037.28      1
# 2     2 1001156.76      1
# 3     3  207930.24      1
# 4     4  366706.44      1
# 5     1   38672.64     23
# 6     2  137839.32     23
# 7     3  178439.76     23
# 8     4    7860.24     23

}


# ----------------------------------------------
# ----------------------------------------------
# CUMULATIVE ACROSS TIME
# ----------------------------------------------
# ----------------------------------------------
{

# ----------------------------------------------
# CUMULATIVE - CLASSSTATS
# ----------------------------------------------
{
CS_inc123<-read.table(paste0(Output_Folder,"CS_inc123.txt"), sep=",", header=TRUE)
# > str(CS_inc123)
# 'data.frame':	14 obs. of  12 variables:
 # $ class               : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ n.patches           : int  204 209 239 276 354 422 448 250 306 355 ...
 # $ total.area          : num  1.57e+10 1.58e+10 1.59e+10 1.65e+10 1.67e+10 ...
 # $ prop.landscape      : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ patch.density       : num  1.30e-08 1.33e-08 1.50e-08 1.68e-08 2.12e-08 ...
 # $ largest.patch.index : num  0.48 0.479 0.476 0.592 0.586 ...
 # $ total.core.area     : num  1.29e+10 1.30e+10 1.31e+10 1.34e+10 1.36e+10 ...
 # $ aggregation.index   : num  95.3 95.3 95.3 95.1 95 ...
 # $ patch.cohesion.index: num  9.95 9.95 9.95 9.96 9.96 ...
 # $ er                  : Factor w/ 2 levels "Blue Ridge","Piedmont": 1 1 1 1 1 1 1 2 2 2 ...
 # $ yrInt               : int  1985 1990 1995 2000 2005 2010 2015 1985 1990 1995 ...
 # $ total.core.area.ha  : num  1294160 1295316 1305198 1341590 1356808 ...

#Split by ecoregion - 7 obs in each, one per yr increment
CS_incB <- filter(CS_inc123, er=="Blue Ridge")
CS_incP <- filter(CS_inc123, er=="Piedmont")
}

# ----------------------------------------------
# CUMULATIVE - NUMBER OF PATCHES
# ----------------------------------------------
{
# Levels: Expanded noMeas Separated

pl_new$TypeID<-as.integer(ifelse(pl_new$Type=="Expanded", 2, ifelse(pl_new$Type=="Separated",1, ifelse(pl_new$Type=="noMeas", 0,0))))
Pat_cnt <- group_by(pl_new) %>%
group_by(TypeID, estYr) %>%
summarize(total = n())
Pat_cnt <- filter(Pat_cnt, TypeID>0)

# WRITE TO FILE
write.table(Pat_cnt, file = paste0(Output_Folder,"Pat_cnt",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
Pat_cnt<-read.table(paste0(Output_Folder,"Pat_cnt.txt"), sep=",", header=TRUE)



# Assign Type ID column
pl_new86_e1$TypeID<-as.integer(ifelse(pl_new86_e1$Type=="Expanded", 2, ifelse(pl_new86_e1$Type=="Separated",1, ifelse(pl_new86_e1$Type=="noMeas", 0,0))))

#Split by ecoregion
pl_new86_e1 <- filter(pl_new86, er_maj==1)#385
pl_new86_e23 <- filter(pl_new86, er_maj>=2)#1151 out of 

pl_new86_e1$TypeID<-as.integer(ifelse(pl_new86_e1$Type=="Expanded", 2, ifelse(pl_new86_e1$Type=="Separated",1, ifelse(pl_new86_e1$Type=="noMeas", 0,0))))
Pat_cnt1 <- group_by(pl_new86_e1) %>%
group_by(TypeID, estYr) %>%
summarize(total = n())
Pat_cnt1$er_maj<-1

pl_new86_e23$TypeID<-as.integer(ifelse(pl_new86_e23$Type=="Expanded", 2, ifelse(pl_new86_e23$Type=="Separated",1, ifelse(pl_new86_e23$Type=="noMeas", 0,0))))
Pat_cnt23 <- group_by(pl_new86_e23) %>%
group_by(TypeID, estYr) %>%
summarize(total = n())
Pat_cnt23$er_maj<-23

Pat_cnt123 <- bind_rows(Pat_cnt1, Pat_cnt23)
Pat_cnt123 <- filter(Pat_cnt123, TypeID>0)

# WRITE TO FILE
write.table(Pat_cnt123, file = paste0(Output_Folder,"Pat_cnt123",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
Pat_cnt123<-read.table(paste0(Output_Folder,"Pat_cnt123.txt"), sep=",", header=TRUE)
}

# ----------------------------------------------
# AREA BY ECOREGION
# ----------------------------------------------
{
cum_Ar1 <- group_by(pl_foc_e1) %>%
group_by(er_maj, estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

cum_Ar23 <- group_by(pl_foc_e23) %>%
group_by(estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_Ar23$er_maj <- 23

# Select only years 1985 and later
cum_Ar1 <-filter(cum_Ar1, estYr>=1985)
cum_Ar23 <-filter(cum_Ar23, estYr>=1985)

cum_ArER <- bind_rows(cum_Ar23, cum_Ar1)

# # WRITE TO FILE
# write.table(cum_TArERA, file = paste0(Output_Folder,"cum_TArERA",".txt"), row.names=FALSE, sep=",")

# # READ FROM FILE
# cum_TArERA<-read.table(paste0(Output_Folder,"cum_TArERA.txt"), sep=",", header=TRUE)

range(cum_Ar1$running_total)
range(cum_Ar23$running_total)
range(cum_CAr1$running_total)
range(cum_CAr23$running_total)

# CORE AREA - (from ClassStatSel function(one I edited))
# READ FROM FILE
CS_inc123<-read.table(paste0(Output_Folder,"CS_inc123.txt"), sep=",", header=TRUE)

#Split by ecoregion - 7 obs in each, one per yr increment
CS_incB <- filter(CS_inc123, er=="Blue Ridge")
CS_incP <- filter(CS_inc123, er=="Piedmont")
}

# ----------------------------------------------
# AREA BY OWNERSHIP
# ----------------------------------------------
{
cum_ArOwn <- group_by(pl_foc) %>%
group_by(own_maj, estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

# Select only years 1985 and later
cum_ArOwn <-filter(cum_ArOwn, estYr>=1985)

# Blue Ridge Only
cum_ArOwn1 <- group_by(pl_foc_e1) %>%
group_by(own_maj, estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

cum_ArOwn1$er_maj<-1

# Select only years 1985 and later
cum_ArOwn1 <-filter(cum_ArOwn1, estYr>=1985)

# Piedmont  Only
cum_ArOwn23 <- group_by(pl_foc_e23) %>%
group_by(own_maj, estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

cum_ArOwn23$er_maj<-23

# Select only years 1985 and later
cum_ArOwn23 <-filter(cum_ArOwn23, estYr>=1985)

cum_ArOwn123 <- bind_rows(cum_ArOwn1, cum_ArOwn23)


# WRITE TO FILE
write.table(cum_ArOwn123, file = paste0(Output_Folder,"cum_ArOwn123",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
cum_ArOwn123<-read.table(paste0(Output_Folder,"cum_ArOwn123.txt"), sep=",", header=TRUE)

range(cum_ArOwn1$running_total)
range(cum_ArOwn23$running_total)
}


# ----------------------------------------------
# HABITAT REPRESENTATION- NLCD
# ----------------------------------------------

{
# ----------------------------------------------
# NLCD - NOT BY MAJORITY AREA
# ----------------------------------------------

# REmove Unkowns
nlcd_un <-filter(nlcd_all, Transition<9999)

# Remove buffer areas
nlcd_foc <- filter(nlcd_un, er_maj==1|er_maj==2|er_maj==3)#2065

# from 1800 onward (since there are not patches with core area before or on 1800)
nlcd_1800 <-filter(nlcd_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
nlcd_new <-filter(nlcd_foc, estYr>=1985)#1536

#Split by ecoregion
nlcd_foc_e1 <- filter(nlcd_foc, er_maj==1)#385
nlcd_foc_e23 <- filter(nlcd_foc, er_maj>=2)#1151 out of 

# Do only Forest, Grasses, and Crop
nlcd_e1 <-filter(nlcd_foc_e1, Class==c("5", "6", "7"))
nlcd_e23 <-filter(nlcd_foc_e23, Class==c("5", "6", "7"))

# nlcd_e123 <- bind_rows(nlcd_e1, nlcd_e23)






# ----------------------------------------------
# CUMULATIVE TOTALS ACROSS TIME- since 1800

cum_NL1 <- group_by(nlcd_e1) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(nlcd_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_NL1$er_maj <- 1

# Select only years 1985 and later
cum_NL1 <-filter(cum_NL1, estYr>=1985)

cum_NL23 <- group_by(nlcd_e23) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(nlcd_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_NL23$er_maj <- 23

# Select only years 1985 and later
cum_NL23 <-filter(cum_NL23, estYr>=1985)


# ----------------------------------------------
# CUMULATIVE SINCE 1985 ("Added habitat")
# ----------------------------------------------

#Split by ecoregion
nlcd_new_e1 <- filter(nlcd_new, er_maj==1)#385
nlcd_new_e23 <- filter(nlcd_new, er_maj>=2)#1151 out of 


# Do only Forest, Grasses, and Crop
nlcd_e1 <-filter(nlcd_new_e1, Class==c("5", "6", "7"))
nlcd_e23 <-filter(nlcd_new_e23, Class==c("5", "6", "7"))


cum_NL1 <- group_by(nlcd_e1) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(nlcd_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_NL1$er_maj <- 1


cum_NL23 <- group_by(nlcd_e23) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(nlcd_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_NL23$er_maj <- 23

cum_NL123 <- bind_rows(cum_NL1, cum_NL23)








range(cum_NL1$running_total)
range(cum_NL23$running_total)

# WRITE TO FILE
write.table(cum_NL123, file = paste0(Output_Folder,"cum_NL123",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
cum_NL123<-read.table(paste0(Output_Folder,"cum_NL123.txt"), sep=",", header=TRUE)

}


# ----------------------------------------------
# MANAGEMENT -  GAP
# ----------------------------------------------
{
# REmove Unkowns
gap_un <-filter(gap_all, Transition<9999)

# Remove buffer areas
gap_foc <- filter(gap_un, er_maj==1|er_maj==2|er_maj==3)#2065

# from 1800 onward (since there are not patches with core area before or on 1800)
gap_1800 <-filter(gap_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
gap_new <-filter(gap_foc, estYr>=1985) #1536

#Split by ecoregion
gap_foc_e1 <- filter(gap_foc, er_maj==1)#385
gap_foc_e23 <- filter(gap_foc, er_maj>=2)#1151 out of 




# ----------------------------------------------
# GAP - NOT BY MAJORITY AREA
# ----------------------------------------------

# ----------------------------------------------
# CUMULATIVE TOTALS ACROSS TIME- since 1800

cum_GAP1 <- group_by(gap_foc_e1) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(gap_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_GAP1$er_maj <- 1

# Select only years 1985 and later
cum_GAP1 <-filter(cum_GAP1, estYr>=1985)

cum_GAP23 <- group_by(gap_foc_e23) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(gap_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_GAP23$er_maj <- 23

# Select only years 1985 and later
cum_GAP23 <-filter(cum_GAP23, estYr>=1985)


# ----------------------------------------------
# CUMULATIVE SINCE 1985 ("Added habitat")

#Split by ecoregion
gap_new_e1 <- filter(gap_new, er_maj==1)#385
gap_new_e23 <- filter(gap_new, er_maj>=2)#1151 out of 

# ----------------------------------------------
# CUMULATIVE TOTALS ACROSS TIME- since 1985

cum_GAP1 <- group_by(gap_new_e1) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(gap_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_GAP1$er_maj <- 1


cum_GAP23 <- group_by(gap_new_e23) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(gap_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_GAP23$er_maj <- 23

cum_GAP123 <- bind_rows(cum_GAP1, cum_GAP23)


# WRITE TO FILE
write.table(cum_GAP123, file = paste0(Output_Folder,"cum_GAP123",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
cum_GAP123<-read.table(paste0(Output_Folder,"cum_GAP123.txt"), sep=",", header=TRUE)

range(cum_GAP1$running_total)
range(cum_GAP23$running_total)
}


# ----------------------------------------------
# CLIMATE CHANGE AND CONNECTIVITY - RESILIENCE CLASS
# ----------------------------------------------
{
# REmove Unkowns
resil_un <-filter(resil_all, Transition<9999)

# Remove buffer areas
resil_foc <- filter(resil_un, er_maj==1|er_maj==2|er_maj==3)#2065

# from 1800 onward (since there are not patches with core area before or on 1800)
resil_1800 <-filter(resil_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
resil_new <-filter(resil_foc, estYr>=1985)#1536

#Split by ecoregion
resil_foc_e1 <- filter(resil_foc, er_maj==1)#385
resil_foc_e23 <- filter(resil_foc, er_maj>=2)#1151 out of 

# ----------------------------------------------
# TOTAL AREA BY RESILIENCE SCORE- NOT USING MAJ, FROM 1800

RESIL1 <- group_by(resil_foc_e1) %>%
group_by(Class) %>%
summarize(sum = sum(resil_area.ha))
RESIL1$er_maj <- 1

RESIL23 <- group_by(resil_foc_e23) %>%
group_by(Class) %>%
summarize(sum = sum(resil_area.ha))
RESIL23$er_maj <- 23

RESIL <- bind_rows(RESIL1, RESIL23)

# RESIL
# Source: local data frame [16 x 3]

   # Class        sum er_maj
# 1      1  126483.12      1
# 2      2   64511.64      1
# 3      3 1267637.04      1
# 4      4   21963.96      1
# 5      5  248222.88      1
# 6      6    4007.88      1
# 7      7    1626.48      1
# 8      8    5857.92      1
# 9      1  162677.16     23
# 10     2   67541.04     23
# 11     3   74934.72     23
# 12     4   13799.16     23
# 13     5   24802.20     23
# 14     6    5964.84     23
# 15     7    3084.48     23
# 16     8    2527.20     23

}
}


# ----------------------------------------------
# ----------------------------------------------
# NEAREST NEIGHBOR
# ----------------------------------------------
# ----------------------------------------------
{
# First remove NAs # Does not include unknowns
pl_new.na<- filter(pl_new, !is.na(min_dist.km))# 1526

dist_n1 <- filter(pl_new.na, er_maj==1) #383
dist_n1$er <- "23"

dist_n23 <- filter(pl_new.na, er_maj>=2) #1143
dist_n23$er <- "23"

par(mfrow=c(2,1))
hist(dist_n1$min_dist.km); hist(dist_n23$min_dist.km)

library(MASS)

nnB <-glm(formula=sqrt(min_dist.km)~estYr, data=dist_n1)
nnP <-glm(formula=sqrt(min_dist.km)~estYr, data=dist_n23)

#QC:
par(mfrow=c(2,2))
plot(nnB, main="Blue Ridge"); plot(nnP, main="Piedmont")
summary(nnB); summary(nnP)

# ----------------------------------------------

DF1<-as.data.frame(cbind(dist_n1$estYr, dist_n1$er_maj))
colnames(DF1)<-c("estYr", "er")
DF1$er_maj <- "1"
inv1 <- exp(as.data.frame(predict(nnB, se.fit=TRUE)))/1+exp(as.data.frame(predict(nnB, se.fit=TRUE)))
inv1$lwr<-inv1$fit-(1.96*inv1$se.fit)
inv1$upr<-inv1$fit+(1.96*inv1$se.fit)
pred1<-cbind(DF1, inv1)

DF23<-as.data.frame(cbind(dist_n23$estYr, dist_n23$er_maj))
colnames(DF23)<-c("estYr", "er")
DF23$er_maj <- "23"
inv23 <- exp(as.data.frame(predict(nnP, se.fit=TRUE)))/1+exp(as.data.frame(predict(nnP, se.fit=TRUE)))
inv23$lwr<-inv23$fit-(1.96*inv23$se.fit)
inv23$upr<-inv23$fit+(1.96*inv23$se.fit)
pred23<-cbind(DF23, inv23)

pred123 <- bind_rows(pred1, pred23)

# # WRITE TO FILE
# write.table(pred123, file = paste0(Output_Folder,"NNpred123",".txt"), row.names=FALSE, sep=",")

# # READ FROM FILE
# NNpred123<-read.table(paste0(Output_Folder,"NNpred123.txt"), sep=",", header=TRUE)

}

# ----------------------------------------------
# ----------------------------------------------
# PROXIMITY TO DEVELOPMENT
# ----------------------------------------------
# ----------------------------------------------
{
# ----------------------------------------------

pl_new #1536 obs. of  45 variables

# Remove zeros
pl_new_z <- filter(pl_new, minDevDis>0) #1409 of 45 vars

# BY ECOREGION

dev_1 <- filter(pl_new_z, er_maj==1) #385 obs of 45 vars
dev_23 <- filter(pl_new_z, er_maj>=2) #1151 obs of 45 vars
# ~~

par(mfrow=c(2,1))
hist(sqrt(dist_n1$minDevDis)); hist(sqrt(dist_n23$minDevDis))


ddevB <-glm(formula=sqrt(minDevDis)~estYr, data=dev_1)
ddevP <-glm(formula=sqrt(minDevDis)~estYr, data=dev_23)


#QC:
par(mfrow=c(2,2))
plot(ddevB); plot(ddevP)
summary(ddevB); summary(ddevP)

# ----------------------------------------------


DF1<-as.data.frame(cbind(dev_1$estYr, dev_1$er_maj))
colnames(DF1)<-c("estYr", "er")
DF1$er_maj <- "Blue Ridge"
	
inv1 <- exp(as.data.frame(predict(ddevB, se.fit=TRUE)))/1+exp(as.data.frame(predict(ddevB, se.fit=TRUE)))
inv1$lwr<-inv1$fit-(1.96*inv1$se.fit)
inv1$upr<-inv1$fit+(1.96*inv1$se.fit)
pred1<-cbind(DF1, inv1)


DF23<-as.data.frame(cbind(dev_23$estYr, dev_23$er_maj))
colnames(DF23)<-c("estYr", "er")
DF23$er_maj <- "Piedmont"

inv23 <- exp(as.data.frame(predict(ddevP, se.fit=TRUE)))/1+exp(as.data.frame(predict(ddevP, se.fit=TRUE)))
inv23$lwr<-inv23$fit-(1.96*inv23$se.fit)
inv23$upr<-inv23$fit+(1.96*inv23$se.fit)
pred23<-cbind(DF23, inv23)

pred123 <- bind_rows(pred1, pred23)

# ----------------------------------------------
# BY PUBLIC OR PRIVATE
# ----------------------------------------------

pp_1 <- filter(pl_foc, pp_maj==1)
pp_2 <- filter(pl_foc, pp_maj==2)
# ~~

library(MASS)
devppB <-glm.nb(formula=minDevDis~estYr, data=pp_1)
devppP <-glm.nb(formula=minDevDis~estYr, data=pp_2)

#QC:
par(mfrow=c(2,2))
plot(devppB); plot(devppP)
summary(devppB); summary(devppP)


DF1<-as.data.frame(cbind(pp_1$estYr, pp_1$pp_maj))
colnames(DF1)<-c("estYr", "pp")
DF1$pp_maj <- "Public"
	
inv1 <- exp(as.data.frame(predict(devppB, se.fit=TRUE)))/1+exp(as.data.frame(predict(devppB, se.fit=TRUE)))
inv1$lwr<-inv1$fit-(1.96*inv1$se.fit)
inv1$upr<-inv1$fit+(1.96*inv1$se.fit)
pred1<-cbind(DF1, inv1)


DF2<-as.data.frame(cbind(pp_2$estYr, pp_2$pp_maj))
colnames(DF2)<-c("estYr", "pp")
DF2$pp_maj <- "Private"

inv2 <- exp(as.data.frame(predict(devppP, se.fit=TRUE)))/1+exp(as.data.frame(predict(devppP, se.fit=TRUE)))
inv2$lwr<-inv2$fit-(1.96*inv2$se.fit)
inv2$upr<-inv2$fit+(1.96*inv2$se.fit)
pred2<-cbind(DF2, inv2)

pred12 <- bind_rows(pred1, pred2)
	

# ----------------------------------------------
# BY PUBLIC OR PRIVATE * AREA.HA
# ----------------------------------------------

pp_1 <- filter(pl_foc, pp_maj==1)
pp_2 <- filter(pl_foc, pp_maj==2)
# ~~

library(MASS)
devppB <-glm.nb(formula=area.ha~minDevDis, data=pp_1)
devppP <-glm.nb(formula=area.ha~minDevDis, data=pp_2)


#QC:
par(mfrow=c(2,2))
plot(devppB); plot(devppP)
summary(devppB); summary(devppP)


DF1<-as.data.frame(cbind(pp_1$minDevDis, pp_1$pp_maj))
colnames(DF1)<-c("minDevDis", "pp")
DF1$pp_maj <- "Public"
	
inv1 <- exp(as.data.frame(predict(devppB, se.fit=TRUE)))/1+exp(as.data.frame(predict(devppB, se.fit=TRUE)))
inv1$lwr<-inv1$fit-(1.96*inv1$se.fit)
inv1$upr<-inv1$fit+(1.96*inv1$se.fit)
pred1<-cbind(DF1, inv1)


DF2<-as.data.frame(cbind(pp_2$minDevDis, pp_2$pp_maj))
colnames(DF2)<-c("minDevDis", "pp")
DF2$pp_maj <- "Private"

inv2 <- exp(as.data.frame(predict(devppP, se.fit=TRUE)))/1+exp(as.data.frame(predict(devppP, se.fit=TRUE)))
inv2$lwr<-inv2$fit-(1.96*inv2$se.fit)
inv2$upr<-inv2$fit+(1.96*inv2$se.fit)
pred2<-cbind(DF2, inv2)

pred12 <- bind_rows(pred1, pred2)
}





# ----------------------------------------------
# ----------------------------------------------
# OTHER
# ----------------------------------------------
# ----------------------------------------------
{

range(cum_Ar1$running_total)
range(cum_Ar23$running_total)
range(cum_CAr1$running_total)
range(cum_CAr23$running_total)

range(cum_ArOwn1$running_total)
range(cum_ArOwn23$running_total)

range(cum_NL1$running_total)
range(cum_NL23$running_total)

range(cum_GAP1$running_total)
range(cum_GAP23$running_total)

# After 1985:
cum_Ar1_85   <-filter(cum_Ar1    , estYr>=1985)
cum_Ar23_85  <-filter(cum_Ar23 , estYr>=1985)
cum_CAr1_85  <-filter(cum_CAr1    , estYr>=1985)
cum_CAr23_85 <-filter(cum_CAr23   , estYr>=1985)

cum_ArOwn1_85    <-filter(cum_ArOwn1    , estYr>=1985)
cum_ArOwn23_85   <-filter(cum_ArOwn23   , estYr>=1985)

# Get Max/min percent change and difference in area over time (prelim calcs- percent change and difference done in excel and then saved as tab-dlim txt. )

cum_TArER_EDIT    <-read.table(paste0(Output_Folder,"cum_TArER_EDIT.txt"), sep="\t", header=TRUE)
cum_ArOwn123_EDIT <-read.table(paste0(Output_Folder,"cum_ArOwn123_EDIT.txt"), sep="\t", header=TRUE)
cum_NL123_EDIT    <-read.table(paste0(Output_Folder,"cum_NL123_EDIT.txt"), sep="\t", header=TRUE)
cum_GAP123_EDIT   <-read.table(paste0(Output_Folder,"cum_GAP123_EDIT.txt"), sep="\t", header=TRUE)



# ----------------------------------------------
# CALC MAX
# ----------------------------------------------
{
# remove nas
cum_TArER_EDIT<-na.omit(cum_TArER_EDIT)

tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 1& cum_TArER_EDIT$Type == "area", ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 1& cum_TArER_EDIT$Type == "core", ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 23& cum_TArER_EDIT$Type == "area", ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 23& cum_TArER_EDIT$Type == "core", ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]


# remove nas
cum_ArOwn123_EDIT<-na.omit(cum_ArOwn123_EDIT)

tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 1, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 2, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 3, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 4, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]

tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 1, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 2, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 3, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 4, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]


# remove nas
cum_NL123_EDIT<-na.omit(cum_NL123_EDIT)

tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 1& cum_NL123_EDIT$Class == 5, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 1& cum_NL123_EDIT$Class == 6, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 1& cum_NL123_EDIT$Class == 7, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]


tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 23& cum_NL123_EDIT$Class == 5, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 23& cum_NL123_EDIT$Class == 6, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 23& cum_NL123_EDIT$Class == 7, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             


# remove nas
cum_GAP123_EDIT<-na.omit(cum_GAP123_EDIT)

tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 1, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 2, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 3, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 4, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]

tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 1, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 2, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 3, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]             
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 4, ]
tmp[ tmp$PercInc == max( tmp$PercInc ) , ]

}

# ----------------------------------------------
# CALC MIN
# ----------------------------------------------
{
# remove nas
cum_TArER_EDIT<-na.omit(cum_TArER_EDIT)

tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 1& cum_TArER_EDIT$Type == "area", ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 1& cum_TArER_EDIT$Type == "core", ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 23& cum_TArER_EDIT$Type == "area", ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_TArER_EDIT[ cum_TArER_EDIT$er_maj == 23& cum_TArER_EDIT$Type == "core", ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]

# remove nas
cum_ArOwn123_EDIT<-na.omit(cum_ArOwn123_EDIT)

tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 1, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 2, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 3, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 1& cum_ArOwn123_EDIT$own_maj == 4, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]

tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 1, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 2, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 3, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_ArOwn123_EDIT[ cum_ArOwn123_EDIT$er_maj == 23& cum_ArOwn123_EDIT$own_maj == 4, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]


# remove nas
cum_NL123_EDIT<-na.omit(cum_NL123_EDIT)

tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 1& cum_NL123_EDIT$Class == 5, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 1& cum_NL123_EDIT$Class == 6, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 1& cum_NL123_EDIT$Class == 7, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]

tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 23& cum_NL123_EDIT$Class == 5, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 23& cum_NL123_EDIT$Class == 6, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_NL123_EDIT[ cum_NL123_EDIT$er_maj == 23& cum_NL123_EDIT$Class == 7, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             

# remove nas
cum_GAP123_EDIT<-na.omit(cum_GAP123_EDIT)

tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 1, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 2, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 3, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 1& cum_GAP123_EDIT$Class == 4, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]

tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 1, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 2, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 3, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]             
tmp <- cum_GAP123_EDIT[ cum_GAP123_EDIT$er_maj == 23& cum_GAP123_EDIT$Class == 4, ]
tmp[ tmp$PercInc == min( tmp$PercInc ) , ]


}


# ----------------------------------------------
# ALL YEARS, INC. 1985
# ----------------------------------------------
{
# ----------------------------------------------
# MAX


   estYr      sum running_total er_maj Type     Diff   PercInc
73  1870 19495.08      19955.16      1 area 19495.08 0.9769443
214  1870 15221.52       15422.4      1 core 15221.52 0.9869748
9  1936 107146.8      145699.6     23 area 107146.8 0.7353955
144  1917 19582.56      21581.64     23 core 19582.56 0.9073713

  own_maj estYr      sum running_total er_maj     Diff   PercInc
3       1  1916 29130.84      30054.24      1 29130.84 0.9692755
57       2  1954 6612.84       6729.48      1 6612.84 0.9826673
95       3  1991 346.68        635.04      1 346.68 0.5459184
110       4  1951 69792.84      94585.32      1 69792.84 0.7378824
125       1  1936 107069        124983     23 107069 0.8566688
179       2  1973 395.28         777.6     23 395.28 0.5083333
224       3  2005 508.68       1027.08     23 508.68 0.4952681
230       4  1951 842.4      21558.96     23 842.4 0.03907424

  Class estYr    sum running_total er_maj   Diff   PercInc
8     5  1996 7387.2      11252.52      1 7387.2 0.6564929
36     6  1997 58.32        142.56      1 58.32 0.4090909
60     7  1992 12.96          32.4      1 12.96     0.4
85     5  1986 414.72         502.2     23 414.72 0.8258065
117     6  1988 492.48        829.44     23 492.48 0.59375
145     7  1986 97.2        126.36     23 97.2 0.7692308

  Class estYr   sum running_total er_maj  Diff PercInc
1     1  1986 307.8         307.8      1 307.8       1
30     2  1986 615.6         615.6      1 615.6       1
62     3  1990 1023.84        1312.2      1 1023.84 0.7802469
88     4  1991 346.68        346.68      1 346.68       1
113     1  1986 123.12        123.12     23 123.12       1
143     2  1986 110.16        110.16     23 110.16       1
175     3  1988 3243.24       6823.44     23 3243.24 0.4753086
203     4  1998 570.24        570.24     23 570.24       1

# ----------------------------------------------
# MIN

    estYr   sum running_total er_maj Type  Diff     PercInc
109  1979 90.72       1618791      1 area 90.72 5.60418e-05
253  1985 9.72       1268272      1 core 9.72 7.66397e-06
71  2015 42.12        362812     23 area 42.12 0.000116093
212  2015 3.24      186471.7     23 core 3.24 1.73753e-05


   own_maj estYr   sum running_total er_maj  Diff     PercInc
39       1  1995 58.32       1522152      1 58.32 3.83142e-05
92       2  2015 304.56      65473.92      1 304.56 0.004651623
104       3  2012 42.12       2190.24      1 42.12 0.01923077
112       4  2001 45.36      94701.96      1 45.36 0.000478976
157       1  1994 48.6      165832.9     23 48.6 0.000293066
221       2  2015 42.12      153786.6     23 42.12 0.000273886
227       3  2010 68.04       1412.64     23 68.04 0.04816514
229       4  1936 77.76      20716.56     23 77.76 0.003753519


   Class estYr    sum running_total er_maj   Diff     PercInc
27     5  2015 187.92       34635.6      1 187.92 0.005425631
29     6  1989   0         29.16      1    0       0
32     6  1993   0         45.36      1    0       0
34     6  1995   0         68.04      1    0       0
51     6  2012   0       1723.68      1    0       0
53     6  2014   0       1765.80      1    0       0
54     6  2015   0       1765.80      1    0       0
56     7  1986   0         16.20      1    0       0
58     7  1990   0         19.44      1    0       0
59     7  1991   0         19.44      1    0       0
61     7  1993   0         32.40      1    0       0
62     7  1994   0         32.40      1    0       0
63     7  1995   0         32.40      1    0       0
64     7  1996   0         32.40      1    0       0
65     7  1997   0         32.40      1    0       0
66     7  1998   0         32.40      1    0       0
67     7  1999   0         32.40      1    0       0
68     7  2000   0         32.40      1    0       0
71     7  2003   0         55.08      1    0       0
72     7  2004   0         55.08      1    0       0
73     7  2005   0         55.08      1    0       0
74     7  2006   0         55.08      1    0       0
78     7  2010   0        100.44      1    0       0
80     7  2012   0        106.92      1    0       0
81     7  2013   0        106.92      1    0       0
83     7  2015   0        110.16      1    0       0
111     5  2012 505.44      30025.08     23 505.44 0.01683393
141     6  2012 255.96      19184.04     23 255.96 0.01334234
172     7  2014 9.72       3120.12     23 9.72 0.003115265

   Class estYr sum running_total er_maj Diff PercInc
6      1  1992   0       1017.36      1    0       0
11     1  1997   0      10662.84      1    0       0
25     1  2011   0      42184.80      1    0       0
28     1  2014   0      43011.00      1    0       0
29     1  2015   0      43011.00      1    0       0
31     2  1988   0        615.60      1    0       0
32     2  1989   0        615.60      1    0       0
33     2  1990   0        615.60      1    0       0
34     2  1991   0        615.60      1    0       0
37     2  1994   0        803.52      1    0       0
38     2  1995   0        803.52      1    0       0
39     2  1996   0        803.52      1    0       0
42     2  1999   0      13711.68      1    0       0
45     2  2002   0      22245.84      1    0       0
57     2  2014   0      35970.48      1    0       0
58     2  2015   0      35970.48      1    0       0
59     3  1986   0        181.44      1    0       0
60     3  1988   0        181.44      1    0       0
89      4  1992   0        346.68      1    0       0
90      4  1993   0        346.68      1    0       0
92      4  1995   0       1270.08      1    0       0
94      4  1997   0       1730.16      1    0       0
95      4  1998   0       1730.16      1    0       0
96      4  1999   0       1730.16      1    0       0
98      4  2001   0       1733.40      1    0       0
99      4  2002   0       1733.40      1    0       0
100     4  2003   0       1733.40      1    0       0
101     4  2004   0       1733.40      1    0       0
104     4  2007   0       1966.68      1    0       0
106     4  2009   0       2523.96      1    0       0
107     4  2010   0       2523.96      1    0       0
108     4  2011   0       2523.96      1    0       0
109     4  2012   0       2523.96      1    0       0
110     4  2013   0       2523.96      1    0       0
111     4  2014   0       2523.96      1    0       0
112     4  2015   0       2523.96      1    0       0
114     1  1987   0        123.12     23    0       0
115     1  1988   0        123.12     23    0       0
116     1  1989   0        123.12     23    0       0
117     1  1990   0        123.12     23    0       0
118     1  1991   0        123.12     23    0       0
119     1  1992   0        123.12     23    0       0
120     1  1993   0        123.12     23    0       0
121     1  1994   0        123.12     23    0       0
122     1  1995   0        123.12     23    0       0
123     1  1996   0        123.12     23    0       0
124     1  1997   0        123.12     23    0       0
125     1  1998   0        123.12     23    0       0
129     1  2002   0       2319.84     23    0       0
142     1  2015   0      16080.12     23    0       0
145     2  1988   0       1594.08     23    0       0
147     2  1990   0       1730.16     23    0       0
148     2  1991   0       1730.16     23    0       0
149     2  1992   0       1730.16     23    0       0
150     2  1993   0       1730.16     23    0       0
151     2  1994   0       1730.16     23    0       0
152     2  1995   0       1730.16     23    0       0
153     2  1996   0       1730.16     23    0       0
154     2  1997   0       1730.16     23    0       0
156     2  1999   0       1895.40     23    0       0
157     2  2000   0       1895.40     23    0       0
159     2  2002   0       2323.08     23    0       0
164     2  2007   0       5864.40     23    0       0
171     2  2014   0       8177.76     23    0       0
172     2  2015   0       8177.76     23    0       0
202     3  2015 42.12      145446.8     23 42.12 0.00028959
204     4  1999   0        570.24     23    0       0
205     4  2000   0        570.24     23    0       0
206     4  2001   0        570.24     23    0       0
207     4  2002   0        570.24     23    0       0
208     4  2003   0        570.24     23    0       0
209     4  2004   0        570.24     23    0       0
210     4  2005   0        570.24     23    0       0
211     4  2006   0        570.24     23    0       0
212     4  2007   0        570.24     23    0       0
213     4  2008   0        570.24     23    0       0
214     4  2009   0        570.24     23    0       0
215     4  2010   0        570.24     23    0       0
216     4  2011   0        570.24     23    0       0
217     4  2012   0        570.24     23    0       0
218     4  2013   0        570.24     23    0       0
219     4  2014   0        570.24     23    0       0
220     4  2015   0        570.24     23    0       0

}



# ----------------------------------------------
# NEW YEARS, >= 1985
# ----------------------------------------------
{


# After 1985:
cum_TArER_EDIT    <-filter(cum_TArER_EDIT    , estYr>=1985)
cum_ArOwn123_EDIT <-filter(cum_ArOwn123_EDIT , estYr>=1985)
cum_NL123_EDIT    <-filter(cum_NL123_EDIT    , estYr>=1985)
cum_GAP123_EDIT   <-filter(cum_GAP123_EDIT   , estYr>=1985)


# ----------------------------------------------
# MAX

   estYr      sum running_total er_maj Type     Diff    PercInc
44  1998 23347.44       1678508      1 area 23347.44 0.01390964
105  1998 14667.48       1303954      1 core 14667.48 0.01124846
23  2007 16177.32      312209.6     23 area 16177.32 0.05181557
84  2007 5799.6      171366.8     23 core 5799.6 0.03384319

   own_maj estYr      sum running_total er_maj     Diff    PercInc
10       1  1998 21840.84       1548697      1 21840.84 0.01410272
32       2  1996 7542.72      28583.28      1 7542.72 0.2638857
53       3  1991 346.68        635.04      1 346.68 0.5459184
65       4  2004 586.44      95356.44      1 586.44 0.006149978
84       1  2007 4270.32      178089.8     23 4270.32 0.02397846
95       2  1989 4296.24      17826.48     23 4296.24 0.2410033
123       3  2005 508.68       1027.08     23 508.68 0.4952681
127       4  2000 605.88      22164.84     23 605.88 0.02733519

  Class estYr    sum running_total er_maj   Diff   PercInc
7     5  1996 7387.2      11252.52      1 7387.2 0.6564929
34     6  1997 58.32        142.56      1 58.32 0.4090909
57     7  1992 12.96          32.4      1 12.96     0.4
81     5  1986 414.72         502.2     23 414.72 0.8258065
112     6  1988 492.48        829.44     23 492.48 0.59375
139     7  1986 97.2        126.36     23 97.2 0.7692308

  Class estYr   sum running_total er_maj  Diff PercInc
1     1  1986 307.8         307.8      1 307.8       1
30     2  1986 615.6         615.6      1 615.6       1
62     3  1990 1023.84        1312.2      1 1023.84 0.7802469
88     4  1991 346.68        346.68      1 346.68       1
113     1  1986 123.12        123.12     23 123.12       1
143     2  1986 110.16        110.16     23 110.16       1
175     3  1988 3243.24       6823.44     23 3243.24 0.4753086
203     4  1998 570.24        570.24     23 570.24       1


# ----------------------------------------------
# MIN

   estYr    sum running_total er_maj Type   Diff     PercInc
35  1989 149.04       1622035      1 area 149.04 9.18846e-05
93  1985 9.72       1268272      1 core 9.72 7.66397e-06
31  2015 42.12        362812     23 area 42.12 0.000116093
92  2015 3.24      186471.7     23 core 3.24 1.73753e-05

  own_maj estYr   sum running_total er_maj  Diff     PercInc
7       1  1995 58.32       1522152      1 58.32 3.83142e-05
51       2  2015 304.56      65473.92      1 304.56 0.004651623
62       3  2012 42.12       2190.24      1 42.12 0.01923077
63       4  2001 45.36      94701.96      1 45.36 0.000478976
74       1  1994 48.6      165832.9     23 48.6 0.000293066
121       2  2015 42.12      153786.6     23 42.12 0.000273886
126       3  2010 68.04       1412.64     23 68.04 0.04816514
135       4  2010 100.44      23946.84     23 100.44 0.00419429

   Class estYr    sum running_total er_maj   Diff     PercInc
26     5  2015 187.92       34635.6      1 187.92 0.005425631
27     6  1989   0         29.16      1    0       0
30     6  1993   0         45.36      1    0       0
32     6  1995   0         68.04      1    0       0
49     6  2012   0       1723.68      1    0       0
51     6  2014   0       1765.80      1    0       0
52     6  2015   0       1765.80      1    0       0
53     7  1986   0         16.20      1    0       0
55     7  1990   0         19.44      1    0       0
56     7  1991   0         19.44      1    0       0
58     7  1993   0         32.40      1    0       0
59     7  1994   0         32.40      1    0       0
60     7  1995   0         32.40      1    0       0
61     7  1996   0         32.40      1    0       0
62     7  1997   0         32.40      1    0       0
63     7  1998   0         32.40      1    0       0
64     7  1999   0         32.40      1    0       0
65     7  2000   0         32.40      1    0       0
68     7  2003   0         55.08      1    0       0
69     7  2004   0         55.08      1    0       0
70     7  2005   0         55.08      1    0       0
71     7  2006   0         55.08      1    0       0
75     7  2010   0        100.44      1    0       0
77     7  2012   0        106.92      1    0       0
78     7  2013   0        106.92      1    0       0
80     7  2015   0        110.16      1    0       0
107     5  2012 505.44      30025.08     23 505.44 0.01683393
136     6  2012 255.96      19184.04     23 255.96 0.01334234
166     7  2014 9.72       3120.12     23 9.72 0.003115265


   Class estYr sum running_total er_maj Diff PercInc
6      1  1992   0       1017.36      1    0       0
11     1  1997   0      10662.84      1    0       0
25     1  2011   0      42184.80      1    0       0
28     1  2014   0      43011.00      1    0       0
29     1  2015   0      43011.00      1    0       0
31     2  1988   0        615.60      1    0       0
32     2  1989   0        615.60      1    0       0
33     2  1990   0        615.60      1    0       0
34     2  1991   0        615.60      1    0       0
37     2  1994   0        803.52      1    0       0
38     2  1995   0        803.52      1    0       0
39     2  1996   0        803.52      1    0       0
42     2  1999   0      13711.68      1    0       0
45     2  2002   0      22245.84      1    0       0
57     2  2014   0      35970.48      1    0       0
58     2  2015   0      35970.48      1    0       0
59     3  1986   0        181.44      1    0       0
60     3  1988   0        181.44      1    0       0
89      4  1992   0        346.68      1    0       0
90      4  1993   0        346.68      1    0       0
92      4  1995   0       1270.08      1    0       0
94      4  1997   0       1730.16      1    0       0
95      4  1998   0       1730.16      1    0       0
96      4  1999   0       1730.16      1    0       0
98      4  2001   0       1733.40      1    0       0
99      4  2002   0       1733.40      1    0       0
100     4  2003   0       1733.40      1    0       0
101     4  2004   0       1733.40      1    0       0
104     4  2007   0       1966.68      1    0       0
106     4  2009   0       2523.96      1    0       0
107     4  2010   0       2523.96      1    0       0
108     4  2011   0       2523.96      1    0       0
109     4  2012   0       2523.96      1    0       0
110     4  2013   0       2523.96      1    0       0
111     4  2014   0       2523.96      1    0       0
112     4  2015   0       2523.96      1    0       0
114     1  1987   0        123.12     23    0       0
115     1  1988   0        123.12     23    0       0
116     1  1989   0        123.12     23    0       0
117     1  1990   0        123.12     23    0       0
118     1  1991   0        123.12     23    0       0
119     1  1992   0        123.12     23    0       0
120     1  1993   0        123.12     23    0       0
121     1  1994   0        123.12     23    0       0
122     1  1995   0        123.12     23    0       0
123     1  1996   0        123.12     23    0       0
124     1  1997   0        123.12     23    0       0
125     1  1998   0        123.12     23    0       0
129     1  2002   0       2319.84     23    0       0
142     1  2015   0      16080.12     23    0       0
145     2  1988   0       1594.08     23    0       0
147     2  1990   0       1730.16     23    0       0
148     2  1991   0       1730.16     23    0       0
149     2  1992   0       1730.16     23    0       0
150     2  1993   0       1730.16     23    0       0
151     2  1994   0       1730.16     23    0       0
152     2  1995   0       1730.16     23    0       0
153     2  1996   0       1730.16     23    0       0
154     2  1997   0       1730.16     23    0       0
156     2  1999   0       1895.40     23    0       0
157     2  2000   0       1895.40     23    0       0
159     2  2002   0       2323.08     23    0       0
164     2  2007   0       5864.40     23    0       0
171     2  2014   0       8177.76     23    0       0
172     2  2015   0       8177.76     23    0       0
202     3  2015 42.12      145446.8     23 42.12 0.00028959
204     4  1999   0        570.24     23    0       0
205     4  2000   0        570.24     23    0       0
206     4  2001   0        570.24     23    0       0
207     4  2002   0        570.24     23    0       0
208     4  2003   0        570.24     23    0       0
209     4  2004   0        570.24     23    0       0
210     4  2005   0        570.24     23    0       0
211     4  2006   0        570.24     23    0       0
212     4  2007   0        570.24     23    0       0
213     4  2008   0        570.24     23    0       0
214     4  2009   0        570.24     23    0       0
215     4  2010   0        570.24     23    0       0
216     4  2011   0        570.24     23    0       0
217     4  2012   0        570.24     23    0       0
218     4  2013   0        570.24     23    0       0
219     4  2014   0        570.24     23    0       0
220     4  2015   0        570.24     23    0       0


}

}
