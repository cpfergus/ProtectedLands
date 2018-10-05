############################
#PURPOSE: Calculating summary stats 
#INPUT: 
#OUTPUT: 
#DEVELOPED: 10/31/16, 11/8/16 - Iara Lacher
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT:
# Need to run the multiplot function. This is in the one note notes.

# For OVERALL - TOTAL FINAL AREA IN * ACTUAL* OWNERSHIP/MANAGEMENT/NLCD/RESIL, ***You MUST include all the variables in order for calculations to ot contain duplicates, unless you split the data first.

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
 library(dplyr)
 library(stringr)

# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")




# ----------------------------------------------
# READ INPUT FILES:

Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/" #Y:Drive
# Output_Folder <- "C:/Users/LacherL/Documents/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" # C:Drive

# Patch Stats(with core only)
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")

iStats_majJoin<-read.table(paste0(Output_Folder,"iStats_majJoin",".txt"), sep=",", header=TRUE) # no core area already removed

GapOwnNlRe_ar_join<-read.table(paste0(Output_Folder,"GapOwnNlRe_ar_join",".txt"), sep=",", header=TRUE) # no core area already removed

maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

gap_area<-read.table(paste0(Output_Folder,"gap_area",".txt"), sep=",", header=TRUE)
own_area<-read.table(paste0(Output_Folder,"own_area",".txt"), sep=",", header=TRUE)
nlcd_area<-read.table(paste0(Output_Folder,"nlcd_area",".txt"), sep=",", header=TRUE)
resil_area<-read.table(paste0(Output_Folder,"resil_area",".txt"), sep=",", header=TRUE)

er_resil<-read.table(paste0(Output_Folder,"er_resil",".txt"), sep=",", header=TRUE)
 
# READ FROM FILE
CS_nlcdall<-read.table(paste0(Output_Folder,"CS_nlcdall.txt"), sep=",", header=TRUE)



############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# DATA CLEANING
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# CLEAN MAJORITY DATA

pl_un <-filter(iStats_majJoin, Transition<9999)

# Remove buffer areas
pl_foc <- filter(pl_un, er_maj==1|er_maj==2|er_maj==3)

# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
# pl_new <-filter(pl_foc, estYr>=1985)
pl_new <-filter(pl_foc, estYr>1985)# 'data.frame':	#1517 obs. of  33 variables

# #Split by ecoregion
# pl_foc_e1 <- filter(pl_foc, er_maj==1)#385
# pl_foc_e23 <- filter(pl_foc, er_maj>=2)#1151

# #Split by type
# pl_foc_t1 <- filter(pl_foc, type_maj==1)#1507
# pl_foc_t2 <- filter(pl_foc, type_maj==2)#512

# ----------------------------------------------
# CLEAN ACTUAL AREA DATA


# REmove Unkowns
acar <-filter(GapOwnNlRe_ar_join, estYr<9999)

# Remove buffer areas
acar_foc <- filter(acar, er_maj==1|er_maj==2|er_maj==3)#2065

# > str(acar_foc)
# 'data.frame':	64768 obs. of  15 variables:
 # $ patchID      : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ estYr        : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ er_maj       : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_count    : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_area_ha  : num  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_count    : int  151 151 151 151 151 151 151 151 0 0 ...
 # $ own_area_ha  : num  489 489 489 489 489 ...
 # $ own_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ nlcd_count   : int  0 1 0 0 150 0 0 0 0 1 ...
 # $ nlcd_area_ha : num  0 3.24 0 0 486 0 0 0 0 3.24 ...
 # $ nlcd_Class   : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_count  : int  13 0 138 0 0 0 0 0 13 0 ...
 # $ resil_area_ha: num  42.1 0 447.1 0 0 ...
 # $ resil_Class  : int  1 2 3 4 5 6 7 8 1 2 ...

# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
acar_new <-filter(acar_foc, estYr>=1985) #48800 obs. of  15 variables

# WRITE TO FILE
write.table(acar_new, file = paste0(Output_Folder,"acar_new",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
acar_new<-read.table(paste0(Output_Folder,"acar_new",".txt"), sep=",", header=TRUE)




# ----------------------------------------------
# UNKNOWNS ONLY:
{# pl_unknowns <-filter(iStats_majJoin, Transition==9999)

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

COUNT <- group_by(iStats_majJoin) %>%
group_by(estYr) %>%
summarize(total = n())

COUNT2 <- group_by(iStats_majJoin) %>%
group_by(er_maj, estYr, Type) %>%
summarize(total = n())

#Filter down by categories
unk <- filter(COUNT2, estYr ==9999)
notunk <- filter(COUNT2, estYr <9999)

# buffer
bcnt85 <- filter(notunk, estYr >1986)
bcnt1800 <- filter(notunk, estYr <1985)
b85exp <- filter(bcnt85, Type=="Expanded")
b85sep <- filter(bcnt85, Type=="Separated")

# no buffer
er <- filter(notunk, er_maj==1|er_maj==2|er_maj==3)
cnt85 <- filter(er, estYr >1986)
er85exp <- filter(cnt85, Type=="Expanded")
er85sep <- filter(cnt85, Type=="Separated")
cnt1800 <- filter(er, estYr <1985)



# ----------------------------------------------
# OVERALL - AREA IN STUDY AREA by ECOREGION
# ----------------------------------------------

# ----------------------------------------------
#Area
TOTcum_Ar <- group_by(iStats_majJoin) %>%
group_by(er_maj, estYr) %>%
summarize(sum = sum(area.ha))
TOTcum_Ar$Type <- "area"

TOTcum_Ar <- group_by(pl_foc) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_Ar$Type <- "area"

# ----------------------------------------------
# #Core Area ### CANNOT USE THIS> MUST USE OTHER MEASURE. (bc core area is cut by individualt patches... need coalesced patch estimates (Class Stats)
# TOTcum_CAr <- group_by(pl_foc) %>%
# group_by(er_maj) %>%
# summarize(sum = sum(core.area.ha))
# TOTcum_CAr$Type <- "core"





# ----------------------------------------------
# OVERALL - TOTAL FINAL AREA IN * MAJORITY* OWNERSHIP/MANAGEMENT. Thus, it is not the full area.
# ----------------------------------------------

TOTcum_ArOM <- group_by(pl_foc) %>%
group_by(er_maj, pp_maj, own_maj, gap_maj) %>%
summarize(sum = sum(area.ha))

# WRITE TO FILE
write.table(TOTcum_ArOM, file = paste0(Output_Folder,"TOTcum_ArOM",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
TOTcum_ArOM<-read.table(paste0(Output_Folder,"TOTcum_ArOM",".txt"), sep=",", header=TRUE)


# ----------------------------------------------
# OVERALL - TOTAL FINAL AREA IN * ACTUAL* OWNERSHIP/MANAGEMENT/NLCD/RESIL
# ***You MUST include all the variables in order for calculations to ot contain duplicates, unless you split the data first.
# ----------------------------------------------

# Overall actual area by public/ private

# JOIN NLCD DATA WITH PUB/PRIV:
# Select columns that matter (full join will result in massive table)
stat_maj <- select(iStats_majJoin, patchID, area.ha, core.area.ha, pp_maj, pp_ha, pp_prop)

nl_act <- select(acar_foc, patchID, estYr, nlcd_count, nlcd_area_ha, nlcd_Class)
res_act <- select(acar_foc, patchID, estYr, resil_count, resil_area_ha, resil_Class)

# Remove duplicate rows
nl_act <- distinct(nl_act)
res_act <- distinct(res_act)

# Join maj values to act values. There are NAs for estYr here, just know this.
nl_join <- full_join(stat_maj, nl_act, by="patchID")
res_act <- full_join(stat_maj, res_act, by="patchID")



# ----------------------------------------------
# NLCD Area- 

# by public/ private:
TOTcum_ArNLCDpp <- group_by(nl_join) %>%
group_by(pp_maj, nlcd_Class) %>%
summarize(sum = sum(nlcd_area_ha))

# all
TOTcum_ArNLCD <- group_by(acar_foc) %>%
group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class) %>%
summarize(sum = sum(nlcd_area_ha))

# WRITE TO FILE
write.table(TOTcum_ArNLCD, file = paste0(Output_Folder,"TOTcum_ArNLCD",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
TOTcum_ArNLCD<-read.table(paste0(Output_Folder,"TOTcum_ArNLCD",".txt"), sep=",", header=TRUE)



# ----------------------------------------------
# RESIL Area

# by public/ private:
TOTcum_ArRESILpp <- group_by(res_act) %>%
group_by(pp_maj, resil_Class) %>%
summarize(sum = sum(resil_area_ha))

# all
TOTcum_ArRESIL <- group_by(acar_foc) %>%
group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class) %>%
summarize(sum = sum(resil_area_ha))

# WRITE TO FILE
write.table(TOTcum_ArRESIL, file = paste0(Output_Folder,"TOTcum_ArRESIL",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
TOTcum_ArRESIL<-read.table(paste0(Output_Folder,"TOTcum_ArRESIL",".txt"), sep=",", header=TRUE)

# add new, reclassified columns
TOTcum_ArRESIL$sub1 <- ifelse(TOTcum_ArRESIL$resil_Class == 1, "Non-Resilient", ifelse(TOTcum_ArRESIL$resil_Class >=2&TOTcum_ArRESIL$resil_Class <=6, "Resilient", ifelse(TOTcum_ArRESIL$resil_Class >=7, "Vulnerable",TOTcum_ArRESIL$resil_Class)))

TOTcum_ArRESIL$sub2 <- ifelse(TOTcum_ArRESIL$resil_Class == 1, "", ifelse(TOTcum_ArRESIL$resil_Class == 2, "Not Prioritized", ifelse(TOTcum_ArRESIL$resil_Class == 3, "Diversity", ifelse(TOTcum_ArRESIL$resil_Class == 4, "Conc flow/Riparian", ifelse(TOTcum_ArRESIL$resil_Class == 5, "Diversity & Conc flow/Riparian", ifelse(TOTcum_ArRESIL$resil_Class == 6, "Diffuse flow", ifelse(TOTcum_ArRESIL$resil_Class == 7, "Linkage", ifelse(TOTcum_ArRESIL$resil_Class == 8, "Linkage", TOTcum_ArRESIL$resil_Class))))))))

TOTcum_ArRESIL$logsum <- log(TOTcum_ArRESIL$sum)
TOTcum_ArRESIL$invsum <- 1/TOTcum_ArRESIL$sum


# str(TOTcum_ArRESIL)
# 'data.frame':	96 obs. of  8 variables:
 # $ er_maj     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_Class  : int  2 2 2 2 2 2 2 2 1 1 ...
 # $ nlcd_Class : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_Class: int  1 2 8 6 5 7 3 4 1 2 ...
 # $ sum        : num  103217 68588 6075 4458 243693 ...
 # $ sub1       : chr  "Non-Resilient" "Resilient" "Vulnerable" "Resilient" ...
 # $ sub2       : chr  "" "Not Prioritized" "Linkage" "Diffuse flow" ...

# ----------------------------------------------
# RESIL Area by ECOREGION

er_resil_1 <- filter(er_resil, er==1)
er_resil_23 <- filter(er_resil, er==2|er==3)


# ----------------------------------------------
# OWN Area
TOTcum_ArOWN <- group_by(acar_foc) %>%
group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class) %>%
summarize(sum = sum(own_area_ha)) %>%

# WRITE TO FILE
write.table(TOTcum_ArOWN, file = paste0(Output_Folder,"TOTcum_ArOWN",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
TOTcum_ArOWN<-read.table(paste0(Output_Folder,"TOTcum_ArOWN",".txt"), sep=",", header=TRUE)

# ----------------------------------------------
# GAP Area
TOTcum_ArGAP <- group_by(acar_foc) %>%
group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class) %>%
summarize(sum = sum(gap_area_ha))

# WRITE TO FILE
write.table(TOTcum_ArGAP, file = paste0(Output_Folder,"TOTcum_ArGAP",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
TOTcum_ArGAP<-read.table(paste0(Output_Folder,"TOTcum_ArGAP",".txt"), sep=",", header=TRUE)



}


# ----------------------------------------------
# ----------------------------------------------
# ACROSS TIME (Since 1985)
# ----------------------------------------------
# ----------------------------------------------
{
# ----------------------------------------------
# NUMBER OF PATCHES:
# ----------------------------------------------
{
# Levels: Expanded noMeas Separated

# Assign Type ID column
pl_new$TypeID<-as.integer(ifelse(pl_new$Type=="Expanded", 2, ifelse(pl_new$Type=="Separated",1, ifelse(pl_new$Type=="noMeas", 0,0))))  #"Expanded is the term used in the patch stats analysis, and therefore the data. But, do not call it this in the figure!

# Cumulative number of patches by type
Pat_cnt <- group_by(pl_new) %>%
group_by(pp_maj, TypeID, estYr) %>%
summarize(total = n())


#Split by publc/private for easier plotting
Pat_cnt1 <- filter(Pat_cnt, pp_maj==1)
Pat_cnt2 <- filter(Pat_cnt, pp_maj==2)


# WRITE TO FILE
write.table(Pat_cnt, file = paste0(Output_Folder,"Pat_cnt",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
Pat_cnt<-read.table(paste0(Output_Folder,"Pat_cnt.txt"), sep=",", header=TRUE)


}

# ----------------------------------------------
# CLASSSTATS
# ----------------------------------------------
{
CS_nlcdall<-read.table(paste0(Output_Folder,"CS_nlcdall.txt"), sep=",", header=TRUE)
CS_incall<-read.table(paste0(Output_Folder,"CS_incall.txt"), sep=",", header=TRUE)

#Select only 5,6,7, nlcd class
CS_nlcd <-filter(CS_nlcdall, class==5|class== 6|class== 7)




}


# ----------------------------------------------
# CORE AREA vs. AREA
# ----------------------------------------------
{
cum_Ar <- group_by(pl_foc) %>%
group_by( estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))
# Select only years 1985 and later
cum_Ar <-filter(cum_Ar, estYr>=1985)

# ----------------------------------------------
# CORE AREA BY PUBLIC/PRIVATE??? * cOME BACK TO?
# ----------------------------------------------


}

# ----------------------------------------------
# CUMULATIVE SINCE 1985 ("Added habitat")- NOT BY MAJORITY AREA
# ----------------------------------------------
{

# ----------------------------------------------
# OWN Area
cum_ArOWN <- group_by(acar_new) %>%
group_by(gap_Class, own_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(own_area_ha)) %>%
mutate(running_total = cumsum(sum))

#Remove duplicate measures based on duplicates across (own class, est year)
cum_ArOWN <- cum_ArOWN[!duplicated(cum_ArOWN[,c(2,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME.


# # # For Table Calculations:
# cum_ArOWN <- group_by(acar_new) %>%
# group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(own_area_ha)) %>%
# mutate(running_total = cumsum(sum))


# ----------------------------------------------
# GAP Area

cum_ArGAP <- group_by(acar_new) %>%
group_by(gap_Class, own_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(gap_area_ha)) %>%
mutate(running_total = cumsum(sum))

#Remove duplicate measures based on duplicates across (gap class, est year)
cum_ArGAP <- cum_ArGAP[!duplicated(cum_ArGAP[,c(1,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME.

# # For Table Calculations:
# cum_ArGAP <- group_by(acar_new) %>%
# group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(gap_area_ha)) %>%
# mutate(running_total = cumsum(sum))



# ----------------------------------------------
# NLCD Area

# For Plotting, do not include er_maj
cum_ArNLCD <- group_by(acar_new) %>%
group_by(gap_Class, own_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(nlcd_area_ha)) %>%
mutate(running_total = cumsum(sum))

# Do only Forest, Grasses, and Crop
cum_ArNLCD <-filter(cum_ArNLCD, nlcd_Class==5|nlcd_Class== 6|nlcd_Class== 7)

#Remove duplicate measures based on duplicates across nlcd class, est year
cum_ArNLCD <- cum_ArNLCD[!duplicated(cum_ArNLCD[,c(3,6)]),]#* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME.

# # For Table Calculations:
# cum_ArNLCD <- group_by(acar_new) %>%
# group_by(er_maj, gap_Class, own_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(nlcd_area_ha)) %>%
# mutate(running_total = cumsum(sum))


# ----------------------------------------------
# RESIL Area
# The resilience category should be only the final amount, not cumulative across time. So, used the one above.






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

# add new, reclassified columns
RESIL$sub1 <- ifelse(RESIL$Class == 1, "Non-Resilient", ifelse(RESIL$Class >=2&RESIL$Class <=6, "Resilient", ifelse(RESIL$Class >=7, "Vulnerable",RESIL$Class)))

RESIL$sub2 <- ifelse(RESIL$Class == 1, "", ifelse(RESIL$Class == 2, "Not Prioritized", ifelse(RESIL$Class == 3, "Diversity", ifelse(RESIL$Class == 4, "Conc flow/Riparian", ifelse(RESIL$Class == 5, "Diversity & Conc flow/Riparian", ifelse(RESIL$Class == 6, "Diffuse flow", ifelse(RESIL$Class == 7, "Linkage", ifelse(RESIL$Class == 8, "Linkage", RESIL$Class))))))))

#filter again
RESIL1 <- filter(RESIL, er_maj==1)#385
RESIL23 <- filter(RESIL, er_maj>=2)#1151 out of


range(RESIL$sum)
# [1]    1626.48 1267637.04
# 1266011

# > RESIL
# Source: local data frame [16 x 5]

   # Class        sum er_maj          sub1                           sub2
# 1      1  126483.12      1 Non-Resilient
# 2      2   64511.64      1     Resilient                Not Prioritized
# 3      3 1267637.04      1     Resilient                      Diversity
# 4      4   21963.96      1     Resilient             Conc flow/Riparian
# 5      5  248222.88      1     Resilient Diversity & Conc flow/Riparian
# 6      6    4007.88      1     Resilient                   Diffuse flow
# 7      7    1626.48      1    Vulnerable                        Linkage
# 8      8    5857.92      1    Vulnerable                        Linkage
# 9      1  162677.16     23 Non-Resilient
# 10     2   67541.04     23     Resilient                Not Prioritized
# 11     3   74934.72     23     Resilient                      Diversity
# 12     4   13799.16     23     Resilient             Conc flow/Riparian
# 13     5   24802.20     23     Resilient Diversity & Conc flow/Riparian
# 14     6    5964.84     23     Resilient                   Diffuse flow
# 15     7    3084.48     23    Vulnerable                        Linkage
# 16     8    2527.20     23    Vulnerable                        Linkage

}
}


# ----------------------------------------------
# NLCD PATCH BUFFERS - NOT BY MAJORITY AREA
# ----------------------------------------------
{

nlcd_buff<-read.table(paste0(Output_Folder,"nlcd_buff.txt"), sep=",", header=TRUE)

# REmove Unkowns
nlbuff_un <-filter(nlcd_buff, Transition<9999)

# Remove (ecoregional) buffer areas
nlbuff_foc <- filter(nlbuff_un, er_maj==1|er_maj==2|er_maj==3)#2065

# from 1800 onward (since there are not patches with core area before or on 1800)
nlbuff_1800 <-filter(nlbuff_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
nlbuff_new <-filter(nlbuff_foc, estYr>=1985)#1536

# Do only Forest, Grasses, and Crop (1985 onward - newly added)
nlbuff_FGC <-filter(nlbuff_new, Class==5|Class== 6|Class== 7)

# nlbuff_e123 <- bind_rows(nlbuff_e1, nlbuff_e23)

# NLCD Area
TOTcum_NLBF <- group_by(nlbuff_FGC) %>%
group_by(own_maj, Class) %>%
summarize(sum = sum(nlcdbf_area_ha))

# c1<-filter(cum_NLBF1,Class==1); c2<-filter(cum_NLBF1,Class==2); c3<-filter(cum_NLBF1,Class==3); c5<-filter(cum_NLBF1,Class==5); c6<-filter(cum_NLBF1,Class==6); c7<-filter(cum_NLBF1,Class==7); c8<-filter(cum_NLBF1,Class==8)

# summary(c1)
# summary(c2)
# summary(c3)
# summary(c5)
# summary(c6)
# summary(c7)
# summary(c8)




# Select only years 1985 and later
cum_NLBF1 <-filter(cum_NLBF1, estYr>=1985)
cum_NLBF23 <-filter(cum_NLBF23, estYr>=1986)



# ----------------------------------------------
# CUMULATIVE SINCE 1985 ("Added habitat")
# ----------------------------------------------

cum_NLBF1 <- group_by(nlbuff_e1) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(nlbuff_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_NLBF1$er_maj <- 1

cum_NLBF23 <- group_by(nlbuff_e23) %>%
group_by(Class, estYr) %>%
summarize(sum = sum(nlbuff_area.ha)) %>%
mutate(running_total = cumsum(sum))
cum_NLBF23$er_maj <- 23

cum_NLBF123 <- bind_rows(cum_NLBF1, cum_NLBF23)



range(cum_NLBF1$running_total)
range(cum_NLBF23$running_total)

# WRITE TO FILE
write.table(cum_NLBF123, file = paste0(Output_Folder,"cum_NLBF123",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
cum_NLBF123<-read.table(paste0(Output_Folder,"cum_NLBF123.txt"), sep=",", header=TRUE)

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

# par(mfrow=c(2,1))
# hist(dist_n1$min_dist.km); hist(dist_n23$min_dist.km)

# par(mfrow=c(2,1))
# hist(sqrt(dist_n1$min_dist.km)); hist(sqrt(dist_n23$min_dist.km))

# par(mfrow=c(2,1))
# hist(log(dist_n1$min_dist.km)); hist(log(dist_n23$min_dist.km))

library(MASS)

# nnB <-glm(formula=sqrt(min_dist.km)~estYr, data=dist_n1)
# nnP <-glm(formula=sqrt(min_dist.km)~estYr, data=dist_n23)

nnB <-glm(min_dist.km+1~estYr, data=dist_n1, family="quasipoisson")
nnP <-glm(min_dist.km+1~estYr, data=dist_n23, family="quasipoisson")

#QC:
par(mfrow=c(2,2))
plot(nnB, main="Blue Ridge"); plot(nnP, main="Piedmont")
summary(nnB); summary(nnP)

# ----------------------------------------------
# Notes on transformations:
# The log function is one of these functions. We know that the inverse of a log function is an exponential. So, we know that the inverse of f(x) = log subb(x) is f^-1(y) = b^y. If the base is e and we are dealing with the natural log, then the inverse of f(x) = ln(x) is f^-1(y) = e^y.

DF1<-as.data.frame(cbind(dist_n1$estYr, dist_n1$er_maj))
colnames(DF1)<-c("estYr", "er")
DF1$er_maj <- "1"
# inv1 <- exp(as.data.frame(predict(nnB, se.fit=TRUE)))/1+exp(as.data.frame(predict(nnB, se.fit=TRUE)))
# inv1 <- exp(as.data.frame(predict(nnB, se.fit=TRUE)))
inv1 <- exp(as.data.frame(predict(nnB, se.fit=TRUE))-1)
inv1$lwr<-inv1$fit-(1.96*inv1$se.fit)
inv1$upr<-inv1$fit+(1.96*inv1$se.fit)
pred1<-cbind(DF1, inv1)

DF23<-as.data.frame(cbind(dist_n23$estYr, dist_n23$er_maj))
colnames(DF23)<-c("estYr", "er")
DF23$er_maj <- "23"
# inv23 <- exp(as.data.frame(predict(nnP, se.fit=TRUE)))/1+exp(as.data.frame(predict(nnP, se.fit=TRUE)))
# inv23 <- exp(as.data.frame(predict(nnP, se.fit=TRUE)))
inv1 <- exp(as.data.frame(predict(nnP, se.fit=TRUE))-1)
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
