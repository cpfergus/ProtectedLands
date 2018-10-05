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

pp_area<-read.table(paste0(Output_Folder,"pp_area",".txt"), sep=",", header=TRUE)
own_area<-read.table(paste0(Output_Folder,"own_area",".txt"), sep=",", header=TRUE)
gap_area<-read.table(paste0(Output_Folder,"gap_area",".txt"), sep=",", header=TRUE)
nlcd_area<-read.table(paste0(Output_Folder,"nlcd_area",".txt"), sep=",", header=TRUE)
resil_area<-read.table(paste0(Output_Folder,"resil_area",".txt"), sep=",", header=TRUE)
er_nlcd<-read.table(paste0(Output_Folder,"er_nlcd",".txt"), sep=",", header=TRUE)
er_resil<-read.table(paste0(Output_Folder,"er_resil",".txt"), sep=",", header=TRUE)
 
CS_incall<-read.table(paste0(Output_Folder,"CS_incall.txt"), sep=",", header=TRUE)# Runs from 1980 to 2015, no unknowns included



############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################




# ----------------------------------------------
# ----------------------------------------------
# DATA CLEANING
# ----------------------------------------------
# ----------------------------------------------
{
# ----------------------------------------------
# CLEAN MAJORITY AREA DATA
# ----------------------------------------------

#Filter down by categories:
majar_unk <- filter(iStats_majJoin, estYr ==9999) # unknown year
majar_notunk <-filter(iStats_majJoin, estYr<9999)# removed unknowns. left with 1800-2015 

# study area only
majar_er_unk <- filter(majar_unk, er_maj==1|er_maj==2|er_maj==3)
majar_er_unk$er_maj<-as.integer(ifelse(majar_er_unk$er_maj==1, 1, ifelse(majar_er_unk$er_maj==2|majar_er_unk$er_maj==3,23,1))) # reclassify to group piedmont into one value:
majar_er <- filter(majar_notunk, er_maj==1|er_maj==2|er_maj==3)
majar_er$er_maj<-as.integer(ifelse(majar_er$er_maj==1, 1, ifelse(majar_er$er_maj==2|majar_er$er_maj==3,23,1)))# reclassify to group piedmont into one value:

#Split by ecoregion
majar_B <- filter(majar_er, er_maj==1) # 1986-2015 
majar_P <- filter(majar_er, er_maj>1) # 1986-2015 


# WRITE TO FILE
write.table(majar_er, file = paste0(Output_Folder,"majar_er",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
majar_er<-read.table(paste0(Output_Folder,"majar_er",".txt"), sep=",", header=TRUE)


# time periods
majar_8615 <-filter(majar_er, estYr>1985) # 1986-2015 
majar_180085 <- filter(majar_er, estYr <=1985) # 1800-1985
majar_8185 <- filter(majar_er, estYr >=1981 & estYr <=1985) #1981-1985

# WRITE TO FILE
write.table(majar_8615, file = paste0(Output_Folder,"majar_8615",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
majar_8615<-read.table(paste0(Output_Folder,"majar_8615",".txt"), sep=",", header=TRUE)

str(majar_8615)
{# 'data.frame':	1517 obs. of  35 variables:
 # $ patchID          : int  2789 2790 2807 2815 2823 2901 2904 2933 2935 2943 ...
 # $ n.cell           : int  36 17 69 75 22 84 97 10 11 172 ...
 # $ n.core.cell      : int  9 1 33 32 4 30 28 1 1 77 ...
 # $ n.edges.perimeter: int  36 22 42 48 22 60 84 14 16 100 ...
 # $ n.edges.internal : int  108 46 234 252 66 276 304 26 28 588 ...
 # $ area             : int  1166400 550800 2235600 2430000 712800 2721600 3142800 324000 356400 5572800 ...
 # $ core.area        : int  291600 32400 1069200 1036800 129600 972000 907200 32400 32400 2494800 ...
 # $ perimeter        : int  6480 3960 7560 8640 3960 10800 15120 2520 2880 18000 ...
 # $ perim.area.ratio : num  0.00556 0.00719 0.00338 0.00356 0.00556 ...
 # $ shape.index      : num  1.5 1.22 1.24 1.33 1.1 ...
 # $ frac.dim.index   : num  1.06 1.04 1.03 1.04 1.02 ...
 # $ core.area.index  : num  0.25 0.0588 0.4783 0.4267 0.1818 ...
 # $ Transition       : int  1986 1986 1986 1986 1986 1986 1986 1986 1986 1986 ...
 # $ Type             : Factor w/ 2 levels "Expanded","Separated": 1 1 1 1 1 1 1 1 1 1 ...
 # $ estYr            : int  1986 1986 1986 1986 1986 1986 1986 1987 1987 1987 ...
 # $ area.ha          : num  116.6 55.1 223.6 243 71.3 ...
 # $ core.area.ha     : num  29.16 3.24 106.92 103.68 12.96 ...
 # $ pp_maj           : int  2 2 2 2 2 1 1 2 2 2 ...
 # $ pp_ha            : num  116.6 55.1 223.6 243 71.3 ...
 # $ pp_prop          : num  100 100 100 100 100 100 100 100 100 100 ...
 # $ gap_maj          : int  4 4 4 2 4 2 2 4 4 4 ...
 # $ gap_ha           : num  116.6 55.1 223.6 129.6 71.3 ...
 # $ gap_prop         : num  100 100 100 53.3 100 ...
 # $ type_maj         : int  1 1 1 2 1 2 2 1 1 1 ...
 # $ type_ha          : num  116.6 55.1 223.6 126.4 71.3 ...
 # $ type_prop        : num  100 100 100 52 100 100 100 100 100 100 ...
 # $ er_maj           : int  2 2 2 2 2 1 1 2 2 2 ...
 # $ er_ha            : num  116.6 55.1 223.6 243 71.3 ...
 # $ er_prop          : num  100 100 100 100 100 100 100 100 100 100 ...
 # $ own_maj          : int  2 2 2 3 2 1 1 2 2 2 ...
 # $ own_ha           : num  116.6 55.1 223.6 126.4 71.3 ...
 # $ own_prop         : num  100 100 100 52 100 100 100 100 100 100 ...
 # $ nlcd_maj         : int  7 7 6 5 5 5 5 6 7 6 ...
 # $ nlcd_ha          : num  64.8 25.9 162 204.1 61.6 ...
 # $ nlcd_prop        : num  55.6 47.1 72.5 84 86.4 ...
 }

#Split by ecoregion
majar_8615_B <- filter(majar_8615, er_maj==1) # 1986-2015 
majar_8615_P <- filter(majar_8615, er_maj>1) # 1986-2015 


# ----------------------------------------------
# CLEAN ACTUAL AREA DATA
# ----------------------------------------------
#Filter down by categories:
acar_unk <- filter(GapOwnNlRe_ar_join, estYr ==9999) # unknown year
acar_notunk <-filter(GapOwnNlRe_ar_join, estYr<9999)# removed unknowns. left with 1800-2015 

# study area only
acar_er_unk <- filter(acar_unk, er_maj==1|er_maj==2|er_maj==3)
acar_er_unk$er_maj<-as.integer(ifelse(acar_er_unk$er_maj==1, 1, ifelse(acar_er_unk$er_maj==2|acar_er_unk$er_maj==3,23,1))) # reclassify to group piedmont into one value:
acar_er <- filter(acar_notunk, er_maj==1|er_maj==2|er_maj==3)
acar_er$er_maj<-as.integer(ifelse(acar_er$er_maj==1, 1, ifelse(acar_er$er_maj==2|acar_er$er_maj==3,23,1)))# reclassify to group piedmont into one value:

# WRITE TO FILE
write.table(acar_er, file = paste0(Output_Folder,"acar_er",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
acar_er<-read.table(paste0(Output_Folder,"acar_er",".txt"), sep=",", header=TRUE)

# All data (except unknowns) split by ecoregion
acar_B <-filter(acar_er, er_maj==1) # 1986-2015 
acar_P <-filter(acar_er, er_maj>1) # 1986-2015 


# time periods
acar_8615 <-filter(acar_er, estYr>1985) # 1986-2015 
acar_180085 <- filter(acar_er, estYr <=1985) # 1800-1985
acar_8185 <- filter(acar_er, estYr >=1981 & estYr <=1985) #1981-1985

acar_8615_B <-filter(acar_8615, er_maj==1) # 1986-2015 
acar_8615_P <-filter(acar_8615, er_maj>1) # 1986-2015 


# WRITE TO FILE
write.table(acar_8615, file = paste0(Output_Folder,"acar_8615",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
acar_8615<-read.table(paste0(Output_Folder,"acar_8615",".txt"), sep=",", header=TRUE)

str(acar_8615)
# 'data.frame':	48544 obs. of  18 variables:
 # $ patchID      : int  2789 2789 2789 2789 2789 2789 2789 2789 2789 2789 ...
 # $ estYr        : int  1986 1986 1986 1986 1986 1986 1986 1986 1986 1986 ...
 # $ er_maj       : int  23 23 23 23 23 23 23 23 23 23 ...
 # $ gap_count    : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_area_ha  : num  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_count    : int  0 0 0 0 0 0 0 0 36 36 ...
 # $ own_area_ha  : num  0 0 0 0 0 ...
 # $ own_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ pp_count     : int  0 36 0 36 0 36 0 36 0 36 ...
 # $ pp_area_ha   : num  0 117 0 117 0 ...
 # $ pp_Class     : int  1 2 1 2 1 2 1 2 1 2 ...
 # $ nlcd_count   : int  0 0 0 0 12 4 20 0 0 0 ...
 # $ nlcd_area_ha : num  0 0 0 0 38.9 ...
 # $ nlcd_Class   : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_count  : int  36 0 0 0 0 0 0 0 36 0 ...
 # $ resil_area_ha: num  117 0 0 0 0 ...
 # $ resil_Class  : i# str(acar_861nt  1 2 3 4 5 6 7 8 1 2 ...


# ----------------------------------------------
# UNKNOWNS ONLY:
{
pl_unknowns <-filter(iStats_majJoin, Transition==9999)

str(pl_unknowns)
# 'data.frame':	595 obs. of  44 variables:

sum(pl_unknowns$area.ha)
# [1] 440008.2
}

}

# ----------------------------------------------
# AREA IN STUDY AREA by ECOREGION
# ----------------------------------------------

# ----------------------------------------------
TOTcum_Ar <- group_by(majar_8615) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_Ar$Type <- "area"

TOTcum_Ar <- group_by(majar_er) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha))
TOTcum_Ar$Type <- "area"


# ----------------------------------------------
# ----------------------------------------------
# OVERALL (end of line)  TOTALS
# ----------------------------------------------
# ----------------------------------------------
{

# ----------------------------------------------
# OVERALL - TOTAL # PATCHES:
# ----------------------------------------------
{
# ----------------------------------------------
# ENTIRE STUDY AREA + BUFFER:

COUNT <- group_by(iStats_majJoin) %>%
group_by(er_maj, estYr, Type) %>%
summarize(total = n()) 

unk <- filter(COUNT, estYr ==9999) # unknown year
notunk <- filter(COUNT, estYr <9999) # removed unknowns. left with 1800-2015 


# WRITE TO FILE
write.table(notunk, file = paste0(Output_Folder,"notunk",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
notunk<-read.table(paste0(Output_Folder,"notunk",".txt"), sep=",", header=TRUE)


sum(unk$total)# unknown year
# [1] 585
sum(notunk$total)# removed unknowns. left with 1800-2015 
# [1] 3570

# ----------------------------------------------
#  STUDY AREA ONLY:

COUNT_er_unk <- group_by(majar_er_unk) %>%
group_by(er_maj, estYr, Type) %>%
summarize(total = n())

COUNT_er <- group_by(majar_er) %>%
group_by(er_maj, estYr, Type) %>%
summarize(total = n()) 

# time periods
cnt8615  <- filter(COUNT_er, estYr >1985) # 1986-2015
cnt1800 <- filter(COUNT_er, estYr <=1985) # 1800-1985
cnt8185 <- filter(COUNT_er, estYr >=1981 & estYr <=1985) #1981-1985

# Adjacent patches
er8615exp <- filter(cnt8615, Type=="Expanded")
er8615sep <- filter(cnt8615, Type=="Separated")



sum(COUNT_er_unk$total)# unknown year
# [1] 233
sum(COUNT_er$total)# removed unknowns. left with 1800-2015 
# [1] 2034

# ----------------------------------------------
# BY ECOREGION- BLUE RIDGE:

sum(filter(COUNT_er_unk, er_maj==1)$total) # unknown year
# [1] 96
sum(filter(COUNT_er, er_maj==1)$total)# removed unknowns. left with 1800-2015 
# [1] 707
sum(filter(cnt8615, er_maj==1)$total)# 1986-2015
# [1] 379
sum(filter(cnt1800, er_maj==1)$total) # 1800-1985
# [1] 328
sum(filter(cnt8185, er_maj==1)$total)#1981-1985
# [1] 5

# ----------------------------------------------
# BY ECOREGION- PIEDMONT:

sum(filter(COUNT_er_unk, er_maj>1)$total) # unknown year
# [1] 137
sum(filter(COUNT_er, er_maj>1)$total)# removed unknowns. left with 1800-2015 
# [1] 1327
sum(filter(cnt8615, er_maj>1)$total)# 1986-2015
# [1] 1138
sum(filter(cnt1800, er_maj>1)$total) # 1800-1985
# [1] 189
sum(filter(cnt8185, er_maj>1)$total)#1981-1985
# [1] 37


# ----------------------------------------------
# BUFFER INCLUDED:

# bcnt85 <- filter(notunk, estYr >1986)
# bcnt1800 <- filter(notunk, estYr <1985)
# b85exp <- filter(bcnt85, Type=="Expanded")
# b85sep <- filter(bcnt85, Type=="Separated")
}


# ----------------------------------------------
# OVERALL - TOTAL FINAL AREA OF PROTECTED LANDS
# ----------------------------------------------
{

# ----------------------------------------------
# ENTIRE STUDY AREA + BUFFER:

Tot_Area_PLunk <- group_by(majar_unk) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha), avg=mean(area.ha), sd=sd(area.ha))

Tot_Area_PLnotunk <- group_by(majar_notunk) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha), avg=mean(area.ha), sd=sd(area.ha))

sum(Tot_Area_PLunk$sum)# unknown year
# [1] 270478.4
sum(Tot_Area_PLnotunk$sum)# removed unknowns. left with 1800-2015 
# [1] 2766341


# ----------------------------------------------
#  STUDY AREA ONLY:

Tot_Area_PL_erunk <- group_by(majar_er_unk) %>%
group_by(er_maj, estYr) %>%
summarize(sum = sum(area.ha), avg=mean(area.ha), sd=sd(area.ha))

Tot_Area_PL_er <- group_by(majar_er) %>%
group_by(er_maj, estYr) %>%
summarize(sum = sum(area.ha))

Tot_Area_PL_erSD <- group_by(majar_er) %>%
group_by(er_maj) %>%
summarize(sum = sum(area.ha), avg=mean(area.ha), sd=sd(area.ha))

# # time periods
TotAr_PL_8615  <- filter(Tot_Area_PL_er, estYr >1985) # 1986-2015
TotAr_PL_1800 <- filter(Tot_Area_PL_er, estYr <=1985) # 1800-1985
TotAr_PL_8185 <- filter(Tot_Area_PL_er, estYr >=1981 & estYr <=1985) #1981-1985

sum(Tot_Area_PL_erunk$sum)# unknown year
# [1] 110639.5
sum(Tot_Area_PL_er$sum)# removed unknowns. left with 1800-2015 
# [1] 2166918

# ----------------------------------------------
# BY ECOREGION- BLUE RIDGE:

sum(filter(TotAr_PL_8615, er_maj==1)$sum)# 1986-2015
# [1] 115447.7
sum(filter(TotAr_PL_1800, er_maj==1)$sum) # 1800-1985
# [1] 1663244
sum(filter(TotAr_PL_8185, er_maj==1)$sum)#1981-1985
# [1] 651.24

# ----------------------------------------------
# BY ECOREGION- PIEDMONT:

sum(filter(TotAr_PL_8615, er_maj>1)$sum)# 1986-2015
# [1] 168729.5
sum(filter(TotAr_PL_1800, er_maj>1)$sum) # 1800-1985
# [1] 219497
sum(filter(TotAr_PL_8185, er_maj>1)$sum)#1981-1985
# [1] 5371.92


}


# ----------------------------------------------
# OVERALL - TOTAL FINAL AREA IN * ACTUAL* OWNERSHIP/MANAGEMENT. Thus, it is not the full area.
# Save and open in excel for pivot table
# ----------------------------------------------

TOTcum_ArOM <- group_by(acar_er) %>%
group_by(estYr, er_maj, pp_Class, gap_Class, own_Class, nlcd_Class, resil_Class) %>%
summarize(pp_s = sum(pp_area_ha), own_s = sum(own_area_ha), gap_s = sum(gap_area_ha), nlcd_s = sum(nlcd_area_ha), resil_s = sum(resil_area_ha))

# WRITE TO FILE
write.table(TOTcum_ArOM, file = paste0(Output_Folder,"TOTcum_ArOM",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
TOTcum_ArOM<-read.table(paste0(Output_Folder,"TOTcum_ArOM",".txt"), sep=",", header=TRUE)

 # str(TOTcum_ArOM)
# 'data.frame':	4576 obs. of  12 variables:
 # $ estYr      : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ er_maj     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ pp_Class   : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_Class  : int  1 1 1 1 2 2 2 2 3 3 ...
 # $ own_Class  : int  1 1 1 1 2 2 2 2 3 3 ...
 # $ nlcd_Class : int  1 3 5 7 1 3 5 7 1 3 ...
 # $ resil_Class: int  1 3 5 7 1 3 5 7 1 3 ...
 # $ pp_s       : num  489 489 489 489 489 ...
 # $ own_s      : num  489 489 489 489 0 ...
 # $ gap_s      : num  0 0 0 0 0 ...
 # $ nlcd_s     : num  0 0 486 0 0 0 486 0 0 0 ...
 # $ resil_s    : num  42.1 447.1 0 0 42.1 ...


# # ----------------------------------------------
# # CLIMATE CHANGE AND CONNECTIVITY - RESILIENCE CLASS
# # ----------------------------------------------
# {
# # REmove Unkowns
# resil_un <-filter(resil_all, Transition<9999)

# # Remove buffer areas
# resil_foc <- filter(resil_un, er_maj==1|er_maj==2|er_maj==3)#2065

# # from 1800 onward (since there are not patches with core area before or on 1800)
# resil_1800 <-filter(resil_foc, estYr>1800)
# # from 1985 only # Focus on 1985 onward focus is on private conservation efforts
# resil_new <-filter(resil_foc, estYr>=1985)#1536

# #Split by ecoregion
# resil_foc_e1 <- filter(resil_foc, er_maj==1)#385
# resil_foc_e23 <- filter(resil_foc, er_maj>=2)#1151 out of

# # ----------------------------------------------
# # TOTAL AREA BY RESILIENCE SCORE- NOT USING MAJ, FROM 1800

# RESIL1 <- group_by(resil_foc_e1) %>%
# group_by(Class) %>%
# summarize(sum = sum(resil_area.ha))
# RESIL1$er_maj <- 1

# RESIL23 <- group_by(resil_foc_e23) %>%
# group_by(Class) %>%
# summarize(sum = sum(resil_area.ha))
# RESIL23$er_maj <- 23

# RESIL <- bind_rows(RESIL1, RESIL23)

# # add new, reclassified columns
# RESIL$sub1 <- ifelse(RESIL$Class == 1, "Non-Resilient", ifelse(RESIL$Class >=2&RESIL$Class <=6, "Resilient", ifelse(RESIL$Class >=7, "Vulnerable",RESIL$Class)))

# RESIL$sub2 <- ifelse(RESIL$Class == 1, "", ifelse(RESIL$Class == 2, "Not Prioritized", ifelse(RESIL$Class == 3, "Diversity", ifelse(RESIL$Class == 4, "Conc flow/Riparian", ifelse(RESIL$Class == 5, "Diversity & Conc flow/Riparian", ifelse(RESIL$Class == 6, "Diffuse flow", ifelse(RESIL$Class == 7, "Linkage", ifelse(RESIL$Class == 8, "Linkage", RESIL$Class))))))))

# #filter again
# RESIL1 <- filter(RESIL, er_maj==1)#385
# RESIL23 <- filter(RESIL, er_maj>=2)#1151 out of


# range(RESIL$sum)
# # [1]    1626.48 1267637.04
# # 1266011

# # > RESIL
# # Source: local data frame [16 x 5]

   # # Class        sum er_maj          sub1                           sub2
# # 1      1  126483.12      1 Non-Resilient
# # 2      2   64511.64      1     Resilient                Not Prioritized
# # 3      3 1267637.04      1     Resilient                      Diversity
# # 4      4   21963.96      1     Resilient             Conc flow/Riparian
# # 5      5  248222.88      1     Resilient Diversity & Conc flow/Riparian
# # 6      6    4007.88      1     Resilient                   Diffuse flow
# # 7      7    1626.48      1    Vulnerable                        Linkage
# # 8      8    5857.92      1    Vulnerable                        Linkage
# # 9      1  162677.16     23 Non-Resilient
# # 10     2   67541.04     23     Resilient                Not Prioritized
# # 11     3   74934.72     23     Resilient                      Diversity
# # 12     4   13799.16     23     Resilient             Conc flow/Riparian
# # 13     5   24802.20     23     Resilient Diversity & Conc flow/Riparian
# # 14     6    5964.84     23     Resilient                   Diffuse flow
# # 15     7    3084.48     23    Vulnerable                        Linkage
# # 16     8    2527.20     23    Vulnerable                        Linkage

# }
 
 
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


# Cumulative number of patches by type
Pat_cnt1 <- group_by(pl_new1) %>%
group_by(TypeID, estYr) %>%
summarize(total = n())

# Cumulative number of patches by type
Pat_cnt23 <- group_by(pl_new23) %>%
group_by(TypeID, estYr) %>%
summarize(total = n())



#Split by publc/private for easier plotting
Pat_cnt1 <- filter(Pat_cnt, er_maj==1)
Pat_cnt23 <- filter(Pat_cnt, er_maj==2|er_maj==3)


# WRITE TO FILE
write.table(Pat_cnt, file = paste0(Output_Folder,"Pat_cnt",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
Pat_cnt<-read.table(paste0(Output_Folder,"Pat_cnt.txt"), sep=",", header=TRUE)


}

# ----------------------------------------------
# CLASSSTATS
# ----------------------------------------------
{

CS_incall<-read.table(paste0(Output_Folder,"CS_incall.txt"), sep=",", header=TRUE)

CS_incall <-filter(CS_incall, yrInt>=1985)

CS_incB <-filter(CS_incall, er=="Blue Ridge")
CS_incP <-filter(CS_incall, er=="Piedmont")

#Select only 5,6,7, nlcd class
CS_incall_8515 <-filter(CS_incall, yrInt>=1985)


CS_inc8515B <-filter(CS_incall_8515, er=="Blue Ridge")
CS_inc8515P <-filter(CS_incall_8515, er=="Piedmont")

# core area= total.core.area.ha
}

# ----------------------------------------------
# AREA vs. CORE AREA
# ----------------------------------------------
{
# Area
cum_ArB <- group_by(majar_B) %>%
group_by( estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

cum_ArP <- group_by(majar_P) %>%
group_by( estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

cum_ArB_8515 <-filter(cum_ArB, estYr>1985)
cum_ArP_8515 <-filter(cum_ArP, estYr>1985)


}


# ----------------------------------------------
# CUMULATIVE SINCE 1985 ("Added habitat")- 
# BY MAJORITY AREA
# ----------------------------------------------
{

# Fix gap years:
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

fixyr <- filter(est_yr, estYr >1985 & estYr <9999)

# years <- unique(est_yr$estYr)
# years <- years[years>1985&years<9999]
# own_maj <- c(1,2,3,4)
# gap_maj <- c(1,2,3,4)
# er_maj  <- c(1,2,3)
# nlcd_maj<- c(1,2,3,4,5,6,7,8)

# tempA <- merge(own_maj, er_maj)
# colnames(tempA) <- c("own_maj", "er_maj")
# tempB <- merge(gap_maj, nlcd_maj)
# colnames(tempB) <- c("gap_maj", "nlcd_maj")
# tempAB <- merge(tempA, tempB)
# yrfix <- merge(years, tempAB)
# colnames(yrfix) <- c("estYr", "own_maj", "er_maj", "gap_maj", "nlcd_maj")

# u_own<-unique(yrfix[,c(2,1)])

area <- select(majar_8615, patchID, area.ha)
fix_8615ar <-full_join(acar_8615, area, by="patchID")

fix_8615ary<-full_join(fix_8615ar, fixyr)
fix_8615ary$area.ha[is.na(fix_8615ary$area.ha)] <- 0


fix_8615_1 <-filter(fix_8615ary, er_maj==1) # 1986-2015 
fix_8615_23 <-filter(fix_8615ary, er_maj>1) # 1986-2015 



# ----------------------------------------------
# OWN Area


cum_OWN <- group_by(fix_8615ary) %>%
group_by(er_maj, own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(own_area_ha)) %>%
mutate(running_total = cumsum(sum))

#Remove duplicate measures based on duplicates across (er_maj, own class, est year)
cum_OWN <- cum_OWN[!duplicated(cum_OWN[,c(1,2,6)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below
cum_OWN <- na.omit(cum_OWN)

# cum_OWN1 <- group_by(fix_8615_1) %>%
# group_by(own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(own_area_ha)) %>%
# mutate(running_total = cumsum(sum))

# #Remove duplicate measures based on duplicates across (own class, est year)
# cum_OWN1 <- cum_OWN1[!duplicated(cum_OWN1[,c(2,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

# cum_OWN23 <- group_by(fix_8615_23) %>%
# group_by(own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(own_area_ha)) %>%
# mutate(running_total = cumsum(sum))

# #Remove duplicate measures based on duplicates across (own class, est year)
# cum_OWN23 <- cum_OWN23[!duplicated(cum_OWN23[,c(2,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

# summary(cum_OWN1[,c(2,5:7)]); summary(cum_OWN23[,c(2,5:7)])



# ----------------------------------------------
# GAP Area


cum_GAP <- group_by(fix_8615ary) %>%
group_by(er_maj, own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(gap_area_ha)) %>%
mutate(running_total = cumsum(sum))

#Remove duplicate measures based on duplicates across (er_maj, own class, est year)
cum_GAP <- cum_GAP[!duplicated(cum_GAP[,c(1,3,6)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below
cum_GAP <- na.omit(cum_GAP)

# cum_GAP1 <- group_by(fix_8615_1y) %>%
# group_by(own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(gap_area_ha)) %>%
# mutate(running_total = cumsum(sum))

# #Remove duplicate measures based on duplicates across (own class, est year)
# cum_GAP1 <- cum_GAP1[!duplicated(cum_GAP1[,c(1,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

# cum_GAP23 <- group_by(fix_8615_23y) %>%
# group_by(own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
# summarize(sum = sum(gap_area_ha)) %>%
# mutate(running_total = cumsum(sum))

# #Remove duplicate measures based on duplicates across (own class, est year)
# cum_GAP23 <- cum_GAP23[!duplicated(cum_GAP23[,c(1,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

# summary(cum_GAP1[,c(1,5:7)]); summary(cum_GAP23[,c(1,5:7)])



# ----------------------------------------------
# NLCD Area

cum_NLCD <- group_by(fix_8615ary) %>%
group_by(er_maj, own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(nlcd_area_ha)) %>%
mutate(running_total = cumsum(sum))

cum_NLCD <- na.omit(cum_NLCD)
# Do only Forest, Grasses, and Crop
cum_NLCD <-filter(cum_NLCD, nlcd_Class==5|nlcd_Class== 6|nlcd_Class== 7)

#Remove duplicate measures based on duplicates across (er_maj, nlcd class, est year)
cum_NLCD <- cum_NLCD[!duplicated(cum_NLCD[,c(1,4,6)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below


cum_NLCD1 <- group_by(fix_8615_1y) %>%
group_by(own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(nlcd_area_ha)) %>%
mutate(running_total = cumsum(sum))

# Do only Forest, Grasses, and Crop
cum_NLCD1 <-filter(cum_NLCD1, nlcd_Class==5|nlcd_Class== 6|nlcd_Class== 7)

#Remove duplicate measures based on duplicates across (own class, est year)
cum_NLCD1 <- cum_NLCD1[!duplicated(cum_NLCD1[,c(3,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

cum_NLCD23 <- group_by(fix_8615_23y) %>%
group_by(own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
summarize(sum = sum(nlcd_area_ha)) %>%
mutate(running_total = cumsum(sum))

# Do only Forest, Grasses, and Crop
cum_NLCD23 <-filter(cum_NLCD23, nlcd_Class==5|nlcd_Class== 6|nlcd_Class== 7)

#Remove duplicate measures based on duplicates across (own class, est year)
cum_NLCD23 <- cum_NLCD23[!duplicated(cum_NLCD23[,c(3,5)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

summary(cum_NLCD1[,c(3,5:7)]); summary(cum_NLCD23[,c(3,5:7)])




# ----------------------------------------------
# RESIL Area
# The resilience category should be only the final amount, not cumulative across time. So, used the one above.

# ----------------------------------------------
# ----------------------------------------------
# > head(cum_ArOWN)
# Source: local data frame [6 x 7]
# Groups: gap_Class, own_Class, nlcd_Class, resil_Class

  # gap_Class own_Class nlcd_Class resil_Class estYr     sum running_total
# 1         1         1          1           1  1986 1046.52       1046.52
# 2         1         1          1           1  1987 1522.80       2569.32
# 3         1         1          1           1  1988    0.00       2569.32
# 4         1         1          1           1  1989  149.04       2718.36
# 5         1         1          1           1  1990  110.16       2828.52
# 6         1         1          1           1  1991  424.44       3252.96
# > head(cum_ArGAP)
# Source: local data frame [6 x 7]
# Groups: gap_Class, own_Class, nlcd_Class, resil_Class

  # gap_Class own_Class nlcd_Class resil_Class estYr    sum running_total
# 1         1         1          1           1  1986   0.00          0.00
# 2         1         1          1           1  1987   0.00          0.00
# 3         1         1          1           1  1988   0.00          0.00
# 4         1         1          1           1  1989   0.00          0.00
# 5         1         1          1           1  1990   0.00          0.00
# 6         1         1          1           1  1991 333.72        333.72
# > head(cum_ArNLCD)
# Source: local data frame [6 x 7]
# Groups: gap_Class, own_Class, nlcd_Class, resil_Class

  # gap_Class own_Class nlcd_Class resil_Class estYr     sum running_total
# 1         1         1          5           5  1986 1668.60       1668.60
# 2         1         1          5           5  1987 1632.96       3301.56
# 3         1         1          5           5  1988 1198.80       4500.36
# 4         1         1          5           5  1989 2057.40       6557.76
# 5         1         1          5           5  1990 2890.08       9447.84
# 6         1         1          5           5  1991 1383.48      10831.32




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
