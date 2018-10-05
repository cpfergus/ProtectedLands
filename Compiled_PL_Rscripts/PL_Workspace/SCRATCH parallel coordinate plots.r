############################ 
#PURPOSE: Create one plot to visualize the relationship between total area of public and private, management, gap status, land cover, and climate resiliency
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
 #** I COMBINED ALL THE DATA IN EXCEL AND SAVED AS ParallelCoordData.csv

##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 

# ----------------------------------------------
################################################


# PACKAGES NEEDED

# Header for what grouped packages are for


# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:


TOTcum_ArOWN<-read.table(paste0(Output_Folder,"TOTcum_ArOWN",".txt"), sep=",", header=TRUE)
TOTcum_ArGAP<-read.table(paste0(Output_Folder,"TOTcum_ArGAP",".txt"), sep=",", header=TRUE)
TOTcum_ArNLCD<-read.table(paste0(Output_Folder,"TOTcum_ArNLCD",".txt"), sep=",", header=TRUE)
TOTcum_ArRESIL<-read.table(paste0(Output_Folder,"TOTcum_ArRESIL",".txt"), sep=",", header=TRUE)

# add new, reclassified columns
TOTcum_ArRESIL$sub1 <- ifelse(TOTcum_ArRESIL$resil_Class == 1, "Non-Resilient", ifelse(TOTcum_ArRESIL$resil_Class >=2&TOTcum_ArRESIL$resil_Class <=6, "Resilient", ifelse(TOTcum_ArRESIL$resil_Class >=7, "Vulnerable",TOTcum_ArRESIL$resil_Class)))

TOTcum_ArRESIL$sub2 <- ifelse(TOTcum_ArRESIL$resil_Class == 1, "", ifelse(TOTcum_ArRESIL$resil_Class == 2, "Not Prioritized", ifelse(TOTcum_ArRESIL$resil_Class == 3, "Diversity", ifelse(TOTcum_ArRESIL$resil_Class == 4, "Conc flow/Riparian", ifelse(TOTcum_ArRESIL$resil_Class == 5, "Diversity & Conc flow/Riparian", ifelse(TOTcum_ArRESIL$resil_Class == 6, "Diffuse flow", ifelse(TOTcum_ArRESIL$resil_Class == 7, "Linkage", ifelse(TOTcum_ArRESIL$resil_Class == 8, "Linkage", TOTcum_ArRESIL$resil_Class))))))))


# > str(TOTcum_ArOWN); str(TOTcum_ArGAP); str(TOTcum_ArNLCD); str(TOTcum_ArRESIL)
# 'data.frame':	96 obs. of  6 variables:
 # $ er_maj     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_Class  : int  2 2 2 2 2 2 2 2 1 1 ...
 # $ nlcd_Class : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_Class: int  1 2 8 6 5 7 3 4 1 2 ...
 # $ sum        : num  73927 73927 73927 73927 73927 ...
# 'data.frame':	96 obs. of  6 variables:
 # $ er_maj     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ nlcd_Class : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_Class: int  1 2 3 4 5 6 7 8 1 2 ...
 # $ sum        : num  362886 362886 362886 362886 362886 ...
# 'data.frame':	96 obs. of  6 variables:
 # $ er_maj     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ nlcd_Class : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_Class: int  1 2 3 4 5 6 7 8 1 2 ...
 # $ sum        : num  4944 35928 1996 1030 1673651 ...
# 'data.frame':	96 obs. of  8 variables:
 # $ er_maj     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_Class  : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ nlcd_Class : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_Class: int  1 2 3 4 5 6 7 8 1 2 ...
 # $ sum        : num  103217 68588 1289011 22985 243693 ...
 # $ sub1       : chr  "Non-Resilient" "Resilient" "Resilient" "Resilient" ...
 # $ sub2       : chr  "" "Not Prioritized" "Diversity" "Conc flow/Riparian" ...
 
 # full_join(TOTcum_ArOWN, TOTcum_ArGAP, TOTcum_ArNLCD, TOTcum_ArRESIL)

 #** I COMBINED ALL THE DATA IN EXCEL AND SAVED AS ParallelCoordData.csv

 
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/" #Y:Drive
# Output_Folder <- "C:/Users/LacherL/Documents/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" # C:Drive

# Patch Stats(with core only)
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")

iStats_majJoin<-read.table(paste0(Output_Folder,"iStats_majJoin",".txt"), sep=",", header=TRUE) # no core area already removed
 
 
############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


ggparallel(list("gear", "cyl"), data=mtcars)

titanic <- as.data.frame(Titanic)
ggparallel(names(titanic)[c(1,4,2,1)], order=0, titanic, weight="Freq") +
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")

ggparallel(names(acar_foc)[c(6,9,12,15)], order=0, acar_foc) 
ggparallel(names(acar_foc)[c(6,9,12,15)], order=0, acar_foc) 

ggparallel(names(acar_foc)[c(5,8,11,14)], order=0, acar_foc) # using area value...

# > names(acar_foc)[c(6,9,12,15)]
# [1] "gap_Class"   "own_Class"   "nlcd_Class"  "resil_Class"

# with iStats_majJoin
ggparallel(names(iStats_majJoin)[c(18,21,30,33)], order=0, asp=0.5, iStats_majJoin, weight="area.ha", method="hammock")+
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")
# > names(iStats_majJoin)[c(18,21,30,33)]
# [1] "pp_maj"   "gap_maj"  "own_maj"  "nlcd_maj"


# Charge the circlize library
library(circlize)


test <- iStats_majJoin[,c(16,18,21,30,33)]
 
# Make the circular plot
chordDiagram(test, transparency = 0.5)

















