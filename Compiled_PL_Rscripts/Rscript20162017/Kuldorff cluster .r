############################ 
#PURPOSE:
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 

# ----------------------------------------------
################################################


# PACKAGES NEEDED
library(raster)
library(rgdal)
# library(rgeos)

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 

BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:
iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

# ----------------------------------------------
# # First remove patches that are too small, then use ARc to calc NN
# # Join to get est_Yr and core.area
# temp <- full_join(est_yr, iStats_all, by="patchID")

# core_pat<-filter(temp, core.area >= 1) 
# ncor <-filter(temp, core.area == 0) 

# NCyrly_patID<-yrly_patID
# NCyrly_patID[yrly_patID %in% ncor$patchID] <- NA
# ----------------------------------------------
# READ FROM FILE
NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif"))#in NAD UTM 17
	
# ----------------------------------------------
# First remove patches that are too small, then use ARc to calc NN
# Join to get est_Yr and core.area
temp <- full_join(est_yr, iStats_all, by="patchID")

core_pat<-filter(temp, core.area >= 1) 
ncor <-filter(temp, core.area == 0) 

NCyrly_patID<-yrly_patID
NCyrly_patID[yrly_patID %in% ncor$patchID] <- NA
# ----------------------------------------------
NCpl_yr_z<-raster(paste0(BR_fileLoc, "IndPatches/NCpl_yr_z.tif"))


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# PREP DATA INTO CORRECT FORMAT
# ----------------------------------------------

# Break into 5 year increments after 1985

NCpl_yr_z<-raster(paste0(BR_fileLoc, "IndPatches/NCpl_yr_z.tif"))# this includes 9999, but the selection below does not include this to be used in the analysis.
yr_valsNC <- getValues(NCpl_yr_z)
sort(unique(yr_valsP))


pl_1985valsNC <- ifelse(yr_valsNC <= 1985| is.na(yr_valsNC), yr_valsNC, 0) 
pl_85NC <- setValues(NCpl_yr_z, pl_1985valsNC)



{# Start with one above to see how things go!
pl_1990valsP <- ifelse(yr_valsP <= 1990| is.na(yr_valsP), yr_valsP, 0)
pl_90P <- setValues(pl_yr_zP, pl_1990valsP)

pl_1995valsP <- ifelse(yr_valsP <= 1995| is.na(yr_valsP), yr_valsP, 0)
pl_95P <- setValues(pl_yr_zP, pl_1995valsP)

pl_2000valsP <- ifelse(yr_valsP <= 2000| is.na(yr_valsP), yr_valsP, 0)
pl_00P <- setValues(pl_yr_zP, pl_2000valsP)

pl_2005valsP <- ifelse(yr_valsP <= 2005| is.na(yr_valsP), yr_valsP, 0)
pl_05P <- setValues(pl_yr_zP, pl_2005valsP)

pl_2010valsP <- ifelse(yr_valsP <= 2010| is.na(yr_valsP), yr_valsP, 0)
pl_10P <- setValues(pl_yr_zP, pl_2010valsP)

pl_2015valsP <- ifelse(yr_valsP <= 2015| is.na(yr_valsP), yr_valsP, 0)
pl_15P <- setValues(pl_yr_zP, pl_2015valsP)
}

# FIRST CONVERT MY DATA TO SPATIAL POLYGONS:

rpoly85 <- rasterToPolygons(pl_85NC, dissolve=T)# rasterToPolygon method
# This is now in formal class 'Polygon', SpatialPolygonsDataFrame
# > rpoly85
# class       : SpatialPolygonsDataFrame 
# features    : 61 
# extent      : 821265, 1704345, 1078125, 2124645  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# variables   : 1
# names       : NCpl_yr_z 
# min values  :         0 
# max values  :      1985 
# Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  # ..@ data       :'data.frame':	61 obs. of  1 variable:
  # .. ..$ NCpl_yr_z: num [1:61] 0 1800 1819 1870 1890 ...
  # ..@ polygons   :List of 61
  # .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  # .. .. .. ..@ Polygons :List of 5230
  # .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  # .. .. .. .. .. .. ..@ labpt  : num [1:2] 910960 1083901
  # .. .. .. .. .. .. ..@ area   : num 1490400
  # .. .. .. .. .. .. ..@ hole   : logi FALSE
  # .. .. .. .. .. .. ..@ ringDir: int 1
  # .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] 911445 911265 911085 910905 910905 ...
  # .. .. .. .. .. .. .. ..- attr(*, "dimnames")=List of 2
  # .. .. .. .. .. .. .. .. ..$ : NULL
  # .. .. .. .. .. .. .. .. ..$ : chr [1:2] "x" "y"


# This will give you the spatial polygons - just check to make sure are in the correct format!

# ----------------------------------------------
# NEED TABLE THAT DEFINES STRATA
 # - AREA
 # - ECOREGION??- MAYBE LATER. CAN SEE ON MAP


# RELATE TO REAL AREA STATS FOR NLCD, RESILIENCY, ETC.
