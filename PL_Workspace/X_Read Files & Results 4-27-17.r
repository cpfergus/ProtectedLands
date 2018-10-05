
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

# PACKAGES NEEDED

# rasters
library(raster)

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

# ----------------------------------------------
################################################

# ----------------------------------------------
# ----------------------------------------------
# READ FILES: 
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# FILE LOCATIONS: 
# ----------------------------------------------


BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
Comb_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Tables/" 
CombRas_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Rasters/"


# ----------------------------------------------
# RAW RASTERS:
# ----------------------------------------------
# Format of original raw rasters is: 
# > pl_year
# class       : RasterLayer 
# dimensions  : 5821, 4930, 28697530  (nrow, ncol, ncell)
# resolution  : 180, 180  (x, y)
# extent      : 819105, 1706505, 1077045, 2124825  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 
# data source : Y:\Lacher\IaraSpatialLayers_HF\PreparedRasters\ProLands\BlueRidge\pl_year.tif 
# names       : pl_year 
# values      : 1800, 9999  (min, max)


# Protected Lands yes/no '0', '1'
prot_yn <- 	raster(paste0(BR_fileLoc, "prot_yn.tif" )) 

# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) # nodata, years, 9999
pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" )) # 0, years, 9999 # Raster reclassified so background =zero. This is at 360*360 resolution

# Protected Lands Classifications	
pl_gap 	<- 	raster(paste0(BR_fileLoc, "pl_gap.tif"  ))
pl_own 	<- 	raster(paste0(BR_fileLoc, "pl_own.tif"  ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.tif"   ))
pl_type <- 	raster(paste0(BR_fileLoc, "pl_type.tif" ))

pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.tif"   ))
pl_state <- raster(paste0(BR_fileLoc, "pl_state.tif"))
pl_resil <- raster(paste0(BR_fileLoc, "pl_resil.tif" ))
# pl_biopri <- raster(paste0(BR_fileLoc, "pl_biopri.tif")) # maybe come back to see note above


# ----------------------------------------------
# GENERATED RASTERS
# ----------------------------------------------


# Unique Patch ID raster- patches distinguished by YEAR
yrly_patID<- raster(paste0(BR_fileLoc, "yrly_patID.tif", sep="")) #Includes buffer areas. Total of 14,746 individual patches of all sizes.


# Time Increment Raster
pl_85<-raster(paste0(BR_fileLoc, "pl_85.tif"))
pl_95<-raster(paste0(BR_fileLoc, "pl_95.tif"))
pl_05<-raster(paste0(BR_fileLoc, "pl_05.tif"))
pl_15<-raster(paste0(BR_fileLoc, "pl_15.tif"))

# Change Raster
pl_8595<-raster(paste0(CombRas_output, "pl_Comb8595.tif"))
pl_9505<-raster(paste0(CombRas_output, "pl_Comb9505.tif"))
pl_0515<-raster(paste0(CombRas_output, "pl_Comb0515.tif"))

# Patch ID Raster
plID_8595<-raster(paste0(CombRas_output, "pl_Comb8595patID.tif"))
plID_9505<-raster(paste0(CombRas_output, "pl_Comb9505patID.tif"))
plID_0515<-raster(paste0(CombRas_output, "pl_Comb0515patID.tif"))

# Adjacent Raster
adj8595_raster<-raster(paste0(CombRas_output, "adj_8595.tif"))
adj9505_raster<-raster(paste0(CombRas_output, "adj_9505.tif"))
adj0515_raster<-raster(paste0(CombRas_output, "adj_0515.tif"))


# ----------------------------------------------
# ----------------------------------------------
# TABLES
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# PATCH STATS
# ----------------------------------------------

# ----------------------------------------------
# RUN PATCH STATS FOR EVERYTHING *PRIOR* TO 1981
# note how this is for less than 1981, not <= 1981, i.e. 1980.
# BRold_Stats

# ----------------------------------------------
# RUN PATCH STATS FOR  UNKNOWNS (9999)
# BRunk_Stats

# ----------------------------------------------
# PATCH STATS FOR SEVEN TIME INCREMENTS (1981, 1986, 1991, 1996, 2001, 2006, 2011)

# READ FROM FILE
		iPatch_inc<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iPatchStats_inc.csv")
		
str(iPatchStats_inc.csv)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	8044 obs. of  14 variables:
 # $ patchID          : int  2524 2525 2526 2527 2528 2529 2530 2531 2532 2533 ...
 # $ n.cell           : int  1 17 15 21 39 22 2 17 1 19 ...
 # $ n.core.cell      : int  0 3 0 2 8 0 0 1 0 4 ...
 # $ n.edges.perimeter: int  4 20 22 24 38 30 6 22 4 20 ...
 # $ n.edges.internal : int  0 48 38 60 118 58 2 46 0 56 ...
 # $ area             : num  32400 550800 486000 680400 1263600 ...
 # $ core.area        : num  0 97200 0 64800 259200 ...
 # $ perimeter        : num  720 3600 3960 4320 6840 5400 1080 3960 720 3600 ...
 # $ perim.area.ratio : num  0.02222 0.00654 0.00815 0.00635 0.00541 ...
 # $ shape.index      : num  1 1.11 1.38 1.2 1.46 ...
 # $ frac.dim.index   : num  1 1.03 1.05 1.04 1.06 ...
 # $ core.area.index  : num  0 0.1765 0 0.0952 0.2051 ...
 # $ Transition       : num  1981 1981 1981 1981 1981 ...
 # $ Type             : chr  "Separated" "Separated" "Separated" "Separated" ...
# ----------------------------------------------
# BIND TABLES: INCREMENT TABLE WITH PATCHES PRIOR TO 1985 AND UNKNOWNS (9999)

# READ FROM FILE
	iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")	
	
# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA <1 FROM *TABLE*

# core_pat<-filter(iStats_all, n.core.cell>=1)

# READ FROM FILE
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")
 
# ----------------------------------------------
# SUMMARY CALCULATIONS
# ----------------------------------------------
 
# Total area within ecoregions (not only protected lands)
er_nlcd<-read.table(paste0(Output_Folder,"er_nlcd",".txt"), sep=",", header=TRUE)
# > str(er_nlcd)
# 'data.frame':	80 obs. of  5 variables:
 # $ er          : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ count       : int  10351 2443 33094 0 2078 6 3428 23423 21544 11696 ...
 # $ NLCD_area_ha: num  33537 7915 107225 0 6733 ...
 # $ Raster      : Factor w/ 1 level "pl_nlcd": 1 1 1 1 1 1 1 1 1 1 ...
 # $ Class       : int  1 1 1 1 1 1 1 1 1 1 ...
 
er_resil<-read.table(paste0(Output_Folder,"er_resil",".txt"), sep=",", header=TRUE)
# str(er_resil)
# 'data.frame':	80 obs. of  5 variables:
 # $ er           : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ count        : int  304238 268946 1067432 581 1112 4968 230554 792233 926508 182001 ...
 # $ resil_area_ha: num  985731 871385 3458480 1882 3603 ...
 # $ Raster       : Factor w/ 1 level "pl_resil": 1 1 1 1 1 1 1 1 1 1 ...
 # $ Class        : int  1 1 1 1 1 1 1 1 1 1 ...

est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

# > str(est_yr)
# 'data.frame':	14746 obs. of  2 variables:
 # $ patchID: int  1 2 3 4 5 6 7 8 9 10 ...
 # $ estYr  : int  1800 1800 1800 1800 1800 1800 1819 1819 1819 1819 ...
 
iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

# str(iStats_all)# 4-28-17
# 'data.frame':	14746 obs. of  14 variables:
 # $ patchID          : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ n.cell           : int  151 1 7 1 1 1 1 1 5 4 ...
 # $ n.core.cell      : int  61 0 0 0 0 0 0 0 0 0 ...
 # $ n.edges.perimeter: int  116 4 22 4 4 4 4 4 16 16 ...
 # $ n.edges.internal : int  488 0 6 0 0 0 0 0 4 0 ...
 # $ area             : num  4892400 32400 226800 32400 32400 ...
 # $ core.area        : num  1976400 0 0 0 0 ...
 # $ perimeter        : num  20880 720 3960 720 720 ...
 # $ perim.area.ratio : num  0.00427 0.02222 0.01746 0.02222 0.02222 ...
 # $ shape.index      : num  2.32 1 1.83 1 1 ...
 # $ frac.dim.index   : num  1.11 1 1.12 1 1 ...
 # $ core.area.index  : num  0.404 0 0 0 0 ...
 # $ Transition       : num  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : chr  "noMeas" "noMeas" "noMeas" "noMeas" ...
 
 iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")

> str(iStats_corepat)
'data.frame':	4155 obs. of  17 variables:
 $ patchID          : int  1 11 17 49 64 69 97 120 145 158 ...
 $ n.cell           : int  151 19 57 272 48 365 349 190 21 503 ...
 $ n.core.cell      : int  61 1 15 8 4 109 96 3 2 75 ...
 $ n.edges.perimeter: int  116 32 52 418 64 282 328 368 26 628 ...
 $ n.edges.internal : int  488 44 176 670 128 1178 1068 392 58 1384 ...
 $ area             : num  4892400 615600 1846800 8812800 1555200 ...
 $ core.area        : int  1976400 32400 486000 259200 129600 3531600 3110400 97200 64800 2430000 ...
 $ perimeter        : int  20880 5760 9360 75240 11520 50760 59040 66240 4680 113040 ...
 $ perim.area.ratio : num  0.00427 0.00936 0.00507 0.00854 0.00741 ...
 $ shape.index      : num  2.32 1.78 1.62 6.33 2.29 ...
 $ frac.dim.index   : num  1.11 1.09 1.08 1.23 1.12 ...
 $ core.area.index  : num  0.404 0.0526 0.2632 0.0294 0.0833 ...
 $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ estYr            : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 $ area.ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 $ core.area.ha     : num  197.64 3.24 48.6 25.92 12.96 ...

 # remove estYr = 9999
 iStats_corepat_2015 <- filter(iStats_corepat, estYr < 9999)
 
 # WRITE TO FILE
write.table(iStats_corepat_2015, file = paste0(Output_Folder,"iStats_corepat_2015.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
iStats_corepat_2015<-read.table(paste0(Output_Folder,"iStats_corepat_2015.txt"), sep=",", header=TRUE)
 
 
maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)
'data.frame':	4155 obs. of  19 variables:
 $ patchID  : int  1 11 17 49 64 69 97 120 145 158 ...
 $ pp_maj   : int  1 1 1 1 1 1 1 1 1 1 ...
 $ pp_ha    : num  489.2 61.6 184.7 881.3 155.5 ...
 $ pp_prop  : num  100 100 100 100 100 100 100 100 100 100 ...
 $ gap_maj  : int  3 3 3 3 3 3 3 3 3 3 ...
 $ gap_ha   : num  489.2 61.6 184.7 881.3 155.5 ...
 $ gap_prop : num  100 100 100 100 100 100 100 100 100 100 ...
 $ type_maj : int  2 2 2 2 2 2 2 2 2 2 ...
 $ type_ha  : num  489.2 61.6 184.7 881.3 155.5 ...
 $ type_prop: num  100 100 100 100 100 100 100 100 100 100 ...
 $ er_maj   : int  1 9 9 9 9 9 9 3 3 9 ...
 $ er_ha    : num  469.8 61.6 184.7 881.3 155.5 ...
 $ er_prop  : num  96 100 100 100 100 ...
 $ own_maj  : int  1 1 1 1 1 1 1 1 1 1 ...
 $ own_ha   : num  489.2 61.6 184.7 881.3 155.5 ...
 $ own_prop : num  100 100 100 100 100 100 100 100 100 100 ...
 $ nlcd_maj : int  5 1 1 1 1 1 1 1 1 1 ...
 $ nlcd_ha  : num  486 51.8 184.7 816.5 132.8 ...
 $ nlcd_prop: num  99.3 84.2 100 92.6 85.4 ...
 
 
 
 
iStats_majJoin<-read.table(paste0(Output_Folder,"iStats_majJoin",".txt"), sep=",", header=TRUE)
str(iStats_majJoin)
'data.frame':	4155 obs. of  35 variables:
 $ patchID          : int  1 11 17 49 64 69 97 120 145 158 ...
 $ n.cell           : int  151 19 57 272 48 365 349 190 21 503 ...
 $ n.core.cell      : int  61 1 15 8 4 109 96 3 2 75 ...
 $ n.edges.perimeter: int  116 32 52 418 64 282 328 368 26 628 ...
 $ n.edges.internal : int  488 44 176 670 128 1178 1068 392 58 1384 ...
 $ area             : num  4892400 615600 1846800 8812800 1555200 ...
 $ core.area        : int  1976400 32400 486000 259200 129600 3531600 3110400 97200 64800 2430000 ...
 $ perimeter        : int  20880 5760 9360 75240 11520 50760 59040 66240 4680 113040 ...
 $ perim.area.ratio : num  0.00427 0.00936 0.00507 0.00854 0.00741 ...
 $ shape.index      : num  2.32 1.78 1.62 6.33 2.29 ...
 $ frac.dim.index   : num  1.11 1.09 1.08 1.23 1.12 ...
 $ core.area.index  : num  0.404 0.0526 0.2632 0.0294 0.0833 ...
 $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ estYr            : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 $ area.ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 $ core.area.ha     : num  197.64 3.24 48.6 25.92 12.96 ...
 $ pp_maj           : int  1 1 1 1 1 1 1 1 1 1 ...
 $ pp_ha            : num  489.2 61.6 184.7 881.3 155.5 ...
 $ pp_prop          : num  100 100 100 100 100 100 100 100 100 100 ...
 $ gap_maj          : int  3 3 3 3 3 3 3 3 3 3 ...
 $ gap_ha           : num  489.2 61.6 184.7 881.3 155.5 ...
 $ gap_prop         : num  100 100 100 100 100 100 100 100 100 100 ...
 $ type_maj         : int  2 2 2 2 2 2 2 2 2 2 ...
 $ type_ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 $ type_prop        : num  100 100 100 100 100 100 100 100 100 100 ...
 $ er_maj           : int  1 9 9 9 9 9 9 3 3 9 ...
 $ er_ha            : num  469.8 61.6 184.7 881.3 155.5 ...
 $ er_prop          : num  96 100 100 100 100 ...
 $ own_maj          : int  1 1 1 1 1 1 1 1 1 1 ...
 $ own_ha           : num  489.2 61.6 184.7 881.3 155.5 ...
 $ own_prop         : num  100 100 100 100 100 100 100 100 100 100 ...
 $ nlcd_maj         : int  5 1 1 1 1 1 1 1 1 1 ...
 $ nlcd_ha          : num  486 51.8 184.7 816.5 132.8 ...
 $ nlcd_prop        : num  99.3 84.2 100 92.6 85.4 ...
 
 
  # remove estYr = 9999
iStats_majJoin_2015 <- filter(iStats_majJoin, estYr < 9999)
 
 # WRITE TO FILE
write.table(iStats_majJoin_2015, file = paste0(Output_Folder,"iStats_majJoin_2015.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
iStats_majJoin_2015<-read.table(paste0(Output_Folder,"iStats_majJoin_2015.txt"), sep=",", header=TRUE)
 
  
 
gap_area<-read.table(paste0(Output_Folder,"gap_area",".txt"), sep=",", header=TRUE)

own_area<-read.table(paste0(Output_Folder,"own_area",".txt"), sep=",", header=TRUE)

nlcd_area<-read.table(paste0(Output_Folder,"nlcd_area",".txt"), sep=",", header=TRUE)

resil_area<-read.table(paste0(Output_Folder,"resil_area",".txt"), sep=",", header=TRUE)

  
GapOwnNlRe_ar_join<-read.table(paste0(Output_Folder,"GapOwnNlRe_ar_join",".txt"), sep=",", header=TRUE)
 
 
# pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)

majar_8615<-read.table(paste0(Output_Folder,"majar_8615",".txt"), sep=",", header=TRUE)

# str(majar_8615)
# 'data.frame':	1517 obs. of  35 variables:
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

 # # # #Filter down by categories:
 
# # # majar_unk <- filter(iStats_majJoin, estYr ==9999) # unknown year
# # # majar_notunk <-filter(iStats_majJoin, estYr<9999)# removed unknowns. left with 1800-2015 

# # # # study area only

# # # majar_er_unk <- filter(majar_unk, er_maj==1|er_maj==2|er_maj==3)
# # # majar_er_unk$er_maj<-as.integer(ifelse(majar_er_unk$er_maj==1, 1, ifelse(majar_er_unk$er_maj==2|majar_er_unk$er_maj==3,23,1))) # reclassify to group piedmont into one value:

# # # majar_er <- filter(majar_notunk, er_maj==1|er_maj==2|er_maj==3)
# # # majar_er$er_maj<-as.integer(ifelse(majar_er$er_maj==1, 1, ifelse(majar_er$er_maj==2|majar_er$er_maj==3,23,1)))# reclassify to group piedmont into one value:

# # # # time periods

# # # majar_8615 <-filter(majar_er, estYr>1985) # 1986-2015 
# # # majar_1800 <- filter(majar_er, estYr <=1985) # 1800-1985
# # # majar_8185 <- filter(majar_er, estYr >=1981 & estYr <=1985) #1981-1985

 
acar_8615<-read.table(paste0(Output_Folder,"acar_8615",".txt"), sep=",", header=TRUE)

# str(acar_8615)
# 'data.frame':	48544 obs. of  15 variables:
 # $ patchID      : int  2789 2789 2789 2789 2789 2789 2789 2789 2789 2789 ...
 # $ estYr        : int  1986 1986 1986 1986 1986 1986 1986 1986 1986 1986 ...
 # $ er_maj       : int  2 2 2 2 2 2 2 2 2 2 ...
 # $ gap_count    : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_area_ha  : num  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ own_count    : int  0 0 0 0 0 0 0 0 36 36 ...
 # $ own_area_ha  : num  0 0 0 0 0 ...
 # $ own_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ nlcd_count   : int  0 0 0 0 12 4 20 0 0 0 ...
 # $ nlcd_area_ha : num  0 0 0 0 38.9 ...
 # $ nlcd_Class   : int  1 2 3 4 5 6 7 8 1 2 ...
 # $ resil_count  : int  36 0 0 0 0 0 0 0 36 0 ...
 # $ resil_area_ha: num  117 0 0 0 0 ...
 # $ resil_Class  : int  1 2 3 4 5 6 7 8 1 2 ...

 
# ----------------------------------------------
# ----------------------------------------------
# RESULTS
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# CUMULATIVE AREA
# ----------------------------------------------

cum_TArER<-read.table(paste0(Output_Folder,"cum_TArER.txt"), sep=",", header=TRUE)
cum_ArOwn123<-read.table(paste0(Output_Folder,"cum_ArOwn123.txt"), sep=",", header=TRUE)
cum_NL123<-read.table(paste0(Output_Folder,"cum_NL123.txt"), sep=",", header=TRUE)
cum_GAP123<-read.table(paste0(Output_Folder,"cum_GAP123.txt"), sep=",", header=TRUE)

# Edited for 
cum_TArER_EDIT    <-read.table(paste0(Output_Folder,"cum_TArER_EDIT.txt"), sep="\t", header=TRUE)
cum_ArOwn123_EDIT <-read.table(paste0(Output_Folder,"cum_ArOwn123_EDIT.txt"), sep="\t", header=TRUE)
cum_NL123_EDIT    <-read.table(paste0(Output_Folder,"cum_NL123_EDIT.txt"), sep="\t", header=TRUE)
cum_GAP123_EDIT   <-read.table(paste0(Output_Folder,"cum_GAP123_EDIT.txt"), sep="\t", header=TRUE)




# ----------------------------------------------
# COALESCED CORE AREA 
# ----------------------------------------------


coal_vals1111 <- raster(paste0(BR_fileLoc, "coal_vals1111.tif", sep=""))
coal_vals8595 <- raster(paste0(BR_fileLoc, "coal_vals8595.tif", sep=""))
coal_vals9505 <- raster(paste0(BR_fileLoc, "coal_vals9505.tif", sep=""))
coal_vals0515 <- raster(paste0(BR_fileLoc, "coal_vals0515.tif", sep=""))

# ----------------------------------------------
# Nearest Neighbot Regression Outputs - Version 12-15-16
# ----------------------------------------------

# NN prediction
NNpred123<-read.table(paste0(Output_Folder,"NNpred123.txt"), sep=",", header=TRUE)


> summary(nnB); summary(nnP)

Call:
glm(formula = sqrt(min_dist.km) ~ estYr, data = dist_n1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0534  -0.7075  -0.2304   0.5754   3.4236  

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept) 37.202374  13.430689   2.770  0.00588 **
estYr       -0.018202   0.006705  -2.715  0.00693 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.7057707)

    Null deviance: 275.51  on 384  degrees of freedom
Residual deviance: 270.31  on 383  degrees of freedom
AIC: 962.42

Number of Fisher Scoring iterations: 2


Call:
glm(formula = sqrt(min_dist.km) ~ estYr, data = dist_n23)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0796  -0.7910  -0.1411   0.5091   3.4071  

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 27.116821   7.842051   3.458 0.000564 ***
estYr       -0.013117   0.003914  -3.351 0.000831 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.7909091)

    Null deviance: 915.26  on 1147  degrees of freedom
Residual deviance: 906.38  on 1146  degrees of freedom
AIC: 2992.6

Number of Fisher Scoring iterations: 2

