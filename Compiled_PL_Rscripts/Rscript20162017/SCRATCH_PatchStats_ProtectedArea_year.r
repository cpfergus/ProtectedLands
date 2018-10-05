
detach(name="package:dplyr", unload=TRUE)
detach(name="package:SDMTools", unload=TRUE)
detach(name="package:Hmisc", unload=TRUE)
detach(name="package:igraph", unload=TRUE)
detach(name="package:raster", unload=TRUE)
detach(name="package:stringr", unload=TRUE)
detach(name="package:rgdal", unload=TRUE)
detach(name="package:gdistance", unload=TRUE)


# ----------------------------------------------
# ----------------------------------------------



# FILE LOCATIONS: 
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats"
Comb_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Tables/" # Combine (01-11)
CombRas_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Rasters/"
# Final_output<-"Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/" # final (2011) histogram by county

inRasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/" #HF server
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"


# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_yearL.tif" )) #seeking large resolution one.

pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" )) # 0, years, 9999 # Raster reclassified so background =zero

# Unique Patch ID raster- patches distinguished by YEAR
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))

# NLCD 2011
pl_nlcd <- raster(paste0(BR_fileLoc, "pl_nlcdL.tif" ))

# Patch Stats table
iPatch_inc<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iPatchStats_inc.csv")



> head(maj_nlcd11)
  patchID MajorityNLCD NLCDArea PercPatch
1       1            5     0.72  29.62963
2       2            0     0.09   0.00000
3       3           10     1.08 100.00000
4       4           10     3.15  81.39535
5       5            0     0.09   0.00000
6       6           10     1.44  80.00000
> head(ras_patches)
Source: local data frame [6 x 5]

  zone count area.ha  Raster Class
1    1     0       0 pl_type     1
2    2     0       0 pl_type     1
3    3     0       0 pl_type     1
4    4     0       0 pl_type     1
5    5     0       0 pl_type     1
6    6     0       0 pl_type     1



, str_sub(names(ras_patches[f]), start=4)




# ----------------------------------------------
# ----------------------------------------------
# CLASS STATS FOR ENTIRE LANDSCAPE
# ----------------------------------------------
# ----------------------------------------------

	# CS_yr <- ClassStat(yr_mask, cellsize = 360)
	
	# # WRITE TO FILE
	# write.csv(CS_yr,paste(Output_Folder,"/ClassStats_yrly", ".csv",sep=""), row.names = F)
	
	# # READ FROM FILE
	# CS_yr<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_SA/Patch_Stats/ClassStats_yrly.csv")

	
	CS_yr <- ClassStat(yrly_patID, cellsize = 360)
	
	# WRITE TO FILE
	write.csv(CS_yr,paste(Output_Folder,"/ClassStats_yrly", ".csv",sep=""), row.names = F)
	
	# READ FROM FILE
	CS_yr<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_SA/Patch_Stats/ClassStats_yrly.csv")
	






test_ps <- PatchStat(plID_8595_p, cellsize = 360)

test_cs <- ClassStat(plID_8595_p, cellsize = 360)


test_cs3 <- ClassStat(yr_mask, cellsize = 360)



# Create matrices to fill in.
  ClassStats <- matrix(nrow = length(ProtLandConversions), ncol = 17)

  colnames(Expansion_PatchStats) <- c("Transition", "Number of Patches", "Total Patch Area(ha)", "Mean Patch Area(ha)", "Min Patch Area(ha)", "Max Patch Area(ha)", "StDev Patch Area(ha)", "Variance Patch Area(ha)", "Total Patch Core Area(ha)", "Mean Patch Core Area(ha)", "Min Patch Core Area(ha)", "Max Patch Core Area(ha)", "StDev Patch Core Area(ha)", "Variance Patch Core Area(ha)", "Mean Shape Index", "Mean Core Area Index", "Perc Expansion Area")
 
prop.like.adjacencies 
aggregation.index
patch.cohesion.index





# ----------------------------------------------
# ----------------------------------------------
# REMOVE CORE.PATCHES <=1 & JOIN TABLES BY PATCH ID
# ----------------------------------------------
# ----------------------------------------------


iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")
# str(iStats_all)
# 'data.frame':	5531 obs. of  14 variables:
 # $ patchID          : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ n.cell           : int  27 1 12 43 1 20 58 31 12 244 ...
 # $ n.core.cell      : int  1 0 0 0 0 0 0 0 0 22 ...
 # $ n.edges.perimeter: int  38 4 28 70 4 44 110 60 24 298 ...
 # $ n.edges.internal : int  70 0 20 102 0 36 122 64 24 678 ...
 # $ area             : num  3499200 129600 1555200 5572800 129600 ...
 # $ core.area        : int  129600 0 0 0 0 0 0 0 0 2851200 ...
 # $ perimeter        : int  13680 1440 10080 25200 1440 15840 39600 21600 8640 107280 ...
 # $ perim.area.ratio : num  0.00391 0.01111 0.00648 0.00452 0.01111 ...
 # $ shape.index      : num  1.73 1 2 2.5 1 ...
 # $ frac.dim.index   : num  1.08 1 1.1 1.13 1 ...
 # $ core.area.index  : num  0.037 0 0 0 0 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...

maj_categ<-read.table(paste0(Comb_output,"maj_categ.txt"), sep=",", header=TRUE)
 # str(maj_categ)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	38717 obs. of  5 variables:
 # $ patchID   : num  1 2 3 4 5 6 7 8 9 10 ...
 # $ raster    : chr  "nlcd" "nlcd" "nlcd" "nlcd" ...
 # $ majority  : chr  "6" "0" "10" "10" ...
 # $ area.ha   : num  0.72 0.09 1.08 3.15 0.09 ...
 # $ proportion: num  29.6 0 100 81.4 0 ...


est_yr<-read.table(paste0(Comb_output,"est_yr.txt"), sep=",", header=TRUE)
# str(est_yr)
# 'data.frame':	5531 obs. of  2 variables:
 # $ patchID: int  1 2 3 4 5 6 7 8 9 10 ...
 # $ estYr  : int  1800 1800 1819 1819 1819 1819 1819 1819 1819 1819 ...



nldev_dist<-read.table(paste0(Comb_output,"nldev_distL.txt"), sep=",", header=TRUE)
# str(nldev_dist)
# 'data.frame':	5531 obs. of  2 variables:
 # $ zone: int  1 2 3 4 5 6 7 8 9 10 ...
 # $ min : num  4.2 5.96 6.21 8.53 7.29 ...

 
# CORE PATCHES <1 REMOVED ALREADY
cmp_dist_min<-read.table(paste0(Comb_output,"cmp_dist_minL.txt"), sep=",", header=TRUE)
# str(cmp_dist_min)
 # 'data.frame':	1501 obs. of  3 variables:
 # $ patchID     : int  1 10 11 13 22 23 27 31 33 34 ...
 # $ min_dist(km): num  0 0 0 0 0 0.36 0 0 0.72 0 ...
 # $ d_patchID   : Factor w/ 1184 levels "10","1001","1009",..: 1169 159 59 175 178 24 201 1144 674 140 ...'data.frame':	5531 obs. of  2 variables:
 # $ patchID: int  1 2 3 4 5 6 7 8 9 10 ...
 # $ estYr  : int  1800 1800 1819 1819 1819 1819 1819 1819 1819 1819 ...str(est_yr)

# First must join  
# est_yr, maj_categ, nldev_dist, 
# cmp_dist_min
# iStats_all
	








