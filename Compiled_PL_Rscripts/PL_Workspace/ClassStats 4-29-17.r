############################ 
#PURPOSE: Calculate eco-regional patch configuration statistics using ClassStats
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:
# Version as of 4-28-17 is only calculated on patches that do have core area and are not of unknown year.
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

# PACKAGES NEEDED
# Table manipulation
library(dplyr)
# Rasters
library(raster)
library(rgdal) # For writing .tif files
# library(gdistance)

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/" #Y:Drive
# Output_Folder <- "C:/Users/LacherL/Documents/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" # C:Drive
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"


# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:


# ----------------------------------------------
# INPUT FILES: 

# Patch Stats(all)
iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

# Patch Stats(with core only)
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")


#Estimated Year
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

# Protected Lands yes/no '0', '1'
prot_yn <- 	raster(paste0(BR_fileLoc, "prot_yn.tif" )) 

# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) # nodata, years, 9999
# pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" )) # 0, years, 9999 # Raster reclassified so background =zero. This is at 360*360 resolution

# Unique Patch ID raster- patches distinguished by YEAR, no core INCLUDED.
yrly_patID<- raster(paste0(BR_fileLoc, "yrly_patID.tif", sep=""))
# Unique Patch ID raster- patches distinguished by YEAR, no core REMOVED.
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))



############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# CLASS STATS: 
# ----------------------------------------------
# ----------------------------------------------
{
# In ArcMap:
# 1) Convert to polygon <- use what was used in landscape config prep?
# 2) select by location (DO BR AND PIEDMONT SEPARATELY)
# 3) back to raster

# Steps:
# 1) Use the select by location tool and click all the layers you want selected from. Use "are **within** the source layer feature " and export each selection to new layer
# 2) Convert all to raster so can run ClassStats on it. Bulk tool export to this location: BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"



# ----------------------------------------------
# USING INCREMENTAL YEAR AS THE CLASS CATEGORY
# ----------------------------------------------

# READ RASTERS INTO R
pl_80clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID1980clB.tif'))
pl_85clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID1985clB.tif'))
pl_90clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID1990clB.tif'))
pl_95clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID1995clB.tif'))
pl_00clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID2000clB.tif'))
pl_05clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID2005clB.tif'))
pl_10clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID2010clB.tif'))
pl_15clB<-raster(paste0(BR_fileLoc,'sNCyrly_patID2015clB.tif'))


pl_80clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID1980clP.tif'))
pl_85clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID1985clP.tif'))
pl_90clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID1990clP.tif'))
pl_95clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID1995clP.tif'))
pl_00clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID2000clP.tif'))
pl_05clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID2005clP.tif'))
pl_10clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID2010clP.tif'))
pl_15clP<-raster(paste0(BR_fileLoc,'sNCyrly_patID2015clP.tif'))


# # QC:
# length(unique(pl_80cl)); length(unique(pl_85cl)); length(unique(pl_90cl)); length(unique(pl_95cl)); length(unique(pl_00cl)); length(unique(pl_05cl)); length(unique(pl_10cl)); length(unique(pl_15cl))

# [1] 487
# [1] 530
# [1] 626
# [1] 740
# [1] 969
# [1] 1386
# [1] 1883
# [1] 2089


# ! LOAD CLASS STAT SELECT FUNCTION !

# ----------------------------------------------
# BLUE RIDGE
# ----------------------------------------------

ras_incB <- c(pl_80clB, pl_85clB, pl_90clB, pl_95clB, pl_00clB, pl_05clB, pl_10clB, pl_15clB)
names(ras_incB)<- c("pl_80clB", "pl_85clB", "pl_90clB", "pl_95clB", "pl_00clB", "pl_05clB", "pl_10clB", "pl_15clB")


CS_incB<-list()
c_r<-1
old<-Sys.time()
for(r in 1:length(ras_incB)){
	print(paste0(names(ras_incB[r])))
	
	# Change protected areas to 1 and zeros to NA
	ras_incB[[r]][ras_incB[[r]] >0]<-1
	ras_incB[[r]][ras_incB[[r]] == 0]<-NA
	
	# ClassStatSel
	CS_incB[[c_r]] <- ClassStatSel(ras_incB[[r]], cellsize = 180)
	c_r<-c_r+1
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 2.299063 mins



# ----------------------------------------------
# PIEDMONT
# ----------------------------------------------

ras_incP <- c(pl_80clP, pl_85clP, pl_90clP, pl_95clP, pl_00clP, pl_05clP, pl_10clP, pl_15clP)
names(ras_incP)<- c("pl_80clP", "pl_85clP", "pl_90clP", "pl_95clP", "pl_00clP", "pl_05clP", "pl_10clP", "pl_15clP")

# ! LOAD CLASS STAT SELECT FUNCTION !

CS_incP<-list()
c_r<-1
old<-Sys.time()
for(r in 1:length(ras_incP)){
	print(paste0(names(ras_incP[r])))
	
	# Change protected areas to 1 and zeros to NA
	ras_incP[[r]][ras_incP[[r]] >0]<-1
	ras_incP[[r]][ras_incP[[r]] == 0]<-NA
	
	# ClassStatSel
	CS_incP[[c_r]] <- ClassStatSel(ras_incP[[r]], cellsize = 180)
	c_r<-c_r+1
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 2.299063 mins


# ----------------------------------------------
# BIND ROWS

library(dplyr)

CS_incByrs <- bind_rows(CS_incB)
CS_incByrs$yrInt <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
CS_incByrs$total.core.area.ha <- CS_incByrs$total.core.area/10000
CS_incByrs$er <- "Blue Ridge"

CS_incPyrs <- bind_rows(CS_incP)
CS_incPyrs$yrInt <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
CS_incPyrs$total.core.area.ha <- CS_incPyrs$total.core.area/10000
CS_incPyrs$er <- "Piedmont"


CS_incall <- bind_rows(CS_incByrs, CS_incPyrs)


# WRITE TO FILE
write.table(CS_incall, file = paste0(Output_Folder,"CS_incall",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
CS_incall<-read.table(paste0(Output_Folder,"CS_incall.txt"), sep=",", header=TRUE)

str(CS_incall)

# 'data.frame':	16 obs. of  12 variables:
 # $ class               : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ n.patches           : int  187 194 200 228 263 332 397 427 150 185 ...
 # $ total.area          : num  1.67e+10 1.67e+10 1.67e+10 1.68e+10 1.74e+10 ...
 # $ prop.landscape      : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ patch.density       : num  1.12e-08 1.16e-08 1.20e-08 1.36e-08 1.51e-08 ...
 # $ largest.patch.index : num  0.374 0.373 0.373 0.372 0.361 ...
 # $ total.core.area     : num  1.37e+10 1.37e+10 1.37e+10 1.37e+10 1.41e+10 ...
 # $ aggregation.index   : num  95.2 95.2 95.2 95.2 95 ...
 # $ patch.cohesion.index: num  9.95 9.95 9.95 9.95 9.95 ...
 # $ yrInt               : int  1980 1985 1990 1995 2000 2005 2010 2015 1980 1985 ...
 # $ total.core.area.ha  : num  1367766 1368113 1369470 1373157 1410839 ...
 # $ er                  : Factor w/ 2 levels "Blue Ridge","Piedmont": 1 1 1 1 1 1 1 1 2 2 ...
 }
 
 
 
 
 

#####################################################################
#################    DEPRECATED??       #############################
#####################################################################

 
 
# ----------------------------------------------
# USING NLCD AS THE CLASS CATEGORY
# ** Need to go to raster relationships and calc nlcd maj table and raster first
# ----------------------------------------------

CS_nlcd <- ClassStatSel(nlcd_maj_ras, cellsize = 180)



pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))

# READ FROM FILE
nlcd_maj_ras<- raster(paste0(BR_fileLoc, "nlcd_maj_ras.tif", sep=""))

# READ RASTERS INTO R
pl_85cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1985cl.tif'))
pl_90cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1990cl.tif'))
pl_95cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1995cl.tif'))
pl_00cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2000cl.tif'))
pl_05cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2005cl.tif'))
pl_10cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2010cl.tif'))
pl_15cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2015cl.tif'))


ext <- extent(nlcd_maj_ras)
# > ext
# class       : Extent 
# xmin        : 819389.5 
# xmax        : 1706429 
# ymin        : 1078140 
# ymax        : 2124660

pl_85cl <- setExtent(pl_85cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_90cl <- setExtent(pl_90cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_95cl <- setExtent(pl_95cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_00cl <- setExtent(pl_00cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_05cl <- setExtent(pl_05cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_10cl <- setExtent(pl_10cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_15cl <- setExtent(pl_15cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)

pl_85m <- mask(nlcd_maj_ras, pl_85cl)
pl_90m <- mask(nlcd_maj_ras, pl_90cl)
pl_95m <- mask(nlcd_maj_ras, pl_95cl)
pl_00m <- mask(nlcd_maj_ras, pl_00cl)
pl_05m <- mask(nlcd_maj_ras, pl_05cl)
pl_10m <- mask(nlcd_maj_ras, pl_10cl)
pl_15m <- mask(nlcd_maj_ras, pl_15cl)


# WRITE TO FILE
writeRaster(pl_85m , filename=paste0(BR_fileLoc, "pl_85m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_90m , filename=paste0(BR_fileLoc, "pl_90m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_95m , filename=paste0(BR_fileLoc, "pl_95m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_00m , filename=paste0(BR_fileLoc, "pl_00m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_05m , filename=paste0(BR_fileLoc, "pl_05m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_10m , filename=paste0(BR_fileLoc, "pl_10m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_15m , filename=paste0(BR_fileLoc, "pl_15m.tif", sep=""), format='GTiff', overwrite=TRUE)

	
# READ FROM FILE
pl_85m<- raster(paste0(BR_fileLoc, "pl_85m.tif", sep=""))
pl_90m<- raster(paste0(BR_fileLoc, "pl_90m.tif", sep=""))
pl_95m<- raster(paste0(BR_fileLoc, "pl_95m.tif", sep=""))
pl_00m<- raster(paste0(BR_fileLoc, "pl_00m.tif", sep=""))
pl_05m<- raster(paste0(BR_fileLoc, "pl_05m.tif", sep=""))
pl_10m<- raster(paste0(BR_fileLoc, "pl_10m.tif", sep=""))
pl_15m<- raster(paste0(BR_fileLoc, "pl_15m.tif", sep=""))




# QC:
# unique(pl_85m);unique(pl_90m);unique(pl_95m);unique(pl_00m);unique(pl_05m);unique(pl_10m);unique(pl_15m)

ras_inc <- c(pl_85m, pl_90m, pl_95m, pl_00m, pl_05m, pl_10m, pl_15m)
names(ras_inc)<- c("pl_85m", "pl_90m", "pl_95m", "pl_00m", "pl_05m", "pl_10m", "pl_15m")


CS_nlcd<-list()
c_r<-1
old<-Sys.time()
for(r in 1:length(ras_inc)){
	print(paste0(names(ras_inc[r])))

	# ClassStatSel
	CS_nlcd[[c_r]] <- ClassStatSel(ras_inc[[r]], cellsize = 180)
	c_r<-c_r+1
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 3.885805 mins


CS_nlcdall <- bind_rows(CS_nlcd)
CS_nlcdall$yrInt <- c(rep(1985,7), rep(1990,7), rep(1995,7), rep(2000,7), rep(2005,7), rep(2010,7), rep(2015,7))
CS_nlcdall$total.core.area.ha <- CS_nlcdall$total.core.area/10000


# WRITE TO FILE
write.table(CS_nlcdall, file = paste0(Output_Folder,"CS_nlcdall",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
CS_nlcdall<-read.table(paste0(Output_Folder,"CS_nlcdall.txt"), sep=",", header=TRUE)




 
}


