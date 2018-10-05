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

# Rasters
library(raster)


# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 

# ----------------------------------------------
# READ OUTPUT FILES:

# file name:  / R label: 


# READ INPUT FILES:

# file name:  / R label: 

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# DECREASE RESOLUTION FOR ORIGINAL RASTERS FROM 30M TO 360M
# ----------------------------------------------
# ----------------------------------------------
# # Protected Lands yes/no '0', '1'
# pl <- 	raster(paste0(BR_fileLoc, "plL.tif" )) 

# # Protected Lands by YEAR 
# pl_year <- 	raster(paste0(BR_fileLoc, "pl_yearL.tif" )) # nodata, years, 9999
# # pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" )) # 0, years, 9999 # Raster reclassified so background =zero. This is at 360*360 resolution

# # Unique Patch ID raster- patches distinguished by YEAR
# yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))


# pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcdL.tif" ))#dev of nlcd will be created in raster rel script.
# pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_erL.tif"   ))
# pl_gap 	<- 	raster(paste0(BR_fileLoc, "pl_gapL.tif"  ))
# pl_own 	<- 	raster(paste0(BR_fileLoc, "pl_ownL.tif"  ))
# pl_pp   <-	raster(paste0(BR_fileLoc, "pl_ppL.tif"   ))
# pl_state <- raster(paste0(BR_fileLoc, "pl_stateL.tif"))
# pl_type <- 	raster(paste0(BR_fileLoc, "pl_typeL.tif" ))
# pl_resil <- 	raster(paste0(BR_fileLoc, "pl_resilL.tif" ))



BR_files<-list.files(path = "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/", pattern = ".img$")
BR_filename<-lapply(BR_files, str_sub, start=1, end=-5) #remove file extension.

BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"


for(f in 1:length(BR_files)){
	print(paste0(BR_fileLoc, BR_filename[f]))
	
	BR_rasters <- raster(paste0(BR_fileLoc, BR_files[f]))
		
	# Decrease resolution to 180
	BR_rasters <- aggregate(BR_rasters, fact=6, fun=modal)

	# WRITE TO FILE
	writeRaster(BR_rasters,filename= paste0(BR_fileLoc, BR_filename[f], "L.tif"), format='GTiff', overwrite=TRUE)
	
	}
	

	pl   <-	raster(paste0(BR_fileLoc, "pl.img"))
	
	# Decrease resolution to 180
	plL <- aggregate(pl, fact=6, fun=modal)
	
	# WRITE TO FILE
	writeRaster(plL,filename= paste0(BR_fileLoc,"pl_L.tif"), format='GTiff', overwrite=TRUE)
	
	pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.img"))
	pl_ppTEST <- aggregate(pl_pp, fact=6, fun=modal)

	# WRITE TO FILE
	writeRaster(pl_ppTEST,filename= paste0(BR_fileLoc,"pl_ppTEST.tif"), format='GTiff', overwrite=TRUE)
	
	
		plsm   <-	raster(paste0(BR_fileLoc, "plsm.tif"))

	
	
	
	
	
	
	
	
	
	
	
	
	
	

	
	