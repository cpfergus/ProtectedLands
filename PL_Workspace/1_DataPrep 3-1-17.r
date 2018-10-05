############################ 
#PURPOSE: Prep Data where needed. Reclassify rasters, etc.
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

# Header for what grouped packages are for


# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################



pl_resil_rc <- raster(paste0(BR_fileLoc, "pl_resil_rc.tif" )) # this is reclassified from the original, but is still wrong...

m <- c(0, 1)
rclmat <- matrix(m, ncol=2, byrow=TRUE)

pl_resil <- reclassify(pl_resil_rc, rclmat)	

# WRITE TO FILE
writeRaster(pl_resil,filename= "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/pl_resil.tif", format='GTiff', overwrite=TRUE)
	
### ORIGINAL RESILIENCE CLASS:
# Need to reclassify to remove zero.and to make ordering make sense
# 0   Non-resilient
# 4   Linkage: Vulnerable Portion
# 2   Linkage: Resilient Portion
# 3   Resilient: Not Prioritized
# 12 Resilient: Prioritized for Diversity
# 11 Resilient: Prioritized for Diversity and Concentrated Flow or Riparian Corridor
# 13 Resilient: Prioritized for Concentrated Flow or Riparian Corridor
# 14  Resilient: Prioritized for Diffuse Flow

# m <- c(0, 1, 4, 2, 2, 3, 3, 4, 12, 5, 11, 6, 13, 7, 14, 8)
# rclmat <- matrix(m, ncol=2, byrow=TRUE)

# pl_resil <- reclassify(pl_resil, rclmat)	

m <- c(0, 1, 4, 2, 2, 3, 3, 4, 12, 5, 11, 6, 13, 7, 14, 8)
rclmat <- matrix(m, ncol=2, byrow=TRUE)

# pl_resil <- reclassify(pl_resil, rclmat)	
