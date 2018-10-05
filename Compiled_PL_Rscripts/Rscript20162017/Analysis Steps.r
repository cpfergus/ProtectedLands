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

# Header for what grouped packages are for


# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

# ----------------------------------------------
# READ OUTPUT FILES:

# file name:  / R label: 


# READ INPUT FILES:

# file name:  / R label: 

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

### NEED ###

# # Original Rasters:

# Protected Lands Rasters
# - Protected or Not (1-2)
# ** Increments for patch stats. 
# - Gap Status (1-4)
# - Ownership (Public v. Private)
# - Year of Establishment

# Land Cover Raster
# - Land Fire (reclassified)

# Zones
# - States
# - Ecoregions 
	# - Blue Ridge v Piedmont
	# - Subsections
	
# # Create these rasters:

# - Individual patches based off of Protected or Not
# - Adjacent patches for each 5 year? increment of some time period. Base this off of earliest private conservation establishment
	

# ----------------------------------------------
# ----------------------------------------------
{# RASTER MANIPULATION
# ----------------------------------------------
# ----------------------------------------------

# First set NAs in pl_year within study area to zero. (ArcGIS Raster Calculator - Con("Protected Lands\Raster\pl.tif"==0, 0, "Protected Lands\Raster\pl_year.tif"))

# # Raster reclassified so background =zero
# pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" ))# 0, years, 9999
# Rpl_yr_z <- reclassify(pl_yr_z, rcl=(matrix(c(NA,0), ncol=2)))
# # WRITE TO FILE
# writeRaster(Rpl_yr_z, filename=paste0(BR_fileLoc, "pl_yr_z.tif", sep=""), format="GTiff", overwrite=TRUE)

# - reclassify pl_resil 
rclmat<-matrix(c(0,1,2,7,3,2,4,8,11,5,12,3,13,4,14,6), ncol=2, byrow=TRUE)
pl_resilR<- reclassify(pl_resil, rclmat)
# WRITE TO FILE
writeRaster(pl_resilR, filename=paste0(BR_fileLoc, "pl_resilR.tif", sep=""), format="GTiff", overwrite=TRUE)
pl_resilR <- raster(paste0(BR_fileLoc, "pl_resilR.tif" ))# reclassified version


}



# ----------------------------------------------
# ----------------------------------------------
{# PATCH STATS 
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# SEPARATE RASTERS BY TIME INCREMENTS
# ----------------------------------------------

- Get 4 rasters, one for 1985,1995,2005,2015
- These rasters include the previous years, so they build upon each other

# ----------------------------------------------
# CREATE CHANGE RASTERS FOR EACH TIME INCREMENT
# ----------------------------------------------

- Identify per year the change that occurs within each time increment
- Assign value of '1111' to protected areas that do not change between that time increment
- This results in 3 rasters, each depicting change only that has occurred from that increment and before. 



# ----------------------------------------------
# EXTRACT PATCH ID FOR CHANGE YEARS *only* (No Persistent LU)
# ----------------------------------------------

- Identify the Patch ID within each of the above 3 change rasters
- Assign patch ID to 3 new rasters, each within the 3 time periods

# ----------------------------------------------
# CREATE RASTER OF ADJACENT CELLS 
# ----------------------------------------------

- Use self-defined matrix to create rasters that identify patches which are that distance from persistent protected land in the previous increment

# ----------------------------------------------
# RUN PATCH STATS FOR everything *prior* to 1986
# ----------------------------------------------

# ----------------------------------------------
# 30 YEARS IN 10 YR INCREMENTS
# ----------------------------------------------	

# ----------------------------------------------
# RUN FOR THREE TIME INCREMENTS
# ----------------------------------------------



# ----------------------------------------------
# BIND WITH PATCHES PRIOR TO 1985 AND UNKNOWNS (9999)
# ----------------------------------------------



# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA <1


}









