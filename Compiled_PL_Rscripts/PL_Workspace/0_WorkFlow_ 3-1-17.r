############################ 
#PURPOSE:Outline the steps with reference to documents, data, and R models
#DEVELOPED: 4-21-17
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
##### NEXT STEPS #####

############################

	
################################################
########        DATA CREATION           ########
################################################


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
{# CREATE RASTERS
# ----------------------------------------------
# ----------------------------------------------


# create pl.tif - now "prot_yn.tif" (in ArcGIS) 
	# 1) Calculate euclidean distance on pl_year
	# 2) Clip to the study area + buffer "pl_er_simplePoly"
	# 3) Convert to integer
	# 4) Classify values into appropriate bins for reclassification
	# 5) Reclassify 0-> 1, the rest-> 0. Must change environments to snap raster.

# ----------------------------------------------
# ---------------------------------------------- 
# Raster reclassified so background =zero (in ArcGIS) 
# ----------------------------------------------
# ----------------------------------------------
 # Set NAs in pl_year within study area to zero. (ArcGIS Raster Calculator) (create pl_yr_z.tif)
	 # Con("Protected Lands\Raster\prot_yn.tif"==0, 0, "Protected Lands\Raster\pl_year.tif")
	
	

}

# ----------------------------------------------
# ----------------------------------------------
# STEP 1. ASSIGN UNIQUE ID TO EACH PATCH BASED ON YEAR OF ESTABLISHMENT
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# R SCRIPT 
# IndYearRasters.r

#PURPOSE: 
#INPUTS:
	# Rasters:  
		# Raster with patch values = year of establishment
			# pl_year <- raster(paste0(BR_fileLoc, "pl_year.tif" ))
		
		
#OUTPUTS:
	# Rasters: 
		# Rasters with patches within each *individual* year (111 total)
		# Raster with patch values = unique ID 
			# yrly_patID<- raster(paste0(BR_fileLoc, "yrly_patID.tif", sep=""))

	

# ----------------------------------------------
# ----------------------------------------------
{# PATCH STATS 
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# CREATE est_yr TABLE
# ----------------------------------------------

est_yr<-read.table(paste0(Output_Folder,"est_yr",".txt"), sep=",", header=TRUE)




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
-  For 1985-1995, 1995-2005, and 2005-2015 change

# ----------------------------------------------
# RUN PATCH STATS FOR everything *prior* to 1986 and Unknown patches
# ----------------------------------------------

# ----------------------------------------------
# RECLASSIFY RASTERS FOR 30 YEARS IN 10 YR INCREMENTS
# ----------------------------------------------	

# ----------------------------------------------
# RUN PATCH STATS FOR THREE TIME INCREMENTS
# ----------------------------------------------
iPatch_inc<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iPatchStats_inc.csv")


# ----------------------------------------------
# BIND WITH PATCHES PRIOR TO 1985 AND UNKNOWNS (9999)
# ----------------------------------------------

# ----------------------------------------------
# JOIN TO EST YEAR FILE
# ----------------------------------------------	

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")	


# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA <1

iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")

# ----------------------------------------------
# REMOVE CORE AREA ==0 FROM RASTER
# ----------------------------------------------
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))


}



# ----------------------------------------------
# ----------------------------------------------
# CLASS STATS: 
# ----------------------------------------------
# ----------------------------------------------

# Class is defined as year (in 5 year increents)

# NOTES: Clip to buffer region to get stats on region? What about the problem of cutting patches in half? Do clip that includes overlapping patches? Cannot do this on a raster. Use the polygon layers that have already been divided into year increments via the getis ord analysis? BUT need further divisiion even... so first further select by attributes to create 4 more increments representing 5 year increments of 90, 00, 10

# Steps:
# 1) Use the select by location tool and click all the layers you want selected from. Use "intersect the source layer feature " and eExport each selection to new layer
# 2) Convert all to raster so can run ClassStats on it. Bulk tool export to this location: BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"


# # # If want by region, edit rasters by clipping to blue ridge and piedmont in ArcGIS
# # pl_yr_zB<-raster(paste0(BR_fileLoc, "pl_yr_zclB.tif")) # this includes 9999, but the selection below does not include this to be used in the analysis.
# # pl_yr_zP<-raster(paste0(BR_fileLoc, "pl_yr_zclP.tif"))# this includes 9999, but the selection below does not include this to be used in the analysis.



