########################### 
#PURPOSE: Relate patch raster to additional variables
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:
# For distance to development ** THIS IS LIGHTYEARS FASTER IN ARCGIS. 
#IMPORTANT: 
##### NEXT STEPS #####
# EVENTUALLY Remove all patches that have area < something... core area? # pixels? ** Can do this at the end.
# Correct Output Folder Address - - the forward slash should be included at the end.
# NEAREST DISTANCE BETWEEN PATCHES in km. # Re write code so that it can only relate to patches that were created at or before the same year. 
#Biodiversity - Come back to this later. probably have to look at other spatial data by taxa - pl_biopri <- raster(paste0(BR_fileLoc, "pl_biopri.tif"))


############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 

# ----------------------------------------------
################################################


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

# # ----------------------------------------------
# # OUTPUT FILES:

# # Actual area calcs:
# gap_area<-read.table(paste0(Output_Folder,"gap_area",".txt"), sep=",", header=TRUE)
# own_area<-read.table(paste0(Output_Folder,"own_area",".txt"), sep=",", header=TRUE)
# nlcd_area<-read.table(paste0(Output_Folder,"nlcd_area",".txt"), sep=",", header=TRUE)
# resil_area<-read.table(paste0(Output_Folder,"resil_area",".txt"), sep=",", header=TRUE)

# # Majority of patch:
# maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)


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

# Unique Patch ID raster- patches distinguished by YEAR, no core included.
yrly_patID<- raster(paste0(BR_fileLoc, "yrly_patID.tif", sep=""))
# Unique Patch ID raster- patches distinguished by YEAR, no core removed.
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))

pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.tif"   ))
pl_gap 	<- 	raster(paste0(BR_fileLoc, "pl_gap.tif"  ))
pl_own 	<- 	raster(paste0(BR_fileLoc, "pl_own.tif"  ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.tif"   ))
pl_state <- raster(paste0(BR_fileLoc, "pl_state.tif"))
pl_type <- 	raster(paste0(BR_fileLoc, "pl_type.tif" ))
pl_resil <- raster(paste0(BR_fileLoc, "pl_resil.tif" ))
# pl_biopri <- raster(paste0(BR_fileLoc, "pl_biopri.tif")) # maybe come back to see note above
	

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# ZONAL STATS FOR MAJORITY AND ACTUAL AREA
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# AREA OF ENTIRE LANDSCAPE BY ECOREGION
# ----------------------------------------------
{
tot_cnt <- as.data.frame(zonal(prot_yn, pl_er, fun='count', na.rm=TRUE))# ALL LAND
pl_cnt <- as.data.frame(zonal(pl_pp, pl_er, fun='count', na.rm=TRUE))# PL ONLY

Tot_Area_PL <- full_join(tot_cnt, pl_cnt, by="zone")
colnames(Tot_Area_PL) <- c("er", "tot_cnt", "pl_cnt")

Tot_Area_PL$totArm2<-Tot_Area_PL$tot_cnt*32400#Makes sure resolution is right here. 30*30=900, 180*180=32400, 360*360=129600
Tot_Area_PL$PLArm2<-Tot_Area_PL$pl_cnt*32400#Makes sure resolution is right here. 30*30=900, 180*180=32400, 360*360=129600

Tot_Area_PL$totAr.ha<-Tot_Area_PL$tot_cnt*32400/10000#Makes sure resolution is right here. 30*30=900, 180*180=32400, 360*360=129600
Tot_Area_PL$PLAr.ha<-Tot_Area_PL$pl_cnt*32400/10000#Makes sure resolution is right here. 30*30=900, 180*180=32400,  360*360=129600

# For area of unknown, prot/priv, etc. calculate totals from table.
}

# ----------------------------------------------
# MAJORITY VALUES FOR GAP, TYPE, ECOREGION, OWNERSHIP
# ----------------------------------------------
{

u_patch <- unique(sNCyrly_patID)

rasters <- c(pl_pp, pl_gap, pl_type, pl_er, pl_own, pl_nlcd)
names(rasters)<-c("pl_pp", "pl_gap", "pl_type", "pl_er", "pl_own", "pl_nlcd")


ras_patches <- list()
n_ps <- 1
old<-Sys.time()
for(f in 1:length(rasters)){
	print(paste0("start:",names(rasters[f])))
	ras_vals <- getValues(rasters[[f]])
	u_vals <- sort(unique(ras_vals)[-1])


		ras_patch <- list()
		n_p <- 1
		for(categ in u_vals){
		
		print(paste0(names(rasters[f]),":",categ))

			categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
			categ_p <- setValues(rasters[[f]], categ_val)
			ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, sNCyrly_patID, fun='count', na.rm=TRUE))
			ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
			ras_patch[[n_p]]$Raster <- paste0(names(rasters[f]))
			ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
				n_p <- n_p +1
		}

		ras_patches[[n_ps]] <- bind_rows(ras_patch)
		n_ps <- n_ps +1
}
					
names(ras_patches)<-names(rasters)


library(stringr)		

maj_categ<-list()
r_c<-1
for(f in 1:length(ras_patches)){
	print(paste0("start:",names(ras_patches[f])))
	
	# Create matrices to fill in.
	  maj_categ[[r_c]] <- as.data.frame(matrix(nrow = length(u_patch), ncol = 4))
	  colnames(maj_categ[[r_c]]) <- c(paste0(str_sub(names(ras_patches[f]), start=4), "_patchID"), paste0(str_sub(names(ras_patches[f]), start=4), "_maj"), paste0(str_sub(names(ras_patches[f]), start=4), "_ha"), paste0(str_sub(names(ras_patches[f]), start=4), "_prop")) 
	
		for(p in 1:length(u_patch)){
			patchID <- u_patch[p]
			try(temp<-filter(ras_patches[[f]],ras_patches[[f]]$'zone' == u_patch[[p]]))
			try(area<-max(temp$area.ha))
			try(maj<-ifelse(area>3.24,filter(temp, area.ha == max(temp$area.ha))$Class,0))#Make sure resolution is right here.
			try(prop<-ifelse(area>3.24,100*(max(temp$area.ha)/sum(temp$area.ha)),0))#Make sure resolution is right here.
				maj_categ[[r_c]][p,1] <- patchID
				# maj_categ[[r_c]][p,2] <- str_sub(names(ras_patches[f]), start=4)
				maj_categ[[r_c]][p,2] <- maj
				maj_categ[[r_c]][p,3] <- area
				maj_categ[[r_c]][p,4] <- prop
			}
	print(paste0("finish:",names(ras_patches[f])))
	r_c <- r_c +1
}
new<-Sys.time()-old
print(new)
# Time difference of 5.091626 mins

maj_categ <- bind_cols(maj_categ)
maj_categ <- maj_categ[,c(1:4, 6:8, 10:12, 14:16, 18:20, 22:24)]# This has to be changed if you use less or add more raster layers.
colnames(maj_categ)[colnames(maj_categ)=="pp_patchID"] <- "patchID"

# WRITE TO FILE
write.table(maj_categ, file = paste0(Output_Folder,"maj_categ",".txt"), row.names=FALSE, sep=",")
# READ FROM FILE
maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

 
# ----------------------------------------------
# JOIN TO PATCH STATS TABLE
iStats_majJoin <- full_join(iStats_corepat, maj_categ, by="patchID") 


# WRITE TO FILE
write.table(iStats_majJoin, file = paste0(Output_Folder,"iStats_majJoin",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
iStats_majJoin<-read.table(paste0(Output_Folder,"iStats_majJoin",".txt"), sep=",", header=TRUE)
}

# ----------------------------------------------
# MAJORITY VALUES NLCD - TABLE AND RASTER
# ----------------------------------------------
{
u_patch <- unique(sNCyrly_patID)
yrly_vals <- getValues(sNCyrly_patID)

# CREATE MAJORITY NLCD TABLE
	
	ras_vals <- getValues(pl_nlcd)
	u_vals <- sort(unique(pl_nlcd))


ras_patch <- list()
n_p <- 1
for(categ in u_vals){

print(paste0(names(pl_nlcd),":",categ))

	categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
	categ_p <- setValues(pl_nlcd, categ_val)
	ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, sNCyrly_patID, fun='count', na.rm=TRUE))
	ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
	ras_patch[[n_p]]$Raster <- paste0(names(pl_nlcd))
	ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
		n_p <- n_p +1
}

ras_patches<- bind_rows(ras_patch)


library(stringr)		

# Create matrices to fill in.
nlcd_maj<- as.data.frame(matrix(nrow = length(u_patch), ncol = 4))


for(p in 1:length(u_patch)){
	patchID <- u_patch[p]
	try(temp<-filter(ras_patches,ras_patches$'zone' == u_patch[[p]]))
	try(area<-max(temp$area.ha))
	try(maj<-ifelse(area>3.24,filter(temp, area.ha == max(temp$area.ha))$Class,0))#Make sure resolution is right here.
	try(prop<-ifelse(area>3.24,100*(max(temp$area.ha)/sum(temp$area.ha)),0))#Make sure resolution is right here.
		nlcd_maj[p,1] <- patchID
		# nlcd_maj[p,2] <- str_sub(names(ras_patches), start=4)
		nlcd_maj[p,2] <- maj
		nlcd_maj[p,3] <- area
		nlcd_maj[p,4] <- prop
	}
	
colnames(nlcd_maj) <-  c("patchID", "nlcd_maj", "nlcd_ha", "nlcd_prop")
	
	
# WRITE TO FILE
write.table(nlcd_maj, file = paste0(Output_Folder,"nlcd_maj",".txt"), row.names=FALSE, sep=",")
# READ FROM FILE
nlcd_maj<-read.table(paste0(Output_Folder,"nlcd_maj.txt"), sep=",", header=TRUE)

# ----------------------------------------------
# RECLASSIFY TO NLCD MAJ VALUES		
	nlrecl <- nlcd_maj[,1:2]
			
	nlcd_maj_ras <- reclassify(sNCyrly_patID, nlrecl)		

	# WRITE TO FILE
	writeRaster(nlcd_maj_ras, filename=paste0(BR_fileLoc, "nlcd_maj_ras.tif", sep=""), format='GTiff', overwrite=TRUE)
	# READ FROM FILE
	sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))


	
# ----------------------------------------------
# JOIN TO PATCH STATS TABLE

iStats_majJoin <- full_join(iStats_majJoin, nlcd_maj, by="patchID") 

# WRITE TO FILE
write.table(iStats_majJoin, file = paste0(Output_Folder,"iStats_majJoin",".txt"), row.names=FALSE, sep=",")
# READ FROM FILE
iStats_majJoin<-read.table(paste0(Output_Folder,"iStats_majJoin.txt"), sep=",", header=TRUE)	
}	

# ----------------------------------------------
# ACTUAL AREA VALUES FOR GAP, OWNERSHIP, LAND COVER, RESILIENCY
# ----------------------------------------------
{

rasters <- c(pl_gap, pl_own, pl_nlcd, pl_resil)
names(rasters)<-c("pl_gap", "pl_own", "pl_nlcd", "pl_resil")

# ras_vals <- getValues(pl_gap)
# > unique(ras_vals)
# [1] NA  3  4  2  1
# > ras_vals <- getValues(pl_nlcd)
# > unique(ras_vals)
# [1] NA  6  2  5  3  7  1  4  8
# > ras_vals <- getValues(pl_own)
# > unique(ras_vals)
# [1] NA  1  2  4  3
# > ras_vals <- getValues(pl_resil)
# > unique(ras_vals)
# [1] NA  1  2  3  7  8  4  5  6


ras_patches <- list()
n_ps <- 1
old<-Sys.time()
for(f in 1:length(rasters)){
	print(paste0("start:",names(rasters[f])))
	ras_vals <- getValues(rasters[[f]])
	u_vals <- sort(unique(ras_vals)[-1])
	
		ras_patch <- list()
		n_p <- 1
		for(categ in u_vals){
		
		print(paste0(names(rasters[f]),":",categ))

			categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
			categ_p <- setValues(rasters[[f]], categ_val)
			ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, sNCyrly_patID, fun='count', na.rm=TRUE))
			ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
			ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
				n_p <- n_p +1
		}
		
		ras_patches[[n_ps]] <- bind_rows(ras_patch)
		n_ps <- n_ps +1
}
					
names(ras_patches)<-names(rasters)

# > str(ras_patches)
# List of 4

colnames(ras_patches[[1]]) <- c("patchID", "gap_count", "gap_area_ha", "gap_Class")
colnames(ras_patches[[2]]) <- c("patchID", "own_count", "own_area_ha", "own_Class")
colnames(ras_patches[[3]]) <- c("patchID", "nlcd_count", "nlcd_area_ha", "nlcd_Class")
colnames(ras_patches[[4]]) <- c("patchID", "resil_count", "resil_area_ha", "resil_Class")
 
# WRITE TO FILE
write.table(ras_patches[[1]], file = paste0(Output_Folder,"gap_area",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
gap_area<-read.table(paste0(Output_Folder,"gap_area",".txt"), sep=",", header=TRUE)

# WRITE TO FILE
write.table(ras_patches[[2]], file = paste0(Output_Folder,"own_area",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
own_area<-read.table(paste0(Output_Folder,"own_area",".txt"), sep=",", header=TRUE)

# WRITE TO FILE
write.table(ras_patches[[3]], file = paste0(Output_Folder,"nlcd_area",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
nlcd_area<-read.table(paste0(Output_Folder,"nlcd_area",".txt"), sep=",", header=TRUE)

# WRITE TO FILE
write.table(ras_patches[[4]], file = paste0(Output_Folder,"resil_area",".txt"), row.names=FALSE, sep=",")
# READ TO FILE 
resil_area<-read.table(paste0(Output_Folder,"resil_area",".txt"), sep=",", header=TRUE)

#QC:
# str(gap_area); str(own_area); str(nlcd_area); str(resil_area)
# 'data.frame':	16484 obs. of  4 variables:
 # $ patchID    : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ gap_count  : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_area_ha: num  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_Class  : int  2 2 2 2 2 2 2 2 2 2 ...
# 'data.frame':	16484 obs. of  4 variables:
 # $ patchID    : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ own_count  : int  151 19 57 272 48 365 349 190 21 503 ...
 # $ own_area_ha: num  489.2 61.6 184.7 881.3 155.5 ...
 # $ own_Class  : int  1 1 1 1 1 1 1 1 1 1 ...
# 'data.frame':	32968 obs. of  4 variables:
 # $ patchID     : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ nlcd_count  : int  0 16 57 252 41 356 336 174 21 488 ...
 # $ nlcd_area_ha: num  0 51.8 184.7 816.5 132.8 ...
 # $ nlcd_Class  : int  1 1 1 1 1 1 1 1 1 1 ...
# 'data.frame':	32968 obs. of  4 variables:
 # $ patchID      : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ resil_count  : int  13 0 2 15 0 1 6 9 2 7 ...
 # $ resil_area_ha: num  42.12 0 6.48 48.6 0 ...
 # $ resil_Class  : int  1 1 1 1 1 1 1 1 1 1 ...
 
 }
 
# ----------------------------------------------
# ACTUAL AREA OF RESILIENCE CLASSES IN STUDY AREA (BLUE RIDGE AND PIEDMONT)
# ** consider masking pl_er to only 3 regions in study area. right now does all 10.
# ----------------------------------------------
{
ras_vals <- getValues(pl_resil)
u_vals <- unique(ras_vals)[-1] # remove NA

ras_patch <- list()
n_p <- 1
for(categ in u_vals){
print(u_vals[categ])		
	categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
	categ_p <- setValues(pl_resil, categ_val)
	ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, pl_er, fun='count', na.rm=TRUE))
	ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
	ras_patch[[n_p]]$Raster <- paste0(names(pl_resil))
	ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
		n_p <- n_p +1
}
ras_patches<- bind_rows(ras_patch)
colnames(ras_patches) <- c("er", "count", "resil_area_ha", "Raster", "Class")
er_resil <- ras_patches


# WRITE TO FILE
write.table(er_resil, file = paste0(Output_Folder,"er_resil",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
er_resil<-read.table(paste0(Output_Folder,"er_resil",".txt"), sep=",", header=TRUE)
 
}
  
# ----------------------------------------------  
# ----------------------------------------------
# JOIN ALL TABLES TOGETHER
# ----------------------------------------------
# ----------------------------------------------
{

bindA <- cbind(gap_area, own_area)
bindA <- bindA[,c(1:4,6:8)]

bindB <- cbind(nlcd_area, resil_area)
bindB <- bindB[,c(1:4,6:8)]

#Join to patchId and estYr columns from iStats_corepat and er_maj from maj_categ
tempA <- select(iStats_corepat,patchID,estYr) #select desired columns 
tempB <- select(maj_categ,patchID,er_maj) #select desired columns # Don't add an area calc since this would be for some patches more than that represented by the categoryical area.
tempAB <- full_join(tempA, tempB)
bindAB <- full_join(bindA, bindB)

GapOwnNlRe_ar_join <- full_join(tempAB, bindAB)

# WRITE TO FILE
write.table(GapOwnNlRe_ar_join, file = paste0(Output_Folder,"GapOwnNlRe_ar_join",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
GapOwnNlRe_ar_join<-read.table(paste0(Output_Folder,"GapOwnNlRe_ar_join",".txt"), sep=",", header=TRUE)
 
# str(GapOwnNlRe_ar_join)
# 'data.frame':	131872 obs. of  15 variables:
 # $ patchID      : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ estYr        : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ er_maj       : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ gap_count    : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_area_ha  : num  0 0 0 0 0 0 0 0 0 0 ...
 # $ gap_Class    : int  2 2 2 2 2 2 2 2 1 1 ...
 # $ own_count    : int  151 151 151 151 151 151 151 151 0 0 ...
 # $ own_area_ha  : num  489 489 489 489 489 ...
 # $ own_Class    : int  1 1 1 1 1 1 1 1 2 2 ...
 # $ nlcd_count   : int  0 1 0 150 0 0 0 0 0 1 ...
 # $ nlcd_area_ha : num  0 3.24 0 486 0 0 0 0 0 3.24 ...
 # $ nlcd_Class   : int  1 2 7 5 4 6 3 8 1 2 ...
 # $ resil_count  : int  13 0 138 0 0 0 0 0 13 0 ...
 # $ resil_area_ha: num  42.1 0 447.1 0 0 ...
 # $ resil_Class  : int  1 2 3 5 6 7 8 4 1 2 ...

} 
 
 
# ----------------------------------------------
# ----------------------------------------------
# CALCULATE LAND COVER IN 1 KM BUFFER THAT SURROUNDS EACH PATCH
# ----------------------------------------------
# ----------------------------------------------
{
 
# INPUT FILES
pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))

# USE ARC GIS TO CREATE BUFFER ZONES WITH UNIQUE PATCH ID:
NCyrly_buff <- raster(paste0(BR_fileLoc, "sNCyrly_pidIdizBuffinv.tif" ))# Unique Patch ID raster- patches distinguished by YEAR

ras_vals <- getValues(pl_nlcd)
u_vals <- unique(ras_vals)[-1] # remove NA

ras_patch <- list()
n_p <- 1
for(categ in u_vals){
print(u_vals[categ])		
	categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
	categ_p <- setValues(pl_nlcd, categ_val)
	ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, NCyrly_buff, fun='count', na.rm=TRUE))
	ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
	ras_patch[[n_p]]$Raster <- paste0(names(pl_nlcd))
	ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
		n_p <- n_p +1
}
ras_patches<- bind_rows(ras_patch)
colnames(ras_patches) <- c("patchID", "count", "nlcdbf_area_ha", "Raster", "Class")

 # Join files:
 JoinA <- full_join(iStats_corepat, maj_categ)
 buff_Join <- full_join(JoinA, ras_patches)

# maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)
# est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

# WRITE TO FILE
write.table(buff_Join, file = paste0(Output_Folder,"nlcd_buff",".txt"), row.names=FALSE, sep=",")
# READ TO FILE
nlcd_buff<-read.table(paste0(Output_Folder,"nlcd_buff",".txt"), sep=",", header=TRUE)
 
 }
 
 
################################################
### OTHER POSSIBILITIES:
################################################

# ----------------------------------------------
# NLCD Distance to Development (nlcd 3) # !Can only have raster package loaded!
{# Estimated time for distance() function = 12 hours
# ----------------------------------------------

nlcd11_vals <- getValues(pl_nlcd)
nldev_vals <- ifelse(nlcd11_vals == 3|is.na(nlcd11_vals),nlcd11_vals,NA) 
nldev <- setValues(pl_nlcd, nldev_vals)
nldev_dis <- distance(nldev)/1000


# WRITE TO FILE
writeRaster(nldev, filename=paste0(BR_fileLoc, "nldev.tif"), format="GTiff", overwrite=TRUE)

# WRITE TO FILE
writeRaster(nldev_dis, filename=paste0(BR_fileLoc, "nldev_dis.tif"), format="GTiff", overwrite=TRUE)

# READ FROM FILE
nldev_dis  <- raster(paste0(BR_fileLoc, "nldev_dis.tif"))

nldev_dist <- as.data.frame(zonal(nldev_dis, yrly_patID, fun='min', na.rm=TRUE))

colnames(nldev_dist) <- c("patchID", "minDevDis")

# WRITE TO FILE
write.table(nldev_dist, file = paste0(Output_Folder,"nldev_distL",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
nldev_dist<-read.table(paste0(Output_Folder,"nldev_distL",".txt"), sep=",", header=TRUE)
}




