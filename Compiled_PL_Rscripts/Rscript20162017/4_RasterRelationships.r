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
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# ----------------------------------------------
# OUTPUT FILES:

# Collated table for PatchStats and patch traits:
# maj_categ<-read.table(paste0(Comb_output,"maj_categ.txt"), sep=",", header=TRUE)


# ----------------------------------------------
# INPUT FILES: 

# Protected Lands yes/no '0', '1'
pl <- 	raster(paste0(BR_fileLoc, "pl.tif" )) 

# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) # nodata, years, 9999
# pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" )) # 0, years, 9999 # Raster reclassified so background =zero. This is at 360*360 resolution

# Unique Patch ID raster- patches distinguished by YEAR
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))



pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.tif"   ))
pl_gap 	<- 	raster(paste0(BR_fileLoc, "pl_gap.tif"  ))
pl_own 	<- 	raster(paste0(BR_fileLoc, "pl_own.tif"  ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.tif"   ))
pl_state <- raster(paste0(BR_fileLoc, "pl_state.tif"))
pl_type <- 	raster(paste0(BR_fileLoc, "pl_type.tif" ))
pl_resilR <- raster(paste0(BR_fileLoc, "pl_resilR.tif" ))# reclassified version
# pl_biopri <- raster(paste0(BR_fileLoc, "pl_biopri.tif")) # maybe come back to see note above


	

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# ZONAL STATS 
# LandCover, Ecoregions, GAP status, Ownership
# Public/ Private, State, Type
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# ENTIRE LANDSCAPE BY ECOREGION
# ----------------------------------------------

tot_cnt <- as.data.frame(zonal(pl, pl_er, fun='count', na.rm=TRUE))# ALL LAND
pl_cnt <- as.data.frame(zonal(pl_pp, pl_er, fun='count', na.rm=TRUE))# PL ONLY

Tot_Area_PL <- full_join(tot_cnt, pl_cnt, by="zone")
colnames(Tot_Area_PL) <- c("er", "tot_cnt", "pl_cnt")

Tot_Area_PL$totArm2<-Tot_Area_PL$tot_cnt*32400#Makes sure resolution is right here. 30*30=900, 180*180=32400, 360*360=129600
Tot_Area_PL$PLArm2<-Tot_Area_PL$pl_cnt*32400#Makes sure resolution is right here. 30*30=900, 180*180=32400, 360*360=129600

Tot_Area_PL$totAr.ha<-Tot_Area_PL$tot_cnt*32400/10000#Makes sure resolution is right here. 30*30=900, 180*180=32400, 360*360=129600
Tot_Area_PL$PLAr.ha<-Tot_Area_PL$pl_cnt*32400/10000#Makes sure resolution is right here. 30*30=900, 180*180=32400,  360*360=129600


# For area of unknown, prot/priv, etc. calculate totals from table.


# ----------------------------------------------
# YEARLY 
# ----------------------------------------------

yrly_vals <- getValues(pl_year)

unique(yrly_vals) 

yrly_p <- setValues(yrly_patID, yrly_vals) 

est_yr <- as.data.frame(zonal(yrly_p, yrly_patID, na.rm=TRUE))

colnames(est_yr) <- c("patchID", "estYr")

# WRITE TO FILE
write.table(est_yr, file = paste0(Output_Folder,"est_yr",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
est_yr<-read.table(paste0(Output_Folder,"est_yr",".txt"), sep=",", header=TRUE)



# ----------------------------------------------
# BY PATCH
# ----------------------------------------------

library(dplyr)
u_patch <- unique(yrly_patID)

rasters <- c(pl_nlcd, pl_er, pl_gap, pl_own, pl_pp, pl_state, pl_type, pl_resilR)
names(rasters)<-c("pl_nlcd", "pl_er", "pl_gap", "pl_own", "pl_pp", "pl_state", "pl_type", "pl_resilR")


# rasters <- c(pl_gap, pl_type)
# names(rasters)<-c("pl_gap", "pl_type")



ras_patches <- list()
n_ps <- 1
old<-Sys.time()
for(f in 1:length(rasters)){
	print(paste0("start:",names(rasters[f])))
	ras_vals <- getValues(rasters[[f]])
	u_vals <- unique(ras_vals)[-1]


		ras_patch <- list()
		n_p <- 1
		for(categ in u_vals){
		
		print(paste0(names(rasters[f]),":",categ))

			categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
			categ_p <- setValues(rasters[[f]], categ_val)
			ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, yrly_patID, fun='count', na.rm=TRUE))
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

maj_categ <- bind_cols(maj_categ)
maj_categ <- maj_categ[,c(1:4, 6:8, 10:12, 14:16, 18:20, 22:24, 26:28, 30:32)]# This has to be changed if you add more raster layers.
colnames(maj_categ)[colnames(maj_categ)=="nlcd_patchID"] <- "patchID"

# WRITE TO FILE
write.table(maj_categ, file = paste0(Output_Folder,"maj_categ",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

# > str(maj_categ)
# 'data.frame':	14635 obs. of  25 variables:
 # $ patchID    : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ nlcd_maj   : int  5 5 0 0 0 0 0 0 0 0 ...
 # $ nlcd_ha    : num  456.84 16.2 3.24 12.96 12.96 ...
 # $ nlcd_prop  : num  99.3 100 0 0 0 ...
 # $ er_maj     : int  1 1 0 0 1 0 0 0 0 0 ...
 # $ er_ha      : num  437.4 16.2 3.24 12.96 16.2 ...
 # $ er_prop    : num  95.1 100 0 0 100 ...
 # $ gap_maj    : int  2 2 0 0 2 0 0 0 0 0 ...
 # $ gap_ha     : num  460.08 16.2 3.24 12.96 16.2 ...
 # $ gap_prop   : num  100 100 0 0 100 0 0 0 0 0 ...
 # $ own_maj    : int  1 1 0 0 1 0 0 0 0 0 ...
 # $ own_ha     : num  460.08 16.2 3.24 12.96 16.2 ...
 # $ own_prop   : num  100 100 0 0 100 0 0 0 0 0 ...
 # $ pp_maj     : int  1 1 0 0 1 0 0 0 0 0 ...
 # $ pp_ha      : num  460.08 16.2 3.24 12.96 16.2 ...
 # $ pp_prop    : num  100 100 0 0 100 0 0 0 0 0 ...
 # $ state_maj  : int  6 6 0 0 6 0 0 0 0 0 ...
 # $ state_ha   : num  460.08 16.2 3.24 12.96 16.2 ...
 # $ state_prop : num  100 100 0 0 100 0 0 0 0 0 ...
 # $ type_maj   : int  2 2 0 0 2 0 0 0 0 0 ...
 # $ type_ha    : num  460.08 16.2 3.24 12.96 16.2 ...
 # $ type_prop  : num  100 100 0 0 100 0 0 0 0 0 ...
 # $ resilR_maj : int  3 0 0 0 3 0 0 0 0 0 ...
 # $ resilR_ha  : num  421.2 12.96 3.24 12.96 16.2 ...
 # $ resilR_prop: num  91.5 0 0 0 100 ...



# ----------------------------------------------
# NEAREST DISTANCE BETWEEN PATCHES in km.
# ----------------------------------------------

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)


# First remove patches that are too small, then use ARc to calc NN
# Join to get est_Yr and core.area
temp <- full_join(est_yr, iStats_all, by="patchID")

core_pat<-filter(temp, core.area >= 1) 
ncor <-filter(temp, core.area == 0) 

NCyrly_patID<-yrly_patID
NCyrly_patID[yrly_patID %in% ncor$patchID] <- NA

# Remove unknowns (9999) done at plotting.


# # WRITE TO FILE - IS SAVED OVER BELOW WITH REPROJECTED VERSION.
# writeRaster(NCyrly_patID, filename=paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""), format='GTiff', overwrite=TRUE)
	
# # READ FROM FILE
# NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""))

# > length(unique(NCyrly_patID))
# [1] 4152
# > str(pat_dist_min)
# 'data.frame':	4104 obs. of  3 variables:

# ----------------------------------------------
# ----------------------------------------------
# # USE ARC GIS TO CONVERT TO POLYGON AND COMPUTE MIN DISTANCE. 
# Time start 50 feature - 19:50:32

# 1. project to NAD UTM 17
# 2. convert raster to integer tool: Int
# 3. Raster to Polygon
# 4. Generate Near Table - use kilometers as unit

# ----------------------------------------------
# ----------------------------------------------

library(dplyr)

temp <- full_join(est_yr, iStats_all, by="patchID")
core_pat<-filter(temp, core.area >= 1) # no core area filtered out before nn calc - used NC yrly raster.

# table with link to patch id
pat <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/NCyrly_patID_p.txt", sep=",", header=TRUE)

# the min distance in Meters
dis <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/cmp_dist_minARCdd7.txt", sep=",", header=TRUE)


pat<-pat[,c(1,3)]
colnames(pat) <-c("near_FID", "patchID")

dis<-dis[,2:5]
colnames(dis) <-c("FID", "near_FID", "near_dist", "near_rank")

pat_dis <- full_join(pat, dis, by="near_FID")

str(pat_dis)
# 'data.frame':	336750 obs. of  5 variables:
 # $ near_FID : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID  : int  10452 10452 10452 10452 10452 10452 10452 10452 10452 10452 ...
 # $ FID      : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ near_dist: num  1584 6945 12370 5544 10143 ...
 # $ near_rank: int  3 22 28 16 28 26 27 9 6 32 ...
 
# rename for second join
pat2<-pat
colnames(pat2) <-c("FID", "patchID")

pat_dis2 <- full_join(pat2, pat_dis, by="FID")
 str(pat_dis2)
# 'data.frame':	336750 obs. of  6 variables:
 # $ FID      : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID.x: int  10452 10452 10452 10452 10452 10452 10452 10452 10452 10452 ...
 # $ near_FID : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ patchID.y: int  3135 3964 8449 4614 6640 8449 4991 8448 7789 7793 ...
 # $ near_dist: num  1584 6945 12370 5544 10143 ...
 # $ near_rank: int  3 13 32 8 28 34 40 4 2 36 ...
 
 pat_dis2<-pat_dis2[,c(2,4:6)]
 colnames(pat_dis2) <- c("patchID", "d_patchID", "near_dist", "near_rank")

# select only those with core area
pat_distm<-filter(pat_dis2, patchID %in% core_pat$patchID)
pat_distm2<-filter(pat_distm,d_patchID %in% core_pat$patchID) 
pat_distm3 <- pat_distm2[,1:3]

# sort by patchID
pat_distm3 <- pat_distm3[order(pat_distm3$patchID),] 

#remove where patchID=d_patchID
pat_distm4 <- pat_distm3[!pat_distm3$patchID==pat_distm3$d_patchID,]


# Create Matrix to fill in
u_p <- unique(pat_distm4$patchID)

# pat_dist_min <- matrix(nrow = length(unique(pat_distm4$patchID)), ncol = 3)
# colnames(pat_dist_min) <- c("patchID", "min_dist.km", "d_patchID")

library(stringr)

# old<-Sys.time()
# for(i in 1:length(u_p)){
	# pat_dist_min[i,1] <- u_p[i]
	# ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[i,1],"estYr"]]))
	# pat_sel<- filter(pat_distm4, patchID==u_p[i])
	# try(pat_dist_min[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# # This next line selects the id of the closest patch:
	# try(pat_dist_min[i,3] <- str_c(unique(pat_sel[pat_sel$near_dist==pat_dist_min[i,2], "d_patchID"]), collapse=","))
# }
# new<-Sys.time()-old
# print(new) #~Time difference of 41.46715 secs 
# # error message is ok. means it couldn't find a value. that is why i included the try() wrapper - so that the function would keep going. 
	
# pat_dist_min<- as.data.frame(pat_dist_min)	
	
	
 # USING REAL DATA:
  #create mini matrix
 	pat_dist_min <- matrix(nrow = length(unique(pat_distm4$patchID)),  ncol = 4)
	colnames(pat_dist_min) <- c("patchID", "min_dist.km", "d_patchID", "numDP")

 # u_p <-u_p[94:95]
 
 # Where did it come from??
for(i in 1:length(u_p)){
	pat_dist_min[i,1] <- u_p[i]
	ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[i,1],"estYr"]]))
	pat_sel<- filter(pat_distm4, patchID==u_p[i])
	try(pat_dist_min[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# This next line selects the id of the closest patch:
	try(pat_dist_min[i,3] <- str_c(unique(pat_sel[pat_sel$near_dist==pat_dist_min[i,2], "d_patchID"]), collapse=","))
	try(pat_dist_min[i,4] <- length(unlist(strsplit(pat_dist_min[i,3], ","))))
}
  
 # Remove rows with NAs
pat_dist_min <-  na.omit(pat_dist_min) #4104 obs
  
 # convert data types
pat_dist_min<- as.data.frame(pat_dist_min)	
pat_dist_min$numDP <- as.numeric(as.character(pat_dist_min$numDP))
pat_dist_min$min_dist.km <- as.numeric(as.character(pat_dist_min$min_dist.km))
pat_dist_min$min_dist.km<-pat_dist_min$min_dist.km/1000



# WRITE TO FILE
write.table(pat_dist_min, file = paste0(Output_Folder,"pat_dist_min.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)

# str(pat_dist_min)
# 'data.frame':	4104 obs. of  4 variables:
 # $ patchID    : int  15 20 62 70 77 101 129 150 186 264 ...
 # $ min_dist.km: num  7.552 7.552 0.18 0.18 0.899 ...
 # $ d_patchID  : Factor w/ 2529 levels "1","1003","10040",..: 565 421 1994 1777 13 2150 422 242 773 520 ...
 # $ numDP      : int  1 1 1 1 1 1 1 1 1 1 ...

 
 # TRY TO IDENTIFY AREAS WITH THE MOST FOCUS
 
 
#ID the maximum number of unique numDPs:
# max(pat_dist_min$numDP) #29
 
 
 
 

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


# ----------------------------------------------
# ----------------------------------------------
# JOIN TABLES BY PATCH ID - WITH Buffer Area
# ----------------------------------------------
# ----------------------------------------------

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")
maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)
nldev_dist<-read.table(paste0(Output_Folder,"nldev_distL.txt"), sep=",", header=TRUE)
# cmp_dist_min<-read.table(paste0(Output_Folder,"cmp_dist_min.txt"), sep=",", header=TRUE)
pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)



#QC:
str(iStats_all); str(maj_categ); str(est_yr); str(nldev_dist); str(pat_dist_min)
# unique(iStats_all$patchID); unique(maj_categ$patchID); unique(est_yr$patchID); unique(nldev_dist$patchID); unique(pat_dist_min$patchID)


join <- full_join(full_join(full_join(full_join(est_yr, maj_categ, by="patchID"), nldev_dist, by="patchID"), pat_dist_min, by="patchID"), iStats_all, by="patchID")

# str(join)
# 'data.frame':	38717 obs. of  22 variables:

iStats_Join <- filter(join, n.core.cell>=1) 
  
iStats_Join$area.ha <- iStats_Join$area/10000
iStats_Join$core.area.ha <- iStats_Join$core.area/10000


# WRITE TO FILE
write.table(iStats_Join, file = paste0(Output_Folder,"iStats_Join",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)


# AnalysisLoc <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/BasicDataAnalysis"



