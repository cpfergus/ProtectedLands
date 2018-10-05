############################ 
#PURPOSE: Prep data and build tables for use in Getis Ord analysis and ClassStats
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

library(dplyr)

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
# NEAREST DISTANCE BETWEEN PATCHES in km.
# ----------------------------------------------

# # !! Use new yearly rasters that have no core area patches removed. Use these files: sNCpl_80, sNCpl_85, sNCpl_90, sNCpl_95, sNCpl_00, sNCpl_05, sNCpl_10, sNCpl_15. # Class is defined as year (in 5 year increments). These were generated at the end of the PatchStats script.


# ***INCLUDES BLUE RIDGE AND PIEDMONT, WITH PATCHES THAT OVERLAP BOUNDARY (SEE ABOVE)
# ----------------------------------------------
# ----------------------------------------------

# # USE ARC GIS TO CONVERT TO POLYGON AND COMPUTE MIN DISTANCE. 
# Time start 50 feature - 19:50:32

# 1. convert raster to integer tool: Int
# 2. Raster to Polygon 
# 3. Generate Near Table - use meters as unit
# 4. Load into arc map and export as .txt
# 5. Dissolve polygons by patch ID in preparation for next steps


# ----------------------------------------------
# ----------------------------------------------



# table with link to patch id. # This was generated using ArcMap Generate NEar Table
pat <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/sNCyrly_patid_15I.txt", sep=",", header=TRUE) # this is the exported table from the polygon layer used to calculate NN

# the min distance in Meters
dis <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/sncyrly_patid_near.txt", sep=",", header=TRUE)

# > str(pat)
# 'data.frame':	3570 obs. of  2 variables:
 # $ FID     : int  0 1 2 3 4 5 6 7 8 9 ...
 # $ GRIDCODE: int  1 11 17 49 64 69 97 120 145 158 ...
 
# > str(dis)
# 'data.frame':	282500 obs. of  6 variables:
 # $ Rowid_   : logi  NA NA NA NA NA NA ...
 # $ OBJECTID : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ IN_FID   : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ NEAR_FID : int  4 3 1 14 5 13 8 2 18 7 ...
 # $ NEAR_DIST: num  0 761 2956 3938 4002 ...
 # $ NEAR_RANK: int  1 2 3 4 5 6 7 8 9 10 ...


patx<-pat[,c(1,3)]
colnames(patx) <-c("near_FID", "patchID")

disx<-dis[,3:6]
colnames(disx) <-c("FID", "near_FID", "near_dist", "near_rank")

pat_dis <- full_join(patx, disx, by="near_FID")

str(pat_dis)

# NEW:
# > str(pat_dis)
# 'data.frame':	282501 obs. of  5 variables:
 # $ near_FID : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID  : int  4753 4753 4753 4753 4753 4753 4753 4753 4753 4753 ...
 # $ FID      : int  1 2 3 4 5 6 7 8 9 12 ...
 # $ near_dist: num  2956 5135 761 0 4002 ...
 # $ near_rank: int  6 10 2 1 7 8 8 11 8 22 ...
 
# rename for second join
pat2<-patx
colnames(pat2) <-c("FID", "patchID")

pat_dis2 <- full_join(pat2, pat_dis, by="FID")
# > str(pat_dis2)
# 'data.frame':	282501 obs. of  6 variables:
 # $ FID      : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID.x: int  4753 4753 4753 4753 4753 4753 4753 4753 4753 4753 ...
 # $ near_FID : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ patchID.y: int  4752 7934 4092 4753 6798 5140 8595 8596 3479 1774 ...
 # $ near_dist: num  2956 5135 761 0 4002 ...
 # $ near_rank: int  3 8 2 1 5 13 10 7 11 49 ...
 
 pat_dis2<-pat_dis2[,c(2,4:6)]
 colnames(pat_dis2) <- c("patchID", "d_patchID", "near_dist", "near_rank")

# # select only those with core area 
pat_distm<-filter(pat_dis2, patchID %in% core_pat$patchID)
pat_distm2<-filter(pat_distm,d_patchID %in% core_pat$patchID) 

pat_distm3 <- pat_distm2[,1:3]

# sort by patchID
pat_distm3 <- pat_distm3[order(pat_distm3$patchID),] 

#remove where patchID=d_patchID
pat_distm4 <- pat_distm3[!pat_distm3$patchID==pat_distm3$d_patchID,]

 # str(pat_distm4)
# 'data.frame':	252929 obs. of  3 variables:
 # $ patchID  : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ d_patchID: int  3765 4727 945 2904 945 4501 4501 3767 4727 2904 ...
 # $ near_dist: num  19521 20570 20020 18424 16521 ...

 # WRITE TO FILE
write.table(pat_distm4, file = paste0(Output_Folder,"pat_distm4.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
pat_distm4<-read.table(paste0(Output_Folder,"pat_distm4.txt"), sep=",", header=TRUE)

# Create Matrix to fill in
u_p <- unique(pat_distm4$patchID)

library(stringr)

  #create empty  matrix
 	pat_dist_min <- matrix(nrow = length(unique(pat_distm4$patchID)),  ncol = 4)
	colnames(pat_dist_min) <- c("patchID", "min_dist_km", "d_patchID", "numDP")

  
 # Where did it come from??
for(i in 1:length(u_p)){
	pat_dist_min[i,1] <- u_p[i]
	ttt<-as.character(c(est_yr$patchID[est_yr[est_yr$patchID,"estYr"] <= est_yr[est_yr$patchID==pat_dist_min[i,1],"estYr"]]))
	pat_sel<- filter(pat_distm4, patchID==u_p[i])
	try(pat_dist_min[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# This next line selects the id of the closest patch:
	try(pat_dist_min[i,3] <- str_c(unique(pat_sel[pat_sel$near_dist==pat_dist_min[i,2], "d_patchID"]), collapse=","))
	try(pat_dist_min[i,4] <- length(unlist(strsplit(pat_dist_min[i,3], ","))))
}
# # error message is ok. means it couldn't find a value. that is why I included the try() wrapper - so that the function would keep going. 
  
 # Remove rows with NAs
pat_dist_min <-  na.omit(pat_dist_min) #4104 obs
  
 # convert data types
pat_dist_min<- as.data.frame(pat_dist_min)	
pat_dist_min$numDP <- as.numeric(as.character(pat_dist_min$numDP))
pat_dist_min$min_dist_km <- as.numeric(as.character(pat_dist_min$min_dist_km))
pat_dist_min$min_dist_km<-pat_dist_min$min_dist_km/1000

str(pat_dist_min)
# 'data.frame':	3531 obs. of  4 variables:
 # $ patchID    : Factor w/ 3531 levels "10002","10003",..: 283 471 1507 2061 2245 3398 315 395 428 692 ...
 # $ min_dist_km: num  7.47 7.47 0.179 0.179 0.906 ...
 # $ d_patchID  : Factor w/ 2151 levels "1","10011","10086",..: 212 52 1499 1120 2117 1623 155 80 413 172 ...
 # $ numDP      : num  1 1 1 1 1 1 1 1 1 1 ...
 
# WRITE TO FILE
write.table(pat_dist_min, file = paste0(Output_Folder,"pat_dist_min.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)

# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------

> str(pat_distm4)
# 'data.frame':	252930 obs. of  3 variables:
 # $ patchID  : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ d_patchID: int  3765 4727 945 2904 945 4501 4501 3767 4727 2904 ...
 # $ near_dist: num  19521 20570 20020 18424 16521 ...
> str(pat_dist_min)
# 'data.frame':	3531 obs. of  4 variables:
 # $ patchID    : int  11 17 49 64 69 97 120 145 158 250 ...
 # $ min_dist_km: num  7.47 7.47 0.179 0.179 0.906 ...
 # $ d_patchID  : Factor w/ 2151 levels "1","10011","10086",..: 212 52 1499 1120 2117 1623 155 80 413 172 ...
 # $ numDP      : int  1 1 1 1 1 1 1 1 1 1 ...
 
 iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")
 
 # remove estYr = 9999
 iStats_corepat_2015 <- filter(iStats_corepat, estYr < 9999)
 
> str(iStats_corepat)
# 'data.frame':	4155 obs. of  17 variables:
 # $ patchID          : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ n.cell           : int  151 19 57 272 48 365 349 190 21 503 ...
 # $ n.core.cell      : int  61 1 15 8 4 109 96 3 2 75 ...
 # $ n.edges.perimeter: int  116 32 52 418 64 282 328 368 26 628 ...
 # $ n.edges.internal : int  488 44 176 670 128 1178 1068 392 58 1384 ...
 # $ area             : num  4892400 615600 1846800 8812800 1555200 ...
 # $ core.area        : int  1976400 32400 486000 259200 129600 3531600 3110400 97200 64800 2430000 ...
 # $ perimeter        : int  20880 5760 9360 75240 11520 50760 59040 66240 4680 113040 ...
 # $ perim.area.ratio : num  0.00427 0.00936 0.00507 0.00854 0.00741 ...
 # $ shape.index      : num  2.32 1.78 1.62 6.33 2.29 ...
 # $ frac.dim.index   : num  1.11 1.09 1.08 1.23 1.12 ...
 # $ core.area.index  : num  0.404 0.0526 0.2632 0.0294 0.0833 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
 # $ estYr            : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 # $ area.ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 # $ core.area.ha     : num  197.64 3.24 48.6 25.92 12.96 ...

 # str(iStats_corepat_2015)
# 'data.frame':	3570 obs. of  17 variables:
 # $ patchID          : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ n.cell           : int  151 19 57 272 48 365 349 190 21 503 ...
 # $ n.core.cell      : int  61 1 15 8 4 109 96 3 2 75 ...
 # $ n.edges.perimeter: int  116 32 52 418 64 282 328 368 26 628 ...
 # $ n.edges.internal : int  488 44 176 670 128 1178 1068 392 58 1384 ...
 # $ area             : num  4892400 615600 1846800 8812800 1555200 ...
 # $ core.area        : int  1976400 32400 486000 259200 129600 3531600 3110400 97200 64800 2430000 ...
 # $ perimeter        : int  20880 5760 9360 75240 11520 50760 59040 66240 4680 113040 ...
 # $ perim.area.ratio : num  0.00427 0.00936 0.00507 0.00854 0.00741 ...
 # $ shape.index      : num  2.32 1.78 1.62 6.33 2.29 ...
 # $ frac.dim.index   : num  1.11 1.09 1.08 1.23 1.12 ...
 # $ core.area.index  : num  0.404 0.0526 0.2632 0.0294 0.0833 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
 # $ estYr            : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 # $ area.ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 # $ core.area.ha     : num  197.64 3.24 48.6 25.92 12.96 ...
 
# ----------------------------------------------
 # Join distance to patch stats:
 joinA <- full_join(iStats_corepat_2015, pat_dist_min)
 
str(joinA)
# 'data.frame':	3570 obs. of  20 variables:
 # $ patchID          : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ n.cell           : int  151 19 57 272 48 365 349 190 21 503 ...
 # $ n.core.cell      : int  61 1 15 8 4 109 96 3 2 75 ...
 # $ n.edges.perimeter: int  116 32 52 418 64 282 328 368 26 628 ...
 # $ n.edges.internal : int  488 44 176 670 128 1178 1068 392 58 1384 ...
 # $ area             : num  4892400 615600 1846800 8812800 1555200 ...
 # $ core.area        : int  1976400 32400 486000 259200 129600 3531600 3110400 97200 64800 2430000 ...
 # $ perimeter        : int  20880 5760 9360 75240 11520 50760 59040 66240 4680 113040 ...
 # $ perim.area.ratio : num  0.00427 0.00936 0.00507 0.00854 0.00741 ...
 # $ shape.index      : num  2.32 1.78 1.62 6.33 2.29 ...
 # $ frac.dim.index   : num  1.11 1.09 1.08 1.23 1.12 ...
 # $ core.area.index  : num  0.404 0.0526 0.2632 0.0294 0.0833 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
 # $ estYr            : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 # $ area.ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 # $ core.area.ha     : num  197.64 3.24 48.6 25.92 12.96 ...
 # $ min_dist_km      : num  NA 7.47 7.47 0.179 0.179 ...
 # $ d_patchID        : Factor w/ 2151 levels "1","10011","10086",..: NA 212 52 1499 1120 2117 1623 155 80 413 ...
 # $ numDP            : int  NA 1 1 1 1 1 1 1 1 1 ...
 
 #ID the maximum number of unique numDPs:
# max(pat_dist_min$numDP) #OLD: 32, NEW: 26

# ----------------------------------------------
# remember, this excludes unknowns and patches with no core area

# TRY TO IDENTIFY AREAS WITH THE MOST FOCUS
# create table for moran's I analysis.
# (1) Need to filter by distance <= 500m 
# (2) Connect area of patch to table. Weight = inverse area
 


# reshape 

library(reshape)

test<-cast(pat_distm4, patchID~d_patchID, min)# ~ 9 minutes
npat <- test


# create new column with the number of times distance >= 500
npat$pat_ct500 <- apply(test,1,function(x) sum(x <=500)) 
npat$pat_ct1000 <- apply(test,1,function(x) sum(x <=1000)) 

summary(npat$pat_ct500)
 # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.471   2.000  33.000 
summary(npat$pat_ct1000)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   1.000   2.000   2.239   3.000  40.000 
  
# Select the columns to join in each df
npat2 <- select(npat, patchID, pat_ct500, pat_ct1000)
joinA2 <- select(joinA, patchID, estYr, min_dist_km, d_patchID, area.ha)

# Join to test by patchID
moran <- full_join(joinA2, npat2)

# Create column for inverse area
moran$inv_area <- 1/moran$area.ha

# create weighted pat_cnt column
moran$wpat_ct500 <- moran$pat_ct500 * moran$inv_area
moran$wpat_ct1000 <- moran$pat_ct1000 * moran$inv_area

#save to it can be joined to polygon layer in ArcGIS 
# WRITE TO FILE
write.table(moran, file = "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/moran.txt", row.names=FALSE, sep=",")

# READ TO FILE
moran<-read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/moran.txt", sep=",", header=TRUE)

str(moran) #NEW

# 'data.frame':	3570 obs. of  10 variables:
 # $ patchID    : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ estYr      : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 # $ min_dist_km: num  NA 7.47 7.47 0.179 0.179 ...
 # $ d_patchID  : Factor w/ 2151 levels "1","10011","10086",..: NA 212 52 1499 1120 2117 1623 155 80 413 ...
 # $ area.ha    : num  489.2 61.6 184.7 881.3 155.5 ...
 # $ pat_ct500  : int  1 0 0 1 1 0 0 0 0 0 ...
 # $ pat_ct1000 : int  2 0 0 1 1 1 1 0 0 0 ...
 # $ inv_area   : num  0.00204 0.01624 0.00541 0.00113 0.00643 ...
 # $ wpat_ct500 : num  0.00204 0 0 0.00113 0.00643 ...
 # $ wpat_ct1000: num  0.00409 0 0 0.00113 0.00643 ...

 
 # ----------------------------------------------
 # NOW JOIN TO THE POLYGON LAYERS so can run Getis Ord  (ArcMap)
 # sNCyrly_patid_85idiz # on years less than 2015, select option to "keep only matching records"
 # sNCyrly_patid_95idiz
 # sNCyrly_patid_05idiz
 # sNCyrly_patid_15idiz
 # ----------------------------------------------

# ----------------------------------------------
# To calc correlations, export table to patch Stats folder as "sNCyrly_pidIJoin_tab.txt"


 jointab<-read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/sNCyrly_pidIJoin_tab.txt", sep=",", header=TRUE)

 plot(jointab$area_ha,jointab$pat_ct1000)

> cor.test(jointab$area_ha,jointab$pat_ct1000)

	# Pearson's product-moment correlation NEW:

# data:  jointab$area_ha and jointab$pat_ct1000
# t = 79.356, df = 5266, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
 # 0.7254178 0.7500240
# sample estimates:
      # cor 
# 0.7379661 

# Strong correlation between area and pat_ct1000


	# # Pearson's product-moment correlation OLD

# data:  jointab$area_ha and jointab$pat_ct1000
# t = 79.418, df = 5227, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
 # 0.7269433 0.7515201
# sample estimates:
     # cor 
# 0.739478 

# Strong correlation between area and pat_ct1000

### NOW FOLLOW GETIS ORD PARAMETER PREP
