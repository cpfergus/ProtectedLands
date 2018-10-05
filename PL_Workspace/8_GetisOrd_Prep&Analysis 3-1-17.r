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

# ----------------------------------------------
# ----------------------------------------------
# # USE ARC GIS TO CONVERT TO POLYGON AND COMPUTE MIN DISTANCE. 
# Time start 50 feature - 19:50:32

# 1. project to NAD UTM 17
# 2. convert raster to integer tool: Int
# 3. Raster to Polygon 
# 4. Generate Near Table - use meters as unit
# 5. Load into arc map and export as .txt

# ----------------------------------------------
# ----------------------------------------------



# table with link to patch id

pat <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/sNCyrly_pidI.txt", sep=",", header=TRUE) # this is the exported table from the polygon layer used to calculate NN

# the min distance in Meters
# disold <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/old/cmp_dist_minARCdd7.txt", sep=",", header=TRUE)
dis <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/sNCyrly_pidI_NN.txt", sep=",", header=TRUE)



# patoldx<-patold[,c(1,3)]
# colnames(patoldx) <-c("near_FID", "patchID")

# disoldx<-disold[,2:5]
# colnames(disoldx) <-c("FID", "near_FID", "near_dist", "near_rank")

# pat_disold <- full_join(patoldx, disoldx, by="near_FID")

# str(pat_disold)




patx<-pat[,c(1,3)]
colnames(patx) <-c("near_FID", "patchID")

disx<-dis[,3:6]
colnames(disx) <-c("FID", "near_FID", "near_dist", "near_rank")

pat_dis <- full_join(patx, disx, by="near_FID")

str(pat_dis)

# NEW:
# 'data.frame':	341300 obs. of  5 variables:
 # $ near_FID : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID  : int  10363 10363 10363 10363 10363 10363 10363 10363 10363 10363 ...
 # $ FID      : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ near_dist: num  0 6322 9276 1607 7088 ...
 # $ near_rank: int  1 15 18 4 19 15 25 25 26 8 ...
 
 # OLD:
# 'data.frame':	336750 obs. of  5 variables:
 # $ near_FID : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID  : int  10452 10452 10452 10452 10452 10452 10452 10452 10452 10452 ...
 # $ FID      : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ near_dist: num  1584 6945 12370 5544 10143 ...
 # $ near_rank: int  3 22 28 16 28 26 27 9 6 32 ...
 
# rename for second join
pat2<-patx
colnames(pat2) <-c("FID", "patchID")

pat_dis2 <- full_join(pat2, pat_dis, by="FID")
 str(pat_dis2)
# 'data.frame':	341300 obs. of  6 variables:
 # $ FID      : int  0 0 0 0 0 0 0 0 0 0 ...
 # $ patchID.x: int  10363 10363 10363 10363 10363 10363 10363 10363 10363 10363 ...
 # $ near_FID : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ patchID.y: int  10363 4560 4559 7730 3900 4560 6595 4947 8391 8392 ...
 # $ near_dist: num  0 6322 9276 1607 7088 ...
 # $ near_rank: int  1 9 19 2 12 7 26 40 34 3 ...
 
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

# WRITE TO FILE
write.table(pat_distm4, file = paste0(Output_Folder,"pat_distm4.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
pat_distm4<-read.table(paste0(Output_Folder,"pat_distm4.txt"), sep=",", header=TRUE)


# str(pat_distm4)
# 'data.frame':	305925 obs. of  3 variables:
 # $ patchID  : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ d_patchID: int  3573 925 2720 925 4309 4309 3575 4534 2720 3575 ...
 # $ near_dist: num  19521 20020 18424 16521 13152 ...

# Create Matrix to fill in
u_p <- unique(pat_distm4$patchID)

library(stringr)

  #create empty  matrix
 	pat_dist_min <- matrix(nrow = length(unique(pat_distm4$patchID)),  ncol = 4)
	colnames(pat_dist_min) <- c("patchID", "min_dist.km", "d_patchID", "numDP")

  
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
# # error message is ok. means it couldn't find a value. that is why I included the try() wrapper - so that the function would keep going. 
  
 # Remove rows with NAs
pat_dist_min <-  na.omit(pat_dist_min) #4104 obs
  
 # convert data types
pat_dist_min<- as.data.frame(pat_dist_min)	
pat_dist_min$numDP <- as.numeric(as.character(pat_dist_min$numDP))
pat_dist_min$min_dist.km <- as.numeric(as.character(pat_dist_min$min_dist.km))
pat_dist_min$min_dist.km<-pat_dist_min$min_dist.km/1000

# > str(pat_dist_min)
# 'data.frame':	4077 obs. of  4 variables:
 # $ patchID    : Factor w/ 4077 levels "10000","10010",..: 327 995 2058 2614 2810 3946 435 887 970 1216 ...
 # $ min_dist.km: num  7.47 7.47 0.179 0.179 0.906 ...
 # $ d_patchID  : Factor w/ 2513 levels "1","10000","10015",..: 471 80 1846 1466 2477 1976 387 154 705 444 ...
 # $ numDP      : num  1 1 1 1 1 1 1 1 1 1 ...


# WRITE TO FILE
write.table(pat_dist_min, file = paste0(Output_Folder,"pat_dist_min.txt"), row.names=FALSE, sep=",")

# READ TO FILE # 
pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)

# NEW:
# > str(pat_dist_min)
# 'data.frame':	4077 obs. of  4 variables:
 # $ patchID    : int  11 17 49 64 69 97 120 145 158 250 ...
 # $ min_dist.km: num  7.47 7.47 0.179 0.179 0.906 ...
 # $ d_patchID  : Factor w/ 2513 levels "1","10000","10015",..: 471 80 1846 1466 2477 1976 387 154 705 444 ...
 # $ numDP      : int  1 1 1 1 1 1 1 1 1 1 ...

 # OLD:
# str(pat_dist_min)
# 'data.frame':	4104 obs. of  4 variables:
 # $ patchID    : int  15 20 62 70 77 101 129 150 186 264 ...
 # $ min_dist.km: num  7.552 7.552 0.18 0.18 0.899 ...
 # $ d_patchID  : Factor w/ 2529 levels "1","1003","10040",..: 565 421 1994 1777 13 2150 422 242 773 520 ...
 # $ numDP      : int  1 1 1 1 1 1 1 1 1 1 ...

