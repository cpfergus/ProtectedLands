
# ----------------------------------------------
## NOTES:
# ----------------------------------------------

# WHAT IS WHAT?
# ----------------------------------------------



# ----------------------------------------------
# ORIG YRLY PATCH RASTERS
# ----------------------------------------------

# Original Raster
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))

# > yrly_patID
# class       : RasterLayer 
# dimensions  : 5821, 4930, 28697530  (nrow, ncol, ncell)
# resolution  : 180, 180  (x, y)
# extent      : 819105, 1706505, 1077045, 2124825  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# data source : Y:\Lacher\IaraSpatialLayers_HF\PreparedRasters\ProLands\BlueRidge\IndPatches\brPLiyrRSM.tif 
# names       : brPLiyrRSM 
# values      : 1, 14635  (min, max)

# Removed patches with zero core area 
NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""))#in NAD UTM 17
# > NCyrly_patID
# class       : RasterLayer 
# dimensions  : 5821, 4930, 28697530  (nrow, ncol, ncell)
# resolution  : 180, 180  (x, y)
# extent      : 819105, 1706505, 1077045, 2124825  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
# data source : Y:\Lacher\IaraSpatialLayers_HF\PreparedRasters\ProLands\BlueRidge\IndPatches\NCyrly_patID.tif 
# names       : NCyrly_patID 
# values      : 1, 14635  (min, max)


# sNCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/sNCyrly_patID.tif", sep=""))#in WGS ALBERS EQUAL AREA

# par(mfrow=c(1,3))
# plot(NCyrly_patID, useRaster=FALSE, main="NCyrly_patID");  plot(test2, useRaster=FALSE, main="test2");  plot(test2b, useRaster=FALSE, main="test2b")








# ----------------------------------------------
# PROJECTIONS TO ORIG RASTERS 
# we need the rgdal package for this
# ----------------------------------------------

# ----------------------------------------------
# DEFINE PROJECTION
newproj <- "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# ----------------------------------------------
# USE TEMPLATE RASTER THAT I MADE BASED ON ABOVE PROJ
test1<- raster(paste0(BR_fileLoc, "IndPatches/test1.tif", sep=""))
# rastemp <- projectExtent(yrly_patID, crs=newproj)
# res(rastemp) <- 180
# test1 <- projectRaster(yrly_patID, rastemp, res=180, crs=newproj, method="ngb")

# ----------------------------------------------
# ASSIGN PROJECTION SELF-DEFINED
test2<- raster(paste0(BR_fileLoc, "IndPatches/test2.tif", sep=""))
# test2 <- projectRaster(yrly_patID, res=180, crs=newproj, method="ngb")
# > extent(test2)
# class       : Extent 
# xmin        : -8247410 
# xmax        : -6614090 
# ymin        : 4657940 
# ymax        : 6433100 
	
# ----------------------------------------------
# CROPPED TO EXTENT
test2b<- raster(paste0(BR_fileLoc, "IndPatches/test2b.tif", sep=""))

# test2 <- crop(test2, extent(-8247410 , -6614090 , 5000000, 6000000))
# test2[test2 == 0]<-NA

# > extent(test2b)
# class       : Extent 
# xmin        : -8247410 
# xmax        : -6614090 
# ymin        : 4999940 
# ymax        : 6000020 

> test2b
class       : RasterLayer 
dimensions  : 5556, 9074, 50415144  (nrow, ncol, ncell)
resolution  : 180, 180  (x, y)
extent      : -8247410, -6614090, 4999940, 6000020  (xmin, xmax, ymin, ymax)
coord. ref. : +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
data source : Y:\Lacher\IaraSpatialLayers_HF\PreparedRasters\ProLands\BlueRidge\IndPatches\test2b.tif 
names       : test2b 
values      : 1, 14635  (min, max)

# a<-c(unique(NCyrly_patID)); b<-c(unique(test2b))
# > length(a)
# [1] 4152
# > length(b)
# [1] 14633




# ----------------------------------------------
# GDISTANCE TABLES
# ----------------------------------------------

# ----------------------------------------------
# ORIG TABLE

# 1) RESULT OF GDISTANCE
cmp_dist<-read.table(paste0(Output_Folder,"cmp_dist",".txt"), sep=",", header=TRUE)

# 2) patchID %in% core_pat$patchID
cmp_distm2<-read.table(paste0(Output_Folder,"cmp_distm2.txt"), sep=",", header=TRUE)

# 3) MINIMUM DISTANCES AND RELATED PATCHES SELECTED
cmp_dist_min<-read.table(paste0(Output_Folder,"cmp_dist_min.txt"), sep=",", header=TRUE)

# ----------------------------------------------
# MOST RECENT TABLE

# 1) RESULT OF GDISTANCE
cmp_dist2<-read.table(paste0(Output_Folder,"cmp_dist2",".txt"), sep=",", header=TRUE)



# ----------------------------------------------
# ARCGIS TABLE
# raw output from Arc
dis <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/cmp_dist_minARCdd7.txt", sep=",", header=TRUE)
# table with link to patch id
pat <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/NCyrly_patID_p.txt", sep=",", header=TRUE)
# final output after data wrangling. based off of NCyrly_patid, later removed inf and converted to km 
Opat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)


# > length(cmp_dist2)
# [1] 14633
# > length(cmp_dist)
# [1] 14636


# ----------------------------------------------
# ----------------------------------------------

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84


# ----------------------------------------------
# Tables used for creating figures
# ----------------------------------------------
iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)




	
#################
### TEST ###
#################


	
	# ID the length of the longest concatenated chain
	# want to make multiple rows for uktpile concatenations
	# patchID 700 = very long.
	# first filter to work with only that row:
	
test<-filter(pat_dist_min, patchID=="700") # there are lots of repeats
# > test[,3]
# [1] 1236,1236,1236,1236,2034,2034,1236,1236,1236,1243,695,1243,1243,1243,1243,1243,1243,1243,1243,1243,1243,3955,1179,1179,1179,1219,1219,3466,4962,1219,4962,4962,4962,1219,1219,1224,4965,1224,13047,1586,6581,1236,1236,1236,1517,1236,6582,1236,1243,1243,1243,1243,1243,1243,1243,1243,1243
 # str(test)
# 'data.frame':	1 obs. of  3 variables:
 # $ patchID    : Factor w/ 4152 levels "1","10001","10006",..: 2946
 # $ min_dist.km: Factor w/ 2746 levels "0","1000.8392311891",..: 1
 # $ d_patchID  : Factor w/ 2583 levels "1","1003","10040",..: 204
 
 
 
 
 #create mini matrix
 	test <- matrix(nrow = 2, ncol = 4)
	colnames(test) <- c("patchID", "min_dist.km", "d_patchID", "numDP")

 # u_p <-u_p[94:95]
 
 # Where did it come from??
for(i in 1:length(u_p)){
	test[i,1] <- u_p[i]
	ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==test[i,1],"estYr"]]))
	pat_sel<- filter(pat_distm4, patchID==u_p[i])
	try(test[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# This next line selects the id of the closest patch:
	try(test[i,3] <- str_c(unique(pat_sel[pat_sel$near_dist==test[i,2], "d_patchID"]), collapse=","))
	test[i,4] <- length(unlist(strsplit(test[i,3], ",")))
}
 
 # convert data types
test<- as.data.frame(test)	
test$numDP <- as.numeric(as.character(test$numDP))

#ID the maximum number of unique numDPs:
max(test$numDP) #16

# REMAKE TEST TEMPLATE

 
 #create mini matrix
 	test <- matrix(nrow = 2, ncol = 20)
	colnames(test) <- c("patchID", "min_dist.km", "d_patchID", "numDP", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

	# Come back to this.
# unlist(
# strsplit(rep("," 16))

# names(strsplit(rep("", 16)))<- c(1:16)

# )



 # Where did it come from??
for(i in 1:length(u_p)){

	test[i,1] <- u_p[i]
	ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==test[i,1],"estYr"]]))
	pat_sel<- filter(pat_distm4, patchID==u_p[i])
	try(test[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# This next line selects the id of the closest patch:
	try(test[i,3] <- str_c(unique(pat_sel[pat_sel$near_dist==test[i,2], "d_patchID"]), collapse=","))
	test[i,4] <- length(unlist(strsplit(test[i,3], ",")))
	# maybe try an ifelse for below when the # columns too long
	try(test[i,5] <- unlist(strsplit(test[i,3], ","))[[1]])
	try(test[i,6] <- unlist(strsplit(test[i,3], ","))[[2]])
	try(test[i,7] <- unlist(strsplit(test[i,3], ","))[[3]])
	try(test[i,8] <- unlist(strsplit(test[i,3], ","))[[4]])
	try(test[i,9] <- unlist(strsplit(test[i,3], ","))[[5]])
	try(test[i,10] <-unlist(strsplit(test[i,3], ","))[[6]])
	try(test[i,11] <-unlist(strsplit(test[i,3], ","))[[7]])
	try(test[i,12] <-unlist(strsplit(test[i,3], ","))[[8]])
	try(test[i,13] <-unlist(strsplit(test[i,3], ","))[[9]])
	try(test[i,14] <-unlist(strsplit(test[i,3], ","))[[10]])
	try(test[i,15] <-unlist(strsplit(test[i,3], ","))[[11]])
	try(test[i,16] <-unlist(strsplit(test[i,3], ","))[[12]])
	try(test[i,17] <-unlist(strsplit(test[i,3], ","))[[13]])
	try(test[i,18] <-unlist(strsplit(test[i,3], ","))[[14]])
	try(test[i,19] <-unlist(strsplit(test[i,3], ","))[[15]])
	try(test[i,20] <-unlist(strsplit(test[i,3], ","))[[16]])
}



# TRY the above with a loop

 # Where did it come from??
for(i in 1:length(u_p)){

	test[i,1] <- u_p[i]
	ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==test[i,1],"estYr"]]))
	pat_sel<- filter(pat_distm4, patchID==u_p[i])
	try(test[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# This next line selects the id of the closest patch:
	try(test[i,3] <- str_c(unique(pat_sel[pat_sel$near_dist==test[i,2], "d_patchID"]), collapse=","))
	test[i,4] <- length(unlist(strsplit(test[i,3], ",")))
	test[i,5] <- 
	
	
	test[i,5] <- unlist(strsplit(test[1,3], ","))[[1]]
	test[i,6] <- 
	test[i,7] <- 
	test[i,8] <- 
	test[i,9] <- 
	test[i,10] <- 
	test[i,11] <- 
	test[i,12] <- 
	test[i,13] <- 
	test[i,14] <- 
	test[i,15] <- 
	test[i,16] <- 
	test[i,17] <- 
	test[i,18] <- 
	test[i,19] <- 
	test[i,20] <- 
}



# it works!!
d <- unlist(strsplit(test[1,3], ","))
test[1,5] <- d[[1]]

unlist(strsplit(test[1,3], ","))[[1]]
unlist(strsplit(test[1,3], ","))[[3]]
unlist(strsplit(test[1,3], ","))[[16]]

# what happens if the column number is greater than the # of patches?
e <- unlist(strsplit(test[2,3], ","))
test[2,5] <- e[[1]]
e <- unlist(strsplit(test[2,3], ","))
try(test[2,6] <- e[[2]])


 # convert data types
test<- as.data.frame(test)	
	
	















 
# ----------------------------------------------
### TEST ###

# TEST BY REMOVING STUFF ONE BY ONE?
# Core area
tcore<-filter(tsub, core.area >= 1)  # nothing to remove since already removed
# > str(tcore)
# 'data.frame':	4152 obs. of  5 variables:
 # $ patchID    : int  1 15 20 62 70 77 101 129 150 186 ...
 # $ estYr      : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 # $ core.area  : int  2008800 32400 486000 97200 129600 3531600 3272400 97200 32400 2203200 ...
 # $ min_dist.km: num  NA 7.55 7.55 0.18 0.18 ...
 # $ d_patchID  : int  NA 20 15 70 62 101 77 150 129 264 ...
  
# 9999 & 1985

tunk<-filter(tcore, estYr <9999& estYr >1985)
# str(tunk)
# 'data.frame':	2814 obs. of  5 variables:
 # $ patchID    : int  2634 2638 2639 2647 2648 2654 2656 2657 2658 2659 ...
 # $ estYr      : int  1986 1986 1986 1986 1986 1986 1986 1986 1986 1986 ...
 # $ core.area  : int  810000 64800 226800 64800 32400 97200 32400 162000 32400 97200 ...
 # $ min_dist.km: num  0 0.177 0.5 3.015 0.397 ...
 # $ d_patchID  : int  NA 2322 2638 2604 2406 NA 2112 NA 2657 NA ...


# min dist=NA
tna <- filter(tunk, !is.na(min_dist.km))
# str(tna)
# 'data.frame':	2807 obs. of  5 variables:
 # $ patchID    : int  2634 2638 2639 2647 2648 2654 2656 2657 2658 2659 ...
 # $ estYr      : int  1986 1986 1986 1986 1986 1986 1986 1986 1986 1986 ...
 # $ core.area  : int  810000 64800 226800 64800 32400 97200 32400 162000 32400 97200 ...
 # $ min_dist.km: num  0 0.177 0.5 3.015 0.397 ...
 # $ d_patchID  : int  NA 2322 2638 2604 2406 NA 2112 NA 2657 NA ...


# TEST BY REMOVING STUFF ONE BY ONE?
# Core area
tcore2<-filter(iStats_Join, core.area >= 1) # 4152 # nothing to remove since already removed

# 9999 & 1985
tunk2a <-filter(tcore2, estYr<9999)# 3557
tunk2<-filter(tunk2a, estYr >1985)#2814

# min dist=NA
tna2 <- filter(tunk2, !is.na(min_dist.km))
# 2807 obs. of  44 variables:



rfoc <- filter(tna2, er_maj==1|er_maj==2|er_maj==3)#2065 #"|"=1526
rfoc1 <- filter(tna2, er_maj==1)
rfoc2 <- filter(tna2, er_maj==2)
rfoc3 <- filter(tna2, er_maj==3)
> nrow(rfoc1); nrow(rfoc2); nrow(rfoc3) # totals 1526
# [1] 383
# [1] 802
# [1] 341

# OLD version

pl_un1 <-filter(iStats_Join, estYr<9999)# 3557
# pl_un2 <-filter(iStats_Join, Transition<9999)# 3557

# Remove buffer areas
# pl_foc <- filter(pl_un, er_maj==1|er_maj==2|er_maj==3)#2065
pl_foc <- pl_un
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
# pl_new <-filter(pl_foc, estYr>=1985)#1536
pl_new <-filter(pl_foc, estYr>1985)# 2814

pl_new.na<- filter(pl_new, !is.na(min_dist.km))# 2807

# # w/out patch 1 (1800)
# dist_n1 <- filter(tna2, er_maj==1) 
# dist_n1$er <- "23"

# dist_n23 <- filter(tna2, er_maj>=2) 
# dist_n23$er <- "23"
















# ----------------------------------------------

# Select column subset:
tsub <- select(pl_new.na, patchID, estYr, er_maj, core.area, min_dist.km, d_patchID)
# str(tsub)
# 'data.frame':	1526 obs. of  6 variables:
 # $ patchID    : int  2657 2658 2668 2673 2681 2682 2688 2689 2690 2694 ...
 # $ estYr      : int  1986 1986 1986 1986 1986 1986 1986 1986 1986 1986 ...
 # $ er_maj     : int  2 2 2 2 2 2 2 2 2 3 ...
 # $ core.area  : int  162000 32400 32400 1198800 1069200 32400 1296000 129600 97200 194400 ...
 # $ min_dist.km: num  0 0.64 0.956 0 0 ...
 # $ d_patchID  : int  NA 2657 2127 NA NA 2627 1567 NA 959 1658 ...

# select only rows with min_dist.km==0

zeros <- filter(tsub, min_dist.km==0)
# > str(zeros)
# 'data.frame':	605 obs. of  6 variables:
 # $ patchID    : int  2657 2673 2681 2689 2761 2764 2799 2803 2827 2844 ...
 # $ estYr      : int  1986 1986 1986 1986 1986 1986 1987 1987 1988 1988 ...
 # $ er_maj     : int  2 2 2 2 1 1 2 2 2 2 ...
 # $ core.area  : int  162000 1198800 1069200 129600 1069200 907200 2494800 97200 129600 259200 ...
 # $ min_dist.km: num  0 0 0 0 0 0 0 0 0 0 ...
 # $ d_patchID  : int  NA NA NA NA NA NA NA NA NA NA ...

# > summary(zeros)
# patchID          estYr          er_maj        core.area      min_dist.km   d_patchID  
 # Min.   : 2657   Min.   :1986   Min.   :1.000   Min.   :   32400   Min.   :0    Min.   : NA  
 # 1st Qu.: 4895   1st Qu.:2000   1st Qu.:1.000   1st Qu.:   64800   1st Qu.:0    1st Qu.: NA  
 # Median : 6871   Median :2005   Median :2.000   Median :  226800   Median :0    Median : NA  
 # Mean   : 6834   Mean   :2004   Mean   :1.845   Mean   : 1138070   Mean   :0    Mean   :NaN  
 # 3rd Qu.: 8760   3rd Qu.:2008   3rd Qu.:2.000   3rd Qu.:  648000   3rd Qu.:0    3rd Qu.: NA  
 # Max.   :10447   Max.   :2015   Max.   :3.000   Max.   :80676000   Max.   :0    Max.   : NA  
                                                                                # NA's   :605  


> summary(pat_dis2)
    patchID        d_patchID       near_dist       near_rank    
 Min.   :    1   Min.   :    1   Min.   :    0   Min.   : 1.00  
 1st Qu.: 2343   1st Qu.: 2221   1st Qu.: 4997   1st Qu.:12.00  
 Median : 6449   Median : 6227   Median : 9121   Median :25.00  
 Mean   : 6588   Mean   : 6480   Mean   :10833   Mean   :24.83  
 3rd Qu.:10125   3rd Qu.:10026   3rd Qu.:14434   3rd Qu.:37.00  
 Max.   :14635   Max.   :14635   Max.   :69222   Max.   :50.00  









