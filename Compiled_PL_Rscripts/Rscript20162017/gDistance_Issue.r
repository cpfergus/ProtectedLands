

#### ORIG CODE ###
{
library(raster)
library(rgeos)

yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/yrly_patID.tif", sep=""))#in NAD UTM 17
cmp<-yrly_patID

# NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""))#in NAD UTM 17
# cmp<-NCyrly_patID

# ----------------------------------------------
# CALCULATE DISTANCE

# rasterToPolygon method
rpoly <- rasterToPolygons(cmp, dissolve=T)

d <- gDistance(rpoly, byid=T)  # returns the cartesian minimum distance between the 2 geometries in the units of the current projection.
# str(d)
 # num [1:5531, 1:5531] 0 295397 433465 637103 550641 ...
 # - attr(*, "dimnames")=List of 2
  # ..$ : chr [1:5531] "1" "2" "3" "4" ...
  # ..$ : chr [1:5531] "1" "2" "3" "4" ...
  
cmp_dist<-as.data.frame(d)

pat_nms<-c(unique(NCyrly_patID))

row.names(cmp_dist)<-pat_nms
colnames(cmp_dist)<-pat_nms
diag(cmp_dist) <- NA
cmp_dist<-cbind(patchID = rownames(cmp_dist), cmp_dist)

# WRITE TO FILE
write.table(cmp_dist, file = paste0(Output_Folder,"cmp_dist",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
cmp_dist<-read.table(paste0(Output_Folder,"cmp_dist",".txt"), sep=",", header=TRUE)

# ----------------------------------------------
# CREATE NEW data.frame WITH MIN DISTANCE FOR EACH PATCH.

# For this, we have to remove patches / remove columns and rows that have core.area<=1 in iPatch

# READ FROM FILE
iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

# READ FROM FILE
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE) 

# Join to get est_Yr and core.area
temp <- full_join(est_yr, iStats_all, by="patchID")

core_pat<-filter(temp, core.area >= 1) # total =4152
cmp_distm<-filter(cmp_dist, patchID %in% core_pat$patchID) # total =4152

cmp_distm2<-cbind(core_pat$patchID, select(cmp_distm[-1], core_pat$patchID) )

library(stringr)
colnames(cmp_distm2) <- c("patchID", str_sub(colnames(cmp_distm2[,-1]), start=2))

# WRITE TO FILE
write.table(cmp_distm2, file = paste0(Output_Folder,"cmp_distm2",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
cmp_distm2<-read.table(paste0(Output_Folder,"cmp_distm2.txt"), sep=",", header=TRUE)
colnames(cmp_distm2) <- c("patchID", str_sub(colnames(cmp_distm2[,-1]), start=2))


# Create Matrix to fill in
cmp_dist_min <- matrix(nrow = nrow(cmp_distm), ncol = 3)
colnames(cmp_dist_min) <- c("patchID", "min_dist(km)", "d_patchID")



old<-Sys.time()
for(i in 1:nrow(cmp_distm2)){
	cmp_dist_min[i,1] <- cmp_distm2$patchID[i]
	ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==cmp_dist_min[i,1],"estYr"]]))
	cmp_distm3 <- as.data.frame(cmp_distm2[names(cmp_distm2[,-1]) %in%  ttt])[,-1]
	try(cmp_dist_min[i,2] <- apply(cmp_distm3[i,], 1, FUN=min, na.rm=TRUE)/1000)
	# This next line selects the column from which the min patch was id'd; in essence the id of the closest patch:
	try(cmp_dist_min[i,3] <- colnames(cmp_distm2[i,-1])[apply(cmp_distm3[i,],1,which.min)])
}
new<-Sys.time()-old
print(new) #~9min


# WRITE TO FILE
write.table(cmp_dist_min, file = paste0(Output_Folder,"cmp_dist_min.txt"), row.names=FALSE, sep=",")

# READ TO FILE
cmp_dist_min<-read.table(paste0(Output_Folder,"cmp_dist_min.txt"), sep=",", header=TRUE)

# Now can join this to master table to get the stats on both the patches that are closest to each other.

}


#############################################################################################
#############################################################################################
#############################################################################################

# ERROR: 
# > d <- gDistance(rpoly, byid=T) 
# Warning message:
# In RGEOSDistanceFunc(spgeom1, spgeom2, byid, "rgeos_distance") :
  # Spatial object 1 is not projected; GEOS expects planar coordinates

  
  
  
 # ----------------------------------------------   
  # other alternatuve - create points for polygon edges and use this package 'FNN'
   
  #function is: knn.dist, # k Nearest Neighbor Distances


# ----------------------------------------------  
# TRY REPROJECTING DATA AND RUNNING GDISTANCE AGAIN.
# ----------------------------------------------
library(raster)
# library(rgeos)

rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"

NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""))#in NAD UTM 17

# READ FROM FILE
test2b<- raster(paste0(BR_fileLoc, "IndPatches/test2b.tif", sep=""))


cmp_dist2<-read.table(paste0(Output_Folder,"cmp_dist2",".txt"), sep=",", header=TRUE)

cmp_dist<-read.table(paste0(Output_Folder,"cmp_dist",".txt"), sep=",", header=TRUE)
pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)

View(cmp_dist); View(cmp_dist2)

# READ FROM FILE
iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

# READ FROM FILE
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE) 


# # ----------------------------------------------
# # RUN gDistance
# # ----------------------------------------------

# old<-Sys.time()
# rpoly2 <- rasterToPolygons(test2b, dissolve=T, na.rm=TRUE)  
# new<-Sys.time()-old
# print(new)
# # Time difference of 20.59281 mins

# old<-Sys.time()
# d2 <- gDistance(rpoly2, byid=T) 
# new<-Sys.time()-old
# print(new)

# cmp_dist2<-as.data.frame(d2)
# pat_nms<-c(unique(test2b))

# row.names(cmp_dist2)<-pat_nms
# colnames(cmp_dist2)<-pat_nms
# diag(cmp_dist2) <- NA
# cmp_dist2<-cbind(patchID = rownames(cmp_dist2), cmp_dist2)

# # WRITE TO FILE
# write.table(cmp_dist2, file = paste0(Output_Folder,"cmp_dist2",".txt"), row.names=FALSE, sep=",")
# # READ TO FILE
# cmp_dist2<-read.table(paste0(Output_Folder,"cmp_dist2",".txt"), sep=",", header=TRUE)
# # ----------------------------------------------


length(unique(yrly_patID))
# [1] 14635
length(unique(test2b))
# [1] 14633
# test1<- raster(paste0(BR_fileLoc, "IndPatches/test1.tif", sep=""))
length(unique(test1))
# [1] 14633

u1<-unique(yrly_patID)
u2<-unique(test2b)

u1[!u1%in%u2]
# [1] 1049 6404









# Join to get est_Yr and core.area
temp <- full_join(est_yr, iStats_all, by="patchID")

core_pat<-filter(temp, core.area >= 1) # total =4152
cmp_distb<-filter(cmp_dist2, patchID %in% core_pat$patchID) # total =4152
cmp_distb2<-cbind(core_pat$patchID, select(cmp_distb[-1],  %in% core_pat$patchID) )

library(stringr)
colnames(cmp_distb2) <- c("patchID", str_sub(colnames(cmp_distb2[,-1]), start=2))

# WRITE TO FILE
write.table(cmp_distb2, file = paste0(Output_Folder,"cmp_distb2",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
cmp_distb2<-read.table(paste0(Output_Folder,"cmp_distb2.txt"), sep=",", header=TRUE)
colnames(cmp_distb2) <- c("patchID", str_sub(colnames(cmp_distb2[,-1]), start=2))


# Create Matrix to fill in
cmp_dist_min2 <- matrix(nrow = nrow(cmp_distb), ncol = 3)
colnames(cmp_dist_min2) <- c("patchID", "min_dist(km)", "d_patchID")



old<-Sys.time()
for(i in 1:nrow(cmp_distb2)){
	cmp_dist_min2[i,1] <- cmp_distb2$patchID[i]
	ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==cmp_dist_min2[i,1],"estYr"]]))
	cmp_distb3 <- as.data.frame(cmp_distb2[names(cmp_distb2[,-1]) %in%  ttt])[,-1]
	try(cmp_dist_min2[i,2] <- apply(cmp_distb3[i,], 1, FUN=min, na.rm=TRUE)/1000)
	# This next line selects the column from which the min patch was id'd; in essence the id of the closest patch:
	try(cmp_dist_min2[i,3] <- colnames(cmp_distb2[i,-1])[apply(cmp_distb3[i,],1,which.min)])
}
new<-Sys.time()-old
print(new) #~9min


# WRITE TO FILE
write.table(cmp_dist_min2, file = paste0(Output_Folder,"cmp_dist_min2.txt"), row.names=FALSE, sep=",")

# READ TO FILE
cmp_dist_min2<-read.table(paste0(Output_Folder,"cmp_dist_min2.txt"), sep=",", header=TRUE)











# # ----------------------------------------------
# # ----------------------------------------------
# # ----------------------------------------------
# # ----------------------------------------------
# # ----------------------------------------------


# # rasterToPolygon method
# old<-Sys.time()
# rpoly <- rasterToPolygons(yrly_patIDpr1, dissolve=T)
# new<-Sys.time()-old
# print(new)

# old<-Sys.time()
# d <- gDistance(rpoly, byid=T) 
# new<-Sys.time()-old
# print(new)




# yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))

# cmp<-yrly_patID

# # > projection(yrly_patID)
# # [1] "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# projection(r)
# # proj.4 projection description
# # Azimuthal Equidistant:  Proj4js.defs["ESRI:54032"] = "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs";

# newproj <- "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# # we need the rgdal package for this



# rastemp <- projectExtent(yrly_patID, crs=newproj)
# res(rastemp) <- 180
# test1 <- projectRaster(yrly_patID, rastemp, res=180, crs=newproj, method="ngb")
# test2 <- projectRaster(yrly_patID, res=180, crs=newproj, method="ngb")



# # WRITE TO FILE
# writeRaster(test1, filename=paste0(BR_fileLoc, "IndPatches/test1.tif", sep=""), format='GTiff', overwrite=TRUE)
# # WRITE TO FILE
# writeRaster(test2, filename=paste0(BR_fileLoc, "IndPatches/test2.tif", sep=""), format='GTiff', overwrite=TRUE)

# # WRITE TO FILE
# writeRaster(test2, filename=paste0(BR_fileLoc, "IndPatches/test2b.tif", sep=""), format='GTiff', overwrite=TRUE)

# # READ FROM FILE
# test1<- raster(paste0(BR_fileLoc, "IndPatches/test1.tif", sep=""))
	
# # READ FROM FILE
# test2b<- raster(paste0(BR_fileLoc, "IndPatches/test2b.tif", sep=""))

	
# # READ FROM FILE
# test2<- raster(paste0(BR_fileLoc, "IndPatches/test2.tif", sep=""))


# par(mfrow=c(1,3))
# plot(yrly_patID, useRaster=FALSE, main="yrly_patID");  plot(test1, useRaster=FALSE, main="test1");  plot(test2, useRaster=FALSE, main="test2")

# # > extent(test1)
# # class       : Extent 
# # xmin        : -8246510 
# # xmax        : -6614990 
# # ymin        : 4658840 
# # ymax        : 6432200
 
# # > extent(test2) # ** USE THIS ONE**
# # class       : Extent 
# # xmin        : -8247410 
# # xmax        : -6614090 
# # ymin        : 4657940 
# # ymax        : 6433100 

# test2 <- crop(test2, extent(-8247410 , -6614090 , 5000000, 6000000))
# test2[test2 == 0]<-NA
 # # This did make it smaller:

# # > extent(yrly_patID)
# # class       : Extent 
# # xmin        : 819105 
# # xmax        : 1706505 
# # ymin        : 1077045 
# # ymax        : 2124825 


# # Crop extent
# # xmin xmax ymin ymax


# test2 <- crop(test2, extent(-8247410 , -6614090 , 5000000, 6000000))
# test2[test2 == 0]<-NA
 # # This did make it smaller:
 
 # > test2
# class       : RasterLayer 
# dimensions  : 5556, 9074, 50415144  (nrow, ncol, ncell)
# resolution  : 180, 180  (x, y)
# extent      : -8247410, -6614090, 4999940, 6000020  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
# data source : Y:\Lacher\rtempCLEARME\2016-12-23_145236_1884_81722.grd 
# names       : layer 
# values      : 1, 14635  (min, max)


# # ----------------------------------------------


# # rpoly <- rasterToPolygons(yrly_patIDpr1, dissolve=T)  
# # d <- gDistance(rpoly, byid=T) 
# # cmp_dist<-as.data.frame(d)
# # write.table(cmp_dist, file = paste0(Output_Folder,"cmp_dist",".txt"), row.names=FALSE, sep=",")
# # rm(cmp_dist)


# # rpoly1 <- rasterToPolygons(test1, dissolve=T)  
# # d1 <- gDistance(rpoly1, byid=T) 
# # cmp_dist1<-as.data.frame(d1)
# # write.table(cmp_dist1, file = paste0(Output_Folder,"cmp_dist1",".txt"), row.names=FALSE, sep=",")
# # rm(cmp_dist1)


# rpoly2 <- rasterToPolygons(test2, dissolve=T)  
# d2 <- gDistance(rpoly2, byid=T) 
# cmp_dist2<-as.data.frame(d2)
# write.table(cmp_dist2, file = paste0(Output_Folder,"cmp_dist2",".txt"), row.names=FALSE, sep=",")
# rm(cmp_dist2)


# # rasterToPolygon method
# old<-Sys.time()
# rpoly <- rasterToPolygons(yrly_patIDpr1, dissolve=T)
# new<-Sys.time()-old
# print(new)

# old<-Sys.time()
# d <- gDistance(rpoly, byid=T) 
# new<-Sys.time()-old
# print(new)

# # ----------------------------------------------

# # #simplest approach
# # # pr1 <- projectRaster(r, crs=newproj)
# # pr1 <- projectRaster(yrly_patID, crs=newproj)

# # # WRITE TO FILE
# # writeRaster(pr1, filename=paste0(BR_fileLoc, "IndPatches/brPLiyrRSM_pr1.tif", sep=""), format='GTiff', overwrite=TRUE)
	
# # # READ FROM FILE
# # yrly_patIDpr1<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM_pr1.tif", sep=""))


# # ----------------------------------------------

# > yrly_patID
# class       : RasterLayer 
# dimensions  : 5821, 4930, 28697530  (nrow, ncol, ncell)
# resolution  : 180, 180  (x, y)
# extent      : 819105, 1706505, 1077045, 2124825  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# data source : Y:\Lacher\IaraSpatialLayers_HF\PreparedRasters\ProLands\BlueRidge\IndPatches\brPLiyrRSM.tif 
# names       : brPLiyrRSM 
# values      : 1, 14635  (min, max)

# > yrly_patIDpr1
# class       : RasterLayer 
# dimensions  : 8789, 12011, 105564679  (nrow, ncol, ncell)
# resolution  : 136, 202  (x, y)
# extent      : -8247462, -6613966, 4657832, 6433210  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : Y:\Lacher\IaraSpatialLayers_HF\PreparedRasters\ProLands\BlueRidge\IndPatches\brPLiyrRSM_pr1.tif 
# names       : brPLiyrRSM_pr1 
# values      : 1, 14635  (min, max)

# > rastemp
# class       : RasterLayer 
# dimensions  : 5821, 4930, 28697530  (nrow, ncol, ncell)
# resolution  : 330.9487, 304.6489  (x, y)
# extent      : -8246510, -6614933, 4658839, 6432200  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 

# > test1
# class       : RasterLayer 
# dimensions  : 5821, 4930, 28697530  (nrow, ncol, ncell)
# resolution  : 330.9487, 304.6489  (x, y) 
# extent      : -8246510, -6614933, 4658839, 6432200  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
# data source : Y:\Lacher\rtempCLEARME\2016-12-22_200913_15636_74691.grd 
# names       : brPLiyrRSM 
# values      : 1, 14635  (min, max)

# >   test2 #this one.
# class       : RasterLayer 
# dimensions  : 9862, 9074, 89487788  (nrow, ncol, ncell)
# resolution  : 180, 180  (x, y)
# extent      : -8247410, -6614090, 4657940, 6433100  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0 
# data source : Y:\Lacher\rtempCLEARME\2016-12-22_210424_15636_62649.grd 
# names       : brPLiyrRSM 
# values      : 1, 14635  (min, max)



# #############################################################################################
# #############################################################################################

# # PACKAGES NEEDED
# # Table manipulation
# library(dplyr)
# # Rasters
# library(raster)
# library(rgdal) # For writing .tif files
# # library(gdistance)

# # SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

# # ----------------------------------------------
# # FILE LOCATIONS: 
# Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
# BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"


# # READ FROM FILE
# iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

# # READ FROM FILE
# est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE) 



# # Unique Patch ID raster- patches distinguished by YEAR
# yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))


# #############################################################################################
# ### TEST ###
# # in arcgis
# # decimal degrees - near tool

# # First remove patches that are too small, then use ARc to calc NN

# # Join to get est_Yr and core.area
# temp <- full_join(est_yr, iStats_all, by="patchID")

# core_pat<-filter(temp, core.area >= 1) 
# ncor <-filter(temp, core.area == 0) 

# NCyrly_patID<-yrly_patID
# NCyrly_patID[yrly_patID %in% ncor$patchID] <- NA


# # WRITE TO FILE
# writeRaster(NCyrly_patID, filename=paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""), format='GTiff', overwrite=TRUE)
	
# # READ FROM FILE
# NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""))

# # ----------------------------------------------
# # ----------------------------------------------
# # # USE ARC GIS TO CONVERT TO POLYGON AND COMPUTE MIN DISTANCE. start 50 feature - 19:50:32

# # 1. convert raster to integer tool: Int
# # 2. Raster to Polygon
# # 3. Generate Near Table - use kilometers as unit

# # ----------------------------------------------
# # ----------------------------------------------


# # table with link to patch id
# pat <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/NCyrly_patID_p.txt", sep=",", header=TRUE)

# # the min distance in Meters
# dis <- read.table("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/cmp_dist_minARCdd7.txt", sep=",", header=TRUE)


# library(dplyr)

# pat<-pat[,c(1,3)]
# colnames(pat) <-c("near_FID", "patchID")

# dis<-dis[,2:5]
# colnames(dis) <-c("FID", "near_FID", "near_dist", "near_rank")

# pat_dis <- full_join(pat, dis, by="near_FID")

# str(pat_dis)
# # 'data.frame':	336750 obs. of  5 variables:
 # # $ near_FID : int  0 0 0 0 0 0 0 0 0 0 ...
 # # $ patchID  : int  10452 10452 10452 10452 10452 10452 10452 10452 10452 10452 ...
 # # $ FID      : int  1 2 3 4 5 6 7 8 9 10 ...
 # # $ near_dist: num  1584 6945 12370 5544 10143 ...
 # # $ near_rank: int  3 22 28 16 28 26 27 9 6 32 ...
 
# # rename for second join
# pat2<-pat
# colnames(pat2) <-c("FID", "patchID")

# pat_dis2 <- full_join(pat2, pat_dis, by="FID")
 # str(pat_dis2)
# # 'data.frame':	336750 obs. of  6 variables:
 # # $ FID      : int  0 0 0 0 0 0 0 0 0 0 ...
 # # $ patchID.x: int  10452 10452 10452 10452 10452 10452 10452 10452 10452 10452 ...
 # # $ near_FID : int  1 2 3 4 5 6 7 8 9 10 ...
 # # $ patchID.y: int  3135 3964 8449 4614 6640 8449 4991 8448 7789 7793 ...
 # # $ near_dist: num  1584 6945 12370 5544 10143 ...
 # # $ near_rank: int  3 13 32 8 28 34 40 4 2 36 ...
 
 # pat_dis2<-pat_dis2[,c(2,4:6)]
 # colnames(pat_dis2) <- c("patchID", "d_patchID", "near_dist", "near_rank")


# pat_distm<-filter(pat_dis2, patchID %in% core_pat$patchID)
# pat_distm2<-filter(pat_distm,d_patchID %in% core_pat$patchID) 
# pat_distm3 <- pat_distm2[,1:3]

# # sort by patchID
# pat_distm3 <- pat_distm3[order(pat_distm3$patchID),] 

# #remove where patchID=d_patchID
# pat_distm4 <- pat_distm3[!pat_distm3$patchID==pat_distm3$d_patchID,]


# # Create Matrix to fill in
# u_p <- unique(pat_distm4$patchID)

# pat_dist_min <- matrix(nrow = length(unique(pat_distm4$patchID)), ncol = 3)
# colnames(pat_dist_min) <- c("patchID", "min_dist(km)", "d_patchID")


# old<-Sys.time()
# for(i in 1:length(u_p)){
	# pat_dist_min[i,1] <- u_p[i]
	# ttt<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[i,1],"estYr"]]))
	# pat_sel<- filter(pat_distm4, patchID==u_p[i])
	# try(pat_dist_min[i,2] <- min(pat_sel[pat_sel$d_patchID %in%  ttt,]$near_dist, na.rm=TRUE))
	# # This next line selects the id of the closest patch:
	# try(pat_dist_min[i,3] <- pat_distm4[pat_distm4$near_dist==pat_dist_min[i,2], "d_patchID"][pat_distm4[pat_distm4$near_dist==pat_dist_min[i,2], "d_patchID"]!=u_p[i]])

# }
# new<-Sys.time()-old
# print(new) #~Time difference of 41.46715 secs 
# # error message is ok. means it couldn't find a value. that is why i included the try() wrapper - so that the function would keep going. 



# # WRITE TO FILE
# write.table(pat_dist_min, file = paste0(Output_Folder,"pat_dist_min.txt"), row.names=FALSE, sep=",")

# # READ TO FILE # ** This table has Inf in the min dist column. remove prior to analysis
# pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)

# pat_dist_min$min_dist.km.<-pat_dist_min$min_dist.km./1000



# # pat_dist_min[1,1] <- u_p[1]
# # ttt1<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[1,1],"estYr"]]))
# # test1<- filter(pat_distm4, patchID==u_p[1])
# # test1<- test1[test1$d_patchID %in%  ttt1,]

# # pat_dist_min[2,1] <- u_p[2]
# # ttt2<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[2,1],"estYr"]]))
# # pat_sel2<- filter(pat_distm4, patchID==u_p[2])
# # pat_dist_min[2,2]<- min(pat_sel2[pat_sel2$d_patchID %in%  ttt2,]$near_dist, na.rm=TRUE)


# # pat_dist_min[6,1] <- u_p[6]
# # ttt6<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[6,1],"estYr"]]))
# # pat_sel6<- filter(pat_distm4, patchID==u_p[6])
# # pat_dist_min[6,2]<- min(pat_sel6[pat_sel6$d_patchID %in%  ttt6,]$near_dist, na.rm=TRUE)
# # pat_dist_min[6,3] <- pat_distm4[pat_distm4$near_dist==pat_dist_min[6,2], "d_patchID"][pat_distm4[pat_distm4$near_dist==pat_dist_min[6,2], "d_patchID"]!=u_p[6]]



# # pat_dist_min[6,3] <- pat_distm4[pat_distm4$near_dist==pat_dist_min[6,2], "d_patchID"]


# # test<-pat_distm4[pat_distm4$near_dist==pat_dist_min[6,2], "d_patchID"]

# # pat_distm4[pat_distm4$near_dist==pat_dist_min[6,2], "d_patchID"][pat_distm4[pat_distm4$near_dist==pat_dist_min[6,2], "d_patchID"]!=u_p[6]]

# # test[test!=u_p[6]]
# # !=u_p[6]]


# # pat_dist_min[10,1] <- u_p[10]
# # ttt10<-as.character(c(temp$patchID[temp[temp$patchID,"estYr"] <= temp[temp$patchID==pat_dist_min[10,1],"estYr"]]))
# # pat_sel10<- filter(pat_distm4, patchID==u_p[10])
# # pat_dist_min[10,2]<- min(pat_sel10[pat_sel10$d_patchID %in%  ttt10,]$near_dist, na.rm=TRUE)
# # pat_dist_min[10,3] <- pat_distm4[pat_distm4$near_dist==pat_dist_min[10,2], "d_patchID"]



# # > str(test1)
# # 'data.frame':	188 obs. of  3 variables:
 # # $ patchID  : int  1 1 1 1 1 1 1 1 1 1 ...
 # # $ d_patchID: int  3623 2764 1108 888 4588 13368 2764 4377 4377 1109 ...
 # # $ near_dist: num  18685 17166 19045 16284 12794 ...
# # > test2<- filter(pat_distm4, patchID==u_p[2])
# # > 
# # > str(test2)
# # 'data.frame':	50 obs. of  3 variables:
 # # $ patchID  : int  15 15 15 15 15 15 15 15 15 15 ...
 # # $ d_patchID: int  8404 10429 5415 2215 3134 3134 8932 3134 13814 13825 ...
 # # $ near_dist: num  44638 44602 40131 27352 26465 ...

 
 # #############################################################################################
 # ### TRYING TO ( UNSUCCESSFULLY ) REPROJECT IN R IN ORDER TO CALCULATE NN USING GDISTANCE. IT WAS GIVING ME DEGREES INSTEAD OF METERS.

# # yrly_patID2<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM2.tif", sep=""))


# {# > str(yrly_patID)
# # Formal class 'RasterLayer' [package "raster"] with 12 slots
  # # ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  # # .. .. ..@ name        : chr "Y:\\Lacher\\IaraSpatialLayers_HF\\PreparedRasters\\ProLands\\BlueRidge\\IndPatches\\brPLiyrRSM.tif"
  # # .. .. ..@ datanotation: chr "FLT8S"
  # # .. .. ..@ byteorder   : chr "little"
  # # .. .. ..@ nodatavalue : num -Inf
  # # .. .. ..@ NAchanged   : logi FALSE
  # # .. .. ..@ nbands      : int 1
  # # .. .. ..@ bandorder   : chr "BIL"
  # # .. .. ..@ offset      : int 0
  # # .. .. ..@ toptobottom : logi TRUE
  # # .. .. ..@ blockrows   : int 1
  # # .. .. ..@ blockcols   : int 4930
  # # .. .. ..@ driver      : chr "gdal"
  # # .. .. ..@ open        : logi FALSE
  # # ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  # # .. .. ..@ values    : logi(0) 
  # # .. .. ..@ offset    : num 0
  # # .. .. ..@ gain      : num 1
  # # .. .. ..@ inmemory  : logi FALSE
  # # .. .. ..@ fromdisk  : logi TRUE
  # # .. .. ..@ isfactor  : logi FALSE
  # # .. .. ..@ attributes: list()
  # # .. .. ..@ haveminmax: logi TRUE
  # # .. .. ..@ min       : num 1
  # # .. .. ..@ max       : num 14635
  # # .. .. ..@ band      : int 1
  # # .. .. ..@ unit      : chr ""
  # # .. .. ..@ names     : chr "brPLiyrRSM"
  # # ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  # # .. .. ..@ type      : chr(0) 
  # # .. .. ..@ values    : logi(0) 
  # # .. .. ..@ color     : logi(0) 
  # # .. .. ..@ names     : logi(0) 
  # # .. .. ..@ colortable: logi(0) 
  # # ..@ title   : chr(0) 
  # # ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  # # .. .. ..@ xmin: num 819105
  # # .. .. ..@ xmax: num 1706505
  # # .. .. ..@ ymin: num 1077045
  # # .. .. ..@ ymax: num 2124825
  # # ..@ rotated : logi FALSE
  # # ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  # # .. .. ..@ geotrans: num(0) 
  # # .. .. ..@ transfun:function ()  
  # # ..@ ncols   : int 4930
  # # ..@ nrows   : int 5821
  # # ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  # # .. .. ..@ projargs: chr "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  # # ..@ history : list()
  # # ..@ z       : list()
  
  
# # > yrly_patID2<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM2.tif", sep=""))
# # > 
# # > str(yrly_patID2)
# # Formal class 'RasterLayer' [package "raster"] with 12 slots
  # # ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  # # .. .. ..@ name        : chr "Y:\\Lacher\\IaraSpatialLayers_HF\\PreparedRasters\\ProLands\\BlueRidge\\IndPatches\\brPLiyrRSM2.tif"
  # # .. .. ..@ datanotation: chr "FLT8S"
  # # .. .. ..@ byteorder   : chr "little"
  # # .. .. ..@ nodatavalue : num -Inf
  # # .. .. ..@ NAchanged   : logi FALSE
  # # .. .. ..@ nbands      : int 1
  # # .. .. ..@ bandorder   : chr "BIL"
  # # .. .. ..@ offset      : int 0
  # # .. .. ..@ toptobottom : logi TRUE
  # # .. .. ..@ blockrows   : int 128
  # # .. .. ..@ blockcols   : int 128
  # # .. .. ..@ driver      : chr "gdal"
  # # .. .. ..@ open        : logi FALSE
  # # ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  # # .. .. ..@ values    : logi(0) 
  # # .. .. ..@ offset    : num 0
  # # .. .. ..@ gain      : num 1
  # # .. .. ..@ inmemory  : logi FALSE
  # # .. .. ..@ fromdisk  : logi TRUE
  # # .. .. ..@ isfactor  : logi FALSE
  # # .. .. ..@ attributes: list()
  # # .. .. ..@ haveminmax: logi TRUE
  # # .. .. ..@ min       : num 1
  # # .. .. ..@ max       : num 14635
  # # .. .. ..@ band      : int 1
  # # .. .. ..@ unit      : chr ""
  # # .. .. ..@ names     : chr "brPLiyrRSM2"
  # # ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  # # .. .. ..@ type      : chr(0) 
  # # .. .. ..@ values    : logi(0) 
  # # .. .. ..@ color     : logi(0) 
  # # .. .. ..@ names     : logi(0) 
  # # .. .. ..@ colortable: logi(0) 
  # # ..@ title   : chr(0) 
  # # ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  # # .. .. ..@ xmin: num -86475
  # # .. .. ..@ xmax: num 962925
  # # .. .. ..@ ymin: num 3466905
  # # .. .. ..@ ymax: num 4633485
  # # ..@ rotated : logi FALSE
  # # ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  # # .. .. ..@ geotrans: num(0) 
  # # .. .. ..@ transfun:function ()  
  # # ..@ ncols   : int 5830
  # # ..@ nrows   : int 6481
  # # ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  # # .. .. ..@ projargs: chr "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # # ..@ history : list()
  # # ..@ z       : list()
# }


# cmp<-yrly_patID
# cmp2<-yrly_patID2

# rpoly <- rasterToPolygons(cmp, dissolve=T)
# rpoly2 <- rasterToPolygons(cmp2, dissolve=T)

# # > rpoly
# # class       : SpatialPolygonsDataFrame 
# # features    : 14635 
# # extent      : 819465, 1706505, 1078125, 2124645  (xmin, xmax, ymin, ymax)
# # coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# # variables   : 1
# # names       : brPLiyrRSM 
# # min values  :          1 
# # max values  :      14635 

# # > rpoly2 # 14566 total patches
# # class       : SpatialPolygonsDataFrame 
# # features    : 14566 
# # extent      : -75675, 956085, 3593625, 4503345  (xmin, xmax, ymin, ymax)
# # coord. ref. : +proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 
# # variables   : 1
# # names       : brPLiyrRSM2 
# # min values  :           1 
# # max values  :       14635 



# d <- gDistance(rpoly, byid=T)
# d2 <- gDistance(rpoly2, byid=T)

# cmp_dist2<-as.data.frame(d2) #looks the same. 

# pat_nms2<-c(unique(yrly_patID2))

# row.names(cmp_dist2)<-pat_nms2
# colnames(cmp_dist2)<-pat_nms2
# diag(cmp_dist2) <- NA
# cmp_dist2<-cbind(patchID = rownames(cmp_dist2), cmp_dist2)

# # WRITE TO FILE
# write.table(cmp_dist, file = paste0(Output_Folder,"cmp_dist",".txt"), row.names=FALSE, sep=",")

# # READ TO FILE
# cmp_dist<-read.table(paste0(Output_Folder,"cmp_dist",".txt"), sep=",", header=TRUE)




