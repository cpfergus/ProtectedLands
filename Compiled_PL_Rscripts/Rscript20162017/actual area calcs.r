


# FILE LOCATIONS: 
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"







# READ TO FILE
resil_all<-read.table(paste0(Output_Folder,"resil_all",".txt"), sep=",", header=TRUE)
# READ TO FILE
nlcd_all<-read.table(paste0(Output_Folder,"nlcd_all",".txt"), sep=",", header=TRUE)
# READ TO FILE
gap_all<-read.table(paste0(Output_Folder,"gap_all",".txt"), sep=",", header=TRUE)



library(raster)

# Unique Patch ID raster- patches distinguished by YEAR
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))



pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.tif"   ))
pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
pl_gap 	<- 	raster(paste0(BR_fileLoc, "pl_gap.tif"  ))



pl_resilR <- raster(paste0(BR_fileLoc, "pl_resilR.tif" ))

library(dplyr)
u_patch <- unique(yrly_patID)


ras_vals <- getValues(pl_resilR)
u_vals <- unique(ras_vals)[-1]

ras_patch <- list()
n_p <- 1
for(categ in u_vals){
		
	categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
	categ_p <- setValues(pl_resilR, categ_val)
	ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, yrly_patID, fun='count', na.rm=TRUE))
	ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
	ras_patch[[n_p]]$Raster <- paste0(names(pl_resilR))
	ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
		n_p <- n_p +1
}

ras_patches<- bind_rows(ras_patch)
colnames(ras_patches) <- c("patchID", "count", "resil_area.ha", "Raster", "Class")


maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)


tbl_df(maj_categ)

er_join <- select(maj_categ, patchID, er_maj)

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

joinA <- full_join(iStats_all, est_yr, by="patchID")
joinB <- full_join(ras_patches, er_join, by="patchID")
join <- full_join(joinA, joinB, by="patchID")

resil_Join <- filter(join, n.core.cell>=1) 
  
resil_Join$area.ha <- resil_Join$area/10000
resil_Join$core.area.ha <- resil_Join$core.area/10000


# WRITE TO FILE
write.table(resil_Join, file = paste0(Output_Folder,"resil_all",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
resil_all<-read.table(paste0(Output_Folder,"resil_all",".txt"), sep=",", header=TRUE)
# ----------------------------------------------
# ----------------------------------------------

library(dplyr)
u_patch <- unique(yrly_patID)


ras_vals <- getValues(pl_nlcd)
u_vals <- unique(ras_vals)[-1]

ras_patch <- list()
n_p <- 1
for(categ in u_vals){
print(u_vals[categ])		
	categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
	categ_p <- setValues(pl_nlcd, categ_val)
	ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, yrly_patID, fun='count', na.rm=TRUE))
	ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
	ras_patch[[n_p]]$Raster <- paste0(names(pl_nlcd))
	ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
		n_p <- n_p +1
}


ras_patches<- bind_rows(ras_patch)
colnames(ras_patches) <- c("patchID", "count", "nlcd_area.ha", "Raster", "Class")


maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)


tbl_df(maj_categ)

er_join <- select(maj_categ, patchID, er_maj)

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

joinA <- full_join(iStats_all, est_yr, by="patchID")
joinB <- full_join(ras_patches, er_join, by="patchID")
join <- full_join(joinA, joinB, by="patchID")

nlcd_Join <- filter(join, n.core.cell>=1) 
  
nlcd_Join$area.ha <- nlcd_Join$area/10000
nlcd_Join$core.area.ha <- nlcd_Join$core.area/10000


# WRITE TO FILE
write.table(nlcd_Join, file = paste0(Output_Folder,"nlcd_all",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
nlcd_all<-read.table(paste0(Output_Folder,"nlcd_all",".txt"), sep=",", header=TRUE)

# ----------------------------------------------
# ----------------------------------------------

ras_vals <- getValues(pl_gap)
u_vals <- unique(ras_vals)[-1]

ras_patch <- list()
n_p <- 1
for(categ in u_vals){
		
	categ_val <- ifelse(ras_vals == u_vals[categ]|is.na(ras_vals),ras_vals,NA) 
	categ_p <- setValues(pl_gap, categ_val)
	ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, yrly_patID, fun='count', na.rm=TRUE))
	ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*32400/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
	ras_patch[[n_p]]$Raster <- paste0(names(pl_gap))
	ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
		n_p <- n_p +1
}

ras_patches<- bind_rows(ras_patch)
colnames(ras_patches) <- c("patchID", "count", "gap_area.ha", "Raster", "Class")


maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)


tbl_df(maj_categ)

er_join <- select(maj_categ, patchID, er_maj)

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

joinA <- full_join(iStats_all, est_yr, by="patchID")
joinB <- full_join(ras_patches, er_join, by="patchID")
join <- full_join(joinA, joinB, by="patchID")

gap_Join <- filter(join, n.core.cell>=1) 
  
gap_Join$area.ha <- gap_Join$area/10000
gap_Join$core.area.ha <- gap_Join$core.area/10000


# WRITE TO FILE
write.table(gap_Join, file = paste0(Output_Folder,"gap_all",".txt"), row.names=FALSE, sep=",")

# READ TO FILE
gap_all<-read.table(paste0(Output_Folder,"gap_all",".txt"), sep=",", header=TRUE)

