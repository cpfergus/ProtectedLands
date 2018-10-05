





	pl_biopri <- raster("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/pl_biopri.img")
	
	# Decrease resolution to 360
	pl_biopri <- aggregate(pl_biopri, fact=12, fun=max)

	# WRITE TO FILE
	writeRaster(pl_biopri,filename= "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/pl_biopriL.tif", format='GTiff', overwrite=TRUE)
	
	# Need to reclassify pl_biopri. 
	pl_biopriR <- reclassify(pl_biopri, rclmat)
	
	
	
	pl_resil <- raster("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/pl_resil.img")
	
	# Decrease resolution to 360
	pl_resil <- aggregate(pl_resil, fact=12, fun=max)

	# WRITE TO FILE
	writeRaster(pl_resil,filename= "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/pl_resilL.tif", format='GTiff', overwrite=TRUE)
	
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

# # # m <- c(0, 1, 4, 2, 2, 3, 3, 4, 12, 5, 11, 6, 13, 7, 14, 8)
# # # rclmat <- matrix(m, ncol=2, byrow=TRUE)

# # # pl_resil <- reclassify(pl_resil, rclmat)	

	# # # > summary(pl_biopri)
          # # # pl_biopriL
# # # Min.       0.6665751
# # # 1st Qu.    3.7190761
# # # Median     6.5298855
# # # 3rd Qu.    9.8407572
# # # Max.    2207.8338950
# # # NA's       0.0000000
	
	
	
	
# ----------------------------------------------
# FILE LOCATIONS: 
Comb_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Tables/" # Combine (01-11)
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# ----------------------------------------------
# # OUTPUT FILES:

# maj_categ<-read.table(paste0(Comb_output,"maj_categ.txt"), sep=",", header=TRUE)


# file name:  / R label: 

# ----------------------------------------------
# INPUT FILES: 

# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_yearL.tif" )) #seeking large resolution one.

pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" )) # 0, years, 9999 # Raster reclassified so background =zero

# Unique Patch ID raster- patches distinguished by YEAR
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))

# NLCD 2011
pl_nlcd <- raster(paste0(BR_fileLoc, "pl_nlcdL.tif" ))

# Patch Stats table
iPatch_inc<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iPatchStats_inc.csv")
	
	
	detach(name="package:dplyr", unload=TRUE)
	detach(name="package:stringr", unload=TRUE)

	
	
# pl_biopri <- raster(paste0(BR_fileLoc, "pl_biopriL.tif"))
pl_resil <- 	raster(paste0(BR_fileLoc, "pl_resilL.tif" ))

rasters <- c(pl_resil)
names(rasters)<-c("pl_resil")

# rasters <- c(pl_biopri)
# names(rasters)<-c("pl_biopri")


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
			ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*129600/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
			ras_patch[[n_p]]$Raster <- paste0(names(rasters[f]))
			ras_patch[[n_p]]$Class <- paste0(u_vals[categ])
				n_p <- n_p +1
		}

		ras_patches[[n_ps]] <- bind_rows(ras_patch)
		n_ps <- n_ps +1
}
					
names(ras_patches)<-names(rasters)




library(stringr)		

maj_categX<-list()
r_c<-1
for(f in 1:length(ras_patches)){
	print(paste0("start:",names(ras_patches[f])))
	
	# Create matrices to fill in.
	  maj_categX[[r_c]] <- as.data.frame(matrix(nrow = length(u_patch), ncol = 4))
	  colnames(maj_categX[[r_c]]) <- c(paste0(str_sub(names(ras_patches[f]), start=4), "_patchID"), paste0(str_sub(names(ras_patches[f]), start=4), "_maj"), paste0(str_sub(names(ras_patches[f]), start=4), "_ha"), paste0(str_sub(names(ras_patches[f]), start=4), "_prop")) 
	
		for(p in 1:length(u_patch)){
			patchID <- u_patch[p]
			try(temp<-filter(ras_patches[[f]],ras_patches[[f]]$'zone' == u_patch[[p]]))
			try(area<-max(temp$area.ha))
			try(maj<-ifelse(area>12.96,filter(temp, area.ha == max(temp$area.ha))$Class,0))
			try(prop<-ifelse(area>12.96,100*(max(temp$area.ha)/sum(temp$area.ha)),0))
				maj_categX[[r_c]][p,1] <- patchID
				# maj_categX[[r_c]][p,2] <- str_sub(names(ras_patches[f]), start=4)
				maj_categX[[r_c]][p,2] <- maj
				maj_categX[[r_c]][p,3] <- area
				maj_categX[[r_c]][p,4] <- prop
			}
	print(paste0("finish:",names(ras_patches[f])))
	r_c <- r_c +1
}
new<-Sys.time()-old
print(new)

maj_categX <- bind_cols(maj_categX)

maj_categ<-read.table(paste0(Comb_output,"maj_categ.txt"), sep=",", header=TRUE)

maj_categX2 <- bind_cols(maj_categ, maj_categX)


maj_categX3 <- maj_categX2[,-23)]
# colnames(maj_categX)[colnames(maj_categX)=="nlcd_patchID"] <- "patchID"

# WRITE TO FILE
write.table(maj_categX3, file = paste0(Comb_output,"maj_categ",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
maj_categ<-read.table(paste0(Comb_output,"maj_categ.txt"), sep=",", header=TRUE)
























	