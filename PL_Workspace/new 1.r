



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
 