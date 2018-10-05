
# ----------------------------------------------
# CREATE RASTER OF NLCD_MAJ CATEGORY ASSIGNMENTS FOR CLASSSTATS *** come back to this. brain dead at the moment. 
# ----------------------------------------------

pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))

library(raster)
library(dplyr)

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

# RECLASSIFY TO NLCD MAJ VALUES		
	nlrecl <- nlcd_maj[,1:2]
			
	nlcd_maj_ras <- reclassify(sNCyrly_patID, nlrecl)		

	# WRITE TO FILE
	writeRaster(nlcd_maj_ras, filename=paste0(BR_fileLoc, "nlcd_maj_ras.tif", sep=""), format='GTiff', overwrite=TRUE)
	# READ FROM FILE
	nlcd_maj_ras<- raster(paste0(BR_fileLoc, "nlcd_maj_ras.tif", sep=""))


# ----------------------------------------------










> CS_nlcd
  class n.patches  total.area prop.landscape patch.density
1     1        28  1243836000   4.093798e-02  9.215551e-10
2     2        46    61398000   2.020773e-03  1.513983e-09
3     3        32    91951200   3.026361e-03  1.053206e-09
4     4         1      907200   2.985839e-05  3.291268e-11
5     5      1539 27160304400   8.939185e-01  5.065262e-08
6     6       808  1323086400   4.354632e-02  2.659345e-08
7     7       442   454312800   1.495265e-02  1.454741e-08
8     8        25    47628000   1.567565e-03  8.228171e-10
  largest.patch.index total.core.area aggregation.index
1        1.255972e-02       661672800          85.17568
2        2.591281e-04        12020400          72.17720
3        5.907695e-04        51451200          88.11277
4        2.985839e-05          259200          95.55556
5        2.093862e-01     20221812000          92.95158
6        3.669382e-03       415627200          79.26342
7        5.310527e-04        95580000          74.83727
8        3.391060e-04        13251600          78.79846
  patch.cohesion.index
1             9.875333
2             8.874860
3             9.374178
4             8.101803
5             9.947173
6             9.321457
7             8.686180
8             9.046668
