
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

# PACKAGES NEEDED

# rasters
library(raster)

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

# ----------------------------------------------
################################################

# ----------------------------------------------
# ----------------------------------------------
# READ FILES: 
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# FILE LOCATIONS: 
# ----------------------------------------------


BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
Comb_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Tables/" 
CombRas_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Rasters/"


# ----------------------------------------------
# RAW RASTERS:
# ----------------------------------------------

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


# ----------------------------------------------
# GENERATED RASTERS
# ----------------------------------------------

# Original Raster
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))

# Removed patches with zero core area 
NCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/NCyrly_patID.tif", sep=""))#in NAD UTM 17
sNCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/sNCyrly_patID.tif", sep=""))#in WGS ALBERS EQUAL AREA
# yrly_patID2<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM2.tif", sep="")) # utm? Delete??


 
# ----------------------------------------------
# TABLES
# ----------------------------------------------

 
iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")

maj_categ<-read.table(paste0(Output_Folder,"maj_categ.txt"), sep=",", header=TRUE)

est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

nldev_dist<-read.table(paste0(Output_Folder,"nldev_distL.txt"), sep=",", header=TRUE)

pat_dist_min<-read.table(paste0(Output_Folder,"pat_dist_min.txt"), sep=",", header=TRUE)
 
iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)


gap_all<-read.table(paste0(Output_Folder,"gap_all",".txt"), sep=",", header=TRUE)

nlcd_all<-read.table(paste0(Output_Folder,"nlcd_all",".txt"), sep=",", header=TRUE)


resil_all<-read.table(paste0(Output_Folder,"resil_all",".txt"), sep=",", header=TRUE)

 
# ----------------------------------------------
# ----------------------------------------------
# RESULTS
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# CUMULATIVE AREA
# ----------------------------------------------

cum_TArER<-read.table(paste0(Output_Folder,"cum_TArER.txt"), sep=",", header=TRUE)
cum_ArOwn123<-read.table(paste0(Output_Folder,"cum_ArOwn123.txt"), sep=",", header=TRUE)
cum_NL123<-read.table(paste0(Output_Folder,"cum_NL123.txt"), sep=",", header=TRUE)
cum_GAP123<-read.table(paste0(Output_Folder,"cum_GAP123.txt"), sep=",", header=TRUE)

# Edited for 
cum_TArER_EDIT    <-read.table(paste0(Output_Folder,"cum_TArER_EDIT.txt"), sep="\t", header=TRUE)
cum_ArOwn123_EDIT <-read.table(paste0(Output_Folder,"cum_ArOwn123_EDIT.txt"), sep="\t", header=TRUE)
cum_NL123_EDIT    <-read.table(paste0(Output_Folder,"cum_NL123_EDIT.txt"), sep="\t", header=TRUE)
cum_GAP123_EDIT   <-read.table(paste0(Output_Folder,"cum_GAP123_EDIT.txt"), sep="\t", header=TRUE)




# ----------------------------------------------
# COALESCED CORE AREA 
# ----------------------------------------------


coal_vals1111 <- raster(paste0(BR_fileLoc, "coal_vals1111.tif", sep=""))
coal_vals8595 <- raster(paste0(BR_fileLoc, "coal_vals8595.tif", sep=""))
coal_vals9505 <- raster(paste0(BR_fileLoc, "coal_vals9505.tif", sep=""))
coal_vals0515 <- raster(paste0(BR_fileLoc, "coal_vals0515.tif", sep=""))

# ----------------------------------------------
# Nearest Neighbot Regression Outputs - Version 12-15-16
# ----------------------------------------------

# NN prediction
NNpred123<-read.table(paste0(Output_Folder,"NNpred123.txt"), sep=",", header=TRUE)


> summary(nnB); summary(nnP)

Call:
glm(formula = sqrt(min_dist.km) ~ estYr, data = dist_n1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0534  -0.7075  -0.2304   0.5754   3.4236  

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept) 37.202374  13.430689   2.770  0.00588 **
estYr       -0.018202   0.006705  -2.715  0.00693 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.7057707)

    Null deviance: 275.51  on 384  degrees of freedom
Residual deviance: 270.31  on 383  degrees of freedom
AIC: 962.42

Number of Fisher Scoring iterations: 2


Call:
glm(formula = sqrt(min_dist.km) ~ estYr, data = dist_n23)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0796  -0.7910  -0.1411   0.5091   3.4071  

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 27.116821   7.842051   3.458 0.000564 ***
estYr       -0.013117   0.003914  -3.351 0.000831 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.7909091)

    Null deviance: 915.26  on 1147  degrees of freedom
Residual deviance: 906.38  on 1146  degrees of freedom
AIC: 2992.6

Number of Fisher Scoring iterations: 2

