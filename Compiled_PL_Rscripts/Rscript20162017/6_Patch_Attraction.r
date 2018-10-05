############################ 
#PURPOSE: Create map depicting patch "attraction"
#INPUT: Table on patch stats.  iStats_Join.txt
#OUTPUT: 
#DEVELOPED: 11-29-16 - Iara Lacher
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 

##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER

# ----------------------------------------------
################################################


# PACKAGES NEEDED

# Plotting
 library(raster)
 library(dplyr)

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
# Output_Folder <- "C:/Users/LacherL/Documents/AA_Smithsonian/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" #V Drive

# ----------------------------------------------
# READ INPUT FILES:
iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)

iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")	

yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ATTRACTION VALUE BASED ON FREQUENCY ALONE
# ----------------------------------------------

# Use columns "patchID" and "d_patchID"
# Create table that assigns value to patchID = number of times it is mentioned in d_patchID
freq<-as.data.frame(table(iStats_Join$d_patchID))
colnames(freq)<-c("patchID", "Freq")
 
temp<-as.data.frame(as.factor(iStats_all$patchID))
colnames(temp)<-c("patchID")

freq_join<-left_join(temp,freq, by="patchID")
freq_join[is.na(freq_join)] <- 0

# Reclassify raster based on frequency values
freq_mat<-as.matrix(sapply(freq_join, as.numeric))  

attraction <- reclassify(yrly_patID, freq_mat)

writeRaster(attraction, filename=paste0(BR_fileLoc, "attraction.tif", sep=""), format="GTiff", overwrite=TRUE)


# ----------------------------------------------
# ATTRACTION VALUE BASED ON FREQUENCY *AND* TOTAL AREA (weights)
# ----------------------------------------------

# Edited version of wpct function

library(weights)

wpct <- function (x, weight = NULL, na.rm = TRUE) 
{
    if (is.null(weight)) {
        weight <- rep(1, length(x))
    }
    y <- wtd.table(x, weight, na.rm = na.rm)$sum.of.weights/sum(as.numeric(wtd.table(x, 
        weight, na.rm = na.rm)$sum.of.weights))
    z <- as.vector(y)
    names(z) <- names(y)
    if (is.logical(x)) 
        z <- rev(z)
    z
}

wfreq<-as.data.frame(wpct(iStats_Join$d_patchID, 1/iStats_Join$area))
wfreq$patchID <- row.names(wfreq)
colnames(wfreq)<-c("wFreq", "patchID")
 
temp<-as.data.frame(as.factor(iStats_all$patchID))
colnames(temp)<-c("patchID")

wfreq_join<-left_join(temp,wfreq, by="patchID")
wfreq_join[is.na(wfreq_join)] <- 0

# # Reclassify raster based on wfrequency values
# wfreq_mat<-as.matrix(sapply(wfreq_join, as.numeric))  

# w_attraction <- reclassify(yrly_patID, wfreq_mat)

# # QC:
# # plot(w_attraction, useRaster=FALSE)

# writeRaster(w_attraction, filename=paste0(BR_fileLoc, "w_attraction2.tif", sep=""), format="GTiff", overwrite=TRUE)



# ----------------------------------------------
# NORMALIZING DATA for presentation
# ATTRACTION VALUE BASED ON FREQUENCY *AND* TOTAL AREA (weights)
# ----------------------------------------------


# separate wfreq == 0 and not
zero <- wfreq_join[wfreq_join$wFreq==0,]
notzero <- wfreq_join[!wfreq_join$wFreq==0,]

#Transform datafor patches that have nearest neighbors
notzero$wFreq <- log(notzero$wFreq)

# > summary(notzero)
   # patchID              wFreq          
 # Length:1676        Min.   :3.400e-07  
 # Class :character   1st Qu.:2.533e-04  
 # Mode  :character   Median :4.690e-04  
                    # Mean   :5.967e-04  
                    # 3rd Qu.:7.748e-04  
                    # Max.   :9.590e-03  
 # > summary(log(notzero$wFreq))
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -14.890  -8.281  -7.665  -7.859  -7.163  -4.647 
 
 
# Join data back together
wfreq_join<-rbind(notzero,zero)
 
 # Reclassify raster based on wfrequency values
wfreq_mat<-as.matrix(sapply(wfreq_join, as.numeric))  

lw_attraction <- reclassify(yrly_patID, wfreq_mat)

# QC:
# plot(lw_attraction, useRaster=FALSE)

writeRaster(lw_attraction, filename=paste0(BR_fileLoc, "lw_attraction2.tif", sep=""), format="GTiff", overwrite=TRUE)























