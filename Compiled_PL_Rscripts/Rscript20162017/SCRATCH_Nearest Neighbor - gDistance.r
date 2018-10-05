#### ORIG CODE ###

# ----------------------------------------------
# NEAREST DISTANCE BETWEEN PATCHES in km.
# ----------------------------------------------

library(raster)
library(rgeos)

cmp<-yrly_patID

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

pat_nms<-c(unique(yrly_patID))

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


> summary(nnB); summary(nnP)

Call:
glm(formula = sqrt(min_dist.km.) ~ estYr, data = dist_n1)

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
glm(formula = sqrt(min_dist.km.) ~ estYr, data = dist_n23)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.4184  -0.7037  -0.1793   0.5080   4.3349  

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 54.113831   8.165348   6.627 5.25e-11 ***
estYr       -0.026401   0.004075  -6.478 1.38e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.8698142)

    Null deviance: 1035.92  on 1150  degrees of freedom
Residual deviance:  999.42  on 1149  degrees of freedom
AIC: 3109.9

Number of Fisher Scoring iterations: 2

###########################################################################
