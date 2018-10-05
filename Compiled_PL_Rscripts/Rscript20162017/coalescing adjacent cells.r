

# NOTE: 12-20-16 - I've used the stuff at the end of the patch stats script to do this.
# ----------------------------------------------
# 30 YEARS IN 10 YR INCREMENTS
# ----------------------------------------------		

# COMPARE TO INITIAL *INCREMENT* YEAR

# Change Raster - These do NOT include 9999 (Unknowns)
pl_8595<-raster(paste0(CombRas_output, "pl_Comb8595.tif"))
pl_9505<-raster(paste0(CombRas_output, "pl_Comb9505.tif"))
pl_0515<-raster(paste0(CombRas_output, "pl_Comb0515.tif"))


# unique(pl_8595)
 # [1]    0 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
# unique(adj8595_raster)
 # [1]    0    1 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995

# RECLASSIFY TO c(1986, 1996, 2006)
m1 <- c(1985, 1995, 0)
rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
m2 <- c(1985, 1995, 1111)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
m3 <- c(1995, 2005, 1111)
rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
m4 <- c(2005, 2015, 1111)
rclmat4 <- matrix(m4, ncol=3, byrow=TRUE)

pl_1111 <- reclassify(pl_8595, rclmat1)
pl_8595 <- reclassify(pl_8595, rclmat2)
pl_9505 <- reclassify(pl_9505, rclmat3)
pl_0515 <- reclassify(pl_0515, rclmat4)


# QC: unique vals
unique(pl_1111); unique(pl_8595); unique(pl_9505); unique(pl_0515)
unique(adj1111_raster); unique(adj8595_raster); unique(adj9505_raster); unique(adj0515_raster)
plot(pl_1111, useRaster=FALSE); plot(pl_8595, useRaster=FALSE); plot(pl_9505, useRaster=FALSE); plot(pl_0515, useRaster=FALSE)  

# ----------------------------------------------
# RUN PATCH STATS FOR THREE TIME INCREMENTS
# ----------------------------------------------


	vals1111 <- getValues(pl_1111)

	boolean_vals1111 <- ifelse(vals1111 == 1111|is.na(vals1111),vals1111,0) 
    Change_vals1111 <- setValues(pl_8595,boolean_vals1111) 
	temp_vals1111 <- Change_vals1111; temp_vals1111[temp_vals1111 <= 0]<-NA
	
	vals8595 <- getValues(pl_8595)
	
	boolean_vals8595 <- ifelse(vals8595 == 1111|is.na(vals8595),vals8595,0) 
    Change_vals8595 <- setValues(pl_8595,boolean_vals8595) 
	temp_vals8595 <- Change_vals8595; temp_vals8595[temp_vals8595 <= 0]<-NA
	
	vals9505 <- getValues(pl_9505)
	
	boolean_vals9505 <- ifelse(vals9505 == 1111|is.na(vals9505),vals9505,0) 
    Change_vals9505 <- setValues(pl_9505,boolean_vals9505) 
	temp_vals9505 <- Change_vals9505; temp_vals9505[temp_vals9505 <= 0]<-NA
	
	vals0515 <- getValues(pl_0515)
	
	boolean_vals0515 <- ifelse(vals0515 == 1111|is.na(vals0515),vals0515,0) 
    Change_vals0515 <- setValues(pl_0515,boolean_vals0515) 
	temp_vals0515 <- Change_vals0515; temp_vals0515[temp_vals0515 <= 0]<-NA
		
	unique(vals1111); unique(vals8595); unique(vals9505); unique(vals0515)
	unique(boolean_vals1111); unique(boolean_vals8595); unique(boolean_vals9505); unique(boolean_vals0515)
	unique(temp_vals1111); unique(temp_vals8595); unique(temp_vals9505); unique(temp_vals0515)
	plot(Change_vals1111, useRaster=FALSE); plot(Change_vals8595, useRaster=FALSE); plot(Change_vals9505, useRaster=FALSE); plot(Change_vals0515, useRaster=FALSE)  
	
	
	# clump class raster
		coal_vals1111 <- clump(temp_vals1111, gap=FALSE)
		coal_vals8595 <- clump(temp_vals8595, gap=FALSE)
		coal_vals9505 <- clump(temp_vals9505, gap=FALSE)
		coal_vals0515 <- clump(temp_vals0515, gap=FALSE)
		
		writeRaster(coal_vals1111, filename=paste0(BR_fileLoc, "coal_vals1111.tif", sep=""), format="GTiff", overwrite=TRUE)
		writeRaster(coal_vals8595, filename=paste0(BR_fileLoc, "coal_vals8595.tif", sep=""), format="GTiff", overwrite=TRUE)
		writeRaster(coal_vals9505, filename=paste0(BR_fileLoc, "coal_vals9505.tif", sep=""), format="GTiff", overwrite=TRUE)
		writeRaster(coal_vals0515, filename=paste0(BR_fileLoc, "coal_vals0515.tif", sep=""), format="GTiff", overwrite=TRUE)

# ----------------------------------------------		
### MUST CLIP TO STUDY AREA	- ***used ARC MAP - use input features, but do NOT maintain clipping extent, 
# ----------------------------------------------

# ----------------------------------------------
# BLUE RIDGE:							
	coal_vals1111B <- raster(paste0(BR_fileLoc, "coal_vals1111clB.tif", sep=""))
	coal_vals8595B <- raster(paste0(BR_fileLoc, "coal_vals8595clB.tif", sep=""))
	coal_vals9505B <- raster(paste0(BR_fileLoc, "coal_vals9505clB.tif", sep=""))
	coal_vals0515B <- raster(paste0(BR_fileLoc, "coal_vals0515clB.tif", sep=""))
	
	coalPS_1111B <- PatchStat(coal_vals1111B , cellsize = 180)
	coalPS_8595B <- PatchStat(coal_vals8595B , cellsize = 180)
	coalPS_9505B <- PatchStat(coal_vals9505B , cellsize = 180)
	coalPS_0515B <- PatchStat(coal_vals0515B , cellsize = 180)
			
	# Create new patch ID that is unique to each increment
	coalPS_1111B$patchID<-1:nrow(coalPS_1111B)
	coalPS_8595B$patchID<-paste0(1+coalPS_1111B$patchID[nrow(coalPS_1111B)]):paste0((coalPS_1111B$patchID[nrow(coalPS_1111B)])+nrow(coalPS_8595B))
	coalPS_9505B$patchID<-paste0(1+coalPS_8595B$patchID[nrow(coalPS_8595B)]):paste0((coalPS_8595B$patchID[nrow(coalPS_8595B)])+nrow(coalPS_9505B))
	coalPS_0515B$patchID<-paste0(1+coalPS_9505B$patchID[nrow(coalPS_9505B)]):paste0((coalPS_9505B$patchID[nrow(coalPS_9505B)])+nrow(coalPS_0515B))		
		
		coalPS_1111B$yrInt <- 1985
		coalPS_8595B$yrInt <- 1995
		coalPS_9505B$yrInt <- 2005
		coalPS_0515B$yrInt <- 2015
		
		IncStatsB <- rbind(coalPS_1111B, coalPS_8595B, coalPS_9505B, coalPS_0515B)
		
		IncStatsB$er <- "Blue Ridge"

# PIEDMONT:			
	coal_vals1111P <- raster(paste0(BR_fileLoc, "coal_vals1111clP.tif", sep=""))
	coal_vals8595P <- raster(paste0(BR_fileLoc, "coal_vals8595clP.tif", sep=""))
	coal_vals9505P <- raster(paste0(BR_fileLoc, "coal_vals9505clP.tif", sep=""))
	coal_vals0515P <- raster(paste0(BR_fileLoc, "coal_vals0515clP.tif", sep=""))


	coalPS_1111P <- PatchStat(coal_vals1111P , cellsize = 180)
	coalPS_8595P <- PatchStat(coal_vals8595P , cellsize = 180)
	coalPS_9505P <- PatchStat(coal_vals9505P , cellsize = 180)
	coalPS_0515P <- PatchStat(coal_vals0515P , cellsize = 180)
			
	# Create new patch ID that is unique to each increment
	coalPS_1111P$patchID<-1:nrow(coalPS_1111P)
	coalPS_8595P$patchID<-paste0(1+coalPS_1111P$patchID[nrow(coalPS_1111P)]):paste0((coalPS_1111P$patchID[nrow(coalPS_1111P)])+nrow(coalPS_8595P))
	coalPS_9505P$patchID<-paste0(1+coalPS_8595P$patchID[nrow(coalPS_8595P)]):paste0((coalPS_8595P$patchID[nrow(coalPS_8595P)])+nrow(coalPS_9505P))
	coalPS_0515P$patchID<-paste0(1+coalPS_9505P$patchID[nrow(coalPS_9505P)]):paste0((coalPS_9505P$patchID[nrow(coalPS_9505P)])+nrow(coalPS_0515P))		
		
		coalPS_1111P$yrInt <- 1985
		coalPS_8595P$yrInt <- 1995
		coalPS_9505P$yrInt <- 2005
		coalPS_0515P$yrInt <- 2015
		
		IncStatsP <- rbind(coalPS_1111P, coalPS_8595P, coalPS_9505P, coalPS_0515P)
		IncStatsP$er <- "Piedmont"
				
		IncStats <- rbind(IncStatsB, IncStatsP)
		IncStats$core.area.ha<-IncStats$core.area/10000
		
		# WRITE TO FILE
		write.csv(IncStats,paste(Output_Folder,"/IncStats", ".csv",sep=""), row.names = F)
		
		# READ FROM FILE
		IncStats<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/IncStats.csv")	
			
				
		# ----------------------------------------------
		# REMOVE PATCHES WITH CORE AREA <1

		Inccore_pat<-filter(IncStats, core.area > 1) # total = 4152
		str(Inccore_pat)

		# WRITE TO FILE
		write.csv(Inccore_pat,paste(Output_Folder,"/Inccore_pat", ".csv",sep=""), row.names = F)

		# READ FROM FILE
		Inccore_pat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/Inccore_pat.csv")

		
>> str(Inccore_pat)
'data.frame':	2698 obs. of  15 variables:
 $ patchID          : int  1 2 7 13 14 15 16 21 23 26 ...
 $ n.cell           : int  210 175 84 22 169 18 22 38 162 1375 ...
 $ n.core.cell      : int  52 105 13 3 75 2 2 3 65 993 ...
 $ n.edges.perimeter: int  204 76 88 26 100 22 32 44 108 414 ...
 $ n.edges.internal : int  636 624 248 62 576 50 56 108 540 5086 ...
 $ area             : num  6804000 5670000 2721600 712800 5475600 ...
 $ core.area        : num  1684800 3402000 421200 97200 2430000 ...
 $ perimeter        : int  36720 13680 15840 4680 18000 3960 5760 7920 19440 74520 ...
 $ perim.area.ratio : num  0.0054 0.00241 0.00582 0.00657 0.00329 ...
 $ shape.index      : num  3.52 1.41 2.32 1.3 1.92 ...
 $ frac.dim.index   : num  1.16 1.05 1.12 1.05 1.08 ...
 $ core.area.index  : num  0.248 0.6 0.155 0.136 0.444 ...
 $ yrInt            : int  1985 1985 1985 1985 1985 1985 1985 1985 1985 1985 ...
 $ er               : Factor w/ 2 levels "Blue Ridge","Piedmont": 1 1 1 1 1 1 1 1 1 1 ...
 $ core.area.ha     : num  168.48 340.2 42.12 9.72 243 ...
		
		
		
> str(coalPS_1111)
'data.frame':	1068 obs. of  12 variables:
 $ patchID          : int  21 22 23 24 28 29 30 44 45 46 ...
 $ n.cell           : int  11 732 2 578 70 5 276 210 270 185 ...
 $ n.core.cell      : int  0 446 0 340 29 0 162 52 166 87 ...
 $ n.edges.perimeter: int  16 302 6 264 46 12 122 204 110 114 ...
 $ n.edges.internal : int  28 2626 2 2048 234 8 982 636 970 626 ...
 $ area             : num  356400 23716800 64800 18727200 2268000 ...
 $ core.area        : num  0 14450400 0 11016000 939600 ...
 $ perimeter        : num  2880 54360 1080 47520 8280 ...
 $ perim.area.ratio : num  0.00808 0.00229 0.01667 0.00254 0.00365 ...
 $ shape.index      : num  1.14 2.75 1 2.69 1.35 ...
 $ frac.dim.index   : num  1.03 1.12 1.01 1.12 1.04 ...
 $ core.area.index  : num  0 0.609 0 0.588 0.414 ...
 
> str(coalPS_8595)
'data.frame':	1506 obs. of  12 variables:
 $ patchID          : int  48 49 50 51 54 57 62 67 68 70 ...
 $ n.cell           : int  11 732 1 11 2 578 70 5 5 4 ...
 $ n.core.cell      : int  0 446 0 0 0 340 29 0 0 0 ...
 $ n.edges.perimeter: int  16 302 4 20 6 264 46 12 10 10 ...
 $ n.edges.internal : int  28 2626 0 24 2 2048 234 8 10 6 ...
 $ area             : num  356400 23716800 32400 356400 64800 ...
 $ core.area        : num  0 14450400 0 0 0 ...
 $ perimeter        : num  2880 54360 720 3600 1080 ...
 $ perim.area.ratio : num  0.00808 0.00229 0.02222 0.0101 0.01667 ...
 $ shape.index      : num  1.14 2.75 1 1.43 1 ...
 $ frac.dim.index   : num  1.03 1.12 1 1.06 1.01 ...
 $ core.area.index  : num  0 0.609 0 0 0 ...

> str(coalPS_9505)
'data.frame':	2235 obs. of  12 variables:
 $ patchID          : int  121 123 124 126 130 134 136 144 147 153 ...
 $ n.cell           : int  23 803 8 11 1 11 610 11 6 2 ...
 $ n.core.cell      : int  3 457 0 0 0 0 348 0 0 0 ...
 $ n.edges.perimeter: int  30 380 14 16 4 20 294 24 14 6 ...
 $ n.edges.internal : int  62 2832 18 28 0 24 2146 20 10 2 ...
 $ area             : num  745200 26017200 259200 356400 32400 ...
 $ core.area        : num  97200 14806800 0 0 0 ...
 $ perimeter        : num  5400 68400 2520 2880 720 ...
 $ perim.area.ratio : num  0.00725 0.00263 0.00972 0.00808 0.02222 ...
 $ shape.index      : num  1.5 3.33 1.17 1.14 1 ...
 $ frac.dim.index   : num  1.07 1.14 1.03 1.03 1 ...
 $ core.area.index  : num  0.13 0.569 0 0 0 ...

 > str(coalPS_0515)
'data.frame':	2942 obs. of  12 variables:
 $ patchID          : int  177 178 181 182 185 189 193 196 209 210 ...
 $ n.cell           : int  23 3 812 8 11 1 11 634 11 12 ...
 $ n.core.cell      : int  3 0 457 0 0 0 0 356 0 0 ...
 $ n.edges.perimeter: int  30 8 388 14 16 4 20 318 24 20 ...
 $ n.edges.internal : int  62 4 2860 18 28 0 24 2218 20 28 ...
 $ area             : num  745200 97200 26308800 259200 356400 ...
 $ core.area        : num  97200 0 14806800 0 0 ...
 $ perimeter        : num  5400 1440 69840 2520 2880 ...
 $ perim.area.ratio : num  0.00725 0.01481 0.00265 0.00972 0.00808 ...
 $ shape.index      : num  1.5 1 3.4 1.17 1.14 ...
 $ frac.dim.index   : num  1.07 1.03 1.14 1.03 1.03 ...
 $ core.area.index  : num  0.13 0 0.563 0 0 ...

 
 
# ----------------------------------------------
# AREA BY ECOREGION, COALESCED AGGREGATE PATCHES FOR NEW CORE.AREA MEASURE
# ----------------------------------------------

cum_Ar1 <- group_by(pl_foc_e1) %>%
group_by(er_maj, estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

# Select only years 1985 and later
cum_Ar1 <-filter(cum_Ar1, estYr>=1985)

cum_Ar23 <- group_by(pl_foc_e23) %>%
group_by(estYr) %>%
summarize(sum = sum(area.ha)) %>%
mutate(running_total = cumsum(sum))

# Select only years 1985 and later
cum_Ar23 <-filter(cum_Ar23, estYr>=1985)




> str(Inccore_pat)
'data.frame':	2698 obs. of  15 variables:
 $ patchID          : int  1 2 7 13 14 15 16 21 23 26 ...
 $ n.cell           : int  210 175 84 22 169 18 22 38 162 1375 ...
 $ n.core.cell      : int  52 105 13 3 75 2 2 3 65 993 ...
 $ n.edges.perimeter: int  204 76 88 26 100 22 32 44 108 414 ...
 $ n.edges.internal : int  636 624 248 62 576 50 56 108 540 5086 ...
 $ area             : num  6804000 5670000 2721600 712800 5475600 ...
 $ core.area        : num  1684800 3402000 421200 97200 2430000 ...
 $ perimeter        : int  36720 13680 15840 4680 18000 3960 5760 7920 19440 74520 ...
 $ perim.area.ratio : num  0.0054 0.00241 0.00582 0.00657 0.00329 ...
 $ shape.index      : num  3.52 1.41 2.32 1.3 1.92 ...
 $ frac.dim.index   : num  1.16 1.05 1.12 1.05 1.08 ...
 $ core.area.index  : num  0.248 0.6 0.155 0.136 0.444 ...
 $ yrInt            : int  1985 1985 1985 1985 1985 1985 1985 1985 1985 1985 ...
 $ er               : Factor w/ 2 levels "Blue Ridge","Piedmont": 1 1 1 1 1 1 1 1 1 1 ...
 $ core.area.ha     : num  168.48 340.2 42.12 9.72 243 ...

 

#Split by ecoregion
Inccore_pat1 <- filter(Inccore_pat, er=="Blue Ridge")#
Inccore_pat23 <- filter(Inccore_pat, er=="Piedmont")# 

cum_CAinc1 <- group_by(Inccore_pat1) %>%
group_by(er, yrInt) %>%
summarize(sum = sum(core.area.ha)) 

cum_CAinc23 <- group_by(Inccore_pat23) %>%
group_by(er, yrInt) %>%
summarize(sum = sum(core.area.ha))

cum_CAincER <- bind_rows(cum_CAinc1, cum_CAinc23)

# > cum_CAincER
# Source: local data frame [8 x 3]

          # er yrInt       sum
# 1 Blue Ridge  1985 1303685.3
# 2 Blue Ridge  1995 1315329.8
# 3 Blue Ridge  2005 1369237.0
# 4 Blue Ridge  2015 1381153.7
# 5   Piedmont  1985  151528.3
# 6   Piedmont  1995  161102.5
# 7   Piedmont  2005  189799.2
# 8   Piedmont  2015  225792.4
		  