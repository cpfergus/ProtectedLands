############################ 
#PURPOSE: Create patch statistics table for each unique patch. Do by year and by 10 year increments between 1985-2015. 
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:
#	UNKNOWN protected lands are labeled with '9999'
#	'PERSISTENT' protected lands are labeled with '1111'
#	 protected lands are labeled with '8888'
#	Time for study area basic list loop = Time difference of 49.84417 mins
# Clean script, remove file locations that are not neccesary.
#IMPORTANT: 

# remove dplyr in order to run clump

##### NEXT STEPS #####
# 		Think about if you want zonal stats here or later. 
# There is something weird going on with no data values for the increment files, both year and patch id. see one note notes and: http://gis.stackexchange.com/questions/29702/nodata-values-recognised-in-raster-in-arcgis-but-not-following-symbology-rules
############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# ----------------------------------------------
################################################



# PACKAGES NEEDED
library(raster) # LOAD first alone. add other packages as needed. The function "mask" gets replaced by Hmisc - that is why you get an error with mask()
library(rgdal) # For writing .tif files
# library(stringr) # for modifying character strings
require(SDMTools) #PatchStat()
require(Hmisc)
require(igraph) # Run function clump().
library(dplyr) # remove dplyr in order to run clump detach(name="package:dplyr", unload=TRUE)


###############################

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/") # HF
# rasterOptions(tmpdir = "V:/IaraSpatialLayers/rtempCLEARME") #SCBI


# ----------------------------------------------
# FILE LOCATIONS: 

# ----------------------------------------------
# OUTPUT FILES:
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
CombRas_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Rasters/"
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# Final_output<-"Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/" # final (2011) histogram by county


# file name:  / R label: 

# ----------------------------------------------
# INPUT FILES: 
# ** Select only what is inside the study area

# Set location for the input study area rasters
inRasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/" #HF server
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) #seeking large resolution one.

# Unique Patch ID - patches distinguished by YEAR
yrly_patID<- raster(paste0(BR_fileLoc, "yrly_patID.tif", sep=""))

# Protected y/n 0 or 1
prot_yn <- raster(paste0(BR_fileLoc, "prot_yn.tif" )) # 0 or 1

#NAs in pl_year within study area to zero
pl_yr_z<-raster(paste0(BR_fileLoc, "pl_yr_z.tif"))


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# ----------------------------------------------
# CREATE est_yr TABLE
# ----------------------------------------------
# ----------------------------------------------

yrly_vals <- getValues(pl_year)

unique(yrly_vals) 

yrly_p <- setValues(yrly_patID, yrly_vals) 

est_yr <- as.data.frame(zonal(yrly_p, yrly_patID, na.rm=TRUE))

colnames(est_yr) <- c("patchID", "estYr")

# WRITE TO FILE
write.table(est_yr, file = paste0(Output_Folder,"est_yr",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
est_yr<-read.table(paste0(Output_Folder,"est_yr",".txt"), sep=",", header=TRUE)

# ----------------------------------------------
# ----------------------------------------------
# PATCH STATS FOR *CHANGE* - PART A
# ----------------------------------------------
# ----------------------------------------------


# QC: Plot and find unique vals
par(mfrow=c(2,2))
plot(pl_year, useRaster=FALSE); plot(yrly_patID, useRaster=FALSE); plot(prot_yn, useRaster=FALSE); plot(pl_yr_z, useRaster=FALSE)
unique(prot_yn); unique(pl_yr_z)
# [1] 0 1
  # [1]    0 1800 1819 1866 1870 1876 1890 1894 1895 1902 1903 1905 1907 1909 1915 1916 1917 1918 1920
 # [20] 1922 1923 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941
 # [39] 1944 1945 1946 1947 1948 1950 1951 1952 1953 1954 1955 1956 1958 1959 1961 1962 1963 1964 1965
 # [58] 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984
 # [77] 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003
 # [96] 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999
# 

# ----------------------------------------------
# SEPARATE RASTERS BY TIME INCREMENTS
# ----------------------------------------------
yr_vals <- getValues(pl_yr_z)

sort(unique(yr_vals))
# [1]    0 1800 1819 1866 1870 1876 1890 1894 1895 1902 1903 1905 1907 1909 1915 1916 1917 1918 1920
# [20] 1922 1923 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941
# [39] 1944 1945 1946 1947 1948 1950 1951 1952 1953 1954 1955 1956 1958 1959 1961 1962 1963 1964 1965
# [58] 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984
# [77] 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003
# [96] 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999

pl_1985vals <- ifelse(yr_vals <= 1985| is.na(yr_vals), yr_vals, 0) 
pl_85 <- setValues(pl_yr_z, pl_1985vals)

pl_1995vals <- ifelse(yr_vals <= 1995| is.na(yr_vals), yr_vals, 0)
pl_95 <- setValues(pl_yr_z, pl_1995vals)

pl_2005vals <- ifelse(yr_vals <= 2005| is.na(yr_vals), yr_vals, 0)
pl_05 <- setValues(pl_yr_z, pl_2005vals)

pl_2015vals <- ifelse(yr_vals <= 2015| is.na(yr_vals), yr_vals, 0)
pl_15 <- setValues(pl_yr_z, pl_2015vals)

# WRITE TO FILE
writeRaster(pl_85, filename=paste0(BR_fileLoc, "pl_85.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_95, filename=paste0(BR_fileLoc, "pl_95.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_05, filename=paste0(BR_fileLoc, "pl_05.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_15, filename=paste0(BR_fileLoc, "pl_15.tif", sep=""), format="GTiff", overwrite=TRUE)

# READ FROM FILE
pl_85<-raster(paste0(BR_fileLoc, "pl_85.tif"))
pl_95<-raster(paste0(BR_fileLoc, "pl_95.tif"))
pl_05<-raster(paste0(BR_fileLoc, "pl_05.tif"))
pl_15<-raster(paste0(BR_fileLoc, "pl_15.tif"))

# # QC: Plot rasters
# par(mfrow=c(2,2))
# plot(pl_85, useRaster=FALSE, main="pl_85"); plot(pl_95, useRaster=FALSE, main="pl_95"); plot(pl_05, useRaster=FALSE, main="pl_05"); plot(pl_15, useRaster=FALSE, main="pl_15")

# unique(pl_85); unique(pl_95); unique(pl_05); unique(pl_15)

# ----------------------------------------------
# CREATE CHANGE RASTERS FOR EACH TIME INCREMENT
# ----------------------------------------------

# 30 years of protected lands change. Should produce 3 change rasters.
PL_trans <-  list('pl_8595' = c('pl_85.tif', 'pl_95.tif', 'pl_Comb8595.txt', 'pl_Comb8595'), 'pl_9505' = c('pl_95.tif', 'pl_05.tif', 'pl_Comb9505.txt', 'pl_Comb9505'), 'pl_0515' = c('pl_05.tif', 'pl_15.tif', 'pl_Comb0515.txt', 'pl_Comb0515'))

  # StaticPL does not include years *after* 2005 because they cannot be static in this time increment changes
StaticPL <- c(18001800, 18191819, 18661866, 18701870, 18761876, 18901890, 18941894, 18951895, 19021902, 19031903, 19051905, 19071907, 19091909, 19151915, 19161916, 19171917, 19181918, 19201920, 19221922, 19231923, 19251925, 19261926, 19271927, 19281928, 19291929, 19301930, 19311931, 19321932, 19331933, 19341934, 19351935, 19361936, 19371937, 19381938, 19391939, 19401940, 19411941, 19441944, 19451945, 19461946, 19471947, 19481948, 19501950, 19511951, 19521952, 19531953, 19541954, 19551955, 19561956, 19571957, 19581958, 19591959, 19611961, 19621962, 19631963, 19641964, 19651965, 19661966, 19671967, 19681968, 19691969, 19701970, 19711971, 19721972, 19731973, 19741974, 19751975, 19761976, 19771977, 19781978, 19791979, 19801980, 19811981, 19821982, 19831983, 19841984, 19851985, 19861986, 19871987, 19881988, 19891989, 19901990, 19911991, 19921992, 19931993, 19941994, 19951995, 19961996, 19971997, 19981998, 19991999, 20002000, 20012001, 20022002, 20032003, 20042004, 20052005)

 library(stringr)
 	
old<-Sys.time() 
for(in_to_fin in names(PL_trans)){ # Makes code flexible for use with more than 2 landscapes. 
	print(in_to_fin)
	# print(paste0(CombRas_output, PL_trans[[in_to_fin]][4], ".tif", sep=""))
		
	PL_Initial <- paste0(BR_fileLoc, PL_trans[[in_to_fin]][1])
	PL_Final <-paste0(BR_fileLoc, PL_trans[[in_to_fin]][2]) 

	PL_InitialRas <- raster(PL_Initial)
	PL_FinalRas <- raster(PL_Final)
  
	PL_InitialVals <- getValues(PL_InitialRas)
	PL_FinalVals <- getValues(PL_FinalRas)

	  Change_Values <- paste0(PL_InitialVals, PL_FinalVals, sep="")
	  Change_Values[Change_Values == "NANA"] <- NA
	  Change_Values <- ifelse(Change_Values %in% StaticPL, 11111, Change_Values)
	  # Change_Values <- ifelse(Change_Values %in% paste0("0",u_yr)| is.na(Change_Values), Change_Values , 0) 
	  Change_Values <- str_sub(Change_Values, start=2)
	  	  
	  Change_Raster <- PL_FinalRas 
	  Change_Raster <- setValues(Change_Raster, as.numeric(Change_Values)) 
			
	writeRaster(Change_Raster, filename=paste0(CombRas_output, PL_trans[[in_to_fin]][4], ".tif", sep=""), format='GTiff', overwrite=TRUE)
				
	print(paste0("Saved to file : ", PL_trans[[in_to_fin]][4]))
	
	# comb_hist_output <- paste0(Output_Folder, gsub(".img","_hist.txt",PL_trans[[in_to_fin]][3]))
	# Final_hist_output <- paste0(Output_Folder, gsub(".img","_hist.txt",PL_trans[[in_to_fin]][2]))
	
}
new<-Sys.time()-old
print(new)
# Time difference of 2.924009 mins

########################################################################

# READ FROM FILE
pl_8595<-raster(paste0(CombRas_output, "pl_Comb8595.tif"))
pl_9505<-raster(paste0(CombRas_output, "pl_Comb9505.tif"))
pl_0515<-raster(paste0(CombRas_output, "pl_Comb0515.tif"))

# # QC: Get unique values and Plot rasters
# unique(pl_8595); unique(pl_9505); unique(pl_0515) 
 # [1]    0 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
 # [1]    0 1111 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005
 # [1]    0 1111 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015

# par(mfrow=c(1,3))
# plot(pl_8595, useRaster=FALSE, main="pl_8595"); plot(pl_9505, useRaster=FALSE, main="pl_9505"); plot(pl_0515, useRaster=FALSE, main="pl_0515")


# ----------------------------------------------
# EXTRACT PATCH ID FOR CHANGE YEARS *only* (No Persistent LU)
# ----------------------------------------------


yrlyPID_vals <- getValues(yrly_patID)
chg_yrs <-c("pl_Comb8595", "pl_Comb9505", "pl_Comb0515")

for(comb in chg_yrs){
	print(paste0(CombRas_output,comb, ".tif", sep=""))
	
	ras <- raster(paste0(CombRas_output,comb, ".tif", sep=""))
	chg_vals <- getValues(ras)
	chg_vals <- chg_vals[chg_vals>1111] # NOTE: '1111' = persistent PL
	boolean_vals <- ifelse(yr_vals %in% chg_vals, yrlyPID_vals, NA) 
	
	chg_yr <- setValues(ras, boolean_vals)

	# WRITE TO FILE
	writeRaster(chg_yr, filename=paste0(CombRas_output, comb, "patID", ".tif", sep=""), format='GTiff', overwrite=TRUE)

}

# READ FROM FILE
plID_8595<-raster(paste0(CombRas_output, "pl_Comb8595patID.tif"))
plID_9505<-raster(paste0(CombRas_output, "pl_Comb9505patID.tif"))
plID_0515<-raster(paste0(CombRas_output, "pl_Comb0515patID.tif"))

# # QC: Plot rasters
# par(mfrow=c(1,3))

# plot(plID_8595, useRaster=FALSE, main="plID_8595"); plot(plID_9505, useRaster=FALSE, main="plID_9505"); plot(plID_0515, useRaster=FALSE, main="plID_0515")


# ----------------------------------------------
# CREATE RASTER OF ADJACENT CELLS ** Make this a loop of apply at some point.
# ----------------------------------------------

#, These, are, the, changes, that, occur, between, value, =0, to, the, year,, starting, with, 1986
ProtLandConversions <- c(1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)

m<-matrix(
	c(rep(NA, 3), 1, rep(NA,3), 
	rep(NA, 1),1, 1, 1, 1, 1,rep(NA,1),
	rep(NA, 1), 1, 1, 1, 1, 1, rep(NA,1),
	1, 1, 1, 0, 1, 1, 1,
	rep(NA, 1), 1, 1, 1, 1, 1, rep(NA,1),
	rep(NA, 1),1, 1, 1, 1, 1,rep(NA,1),
	rep(NA, 3), 1, rep(NA,3)
	), nrow = 7, ncol = 7, byrow = T)
		
pl8595_vals<-getValues(pl_8595)
pl9505_vals<-getValues(pl_9505)
pl0515_vals<-getValues(pl_0515)


# ----------------------------------------------
# FOR 1985-1995 CHANGE

Adjacent_Cells <- adjacent(pl_8595, cell=which(pl8595_vals== 1111),directions = m, pairs = F, include=F, sorted=TRUE, target= which(pl8595_vals != 1111& pl8595_vals !=0 & pl8595_vals %in% ProtLandConversions))

pl8595_vals[Adjacent_Cells] <- 1 ;  pl8595_vals[!Adjacent_Cells] <- NA #make values from lu raster (pl8595_vals) that are adjacent =1 and those that are not adjacent =0
adj8595_raster <- setValues(pl_8595,pl8595_vals) # create a new raster that sets the LU change raster values to 1 or 0 depending on if they are adjacent to persistent lu or not.

writeRaster(adj8595_raster, filename=paste0(CombRas_output, "adj_8595.tif", sep=""), format='GTiff', overwrite=TRUE)
				
# Get Values from adj_raster
adj8595_vals <- getValues(adj8595_raster);	adj8595_vals <- which(adj8595_vals == 1) # mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.

# ----------------------------------------------
# FOR 1995-2005 CHANGE

Adjacent_Cells <- adjacent(pl_9505, cell=which(pl9505_vals== 1111),directions = m, pairs = F, include=F, sorted=TRUE, target= which(pl9505_vals != 1111& pl9505_vals !=0 & pl9505_vals %in% ProtLandConversions))

pl9505_vals[Adjacent_Cells] <- 1 ;  pl9505_vals[!Adjacent_Cells] <- NA #make values from lu raster (pl9505_vals) that are adjacent =1 and those that are not adjacent =0
adj9505_raster <- setValues(pl_9505,pl9505_vals) # create a new raster that sets the LU change raster values to 1 or 0 depending on if they are adjacent to persistent lu or not.

writeRaster(adj9505_raster, filename=paste0(CombRas_output, "adj_9505.tif", sep=""), format='GTiff', overwrite=TRUE)
				
# Get Values from adj_raster
adj9505_vals <- getValues(adj9505_raster);	adj9505_vals <- which(adj9505_vals == 1) # mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.

# ----------------------------------------------
# FOR 2005-2015 CHANGE

Adjacent_Cells <- adjacent(pl_0515, cell=which(pl0515_vals== 1111),directions = m, pairs = F, include=F, sorted=TRUE, target= which(pl0515_vals != 1111& pl0515_vals !=0 & pl0515_vals %in% ProtLandConversions))

pl0515_vals[Adjacent_Cells] <- 1 ;  pl0515_vals[!Adjacent_Cells] <- NA #make values from lu raster (pl0515_vals) that are adjacent =1 and those that are not adjacent =0
adj0515_raster <- setValues(pl_0515,pl0515_vals) # create a new raster that sets the LU change raster values to 1 or 0 depending on if they are adjacent to persistent lu or not.

writeRaster(adj0515_raster, filename=paste0(CombRas_output, "adj_0515.tif", sep=""), format='GTiff', overwrite=TRUE)
				
# Get Values from adj_raster
adj0515_vals <- getValues(adj0515_raster);	adj0515_vals <- which(adj0515_vals == 1) # mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.
	
	
# QC: Plot rasters
par(mfrow=c(1,3))
plot(adj8595_raster, useRaster=FALSE, main="adj8595_raster"); plot(adj9505_raster, useRaster=FALSE, main="adj9505_raster"); plot(adj0515_raster, useRaster=FALSE, main="adj0515_raster")


# ----------------------------------------------
# RUN PATCH STATS FOR everything *prior* to 1986
# ----------------------------------------------

pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) #seeking large resolution one.

yrly_vals <- getValues(pl_year)

# Years before 1985
boolean_vals <- ifelse(yrly_vals <=1985|is.na(yrly_vals),yrly_vals,0) 
Change_Masked <- setValues(pl_year,boolean_vals) 
Change_Masked[Change_Masked <= 0]<-NA
Change_Clumps <- raster::mask(yrly_patID, Change_Masked) ##1-1035
BR_pstat <- PatchStat(Change_Clumps, cellsize = 180)

Change_Masked_vals <- getValues(Change_Masked);	Change_Masked_vals <- which(Change_Masked_vals != "NA") 
yr_patches <- extract(Change_Clumps, Change_Masked_vals) #: extract the clump ID for the previously identified patches 

BRold_Stats <- subset(BR_pstat, BR_pstat[,1]%in% unique(yr_patches))
BRold_Stats$Transition <- 1800
BRold_Stats$Type <- "noMeas"

# Unknowns (9999)
boolean_vals_unk <- ifelse(yrly_vals >=9999|is.na(yrly_vals),yrly_vals,0) 
Change_Masked_unk <- setValues(pl_year,boolean_vals_unk) 
Change_Masked_unk[Change_Masked_unk <= 0]<-NA
# Change_Masked_unk[Change_Masked_unk !=9999]<-NA
Change_Clumps_unk <- raster::mask(yrly_patID, Change_Masked_unk) ##1-975

BR_pstat_unk <- PatchStat(Change_Clumps_unk, cellsize = 180)

par(mfrow=c(1,3))
plot(yrly_patID, useRaster=FALSE); plot(Change_Masked_unk, useRaster=FALSE); plot(pl_year, useRaster=FALSE)

Change_Masked_vals_unk <- getValues(Change_Masked_unk);	Change_Masked_vals_unk <- which(Change_Masked_vals_unk != "NA") 
yr_patches_unk <- extract(Change_Clumps_unk, Change_Masked_vals_unk) #: extract the clump ID for the previously identified patches 

BRunk_Stats <- subset(BR_pstat_unk, BR_pstat_unk[,1]%in% unique(yr_patches_unk))
BRunk_Stats$Transition <- 9999
BRunk_Stats$Type <- "noMeas"


	
# ----------------------------------------------
# RECLASSIFY RASTERS FOR 30 YEARS IN 10 YR INCREMENTS
# ----------------------------------------------	

# COMPARE TO INITIAL *INCREMENT* YEAR

# Change Raster
pl_8595<-raster(paste0(CombRas_output, "pl_Comb8595.tif"))
pl_9505<-raster(paste0(CombRas_output, "pl_Comb9505.tif"))
pl_0515<-raster(paste0(CombRas_output, "pl_Comb0515.tif"))

# Adjacent Raster
adj8595_raster<-raster(paste0(CombRas_output, "adj_8595.tif"))
adj9505_raster<-raster(paste0(CombRas_output, "adj_9505.tif"))
adj0515_raster<-raster(paste0(CombRas_output, "adj_0515.tif"))

# unique(pl_8595)
 # [1]    0 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
# unique(adj8595_raster)
 # [1]    0    1 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995

# RECLASSIFY TO c(1986, 1996, 2006)
m1 <- c(1986, 1995, 1986)
rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
m2 <- c(1996, 2005, 1996)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
m3 <- c(2006, 2015, 2006)
rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)

pl_8595 <- reclassify(pl_8595, rclmat1)
pl_9505 <- reclassify(pl_9505, rclmat2)
pl_0515 <- reclassify(pl_0515, rclmat3)
adj8595_raster <- reclassify(adj8595_raster, rclmat1)
adj9505_raster <- reclassify(adj9505_raster, rclmat2)
adj0515_raster <- reclassify(adj0515_raster, rclmat3)

# QC: unique vals
unique(pl_8595); unique(pl_9505); unique(pl_0515)
unique(adj8595_raster); unique(adj9505_raster); unique(adj0515_raster)

# Get Values from adj_raster
# mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.
adj8595_vals <- getValues(adj8595_raster);	adj8595_vals <- which(adj8595_vals == 1) 
adj9505_vals <- getValues(adj9505_raster);	adj9505_vals <- which(adj9505_vals == 1) 
adj0515_vals <- getValues(adj0515_raster);	adj0515_vals <- which(adj0515_vals == 1) 

# Unique Patch ID - patches distinguished by YEAR
# yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep="")) #<- **FIX THIS LOCATION.



# ----------------------------------------------
# RUN PATCH STATS FOR THREE TIME INCREMENTS
# ----------------------------------------------



# PREPARE RASTER FOR DISTINCT ANALYSES 
change_sa <- pl_8595
ch_savalsA <- getValues(change_sa)
ch_savalsB <- ch_savalsA

	boolean_vals <- ifelse(ch_savalsB == 1986|is.na(ch_savalsB),ch_savalsB,0) # If trans values in the cty = the transition in question or if trans vals in cty mask are NA, assign the trans values for cty vals mask, other wise = 0. This gives a raster that only represents NA and transition areas.
    Change_Masked <- setValues(change_sa,boolean_vals) # create a new raster that sets the county change raster values to the value of the desired transition and NA, excluding other transitions or persistent landuses. 
	temp_mask <- Change_Masked; temp_mask[temp_mask <= 0]<-NA
	Change_Clumps <- raster::mask(yrly_patID, temp_mask)

	   print(paste("masked raster created:",1986)) 
	   	   
		# ----------------------------------------------
		# PATCHSTATS
		try(cty_pstat <- PatchStat(Change_Clumps, cellsize = 180)) #: calculate patch statistics
		
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~
		# *** RUN ONE OF THESE FOR EACH TIME INCREMENT ***
		# adj8595_vals
		try(adj_patches <- extract(Change_Clumps,adj8595_vals)) #: extract the clump ID for the previously identified adjacent cells 

		try(adj_Stats <- subset(cty_pstat, cty_pstat[,1]%in% unique(adj_patches))) #: extract the stats of clumps that contain adjacent cells
		adj_iStats8595 <- adj_Stats
		try(new_Stats <- subset(cty_pstat, cty_pstat[,1]%nin%unique(adj_patches))) #: extract the stats of clumps that don't contain adjacent cells
		new_iStats8595 <- new_Stats
		# ----------------------------------------------
	  
	  try(adj_iStats8595$Transition <- 1986)
	  try(new_iStats8595$Transition <- 1986) # get an error bc in some cases there may not be patches in this transition.

		
# ----------------------------------------------



# PREPARE RASTER FOR DISTINCT ANALYSES 
change_sa <- pl_9505
ch_savalsA <- getValues(change_sa)
ch_savalsB <- ch_savalsA

	boolean_vals <- ifelse(ch_savalsB == 1996|is.na(ch_savalsB),ch_savalsB,0) # If trans values in the cty = the transition in question or if trans vals in cty mask are NA, assign the trans values for cty vals mask, other wise = 0. This gives a raster that only represents NA and transition areas.
    Change_Masked <- setValues(change_sa,boolean_vals) # create a new raster that sets the county change raster values to the value of the desired transition and NA, excluding other transitions or persistent landuses. 
	temp_mask <- Change_Masked; temp_mask[temp_mask <= 0]<-NA
	Change_Clumps <- raster::mask(yrly_patID, temp_mask)

	   print(paste("masked raster created:",1996)) 
	   	   
		# ----------------------------------------------
		# PATCHSTATS
		try(cty_pstat <- PatchStat(Change_Clumps, cellsize = 180)) #: calculate patch statistics
		
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~
		# *** RUN ONE OF THESE FOR EACH TIME INCREMENT ***
		# adj9505_vals
		try(adj_patches <- extract(Change_Clumps,adj9505_vals)) #: extract the clump ID for the previously identified adjacent cells 

		try(adj_Stats <- subset(cty_pstat, cty_pstat[,1]%in% unique(adj_patches))) #: extract the stats of clumps that contain adjacent cells
		adj_iStats9505 <- adj_Stats
		try(new_Stats <- subset(cty_pstat, cty_pstat[,1]%nin%unique(adj_patches))) #: extract the stats of clumps that don't contain adjacent cells
		new_iStats9505 <- new_Stats
		# ----------------------------------------------
	  
	  try(adj_iStats9505$Transition <- 1996)
	  try(new_iStats9505$Transition <- 1996) # get an error bc in some cases there may not be patches in this transition.

# ----------------------------------------------



# PREPARE RASTER FOR DISTINCT ANALYSES 
change_sa <- pl_0515
ch_savalsA <- getValues(change_sa)
ch_savalsB <- ch_savalsA

	boolean_vals <- ifelse(ch_savalsB == 2006|is.na(ch_savalsB),ch_savalsB,0) # If trans values in the cty = the transition in question or if trans vals in cty mask are NA, assign the trans values for cty vals mask, other wise = 0. This gives a raster that only represents NA and transition areas.
    Change_Masked <- setValues(change_sa,boolean_vals) # create a new raster that sets the county change raster values to the value of the desired transition and NA, excluding other transitions or persistent landuses. 
	temp_mask <- Change_Masked; temp_mask[temp_mask <= 0]<-NA
	Change_Clumps <- raster::mask(yrly_patID, temp_mask)

	   print(paste("masked raster created:",2006)) 
	   	   
		# ----------------------------------------------
		# PATCHSTATS
		try(cty_pstat <- PatchStat(Change_Clumps, cellsize = 180)) #: calculate patch statistics
		
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~
		# *** RUN ONE OF THESE FOR EACH TIME INCREMENT ***
		# adj0515_vals
		try(adj_patches <- extract(Change_Clumps,adj0515_vals)) #: extract the clump ID for the previously identified adjacent cells 

		try(adj_Stats <- subset(cty_pstat, cty_pstat[,1]%in% unique(adj_patches))) #: extract the stats of clumps that contain adjacent cells
		adj_iStats0515 <- adj_Stats
		try(new_Stats <- subset(cty_pstat, cty_pstat[,1]%nin%unique(adj_patches))) #: extract the stats of clumps that don't contain adjacent cells
		new_iStats0515 <- new_Stats
		# ----------------------------------------------
	  
	  try(adj_iStats0515$Transition <- 2006)
	  try(new_iStats0515$Transition <- 2006) # get an error bc in some cases there may not be patches in this transition.

# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------
		
	library(dplyr)
		
		
		adj_iStats8595$Type <- "Expanded"
		new_iStats8595$Type <- "Separated"
		iStats8595 <- rbind(adj_iStats8595, new_iStats8595)

		adj_iStats9505$Type <- "Expanded"
		new_iStats9505$Type <- "Separated"
		iStats9505 <- rbind(adj_iStats9505, new_iStats9505)
		
		adj_iStats0515$Type <- "Expanded"
		new_iStats0515$Type <- "Separated"
		iStats0515 <- rbind(adj_iStats0515, new_iStats0515)
		
		iStatsInc <- rbind(iStats8595, iStats9505, iStats0515)
		
		# WRITE TO FILE
		write.csv(iStatsInc,paste(Output_Folder,"/iPatchStats_inc", ".csv",sep=""), row.names = F)
		
		# READ FROM FILE
		iPatch_inc<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iPatchStats_inc.csv")		
		
		#This patch file starts at patch #1038, the patch added in 1986. 
		# > str(iStatsInc)
		# 'data.frame':	7782 obs. of  14 variables:
		 # $ patchID          : int  2581 2582 2583 2585 2586 2587 2588 2597 2600 2602 ...
		 # $ n.cell           : int  83 6 8 12 15 22 6 6 5 21 ...
		 # $ n.core.cell      : int  27 0 0 0 0 6 0 0 0 3 ...
		 # $ n.edges.perimeter: int  64 14 20 16 20 20 16 10 10 24 ...
		 # $ n.edges.internal : int  268 10 12 32 40 68 8 14 10 60 ...
		 # $ area             : num  2689200 194400 259200 388800 486000 ...
		 # $ core.area        : num  874800 0 0 0 0 ...
		 # $ perimeter        : num  11520 2520 3600 2880 3600 ...
		 # $ perim.area.ratio : num  0.00428 0.01296 0.01389 0.00741 0.00741 ...
		 # $ shape.index      : num  1.68 1.4 1.67 1.14 1.25 ...
		 # $ frac.dim.index   : num  1.08 1.06 1.09 1.02 1.04 ...
		 # $ core.area.index  : num  0.325 0 0 0 0 ...
		 # $ Transition       : num  1986 1986 1986 1986 1986 ...
		# $ Type             : chr  "Expanded" "Expanded" "Expanded" "Expanded" ...
# ----------------------------------------------
# BIND WITH PATCHES PRIOR TO 1985 AND UNKNOWNS (9999)
# ----------------------------------------------

iStats_all <- rbind(BRold_Stats, iPatch_inc, BRunk_Stats) #new:14595

# ----------------------------------------------
# JOIN TO EST YEAR FILE
# ----------------------------------------------	

est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

join <- full_join(iStats_all, est_yr, by="patchID")

		
	# WRITE TO FILE
	write.csv(join,paste(Output_Folder,"/iStats_all", ".csv",sep=""), row.names = F)
	
	# READ FROM FILE
	iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")	

	# > str(iStats_all)
	# 'data.frame':	14595 obs. of  15 variables:
	 # $ patchID          : int  1 2 3 4 5 6 7 8 9 10 ...
	 # $ n.cell           : int  151 1 7 1 1 1 1 1 5 4 ...
	 # $ n.core.cell      : int  61 0 0 0 0 0 0 0 0 0 ...
	 # $ n.edges.perimeter: int  116 4 22 4 4 4 4 4 16 16 ...
	 # $ n.edges.internal : int  488 0 6 0 0 0 0 0 4 0 ...
	 # $ area             : num  4892400 32400 226800 32400 32400 ...
	 # $ core.area        : int  1976400 0 0 0 0 0 0 0 0 0 ...
	 # $ perimeter        : int  20880 720 3960 720 720 720 720 720 2880 2880 ...
	 # $ perim.area.ratio : num  0.00427 0.02222 0.01746 0.02222 0.02222 ...
	 # $ shape.index      : num  2.32 1 1.83 1 1 ...
	 # $ frac.dim.index   : num  1.11 1 1.12 1 1 ...
	 # $ core.area.index  : num  0.404 0 0 0 0 ...
	 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
	 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
	 # $ estYr            : int  1800 1800 1800 1800 1800 1800 1819 1819 1819 1819 ...
	
# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA <1

core_pat<-filter(iStats_all, n.core.cell>=1) # total = 4152 new:4121
core_pat$area.ha <- core_pat$area/10000
core_pat$core.area.ha <- core_pat$core.area/10000

str(core_pat)

# WRITE TO FILE
write.csv(core_pat,paste(Output_Folder,"/iStats_corepat", ".csv",sep=""), row.names = F)

# READ FROM FILE
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")


# > str(iStats_corepat)
# 'data.frame':	4121 obs. of  17 variables:
 # $ patchID          : int  1 11 17 49 64 69 97 120 145 158 ...
 # $ n.cell           : int  151 19 57 272 48 365 349 190 21 503 ...
 # $ n.core.cell      : int  61 1 15 8 4 109 96 3 2 75 ...
 # $ n.edges.perimeter: int  116 32 52 418 64 282 328 368 26 628 ...
 # $ n.edges.internal : int  488 44 176 670 128 1178 1068 392 58 1384 ...
 # $ area             : num  4892400 615600 1846800 8812800 1555200 ...
 # $ core.area        : int  1976400 32400 486000 259200 129600 3531600 3110400 97200 64800 2430000 ...
 # $ perimeter        : int  20880 5760 9360 75240 11520 50760 59040 66240 4680 113040 ...
 # $ perim.area.ratio : num  0.00427 0.00936 0.00507 0.00854 0.00741 ...
 # $ shape.index      : num  2.32 1.78 1.62 6.33 2.29 ...
 # $ frac.dim.index   : num  1.11 1.09 1.08 1.23 1.12 ...
 # $ core.area.index  : num  0.404 0.0526 0.2632 0.0294 0.0833 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
 # $ estYr            : int  1800 1819 1819 1819 1819 1819 1819 1819 1819 1819 ...
 # $ area.ha          : num  489.2 61.6 184.7 881.3 155.5 ...
 # $ core.area.ha     : num  197.64 3.24 48.6 25.92 12.96 ...
 

# ----------------------------------------------
# REMOVE CORE AREA ==0 FROM RASTER
# ----------------------------------------------

# Join to get est_Yr and core.area
temp <- full_join(est_yr, iStats_all, by="patchID")

core_pat<-filter(temp, core.area >= 1) 
ncor <-filter(temp, core.area == 0) 


sNCyrly_patID<-yrly_patID
sNCyrly_patID[yrly_patID %in% ncor$patchID] <- NA

# WRITE TO FILE
writeRaster(sNCyrly_patID, filename=paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""), format='GTiff', overwrite=TRUE)
	
# READ FROM FILE
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))


# QC:
length(unique(yrly_patID))
# [1] 14635, new: 14595
length(unique(sNCyrly_patID))
# [1] 4152, new: 4121
plot(yrly_patID, useRaster=FALSE); plot(sNCyrly_patID, useRaster=FALSE)


	

# ----------------------------------------------
# ----------------------------------------------
# CLASS STATS: 
# ----------------------------------------------
# ----------------------------------------------
{


# Class is defined as year (in 5 year increents)

# NOTES: Clip to buffer region to get stats on region? What about the problem of cutting patches in half? Do clip that includes overlapping patches? Cannot do this on a raster. Use the polygon layers that have already been divided into year increments via the getis ord analysis? BUT need further divisiion even... so first further select by attributes to create 4 more increments representing 5 year increments of 90, 00, 10

# Steps:
# 1) Use the select by location tool and click all the layers you want selected from. Use "intersect the source layer feature " and eExport each selection to new layer
# 2) Convert all to raster so can run ClassStats on it. Bulk tool export to this location: BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# ***INCLUDES BLUE RIDGE AND PIEDMONT, WITH PATCHES THAT OVERLAP BOUNDARY (SEE ABOVE)


# ----------------------------------------------
# USING INCREMENTAL YEAR AS THE CLASS CATEGORY
# ----------------------------------------------

# READ RASTERS INTO R
pl_85cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1985cl.tif'))
pl_90cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1990cl.tif'))
pl_95cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1995cl.tif'))
pl_00cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2000cl.tif'))
pl_05cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2005cl.tif'))
pl_10cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2010cl.tif'))
pl_15cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2015cl.tif'))

# QC:
unique(pl_85cl);unique(pl_90cl);unique(pl_95cl);unique(pl_00cl);unique(pl_05cl);unique(pl_10cl);unique(pl_15cl)

ras_inc <- c(pl_85cl, pl_90cl, pl_95cl, pl_00cl, pl_05cl, pl_10cl, pl_15cl)
names(ras_inc)<- c("pl_85cl", "pl_90cl", "pl_95cl", "pl_00cl", "pl_05cl", "pl_10cl", "pl_15cl")


CS_inc<-list()
c_r<-1
old<-Sys.time()
for(r in 1:length(ras_inc)){
	print(paste0(names(ras_inc[r])))
	
	# Change protected areas to 1 and zeros to NA
	ras_inc[[r]][ras_inc[[r]] >0&ras_inc[[r]] <9999]<-1
	ras_inc[[r]][ras_inc[[r]] == 0]<-NA
	
	# ClassStatSel
	CS_inc[[c_r]] <- ClassStatSel(ras_inc[[r]], cellsize = 180)
	c_r<-c_r+1
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 3.885805 mins


CS_incall <- bind_rows(CS_inc)
CS_incall$yrInt <- c(1985, 1990, 1995, 2000, 2005, 2010, 2015)
CS_incall$total.core.area.ha <- CS_incall$total.core.area/10000




# WRITE TO FILE
write.table(CS_incall, file = paste0(Output_Folder,"CS_incall",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
CS_incall<-read.table(paste0(Output_Folder,"CS_incall.txt"), sep=",", header=TRUE)

# > str(CS_incall)
# 'data.frame':	7 obs. of  12 variables:
 # $ class               : int  1 1 1 1 1 1 1
 # $ n.patches           : int  342 407 484 605 816 1067 1185
 # $ total.area          : num  1.83e+10 1.85e+10 1.87e+10 1.98e+10 2.05e+10 ...
 # $ prop.landscape      : int  1 1 1 1 1 1 1
 # $ patch.density       : num  1.87e-08 2.21e-08 2.59e-08 3.06e-08 3.98e-08 ...
 # $ largest.patch.index : num  0.341 0.338 0.335 0.317 0.308 ...
 # $ total.core.area     : num  1.48e+10 1.48e+10 1.50e+10 1.57e+10 1.60e+10 ...
 # $ aggregation.index   : num  94.9 94.7 94.6 94.4 94 ...
 # $ patch.cohesion.index: num  9.95 9.95 9.95 9.95 9.95 ...
 # $ er                  : Factor w/ 1 level "Blue Ridge": 1 1 1 1 1 1 1
 # $ yrInt               : int  1985 1990 1995 2000 2005 2010 2015
 # $ total.core.area.ha  : num  1475473 1483444 1495727 1566180 1600566 ...
  
 
# ----------------------------------------------
# USING NLCD AS THE CLASS CATEGORY
# ** Need to go to raster relationships and calc nlcd maj table and raster first
# ----------------------------------------------

CS_nlcd <- ClassStatSel(nlcd_maj_ras, cellsize = 180)



pl_nlcd <- 	raster(paste0(BR_fileLoc, "pl_nlcd.tif" ))
sNCyrly_patID<- raster(paste0(BR_fileLoc, "sNCyrly_patID.tif", sep=""))

# READ FROM FILE
nlcd_maj_ras<- raster(paste0(BR_fileLoc, "nlcd_maj_ras.tif", sep=""))

# READ RASTERS INTO R
pl_85cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1985cl.tif'))
pl_90cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1990cl.tif'))
pl_95cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID1995cl.tif'))
pl_00cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2000cl.tif'))
pl_05cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2005cl.tif'))
pl_10cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2010cl.tif'))
pl_15cl<-raster(paste0(BR_fileLoc,'sNCyrly_patID2015cl.tif'))


ext <- extent(nlcd_maj_ras)
# > ext
# class       : Extent 
# xmin        : 819389.5 
# xmax        : 1706429 
# ymin        : 1078140 
# ymax        : 2124660

pl_85cl <- setExtent(pl_85cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_90cl <- setExtent(pl_90cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_95cl <- setExtent(pl_95cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_00cl <- setExtent(pl_00cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_05cl <- setExtent(pl_05cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_10cl <- setExtent(pl_10cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)
pl_15cl <- setExtent(pl_15cl, nlcd_maj_ras, keepres=TRUE, snap=TRUE)

pl_85m <- mask(nlcd_maj_ras, pl_85cl)
pl_90m <- mask(nlcd_maj_ras, pl_90cl)
pl_95m <- mask(nlcd_maj_ras, pl_95cl)
pl_00m <- mask(nlcd_maj_ras, pl_00cl)
pl_05m <- mask(nlcd_maj_ras, pl_05cl)
pl_10m <- mask(nlcd_maj_ras, pl_10cl)
pl_15m <- mask(nlcd_maj_ras, pl_15cl)


# WRITE TO FILE
writeRaster(pl_85m , filename=paste0(BR_fileLoc, "pl_85m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_90m , filename=paste0(BR_fileLoc, "pl_90m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_95m , filename=paste0(BR_fileLoc, "pl_95m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_00m , filename=paste0(BR_fileLoc, "pl_00m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_05m , filename=paste0(BR_fileLoc, "pl_05m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_10m , filename=paste0(BR_fileLoc, "pl_10m.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(pl_15m , filename=paste0(BR_fileLoc, "pl_15m.tif", sep=""), format='GTiff', overwrite=TRUE)

	
# READ FROM FILE
pl_85m<- raster(paste0(BR_fileLoc, "pl_85m.tif", sep=""))
pl_90m<- raster(paste0(BR_fileLoc, "pl_90m.tif", sep=""))
pl_95m<- raster(paste0(BR_fileLoc, "pl_95m.tif", sep=""))
pl_00m<- raster(paste0(BR_fileLoc, "pl_00m.tif", sep=""))
pl_05m<- raster(paste0(BR_fileLoc, "pl_05m.tif", sep=""))
pl_10m<- raster(paste0(BR_fileLoc, "pl_10m.tif", sep=""))
pl_15m<- raster(paste0(BR_fileLoc, "pl_15m.tif", sep=""))




# QC:
# unique(pl_85m);unique(pl_90m);unique(pl_95m);unique(pl_00m);unique(pl_05m);unique(pl_10m);unique(pl_15m)

ras_inc <- c(pl_85m, pl_90m, pl_95m, pl_00m, pl_05m, pl_10m, pl_15m)
names(ras_inc)<- c("pl_85m", "pl_90m", "pl_95m", "pl_00m", "pl_05m", "pl_10m", "pl_15m")


CS_nlcd<-list()
c_r<-1
old<-Sys.time()
for(r in 1:length(ras_inc)){
	print(paste0(names(ras_inc[r])))

	# ClassStatSel
	CS_nlcd[[c_r]] <- ClassStatSel(ras_inc[[r]], cellsize = 180)
	c_r<-c_r+1
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 3.885805 mins


CS_nlcdall <- bind_rows(CS_nlcd)
CS_nlcdall$yrInt <- c(rep(1985,7), rep(1990,7), rep(1995,7), rep(2000,7), rep(2005,7), rep(2010,7), rep(2015,7))
CS_nlcdall$total.core.area.ha <- CS_nlcdall$total.core.area/10000


# WRITE TO FILE
write.table(CS_nlcdall, file = paste0(Output_Folder,"CS_nlcdall",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
CS_nlcdall<-read.table(paste0(Output_Folder,"CS_nlcdall.txt"), sep=",", header=TRUE)




 
}




















