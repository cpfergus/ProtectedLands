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
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))

prot_yn <- raster(paste0(BR_fileLoc, "pl.tif" )) # 0 or 1


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# PATCH STATS FOR *CHANGE* - PART A
# ----------------------------------------------
# ----------------------------------------------

# First set NAs in pl_year within study area to zero. (ArcGIS Raster Calculator - Con("Protected Lands\Raster\pl.tif"==0, 0, "Protected Lands\Raster\pl_year.tif"))

# # Raster reclassified so background =zero
# pl_yr_z <- raster(paste0(BR_fileLoc, "pl_yr_z.tif" ))# 0, years, 9999
# Rpl_yr_z <- reclassify(pl_yr_z, rcl=(matrix(c(NA,0), ncol=2)))
# # WRITE TO FILE
# writeRaster(Rpl_yr_z, filename=paste0(BR_fileLoc, "pl_yr_z.tif", sep=""), format="GTiff", overwrite=TRUE)

pl_yr_z<-raster(paste0(BR_fileLoc, "pl_yr_z.tif"))

# QC: Plot and find unique vals
par(mfrow=c(1,2))
plot(prot_yn, useRaster=FALSE); plot(pl_yr_z, useRaster=FALSE)
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

  # StaticPL does not include years after 2005 because they cannot be static in this time increment changes
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
ProtLandConversions, <- c(1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)



m<-matrix(
	c(rep(NA, 3), 1, rep(NA,3), 
	rep(NA, 1),1, 1, 1, 1, 1,rep(NA,1),
	rep(NA, 1), 1, 1, 1, 1, 1, rep(NA,1),
	1, 1, 1, 0, 1, 1, 1,
	rep(NA, 1), 1, 1, 1, 1, 1, rep(NA,1),
	rep(NA, 1),1, 1, 1, 1, 1,rep(NA,1),
	rep(NA, 3), 1, rep(NA,3)
	), nrow = 7, ncol = 7, byrow = T)


# Chg_inc <- list(pl_8595, pl_9505, pl_0515)

# for(chg in 1:length(Chg_inc)){
	# print(Chg_inc[[chg]])
	
# pl8595_vals<-getValues(chg)

# Adjacent_Cells <- adjacent(chg, cell=which(pl8595_vals== 1111),directions = m, pairs = F, include=F, sorted=TRUE, target= which(pl8595_vals != 1111& pl8595_vals !=0 & pl8595_vals %in% ProtLandConversions))

# pl8595_vals[Adjacent_Cells] <- 1 ;  pl8595_vals[!Adjacent_Cells] <- NA #make values from lu raster (pl8595_vals) that are adjacent =1 and those that are not adjacent =0
# adj8595_raster <- setValues(chg,pl8595_vals) # create a new raster that sets the LU change raster values to 1 or 0 depending on if they are adjacent to persistent lu or not.

# writeRaster(adj8595_raster, filename=paste0(CombRas_output, "adj_8595.tif", sep=""), format='GTiff', overwrite=TRUE)

# }



		
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
# 30 YEARS IN 10 YR INCREMENTS
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
yrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/brPLiyrRSM.tif", sep=""))



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
	
# ----------------------------------------------
# BIND WITH PATCHES PRIOR TO 1985 AND UNKNOWNS (9999)
# ----------------------------------------------

iStats_all <- rbind(BRold_Stats, iPatch_inc, BRunk_Stats)
		
	# WRITE TO FILE
	write.csv(iStats_all,paste(Output_Folder,"/iStats_all", ".csv",sep=""), row.names = F)
	
	# READ FROM FILE
	iStats_all<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_all.csv")	

# > str(iStats_all)
# 'data.frame':	14635 obs. of  14 variables:
 # $ patchID          : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ n.cell           : int  142 5 1 4 5 1 2 1 1 1 ...
 # $ n.core.cell      : int  62 0 0 0 0 0 0 0 0 0 ...
 # $ n.edges.perimeter: int  104 10 4 10 16 4 8 4 4 4 ...
 # $ n.edges.internal : int  464 10 0 6 4 0 0 0 0 0 ...
 # $ area             : int  4600800 162000 32400 129600 162000 32400 64800 32400 32400 32400 ...
 # $ core.area        : int  2008800 0 0 0 0 0 0 0 0 0 ...
 # $ perimeter        : int  18720 1800 720 1800 2880 720 1440 720 720 720 ...
 # $ perim.area.ratio : num  0.00407 0.01111 0.02222 0.01389 0.01778 ...
 # $ shape.index      : num  2.17 1 1 1.25 1.6 ...
 # $ frac.dim.index   : num  1.1 1.02 1 1.04 1.1 ...
 # $ core.area.index  : num  0.437 0 0 0 0 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
	

# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA <1

core_pat<-filter(iStats_all, core.area > 1) # total = 4152
str(core_pat)

# WRITE TO FILE
write.csv(core_pat,paste(Output_Folder,"/iStats_corepat", ".csv",sep=""), row.names = F)

# READ FROM FILE
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")

# > str(iStats_corepat)
# 'data.frame':	4152 obs. of  14 variables:
 # $ patchID          : int  1 15 20 62 70 77 101 129 150 186 ...
 # $ n.cell           : int  142 15 56 271 40 365 346 150 33 393 ...
 # $ n.core.cell      : int  62 1 15 3 4 109 101 3 1 68 ...
 # $ n.edges.perimeter: int  104 20 54 404 54 294 316 256 56 414 ...
 # $ n.edges.internal : int  464 40 170 680 106 1166 1068 344 76 1158 ...
 # $ area             : int  4600800 486000 1814400 8780400 1296000 11826000 11210400 4860000 1069200 12733200 ...
 # $ core.area        : int  2008800 32400 486000 97200 129600 3531600 3272400 97200 32400 2203200 ...
 # $ perimeter        : int  18720 3600 9720 72720 9720 52920 56880 46080 10080 74520 ...
 # $ perim.area.ratio : num  0.00407 0.00741 0.00536 0.00828 0.0075 ...
 # $ shape.index      : num  2.17 1.25 1.8 6.12 2.08 ...
 # $ frac.dim.index   : num  1.1 1.04 1.08 1.23 1.11 ...
 # $ core.area.index  : num  0.4366 0.0667 0.2679 0.0111 0.1 ...
 # $ Transition       : int  1800 1800 1800 1800 1800 1800 1800 1800 1800 1800 ...
 # $ Type             : Factor w/ 3 levels "Expanded","noMeas",..: 2 2 2 2 2 2 2 2 2 2 ...
	

# ----------------------------------------------
# ----------------------------------------------
# CLASS STATS: 
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# SEPARATE RASTERS BY TIME INCREMENTS
# ----------------------------------------------

# No core area removed:

# Join to get est_Yr and core.area
temp <- full_join(est_yr, iStats_all, by="patchID")

core_pat<-filter(temp, core.area >= 1) 
ncor <-filter(temp, core.area == 0) 


sNCyrly_patID<-yrly_patID
sNCyrly_patID[yrly_patID %in% ncor$patchID] <- NA

# WRITE TO FILE
writeRaster(sNCyrly_patID, filename=paste0(BR_fileLoc, "IndPatches/sNCyrly_patID.tif", sep=""), format='GTiff', overwrite=TRUE)
	
# READ FROM FILE
sNCyrly_patID<- raster(paste0(BR_fileLoc, "IndPatches/sNCyrly_patID.tif", sep=""))


# QC:
# length(unique(yrly_patID))
## [1] 14635
# length(unique(sNCyrly_patID))
## [1] 4152
# plot(yrly_patID, useRaster=FALSE); plot(sNCyrly_patID, useRaster=FALSE)

pl_yr_z<-raster(paste0(BR_fileLoc, "pl_yr_z.tif"))

yr_vals <- getValues(pl_yr_z)

test<-pl_yr_z
 
	temp_mask <- sNCyrly_patID; temp_mask[temp_mask <= 0]<-NA
	NCpl_yr_z <- raster::mask(pl_yr_z, temp_mask)

	length(unique(pl_yr_z))
	length(unique(NCpl_yr_z))
	
	# WRITE TO FILE
writeRaster(NCpl_yr_z, filename=paste0(BR_fileLoc, "IndPatches/NCpl_yr_z.tif", sep=""), format='GTiff', overwrite=TRUE)
	
# READ FROM FILE
NCpl_yr_z<- raster(paste0(BR_fileLoc, "IndPatches/NCpl_yr_z.tif", sep=""))

	
# > length(unique(NCpl_yr_z))
# [1] 91
# > length(unique(pl_yr_z))
# [1] 108

# > unique(NCpl_yr_z)
 # [1] 1800 1819 1870 1890 1895 1903 1907 1909 1915 1916 1917 1918 1920 1926 1927 1929 1930 1933 1935 1936 1937 1938 1939
# [24] 1940 1941 1944 1945 1946 1948 1950 1951 1952 1954 1956 1959 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971
# [47] 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994
# [70] 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999
# > unique(pl_yr_z)
  # [1]    0 1800 1819 1866 1870 1876 1890 1894 1895 1902 1903 1905 1907 1909 1915 1916 1917 1918 1920 1922 1923 1925 1926
 # [24] 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1944 1945 1946 1947 1948 1950 1951 1952
 # [47] 1953 1954 1955 1956 1958 1959 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977
 # [70] 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000
 # [93] 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999



# ----------------------------------------------
# BLUE RIDGE:

pl_yr_zB<-raster(paste0(BR_fileLoc, "pl_yr_zclB.tif")) # this includes 9999, but the selection below does not include this to be used in the analysis.

yr_valsB <- getValues(pl_yr_zB)

sort(unique(yr_valsB))


pl_1985valsB <- ifelse(yr_valsB <= 1985| is.na(yr_valsB), yr_valsB, 0) 
pl_85B <- setValues(pl_yr_zB, pl_1985valsB)

pl_1990valsB <- ifelse(yr_valsB <= 1990| is.na(yr_valsB), yr_valsB, 0)
pl_90B <- setValues(pl_yr_zB, pl_1990valsB)

pl_1995valsB <- ifelse(yr_valsB <= 1995| is.na(yr_valsB), yr_valsB, 0)
pl_95B <- setValues(pl_yr_zB, pl_1995valsB)

pl_2000valsB <- ifelse(yr_valsB <= 2000| is.na(yr_valsB), yr_valsB, 0)
pl_00B <- setValues(pl_yr_zB, pl_2000valsB)

pl_2005valsB <- ifelse(yr_valsB <= 2005| is.na(yr_valsB), yr_valsB, 0)
pl_05B <- setValues(pl_yr_zB, pl_2005valsB)

pl_2010valsB <- ifelse(yr_valsB <= 2010| is.na(yr_valsB), yr_valsB, 0)
pl_10B <- setValues(pl_yr_zB, pl_2010valsB)

pl_2015valsB <- ifelse(yr_valsB <= 2015| is.na(yr_valsB), yr_valsB, 0)
pl_15B <- setValues(pl_yr_zB, pl_2015valsB)

# ----------------------------------------------
# PIEDMONT:

pl_yr_zP<-raster(paste0(BR_fileLoc, "pl_yr_zclP.tif"))# this includes 9999, but the selection below does not include this to be used in the analysis.

yr_valsP <- getValues(pl_yr_zP)

sort(unique(yr_valsP))


pl_1985valsP <- ifelse(yr_valsP <= 1985| is.na(yr_valsP), yr_valsP, 0) 
pl_85P <- setValues(pl_yr_zP, pl_1985valsP)

pl_1990valsP <- ifelse(yr_valsP <= 1990| is.na(yr_valsP), yr_valsP, 0)
pl_90P <- setValues(pl_yr_zP, pl_1990valsP)

pl_1995valsP <- ifelse(yr_valsP <= 1995| is.na(yr_valsP), yr_valsP, 0)
pl_95P <- setValues(pl_yr_zP, pl_1995valsP)

pl_2000valsP <- ifelse(yr_valsP <= 2000| is.na(yr_valsP), yr_valsP, 0)
pl_00P <- setValues(pl_yr_zP, pl_2000valsP)

pl_2005valsP <- ifelse(yr_valsP <= 2005| is.na(yr_valsP), yr_valsP, 0)
pl_05P <- setValues(pl_yr_zP, pl_2005valsP)

pl_2010valsP <- ifelse(yr_valsP <= 2010| is.na(yr_valsP), yr_valsP, 0)
pl_10P <- setValues(pl_yr_zP, pl_2010valsP)

pl_2015valsP <- ifelse(yr_valsP <= 2015| is.na(yr_valsP), yr_valsP, 0)
pl_15P <- setValues(pl_yr_zP, pl_2015valsP)

# unique(pl_85); unique(pl_90); unique(pl_95); unique(pl_00); unique(pl_05); unique(pl_10); unique(pl_15);


# ----------------------------------------------
# FOR EACH OF THE  INCREMENTAL LANDSCAPES
# ----------------------------------------------

# Rasters already read into R memory.
ras_inc <- c(pl_85B, pl_90B, pl_95B, pl_00B, pl_05B, pl_10B, pl_15B)
names(ras_inc)<- c("pl_85B", "pl_90B", "pl_95B", "pl_00B", "pl_05B", "pl_10B", "pl_15B")


ras_inc <- c(pl_85P, pl_90P, pl_95P, pl_00P, pl_05P, pl_10P, pl_15P)
names(ras_inc)<- c("pl_85P", "pl_90P", "pl_95P", "pl_00P", "pl_05P", "pl_10P", "pl_15P")


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
# Time difference of 2.936494 mins


CS_incB <- bind_rows(CS_inc)
CS_incB$er <- "Blue Ridge"
CS_incB$yrInt <- c(1985, 1990, 1995, 2000, 2005, 2010, 2015)
CS_incB$total.core.area.ha <- CS_incB$total.core.area/10000


CS_incP <- bind_rows(CS_inc)
CS_incP$er <- "Piedmont"
CS_incP$yrInt <- c(1985, 1990, 1995, 2000, 2005, 2010, 2015)
CS_incP$total.core.area.ha <- CS_incP$total.core.area/10000

CS_inc123 <- rbind(CS_incB, CS_incP)

# WRITE TO FILE
write.table(CS_inc123, file = paste0(Output_Folder,"CS_inc123",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
CS_inc123<-read.table(paste0(Output_Folder,"CS_inc123.txt"), sep=",", header=TRUE)

#Split by ecoregion - 7 obs in each, one per yr increment
CS_incB <- filter(CS_inc123, er=="Blue Ridge")
CS_incP <- filter(CS_inc123, er=="Piedmont")


# > str(CS_inc123)
# 'data.frame':	14 obs. of  12 variables:
 # $ class               : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ n.patches           : int  204 209 239 276 354 422 448 250 306 355 ...
 # $ total.area          : num  1.57e+10 1.58e+10 1.59e+10 1.65e+10 1.67e+10 ...
 # $ prop.landscape      : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ patch.density       : num  1.30e-08 1.33e-08 1.50e-08 1.68e-08 2.12e-08 ...
 # $ largest.patch.index : num  0.48 0.479 0.476 0.592 0.586 ...
 # $ total.core.area     : num  1.29e+10 1.30e+10 1.31e+10 1.34e+10 1.36e+10 ...
 # $ aggregation.index   : num  95.3 95.3 95.3 95.1 95 ...
 # $ patch.cohesion.index: num  9.95 9.95 9.95 9.96 9.96 ...
 # $ er                  : Factor w/ 2 levels "Blue Ridge","Piedmont": 1 1 1 1 1 1 1 2 2 2 ...
 # $ yrInt               : int  1985 1990 1995 2000 2005 2010 2015 1985 1990 1995 ...
 # $ total.core.area.ha  : num  1294160 1295316 1305198 1341590 1356808 ...





