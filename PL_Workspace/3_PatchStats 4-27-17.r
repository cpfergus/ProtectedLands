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
library(dplyr) # !! Remove dplyr in order to run clump detach(name="package:dplyr", unload=TRUE)


###############################

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/") # HF
# rasterOptions(tmpdir = "V:/IaraSpatialLayers/rtempCLEARME") #SCBI


# ----------------------------------------------
# FILE LOCATIONS: 
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"
CombRas_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Combine/Rasters/"
BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"
inRasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/" #HF server

# ----------------------------------------------
# OUTPUT FILES:

# Final_output<-"Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/" # final (2011) histogram by county


# ----------------------------------------------
# INPUT FILES: 
# ** Select only what is inside the study area


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
{
yrly_vals <- getValues(pl_year)

unique(yrly_vals) 

yrly_p <- setValues(yrly_patID, yrly_vals) 

est_yr <- as.data.frame(zonal(yrly_p, yrly_patID, na.rm=TRUE))

colnames(est_yr) <- c("patchID", "estYr")

# WRITE TO FILE
write.table(est_yr, file = paste0(Output_Folder,"est_yr",".txt"), row.names=FALSE, sep=",")

# READ FROM FILE
est_yr<-read.table(paste0(Output_Folder,"est_yr",".txt"), sep=",", header=TRUE)
}

# QC: Plot and find unique vals
par(mfrow=c(2,2))
plot(pl_year, useRaster=FALSE); plot(yrly_patID, useRaster=FALSE); plot(prot_yn, useRaster=FALSE); plot(pl_yr_z, useRaster=FALSE)
unique(prot_yn); unique(pl_yr_z)
# [1] 0 1
  # [1]    0 1800 1819 1866 1870 1876 1890 1894 1895 1898 1902 1903 1904 1905 1907 1909 1912
 # [18] 1915 1916 1917 1918 1920 1922 1923 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934
 # [35] 1935 1936 1937 1938 1939 1940 1941 1942 1944 1945 1946 1947 1948 1950 1951 1952 1953
 # [52] 1954 1956 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972
 # [69] 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989
 # [86] 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006
# [103] 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999

# ----------------------------------------------
# ----------------------------------------------
# SEPARATE RASTERS BY TIME INCREMENTS
# ----------------------------------------------
# ----------------------------------------------
{

yr_vals <- getValues(pl_yr_z)

sort(unique(yr_vals)) #112 values, including 0:
 # [1]    0 1800 1819 1866 1870 1876 1890 1894 1895 1898 1902 1903 1904 1905 1907 1909 1912
 # [18] 1915 1916 1917 1918 1920 1922 1923 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934
 # [35] 1935 1936 1937 1938 1939 1940 1941 1942 1944 1945 1946 1947 1948 1950 1951 1952 1953
 # [52] 1954 1956 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972
 # [69] 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989
 # [86] 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006
# [103] 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999

# patches ** UP TO** the year 85, 90, 95, etc. 10-yr increments are for getis ord, 5 year increments are for patch stats. 

pl_1980vals <- ifelse(yr_vals <= 1980| is.na(yr_vals), yr_vals, 0) ; pl_80 <- setValues(pl_yr_z, pl_1980vals)

pl_1985vals <- ifelse(yr_vals <= 1985| is.na(yr_vals), yr_vals, 0) ; pl_85 <- setValues(pl_yr_z, pl_1985vals)

pl_1990vals <- ifelse(yr_vals <= 1990| is.na(yr_vals), yr_vals, 0) ; pl_90 <- setValues(pl_yr_z, pl_1990vals)

pl_1995vals <- ifelse(yr_vals <= 1995| is.na(yr_vals), yr_vals, 0); pl_95 <- setValues(pl_yr_z, pl_1995vals)

pl_2000vals <- ifelse(yr_vals <= 2000| is.na(yr_vals), yr_vals, 0) ; pl_00 <- setValues(pl_yr_z, pl_2000vals)

pl_2005vals <- ifelse(yr_vals <= 2005| is.na(yr_vals), yr_vals, 0) ; pl_05 <- setValues(pl_yr_z, pl_2005vals)

pl_2010vals <- ifelse(yr_vals <= 2010| is.na(yr_vals), yr_vals, 0) ; pl_10 <- setValues(pl_yr_z, pl_2010vals)

pl_2015vals <- ifelse(yr_vals <= 2015| is.na(yr_vals), yr_vals, 0) ; pl_15 <- setValues(pl_yr_z, pl_2015vals)

# WRITE TO FILE
writeRaster(pl_80, filename=paste0(BR_fileLoc, "pl_80.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_85, filename=paste0(BR_fileLoc, "pl_85.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_90, filename=paste0(BR_fileLoc, "pl_90.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_95, filename=paste0(BR_fileLoc, "pl_95.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_00, filename=paste0(BR_fileLoc, "pl_00.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_05, filename=paste0(BR_fileLoc, "pl_05.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_10, filename=paste0(BR_fileLoc, "pl_10.tif", sep=""), format="GTiff", overwrite=TRUE)
writeRaster(pl_15, filename=paste0(BR_fileLoc, "pl_15.tif", sep=""), format="GTiff", overwrite=TRUE)


# READ FROM FILE
pl_80<-raster(paste0(BR_fileLoc, "pl_80.tif"))
pl_85<-raster(paste0(BR_fileLoc, "pl_85.tif"))
pl_90<-raster(paste0(BR_fileLoc, "pl_90.tif"))
pl_95<-raster(paste0(BR_fileLoc, "pl_95.tif"))
pl_00<-raster(paste0(BR_fileLoc, "pl_00.tif"))
pl_05<-raster(paste0(BR_fileLoc, "pl_05.tif"))
pl_10<-raster(paste0(BR_fileLoc, "pl_10.tif"))
pl_15<-raster(paste0(BR_fileLoc, "pl_15.tif"))

# # QC: Plot rasters
# par(mfrow=c(2,2))
# plot(pl_85, useRaster=FALSE, main="pl_85"); plot(pl_95, useRaster=FALSE, main="pl_95"); plot(pl_05, useRaster=FALSE, main="pl_05"); plot(pl_15, useRaster=FALSE, main="pl_15")

# unique(pl_80); unique(pl_95); unique(pl_05); unique(pl_15)
}

# ----------------------------------------------
# ----------------------------------------------
# CREATE CHANGE RASTERS FOR EACH TIME INCREMENT
# ----------------------------------------------
# ----------------------------------------------
{
# 30 years of protected lands change. Should produce 3 change rasters.

PL_trans <-  list('pl_8085' = c('pl_80.tif', 'pl_85.tif', 'pl_Comb8085.txt', 'pl_Comb8085'),'pl_8590' = c('pl_85.tif', 'pl_90.tif', 'pl_Comb8590.txt', 'pl_Comb8590'),'pl_9095' = c('pl_90.tif', 'pl_95.tif', 'pl_Comb9095.txt', 'pl_Comb9095'),'pl_9500' = c('pl_95.tif', 'pl_00.tif', 'pl_Comb9500.txt', 'pl_Comb9500'),'pl_0005' = c('pl_00.tif', 'pl_05.tif', 'pl_Comb0005.txt', 'pl_Comb0005'),'pl_0510' = c('pl_05.tif', 'pl_10.tif', 'pl_Comb0510.txt', 'pl_Comb0510'),'pl_1015' = c('pl_10.tif', 'pl_15.tif', 'pl_Comb1015.txt', 'pl_Comb1015'))

# StaticPL does not include years *after* 2010 (last transition is 2010-2015) because they will not show up since the raster pl_10 compared to pl_15 will not create duplicate values of years 2011,2012,2013,2014,2015.
  
StaticPL <- c(18001800, 18191819, 18661866, 18701870, 18761876, 18901890, 18941894, 18951895, 18981898, 19021902, 19031903, 19041904, 19051905, 19071907, 19091909, 19121912, 19151915, 19161916, 19171917, 19181918, 19201920, 19221922, 19231923, 19251925, 19261926, 19271927, 19281928, 19291929, 19301930, 19311931, 19321932, 19331933, 19341934, 19351935, 19361936, 19371937, 19381938, 19391939, 19401940, 19411941, 19421942, 19441944, 19451945, 19461946, 19471947, 19481948, 19501950, 19511951, 19521952, 19531953, 19541954, 19561956, 19581958, 19591959, 19601960, 19611961, 19621962, 19631963, 19641964, 19651965, 19661966, 19671967, 19681968, 19691969, 19701970, 19711971, 19721972, 19731973, 19741974, 19751975, 19761976, 19771977, 19781978, 19791979, 19801980, 19811981, 19821982, 19831983, 19841984, 19851985, 19861986, 19871987, 19881988, 19891989, 19901990, 19911991, 19921992, 19931993, 19941994, 19951995, 19961996, 19971997, 19981998, 19991999, 20002000, 20012001, 20022002, 20032003, 20042004, 20052005, 20062006, 20072007, 20082008, 20092009, 20102010)


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
	  Change_Values <- ifelse(Change_Values %in% StaticPL, 11111, Change_Values) # Need extra '1' because of the sub that happens, need 4 digits to remain.
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
# Time difference of 6.85 mins

########################################################################

# READ FROM FILE
pl_8085<-raster(paste0(CombRas_output, "pl_Comb8085.tif"))
pl_8590<-raster(paste0(CombRas_output, "pl_Comb8590.tif"))
pl_9095<-raster(paste0(CombRas_output, "pl_Comb9095.tif"))
pl_9500<-raster(paste0(CombRas_output, "pl_Comb9500.tif"))
pl_0005<-raster(paste0(CombRas_output, "pl_Comb0005.tif"))
pl_0510<-raster(paste0(CombRas_output, "pl_Comb0510.tif"))
pl_1015<-raster(paste0(CombRas_output, "pl_Comb1015.tif"))


# # QC: Get unique values and Plot rasters
# unique(pl_8085); unique(pl_8590); unique(pl_9095); unique(pl_9500); unique(pl_0005); unique(pl_0510); unique(pl_1015)

# [1]    0 1111 1981 1982 1983 1984 1985
# [1]    0 1111 1986 1987 1988 1989 1990
# [1]    0 1111 1991 1992 1993 1994 1995
# [1]    0 1111 1996 1997 1998 1999 2000
# [1]    0 1111 2001 2002 2003 2004 2005
# [1]    0 1111 2006 2007 2008 2009 2010
# [1]    0 1111 2011 2012 2013 2014 2015

# par(mfrow=c(1,3))
# plot(pl_8595, useRaster=FALSE, main="pl_8595"); plot(pl_9505, useRaster=FALSE, main="pl_9505"); plot(pl_0515, useRaster=FALSE, main="pl_0515")


# ----------------------------------------------
# EXTRACT PATCH ID FOR CHANGE YEARS *only* (No Persistent LU). For QC only??
# ----------------------------------------------
{

# yrlyPID_vals <- getValues(yrly_patID)
# chg_yrs <-c('pl_Comb8085', 'pl_Comb8590', 'pl_Comb9095', 'pl_Comb9500', 'pl_Comb0005', 'pl_Comb0510', 'pl_Comb1015')

# for(comb in chg_yrs){
	# print(paste0(CombRas_output,comb, ".tif", sep=""))
	
	# ras <- raster(paste0(CombRas_output,comb, ".tif", sep=""))
	# chg_vals <- getValues(ras)
	# chg_vals <- chg_vals[chg_vals>1111] # NOTE: '1111' = persistent PL
	# boolean_vals <- ifelse(yr_vals %in% chg_vals, yrlyPID_vals, NA) 
	
	# chg_yr <- setValues(ras, boolean_vals)

	# # WRITE TO FILE
	# writeRaster(chg_yr, filename=paste0(CombRas_output, comb, "patID", ".tif", sep=""), format='GTiff', overwrite=TRUE)

# }

# # READ FROM FILE
# plID_8085<-raster(paste0(CombRas_output, "pl_Comb8085patID.tif"))
# plID_8590<-raster(paste0(CombRas_output, "pl_Comb8590patID.tif"))
# plID_9095<-raster(paste0(CombRas_output, "pl_Comb9095patID.tif"))
# plID_9500<-raster(paste0(CombRas_output, "pl_Comb9500patID.tif"))
# plID_0005<-raster(paste0(CombRas_output, "pl_Comb0005patID.tif"))
# plID_0510<-raster(paste0(CombRas_output, "pl_Comb0510patID.tif"))
# plID_1015<-raster(paste0(CombRas_output, "pl_Comb1015patID.tif"))


# # QC: Plot rasters
# par(mfrow=c(1,3))

# plot(plID_8595, useRaster=FALSE, main="plID_8595"); plot(plID_9505, useRaster=FALSE, main="plID_9505"); plot(plID_0515, useRaster=FALSE, main="plID_0515")
}
}

# ----------------------------------------------
# ----------------------------------------------
# CREATE RASTER OF ADJACENT CELLS ** Make this a loop of apply at some point.
# ----------------------------------------------
# ----------------------------------------------
{
# List of rasters for each 5 year increment
inc_ras <- list(pl_8085, pl_8590, pl_9095, pl_9500, pl_0005, pl_0510, pl_1015)

# These are the changes that occur between value =0 to the year starting with 1981
ProtLandConversions <- c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)

m<-matrix(
	c(rep(NA, 3), 1, rep(NA,3), 
	rep(NA, 1),1, 1, 1, 1, 1,rep(NA,1),
	rep(NA, 1), 1, 1, 1, 1, 1, rep(NA,1),
	1, 1, 1, 0, 1, 1, 1,
	rep(NA, 1), 1, 1, 1, 1, 1, rep(NA,1),
	rep(NA, 1),1, 1, 1, 1, 1,rep(NA,1),
	rep(NA, 3), 1, rep(NA,3)
	), nrow = 7, ncol = 7, byrow = T)
		
# ----------------------------------------------
# LOOP THROUGH EACH 5 YEAR INCREMENT STARTING WITH 1980-1985, ending with 2010-2015.
# ----------------------------------------------

old<-Sys.time() 
for(increment in inc_ras){ 
	print(names(increment))
	
pl_inc_vals<-getValues(increment)

Adjacent_Cells <- adjacent(increment, cell=which(pl_inc_vals== 1111),directions = m, pairs = F, include=F, sorted=TRUE, target= which(pl_inc_vals != 1111& pl_inc_vals !=0 & pl_inc_vals %in% ProtLandConversions))

pl_inc_vals[Adjacent_Cells] <- 1 ;  pl_inc_vals[!Adjacent_Cells] <- NA #make values from lu raster (pl_inc_vals) that are adjacent =1 and those that are not adjacent =0
adj_inc_raster <- setValues(increment,pl_inc_vals) # create a new raster that sets the LU change raster values to 1 or 0 depending on if they are adjacent to persistent lu or not.

writeRaster(adj_inc_raster, filename=paste0(CombRas_output, "adj_", str_sub(names(increment), start=-4), ".tif"), format='GTiff', overwrite=TRUE)
	
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 3.564314 mins


# READ FROM FILE
adj8085_raster<-raster(paste0(CombRas_output, "adj_8085.tif"))
adj8590_raster<-raster(paste0(CombRas_output, "adj_8590.tif"))
adj9095_raster<-raster(paste0(CombRas_output, "adj_9095.tif"))
adj9500_raster<-raster(paste0(CombRas_output, "adj_9500.tif"))
adj0005_raster<-raster(paste0(CombRas_output, "adj_0005.tif"))
adj0510_raster<-raster(paste0(CombRas_output, "adj_0510.tif"))
adj1015_raster<-raster(paste0(CombRas_output, "adj_1015.tif"))

	
# # QC: Plot rasters
# par(mfrow=c(1,3))
# plot(adj8590_raster, useRaster=FALSE, main="adj8595_raster"); plot(adj0510_raster, useRaster=FALSE, main="adj0510_raster"); plot(adj1015_raster, useRaster=FALSE, main="adj1015_raster")
}


# ----------------------------------------------
# RECLASSIFY RASTERS FOR 30 YEARS IN 10 YR INCREMENTS
# ----------------------------------------------	

{
# COMPARE TO INITIAL *INCREMENT* YEAR

# Change Raster
pl_Comb8085<-raster(paste0(CombRas_output, "pl_Comb8085.tif"))
pl_Comb8590<-raster(paste0(CombRas_output, "pl_Comb8590.tif"))
pl_Comb9095<-raster(paste0(CombRas_output, "pl_Comb9095.tif"))
pl_Comb9500<-raster(paste0(CombRas_output, "pl_Comb9500.tif"))
pl_Comb0005<-raster(paste0(CombRas_output, "pl_Comb0005.tif"))
pl_Comb0510<-raster(paste0(CombRas_output, "pl_Comb0510.tif"))
pl_Comb1015<-raster(paste0(CombRas_output, "pl_Comb1015.tif"))

# Adjacent Raster
adj_8085<-raster(paste0(CombRas_output, "adj_8085.tif"))
adj_8590<-raster(paste0(CombRas_output, "adj_8590.tif"))
adj_9095<-raster(paste0(CombRas_output, "adj_9095.tif"))
adj_9500<-raster(paste0(CombRas_output, "adj_9500.tif"))
adj_0005<-raster(paste0(CombRas_output, "adj_0005.tif"))
adj_0510<-raster(paste0(CombRas_output, "adj_0510.tif"))
adj_1015<-raster(paste0(CombRas_output, "adj_1015.tif"))

# unique(pl_8595)
 # [1]    0 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
# unique(adj8595_raster)
 # [1]    0    1 1111 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995

# RECLASSIFY TO c(1981, 1986, 1991, 1996, 2001, 2006, 2011)

m1 <- c(1981, 1985, 1981)
rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
m2 <- c(1986, 1990, 1986)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
m3 <- c(1991, 1995, 1991)
rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
m4 <- c(1996, 2000, 1996)
rclmat4 <- matrix(m4, ncol=3, byrow=TRUE)
m5 <- c(2001, 2005, 2001)
rclmat5 <- matrix(m5, ncol=3, byrow=TRUE)
m6 <- c(2006, 2010, 2006)
rclmat6 <- matrix(m6, ncol=3, byrow=TRUE)
m7 <- c(2011, 2015, 2011)
rclmat7 <- matrix(m7, ncol=3, byrow=TRUE)

pl_Comb8085r<-reclassify(pl_Comb8085, rclmat1)
pl_Comb8590r<-reclassify(pl_Comb8590, rclmat2)
pl_Comb9095r<-reclassify(pl_Comb9095, rclmat3)
pl_Comb9500r<-reclassify(pl_Comb9500, rclmat4)
pl_Comb0005r<-reclassify(pl_Comb0005, rclmat5)
pl_Comb0510r<-reclassify(pl_Comb0510, rclmat6)
pl_Comb1015r<-reclassify(pl_Comb1015, rclmat7)


adj_8085r<-reclassify(adj_8085, rclmat1)
adj_8590r<-reclassify(adj_8590, rclmat2)
adj_9095r<-reclassify(adj_9095, rclmat3)
adj_9500r<-reclassify(adj_9500, rclmat4)
adj_0005r<-reclassify(adj_0005, rclmat5)
adj_0510r<-reclassify(adj_0510, rclmat6)
adj_1015r<-reclassify(adj_1015, rclmat7)


# # QC: unique vals
# unique(pl_8595); unique(pl_9505); unique(pl_0515)
# unique(adj_8085r); unique(adj_8590r) #...

# ?? Why does this happen for the first raster. are there no adjacent cells??
# [1]    0 1111 1981
# [1]    0    1 1111 1986
# [1]    0    1 1111 1991
# [1]    0    1 1111 1996
# [1]    0    1 1111 2001
# [1]    0    1 1111 2006
# [1]    0    1 1111 2011
}

# ----------------------------------------------
# ----------------------------------------------
# RUN PATCH STATS
# ----------------------------------------------
# ----------------------------------------------
{


# ----------------------------------------------
# RUN PATCH STATS FOR everything *prior* to 1981
# Note how this is for less than 1981, not <= 1981, i.e. 1980.
# ----------------------------------------------
{
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) #seeking large resolution one.

yrly_vals <- getValues(pl_year)

# Years before 1981
boolean_vals <- ifelse(yrly_vals < 1981|is.na(yrly_vals),yrly_vals,0) # Note how this is for less than 1981, not <= 1981, i.e. 1980.
Change_Masked <- setValues(pl_year,boolean_vals) 
Change_Masked[Change_Masked <= 0]<-NA
Change_Clumps <- raster::mask(yrly_patID, Change_Masked) ##1-1035
BR_pstat <- PatchStat(Change_Clumps, cellsize = 180)

Change_Masked_vals <- getValues(Change_Masked);	Change_Masked_vals <- which(Change_Masked_vals != "NA") 
yr_patches <- extract(Change_Clumps, Change_Masked_vals) #: extract the clump ID for the previously identified patches 

BRold_Stats <- subset(BR_pstat, BR_pstat[,1]%in% unique(yr_patches))
BRold_Stats$Transition <- 1800
BRold_Stats$Type <- "noMeas"
}

# ----------------------------------------------
# RUN PATCH STATS FOR  Unknowns (9999)
# ----------------------------------------------
{
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
}


# Get Values from adj_raster
# mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.

# ----------------------------------------------
# RUN PATCH STATS FOR SEVEN TIME INCREMENTS - Loop this as well.
# ----------------------------------------------

inc_yr <- c(1981, 1986, 1991, 1996, 2001, 2006, 2011)

inc_rasR <- list(pl_Comb8085r, pl_Comb8590r, pl_Comb9095r, pl_Comb9500r, pl_Comb0005r, pl_Comb0510r, pl_Comb1015r)
names(inc_rasR) <- c("pl_Comb8085r", "pl_Comb8590r", "pl_Comb9095r", "pl_Comb9500r", "pl_Comb0005r", "pl_Comb0510r", "pl_Comb1015r")

adj_rasR <- list(adj_8085r, adj_8590r, adj_9095r, adj_9500r, adj_0005r, adj_0510r, adj_1015r)
names(adj_rasR) <- c("adj_8085r", "adj_8590r", "adj_9095r", "adj_9500r", "adj_0005r", "adj_0510r", "adj_1015r")

### TEST TEST TEST ###
# inc_yrTEST <- c(1981, 2011)
# inc_rasRTEST <- list(pl_Comb8085r, pl_Comb1015r)
# names(inc_rasRTEST) <- c("pl_Comb8085r", "pl_Comb1015r")
# adj_rasRTEST <- list(adj_8085r, adj_1015r)
# names(adj_rasRTEST) <- c("adj_8085r", "adj_1015r")

old <- Sys.time()
iStats<-list()
i_s <- 1
for(increment in 1:length(inc_rasR)){ 
	print(inc_yr[increment])
	
	print(names(inc_rasR[increment]))
	ch_savals<-getValues(inc_rasR[[increment]])

	print(names(adj_rasR[increment]))

	adj_vals <- getValues(adj_rasR[[increment]]);	adj_vals <- which(adj_vals == 1) # Get Values from adj_raster, # mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.
	
	boolean_vals <- ifelse(ch_savals == inc_yr[increment]|is.na(ch_savals),ch_savals,0) # If trans values in the cty = the transition in question or if trans vals in cty mask are NA, assign the trans values for cty vals mask, other wise = 0. This gives a raster that only represents NA and transition areas.
    Change_Masked <- setValues(inc_rasR[[increment]],boolean_vals) # create a new raster that sets the county change raster values to the value of the desired transition and NA, excluding other transitions or persistent landuses. 
	temp_mask <- Change_Masked; temp_mask[temp_mask <= 0]<-NA
	Change_Clumps <- raster::mask(yrly_patID, temp_mask)

	   print(paste("masked raster created:",inc_yr[increment])) 
	   	   
		# ----------------------------------------------
		# PATCHSTATS
		try(cty_pstat <- PatchStat(Change_Clumps, cellsize = 180)) #: calculate patch statistics
		
		# ~~~~~~~~~~~~~~~~~~~~~~~~~~
		# *** RUN ONE OF THESE FOR EACH TIME INCREMENT ***
		try(adj_patches <- extract(Change_Clumps,adj_vals)) #: extract the clump ID for the previously identified adjacent cells 

		try(adj_Stats <- subset(cty_pstat, cty_pstat[,1]%in% unique(adj_patches))) #: extract the stats of clumps that contain adjacent cells
		adj_iStats <- adj_Stats
		try(new_Stats <- subset(cty_pstat, cty_pstat[,1]%nin%unique(adj_patches))) #: extract the stats of clumps that don't contain adjacent cells
		new_iStats <- new_Stats
		# ----------------------------------------------
	  
		try(adj_iStats$Transition <- inc_yr[increment])
		try(new_iStats$Transition <- inc_yr[increment]) # get an error bc in some cases there may not be patches in this transition.
		try(adj_iStats$Type <- "Expanded")
		try(new_iStats$Type <- "Separated")
		
		iStats[[i_s]] <- rbind(adj_iStats, new_iStats)
		i_s<-i_s+1
	}
	
	library(dplyr)
	iStatsInc <- bind_rows(iStats)

new<-Sys.time()-old
print(new)
# Time difference of 1.488734 mins
	
# str(iStatsInc)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	8044 obs. of  14 variables:
 # $ patchID          : int  2524 2525 2526 2527 2528 2529 2530 2531 2532 2533 ...
 # $ n.cell           : int  1 17 15 21 39 22 2 17 1 19 ...
 # $ n.core.cell      : int  0 3 0 2 8 0 0 1 0 4 ...
 # $ n.edges.perimeter: int  4 20 22 24 38 30 6 22 4 20 ...
 # $ n.edges.internal : int  0 48 38 60 118 58 2 46 0 56 ...
 # $ area             : num  32400 550800 486000 680400 1263600 ...
 # $ core.area        : num  0 97200 0 64800 259200 ...
 # $ perimeter        : num  720 3600 3960 4320 6840 5400 1080 3960 720 3600 ...
 # $ perim.area.ratio : num  0.02222 0.00654 0.00815 0.00635 0.00541 ...
 # $ shape.index      : num  1 1.11 1.38 1.2 1.46 ...
 # $ frac.dim.index   : num  1 1.03 1.05 1.04 1.06 ...
 # $ core.area.index  : num  0 0.1765 0 0.0952 0.2051 ...
 # $ Transition       : num  1981 1981 1981 1981 1981 ...
 # $ Type             : chr  "Separated" "Separated" "Separated" "Separated" ...
		
		
		# WRITE TO FILE
		write.csv(iStatsInc,paste(Output_Folder,"/iPatchStats_inc", ".csv",sep=""), row.names = F)
		
		# READ FROM FILE
		# iPatch_incOld<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/Old/iPatchStats_inc.csv")	
		
		# READ FROM FILE
		iPatch_inc<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iPatchStats_inc.csv")		

}

# ----------------------------------------------
# BIND TABLES: INCREMENT TABLE WITH PATCHES PRIOR TO 1985 AND UNKNOWNS (9999)
# ----------------------------------------------
{
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
}
		
# ----------------------------------------------
# ----------------------------------------------
# REMOVE PATCHES WITH NO CORE AREA
# ----------------------------------------------
# ----------------------------------------------
{

# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA <1 FROM *TABLE*
# ----------------------------------------------
{
core_pat<-filter(iStats_all, n.core.cell>=1) # total = 4152 new:4121
core_pat$area.ha <- core_pat$area/10000
core_pat$core.area.ha <- core_pat$core.area/10000

str(core_pat)

# WRITE TO FILE
write.csv(core_pat,paste(Output_Folder,"/iStats_corepat", ".csv",sep=""), row.names = F)

# READ FROM FILE
iStats_corepat<-read.csv("Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/iStats_corepat.csv")

# > str(iStats_corepat)
# 'data.frame':	4155 obs. of  17 variables:
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
 
}

# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA ==0 FROM *RASTER*
# ----------------------------------------------
{
core_pat<-filter(iStats_all, core.area >= 1) 
ncor <-filter(iStats_all, core.area == 0) 

# str(core_pat)
# 'data.frame':	4155 obs. of  15 variables:
 # str(ncor)
# 'data.frame':	10591 obs. of  15 variables:

# ----------------------------------------------
# REMOVE PATCHES WITH CORE AREA ==0 FROM *RASTER*: All years

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
# REMOVE PATCHES WITH CORE AREA ==0 FROM *RASTER*: Incremental years

# Need to first create new yearly rasters that have no core area pathces removed.

# READ FROM FILE
pl_80<-raster(paste0(BR_fileLoc, "pl_80.tif"))
pl_85<-raster(paste0(BR_fileLoc, "pl_85.tif"))
pl_90<-raster(paste0(BR_fileLoc, "pl_90.tif"))
pl_95<-raster(paste0(BR_fileLoc, "pl_95.tif"))
pl_00<-raster(paste0(BR_fileLoc, "pl_00.tif"))
pl_05<-raster(paste0(BR_fileLoc, "pl_05.tif"))
pl_10<-raster(paste0(BR_fileLoc, "pl_10.tif"))
pl_15<-raster(paste0(BR_fileLoc, "pl_15.tif"))

# Mask to only show patches with core area (**by Year**)
sNCpl_80<-raster::mask(pl_80, sNCyrly_patID)
sNCpl_85<-raster::mask(pl_85, sNCyrly_patID)
sNCpl_90<-raster::mask(pl_90, sNCyrly_patID)
sNCpl_95<-raster::mask(pl_95, sNCyrly_patID)
sNCpl_00<-raster::mask(pl_00, sNCyrly_patID)
sNCpl_05<-raster::mask(pl_05, sNCyrly_patID)
sNCpl_10<-raster::mask(pl_10, sNCyrly_patID)
sNCpl_15<-raster::mask(pl_15, sNCyrly_patID)

# WRITE TO FILE
writeRaster(sNCpl_80, filename=paste0(BR_fileLoc, "sNCpl_80.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_85, filename=paste0(BR_fileLoc, "sNCpl_85.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_90, filename=paste0(BR_fileLoc, "sNCpl_90.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_95, filename=paste0(BR_fileLoc, "sNCpl_95.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_00, filename=paste0(BR_fileLoc, "sNCpl_00.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_05, filename=paste0(BR_fileLoc, "sNCpl_05.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_10, filename=paste0(BR_fileLoc, "sNCpl_10.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCpl_15, filename=paste0(BR_fileLoc, "sNCpl_15.tif", sep=""), format='GTiff', overwrite=TRUE)



# Mask to only show patches with core area (**by PID**)
# First need to remove zeros so that they do not get used as mask:
sNCpl_80[sNCpl_80 == 0]<-NA
sNCpl_85[sNCpl_85 == 0]<-NA
sNCpl_90[sNCpl_90 == 0]<-NA
sNCpl_95[sNCpl_95 == 0]<-NA
sNCpl_00[sNCpl_00 == 0]<-NA
sNCpl_05[sNCpl_05 == 0]<-NA
sNCpl_10[sNCpl_10 == 0]<-NA
sNCpl_15[sNCpl_15 == 0]<-NA

sNCyrly_patID_80<-raster::mask(sNCyrly_patID, sNCpl_80)
sNCyrly_patID_85<-raster::mask(sNCyrly_patID, sNCpl_85)
sNCyrly_patID_90<-raster::mask(sNCyrly_patID, sNCpl_90)
sNCyrly_patID_95<-raster::mask(sNCyrly_patID, sNCpl_95)
sNCyrly_patID_00<-raster::mask(sNCyrly_patID, sNCpl_00)
sNCyrly_patID_05<-raster::mask(sNCyrly_patID, sNCpl_05)
sNCyrly_patID_10<-raster::mask(sNCyrly_patID, sNCpl_10)
sNCyrly_patID_15<-raster::mask(sNCyrly_patID, sNCpl_15)

# WRITE TO FILE
writeRaster(sNCyrly_patID_80, filename=paste0(BR_fileLoc, "sNCyrly_patID_80.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_85, filename=paste0(BR_fileLoc, "sNCyrly_patID_85.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_90, filename=paste0(BR_fileLoc, "sNCyrly_patID_90.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_95, filename=paste0(BR_fileLoc, "sNCyrly_patID_95.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_00, filename=paste0(BR_fileLoc, "sNCyrly_patID_00.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_05, filename=paste0(BR_fileLoc, "sNCyrly_patID_05.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_10, filename=paste0(BR_fileLoc, "sNCyrly_patID_10.tif", sep=""), format='GTiff', overwrite=TRUE)
writeRaster(sNCyrly_patID_15, filename=paste0(BR_fileLoc, "sNCyrly_patID_15.tif", sep=""), format='GTiff', overwrite=TRUE)

	

}
}
	




















