############################ 
#PURPOSE: Create a raster of individual patches that are defined by the year they were established.
#INPUT: Protected lands by year
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:
	# Entire script takes about 2 hours
	# Parallelization rasters loaded in script below. Do they cause error with the extent function???

#IMPORTANT: 
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 

# ----------------------------------------------
################################################


# PACKAGES NEEDED

# Rasters
library(raster)
#Parallelization rasters loaded in script below. Do they cause error with the extent function???

# SET TEMP DIRECTORY
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE LOCATIONS: 

BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# ----------------------------------------------
# READ OUTPUT FILES:

# file name:  / R label: 

# ----------------------------------------------
# READ INPUT FILES:
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) 

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# RASTER FOR PROTECTED LANDS BY YEAR * IND PATCHES
# ----------------------------------------------
# ----------------------------------------------


yr_na1 <- setValues(pl_year, NA) # * see if can remove this from inside loop

u_yr<-unique(pl_year)


# 1800 1819 1866 1870 1876 1890 1894 1895 1898 
# 1902 1903 1904 1905 1907 1909 1912 1915 1916 1917 1918 1920 1923 1925 1926 1927 1928 1929 1930 
# 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 
# 1941 1942 1944 1945 1946 1947 1948 1950 1951 1952 1953 1954 1955 1956 1958 1959 1960
# 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 
# 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 
# 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 
# 9999
#
# > length(u_yr)
# [1] 111

u_yr1 <- u_yr[1:10]
u_yr2 <- u_yr[11:20]
u_yr3 <- u_yr[21:30]
u_yr4 <- u_yr[31:40]
u_yr5 <- u_yr[41:50]
u_yr6 <- u_yr[51:60]
u_yr7 <- u_yr[61:70]
u_yr8 <- u_yr[71:80]
u_yr9 <- u_yr[81:90]
u_yr10 <- u_yr[91:100]
u_yr11 <- u_yr[101:111]


decades<-list(u_yr1,u_yr2,u_yr3,u_yr4,u_yr5,u_yr6,u_yr7,u_yr8,u_yr9,u_yr10, u_yr11)


# Test with parralell

library(foreach); library(doSNOW)
# library(raster);require (SDMTools);require (Hmisc)

for(dec in 1:length(decades)){
	print(decades[[dec]])
		
	cl <- makeCluster(length(decades[[dec]])) 
	registerDoSNOW(cl)  

	old<-Sys.time() 	
	# for (i in 1:length(decades[[dec]])) {
	foreach (i =  1:length(decades[[dec]]), .packages = c('raster','SDMTools','Hmisc','rgdal', 'igraph')) %dopar% {
		 rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/") 

		# create & fill in class raster
		# yr_na <- setValues(pl_year, NA) # * see if can remove this from inside loop
			yr_na <- yr_na1
			yr_na[pl_year == decades[[dec]][i]] <- 1

		# clump class raster
		clpT <- clump(yr_na, gap=FALSE)
		  
		writeRaster(clpT,filename= paste0("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/IndPatches/","clpyr_patID_", decades[[dec]][i], ".tif", sep=""), format='GTiff', overwrite=TRUE)
		
		removeTmpFiles()
  	} 
	
new<-Sys.time()-old
print(new)

stopCluster(cl)
removeTmpFiles()
		
}
# [1] 1800 1819 1866 1870 1876 1890 1894 1895 1898 1902
# Time difference of 29.45292 secs
 # [1] 1903 1904 1905 1907 1909 1912 1915 1916 1917 1918
# Time difference of 29.6899 secs
 # [1] 1920 1922 1923 1925 1926 1927 1928 1929 1930 1931
# Time difference of 30.47016 secs
 # [1] 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941
# Time difference of 29.83533 secs
 # [1] 1942 1944 1945 1946 1947 1948 1950 1951 1952 1953
# Time difference of 30.29196 secs
 # [1] 1954 1956 1958 1959 1960 1961 1962 1963 1964 1965
# Time difference of 30.61238 secs
 # [1] 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975
# Time difference of 31.54101 secs
 # [1] 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985
# Time difference of 31.28998 secs
 # [1] 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
# Time difference of 31.21718 secs
 # [1] 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005
# Time difference of 31.17259 secs
 # [1] 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 9999
# Time difference of 34.06172 secs
# Total time: approx. 6 minutes

# ----------------------------------------------
# ----------------------------------------------
# PATCH ID FOR ALL YEARS
# ----------------------------------------------
# ----------------------------------------------

yrly_vals<-getValues(pl_year)
u_yr<-unique(pl_year)

# ----------------------------------------------
# Protected Lands by YEAR 
pl_year <- 	raster(paste0(BR_fileLoc, "pl_year.tif" )) 

brPL_files<-list.files(path = paste0(BR_fileLoc, "IndPatches/"), pattern = ".tif$") # This must be cleaned up. right now, there are files that should not be there.


brPL_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/IndPatches/"

	
	brPLiyr<-list()
	i_y<-1
	for(f in 1:length(brPL_files)){
	
	print(paste0(brPL_fileLoc, brPL_files[f]))
	brPLiyr[[i_y]] <- raster(paste0(brPL_fileLoc, brPL_files[[f]]))
	i_y<-i_y+1
	
	}
	names(brPLiyr)<-gsub(".tif", "", brPL_files)
	# For some reason it collected the NAs. Remove them. 
	# brPLiyr <- brPLiyr[-96]
	
	# QC: Examine them. 
	lapply(brPLiyr,unique)
	
# RECLASSIFY RASTER VALUES BASED ON YEAR, LENGTH, ETC.

# First create new matrices to use in reclassification

# Create original/ first matrix
mdatA <- list(as.data.frame(matrix(c(unique(brPLiyr[[1]]), c(unique(brPLiyr[[1]]))), nrow = length(unique(brPLiyr[[1]])), ncol = 2, byrow = FALSE)))

old<-Sys.time() # Time process

	mdatB<-list()
	i_y<-1
	for(f in 1:length(brPLiyr)){
	# for(m in 1:length(brPLiyr)){
	try(print(names(brPLiyr[[f+1]])))
	try(print(names(brPLiyr[[f]])))
	# print(names(test2[m]))
	# print("end")

	# try(mm <- max(unique(brPLiyr[[f]])))
	try(mdatB[[i_y]] <- as.data.frame(matrix(c(unique(brPLiyr[[f+1]]), c(unique(brPLiyr[[f+1]])+max(unique(brPLiyr[[f]])))), nrow = length(unique(brPLiyr[[f+1]])), ncol = 2, byrow = FALSE)))
	
	try(mdatB[[i_y]] <- as.data.frame(matrix(c(unique(brPLiyr[[f+1]]), c(unique(brPLiyr[[f+1]])+max(unique(mdatB[[f-1]])))), nrow = length(unique(brPLiyr[[f+1]])), ncol = 2, byrow = FALSE)))
	
	print(length(mdatB))
	
	i_y<-i_y+1
	
	}
new<-Sys.time()-old
print(new)
# Time difference of 36.36711 mins
	
	# Join lists together
	mdat <- c(mdatA, mdatB)
	names(mdat)<-names(brPLiyr)

# Reclassify all rasters
brPLiyrR <- mapply(reclassify, brPLiyr, mdat)

brPLiyrRS <- stack(brPLiyrR)

brPLiyrRSM <- calc(brPLiyrRS, fun=max, na.rm=TRUE)
# brPLiyrRSM[brPLiyrRSM == -Inf] <- NA

yrly_patID <- brPLiyrRSM

# WRITE TO FILE
writeRaster(yrly_patID, filename=paste0(BR_fileLoc, "yrly_patID.tif", sep=""), format='GTiff', overwrite=TRUE) #looks like it writes as a different raster type, but I don't think it does.
	
# READ FROM FILE
yrly_patID<- raster(paste0(BR_fileLoc, "yrly_patID.tif", sep=""))


# Projection as of 4-27-17
# > projection(yrly_patID)
# [1] "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


# OLLLLLDDDD Projection:
# > projection(yrly_patID)
# [1] "+proj=tpeqd +lat_1=0 +lon_1=0 +lat_2=60 +lon_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
#tpeqd=Two Point Equidistant

