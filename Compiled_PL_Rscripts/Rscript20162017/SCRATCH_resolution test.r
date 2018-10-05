

library(raster)
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

BR_fileLoc<-"Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/"

# ----------------------------------------------
# ----------------------------------------------
# DECREASE RESOLUTION FOR ORIGINAL RASTERS FROM 30M TO 360M
# ----------------------------------------------
# ----------------------------------------------

# Already done:

# Original resolution 30*30
pl <- 	raster(paste0(BR_fileLoc, "pl.img" ))
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.img"   ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.img"   ))

# Resolution at 360,  using max
pl <- 	raster(paste0(BR_fileLoc, "plL.img" ))
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_erL.img"   ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_ppL.img"   ))

# Decrease resolution to 360,  using modal
plm <- aggregate(pl, fact=12, fun=modal)
pl_erm <- aggregate(pl_er, fact=12, fun=modal)
pl_ppm <- aggregate(pl_pp, fact=12, fun=modal)

# Decrease resolution to 180,  using modal
plsm <- aggregate(pl, fact=6, fun=modal)
pl_ersm <- aggregate(pl_er, fact=6, fun=modal)
pl_ppsm <- aggregate(pl_pp, fact=6, fun=modal)








BR_files<-list.files(path = "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/BlueRidge/", pattern = ".img$")
BR_filename<-lapply(BR_files, str_sub, start=1, end=-5) #remove file extension.

 # [1] "pl.img"        "pl_biopri.img" "pl_er.img"     "pl_gap.img"    "pl_nlcd.img"   "pl_own.img"   
 # [7] "pl_pp.img"     "pl_resil.img"  "pl_state.img"  "pl_type.img"   "pl_year.img" 
 
 BR_files <- BR_files[-c(1:3,7)]
 BR_filename <- BR_filename[-c(1:3,7)]


for(f in 1:length(BR_files)){
	print(paste0(BR_fileLoc, BR_filename[f]))
	
	BR_rasters <- raster(paste0(BR_fileLoc, BR_files[f]))
		
	# Decrease resolution to 180
	BR_rasters <- aggregate(BR_rasters, fact=6, fun=modal)

	# WRITE TO FILE
	writeRaster(BR_rasters,filename= paste0(BR_fileLoc, BR_filename[f], "L.tif"), format='GTiff', overwrite=TRUE)
	
	}
	
	
	
	
	
pl <- 	raster(paste0(BR_fileLoc, "plL.tif" )) 
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_erL.tif"   ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_ppL.tif"   ))

> pl_total
  zone  count
1    1 414609
2    2  78353
3    3 365044
> 
> pl_total
  zone    sum
1    1  45678
2    2  22540
3    3 168045

pl <- 	raster(paste0(BR_fileLoc, "pl.img" )) 
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.img"   ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.img"   )) #should match pl_sum bc is ignoring nas.


pl_sum <- as.data.frame(zonal(pl, pl_er, fun='sum', na.rm=TRUE))
pl_count <- as.data.frame(zonal(pl, pl_er, fun='count', na.rm=TRUE))

> pl_sum
  zone      sum
1    1  4212872
2    2  1812523
3    3 20071513
> pl_count
  zone    count
1    1 59753804
2    2 11256015
3    3 51686902


# pl2_sum <- as.data.frame(zonal(pl_pp, pl_er, fun='sum', na.rm=TRUE))
pl2_count <- as.data.frame(zonal(pl_pp, pl_er, fun='count', na.rm=TRUE)) #should match pl_sum bc is ignoring nas.

> pl2_count
  zone    count
1    1  4212872
2    2  1812523
3    3 20071513



# area for zone 1  3791584800 m2
# ha


### Is this because of the difference in resolution??

 # er totArea plArea     totArm2      PLArm2 totAr.ha   PLAr.ha
# 1  1  414609  45678 53733326400  5919868800  5373333  591986.9
# 2  2   78353  22540 10154548800  2921184000  1015455  292118.4
# 3  3  365044 168045 47309702400 21778632000  4730970 2177863.2

# > Tot_Area_PL
  # er tot_cnt pl_cnt     totArm2      PLArm2 totAr.ha   PLAr.ha
# 1  1  414609  45678 53733326400  5919868800  5373333  591986.9
# 2  2   78353  22540 10154548800  2921184000  1015455  292118.4
# 3  3  365044 168045 47309702400 21778632000  4730970 2177863.2

> Tot_Area_PL
  er  tot_cnt   pl_cnt     totArm2      PLArm2
1  1 59753804  4212872 53778423600  3791584800
2  2 11256015  1812523 10130413500  1631270700
3  3 51686902 20071513 46518211800 18064361700

> Tot_Area_PL 
  er tot_cnt pl_cnt     totArm2      PLArm2
1  1  414609  45678 53733326400  5919868800
2  2   78353  22540 10154548800  2921184000
3  3  365044 168045 47309702400 21778632000

VALUE	COUNT	AREA	MIN	MAX	RANGE	MEAN	STD	SUM	VARIETY	MAJORITY	MINORITY	MEDIAN
1	59753804	53778423600	0	1	1	0.070503829	0.255994218	4212872		2	0	1	0
2	11256015	10130413500	0	1	1	0.16102706	0.367555908	1812523		2	0	1	0
3	51686902	46518211800	0	1	1	0.388328807	0.487370029	20071513	2	0	1	0
												
VALUE	COUNT	AREA	MIN	MAX	RANGE	MEAN	STD	SUM	VARIETY	MAJORITY	MINORITY	MEDIAN
1	4212872		3791584800	1	2	1	1.22210098	0.415658676	5148555		2	1	2	1
2	1812523		1631270700	1	2	1	1.813207336	0.389745	3286480		2	2	1	2
3	20071513	18064361700	1	2	1	1.054715307	0.227423707	21169732	2	1	2	1




> 414609/45678
[1] 9.076777
> 78353/22540
[1] 3.476176
> 365044/168045
[1] 2.172299
> 
> 59753804/4212872
[1] 14.18363
> 11256015/1812523
[1] 6.210136
> 51686902/20071513
[1] 2.575137

> 53733326400-53778423600
[1] -45097200
> 10154548800-10130413500
[1] 24135300
> 47309702400-46518211800
[1] 791490600




# TRY CHANGING FILE SIZE AND RUN IT. ??


pl <- 	raster(paste0(BR_fileLoc, "pl.img" ))
pl_er 	<- 	raster(paste0(BR_fileLoc, "pl_er.img"   ))
pl_pp   <-	raster(paste0(BR_fileLoc, "pl_pp.img"   ))





# Decrease resolution to 360, but using modal
plm <- aggregate(pl, fact=12, fun=modal)
pl_erm <- aggregate(pl_er, fact=12, fun=modal)
pl_ppm <- aggregate(pl_pp, fact=12, fun=modal)

# WRITE TO FILE
writeRaster(plm,	filename= paste0(BR_fileLoc, "plm.tif"), format='GTiff', overwrite=TRUE)
writeRaster(pl_erm,	filename= paste0(BR_fileLoc, "pl_erm.tif"), format='GTiff', overwrite=TRUE)
writeRaster(pl_ppm,	filename= paste0(BR_fileLoc, "pl_ppm.tif"), format='GTiff', overwrite=TRUE)


	
# Decrease resolution to 180 using modal as function
plsm <- aggregate(pl, fact=6, fun=modal)
pl_ersm <- aggregate(pl_er, fact=6, fun=modal)
pl_ppsm <- aggregate(pl_pp, fact=6, fun=modal)

# WRITE TO FILE
writeRaster(plsm,	filename= paste0(BR_fileLoc, "plL.tif"), format='GTiff', overwrite=TRUE)
writeRaster(pl_ersm,filename= paste0(BR_fileLoc, "pl_erL.tif"), format='GTiff', overwrite=TRUE)
writeRaster(pl_ppsm,filename= paste0(BR_fileLoc, "pl_ppL.tif"), format='GTiff', overwrite=TRUE)



# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------

plm		<- 	raster(paste0(BR_fileLoc, "plm.tif"))
pl_erm	<- 	raster(paste0(BR_fileLoc, "pl_erm.tif"  ))
pl_ppm  <-	raster(paste0(BR_fileLoc, "pl_ppm.tif"  ))

plsm		<- 	raster(paste0(BR_fileLoc, "plsm.tif"))
pl_ersm	<- 	raster(paste0(BR_fileLoc, "pl_ersm.tif"  ))
pl_ppsm  <-	raster(paste0(BR_fileLoc, "pl_ppsm.tif"  ))

# res(plm);res(pl_erm);res(pl_ppm);res(plsm);res(pl_ersm);res(pl_ppsm)

#PLSM # Decrease resolution to 360, but using modal
tot_cnt <- as.data.frame(zonal(plm, pl_erm, fun='count', na.rm=TRUE))# ALL LAND
pl_cnt <- as.data.frame(zonal(pl_ppm, pl_erm, fun='count', na.rm=TRUE))# PL ONLY

Tot_Area_PL <- full_join(tot_cnt, pl_cnt, by="zone")
colnames(Tot_Area_PL) <- c("er", "tot_cnt", "pl_cnt")

Tot_Area_PL$totArm2<-Tot_Area_PL$tot_cnt*129600#Makes sure resolution is right here. 30*30=900, 360*360=129600
Tot_Area_PL$PLArm2<-Tot_Area_PL$pl_cnt*129600#Makes sure resolution is right here. 30*30=900, 360*360=129600

> Tot_Area_PL
  er tot_cnt pl_cnt     totArm2      PLArm2
1  1  417756  46332 54141177600  6004627200
2  2   78913  22678 10227124800  2939068800
3  3  361337 167253 46829275200 21675988800



#PLSM # Decrease resolution to 180 using modal as function (4 times smaller/larger)
tot_cnt <- as.data.frame(zonal(plsm, pl_ersm, fun='count', na.rm=TRUE))# ALL LAND
pl_cnt <- as.data.frame(zonal(pl_ppsm, pl_ersm, fun='count', na.rm=TRUE))# PL ONLY

Tot_Area_PL <- full_join(tot_cnt, pl_cnt, by="zone")
colnames(Tot_Area_PL) <- c("er", "tot_cnt", "pl_cnt")

Tot_Area_PL$totArm2<-Tot_Area_PL$tot_cnt*32400#Makes sure resolution is right here. 30*30=900, 360*360=129600
Tot_Area_PL$PLArm2<-Tot_Area_PL$pl_cnt*32400#Makes sure resolution is right here. 30*30=900, 360*360=129600

> Tot_Area_PL
  er tot_cnt pl_cnt     totArm2      PLArm2
1  1 1664942 149760 53944120800  4852224000
2  2  314028  69679 10174507200  2257599600
3  3 1440121 613390 46659920400 19873836000


# ArcGIS 30*30 on pl
VALUE	COUNT	AREA	MIN	MAX	RANGE	MEAN	STD	SUM	VARIETY	MAJORITY	MINORITY	MEDIAN
1	59753804	53778423600	0	1	1	0.070503829	0.255994218	4212872		2	0	1	0
2	11256015	10130413500	0	1	1	0.16102706	0.367555908	1812523		2	0	1	0
3	51686902	46518211800	0	1	1	0.388328807	0.487370029	20071513	2	0	1	0
												
VALUE	COUNT	AREA	MIN	MAX	RANGE	MEAN	STD	SUM	VARIETY	MAJORITY	MINORITY	MEDIAN
1	4212872		3791584800	1	2	1	1.22210098	0.415658676	5148555		2	1	2	1
2	1812523		1631270700	1	2	1	1.813207336	0.389745	3286480		2	2	1	2
3	20071513	18064361700	1	2	1	1.054715307	0.227423707	21169732	2	1	2	1


# 30*30 funct max, 'count'
> Tot_Area_PL
  er  tot_cnt   pl_cnt     totArm2      PLArm2
1  1 59753804  4212872 53778423600  3791584800
2  2 11256015  1812523 10130413500  1631270700
3  3 51686902 20071513 46518211800 18064361700

# same values as arc version.


 # 360 funct max, count
> Tot_Area_PL 
  er tot_cnt pl_cnt     totArm2      PLArm2
1  1  414609  45678 53733326400  5919868800
2  2   78353  22540 10154548800  2921184000
3  3  365044 168045 47309702400 21778632000

 # 360 funct modal, count
> Tot_Area_PL
  er tot_cnt pl_cnt     totArm2      PLArm2
1  1  417756  46332 54141177600  6004627200
2  2   78913  22678 10227124800  2939068800
3  3  361337 167253 46829275200 21675988800

#above 2 same area, different distribution among ers.



 # 380 funct modal, count
> Tot_Area_PL
  er tot_cnt pl_cnt     totArm2      PLArm2
1  1 1664942 149760 53944120800  4852224000
2  2  314028  69679 10174507200  2257599600
3  3 1440121 613390 46659920400 19873836000



