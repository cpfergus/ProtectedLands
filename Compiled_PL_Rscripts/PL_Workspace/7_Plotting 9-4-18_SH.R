############################ 
#PURPOSE: Plotting results from Blue Ridge Patch Stats Analysis
#INPUT: Tables on patch stats for expansion and new patches "PLv2_Exp_Table_ppgap", "PLv2_New_Table_ppgap"
#OUTPUT: pdf of plots 
#DEVELOPED: 10/31/16, 11/8/16 - Iara Lacher
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
# Need to run the multiplot function. This is in the one note notes.
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 


# ----------------------------------------------
################################################

# PACKAGES NEEDED

# Plotting
library(ggplot2)
library(dplyr)
library(reshape)
library(stringr)
library(grid) #ggplot unit()

# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")

#----------------------------------------------
############################################
#Multiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




# ----------------------------------------------
# READ INPUT FILES:

# Output_Folder <- "C:/Users/LacherL/Documents/AA_Smithsonian/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" #V Drive
Output_Folder <- "U:/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/" 

iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)

#-------------------------------------------#
Pat_cnt<-read.table("U:/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/Pat_cnt.txt") #there is currently an old version 
#Split by publc/private for easier plotting
Pat_cnt1 <- filter(Pat_cnt, er_maj==1)
Pat_cnt23 <- filter(Pat_cnt, er_maj==2|er_maj==3)
#----------------------------------------------------------------#
CS_incall<-read.table(paste0(Output_Folder,"CS_incall.txt"), sep=",", header=TRUE) 
CS_incall <-filter(CS_incall, yrInt>=1985)

CS_incB <-filter(CS_incall, er=="Blue Ridge")
CS_incP <-filter(CS_incall, er=="Piedmont")

#Select only 5,6,7, nlcd class
CS_incall_8515 <-filter(CS_incall, yrInt>=1985)
#---------------------------------------------------------------#
iStats_majJoin<-read.table(paste0(Output_Folder,"iStats_majJoin",".txt"), sep=",", header=TRUE) # no core area already removed
#Filter down by categories:
majar_unk <- filter(iStats_majJoin, estYr ==9999) # unknown year
majar_notunk <-filter(iStats_majJoin, estYr<9999)# removed unknowns. left with 1800-2015 

# study area only
majar_er_unk <- filter(majar_unk, er_maj==1|er_maj==2|er_maj==3)
majar_er_unk$er_maj<-as.integer(ifelse(majar_er_unk$er_maj==1, 1, ifelse(majar_er_unk$er_maj==2|majar_er_unk$er_maj==3,23,1))) # reclassify to group piedmont into one value:
majar_er <- filter(majar_notunk, er_maj==1|er_maj==2|er_maj==3)
majar_8615 <-filter(majar_er, estYr>1985) # 1986-2015 
majar_er$er_maj<-as.integer(ifelse(majar_er$er_maj==1, 1, ifelse(majar_er$er_maj==2|majar_er$er_maj==3,23,1)))# reclassify to group piedmont into one value:

#Split by ecoregion
majar_B <- filter(majar_er, er_maj==1) # 1986-2015 
majar_P <- filter(majar_er, er_maj>1) 
cum_ArB <- group_by(majar_B) %>%
  group_by( estYr) %>%
  summarize(sum = sum(area.ha)) %>%
  mutate(running_total = cumsum(sum))

cum_ArP <- group_by(majar_P) %>%
  group_by( estYr) %>%
  summarize(sum = sum(area.ha)) %>%
  mutate(running_total = cumsum(sum))

cum_ArB_8515 <-filter(cum_ArB, estYr>1985)
cum_ArP_8515 <-filter(cum_ArP, estYr>1985)

#---------------------------------------------------------------------#
est_yr<-read.table(paste0(Output_Folder,"est_yr.txt"), sep=",", header=TRUE)

fixyr <- filter(est_yr, estYr >1985 & estYr <9999)
area <- select(majar_8615, patchID, area.ha)

acar_8615<-read.table(paste0(Output_Folder,"acar_8615",".txt"), sep=",", header=TRUE)
fix_8615ar <-full_join(acar_8615, area, by="patchID")

fix_8615ary<-full_join(fix_8615ar, fixyr)
fix_8615ary$area.ha[is.na(fix_8615ary$area.ha)] <- 0


fix_8615_1 <-filter(fix_8615ary, er_maj==1) # 1986-2015 
fix_8615_23 <-filter(fix_8615ary, er_maj>1) # 1986-2015 

cum_OWN <- group_by(fix_8615ary) %>%
  group_by(er_maj, own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
  summarize(sum = sum(own_area_ha)) %>%
  mutate(running_total = cumsum(sum))

#Remove duplicate measures based on duplicates across (er_maj, own class, est year)
cum_OWN <- cum_OWN[!duplicated(cum_OWN[,c(1,2,6)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below
cum_OWN <- na.omit(cum_OWN)

cum_GAP <- group_by(fix_8615ary) %>%
  group_by(er_maj, own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
  summarize(sum = sum(gap_area_ha)) %>%
  mutate(running_total = cumsum(sum))

#Remove duplicate measures based on duplicates across (er_maj, own class, est year)
cum_GAP <- cum_GAP[!duplicated(cum_GAP[,c(1,3,6)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below
cum_GAP <- na.omit(cum_GAP)

cum_NLCD <- group_by(fix_8615ary) %>%
  group_by(er_maj, own_Class, gap_Class, nlcd_Class, resil_Class, estYr) %>%
  summarize(sum = sum(nlcd_area_ha)) %>%
  mutate(running_total = cumsum(sum))

cum_NLCD <- na.omit(cum_NLCD)
# Do only Forest, Grasses, and Crop
cum_NLCD <-filter(cum_NLCD, nlcd_Class==5|nlcd_Class== 6|nlcd_Class== 7)

#Remove duplicate measures based on duplicates across (er_maj, nlcd class, est year)
cum_NLCD <- cum_NLCD[!duplicated(cum_NLCD[,c(1,4,6)]),] #* THIS IS SPECIFIC TO EACH MEASURE, CHANGE EACH TIME. see header below

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# LOAD COLOR PALETTES

grey_pal <- c("#1A1A1A", "#1A1A1A", "#1A1A1A", "#595959", "#999999", "#CCCCCC", "#CCCCCC", "#CCCCCC", "#CCCCCC")
grey_pal3 <- c("#999999", "#595959", "#1A1A1A")
grey_pal4 <- c("#CCCCCC", "#999999", "#595959", "#1A1A1A")
nlcd_pal5 <- c("#80a6ff", "#969696", "#9fb480", "#f7e68c", "#cc6550")
nlcd_pal7 <- c("#80a6ff", "#969696", "#969696", "#9fb480", "#f7e68c", "#cc6550", "#80a6ff")




# ----------------------------------------------
# NUMBER OF PATCHES - GROUP BY NUMBER OF PATCHES by Type *USE pl_new*
{# Levels: Expanded noMeas Separated


# NUMBER OF PATCHES BY ECOREGION
ggplot() +
	geom_point(data=Pat_cnt1, size=2, shape=19, aes(x = estYr, y = total))+
	geom_line(data=Pat_cnt1, size=1, aes(x = estYr, y = total, linetype=factor(TypeID))) +
	geom_point(data=Pat_cnt23, size=2, shape=0,  aes(x = estYr, y = total)) +
	geom_line(data=Pat_cnt23, size=0.5, aes(x = estYr, y = total, linetype=factor(TypeID))) +	
	scale_shape_manual(breaks=c(2,1), values=c(19,0), labels=c("Blue Ridge", "Piedmont"), guide = guide_legend(name ="")) +
	scale_linetype_manual(values=c("2"="solid","1"="dashed"), guide = guide_legend(),name ="", breaks=c(2,1), labels=c("Adjacent","Separate")) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	theme_bw() + 
	theme(legend.position="right", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	labs(x="", y="Number of Patches") + 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))

# CLASS STATS BY YEAR 

ggplot() +
	# geom_line(data=CS_incall_8515, size=1, aes(x = yrInt, y = patch.cohesion.index, linetype=er))  +
	geom_line(data=CS_incall_8515, size=1, aes(x = yrInt, y = aggregation.index, linetype=er))  +
	scale_linetype_manual(values=c("Blue Ridge"="solid","Piedmont"="dashed"), guide = guide_legend(),name ="Ecoregion", breaks=c("Blue Ridge","Piedmont"), labels=c("Blue Ridge","Piedmont")) +
	# scale_y_continuous(breaks=seq(85,100,1)) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	theme_bw() + 
	theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	labs(x="Year of Establishment", y="Aggregation Index") + 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))

	


}


		
# ----------------------------------------------
# AREA AND CORE AREA 
{


		
# ----------------------------------------------
# AREA BY ECOREGION - BLUE RIDGE ONLY
{


p1<-ggplot() +
	geom_line(size=1,  data=cum_ArB_8515, aes(x = estYr, y = running_total, linetype="solid")) +
	geom_line(size=1, data=CS_incB, aes(x = yrInt, y = total.core.area.ha, linetype="dashed")) + 
	# scale_linetype_manual(name="Area Metric", values=c("solid" = "solid", "dashed" = "dashed"), breaks=c("solid", "dashed"), labels=c("All area (ha)", "Core area (ha)")) +
	scale_y_continuous(limits=c(1300000,1800000),labels=function(x)x/1000) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
	theme_bw() + 
	theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	labs(x="Year of Establishment", y="Area in Thousands of ha") + 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
	
# ----------------------------------------------
# AREA BY ECOREGION - PIEDMONT ONLY

p2<-ggplot() +
	geom_line(size=1,  data=cum_ArP_8515, aes(x = estYr, y = running_total, linetype="solid")) +
	geom_line(size=1, data=CS_incP, aes(x = yrInt, y = total.core.area.ha, linetype="dashed")) + 
	# scale_linetype_manual(name="Area Metric", values=c("solid" = "solid", "dashed" = "dashed"), breaks=c("solid", "dashed"), labels=c("All area (ha)", "Core area (ha)")) +
	scale_y_continuous(limits=c(100000,1000000),labels=function(x)x/1000) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
	theme_bw() + 
	theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	# labs(x="Year of Establishment", y="Area in Thousands of ha") + 
	labs(x="Year of Establishment", y="") + 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
	
multiplot(p1,p2, cols=2)
	
}


}

	
# ----------------------------------------------
# AREA BY OWNERSHIP

OWN<-ggplot(data=cum_OWN, aes(x = estYr, y = running_total)) +
	geom_bar(stat="identity", aes(fill=factor(own_Class)), size=1, position="stack") +
	scale_fill_manual(name ="Ownership", breaks=c(1,2,3,4), values=grey_pal4, labels=c("Public", "Private", "NGO", "Other")) +
	# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
	scale_y_continuous(breaks=seq(0,700000,100000), labels=function(x)x/1000) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	theme_bw() + 
	theme(legend.position=c("none"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	# theme(legend.position=c("none"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	labs(x="Year of Establishment", y="Area (Thousands of ha)")+ 
	# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Management Status: Blue Ridge", x="Year of Establishment", y="Area (ha)")+ 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, colour = "Black", angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, colour = "Black", angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(1,.25,.25,.25),"cm")) +
	facet_grid(.~er_maj) +
	theme(strip.text.x = element_blank(), strip.background = element_blank())

setwd("U:/ProtectedLandsProject/Figures/")
png("OWN.png", width=480, height=480, units="px", res=600) #can't put units and resolution
OWN
dev.off()


ggsave(file="OWN.png", dpi=600,width=5, height=2)

# ----------------------------------------------
# MANAGEMENT - GAP STATUS
# ----------------------------------------------

# ----------------------------------------------
# MANAGEMENT - GAP STATUS

# BLUE RIDGE
GAP<-ggplot(data=cum_GAP, aes(x = estYr, y = running_total)) +
	geom_bar(stat="identity", aes(fill=factor(gap_Class)), size=1, position="stack") +
	scale_fill_manual(name="Management Status", breaks=c(1,2,3,4), values=grey_pal4, labels=c("Natural", "Primarily Natural", "Extraction permitted", "No known management")) +
	# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
	scale_y_continuous(breaks=seq(0,700000,100000), labels=function(x)x/1000) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	theme_bw() + 
	theme(legend.position=c("none"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	# theme(legend.position=c("r"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	labs(x="Year of Establishment", y="Area (Thousands of ha)")+ 
	# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Management Status: Blue Ridge", x="Year of Establishment", y="Area (ha)")+ 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, colour = "Black", angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, colour = "Black", angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(1,.25,.25,.25),"cm")) +
	facet_grid(.~er_maj) +
	theme(strip.text.x = element_blank(), strip.background = element_blank())

setwd("U:/ProtectedLandsProject/Figures/")
png("GAP.png", width=480, height=480, units="px", res=600) #can't put units and resolution
GAP
dev.off()


ggsave(file="GAP.png", dpi=600,width=5, height=2)

# ----------------------------------------------		
# NLCD LANDCOVER	
# ----------------------------------------------

NLCD<-ggplot(data=cum_NLCD, aes(x = estYr, y = running_total)) +
	geom_bar(stat="identity", aes(fill=factor(nlcd_Class)), size=1, position="stack") +
	scale_fill_manual(name="Land Cover", breaks=c(5,6,7), values=grey_pal3, labels=c("Forest", "Grasses", "Crop")) +
	# scale_fill_manual(name="Land Cover", breaks=c(1,2,3,5,6,7,8), values=nlcd_pal7, labels=c("Open water", "Open Space", "Development", "Forest", "Grasses", "Crop", "Wetlands")) +
	# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
	scale_y_continuous(breaks=seq(0,700000,100000), labels=function(x)x/1000) +
	scale_x_continuous(breaks=seq(1985,2015,5)) +
	theme_bw() + 
	theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	# theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
	theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
	labs(x="Year of Establishment", y="Area (Thousands of ha)")+ 
	# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Natural Cover: Blue Ridge", x="Year of Establishment", y="Area (Thousands of ha)")+ 
	theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
	theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
	theme(axis.text.x = element_text(size=8, colour = "Black", angle = 45, hjust = 1)) +	
	theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
	theme(axis.text.y = element_text(size=8, colour = "Black", angle = 0, hjust = 1)) +	
	theme(plot.margin=unit(c(1,.25,.25,.25),"cm")) +
	facet_grid(.~er_maj) +
	theme(strip.text.x = element_blank(), strip.background = element_blank())
setwd("U:/ProtectedLandsProject/Figures/")
NLCD
dev.off()


ggsave(file="NLCD.png", dpi=600,width=5, height=2)

# # NLCD: BUFFER
# ggplot(data=cum_ArNLCD, aes(x = estYr, y = running_total)) +
# geom_bar(stat="identity", aes(fill=factor(nlcd_Class)), size=1, position="stack") +
# scale_fill_manual(name="Land Cover", breaks=c(5,6,7), values=grey_pal3, labels=c("Forest", "Grasses", "Crop")) +
# # scale_fill_manual(name="Land Cover", breaks=c(1,2,3,4,5,6,7,8), values=nlcd_pal8, labels=c("Open water", "Open Space", "Development", "Barren", "Forest", "Grasses", "Crop", "Wetlands")) +
# # scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
# # scale_y_continuous(breaks=seq(0,150000,50000), labels=function(x)x/1000) +
# scale_x_continuous(breaks=seq(1985,2015,5)) +
# theme_bw() + 
# theme(legend.position="right", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
# # theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
# theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
# labs(x="", y="Area (Thousands of ha)")+ 
# # labs(title="Cumulative Area of Protected Lands Added after 1985\n By Natural Cover: Blue Ridge", x="Year of Establishment", y="Area (Thousands of ha)")+ 
# theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
# theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
# theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
# theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
# theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
# theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))

}


