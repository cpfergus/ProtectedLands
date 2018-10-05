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




# ----------------------------------------------
# READ INPUT FILES:

# Output_Folder <- "C:/Users/LacherL/Documents/AA_Smithsonian/ProtectedLandsProject/PatchesTransitions_BR/PatchStats/" #V Drive
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"

iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)


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
	labs(x="Year of Establishment", y="Area in Thousands of ha") + 
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

ggplot(data=cum_OWN, aes(x = estYr, y = running_total)) +
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


# ----------------------------------------------
# MANAGEMENT - GAP STATUS
# ----------------------------------------------

# ----------------------------------------------
# MANAGEMENT - GAP STATUS

# BLUE RIDGE
ggplot(data=cum_GAP, aes(x = estYr, y = running_total)) +
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


# ----------------------------------------------		
# NLCD LANDCOVER	
# ----------------------------------------------

ggplot(data=cum_NLCD, aes(x = estYr, y = running_total)) +
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


