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
# PlotOutput_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_BR/Patch_Stats/"


iStats_Join<-read.table(paste0(Output_Folder,"iStats_Join",".txt"), sep=",", header=TRUE)



 
############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# SET COLOR PALETTES
# col_pal <- c("#548B54", "#548B54", "#548B54", "#CD8500", "#8B2500", "#6CA6CD", "#6CA6CD")
grey_pal <- c("#1A1A1A", "#595959", "#999999", "#CCCCCC")
grey_pal5 <- c("#1A1A1A", "#595959", "#999999", "#CCCCCC", "#000000")

# ----------------------------------------------
# CUMULATIVE AREA
# ----------------------------------------------


# ----------------------------------------------
# NUMBER OF PATCHES - GROUP BY NUMBER OF PATCHES by Type *USE pl_new*
# Levels: Expanded noMeas Separated

p1<-ggplot(data=Pat_cnt, aes(x = estYr, y = total, linetype=factor(TypeID))) +
		geom_point(size=2) +
		geom_line(size=0.5) +
		scale_linetype_manual(values=c(2,1), guide = guide_legend(),name ="Aggregation Type", breaks=c(2,1), labels=c("Expansion", "New")) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position="top", legend.text=element_text(size=8), legend.key.size=unit(c(0.25),"cm")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="# Patches by Aggregation Type", x="Year of Establishment", y="Number Patches") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
		
		

# ----------------------------------------------
# GROUP BY ECOREGION

p2<-ggplot(data=cum_TArER, aes(x = estYr, y = running_total, size = factor(er_maj), linetype=factor(Type))) +
		geom_line() +
		scale_linetype_discrete(name="Area Metric", breaks=c("area", "core"), labels=c("All area (ha)", "Core area (ha)")) +
		scale_size_manual(values=c(1,0.5), name ="Ecoregion", breaks=c(1,23), labels=c("BlueRidge", "Piedmont"), guide = guide_legend()) +
		scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		theme_bw() + 
		theme(legend.position="top", legend.text=element_text(size=8), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area By Ecoregion", x="Year of Establishment", y="Log Area (Thousands of ha)") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
			
		

# ----------------------------------------------
# GROUP BY OWNERSHIP

p3<-   ggplot(data=cum_ArOwn, aes(x = estYr, y = running_total, linetype = factor(own_maj))) +
		geom_line(size=1) +
		# scale_linetype_discrete(name="Ownership", breaks=c(1,2,3,4), labels=c("Public", "Private", "NGO", "Unknown")) +
		scale_linetype_manual(values=c("dotdash", "solid", "longdash","dotted"), guide = guide_legend(),name ="Ownership", breaks=c(1,2,3,4), labels=c("Public", "Private", "NGO", "Other")) +
		# scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		scale_y_log10(labels=function(n){format(n/1000, scientific = FALSE)}) +
		theme_bw() + 
		theme(legend.position="top", legend.text=element_text(size=8), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area By Ownership", x="Year of Establishment", y="Log Area (Thousands of ha)") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))

# ----------------------------------------------
# GROUP BY EASEMENT STATUS
# Not that useful. Just put a comparison in the text stating how many are easements within each category.		

p4<-ggplot(data=cum_ArEA, aes(x = estYr, y = running_total, linetype = factor(type_maj))) +
		geom_line(size=1) +
		scale_linetype_manual(values=c("solid", "dashed"), guide = guide_legend(),name ="Easement Status", breaks=c(1,2), labels=c("Easement", "Not Easement")) +
		scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		theme_bw() + 
		theme(legend.position="top", legend.text=element_text(size=8), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area in Easements", x="Year of Establishment", y="Log Area (Thousands of ha)") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
		
		
# ----------------------------------------------
# MULTIPLOT
		
multiplot(p1,p2,p3,p4, cols=2)		

# ----------------------------------------------
# NEAREST NEIGHBOR
# ----------------------------------------------

	ggplot(data=pl_1800, aes(x = estYr, y = min_dist.km., linetype = factor(er_maj))) +
		geom_point() +
		geom_line(data = pred123,size=1, aes(y = fit)) +
		geom_ribbon(data = pred123, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.1) +
		scale_linetype_discrete(name="Ecoregion", breaks=c("Blue Ridge", "Piedmont"), labels=c("Blue Ridge", "Piedmont"), guide = guide_legend()) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.75,.7), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Distance to Nearest Patch", x="Year of Establishment", y="Distance in km") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
		

# ----------------------------------------------
# PROXIMITY TO DEVELOPMENT
# ----------------------------------------------

# ----------------------------------------------
# BY ECOREGION

	ggplot(data=pl_foc, aes(x = estYr, y = minDevDis, linetype = factor(er_maj))) +
		geom_point() +
		geom_line(data = pred123,size=1, aes(y = fit)) +
		geom_ribbon(data = pred123, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.1) +
		scale_linetype_discrete(name="Ecoregion", breaks=c("Piedmont", "Blue Ridge"), labels=c("Piedmont", "Blue Ridge"), guide = guide_legend()) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.2,.9), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Distance to Development", x="Year of Establishment", y="Distance in km") + 
		theme(plot.title = element_text(hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))


# ----------------------------------------------
# BY PUBLIC OR PRIVATE

	ggplot(data=pl_foc, aes(x = estYr, y = minDevDis, linetype = factor(pp_maj))) +
		geom_point() +
		geom_line(data = pred12,size=1, aes(y = fit)) +
		geom_ribbon(data = pred12, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.1) +
		scale_linetype_discrete(name="Access", breaks=c("Public", "Private"), labels=c("Public", "Private"), guide = guide_legend()) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.2,.9), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Distance to Development", x="Year of Establishment", y="Distance in km") + 
		theme(plot.title = element_text(hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))
		

# ----------------------------------------------
# BY PUBLIC OR PRIVATE * AREA.HA

	ggplot(data=pl_foc, aes(x = minDevDis, y = area.ha, linetype = factor(pp_maj))) +
		geom_point(aes(colour=factor(nlcd_maj))) +
		geom_line(data = pred12,size=1, aes(y = fit)) +
		geom_ribbon(data = pred12, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.1) +
		scale_linetype_discrete(name="Access", breaks=c("Public", "Private"), labels=c("Public", "Private"), guide = guide_legend()) +
		scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=seq(0,10,1)) +
		theme_bw() + 
		theme(legend.position=c(0.2,.9), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Distance to Development", x="Distance in km", y="Log Area (ha)") + 
		theme(plot.title = element_text(hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))
		
		
		
# ----------------------------------------------
# HABITAT REPRESENTATION
# ----------------------------------------------

# ----------------------------------------------
# GROUP BY NLCD

# col_pal <- c("#548B54", "#548B54", "#548B54", "#CD8500", "#8B2500", "#6CA6CD", "#6CA6CD")
grey_pal <- c("#1A1A1A", "#595959", "#999999", "#CCCCCC")
grey_pal5 <- c("#1A1A1A", "#595959", "#999999", "#CCCCCC", "#000000")


# BLUE RIDGE
	ggplot(data=cum_NL1, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(nlcd_maj)), size=1) +
		scale_fill_manual(name="Cover", breaks=c(5,6,7,8), values=grey_pal, labels=c("Forest", "Grasses", "Crop", "Wetlands")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.3,.7), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area of Protected Lands By Natural Cover: Blue Ridge", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, fac="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))	

# PIEDMONT
	ggplot(data=cum_NL23, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(nlcd_maj)), size=1) +
		scale_fill_manual(name="Cover", breaks=c(2,5,6,7,8), values=grey_pal5, labels=c("Open Space", "Forest", "Grasses", "Crop", "Wetlands")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.3,.7), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area of Protected Lands By Natural Cover: Piedmont", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))		

		
# ----------------------------------------------
# GROUP BY GAP

grey_pal <- c("#1A1A1A", "#595959", "#999999", "#CCCCCC")

# PIEDMONT
	ggplot(data=cum_GAP12, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(gap_maj)), size=1) +
		scale_fill_manual(name="GAP Status", breaks=c(1,2,3,4), values=grey_pal, labels=c("GAP 1", "GAP 2", "GAP 3", "GAP 4")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.3,.7), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area of Protected Lands By GAP Status: Blue Ridge", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))	


# BLUE RIDGE
	ggplot(data=cum_GAP3, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(gap_maj)), size=1) +
		scale_fill_manual(name="GAP Status", breaks=c(1,2,3,4), values=grey_pal, labels=c("GAP 1", "GAP 2", "GAP 3", "GAP 4")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.3,.7), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area of Protected Lands By GAP Status: Piedmont", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))	

		
# ----------------------------------------------
# RESILIENCE CLASS
# ----------------------------------------------

 ggplot(data=RESIL, aes(x = Class, y = sum)) +
		geom_bar(stat="identity", aes(fill=factor(er_maj)), size=1) +
		scale_fill_discrete(name="Resilience Score", breaks=c(1,2,3,4,5,6,7,8)) + # !! fix the title for this.
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=seq(1,8,1)) +
		theme_bw() + 
		theme(legend.position="right", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area of Protected Lands By Resilience Cat", x="Resilience Class", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))	


		
		
# ----------------------------------------------
# SAVE GGPLOT TO FILE
# ----------------------------------------------

ggsave("img/fig-io-practice-scale-0.3.png", p1, scale = 0.6)



