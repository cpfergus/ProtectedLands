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

grey_pal <- c("#1A1A1A", "#1A1A1A", "#1A1A1A", "#595959", "#999999", "#CCCCCC", "#CCCCCC", "#CCCCCC", "#CCCCCC")

# ----------------------------------------------
# NUMBER OF PATCHES - GROUP BY NUMBER OF PATCHES by Type *USE pl_new*
{# Levels: Expanded noMeas Separated

	
p1<- ggplot() +
		geom_point(data=Pat_cnt1, size=2, shape=17,  aes(x = estYr, y = total))+
		geom_line(data=Pat_cnt1, size=1, aes(x = estYr, y = total, linetype=factor(TypeID))) +
		geom_point(data=Pat_cnt23, size=2, shape=1,  aes(x = estYr, y = total)) +
		geom_line(data=Pat_cnt23, size=0.5, aes(x = estYr, y = total, linetype=factor(TypeID))) +	
		scale_linetype_manual(values=c(2,1), guide = guide_legend(),name ="Aggregation Type", breaks=c(2,1), labels=c("Expansion", "New")) +
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
			
p3<- ggplot(data=CS_inc123, aes(x = yrInt, y = patch.cohesion.index, linetype=er)) +
		geom_point(size=2) +
		geom_line(size=0.5) +
		scale_linetype_manual(values=c("Blue Ridge"="solid","Piedmont"="dashed"), guide = guide_legend(),name ="Ecoregion", breaks=c("Blue Ridge","Piedmont"), labels=c("Blue Ridge","Piedmont")) +
		# scale_y_continuous(breaks=seq(85,100,1)) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position="right", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="Year of Establishment", y="Patch Cohesion Index") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))
			

multiplot(p1,p3, cols=1)	
}


			
# ----------------------------------------------
# AREA BY ECOREGION - BLUE RIDGE ONLY
{
# linetype outside of aes:
p1<- ggplot() +
		geom_line(size=1,  data=cum_Ar1, linetype="solid", aes(x = estYr, y = running_total)) +
		geom_line(size=1, data=CS_incB, linetype="dashed", aes(x = yrInt, y = total.core.area.ha)) + 
		scale_linetype_manual(name="Area Metric", values=c("solid" = "solid", "dashed" = "dashed"), breaks=c("solid", "dashed"), labels=c("All area (ha)", "Core area (ha)")) +
		scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		theme_bw() + 
		theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="", y="Area in Thousands of ha") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
		
		
# ----------------------------------------------
# AREA BY ECOREGION - PIEDMONT ONLY

p2<- ggplot() +
		geom_line(size=1,  data=cum_Ar23, aes(x = estYr, y = running_total, linetype="solid")) +
		geom_line(size=1, data=CS_incP, aes(x = yrInt, y = total.core.area.ha, linetype="dashed")) + 
		scale_linetype_manual(name="Area Metric", values=c("solid" = "solid", "dashed" = "dashed"), breaks=c("solid", "dashed"), labels=c("All area (ha)", "Core area (ha)")) +
		scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		theme_bw() + 
		# use to get legend alone:
		# theme(legend.position="right", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="", y="") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
}

		
# ----------------------------------------------
# AREA BY OWNERSHIP - BLUE RIDGE ONLY
{		
p3<- ggplot(data=cum_ArOwn1, aes(x = estYr, y = running_total, linetype = factor(own_maj))) +
		geom_line(size=1) +
		# scale_linetype_discrete(name="Ownership", breaks=c(1,2,3,4), labels=c("Public", "Private", "NGO", "Unknown")) +
		scale_linetype_manual(values=c("dotdash", "solid", "longdash","dotted"), guide = guide_legend(),name ="Ownership", breaks=c(1,2,3,4), labels=c("Public", "Private", "NGO", "Other")) +
		scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		# scale_y_log10(labels=function(n){format(n/1000, scientific = FALSE)}) +
		theme_bw() + 
		theme(legend.position="none", legend.text=element_text(size=8), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs( x="Year of Establishment: Blue Ridge", y="Area (Thousands of ha)") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
# ----------------------------------------------
# AREA BY OWNERSHIP - PIEDMONT ONLY
		
p4<- ggplot(data=cum_ArOwn23, aes(x = estYr, y = running_total, linetype = factor(own_maj))) +
		geom_line(size=1) +
		# scale_linetype_discrete(name="Ownership", breaks=c(1,2,3,4), labels=c("Public", "Private", "NGO", "Unknown")) +
		scale_linetype_manual(values=c("dotdash", "solid", "longdash","dotted"), guide = guide_legend(),name ="Ownership", breaks=c(1,2,3,4), labels=c("Public", "Private", "NGO", "Other")) +
		scale_y_continuous(labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		# scale_y_log10(labels=function(n){format(n/1000, scientific = FALSE)}) +
		theme_bw() + 
		# use to get legend alone:
		# theme(legend.position="right", legend.text=element_text(size=8), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(legend.position="none", legend.text=element_text(size=8), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs( x="Year of Establishment: Piedmont", y="") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
		
multiplot(p1,p3,p2,p4, cols=2)	

}



# ----------------------------------------------
# NEAREST NEIGHBOR
# ----------------------------------------------
{# * using different data for this one because had to do a little cleanup for the model. 
	
	tiff('test.tiff', units="in", width=20, height=20, res=300)

	ggplot(data=pl_new.na, aes(x = estYr, y = min_dist.km)) +
		geom_point(data=dist_n1, size=2,  shape=17, colour="blue") +
		geom_point(data=dist_n23, size=1.5,  shape=1, colour="red") +
		geom_line(data = pred1, size=1, linetype = "solid", colour="blue",aes(y = fit)) +
		geom_line(data = pred23, size=1, linetype = "dashed", colour="red",aes(y = fit)) +
		# geom_ribbon(data = pred123, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.1) +
		scale_linetype_discrete(name="Ecoregion", breaks=c("1", "23"), labels=c("Blue Ridge", "Piedmont")) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c("top"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Distance to Nearest Patch", x="Year of Establishment", y="Distance in km") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))
	# dev.off()	
	ggsave("test.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
	
}


		
# ----------------------------------------------
# DISTANCE TO DEVELOPMENT
# ----------------------------------------------
{
	ggplot(data=pl_new, aes(x = estYr, y = minDevDis, linetype = factor(er_maj))) +
		geom_point() +
		geom_line(data = pred123,size=1, aes(y = fit)) +
		geom_ribbon(data = pred123, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.1) +
		scale_linetype_discrete(name="Ecoregion", breaks=c("Blue Ridge", "Piedmont"), labels=c("Blue Ridge", "Piedmont"), guide = guide_legend()) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c(0.75,.7), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Distance to Development", x="Year of Establishment", y="Distance in km") + 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))
}
	

	
# ----------------------------------------------		
# NLCD NOT BY MAJORITY CLASS		
# ----------------------------------------------
{
grey_pal3 <- c("#999999", "#595959", "#1A1A1A")

# BLUE RIDGE
p1 <-ggplot(data=cum_NL1, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(Class)), size=1, position="stack") +
		scale_fill_manual(name="Land Cover", breaks=c(5,6,7), values=grey_pal3, labels=c("Forest", "Grasses", "Crop")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_y_continuous(breaks=seq(0,50000,10000), labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position="right", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		# theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="", y="Area (Thousands of ha)")+ 
		# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Natural Cover: Blue Ridge", x="Year of Establishment", y="Area (Thousands of ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))

# PIEDMONT
p2 <- ggplot(data=cum_NL23, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(Class)), size=1, position="stack") +
		scale_fill_manual(name="Cover", breaks=c(5,6,7), values=grey_pal3, labels=c("Forest", "Grasses", "Crop")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_y_continuous(breaks=seq(0,50000,10000), labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position="none", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="", y="")+ 
		# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Management Status: Piedmont", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))	

multiplot(p1,p2, cols=2)	
}



# ----------------------------------------------
# MANAGEMENT - GAP STATUS
# ----------------------------------------------
{

grey_pal4 <- c("#CCCCCC", "#999999", "#595959", "#1A1A1A")
	
# BLUE RIDGE
p1 <-ggplot(data=cum_GAP1, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(Class)), size=1, position="stack") +
		scale_fill_manual(name="Management Status", breaks=c(1,2,3,4), values=grey_pal4, labels=c("Natural", "Primarily Natural", "Extraction permitted", "No known management")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_y_continuous(breaks=seq(0,150000,50000), labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c("right"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		# theme(legend.position=c("none"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="Year of Establishment", y="Area (Thousands of ha)")+ 
		# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Management Status: Blue Ridge", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))

# PIEDMONT
p2 <- ggplot(data=cum_GAP23, aes(x = estYr, y = running_total)) +
		geom_bar(stat="identity", aes(fill=factor(Class)), size=1, position="stack") +
		scale_fill_manual(name="Management Status", breaks=c(1,2,3,4), values=grey_pal4, labels=c("Natural", "Primarily Natural", "Extraction permitted", "No known management")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_y_continuous(breaks=seq(0,150000,50000), labels=function(x)x/1000) +
		scale_x_continuous(breaks=seq(1985,2015,5)) +
		theme_bw() + 
		theme(legend.position=c("none"), legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(x="Year of Establishment", y="")+ 
		# labs(title="Cumulative Area of Protected Lands Added after 1985\n By Natural Cover: Piedmont", x="Year of Establishment", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5, face="bold")) +
		theme(axis.title.x = element_text(size=10, colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1)) +	
		theme(axis.title.y = element_text(size=10, colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(1,.25,.25,.25),"cm"))

multiplot(p1,p2, cols=2)	

}


	
# ----------------------------------------------
# RESILIENCE CLASS
# ----------------------------------------------
{
grey_pal2 <- c("#1A1A1A", "#999999")

 ggplot(data=RESIL, aes(x = Class, y = sum)) +
		geom_bar(stat="identity", aes(fill=factor(er_maj)), size=1, position="dodge") +
		scale_fill_manual(name="Ecoregion", values=grey_pal2, labels=c("Blue Ridge", "Piedmont")) +
		# scale_y_log10(labels=function(n){format(n, scientific = FALSE)}) +
		scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8),            labels=c("Non-Res", "Res: Not Prior", "Res: Diversity", "Res: Conc flow/ Riparian", "Res: Diversity & Conc flow/ Riparian", "Res: Diffuse flow", "Linkage: Resilient", "Linkage: Vulnerable")) +
		theme_bw() + 
		theme(legend.position="top", legend.key.height=unit(c(0.25),"cm"), legend.key.width=unit(c(1),"cm"), legend.key=element_rect(color="white")) +
		theme(axis.line = element_line(colour = "black"), panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.border =element_blank(), panel.background = element_blank()) +
		labs(title="Cumulative Area of Protected Lands By Resilience Cat", x="Resilience Class", y="Area (ha)")+ 
		theme(plot.title = element_text(size=12, hjust = 0.5, vjust=1.5)) +
		theme(axis.title.x = element_text(size=12, face="bold",colour = "Black",vjust=-.15,hjust=0.5)) +
		theme(axis.text.x = element_text(size=8, angle = 20, hjust = 1)) +	
		theme(axis.title.y = element_text(size=12, face="bold",colour = "Black",vjust=1.25,hjust=0.5)) +
		theme(axis.text.y = element_text(size=8, angle = 0, hjust = 1)) +	
		theme(plot.margin=unit(c(.25,.25,.25,.25),"cm"))	
		
			
			
			scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"),
                      labels=c("Control", "Treat 1", "Treat 2"))
}		