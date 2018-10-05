





























# 
maj_categ<-list()
r_c<-1
for(f in 1:length(ras_patches)){
	print(paste0("start:",names(ras_patches[f])))
	
	# Create matrices to fill in.
	  maj_categ[[r_c]] <- as.data.frame(matrix(nrow = length(u_patch), ncol = 4))
	  colnames(maj_categ[[r_c]]) <- c(paste0(str_sub(names(ras_patches[f]), start=4), "_patchID"), paste0(str_sub(names(ras_patches[f]), start=4), "_maj"), paste0(str_sub(names(ras_patches[f]), start=4), "_ha"), paste0(str_sub(names(ras_patches[f]), start=4), "_prop")) 
	
		for(p in 1:length(u_patch)){
			patchID <- u_patch[p]
			try(temp<-filter(ras_patches[[f]],ras_patches[[f]]$'zone' == u_patch[[p]]))
			try(area<-max(temp$area.ha))
			try(maj<-ifelse(area>3.24,filter(temp, area.ha == max(temp$area.ha))$Class,0))#Make sure resolution is right here.
			try(prop<-ifelse(area>3.24,100*(max(temp$area.ha)/sum(temp$area.ha)),0))#Make sure resolution is right here.
				maj_categ[[r_c]][p,1] <- patchID
				# maj_categ[[r_c]][p,2] <- str_sub(names(ras_patches[f]), start=4)
				maj_categ[[r_c]][p,2] <- maj
				maj_categ[[r_c]][p,3] <- area
				maj_categ[[r_c]][p,4] <- prop
			}
	print(paste0("finish:",names(ras_patches[f])))
	r_c <- r_c +1
}
new<-Sys.time()-old
print(new)

# NLCD secondary proportion
for(p in 1:length(u_patch)){
			patchID <- u_patch[p]
			try(temp<-filter(ras_patches[[f]],ras_patches[[f]]$'zone' == u_patch[[p]]))
			try(area<-max(temp$area.ha))
			try(maj<-ifelse(area>3.24,filter(temp, area.ha == max(temp$area.ha))$Class,0))#Make sure resolution is right here.
			try(prop<-ifelse(area>3.24,100*(max(temp$area.ha)/sum(temp$area.ha)),0))#Make sure resolution is right here.
				maj_categ[[r_c]][p,1] <- patchID
				# maj_categ[[r_c]][p,2] <- str_sub(names(ras_patches[f]), start=4)
				maj_categ[[r_c]][p,2] <- maj
				maj_categ[[r_c]][p,3] <- area
				maj_categ[[r_c]][p,4] <- prop
			}
			
			
			> head(ras_patches)
$pl_nlcd
Source: local data frame [117,080 x 5]

   zone count area.ha  Raster Class
1     1     0       0 pl_nlcd     4
2     2     0       0 pl_nlcd     4
3     3     0       0 pl_nlcd     4
4     4     0       0 pl_nlcd     4
5     5     0       0 pl_nlcd     4
6     6     0       0 pl_nlcd     4
7     7     0       0 pl_nlcd     4
8     8     0       0 pl_nlcd     4
9     9     0       0 pl_nlcd     4
10   10     0       0 pl_nlcd     4
..  ...   ...     ...     ...   ...

$pl_er
Source: local data frame [146,350 x 5]

   zone count area.ha Raster Class
1     1     0       0  pl_er     8
2     2     0       0  pl_er     8
3     3     0       0  pl_er     8
4     4     0       0  pl_er     8
5     5     0       0  pl_er     8
6     6     0       0  pl_er     8
7     7     0       0  pl_er     8
8     8     0       0  pl_er     8
9     9     0       0  pl_er     8
10   10     0       0  pl_er     8
..  ...   ...     ...    ...   ...

$pl_gap
Source: local data frame [58,540 x 5]

   zone count area.ha Raster Class
1     1     0       0 pl_gap     3
2     2     0       0 pl_gap     3
3     3     0       0 pl_gap     3
4     4     0       0 pl_gap     3
5     5     0       0 pl_gap     3
6     6     0       0 pl_gap     3
7     7     0       0 pl_gap     3
8     8     0       0 pl_gap     3
9     9     0       0 pl_gap     3
10   10     0       0 pl_gap     3
..  ...   ...     ...    ...   ...

$pl_own
Source: local data frame [58,540 x 5]

   zone count area.ha Raster Class
1     1   142  460.08 pl_own     1
2     2     5   16.20 pl_own     1
3     3     1    3.24 pl_own     1
4     4     4   12.96 pl_own     1
5     5     5   16.20 pl_own     1
6     6     1    3.24 pl_own     1
7     7     2    6.48 pl_own     1
8     8     1    3.24 pl_own     1
9     9     1    3.24 pl_own     1
10   10     1    3.24 pl_own     1
..  ...   ...     ...    ...   ...

$pl_pp
Source: local data frame [29,270 x 5]

   zone count area.ha Raster Class
1     1   142  460.08  pl_pp     1
2     2     5   16.20  pl_pp     1
3     3     1    3.24  pl_pp     1
4     4     4   12.96  pl_pp     1
5     5     5   16.20  pl_pp     1
6     6     1    3.24  pl_pp     1
7     7     2    6.48  pl_pp     1
8     8     1    3.24  pl_pp     1
9     9     1    3.24  pl_pp     1
10   10     1    3.24  pl_pp     1
..  ...   ...     ...    ...   ...

$pl_state
Source: local data frame [146,350 x 5]

   zone count area.ha   Raster Class
1     1     0       0 pl_state     1
2     2     0       0 pl_state     1
3     3     0       0 pl_state     1
4     4     0       0 pl_state     1
5     5     0       0 pl_state     1
6     6     0       0 pl_state     1
7     7     0       0 pl_state     1
8     8     0       0 pl_state     1
9     9     0       0 pl_state     1
10   10     0       0 pl_state     1
..  ...   ...     ...      ...   ...