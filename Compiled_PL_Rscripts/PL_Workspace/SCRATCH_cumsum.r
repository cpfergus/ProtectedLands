

# AREA IS OFF FOR PRIVATE V PUBLIC LANDS...!!!!!!ARGH
gap_area<-read.table(paste0(Output_Folder,"gap_area",".txt"), sep=",", header=TRUE)
own_area<-read.table(paste0(Output_Folder,"own_area",".txt"), sep=",", header=TRUE)
nlcd_area<-read.table(paste0(Output_Folder,"nlcd_area",".txt"), sep=",", header=TRUE)
resil_area<-read.table(paste0(Output_Folder,"resil_area",".txt"), sep=",", header=TRUE)



own_area1 <-filter(own_area, own_Class==1) #this includes all years.
own_area2 <-filter(own_area, own_Class==2)

sum(own_area1$own_area_ha)
# [1] 2295832
sum(own_area2$own_area_ha)
# [1] 395863.2


own_acar1 <-filter(acar_new, own_Class==1) # This is 1985 onward
own_acar2 <-filter(acar_new, own_Class==2)

sum(own_acar1$own_area_ha)
# [1] 658368
sum(own_acar2$own_area_ha)
# [1] 1635396

cum_ArOWN1 <-filter(cum_ArOWN, own_Class==1)
cum_ArOWN2 <-filter(cum_ArOWN, own_Class==2)

sum(cum_ArOWN1$sum)
# [1] 658368
sum(cum_ArOWN2$sum)
# [1] 1635396


# Try with only one of the .csvs at a time
tempA <- select(iStats_corepat,patchID,estYr) 
tempB <- select(maj_categ,patchID,er_maj)
tempAB <- full_join(tempA, tempB)

own_test <- full_join(tempAB, own_area)

##

own_test2 <-filter(own_test, estYr<9999)

# Remove buffer areas
own_foc <- filter(own_test2, er_maj==1|er_maj==2|er_maj==3)

# from 1800 onward (since there are not patches with core area before or on 1800)
own_1800 <-filter(own_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
# pl_new <-filter(pl_foc, estYr>=1985)
own_new <-filter(own_foc, estYr>1985)# 'data.frame':	#1517 obs. of  33 variables

##
own_new1 <-filter(own_new, own_Class==1)
own_new2 <-filter(own_new, own_Class==2)

sum(own_new1$own_area_ha)
# [1] 82153.44
sum(own_new2$own_area_ha)
# [1] 202817.5



cum_ArOWNnew <- group_by(own_new) %>%
group_by(own_Class, estYr) %>%
summarize(sum = sum(own_area_ha)) %>%
mutate(running_total = cumsum(sum))




# Try with only one of the .csvs at a time
tempA <- select(iStats_corepat,patchID,estYr) 
tempB <- select(maj_categ,patchID,er_maj)
tempAB <- full_join(tempA, tempB)

nlcd_test <- full_join(tempAB, nlcd_area)

##

nlcd_test2 <-filter(nlcd_test, estYr<9999)

# Remove buffer areas
nlcd_foc <- filter(nlcd_test2, er_maj==1|er_maj==2|er_maj==3)

# from 1800 onward (since there are not patches with core area before or on 1800)
nlcd_1800 <-filter(nlcd_foc, estYr>1800)
# from 1985 only # Focus on 1985 onward focus is on private conservation efforts
# pl_new <-filter(pl_foc, estYr>=1985)
nlcd_new <-filter(nlcd_foc, estYr>1985)# 'data.frame':	#1517 obs. of  33 variables

##
nlcd_new5 <-filter(nlcd_new, nlcd_Class==5)
nlcd_new7 <-filter(nlcd_new, nlcd_Class==7)

sum(nlcd_new5$nlcd_area_ha)
sum(nlcd_new7$nlcd_area_ha)