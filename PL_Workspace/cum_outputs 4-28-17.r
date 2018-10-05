





> TOTcum_Ar
Source: local data frame [3 x 3]

  er_maj       sum Type
1      1 1778692.0 area
2      2  122637.2 area
3      3  265589.3 area



> Tot_Area_PLunk
Source: local data frame [9 x 4]

  er_maj      sum      avg        sd
1      1 45091.08 469.6988  886.2909
2      2  4474.44 165.7200  225.9630
3      3 61074.00 555.2182 1553.0080
4      5   771.12 385.5600  366.5642
5      6  1458.00 208.2857  204.6141
6      7 21306.24 318.0036  401.0794
7      8 33009.12 340.3002  989.2539
8      9 90791.28 662.7101 1728.1678
9     10 12503.16 297.6943  398.0158


> Tot_Area_PL_erunk
Source: local data frame [2 x 5]
Groups: er_maj

  er_maj estYr      sum      avg        sd
1      1  9999 45091.08 469.6988  886.2909
2     23  9999 65548.44 478.4558 1402.4828

> Tot_Area_PLnotunk
Source: local data frame [10 x 4]

   er_maj        sum       avg          sd
1       1 1778691.96 2515.8302 16925.82535
2       2  122637.24  136.7193   168.91837
3       3  265589.28  617.6495  4718.07006
4       4    3052.08 1526.0400  1493.74893
5       5     913.68  228.4200   246.70141
6       6     171.72   85.8600    43.52949
7       7   46552.32  117.8540   259.81810
8       8   82940.76  564.2229  2711.74822
9       9  381519.72  426.7558  2293.86174
10     10   84272.40  916.0043  3831.79361

> Tot_Area_PL_erSD
Source: local data frame [2 x 4]

  er_maj       sum       avg        sd
1      1 1778692.0 2515.8302 16925.825
2     23  388226.5  292.5595  2696.629

## OPEN in pivot table to calc vals
TOTcum_ArOM<-read.table(paste0(Output_Folder,"TOTcum_ArOM",".txt"), sep=",", header=TRUE)

pp_maj
Row Labels	1	23	(blank)	Grand Total
1	1703647.08	232654.68		1936301.76
2	75044.88	155571.84		230616.72
(blank)				
Grand Total	1778691.96	388226.52		2166918.48



Own_maj
Row Labels	1	23	(blank)	Grand Total
1	1608203.16	186614.28		1794817.44
2	72695.88	153815.76		226511.64
3	2216.16	1445.04		3661.2
4	95576.76	46351.44		141928.2


Gap_maj
Row Labels	1	23	(blank)	Grand Total
1	235596.6	1299.24		236895.84
2	186160.68	39599.28		225759.96
3	1198349.64	142420.68		1340770.32
4	158585.04	204907.32		363492.36




TOTcum_ArNLCD<-read.table(paste0(Output_Folder,"TOTcum_ArNLCD",".txt"), sep=",", header=TRUE)
	BR	P
1	5031.72	28155.6
2	37230.84	12895.2
3	2002.32	1982.88
4	1049.76	479.52
5	1704881.52	253118.52
6	26700.84	70794
7	638.28	14152.32
8	1156.68	6648.48



TOTcum_ArRESIL<-read.table(paste0(Output_Folder,"TOTcum_ArRESIL",".txt"), sep=",", header=TRUE)
	# BR	P
# 1	109443.96	159881.04
# 2	70427.88	73091.16
# 3	1296660.96	76496.4
# 4	22984.56	14443.92
# 5	243871.56	24180.12
# 6	4458.24	6340.68
# 7	10060.2	2967.84
# 8	14521.68	2964.6





> summary(cum_OWN1[,c(2,5:7)]); summary(cum_OWN23[,c(2,5:7)])
   own_Class        estYr           sum          running_total   
 Min.   :1.00   Min.   :1985   Min.   :    0.0   Min.   :     0  
 1st Qu.:1.75   1st Qu.:1993   1st Qu.:    0.0   1st Qu.:  2910  
 Median :2.50   Median :2001   Median :  233.3   Median :  6474  
 Mean   :2.50   Mean   :2001   Mean   : 3980.9   Mean   : 59268  
 3rd Qu.:3.25   3rd Qu.:2008   3rd Qu.: 3975.5   3rd Qu.:107895  
 Max.   :4.00   Max.   :2015   Max.   :87583.7   Max.   :250141  
 NA's   :2                     NA's   :2         NA's   :2       
   own_Class        estYr           sum          running_total     
 Min.   :1.00   Min.   :1985   Min.   :    0.0   Min.   :     0.0  
 1st Qu.:1.75   1st Qu.:1993   1st Qu.:    0.0   1st Qu.:   803.5  
 Median :2.50   Median :2000   Median :  401.8   Median :  7711.2  
 Mean   :2.50   Mean   :2000   Mean   : 5818.3   Mean   : 65759.4  
 3rd Qu.:3.25   3rd Qu.:2007   3rd Qu.: 6266.2   3rd Qu.: 61469.3  
 Max.   :4.00   Max.   :2015   Max.   :48289.0   Max.   :581087.5  
 NA's   :2                     NA's   :2         NA's   :2         
> 
> summary(cum_GAP1[,c(1,5:7)]); summary(cum_GAP23[,c(1,5:7)])
   gap_Class        estYr           sum          running_total   
 Min.   :1.00   Min.   :1985   Min.   :    0.0   Min.   :     0  
 1st Qu.:1.75   1st Qu.:1993   1st Qu.:    0.0   1st Qu.:  5210  
 Median :2.50   Median :2001   Median :  790.6   Median : 19401  
 Mean   :2.50   Mean   :2001   Mean   : 3980.9   Mean   : 59268  
 3rd Qu.:3.25   3rd Qu.:2008   3rd Qu.: 3965.8   3rd Qu.:126425  
 Max.   :4.00   Max.   :2015   Max.   :55870.6   Max.   :167702  
 NA's   :2                     NA's   :2         NA's   :2       
   gap_Class        estYr           sum          running_total   
 Min.   :1.00   Min.   :1985   Min.   :    0.0   Min.   :     0  
 1st Qu.:1.75   1st Qu.:1993   1st Qu.:    0.0   1st Qu.:  2372  
 Median :2.50   Median :2000   Median :  447.1   Median :  7841  
 Mean   :2.50   Mean   :2000   Mean   : 5818.3   Mean   : 65759  
 3rd Qu.:3.25   3rd Qu.:2007   3rd Qu.: 6149.5   3rd Qu.: 54740  
 Max.   :4.00   Max.   :2015   Max.   :48574.1   Max.   :575346  
 NA's   :2                     NA's   :2         NA's   :2    

> summary(cum_NLCD1[,c(3,5:7)]); summary(cum_NLCD23[,c(3,5:7)])
   nlcd_Class     estYr           sum           running_total     
 Min.   :5    Min.   :1986   Min.   :    0.00   Min.   :     0.0  
 1st Qu.:5    1st Qu.:1994   1st Qu.:   38.88   1st Qu.:   777.6  
 Median :6    Median :2001   Median :  492.48   Median :  6207.8  
 Mean   :6    Mean   :2001   Mean   : 5196.22   Mean   : 77452.7  
 3rd Qu.:7    3rd Qu.:2008   3rd Qu.: 3894.48   3rd Qu.: 30857.8  
 Max.   :7    Max.   :2015   Max.   :89748.00   Max.   :422379.4  
   nlcd_Class     estYr           sum          running_total   
 Min.   :5    Min.   :1986   Min.   :  233.3   Min.   :   648  
 1st Qu.:5    1st Qu.:1993   1st Qu.: 1561.7   1st Qu.: 17814  
 Median :6    Median :2000   Median : 4030.6   Median : 38906  
 Mean   :6    Mean   :2000   Mean   : 7329.1   Mean   : 82780  
 3rd Qu.:7    3rd Qu.:2007   3rd Qu.:11249.3   3rd Qu.:118902  
 Max.   :7    Max.   :2014   Max.   :39696.5   Max.   :353977 

