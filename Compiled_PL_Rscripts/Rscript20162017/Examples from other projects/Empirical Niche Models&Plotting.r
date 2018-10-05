# Empirical Niche R Code
# Paper: "Empirical tests on the relationship between geographic range and climatic niche breadth for narrowly and broadly distributed species"
# Corresponding Author: Iara Lacher - illacher@ucdavis.edu

# LOAD PACKAGES
library(rethinking)
library(bbmle)

# READ .CVS
CBR<-read.csv("CGCP_e1_B_R.csv")
MBR<-read.csv("MGMN_e1_B_R.csv")

# SPLIT DATA BY SPECIES
mgBR<-subset(MBR, Species=="MIGU")
mnBR<-subset(MBR, Species=="MINU")
cpBR<-subset(CBR, Species=="CLPU")
cgBR<-subset(CBR, Species=="CLGR")

# MODEL BUILDING
MBi2s<-mle2(BiomassGrams~dnorm(mean=a+bt*TempScen+bt2*TempScen^2+bp*TreatmentRev+bs*Species_D+btp*TempScen*TreatmentRev+bst*Species_D*TempScen+bt2s*Species_D*TempScen^2+bps*TreatmentRev*Species_D+bstp*Species_D*TempScen*TreatmentRev, sd=sigma), data=MBR, start=list(a=mean(MBR$BiomassGrams), bt=0, bt2=0, bp=0, bs=0, btp=0, bst=0, bt2s=0, bps=0, bstp=0, sigma=sd(MBR$BiomassGrams)))

MSi2s<-mle2(S~dpois(lambda=exp(a+bt*TempScen+bt2*TempScen^2+bp*TreatmentRev+bs*Species_D+btp*TempScen*TreatmentRev+bst*Species_D*TempScen+bt2s*Species_D*TempScen^2+bps*TreatmentRev*Species_D+bstp*Species_D*TempScen*TreatmentRev)), data=MBR, start=list(a=mean(MBR$S), bt=0, bt2=0, bp=0, bs=0, btp=0, bst=0, bt2s=0, bps=0, bstp=0))

CBi2s<-mle2(BiomassGrams~dnorm(mean=a+bt*TempScen+bt2*TempScen^2+bp*TreatmentRev+bs*Species_D+btp*TempScen*TreatmentRev+bst*Species_D*TempScen+bt2s*Species_D*TempScen^2+bps*TreatmentRev*Species_D+bstp*Species_D*TempScen*TreatmentRev, sd=sigma), data=CBR, start=list(a=mean(CBR$BiomassGrams), bt=0, bt2=0, bp=0, bs=0, btp=0, bst=0, bt2s=0, bps=0, bstp=0, sigma=sd(CBR$BiomassGrams)))

CSi2s<-mle2(S~dpois(lambda=exp(a+bt*TempScen+bt2*TempScen^2+bp*TreatmentRev+bs*Species_D+btp*TempScen*TreatmentRev+bst*Species_D*TempScen+bt2s*Species_D*TempScen^2+bps*TreatmentRev*Species_D+bstp*Species_D*TempScen*TreatmentRev)), data=CBR, start=list(a=mean(CBR$S), bt=0, bt2=0, bp=0, bs=0, btp=0, bst=0, bt2s=0, bps=0, bstp=0))

# AIC VALUES
sapply(mMBlist,"AIC")
sapply(mCBlist,"AIC")
sapply(mMSlist,"AIC")
sapply(mCSlist,"AIC")

# SAMPLE THE POSTERIOR
pMBi2s<-sample.naive.posterior(MBi2s, n=10000, nobs=nrow(MBR))
pMSi2s <- sample.naive.posterior(MSi2s, n=10000 , nobs=nrow(MBR) )
pCBi2s<-sample.naive.posterior(CBi2s, n=10000, nobs=nrow(CBR))
pCSi2s <- sample.naive.posterior(CSi2s, n=10000 , nobs=nrow(CBR) )


# PLOT (WITHOUT POINTS OR LEGEND)

# TO PLACE ALL ON ONE GRAPH:
par(mfrow=c(2,6))
par(mar=c(1,1,1,1))

# MIMULUS BIOMASS
for(t in 1:3){
dt<-MBR[MBR$TempScen==t,]
plot(BiomassGrams~TreatmentRev, data=dt, type="n", col="grey60", xaxp=c(1,7,6), ylim=c(0,0.5), axes=FALSE, xlab="", ylab="")
axis(side=1, las=0, tck=0.02, labels=FALSE)
axis(side=2, las=2, tck=0.02, labels=FALSE)
Ppt.seq<-seq(from=1, to=7, by=0.001)
mu.MN <- sapply( Ppt.seq , function(z)
mean(pMBi2s$a + pMBi2s$bt*t+pMBi2s$bt2*t^2+pMBi2s$bp*z +pMBi2s$bs*0+ pMBi2s$btp*t*z+ pMBi2s$bst*0*t+pMBi2s$bt2s*0*t^2+pMBi2s$bps*z*0+pMBi2s$bstp*t*z*0))
mu.MG <- sapply( Ppt.seq , function(z)
mean(pMBi2s$a + pMBi2s$bt*t+pMBi2s$bt2*t^2+pMBi2s$bp*z +pMBi2s$bs*1+ pMBi2s$btp*t*z+ pMBi2s$bst*1*t+pMBi2s$bt2s*1*t^2+pMBi2s$bps*z*1+pMBi2s$bstp*t*z*1))
mu.ci.MN <- sapply( Ppt.seq , function(z)
HPDI( pMBi2s$a + pMBi2s$bt*t+pMBi2s$bt2*t^2+pMBi2s$bp*z +pMBi2s$bs*0+ pMBi2s$btp*t*z+ pMBi2s$bst*0*t+pMBi2s$bt2s*0*t^2+pMBi2s$bps*z*0+pMBi2s$bstp*t*z*0) ) 
mu.ci.MG <- sapply( Ppt.seq , function(z)
HPDI( pMBi2s$a + pMBi2s$bt*t+pMBi2s$bt2*t^2+pMBi2s$bp*z +pMBi2s$bs*1+ pMBi2s$btp*t*z+ pMBi2s$bst*1*t+pMBi2s$bt2s*1*t^2+pMBi2s$bps*z*1+pMBi2s$bstp*t*z*1) ) 
lines(Ppt.seq, mu.MN, lwd=3, col="grey10")
lines(Ppt.seq, mu.MG, lwd=3, col="grey60")
lines(Ppt.seq, mu.ci.MN[1,], lty=2, col="grey10") 
lines(Ppt.seq, mu.ci.MN[2,], lty=2, col="grey10")
lines(Ppt.seq, mu.ci.MG[1,], lty=2, col="grey60")
lines(Ppt.seq, mu.ci.MG[2,], lty=2, col="grey60")
}

# CLARKIA BIOMASS
for(t in 1:3){
dt<-CBR[CBR$TempScen==t,]
plot(BiomassGrams~TreatmentRev, data=dt, type="n", col="grey60", xaxp=c(1,7,6), ylim=c(0,5), axes=FALSE, xlab="", ylab="")
axis(side=1, las=0, tck=0.02, labels=FALSE)
axis(side=2, las=2, tck=0.02, labels=FALSE)
Ppt.seq<-seq(from=1, to=7, by=0.001)
mu.CG <-sapply(Ppt.seq, function(z) mean(pCBi2s$a + pCBi2s$bt*t+pCBi2s$bt2*t^2+pCBi2s$bp*z +pCBi2s$bs*0+ pCBi2s$btp*t*z+ pCBi2s$bst*0*t+pCBi2s$bt2s*0*t^2+pCBi2s$bps*z*0+pCBi2s$bstp*t*z*0))
mu.CP<-sapply(Ppt.seq, function(z) mean(pCBi2s$a + pCBi2s$bt*t+pCBi2s$bt2*t^2+pCBi2s$bp*z +pCBi2s$bs*1+ pCBi2s$btp*t*z+ pCBi2s$bst*1*t+pCBi2s$bt2s*1*t^2+pCBi2s$bps*z*1+pCBi2s$bstp*t*z*1))
mu.ci.CG<-sapply(Ppt.seq, function(z) HPDI(pCBi2s$a + pCBi2s$bt*t+pCBi2s$bt2*t^2+pCBi2s$bp*z +pCBi2s$bs*0+ pCBi2s$btp*t*z+ pCBi2s$bst*0*t+pCBi2s$bt2s*0*t^2+pCBi2s$bps*z*0+pCBi2s$bstp*t*z*0))
mu.ci.CP<-sapply(Ppt.seq, function(z) HPDI(pCBi2s$a + pCBi2s$bt*t+pCBi2s$bt2*t^2+pCBi2s$bp*z +pCBi2s$bs*1+ pCBi2s$btp*t*z+ pCBi2s$bst*1*t+pCBi2s$bt2s*1*t^2+pCBi2s$bps*z*1+pCBi2s$bstp*t*z*1))
lines(Ppt.seq, mu.CG, lwd=3, col="grey10")
lines(Ppt.seq, mu.CP, lwd=3, col="grey60")
lines(Ppt.seq, mu.ci.CG[1,], lty=2, col="grey10")
lines(Ppt.seq, mu.ci.CG[2,], lty=2, col="grey60")
lines(Ppt.seq, mu.ci.CP[1,], lty=2, col="grey10")
lines(Ppt.seq, mu.ci.CP[2,], lty=2, col="grey60")
}

# MIMULUS SEEDPOD
for(t in 1:3){
dt<-MBR[MBR$TempScen==t,]
plot(S~TreatmentRev, data=dt, type="n", col="grey60", xaxp=c(1,7,6), ylim=c(0,20), axes=FALSE, xlab="", ylab="")
axis(side=1, las=0, tck=0.02, labels=FALSE)
axis(side=2, las=2, tck=0.02, labels=FALSE)
Ppt.seq<-seq(from=1, to=7, by=0.001)
mu.MN <- sapply( Ppt.seq , function(z)
mean( exp( pMSi2s$a + pMSi2s$bt*t+pMSi2s$bt2*t^2+pMSi2s$bp*z +pMSi2s$bs*0+ pMSi2s$btp*t*z+ pMSi2s$bst*0*t+pMSi2s$bt2s*0*t^2+pMSi2s$bps*z*0+pMSi2s$bstp*t*z*0) ) )
mu.MG <- sapply( Ppt.seq , function(z)
mean( exp( pMSi2s$a + pMSi2s$bt*t+pMSi2s$bt2*t^2+pMSi2s$bp*z +pMSi2s$bs*1+ pMSi2s$btp*t*z+ pMSi2s$bst*1*t+pMSi2s$bt2s*1*t^2+pMSi2s$bps*z*1+pMSi2s$bstp*t*z*1)))
mu.ci.MN <- sapply( Ppt.seq , function(z)
PCI( exp( pMSi2s$a + pMSi2s$bt*t+pMSi2s$bt2*t^2+pMSi2s$bp*z +pMSi2s$bs*0+ pMSi2s$btp*t*z+ pMSi2s$bst*0*t+pMSi2s$bt2s*0*t^2+pMSi2s$bps*z*0+pMSi2s$bstp*t*z*0) ) )
mu.ci.MG <- sapply( Ppt.seq , function(z)
PCI( exp( pMSi2s$a + pMSi2s$bt*t+pMSi2s$bt2*t^2+pMSi2s$bp*z +pMSi2s$bs*1+ pMSi2s$btp*t*z+ pMSi2s$bst*1*t+pMSi2s$bt2s*1*t^2+pMSi2s$bps*z*1+pMSi2s$bstp*t*z*1) ) )
lines(Ppt.seq, mu.MN, lwd=3, col="grey10")
lines(Ppt.seq, mu.MG, lwd=3, col="grey60")
lines(Ppt.seq, mu.ci.MN[1,], lty=2, col="grey10") 
lines(Ppt.seq, mu.ci.MN[2,], lty=2, col="grey10")
lines(Ppt.seq, mu.ci.MG[1,], lty=2, col="grey60")
lines(Ppt.seq, mu.ci.MG[2,], lty=2, col="grey60")
}

# CLARKIA SEEDPOD
for(t in 1:3){
dt<-CBR[CBR$TempScen==t,]
plot(S~TreatmentRev, data=dt, type="n", col="grey60", xaxp=c(1,7,6), ylim=c(0,20), axes=FALSE, xlab="", ylab="")
axis(side=1, las=0, tck=0.02, labels=FALSE)
axis(side=2, las=2, tck=0.02, labels=FALSE)
Ppt.seq<-seq(from=1, to=7, by=0.001)
mu.CG <- sapply( Ppt.seq , function(z)
mean( exp( pCSi2s$a + pCSi2s$bt*t+pCSi2s$bt2*t^2+pCSi2s$bp*z +pCSi2s$bs*0+ pCSi2s$btp*t*z+ pCSi2s$bst*0*t+pCSi2s$bt2s*0*t^2+pCSi2s$bps*z*0+pCSi2s$bstp*t*z*0) ) )
mu.CP <- sapply( Ppt.seq , function(z)
mean( exp( pCSi2s$a + pCSi2s$bt*t+pCSi2s$bt2*t^2+pCSi2s$bp*z +pCSi2s$bs*1+ pCSi2s$btp*t*z+ pCSi2s$bst*1*t+pCSi2s$bt2s*1*t^2+pCSi2s$bps*z*1+pCSi2s$bstp*t*z*1) ) )
mu.ci.CG <- sapply( Ppt.seq , function(z)
PCI( exp( pCSi2s$a + pCSi2s$bt*t+pCSi2s$bt2*t^2+pCSi2s$bp*z +pCSi2s$bs*0+ pCSi2s$btp*t*z+ pCSi2s$bst*0*t+pCSi2s$bt2s*0*t^2+pCSi2s$bps*z*0+pCSi2s$bstp*t*z*0 ) ) )
mu.ci.CP <- sapply( Ppt.seq , function(z)
PCI( exp( pCSi2s$a + pCSi2s$bt*t+pCSi2s$bt2*t^2+pCSi2s$bp*z +pCSi2s$bs*1+ pCSi2s$btp*t*z+ pCSi2s$bst*1*t+pCSi2s$bt2s*1*t^2+pCSi2s$bps*z*1+pCSi2s$bstp*t*z*1) ) )
lines(Ppt.seq, mu.CG, lwd=3, col="grey10")
lines(Ppt.seq, mu.CP, lwd=3, col="grey60")
lines(Ppt.seq, mu.ci.CG[1,], lty=2, col="grey10")
lines(Ppt.seq, mu.ci.CG[2,], lty=2, col="grey60")
lines(Ppt.seq, mu.ci.CP[1,], lty=2, col="grey10")
lines(Ppt.seq, mu.ci.CP[2,], lty=2, col="grey60")}


