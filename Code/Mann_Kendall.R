################################################################################################################
#--------------------------------------------Mann Kendall Analysis---------------------------------------------#

# Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

#------------------------------------------------Purpose-------------------------------------------------------#

# The Mann Kendall Trend Test looks for general increasing or decreasing trends. 
# It's non-parametric in nature, therefore removing the need for a normal distribution of values.
# If our data follows a normal distribution, we could perform a simple linear regression. 
# The null hypothesis for this test assumed there is no monotonic (consistently increases/decreases) trend. 
# The alternative hypothesis is that a trend exists (positive, negative, or non-null).
# It ultimately analyzes differences in signs between temporal data points. 

#------------------------------------------Library Initialisation----------------------------------------------#
setwd("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Mann Kendall")

library(Kendall)
library(xts)
library(stats)
library(boot)
library(zyp)
library(tmap)
library(rgdal)#extract files from file geodatabases-like in ArcMap
library(sp)
library(rgeos)
library(maptools)
library(lattice) # required for trellis.par.set():
library(latticeExtra)
library(GISTools)

options(scipen=999) #Disable scientific notation

#------------------------------------------------Inputs--------------------------------------------------------#

# Inputs were populated using the VA_Statewide_Consumptive_Use.R and VA_HUC8_Consumptive_Use.R scripts

#---Timeseries Consumption Statewide---#

# All Facilities
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_CU.RData")
#Matched Facilities
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_CU_Match.RData")

#---Timeseries Consumption by Facility---#
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_CU_Match_Fac.RData")

#---Timeseries Consumption by Water Use Sector---#

# All Facilities
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_Sector_CU.RData")
# Matched Facilities
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_Sector_CU_Match.RData")

#---Timeseries Consumption by HUC8 Watershed---#

# All Facilities
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_HUC8_CU.RData")
# Matched Facilties
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/TS_HUC8_CU_match.RData")

################################################################################################################
#--------------------------------------------------Analysis----------------------------------------------------#

#------------------Statewide Analysis-------------------#

#-------All Facilities--------#

#--Zyp Trend--#
statewide<-TS_CU[-c(1)] # remove date column

statewide_MK<-matrix(data=-999, nrow=ncol(statewide), ncol=2) # Create dataframe to store MK test results
rownames(statewide_MK)<-"Statewide All Facilities"
colnames(statewide_MK)<-c("tau", "pvalue")
for(i in 1:ncol(statewide)){
  MK<-as.list(zyp.trend.vector(statewide[[i]],preserve.range.for.sig.test=F))
  tau<-MK$tau #stores tau output
  pvalue<-MK$sig #stores pvalue output
  statewide_MK[i,1]<-tau
  statewide_MK[i,2]<-pvalue
  # dataframe store
}
statewide_MK

#--Auto Correlation--#
acf(statewide, plot=T)
pacf(statewide)

#--Seasonal Mann Kendall (No Bootstrapping)--#
statewide<-as.ts(TS_CU)
SeasonalMannKendall(as.ts(statewide))

#--Seasonal Mann Kendall with Bootstrapping--#

MK_bootstrap<- function(df){
  df<-as.ts(df)
  z<-matrix(df, ncol=12, byrow=12)
  zm<-apply(z, MARGIN=2, FUN=mean)
  zs<-apply(z, MARGIN=2, FUN=sd)
  z2<-sweep(z, MARGIN=2, STATS=zm) #subtract monthly means
  z3<-sweep(z2, MARGIN=2, STATS=zs, FUN="/") #divide by monthly sd
  zds<-c(t(z3)) 
  attributes(zds)<-attributes(df)
  plot(df)
  adjust<-SeasonalMannKendall(zds)
  MKtau<-function(z){ MannKendall(z)$tau }
  boot.out<-tsboot(zds, MKtau, R=500, l=4, sim="fixed")
  boot.ci<-boot.ci(boot.out,conf=0.95,type="perc") # 95% CI for the slope of the downward trend detected
  
  return(list(boot.ci,boot.out,adjust))
}
MK_bootstrap(statewide)

#-------Matched Facilities--------#

#--Zyp Trend--#
match_statewide<-TS_CU_Match[-c(1)] # remove date column

match_statewide_MK<-matrix(data=-999, nrow=ncol(match_statewide), ncol=2) # Create dataframe to store MK test results
rownames(match_statewide_MK)<-"Statewide Matched Facilities"
colnames(match_statewide_MK)<-c("tau", "pvalue")
for(i in 1:ncol(match_statewide)){
  MK<-as.list(zyp.trend.vector(match_statewide[[i]],preserve.range.for.sig.test = F))
  tau<-MK$tau #stores tau output
  pvalue<-MK$sig #stores pvalue output
  match_statewide_MK[i,1]<-tau
  match_statewide_MK[i,2]<-pvalue
  # dataframe store
}
match_statewide_MK

#--Auto Correlation--#
acf(match_statewide, plot=T)
pacf(match_statewide)

#--Seasonal Mann Kendall (No Bootstrapping)--#
match_statewide<-as.ts(TS_CU_Match)
SeasonalMannKendall(as.ts(match_statewide))

#--Seasonal Mann Kendall with Bootstrapping--#
MK_bootstrap(match_statewide)

#------------------Sector Analysis----------------------#

#------------All Facilities------------#

#--Zyp Trend--#
sector<-TS_Sector_CU[-c(1)] # remove date column

sector_MK<-matrix(data=-999, nrow=ncol(sector), ncol=2) # Create dataframe to store MK test results
rownames(sector_MK)<-colnames(sector)
colnames(sector_MK)<-c("tau", "pvalue")
for(i in 1:ncol(sector)){
  MK<-as.list(zyp.trend.vector(sector[[i]],preserve.range.for.sig.test = F))#MK test for county
  tau<-MK$tau #stores tau output
  pvalue<-MK$sig #stores pvalue output
  sector_MK[i,1]<-tau
  sector_MK[i,2]<-pvalue
  # dataframe store
}
sector_MK
sector_MK<-as.data.frame(sector_MK)

#-----Agriculture-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
agr<-subset(TS_Sector_CU,select=c(1,2))
SeasonalMannKendall(as.ts(agr))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(agr)
agr<-agr[-c(1)]
acf(agr)
pacf(agr)

#-----Commercial-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
commercial<-subset(TS_Sector_CU,select=c(1,3))
SeasonalMannKendall(as.ts(commercial))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(commercial)
commercial<-commercial[-c(1)]
acf(commercial)
pacf(commercial)

#-----Energy-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
energy<-subset(TS_Sector_CU,select=c(1,4))
SeasonalMannKendall(as.ts(energy))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(energy)
energy<-energy[-c(1)]
acf(energy)
pacf(energy)

#-----Industrial-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
industrial<-subset(TS_Sector_CU,select=c(1,5))
SeasonalMannKendall(as.ts(industrial))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(industrial)
industrial<-industrial[-c(1)]
acf(industrial)
pacf(industrial)

#-----Municipal-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
municipal<-subset(TS_Sector_CU,select=c(1,6))
SeasonalMannKendall(as.ts(municipal))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(municipal)
municipal<-municipal[-c(1)]
acf(municipal)
pacf(municipal)


#------------Matched Facilities------------#

#--Zyp Trend--#
match_sector<-TS_Sector_CU_Match[-c(1)] # remove date column

match_sector_MK<-matrix(data=-999, nrow=ncol(match_sector), ncol=2) # Create dataframe to store MK test results
rownames(match_sector_MK)<-colnames(match_sector)
colnames(match_sector_MK)<-c("tau", "pvalue")
for(i in 1:ncol(match_sector)){
  MK<-as.list(zyp.trend.vector(match_sector[[i]],preserve.range.for.sig.test = F))#MK test for county
  tau<-MK$tau #stores tau output
  pvalue<-MK$sig #stores pvalue output
  match_sector_MK[i,1]<-tau
  match_sector_MK[i,2]<-pvalue
  # dataframe store
}
match_sector_MK
match_sector_MK<-as.data.frame(match_sector_MK)

#-----Agriculture-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
agr<-subset(TS_Sector_CU_Match,select=c(1,2))
SeasonalMannKendall(as.ts(agr))
#--Seasonal Adjustment with Block Bootstrapping--#
agr<-agr[complete.cases(agr),]
MK_bootstrap(agr)
agr<-agr[-c(1)]
acf(agr)
pacf(agr)

#-----Commercial-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
commercial<-subset(TS_Sector_CU_Match,select=c(1,3))
SeasonalMannKendall(as.ts(commercial))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(commercial)
commercial<-commercial[-c(1)]
acf(commercial)
pacf(commercial)

#-----Energy-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
energy<-subset(TS_Sector_CU_Match,select=c(1,4))
SeasonalMannKendall(as.ts(energy))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(energy)
energy<-energy[-c(1)]
acf(energy)
pacf(energy)

#-----Industrial-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
industrial<-subset(TS_Sector_CU_Match,select=c(1,5))
SeasonalMannKendall(as.ts(industrial))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(industrial)
industrial<-industrial[-c(1)]
acf(industrial)
pacf(industrial)

#-----Municipal-------#
#--Seasonal Mann Kendall (No Bootstrapping)--#
municipal<-subset(TS_Sector_CU_Match,select=c(1,6))
SeasonalMannKendall(as.ts(municipal))
#--Seasonal Adjustment with Block Bootstrapping--#
MK_bootstrap(municipal)
municipal<-municipal[-c(1)]
acf(municipal)
pacf(municipal)




#------------------Facility Level Analysis-------------------#

facility_MK<- function(VWUDSID){
  fac_MK<-subset(TS_CU_Match_Fac,TS_CU_Match_Fac$VWUDS.Facility.ID==VWUDSID,select=c(2,8))
  
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  fac_MK[is.nan.data.frame(fac_MK)]<-NA
  
  fac_MK<-fac_MK[complete.cases(fac_MK),]
  
  MK<-SeasonalMannKendall(as.ts(fac_MK))
  MK_reg<-MannKendall(as.ts(fac_MK))
  
  fac_MK<-fac_MK[-c(1)]
  acf(fac_MK)
  pacf(fac_MK)
  
  return(list(MK,MK_reg))
  
}
fac_MK<-subset(TS_CU_Match_Fac,TS_CU_Match_Fac$VWUDS.Facility.ID=="67224",select=c(2,8))

facility_MK("67224") # Surry Power Station
facility_MK("72023") # North Anna
facility_MK("73183") # Chesterfield
facility_MK("72566") # Yorktown Fossil Plant
facility_MK("73170") # Possum Point
facility_MK("72489") # Advansix Resins & ChemicalsLLC
facility_MK("74082") # Potomac River Generation Station
facility_MK("72173") # Glen Lyn
facility_MK("73872") # Bremo Bluff Power Plant
facility_MK("72744") # Spruance Plant
facility_MK("72251") # Hopewell District

#-------------------HUC8 Analysis-----------------------#

# All Facilities 
HUC8<-TS_HUC8_CU[-c(1)] # remove date column

HUC8_MK<-matrix(data=-999, nrow=ncol(HUC8), ncol=2) # Create dataframe to store MK test results
rownames(HUC8_MK)<-colnames(HUC8)
colnames(HUC8_MK)<-c("tau", "pvalue")
for(i in 1:ncol(HUC8)){
  MK<-as.list(zyp.trend.vector(HUC8[[i]]))#MK test for county
  tau<-MK$tau #stores tau output
  pvalue<-MK$sig #stores pvalue output
  HUC8_MK[i,1]<-tau
  HUC8_MK[i,2]<-pvalue
  # dataframe store
}
HUC8_MK
HUC8_MK<-as.data.frame(HUC8_MK)
HUC8_MK$HUC8<-gsub("X","",row.names(HUC8_MK))

# Max and Min Values
HUC8_MK[which.max(HUC8_MK$tau),]
HUC8_MK[which.min(HUC8_MK$tau),]
HUC8_MK[which.max(HUC8_MK$pvalue),]
HUC8_MK[which.min(HUC8_MK$pvalue),]

write.table(HUC8_MK, "HUC8_MK_test.txt",sep="\t",row.names=TRUE)

# Matched Facilities
match_HUC8<-TS_HUC8_CU_match[-c(1)] # remove date column

match_HUC8_MK<-matrix(data=-999, nrow=ncol(match_HUC8), ncol=2) # Create dataframe to store MK test results
rownames(match_HUC8_MK)<-colnames(match_HUC8)
colnames(match_HUC8_MK)<-c("tau", "pvalue")
for(i in 1:ncol(match_HUC8)){
  MK<-as.list(zyp.trend.vector(match_HUC8[[i]]))#MK test for county
  tau<-MK$tau #stores tau output
  pvalue<-MK$sig #stores pvalue output
  match_HUC8_MK[i,1]<-tau
  match_HUC8_MK[i,2]<-pvalue
  # dataframe store
}
match_HUC8_MK
match_HUC8_MK<-as.data.frame(match_HUC8_MK)
match_HUC8_MK$HUC8<-gsub("X","",row.names(match_HUC8_MK))

# Max and Min Values
match_HUC8_MK[which.max(match_HUC8_MK$tau),]
match_HUC8_MK[which.min(match_HUC8_MK$tau),]
match_HUC8_MK[which.max(match_HUC8_MK$pvalue),]
match_HUC8_MK[which.min(match_HUC8_MK$pvalue),]

write.table(match_HUC8_MK, "match_HUC8_MK_test.txt",sep="\t",row.names=TRUE)

#-----Plot HUC8 Findings--------#

#Load databases and extract required layers
HUC8<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/HUC.gdb",layer='WBDHU8')
VA<-readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC8<-sp::spTransform(HUC8, CRS("+init=epsg:4269"))
VA<-sp::spTransform(VA, CRS("+init=epsg:4269"))

HUC8@data$MK_pvalue<-ifelse(HUC8_MK$HUC8%in%HUC8@data$HUC8,HUC8_MK$pvalue[match(HUC8@data$HUC8,HUC8_MK$HUC8)],NA)
HUC8@data$MK_tau<-ifelse(HUC8_MK$HUC8%in%HUC8@data$HUC8,HUC8_MK$tau[match(HUC8@data$HUC8,HUC8_MK$HUC8)],NA)
HUC8@data$Match_MK_pvalue<-ifelse(match_HUC8_MK$HUC8%in%HUC8@data$HUC8,match_HUC8_MK$pvalue[match(HUC8@data$HUC8,match_HUC8_MK$HUC8)],NA)
HUC8@data$Match_MK_tau<-ifelse(match_HUC8_MK$HUC8%in%HUC8@data$HUC8,match_HUC8_MK$tau[match(HUC8@data$HUC8,match_HUC8_MK$HUC8)],NA)

HUC8@data$sig<-ifelse(HUC8@data$MK_pvalue<=0.05,1,NA)
HUC8@data$match_sig<-ifelse(HUC8@data$Match_MK_pvalue<=0.05,1,NA)

HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")

# Write shapefile to use in ArcMap
writeOGR(obj=HUC8_Clipped, dsn="tempdir", layer="HUC8", driver="ESRI Shapefile")
