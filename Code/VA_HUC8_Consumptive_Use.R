###########################################################################################################################################
#########################Estimating Consumptive Use in Virginia HUC 8 Watersheds##########################################################

##This code calculates consumptive use by performing a water balance (In-Out=Change in Storage)
#Inputs are defined as the discharges/return flows from NPDES permitted facilities (ECHO Database)
#Outputs are the withdrawals from surface water found in the Virginia Wateruse Data System (VWUDS)
#The change in storage refers to transfers between facilities (releases and deliveries)--This is calculated in this script

###########################################################################################################################################
####House Keeping####

#Load required packages-including spatial analysis
library(dplyr)
library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)

#Basic function to sum values and track NAs
plus<-function(x){
  if(all(is.na(x))){
    c(NA)
  }else{
    sum(x,na.rm = TRUE)}
}

###########################################################################################################################################
####HUC8 and Virginia Shapefile Manipulation####

#Load databases and extract required layers
HUC8<-readOGR("C:/Users/Morgan/Documents/VT/GRA/HUC.gdb",layer='WBDHU8')
VA<-readOGR('C:/Users/Morgan/Documents/VT/GRA/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC8<-spTransform(HUC8, CRS("+init=epsg:4269"))
VA<-spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
#plot(HUC8_Clipped)

#Create HUC8 Dataframe that will be used in future overlay processes
HUC8_Overlay<-HUC8 #Keep integrity of spatial dataframe
HUC8_Overlay@data<-HUC8_Overlay@data[,c(11,12)] 
names(HUC8_Overlay)<-c("HUC8","HUC8Name")

###########################################################################################################################################
###########################################Calculating Transfers###########################################################################
#There are two different types of Transfers: TO and FROM
#This is because VA Hydro stores transfers at both the recieving and departing facilities
#Therefore, we will keep them separate during calculations and refer to transfers as deliveries and releases in both the TO and FROM lists

###########################################################################################################################################
####Delivery Transfers (TO)####
#Download list of delivery transfers from this link
#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/vwuds_data_cb/edit/views_data_export_4
transTo<-read.csv("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/vahydro_transfer_to.csv")
transTo$geom<-as.character(transTo$geom)

####Reformat Coordinates####
#Transfer geometry comes in the form "LINESTRING(TO LONG TO LAT, FROM LONG FROM LAT)
#The following lines extract the to and from geometry and store them in appropriate columns
transTo$geomFlat<-gsub(".*[(]([^,]+),\\s.*","\\1",transTo$geom) 
transTo$geomFlon<-as.numeric(gsub(" [^ ]*$","\\1",transTo$geomFlat)) #FROM
transTo$geomFlat<-as.numeric(gsub("^[^ ]* ","\\1",transTo$geomFlat)) #FROM
transTo$geomTlat<-gsub(".*[,] ([^)]+)).*","\\1",transTo$geom) 
transTo$geomTlon<-as.numeric(gsub(" [^ ]*$","\\1",transTo$geomTlat)) #TO
transTo$geomTlat<-as.numeric(gsub("^[^ ]* ","\\1",transTo$geomTlat)) #TO

####Create Spatial Data Frame from Points and Overlay on Clipped HUC8 Shapefile####
####FROM Delivery Transfers####
#Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
#Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
dFrom<-transTo[!(is.na(transTo$geomFlat)&is.na(transTo$geomFlon)),]
dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
HUC8_Facilities<-over(dFrom,HUC8_Overlay)#Spatial overlay
dFrom@data$HUC8<-HUC8_Facilities$HUC8
dFrom@data$HUC8Name<-HUC8_Facilities$HUC8Name

####TO Delivery Transfers####
#Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
#Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
dTo<-transTo[!(is.na(transTo$geomTlat)&is.na(transTo$geomTlon)),]
dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
HUC8_Facilities<-over(dTo,HUC8_Overlay)#Spatial overlay
dTo@data$HUC8<-HUC8_Facilities$HUC8
dTo@data$HUC8Name<-HUC8_Facilities$HUC8Name

####Determine if TO Transfers are leaving watershed boundaries####
#Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
#We are only concerned with interbasin transfers and need to identify these with the following code
dTo@data$interbasin<-NA
dFrom@data$interbasin<-NA
#Check each transfer in the delivery VA Hydro transfers to see if its FROM HUC is different than its TO HUC
#If transfers occurred outside of HUC boundaries, set HUC as "None"
for (i in 1:length(dTo@data$hydroid)){
  ToHUC<-as.character(dTo@data$HUC8[i])
  if(is.na(ToHUC)){
    ToHUC<-'None'
  }
  FromHUC<-as.character(dFrom@data$HUC8[dFrom@data$hydroid==dTo@data$hydroid[i]])
  if(is.na(FromHUC)){
    FromHUC<-'None' 
  }
  interbasin<-0
  if(ToHUC!=FromHUC){
    interbasin<-1
  }
  dTo@data$interbasin[i]<-interbasin
  dFrom@data$interbasin[i]<-interbasin
}

####Sum Net Water In and Out for each HUC8 Watershed####
###FROM Deliveries###
delf<-summarize(group_by(dFrom@data,HUC8Name,interbasin),waterout=sum(tsvalue/365,na.rm=T)) #units in MGY need in MGD
delf<-delf[delf$interbasin==1,]
delf$HUC8Name<-as.character(delf$HUC8Name)
delf$HUC8Name[is.na(delf$HUC8Name)]<-'None'
###TO Deliveries###
delt<-summarize(group_by(dTo@data,HUC8Name,interbasin),waterin=sum(tsvalue/365,na.rm=T))
delt<-delt[delt$interbasin==1,]
delt$HUC8Name<-as.character(delt$HUC8Name)
delt$HUC8Name[is.na(delt$HUC8Name)]<-'None'
###########################################################################################################################################
####Release Transfers (FROM)####
#Ignoring repeat hydroids (same transfer reported from both recieving and transfer facility),
#repeat the above steps using the remaining FROM transfers available at:
#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/vwuds_data_cb/edit/views_data_export_5
transFrom<-read.csv("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/vahydro_transfer_from.csv")
transFrom$geom<-as.character(transFrom$geom)
transFrom<-transFrom[!(transFrom$hydroid%in%transTo$hydroid),]

####Reformat Coordinates####
#Transfer geometry comes in the form "LINESTRING(TO LONG TO LAT, FROM LONG FROM LAT)
#The following lines extract the to and from geometry and store them in appropriate columns
transFrom$geomFlat<-gsub(".*[(]([^,]+),\\s.*","\\1",transFrom$geom)
transFrom$geomFlon<-as.numeric(gsub(" [^ ]*$","\\1",transFrom$geomFlat))
transFrom$geomFlat<-as.numeric(gsub("^[^ ]* ","\\1",transFrom$geomFlat))
transFrom$geomTlat<-gsub(".*[,] ([^)]+)).*","\\1",transFrom$geom)
transFrom$geomTlon<-as.numeric(gsub(" [^ ]*$","\\1",transFrom$geomTlat))
transFrom$geomTlat<-as.numeric(gsub("^[^ ]* ","\\1",transFrom$geomTlat))

####Create Spatial Data Frame from Points and Overlay on Clipped HUC8 Shapefile####
####FROM Release Transfers####
rFrom<-transFrom[!(is.na(transFrom$geomFlat)&is.na(transFrom$geomFlon)),]
rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
HUC8_Facilities<-over(rFrom,HUC8_Overlay)
rFrom@data$HUC8<-HUC8_Facilities$HUC8
rFrom@data$HUC8Name<-HUC8_Facilities$HUC8Name
####TO Release Transfers####
rTo<-transFrom[!(is.na(transFrom$geomTlat)&is.na(transFrom$geomTlon)),]
rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
HUC8_Facilities<-over(rTo,HUC8_Overlay)
rTo@data$HUC8<-HUC8_Facilities$HUC8
rTo@data$HUC8Name<-HUC8_Facilities$HUC8Name

####Determine if Release FROM Transfers are leaving watershed boundaries####
rTo@data$interbasin<-NA
rFrom@data$interbasin<-NA
for (i in 1:length(rTo@data$hydroid)){
  ToHUC<-as.character(rTo@data$HUC8[i])
  if(is.na(ToHUC)){
    ToHUC<-'None'
  }
  FromHUC<-as.character(rFrom@data$HUC8[rFrom@data$hydroid==rTo@data$hydroid[i]])
  if(is.na(FromHUC)){
    FromHUC<-'None' 
  }
  interbasin<-0
  if(ToHUC!=FromHUC){
    interbasin<-1
  }
  rTo@data$interbasin[i]<-interbasin #1 indicating transfer is crossing watershed boundaries
  rFrom@data$interbasin[i]<-interbasin
}

####Sum Net Water In and Out for each HUC8 Watershed####
###FROM Releases###
relf<-summarize(group_by(rFrom@data,HUC8Name,interbasin),waterout=sum(tsvalue/365,na.rm=T))
relf<-relf[relf$interbasin==1,]
relf$HUC8Name<-as.character(relf$HUC8Name)
relf$HUC8Name[is.na(relf$HUC8Name)]<-'None'
###TO Releases###
relt<-summarize(group_by(rTo@data,HUC8Name,interbasin),waterin=sum(tsvalue/365,na.rm=T))
relt<-relt[relt$interbasin==1,]
relt$HUC8Name<-as.character(relt$HUC8Name)
relt$HUC8Name[is.na(relt$HUC8Name)]<-'None'
###########################################################################################################################################
####Calculate Net Transfers for Each HUC8 Watershed####
#Loop through each HUC 10 and check for summed releases and deliveries
#Water out is defined as the "from's" and Water in are the "to's"
#This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)
HUC8@data$waterin<-NA
HUC8@data$waterout<-NA

for (i in 1:length(HUC8@data$HUC8)){
  if(length(delf$waterout[delf$HUC8Name==HUC8@data$Name[i]])>0){
    HUC8@data$waterout[i]<-delf$waterout[delf$HUC8Name==HUC8@data$Name[i]]
  }
  if(length(delt$waterin[delt$HUC8Name==HUC8@data$Name[i]])>0){
    HUC8@data$waterin[i]<-delt$waterin[delt$HUC8Name==HUC8@data$Name[i]]
  }
  if(length(relf$waterout[relf$HUC8Name==HUC8@data$Name[i]])>0){
    HUC8@data$waterout[i]<-HUC8@data$waterout[i]+relf$waterout[relf$HUC8Name==HUC8@data$Name[i]]
  }
  if(length(relt$waterin[relt$HUC8Name==HUC8@data$Name[i]])>0){
    HUC8@data$waterin[i]<-HUC8@data$waterin[i]+relt$waterin[relt$HUC8Name==HUC8@data$Name[i]]
  }
  
  HUC8@data$transfer[i]<-plus(c(HUC8@data$waterin[i],-HUC8@data$waterout[i]))
}
HUC8_Transfers<-data.frame(HUC8_Name=HUC8@data$Name,HUC8=HUC8@data$HUC8,Transfers_MGD=HUC8@data$transfer)
HUC8_Transfers<-HUC8_Transfers[order(HUC8_Transfers$Transfers_MGD,decreasing=T),]

###########################################################################################################################################
####################################################Calculating Discharges#################################################################

#####Load in Discharge Data####
#Load in .csv file with monthly discharge data from ECHO
ECHO_Discharge<-read.csv("H:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries.csv")


#Create ECHO_Discharge spatial dataframe of all facilities with real geometry
ECHO_Discharge<-ECHO_Discharge[!(is.na(ECHO_Discharge$lat)&is.na(ECHO_Discharge$lon)),]
ECHO_Discharge<-SpatialPointsDataFrame(data.frame(lon=ECHO_Discharge$lon,lat=ECHO_Discharge$lat),ECHO_Discharge,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
ECHO_Discharge@data$FacilityName<-as.character(ECHO_Discharge@data$FacilityName)
ECHO_Discharge@data$MedFlow.MK_plus<-as.numeric(as.character(ECHO_Discharge@data$MedFlow.MK_plus))

####Overlay with HUC 10 Watershed Shapefile####
HUC8_Facilities<-over(ECHO_Discharge,HUC8_Overlay)
ECHO_Discharge@data$HUC8<-HUC8_Facilities$HUC8
ECHO_Discharge@data$HUC8Name<-HUC8_Facilities$HUC8Name

####Sum Discharges in HUC 10 Watersheds####
HUC8_Discharges<-as.data.frame(summarize(group_by(ECHO_Discharge@data,HUC8Name),Discharge=plus(MedFlow.MK_plus)))

#Check to flag ECHO flows that are greater than VPDES Permitted Design Flow 
length(which((ECHO_Discharge@data$MedFlow.MK_plus>ECHO_Discharge@data$VPDES_DesFlow)=="TRUE"))

###########################################################################################################################################
####################################################Calculating Withdrawals################################################################

VWUDS<-read.csv("G:/My Drive/GRA/VWUDS Data/")

#Convert these facilities into a spatial dataframe and overlay with HUC 8s. Then, summarize the data by HUC 8.
VWUDS<-read.csv("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/2017 ECHO/VAHydro_2017FacTable.csv")
#Create a spatial dataframe of all facilities with real geometry
VWUDS<-VWUDS[!(is.na(VWUDS$Lat)&is.na(VWUDS$Long)),]
VWUDS<-SpatialPointsDataFrame(data.frame(lon=VWUDS$Long,lat=VWUDS$Lat),VWUDS,proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
VWUDS<-spTransform(VWUDS,ECHOCRS)
VWUDS@data$Sum<-as.numeric(VWUDS@data$Sum)
#Overlay with HUC names
VAHydHUC<-over(VWUDS,HUC8Overlay)
VWUDS@data$HUC8<-VAHydHUC$HUC8
VWUDS@data$HUC8Name<-VAHydHUC$HUC8Name
#Summarize by HUC to find the total withdrawal occurring in each HUC
d<-as.data.frame(summarize(group_by(VWUDS@data,HUC8Name),Withdraw=plus(Sum)))
d$HUC8Name<-as.character(d$HUC8Name);c$HUC8Name<-as.character(c$HUC8Name)

#Convert the withdraw from MGY to MGD and then find total consumptive use in each
#HUC adjusting for the transfer values set above. Block use into classes and assign a 
#color for plotting
for (i in 1:length(HUC8@data$HUC8)){
  HUC8@data$Withdraw[i]<-d$Withdraw[d$HUC8Name==HUC8@data$Name[i]][1]/365 #converts MGY to MGD
  HUC8@data$Discharge[i]<-c$Discharge[c$HUC8Name==HUC8@data$Name[i]][1]
  if(is.na(HUC8@data$Withdraw[i])){
    HUC8@data$Withdraw[i]<-0
  }
  if(is.na(HUC8@data$Discharge[i])){
    HUC8@data$Discharge[i]<-0
  }
}

#Need to ignore NA values in transfers during computation
HUC8@data$Use<-HUC8@data$Discharge+ifelse(is.na(HUC8@data$transfer),0,HUC8@data$transfer)-HUC8@data$Withdraw

for (i in 1:length(HUC8@data$TNMID)){
  if(!is.na(HUC8@data$Use[i])){
    if(HUC8@data$Use[i]<(-100)){
      HUC8@data$UseClass[i]<-9
      HUC8@data$Color[i]<-"red"
    }else if(HUC8@data$Use[i]<(-10)){
      HUC8@data$UseClass[i]<-8
      HUC8@data$Color[i]<-"orange"
    }else if(HUC8@data$Use[i]<(-1)){
      HUC8@data$UseClass[i]<-7
      HUC8@data$Color[i]<-"yellow"
    }else if(HUC8@data$Use[i]<(0)){
      HUC8@data$UseClass[i]<-6
      HUC8@data$Color[i]<-"green"
    }else if(HUC8@data$Use[i]>(100)){
      HUC8@data$UseClass[i]<-5
      HUC8@data$Color[i]<-"darkblue"
    }else if(HUC8@data$Use[i]>(10)){
      HUC8@data$UseClass[i]<-4
      HUC8@data$Color[i]<-"blue"
    }else if(HUC8@data$Use[i]>(1)){
      HUC8@data$UseClass[i]<-3
      HUC8@data$Color[i]<-"deepskyblue"
    }else if(HUC8@data$Use[i]>(0)){
      HUC8@data$UseClass[i]<-2
      HUC8@data$Color[i]<-"cadetblue1"
    }else{
      HUC8@data$UseClass[i]<-1
      HUC8@data$Color[i]<-"black"
    } 
  }
}


HUC8_Consumptive_Use<-data.frame(HUC8_Name=HUC8@data$Name, HUC8_Code=HUC8@data$HUC8,  Discharge=HUC8@data$Discharge,
                                 Withdrawal=HUC8@data$Withdraw, Transfers=HUC8@data$transfer,Consumptive_Use=HUC8@data$Use)

HUC8_Consumptive_Use<-HUC8_Consumptive_Use[order(HUC8_Consumptive_Use$Consumptive_Use,decreasing=F),]

#write.csv(HUC8_Consumptive_Use,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/2017 ECHO/HUC8 Consumptive Use VPDES Referenced.csv")

######PLOT##############

#Plot consumptive use by first assigning data to the cropped HUC boundary and then by running the plot and legend commands
HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")

#Colorramp<-colorRampPalette(c("blue","red"))
#HUC8@data$Color<-Colorramp(7)[as.numeric(cut(HUC8@data$Use, seq(min(HUC8@data$Use),max(HUC8@data$Use),by=50)))]

leaflet(HUC8_Clipped)
qpal

plot(HUC8_Clipped,col=HUC8@data$Color, axes=T, cex.axis=1)

lines(HUC8_Clipped,col='black')

legend("topleft", c("< -100 MGD", "-100 - -10 MGD", "-10 - -1 MGD", "-1 - 0 MGD","0 - 1 MGD", "1 - 10 MGD","10 - 100 MGD","> 100 MGD","NA"),
       col =c("red","orange","yellow","green","cadetblue1","deepskyblue", "blue", "darkblue", "black"), pch=15,pt.cex=4,lty=0,bty='n',y.intersp = 2,
       x.intersp = 1.5,cex=1,lwd=1,seg.len=0.3)
title(main="HUC 8 Consumptive Use: Replaced Flow if ECHO>VPDES")


####Summary Statistics####
#Show summed discharge and withdraw statewide
sum(HUC8_Clipped$Discharge,na.rm=T);sum(HUC8_Clipped$Withdraw,na.rm=T)

#Number of HUC8 watersheds with more withdrawals than discharge
sum(HUC8@data$Use<0,na.rm = T)

#Number of HUC 8 watershdes with more discharge than withdrawals
sum(HUC8@data$Use>0,na.rm=T)

#Number of HUC 8 Watersheds with no reported withdrawals/discharges
sum(HUC8@data$Use==0,na.rm=T)
####################################################################
#QA/QC Methods

#ECHO>*withdrawal

ECHO_Exceeding_Withdrawals<-data.frame(HUC8Name=HUC8@data$Name[which((HUC8@data$Discharge>0.3*HUC8@data$Withdraw)=="TRUE")])

which((AllFacs$MedFlow.MK_plus>0.3*HUC8@data$Withdraw)=="TRUE")
