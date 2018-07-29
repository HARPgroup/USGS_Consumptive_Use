###########################################################################################################################################
#########################Estimating Consumptive Use in Virginia HUC 12 Watersheds##########################################################

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
####HUC12 and Virginia Shapefile Manipulation####

#Load databases and extract required layers
HUC12<-readOGR("C:/Users/Morgan/Documents/VT/GRA/HUC.gdb",layer='WBDHU12')
VA<-readOGR('C:/Users/Morgan/Documents/VT/GRA/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC12<-spTransform(HUC12, CRS("+init=epsg:4269"))
VA<-spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC12_Clipped<-gIntersection(HUC12,VA,id=as.character(HUC12@data$HUC12),byid=TRUE,drop_lower_td=TRUE)
#plot(HUC12_Clipped)

#Create HUC12 Dataframe that will be used in future overlay processes
HUC12_Overlay<-HUC12 #Keep integrity of spatial dataframe
HUC12_Overlay@data<-HUC12_Overlay@data[,c(11,12)] 
names(HUC12_Overlay)<-c("HUC12","HUC12Name")

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

####Create Spatial Data Frame from Points and Overlay on Clipped HUC12 Shapefile####
####FROM Delivery Transfers####
#Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
#Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
dFrom<-transTo[!(is.na(transTo$geomFlat)&is.na(transTo$geomFlon)),]
dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
HUC12_Facilities<-over(dFrom,HUC12_Overlay)#Spatial overlay
dFrom@data$HUC12<-HUC12_Facilities$HUC12
dFrom@data$HUC12Name<-HUC12_Facilities$HUC12Name

####TO Delivery Transfers####
#Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
#Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
dTo<-transTo[!(is.na(transTo$geomTlat)&is.na(transTo$geomTlon)),]
dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
HUC12_Facilities<-over(dTo,HUC12_Overlay)#Spatial overlay
dTo@data$HUC12<-HUC12_Facilities$HUC12
dTo@data$HUC12Name<-HUC12_Facilities$HUC12Name

####Determine if TO Transfers are leaving watershed boundaries####
#Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
#We are only concerned with interbasin transfers and need to identify these with the following code
dTo@data$interbasin<-NA
dFrom@data$interbasin<-NA
#Check each transfer in the delivery VA Hydro transfers to see if its FROM HUC is different than its TO HUC
#If transfers occurred outside of HUC boundaries, set HUC as "None"
for (i in 1:length(dTo@data$hydroid)){
  ToHUC<-as.character(dTo@data$HUC12[i])
  if(is.na(ToHUC)){
    ToHUC<-'None'
  }
  FromHUC<-as.character(dFrom@data$HUC12[dFrom@data$hydroid==dTo@data$hydroid[i]])
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

####Sum Net Water In and Out for each HUC12 Watershed####
###FROM Deliveries###
delf<-summarize(group_by(dFrom@data,HUC12Name,interbasin),waterout=sum(tsvalue/365,na.rm=T)) #units in MGY need in MGD
delf<-delf[delf$interbasin==1,]
delf$HUC12Name<-as.character(delf$HUC12Name)
delf$HUC12Name[is.na(delf$HUC12Name)]<-'None'
###TO Deliveries###
delt<-summarize(group_by(dTo@data,HUC12Name,interbasin),waterin=sum(tsvalue/365,na.rm=T))
delt<-delt[delt$interbasin==1,]
delt$HUC12Name<-as.character(delt$HUC12Name)
delt$HUC12Name[is.na(delt$HUC12Name)]<-'None'
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

####Create Spatial Data Frame from Points and Overlay on Clipped HUC12 Shapefile####
####FROM Release Transfers####
rFrom<-transFrom[!(is.na(transFrom$geomFlat)&is.na(transFrom$geomFlon)),]
rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
HUC12_Facilities<-over(rFrom,HUC12_Overlay)
rFrom@data$HUC12<-HUC12_Facilities$HUC12
rFrom@data$HUC12Name<-HUC12_Facilities$HUC12Name
####TO Release Transfers####
rTo<-transFrom[!(is.na(transFrom$geomTlat)&is.na(transFrom$geomTlon)),]
rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
HUC12_Facilities<-over(rTo,HUC12_Overlay)
rTo@data$HUC12<-HUC12_Facilities$HUC12
rTo@data$HUC12Name<-HUC12_Facilities$HUC12Name

####Determine if Release FROM Transfers are leaving watershed boundaries####
rTo@data$interbasin<-NA
rFrom@data$interbasin<-NA
for (i in 1:length(rTo@data$hydroid)){
  ToHUC<-as.character(rTo@data$HUC12[i])
  if(is.na(ToHUC)){
    ToHUC<-'None'
  }
  FromHUC<-as.character(rFrom@data$HUC12[rFrom@data$hydroid==rTo@data$hydroid[i]])
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

####Sum Net Water In and Out for each HUC12 Watershed####
###FROM Releases###
relf<-summarize(group_by(rFrom@data,HUC12Name,interbasin),waterout=sum(tsvalue/365,na.rm=T))
relf<-relf[relf$interbasin==1,]
relf$HUC12Name<-as.character(relf$HUC12Name)
relf$HUC12Name[is.na(relf$HUC12Name)]<-'None'
###TO Releases###
relt<-summarize(group_by(rTo@data,HUC12Name,interbasin),waterin=sum(tsvalue/365,na.rm=T))
relt<-relt[relt$interbasin==1,]
relt$HUC12Name<-as.character(relt$HUC12Name)
relt$HUC12Name[is.na(relt$HUC12Name)]<-'None'
###########################################################################################################################################
####Calculate Net Transfers for Each HUC12 Watershed####
#Loop through each HUC 10 and check for summed releases and deliveries
#Water out is defined as the "from's" and Water in are the "to's"
#This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)
HUC12@data$waterin<-NA
HUC12@data$waterout<-NA

for (i in 1:length(HUC12@data$HUC12)){
  if(length(delf$waterout[delf$HUC12Name==HUC12@data$Name[i]])>0){
    HUC12@data$waterout[i]<-delf$waterout[delf$HUC12Name==HUC12@data$Name[i]]
  }
  if(length(delt$waterin[delt$HUC12Name==HUC12@data$Name[i]])>0){
    HUC12@data$waterin[i]<-delt$waterin[delt$HUC12Name==HUC12@data$Name[i]]
  }
  if(length(relf$waterout[relf$HUC12Name==HUC12@data$Name[i]])>0){
    HUC12@data$waterout[i]<-HUC12@data$waterout[i]+relf$waterout[relf$HUC12Name==HUC12@data$Name[i]]
  }
  if(length(relt$waterin[relt$HUC12Name==HUC12@data$Name[i]])>0){
    HUC12@data$waterin[i]<-HUC12@data$waterin[i]+relt$waterin[relt$HUC12Name==HUC12@data$Name[i]]
  }
  
  HUC12@data$transfer[i]<-plus(c(HUC12@data$waterin[i],-HUC12@data$waterout[i]))
}
HUC12_Transfers<-data.frame(HUC12_Name=HUC12@data$Name,HUC12=HUC12@data$HUC12,Transfers_MGD=HUC12@data$transfer)
HUC12_Transfers<-HUC12_Transfers[order(HUC12_Transfers$Transfers_MGD,decreasing=T),]

###########################################################################################################################################
####################################################Calculating Discharges#################################################################

#####Load in Discharge Data####
#Load in .csv file with monthly discharge data from ECHO
ECHO_Discharge<-read.csv("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/2017 ECHO/ECHO_Discharge_2017.csv")
#Create ECHO_Discharge spatial dataframe of all facilities with real geometry
ECHO_Discharge<-ECHO_Discharge[!(is.na(ECHO_Discharge$lat)&is.na(ECHO_Discharge$lon)),]
ECHO_Discharge<-SpatialPointsDataFrame(data.frame(lon=ECHO_Discharge$lon,lat=ECHO_Discharge$lat),ECHO_Discharge,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
ECHO_Discharge@data$FacilityName<-as.character(ECHO_Discharge@data$FacilityName)
ECHO_Discharge@data$MedFlow.MK_plus<-as.numeric(as.character(ECHO_Discharge@data$MedFlow.MK_plus))

####Overlay with HUC 10 Watershed Shapefile####
HUC12_Facilities<-over(ECHO_Discharge,HUC12_Overlay)
ECHO_Discharge@data$HUC12<-HUC12_Facilities$HUC12
ECHO_Discharge@data$HUC12Name<-HUC12_Facilities$HUC12Name

####Sum Discharges in HUC 10 Watersheds####
HUC12_Discharges<-as.data.frame(summarize(group_by(ECHO_Discharge@data,HUC12Name),Discharge=plus(MedFlow.MK_plus)))

#Check to flag ECHO flows that are greater than VPDES Permitted Design Flow 
length(which((ECHO_Discharge@data$MedFlow.MK_plus>ECHO_Discharge@data$VPDES_DesFlow)=="TRUE"))

###########################################################################################################################################
####################################################Calculating Withdrawals################################################################

VWUDS<-read.csv("G:/My Drive/GRA/VWUDS Data/")
