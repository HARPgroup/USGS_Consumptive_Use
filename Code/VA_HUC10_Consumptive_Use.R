###########################################################################################################################################
#########################Estimating Consumptive Use in Virginia HUC 10 Watersheds##########################################################

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
####HUC10 and Virginia Shapefile Manipulation####

#Load databases and extract required layers
HUC10<-readOGR("C:/Users/Morgan/Documents/VT/GRA/HUC.gdb",layer='WBDHU10')
VA<-readOGR('C:/Users/Morgan/Documents/VT/GRA/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC10<-spTransform(HUC10, CRS("+init=epsg:4269"))
VA<-spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC10_Clipped<-gIntersection(HUC10,VA,id=as.character(HUC10@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
#plot(HUC10_Clipped)

#Create HUC10 Dataframe that will be used in future overlay processes
HUC10_Overlay<-HUC10 #Keep integrity of spatial dataframe
HUC10_Overlay@data<-HUC10_Overlay@data[,c(11,12)] 
names(HUC10_Overlay)<-c("HUC10","HUC10Name")

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

####Create Spatial Data Frame from Points and Overlay on Clipped HUC10 Shapefile####
####FROM Delivery Transfers####
#Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
#Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
dFrom<-transTo[!(is.na(transTo$geomFlat)&is.na(transTo$geomFlon)),]
dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
HUC10_Facilities<-over(dFrom,HUC10_Overlay)#Spatial overlay
dFrom@data$HUC10<-HUC10_Facilities$HUC10
dFrom@data$HUC10Name<-HUC10_Facilities$HUC10Name

####TO Delivery Transfers####
#Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
#Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
dTo<-transTo[!(is.na(transTo$geomTlat)&is.na(transTo$geomTlon)),]
dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
HUC10_Facilities<-over(dTo,HUC10_Overlay)#Spatial overlay
dTo@data$HUC10<-HUC10_Facilities$HUC10
dTo@data$HUC10Name<-HUC10_Facilities$HUC10Name

####Determine if TO Transfers are leaving watershed boundaries####
#Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
#We are only concerned with interbasin transfers and need to identify these with the following code
dTo@data$interbasin<-NA
dFrom@data$interbasin<-NA
#Check each transfer in the delivery VA Hydro transfers to see if its FROM HUC is different than its TO HUC
#If transfers occurred outside of HUC boundaries, set HUC as "None"
for (i in 1:length(dTo@data$hydroid)){
  ToHUC<-as.character(dTo@data$HUC10[i])
  if(is.na(ToHUC)){
    ToHUC<-'None'
  }
  FromHUC<-as.character(dFrom@data$HUC10[dFrom@data$hydroid==dTo@data$hydroid[i]])
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

####Sum Net Water In and Out for each HUC10 Watershed####
###FROM Deliveries###
delf<-summarize(group_by(dFrom@data,HUC10Name,interbasin),waterout=sum(tsvalue/365,na.rm=T)) #units in MGY need in MGD
delf<-delf[delf$interbasin==1,]
delf$HUC10Name<-as.character(delf$HUC10Name)
delf$HUC10Name[is.na(delf$HUC10Name)]<-'None'
###TO Deliveries###
delt<-summarize(group_by(dTo@data,HUC10Name,interbasin),waterin=sum(tsvalue/365,na.rm=T))
delt<-delt[delt$interbasin==1,]
delt$HUC10Name<-as.character(delt$HUC10Name)
delt$HUC10Name[is.na(delt$HUC10Name)]<-'None'
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

####Create Spatial Data Frame from Points and Overlay on Clipped HUC10 Shapefile####
####FROM Release Transfers####
rFrom<-transFrom[!(is.na(transFrom$geomFlat)&is.na(transFrom$geomFlon)),]
rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
HUC10_Facilities<-over(rFrom,HUC10_Overlay)
rFrom@data$HUC10<-HUC10_Facilities$HUC10
rFrom@data$HUC10Name<-HUC10_Facilities$HUC10Name
####TO Release Transfers####
rTo<-transFrom[!(is.na(transFrom$geomTlat)&is.na(transFrom$geomTlon)),]
rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
HUC10_Facilities<-over(rTo,HUC10_Overlay)
rTo@data$HUC10<-HUC10_Facilities$HUC10
rTo@data$HUC10Name<-HUC10_Facilities$HUC10Name

####Determine if Release FROM Transfers are leaving watershed boundaries####
rTo@data$interbasin<-NA
rFrom@data$interbasin<-NA
for (i in 1:length(rTo@data$hydroid)){
  ToHUC<-as.character(rTo@data$HUC10[i])
  if(is.na(ToHUC)){
    ToHUC<-'None'
  }
  FromHUC<-as.character(rFrom@data$HUC10[rFrom@data$hydroid==rTo@data$hydroid[i]])
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

####Sum Net Water In and Out for each HUC10 Watershed####
###FROM Releases###
relf<-summarize(group_by(rFrom@data,HUC10Name,interbasin),waterout=sum(tsvalue/365,na.rm=T))
relf<-relf[relf$interbasin==1,]
relf$HUC10Name<-as.character(relf$HUC10Name)
relf$HUC10Name[is.na(relf$HUC10Name)]<-'None'
###TO Releases###
relt<-summarize(group_by(rTo@data,HUC10Name,interbasin),waterin=sum(tsvalue/365,na.rm=T))
relt<-relt[relt$interbasin==1,]
relt$HUC10Name<-as.character(relt$HUC10Name)
relt$HUC10Name[is.na(relt$HUC10Name)]<-'None'
###########################################################################################################################################
####Calculate Net Transfers for Each HUC10 Watershed####
#Loop through each HUC 10 and check for summed releases and deliveries
#Water out is defined as the "from's" and Water in are the "to's"
#This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)
HUC10@data$waterin<-NA
HUC10@data$waterout<-NA

for (i in 1:length(HUC10@data$HUC10)){
  if(length(delf$waterout[delf$HUC10Name==HUC10@data$Name[i]])>0){
    HUC10@data$waterout[i]<-delf$waterout[delf$HUC10Name==HUC10@data$Name[i]]
  }
  if(length(delt$waterin[delt$HUC10Name==HUC10@data$Name[i]])>0){
    HUC10@data$waterin[i]<-delt$waterin[delt$HUC10Name==HUC10@data$Name[i]]
  }
  if(length(relf$waterout[relf$HUC10Name==HUC10@data$Name[i]])>0){
    HUC10@data$waterout[i]<-HUC10@data$waterout[i]+relf$waterout[relf$HUC10Name==HUC10@data$Name[i]]
  }
  if(length(relt$waterin[relt$HUC10Name==HUC10@data$Name[i]])>0){
    HUC10@data$waterin[i]<-HUC10@data$waterin[i]+relt$waterin[relt$HUC10Name==HUC10@data$Name[i]]
  }
  
  HUC10@data$transfer[i]<-plus(c(HUC10@data$waterin[i],-HUC10@data$waterout[i]))
}
HUC10_Transfers<-data.frame(HUC10_Name=HUC10@data$Name,HUC10=HUC10@data$HUC10,Transfers_MGD=HUC10@data$transfer)
HUC10_Transfers<-HUC10_Transfers[order(HUC10_Transfers$Transfers_MGD,decreasing=T),]

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
HUC10_Facilities<-over(ECHO_Discharge,HUC10_Overlay)
ECHO_Discharge@data$HUC10<-HUC10_Facilities$HUC10
ECHO_Discharge@data$HUC10Name<-HUC10_Facilities$HUC10Name

####Sum Discharges in HUC 10 Watersheds####
HUC10_Discharges<-as.data.frame(summarize(group_by(ECHO_Discharge@data,HUC10Name),Discharge=plus(MedFlow.MK_plus)))

#Check to flag ECHO flows that are greater than VPDES Permitted Design Flow 
length(which((ECHO_Discharge@data$MedFlow.MK_plus>ECHO_Discharge@data$VPDES_DesFlow)=="TRUE"))

###########################################################################################################################################
####################################################Calculating Withdrawals################################################################

VWUDS<-read.csv("G:/My Drive/GRA/VWUDS Data/")
