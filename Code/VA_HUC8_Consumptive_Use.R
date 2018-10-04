###########################################################################################################################################
#########################Estimating Consumptive Use in Virginia HUC 8 Watersheds##########################################################

##This code calculates consumptive use by performing a water balance (In-Out=Change in Storage)
#Inputs are defined as the discharges/return flows from NPDES permitted facilities (ECHO Database)
#Outputs are the withdrawals from surface water found in the Virginia Wateruse Data System (VWUDS)
#The change in storage refers to transfers between facilities (releases and deliveries)--This is calculated in this script

###########################################################################################################################################
####House Keeping####

#Load required packages-including spatial analysis
library(httr)
library(plyr)
library(rgdal)#extract files from file geodatabases-like in ArcMap
library(rgeos)
library(raster)
library(ggplot2)
library(devtools)
#devtools::install_github("dgrtwo/gganimate")
library(installr)
#install.ImageMagick()
library(gganimate)
library(sf)
library(plotly)
library(broom)
library(gtools)
library(mapproj)
library(foreign) #allows for easy manipulation of *.dbf file types
library(dplyr)#data manipulation package that speeds up grouping, summarizing, ordering. 
library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(readxl) #reads excel files
library(jsonlite)
library(proj4)
library(data.table)
library(stringr)
library(shiny)
library(lubridate)
library(RColorBrewer)
library(tibble)

options(scipen=999) #Disable scientific notation
options(digits = 9)
memory.limit(size=100000000)

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
HUC8<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/HUC.gdb",layer='WBDHU8')
VA<-readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC8<-spTransform(HUC8, CRS("+init=epsg:4269"))
VA<-spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)

#Create HUC8 Dataframe that will be used in future overlay processes
HUC8_Overlay<-HUC8 #Keep integrity of spatial dataframe
HUC8_Overlay@data<-HUC8_Overlay@data[,c(11,12)] 
names(HUC8_Overlay)<-c("HUC8","HUC8Name")

###########################################################################################################################################
###########################################Calculating Transfers###########################################################################
#There are two different types of Transfers: Deliveries and Releases (both have TO and FROM components)
#This is because VA Hydro stores transfers at both the recieving and departing facilities
#Therefore, we will keep them separate during calculations and refer to transfers as deliveries and releases in both the TO and FROM lists

###########################################################################################################################################
####Delivery Transfers (TO)####
#Monthly Deliveries from 01/01/2010-12/31/2017
#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_4
GET('http://deq1.bse.vt.edu/d.bet/echo_transfer_to',
    write_disk(temp <- tempfile(fileext = ".csv")))
deliveries<-read.csv(temp)
deliveries$geom<-as.character(deliveries$geom)
deliveries$Year<-substring(deliveries$tstime,1,4)

####Reformat Coordinates####
#Transfer geometry comes in the form "LINESTRING(TO LONG TO LAT, FROM LONG FROM LAT)
#The following lines extract the to and from geometry and store them in appropriate columns
deliveries$geomFlat<-gsub(".*[(]([^,]+),\\s.*","\\1",deliveries$geom) 
deliveries$geomFlon<-as.numeric(gsub(" [^ ]*$","\\1",deliveries$geomFlat)) #FROM
deliveries$geomFlat<-as.numeric(gsub("^[^ ]* ","\\1",deliveries$geomFlat)) #FROM
deliveries$geomTlat<-gsub(".*[,] ([^)]+)).*","\\1",deliveries$geom) 
deliveries$geomTlon<-as.numeric(gsub(" [^ ]*$","\\1",deliveries$geomTlat)) #TO
deliveries$geomTlat<-as.numeric(gsub("^[^ ]* ","\\1",deliveries$geomTlat)) #TO

####Create Spatial Data Frame from Points and Overlay on Clipped HUC8 Shapefile####
####FROM Delivery Transfers####
#Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
#Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
dFrom<-deliveries[!(is.na(deliveries$geomFlat)&is.na(deliveries$geomFlon)),]
dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
HUC8_Facilities<-over(dFrom,HUC8_Overlay)#Spatial overlay
dFrom@data$HUC8<-HUC8_Facilities$HUC8
dFrom@data$HUC8Name<-HUC8_Facilities$HUC8Name

####TO Delivery Transfers####
#Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
#Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
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

for (i in 1:length(dTo@data$hydroid)){
  ToHUC<-as.character(dTo@data$HUC8[i])
  if(is.na(ToHUC)){
    ToHUC<-'Null HUC8'
  }
  FromHUC<-as.character(dFrom@data$HUC8[dFrom@data$hydroid==dTo@data$hydroid[i]]) 
  if(is.na(FromHUC)){
    FromHUC<-'Null HUC8' 
  }
  interbasin<-0
  if(ToHUC!=FromHUC){ #if the HUC code does not match, mark as interbasin delivery
    interbasin<-1
  }
  dTo@data$interbasin[i]<-interbasin
  dFrom@data$interbasin[i]<-interbasin
}

####Sum Net Water In and Out for each HUC8 Watershed####
###FROM Deliveries###
delf<-dFrom@data
delf<-delf[delf$interbasin==1,] #if interbasin is equal to 1, it is crossing boundaries---so just include those

delf<-delf%>%
  dplyr::group_by(HUC8Name, Year)%>%
  summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))#units in MGM to MGD

delf$HUC8Name<-as.character(delf$HUC8Name)
delf$HUC8Name[is.na(delf$HUC8Name)]<-'Fell Outside HUC8 Limits'

###TO Deliveries###
delt<-dTo@data
delt<-delt[delt$interbasin==1,] #narrow down to deliveries happening across borders

delt<-
  delt%>%
  group_by(HUC8Name, Year)%>%
  summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))#units in MGM to MGD


delt$HUC8Name<-as.character(delt$HUC8Name)
delt$HUC8Name[is.na(delt$HUC8Name)]<-'Fell Outside HUC8 Limits'
###########################################################################################################################################
####Release Transfers (FROM)####
#Ignoring repeat hydroids (same transfer reported from both recieving and transfer facility),
#repeat the above steps using the remaining FROM transfers available at:
#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_5
GET('http://deq1.bse.vt.edu/d.bet/echo_transfer_from',write_disk(temp <- tempfile(fileext = ".csv")))
releases<-read.csv(temp)
releases$geom<-as.character(releases$geom)
releases<-releases[!(releases$hydroid%in%deliveries$hydroid),] #removing redundant data that exists in TO Transfer data frame
releases$Year<-substring(releases$tstime,1,4)


####Reformat Coordinates####
#Transfer geometry comes in the form "LINESTRING(TO LONG TO LAT, FROM LONG FROM LAT)
#The following lines extract the to and from geometry and store them in appropriate columns
releases$geomFlat<-gsub(".*[(]([^,]+),\\s.*","\\1",releases$geom)
releases$geomFlon<-as.numeric(gsub(" [^ ]*$","\\1",releases$geomFlat))
releases$geomFlat<-as.numeric(gsub("^[^ ]* ","\\1",releases$geomFlat))
releases$geomTlat<-gsub(".*[,] ([^)]+)).*","\\1",releases$geom)
releases$geomTlon<-as.numeric(gsub(" [^ ]*$","\\1",releases$geomTlat))
releases$geomTlat<-as.numeric(gsub("^[^ ]* ","\\1",releases$geomTlat))

####Create Spatial Data Frame from Points and Overlay on Clipped HUC8 Shapefile####
####FROM Release Transfers####
rFrom<-releases[!(is.na(releases$geomFlat)&is.na(releases$geomFlon)),]
rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
HUC8_Facilities<-over(rFrom,HUC8_Overlay)
rFrom@data$HUC8<-HUC8_Facilities$HUC8
rFrom@data$HUC8Name<-HUC8_Facilities$HUC8Name
####TO Release Transfers####
rTo<-releases[!(is.na(releases$geomTlat)&is.na(releases$geomTlon)),]
rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
HUC8_Facilities<-over(rTo,HUC8_Overlay)
rTo@data$HUC8<-HUC8_Facilities$HUC8
rTo@data$HUC8Name<-HUC8_Facilities$HUC8Name

####Determine if Release FROM Transfers are leaving watershed boundaries####
rTo@data$interbasin<-NA
rFrom@data$interbasin<-NA

for (i in 1:length(rTo@data$hydroid)){
  ToHUC<-as.character(rTo@data$HUC8[i])
  if(is.na(ToHUC)){ #if the HUC code is NA
    ToHUC<-'Null HUC8'
  }
  FromHUC<-as.character(rFrom@data$HUC8[rFrom@data$hydroid==rTo@data$hydroid[i]])
  if(is.na(FromHUC)){
    FromHUC<-'Null HUC8' 
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
relf<-rFrom@data
relf<-relf[relf$interbasin==1,]#remember intratransfers are indicated with a 0

relf<-relf%>% #Summarise by HUC8 and year
  group_by(HUC8Name,Year)%>%
  summarise(HUC8=first(HUC8), interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))  #MGM to MGD

relf$HUC8Name<-as.character(relf$HUC8Name)
relf$HUC8Name[is.na(relf$HUC8Name)]<-'Fell Outside HUC8 Limits'

###TO Releases###
relt<-rTo@data
relt<-relt[relt$interbasin==1,]

relt<-relt%>%
  group_by(HUC8Name,Year)%>%
  summarise(HUC8=first(HUC8), interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))

relt$HUC8Name<-as.character(relt$HUC8Name)
relt$HUC8Name[is.na(relt$HUC8Name)]<-'Fell Outside HUC8 Limits'
###########################################################################################################################################
####Calculate Net Transfers for Each HUC8 Watershed####
#Loop through each HUC 8 and check for summed releases and deliveries
#Water out is defined as the "from's" and Water in are the "to's"
#This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

HUC8_waterout<-as.data.table(rbind(relf,delf))
HUC8_waterout<-HUC8_waterout[, lapply(.SD,sum), by=list(HUC8Name, Year, HUC8), .SDcols=c(4,5)]

HUC8_waterin<-as.data.table(rbind(relt,delt))
HUC8_waterin<-HUC8_waterin[, lapply(.SD,sum), by=list(HUC8Name, Year, HUC8), .SDcols=c(4,5)]

#---Year 2010---#
HUC8@data$waterout_2010<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2010"],
                                -HUC8_waterout$waterout[HUC8_waterout$Year=="2010"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2010"])],NA)
HUC8@data$waterin_2010<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2010"],
                               HUC8_waterin$waterin[HUC8_waterin$Year=="2010"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2010"])],NA)
HUC8@data$transferred_2010<- (rowSums(HUC8@data[,(15:16)],na.rm=T))

#---Year 2011---#
HUC8@data$waterout_2011<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2011"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2011"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2011"])],NA)
HUC8@data$waterin_2011<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2011"],HUC8_waterin$waterin[HUC8_waterin$Year=="2011"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2011"])],NA)
HUC8@data$transferred_2011<- (rowSums(HUC8@data[,(18:19)],na.rm=T))

#---Year 2012---#
HUC8@data$waterout_2012<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2012"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2012"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2012"])],NA)
HUC8@data$waterin_2012<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2012"],HUC8_waterin$waterin[HUC8_waterin$Year=="2012"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2012"])],NA)
HUC8@data$transferred_2012<- (rowSums(HUC8@data[,(21:22)],na.rm=T))

#---Year 2013---#
HUC8@data$waterout_2013<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2013"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2013"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2013"])],NA)
HUC8@data$waterin_2013<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2013"],HUC8_waterin$waterin[HUC8_waterin$Year=="2013"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2013"])],NA)
HUC8@data$transferred_2013<- (rowSums(HUC8@data[,(24:25)],na.rm=T))

#---Year 2014---#
HUC8@data$waterout_2014<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2014"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2014"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2014"])],NA)
HUC8@data$waterin_2014<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2014"],HUC8_waterin$waterin[HUC8_waterin$Year=="2014"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2014"])],NA)
HUC8@data$transferred_2014<- (rowSums(HUC8@data[,(27:28)],na.rm=T))

#---Year 2015---#
HUC8@data$waterout_2015<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2015"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2015"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2015"])],NA)
HUC8@data$waterin_2015<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2015"],HUC8_waterin$waterin[HUC8_waterin$Year=="2015"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2015"])],NA)
HUC8@data$transferred_2015<- (rowSums(HUC8@data[,(30:31)],na.rm=T))

#---Year 2016---#
HUC8@data$waterout_2016<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2016"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2016"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2016"])],NA)
HUC8@data$waterin_2016<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2016"],HUC8_waterin$waterin[HUC8_waterin$Year=="2016"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2016"])],NA)
HUC8@data$transferred_2016<- (rowSums(HUC8@data[,(33:34)],na.rm=T))

#---Year 2017---#
HUC8@data$waterout_2017<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2017"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2017"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2017"])],NA)
HUC8@data$waterin_2017<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2017"],HUC8_waterin$waterin[HUC8_waterin$Year=="2017"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2017"])],NA)
HUC8@data$transferred_2017<- (rowSums(HUC8@data[,(36:37)],na.rm=T))


HUC8_Transfers<-data.frame(HUC8_Name=HUC8@data$Name,
                           HUC8=HUC8@data$HUC8,
                           Transfers_2010_MGD=HUC8@data$transferred_2010,
                           Transfers_2011_MGD=HUC8@data$transferred_2011,
                           Transfers_2012_MGD=HUC8@data$transferred_2012,
                           Transfers_2013_MGD=HUC8@data$transferred_2013,
                           Transfers_2014_MGD=HUC8@data$transferred_2014,
                           Transfers_2015_MGD=HUC8@data$transferred_2015,
                           Transfers_2016_MGD=HUC8@data$transferred_2016,
                           Transfers_2017_MGD=HUC8@data$transferred_2017)

HUC8_Transfers<-HUC8_Transfers[order(HUC8_Transfers$HUC8_Name,decreasing=F),]

#rm(delf,delt,dFrom,dTo,relf,relt,rFrom,rTo)
###########################################################################################################################################
####################################################Calculating Discharges#################################################################

#####Load in Discharge Data####
#Load in .txt file with DMR data from 2010-2017. 
ECHO_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC.txt", sep="\t", header=T)

ECHO_2010_2017%>%
  summarise(Facilities=n_distinct(Facility.ID),
  Outfalls=n_distinct(OutfallID),
  Summed_D=sum(Measured_Effluent,na.rm=T),
  Summed_D_QAQC=sum(Resolved_Measured_Effluent_NA,na.rm=T),
  NPDES=sum(Permit_Type=="NPDES Individual Permit"),
  GP=sum(Permit_Type=="General Permit Covered Facility"))

#Filtering out General Permits and Outfalls not ending in 001 
ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&str_sub(ECHO_2010_2017$OutfallID, start=-3)=="001")

#---Separate by Water Use Sector---# Go through individually---don't do scenario with transfers
ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Reclass_Use_Type=="Agriculture/Irrigation")
ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Reclass_Use_Type=="Commercial")
ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Reclass_Use_Type=="Energy")
ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Reclass_Use_Type=="Industrial")
ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Reclass_Use_Type=="Municipal")

#If Outfalls are missing coordinates, use facility coordinates
ECHO_2010_2017$Outfall_Longitude<-ifelse(is.na(ECHO_2010_2017$Outfall_Longitude),ECHO_2010_2017$Facility_Longitude,ECHO_2010_2017$Outfall_Longitude)
ECHO_2010_2017$Outfall_Latitude<-ifelse(is.na(ECHO_2010_2017$Outfall_Latitude),ECHO_2010_2017$Facility_Latitude,ECHO_2010_2017$Outfall_Latitude)

ECHO_2010_2017<-SpatialPointsDataFrame(data.frame(Outfall_Longitude=ECHO_2010_2017$Outfall_Longitude,Outfall_Latitude=ECHO_2010_2017$Outfall_Latitude),ECHO_2010_2017,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83

#----Set Data Type----#
ECHO_2010_2017@data$FacilityName<-as.character(ECHO_2010_2017@data$FacilityName)
ECHO_2010_2017@data$Resolved_Measured_Effluent_NA<-as.numeric(ECHO_2010_2017@data$Resolved_Measured_Effluent_NA)#after QA/QC
ECHO_2010_2017@data$Resolved_Measured_Effluent_Med<-as.numeric(ECHO_2010_2017@data$Resolved_Measured_Effluent_Med)#after QA/QC
ECHO_2010_2017@data$Measured_Effluent<-as.numeric(ECHO_2010_2017@data$Measured_Effluent)#Before QA/QC
ECHO_2010_2017.test<-as.data.frame(ECHO_2010_2017@data)

####Overlay with HUC 8 Watershed Shapefile####
HUC8_Facilities<-over(ECHO_2010_2017,HUC8_Overlay)
ECHO_2010_2017@data$HUC8<-HUC8_Facilities$HUC8
ECHO_2010_2017@data$HUC8Name<-HUC8_Facilities$HUC8Name

####Sum Discharges in HUC 10 Watersheds####
ECHO_Resol_Mean_NA<-ECHO_2010_2017.test%>%group_by(OutfallID)%>%summarise(Resolved_Mean_NA=mean(Resolved_Measured_Effluent_NA,na.rm=T))
ECHO_Resol_Mean_Med<-ECHO_2010_2017.test%>%group_by(OutfallID)%>%summarise(Resolved_Mean_Med=mean(Resolved_Measured_Effluent_Med,na.rm=T))
ECHO_2010_2017.test<-merge(ECHO_2010_2017.test,ECHO_Resol_Mean_NA,by="OutfallID",all.x=T)
ECHO_2010_2017.test<-merge(ECHO_2010_2017.test,ECHO_Resol_Mean_Med,by="OutfallID",all.x=T)

HUC8_Discharges<-ECHO_2010_2017@data%>%
                    group_by(HUC8Name,Year=substring(MP_End_Date,1,4))%>%
                      summarise(HUC8=first(HUC8),Discharge_MGD=plus(Resolved_Measured_Effluent_NA))

HUC8_Discharges_preQAQC<-ECHO_2010_2017@data%>%
                    group_by(HUC8Name,Year=substring(MP_End_Date,1,4))%>%
                    summarise(HUC8=first(HUC8),Discharge_MGD=plus(Measured_Effluent))


###########################################################################################################################################
####################################################Calculating Withdrawals################################################################

load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017_Reclass.RData")
VWUDS_2010_2017<-VWUDS_Monthly_WaterUse_View

#---Separate by Water Use Sector---# Go through individually---don't do scenario with transfers
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Agriculture/Irrigation")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Commercial")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Energy")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Industrial")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Municipal")

#Convert these facilities into a spatial dataframe and overlay with HUC 8s. Then, summarize the data by HUC 8.
#Create a spatial dataframe of all facilities with real geometry
VWUDS_2010_2017<-VWUDS_2010_2017[!(is.na(VWUDS_2010_2017$Corrected_Latitude)&is.na(VWUDS_2010_2017$Corrected_Longitude)),]
VWUDS_2010_2017<-SpatialPointsDataFrame(data.frame(Longitude=VWUDS_2010_2017$Corrected_Longitude,Latitude=VWUDS_2010_2017$Corrected_Latitude),VWUDS_2010_2017,proj4string = CRS("+init=epsg:4269"))
VWUDS_2010_2017@data$Withdrawals_MGD<-as.numeric(VWUDS_2010_2017@data$Withdrawals_MGD)
#Overlay with HUC names
HUC8_VWUDS<-over(VWUDS_2010_2017,HUC8_Overlay)
VWUDS_2010_2017@data$HUC8<-HUC8_VWUDS$HUC8
VWUDS_2010_2017@data$HUC8Name<-HUC8_VWUDS$HUC8Name

VWUDS_2010_2017.text<-VWUDS_2010_2017@data
#Summarize by HUC to find the total withdrawal occurring in each HUC

HUC8_Withdrawals<-VWUDS_2010_2017@data%>%
  group_by(HUC8Name,Year=substring(Date,1,4))%>%
  summarise(HUC8=first(HUC8),Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T))

###########################################################################################################################################
####################################################Put Withdrawals and Discahrges into HUC8 Spatial Dataframe#############################

#---Year 2010----#
HUC8@data$Discharges_2010<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2010"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2010"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2010"])],NA)
HUC8@data$Withdrawals_2010<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2010"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2010"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2010"])],NA)

#---Year 2011----#
HUC8@data$Discharges_2011<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2011"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2011"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2011"])],NA)
HUC8@data$Withdrawals_2011<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2011"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2011"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2011"])],NA)

#---Year 2012----#
HUC8@data$Discharges_2012<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2012"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2012"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2012"])],NA)
HUC8@data$Withdrawals_2012<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2012"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2012"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2012"])],NA)

#---Year 2013----#
HUC8@data$Discharges_2013<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2013"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2013"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2013"])],NA)
HUC8@data$Withdrawals_2013<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2013"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2013"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2013"])],NA)

#---Year 2014----#
HUC8@data$Discharges_2014<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2014"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2014"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2014"])],NA)
HUC8@data$Withdrawals_2014<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2014"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2014"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2014"])],NA)

#---Year 2015----#
HUC8@data$Discharges_2015<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2015"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2015"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2015"])],NA)
HUC8@data$Withdrawals_2015<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2015"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2015"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2015"])],NA)

#---Year 2016----#
HUC8@data$Discharges_2016<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2016"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2016"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2016"])],NA)
HUC8@data$Withdrawals_2016<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2016"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2016"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2016"])],NA)

#---Year 2017----#
HUC8@data$Discharges_2017<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2017"],HUC8_Discharges$Discharge_MGD[HUC8_Discharges$Year=="2017"][match(HUC8@data$HUC8,HUC8_Discharges$HUC8[HUC8_Discharges$Year=="2017"])],NA)
HUC8@data$Withdrawals_2017<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2017"],HUC8_Withdrawals$Withdrawals_MGD[HUC8_Withdrawals$Year=="2017"][match(HUC8@data$HUC8,HUC8_Withdrawals$HUC8[HUC8_Withdrawals$Year=="2017"])],NA)

###########################################################################################################################################
#######################################################Net Water Balance and Consumtpive Use###############################################
HUC8_glimpse<-HUC8@data
#---Year 2010----#

#--With Transfers---#
HUC8@data$NetWB_2010_t<-(ifelse(is.na(HUC8@data$Discharges_2010),0,HUC8@data$Discharges_2010))+
                                                                  HUC8@data$transferred_2010- #transfers have sign--negative mean water is leaving, positive means water is coming in 
                      (ifelse(is.na(HUC8@data$Withdrawals_2010),0,HUC8@data$Withdrawals_2010))

HUC8@data$Consumption_2010_t<-((ifelse(is.na(HUC8@data$Withdrawals_2010),0,HUC8@data$Withdrawals_2010))+(ifelse(is.na(HUC8@data$waterout_2010),0,-HUC8@data$waterout_2010))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2010),0,HUC8@data$Discharges_2010))+(ifelse(is.na(HUC8@data$waterin_2010),0,HUC8@data$waterin_2010))))/
                              ((ifelse(is.na(HUC8@data$Withdrawals_2010),0,HUC8@data$Withdrawals_2010))+(ifelse(is.na(HUC8@data$waterout_2010),0,HUC8@data$waterout_2010)))

#--Without Transfers---#

HUC8@data$NetWB_2010<-(ifelse(is.na(HUC8@data$Discharges_2010),0,HUC8@data$Discharges_2010))-(ifelse(is.na(HUC8@data$Withdrawals_2010),0,HUC8@data$Withdrawals_2010))


HUC8@data$Consumption_2010<-((ifelse(is.na(HUC8@data$Withdrawals_2010),0,HUC8@data$Withdrawals_2010))-
                               (ifelse(is.na(HUC8@data$Discharges_2010),0,HUC8@data$Discharges_2010)))/(ifelse(is.na(HUC8@data$Withdrawals_2010),0,HUC8@data$Withdrawals_2010))

#---Year 2011----#

#----With transfers---#
HUC8@data$NetWB_2011_t<-(ifelse(is.na(HUC8@data$Discharges_2011),0,HUC8@data$Discharges_2011))+
                                                                    HUC8@data$transferred_2011-
                      (ifelse(is.na(HUC8@data$Withdrawals_2011),0,HUC8@data$Withdrawals_2011))

HUC8@data$Consumption_2011_t<-((ifelse(is.na(HUC8@data$Withdrawals_2011),0,HUC8@data$Withdrawals_2011))+(ifelse(is.na(HUC8@data$waterout_2011),0,-HUC8@data$waterout_2011))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2011),0,HUC8@data$Discharges_2011))+(ifelse(is.na(HUC8@data$waterin_2011),0,HUC8@data$waterin_2011))))/
                             ((ifelse(is.na(HUC8@data$Withdrawals_2011),0,HUC8@data$Withdrawals_2011))+(ifelse(is.na(HUC8@data$waterout_2011),0,HUC8@data$waterout_2011)))


#---Without Transfers----#
HUC8@data$NetWB_2011<-(ifelse(is.na(HUC8@data$Discharges_2011),0,HUC8@data$Discharges_2011))-(ifelse(is.na(HUC8@data$Withdrawals_2011),0,HUC8@data$Withdrawals_2011))



HUC8@data$Consumption_2011<-((ifelse(is.na(HUC8@data$Withdrawals_2011),0,HUC8@data$Withdrawals_2011))-
                            (ifelse(is.na(HUC8@data$Discharges_2011),0,HUC8@data$Discharges_2011)))/
                            (ifelse(is.na(HUC8@data$Withdrawals_2011),0,HUC8@data$Withdrawals_2011))


#---Year 2012----#

#---With Transfers---#
HUC8@data$NetWB_2012_t<-(ifelse(is.na(HUC8@data$Discharges_2012),0,HUC8@data$Discharges_2012))+
                      HUC8@data$transferred_2012-
                      (ifelse(is.na(HUC8@data$Withdrawals_2012),0,HUC8@data$Withdrawals_2012))

HUC8@data$Consumption_2012_t<-((ifelse(is.na(HUC8@data$Withdrawals_2012),0,HUC8@data$Withdrawals_2012))+(ifelse(is.na(HUC8@data$waterout_2012),0,-HUC8@data$waterout_2012))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2012),0,HUC8@data$Discharges_2012))+(ifelse(is.na(HUC8@data$waterin_2012),0,HUC8@data$waterin_2012))))/
                              ((ifelse(is.na(HUC8@data$Withdrawals_2012),0,HUC8@data$Withdrawals_2012))+(ifelse(is.na(HUC8@data$waterout_2012),0,HUC8@data$waterout_2012)))

#---Without Transfers---#
HUC8@data$NetWB_2012<-(ifelse(is.na(HUC8@data$Discharges_2012),0,HUC8@data$Discharges_2012))-(ifelse(is.na(HUC8@data$Withdrawals_2012),0,HUC8@data$Withdrawals_2012))

HUC8@data$Consumption_2012<-((ifelse(is.na(HUC8@data$Withdrawals_2012),0,HUC8@data$Withdrawals_2012))-
                               (ifelse(is.na(HUC8@data$Discharges_2012),0,HUC8@data$Discharges_2012)))/
                            (ifelse(is.na(HUC8@data$Withdrawals_2012),0,HUC8@data$Withdrawals_2012))

#---Year 2013----#

#---With Transfers---#
HUC8@data$NetWB_2013_t<-(ifelse(is.na(HUC8@data$Discharges_2013),0,HUC8@data$Discharges_2013))+
                        HUC8@data$transferred_2013-
                        (ifelse(is.na(HUC8@data$Withdrawals_2013),0,HUC8@data$Withdrawals_2013))

HUC8@data$Consumption_2013_t<-((ifelse(is.na(HUC8@data$Withdrawals_2013),0,HUC8@data$Withdrawals_2013))+(ifelse(is.na(HUC8@data$waterout_2013),0,-HUC8@data$waterout_2013))-
                                  ((ifelse(is.na(HUC8@data$Discharges_2013),0,HUC8@data$Discharges_2013))+(ifelse(is.na(HUC8@data$waterin_2013),0,HUC8@data$waterin_2013))))/
                              ((ifelse(is.na(HUC8@data$Withdrawals_2013),0,HUC8@data$Withdrawals_2013))+(ifelse(is.na(HUC8@data$waterout_2013),0,HUC8@data$waterout_2013)))

#---Without Transfers---#
HUC8@data$NetWB_2013<-(ifelse(is.na(HUC8@data$Discharges_2013),0,HUC8@data$Discharges_2013))-(ifelse(is.na(HUC8@data$Withdrawals_2013),0,HUC8@data$Withdrawals_2013))

HUC8@data$Consumption_2013<-((ifelse(is.na(HUC8@data$Withdrawals_2013),0,HUC8@data$Withdrawals_2013))-
                               (ifelse(is.na(HUC8@data$Discharges_2013),0,HUC8@data$Discharges_2013)))/
                            (ifelse(is.na(HUC8@data$Withdrawals_2013),0,HUC8@data$Withdrawals_2013))


#---Year 2014----#

#---With Transfers---#
HUC8@data$NetWB_2014_t<-(ifelse(is.na(HUC8@data$Discharges_2014),0,HUC8@data$Discharges_2014))+
  HUC8@data$transferred_2014-
  (ifelse(is.na(HUC8@data$Withdrawals_2014),0,HUC8@data$Withdrawals_2014))

HUC8@data$Consumption_2014_t<-((ifelse(is.na(HUC8@data$Withdrawals_2014),0,HUC8@data$Withdrawals_2014))+(ifelse(is.na(HUC8@data$waterout_2014),0,-HUC8@data$waterout_2014))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2014),0,HUC8@data$Discharges_2014))+(ifelse(is.na(HUC8@data$waterin_2014),0,HUC8@data$waterin_2014))))/
  ((ifelse(is.na(HUC8@data$Withdrawals_2014),0,HUC8@data$Withdrawals_2014))+(ifelse(is.na(HUC8@data$waterout_2014),0,HUC8@data$waterout_2014)))

#---Without Transfers---#
HUC8@data$NetWB_2014<-(ifelse(is.na(HUC8@data$Discharges_2014),0,HUC8@data$Discharges_2014))-(ifelse(is.na(HUC8@data$Withdrawals_2014),0,HUC8@data$Withdrawals_2014))

HUC8@data$Consumption_2014<-((ifelse(is.na(HUC8@data$Withdrawals_2014),0,HUC8@data$Withdrawals_2014))-
                               (ifelse(is.na(HUC8@data$Discharges_2014),0,HUC8@data$Discharges_2014)))/
  (ifelse(is.na(HUC8@data$Withdrawals_2014),0,HUC8@data$Withdrawals_2014))


#---Year 2015----#

#---With Transfers---#
HUC8@data$NetWB_2015_t<-(ifelse(is.na(HUC8@data$Discharges_2015),0,HUC8@data$Discharges_2015))+
  HUC8@data$transferred_2015-
  (ifelse(is.na(HUC8@data$Withdrawals_2015),0,HUC8@data$Withdrawals_2015))

HUC8@data$Consumption_2015_t<-((ifelse(is.na(HUC8@data$Withdrawals_2015),0,HUC8@data$Withdrawals_2015))+(ifelse(is.na(HUC8@data$waterout_2015),0,-HUC8@data$waterout_2015))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2015),0,HUC8@data$Discharges_2015))+(ifelse(is.na(HUC8@data$waterin_2015),0,HUC8@data$waterin_2015))))/
  ((ifelse(is.na(HUC8@data$Withdrawals_2015),0,HUC8@data$Withdrawals_2015))+(ifelse(is.na(HUC8@data$waterout_2015),0,HUC8@data$waterout_2015)))

#---Without Transfers---#
HUC8@data$NetWB_2015<-(ifelse(is.na(HUC8@data$Discharges_2015),0,HUC8@data$Discharges_2015))-(ifelse(is.na(HUC8@data$Withdrawals_2015),0,HUC8@data$Withdrawals_2015))

HUC8@data$Consumption_2015<-((ifelse(is.na(HUC8@data$Withdrawals_2015),0,HUC8@data$Withdrawals_2015))-
                               (ifelse(is.na(HUC8@data$Discharges_2015),0,HUC8@data$Discharges_2015)))/
  (ifelse(is.na(HUC8@data$Withdrawals_2015),0,HUC8@data$Withdrawals_2015))

#---Year 2016----#

#---With Transfers---#
HUC8@data$NetWB_2016_t<-(ifelse(is.na(HUC8@data$Discharges_2016),0,HUC8@data$Discharges_2016))+
  HUC8@data$transferred_2016-
  (ifelse(is.na(HUC8@data$Withdrawals_2016),0,HUC8@data$Withdrawals_2016))

HUC8@data$Consumption_2016_t<-((ifelse(is.na(HUC8@data$Withdrawals_2016),0,HUC8@data$Withdrawals_2016))+(ifelse(is.na(HUC8@data$waterout_2016),0,-HUC8@data$waterout_2016))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2016),0,HUC8@data$Discharges_2016))+(ifelse(is.na(HUC8@data$waterin_2016),0,HUC8@data$waterin_2016))))/
  ((ifelse(is.na(HUC8@data$Withdrawals_2016),0,HUC8@data$Withdrawals_2016))+(ifelse(is.na(HUC8@data$waterout_2016),0,HUC8@data$waterout_2016)))

#---Without Transfers---#
HUC8@data$NetWB_2016<-(ifelse(is.na(HUC8@data$Discharges_2016),0,HUC8@data$Discharges_2016))-(ifelse(is.na(HUC8@data$Withdrawals_2016),0,HUC8@data$Withdrawals_2016))

HUC8@data$Consumption_2016<-((ifelse(is.na(HUC8@data$Withdrawals_2016),0,HUC8@data$Withdrawals_2016))-
                               (ifelse(is.na(HUC8@data$Discharges_2016),0,HUC8@data$Discharges_2016)))/
  (ifelse(is.na(HUC8@data$Withdrawals_2016),0,HUC8@data$Withdrawals_2016))

#---Year 2017----#

#---With Transfers---#
HUC8@data$NetWB_2017_t<-(ifelse(is.na(HUC8@data$Discharges_2017),0,HUC8@data$Discharges_2017))+
  HUC8@data$transferred_2017-
  (ifelse(is.na(HUC8@data$Withdrawals_2017),0,HUC8@data$Withdrawals_2017))

HUC8@data$Consumption_2017_t<-((ifelse(is.na(HUC8@data$Withdrawals_2017),0,HUC8@data$Withdrawals_2017))+(ifelse(is.na(HUC8@data$waterout_2017),0,-HUC8@data$waterout_2017))-
                                 ((ifelse(is.na(HUC8@data$Discharges_2017),0,HUC8@data$Discharges_2017))+(ifelse(is.na(HUC8@data$waterin_2017),0,HUC8@data$waterin_2017))))/
  ((ifelse(is.na(HUC8@data$Withdrawals_2017),0,HUC8@data$Withdrawals_2017))+(ifelse(is.na(HUC8@data$waterout_2017),0,HUC8@data$waterout_2017)))

#---Without Transfers---#
HUC8@data$NetWB_2017<-(ifelse(is.na(HUC8@data$Discharges_2017),0,HUC8@data$Discharges_2017))-(ifelse(is.na(HUC8@data$Withdrawals_2017),0,HUC8@data$Withdrawals_2017))

HUC8@data$Consumption_2017<-((ifelse(is.na(HUC8@data$Withdrawals_2017),0,HUC8@data$Withdrawals_2017))-
                               (ifelse(is.na(HUC8@data$Discharges_2017),0,HUC8@data$Discharges_2017)))/
  (ifelse(is.na(HUC8@data$Withdrawals_2017),0,HUC8@data$Withdrawals_2017))


#-------------Long Term Average 2010-2017--------------#

for (i in 1:length(HUC8@data$TNMID)){
  HUC8@data$NetWB_sum[i]<-(HUC8@data$NetWB_2010[i]+
                             HUC8@data$NetWB_2011[i]+
                             HUC8@data$NetWB_2012[i]+
                             HUC8@data$NetWB_2013[i]+
                             HUC8@data$NetWB_2014[i]+
                             HUC8@data$NetWB_2015[i]+
                             HUC8@data$NetWB_2016[i]+
                             HUC8@data$NetWB_2017[i])
}
HUC8@data$NetWB_ave<-HUC8@data$NetWB_sum/8

for (i in 1:length(HUC8@data$TNMID)){
  HUC8@data$Consumption_sum[i]<-(HUC8@data$Consumption_2010[i]+
                                   HUC8@data$Consumption_2011[i]+
                                   HUC8@data$Consumption_2012[i]+
                                   HUC8@data$Consumption_2013[i]+
                                   HUC8@data$Consumption_2014[i]+
                                   HUC8@data$Consumption_2015[i]+
                                   HUC8@data$Consumption_2016[i]+
                                   HUC8@data$Consumption_2017[i])
}

HUC8@data$Consumption_ave<-HUC8@data$Consumption_sum/8

#-----------------With Transfers-------------------------#
for (i in 1:length(HUC8@data$TNMID)){
  HUC8@data$NetWB_t_sum[i]<-(HUC8@data$NetWB_2010_t[i]+
                             HUC8@data$NetWB_2011_t[i]+
                             HUC8@data$NetWB_2012_t[i]+
                             HUC8@data$NetWB_2013_t[i]+
                             HUC8@data$NetWB_2014_t[i]+
                             HUC8@data$NetWB_2015_t[i]+
                             HUC8@data$NetWB_2016_t[i]+
                             HUC8@data$NetWB_2017_t[i])
}
HUC8@data$NetWB_t_ave<-HUC8@data$NetWB_t_sum/8


for (i in 1:length(HUC8@data$TNMID)){
  HUC8@data$Consumption_t_sum[i]<-(HUC8@data$Consumption_2010_t[i]+
                                   HUC8@data$Consumption_2011_t[i]+
                                   HUC8@data$Consumption_2012_t[i]+
                                   HUC8@data$Consumption_2013_t[i]+
                                   HUC8@data$Consumption_2014_t[i]+
                                   HUC8@data$Consumption_2015_t[i]+
                                   HUC8@data$Consumption_2016_t[i]+
                                   HUC8@data$Consumption_2017_t[i])
}

HUC8@data$Consumption_t_ave<-HUC8@data$Consumption_t_sum/8

HUC8_glimpse<-HUC8@data

#####################################################################################
#-----------------------------Visualization-----------------------------------------#
HUC8.df<-broom::tidy(HUC8)
HUC8$polyID<-sapply(slot(HUC8,"polygons"), function(x) slot(x, "ID"))
HUC8.df<-merge(HUC8.df, HUC8, by.x="id", by.y="polyID")

#------Merge to Spatial Dataframe by Year to do gganimate-------------#
HUC8_Consumptive_Use<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/HUC.gdb",layer='WBDHU8')
HUC8CU.df<-broom::tidy(HUC8_Consumptive_Use)
HUC8_Consumptive_Use$polyID<-sapply(slot(HUC8_Consumptive_Use,"polygons"), function(x) slot(x, "ID"))
HUC8CU.df<-merge(HUC8CU.df, HUC8_Consumptive_Use, by.x="id", by.y="polyID")

HUC8CU.df<-merge(HUC8CU.df,HUC8_Discharges,by="HUC8",all.x=T)
HUC8CU.df<-merge(HUC8CU.df,HUC8_Withdrawals,by=c("HUC8","Year","HUC8Name"),all.x=T)
HUC8CU.df<-merge(HUC8CU.df,HUC8_waterin,by=c("HUC8","Year","HUC8Name"),all.x=T)
HUC8CU.df<-merge(HUC8CU.df,HUC8_waterout,by=c("HUC8","Year","HUC8Name"),all.x=T)
HUC8CU.df$transferred_MGD<-(ifelse(is.na(HUC8CU.df$waterin),0,HUC8CU.df$waterin))-(ifelse(is.na(HUC8CU.df$waterout),0,HUC8CU.df$waterout))
HUC8CU.df$consumed_pct<-((ifelse(is.na(HUC8CU.df$Withdrawals_MGD),0,HUC8CU.df$Withdrawals_MGD))-(ifelse(is.na(HUC8CU.df$Discharge_MGD),0,HUC8CU.df$Discharge_MGD)))/
                                        ((ifelse(is.na(HUC8CU.df$waterin),0,HUC8CU.df$waterin))+(ifelse(is.na(HUC8CU.df$Withdrawals_MGD),0,HUC8CU.df$Withdrawals_MGD)))

HUC8CU.df$Year<-as.Date(HUC8CU.df$Year, format = "%Y")
HUC8CU.df$consumed_pct<-ifelse(HUC8CU.df$consumed_pct==-Inf,-9999,HUC8CU.df$consumed_pct)

HUC8CU.df<-HUC8CU.df[!duplicated(HUC8CU.df),]

#----Animated Plot-----#

HUC8_Animated<-
  ggplot(data=HUC8CU.df, aes(x=long, y= lat, group=group, fill= consumed_pct))+
  geom_polygon()+
  scale_fill_continuous("Consumptive Use (percentage)", limits=c(0,max(HUC8CU.df$consumed_pct,na.rm = T)), 
                        high="#99000d", low="#fee0d2", space = "Lab", na.value="#f0f0f0", guide="colourbar")+
  labs(title = "Consumptive Use (MGD) by HUC8 Watershed in: ")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+
  coord_equal()+
  transition_states(Year, transition_length=2, state_length=1)

animate(HUC8_Animated, filename="HUC8_Consumption.gif")


#----------------------------------------------------------------------------------------------------------#

HUC8_Centroids<-aggregate(cbind(long,lat)~Name, data=HUC8.df, FUN=function(x)mean(range(x)))

ggplot()+
  geom_polygon(data=HUC8,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  geom_label_repel(data=HUC8_Centroids,aes(x=long,y=lat,label=Name),size=4, segment.color = "#99000d", show.legend = F, arrow = arrow(length = unit(0.02, "npc")))+
  scale_colour_manual(values=c("#252525"))+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#---Distribution of Discharging Outfalls in HUC8-----#

ggplot()+
  geom_polygon(data=VA,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude))+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#---Size---#
ggplot()+
  geom_polygon(data=HUC8,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  labs(title="Distribution of Outfalls in the HUC 8 Watersheds")+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude,
                                           size=ECHO_2010_2017.test$Resolved_Mean_NA))+
  scale_radius(name="Average Discharge MGD")

#----Continous to Discrete----#

ggplot()+
  geom_polygon(data=HUC8,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  labs(title="Distribution of Outfalls in the HUC 8 Watersheds")+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude,
                                           colour=cut_number(ECHO_2010_2017.test$Resolved_Mean,5)),size=1)+
  scale_colour_manual("Mean Discharge (MGD)",values=c("#a6bddb","#74a9cf","#3690c0","#0570b0","#045a8d","#000000"))

ggplot()+
  geom_polygon(data=HUC8,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  labs(title="Distribution of Outfalls in the HUC 8 Watersheds")+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude,
                                           size=ECHO_2010_2017.test$Resolved_Mean))+
  scale_radius("Mean Discharge (MGD)", breaks=c(0,0.00367,0.0442,0.301,2.55),
               labels=c("[0,0.00367]","(0.00367,0.0442]","(0.0442,0.301]","(0.301,2.55]","(2.55,2.36e+03]"))

#---Distribution of Withdrawing Sources in HUC8-----#
VWUDS_2010_2017.test<-VWUDS_2010_2017@data
Mean_Withdrawal<-VWUDS_2010_2017.test%>%group_by(DEQ.ID.of.Source)%>%summarise(Mean_Withdrawal=mean(Withdrawals_MGD, na.rm=T))
VWUDS_2010_2017.test<-merge(VWUDS_2010_2017.test,Mean_Withdrawal, by="DEQ.ID.of.Source",all.x=T)

#---Size---#
ggplot()+
  geom_polygon(data=HUC8,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  labs(title="Distribution of Withdrawing Sources in the HUC 8 Watersheds")+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude, size=Mean_Withdrawal))+
  scale_radius(name="Average Withdrawal MGD")

#----Colour---#
ggplot()+
  geom_polygon(data=HUC8,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  labs(title="Distribution of Withdrawing Sources in the HUC 8 Watersheds")+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude,
                                           colour=cut_number(VWUDS_2010_2017.test$Mean_Withdrawal,4)),size=1)+
  scale_colour_manual("Mean Withdrawal (MGD)",values=c("#fee0d2","#fc9272","#ef3b2c","#99000d"))


#######################################################################################################################
#-----------------------------Without Transfers-----------------------------------------------------------------------#
#Convert Spatialpolygonsdataframe to dataframe 

#----Long Term Average----#

#---Net Water Balance---#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_ave),color="#252525")+
  scale_fill_gradient("Net Water Balance (MGD)", 
                      high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "2010-2017 Ave Net Water Balance (MGD) by HUC8 Watershed")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#---Consumption---#

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_ave),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "2010-2017 Ave Consumptive Use (%) by HUC8 Watershed")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_ave),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", na.value="#252525",space = "Lab", guide="colourbar")+
  labs(title = "2010-2017 Ave Consumptive Use (%) by HUC8 Watershed")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Net Water Balance-------#

#--2010--#

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2010),color="#252525")+
  scale_fill_gradient("Net Water Balance (MGD)", 
                      high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2010),color="#252525")+
  scale_fill_gradient("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                      breaks=c(-5000,0,5000), labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2011),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2011),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2012),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2012),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()
  
#--2013--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2013),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2013),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2014),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2014),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2015),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2015),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2016--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2016),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2016),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#636363", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2017--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2017),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#f0f0f0", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2017),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#9ecae1", low="#a50f15", space = "Lab", na.value="#f0f0f0", guide="colourbar",
                        breaks=c(-5000,0,5000),labels=c("-5,000","0","5,000"))+
  labs(title = "Net Water Balance (MGD) by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()




#----Year by Year Consumption Plots-------#

#--2010--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2010),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2010),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2011),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2011),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2012),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2012),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2013--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2013),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2013),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2014),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2014),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2015),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2015),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2016--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2016),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2016),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2017--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2017),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2017),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#######################################################################################################################
#-----------------------------Transfers----------------------------#

#----Long Term Average----#

#---Net Water Balance---#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_t_ave),color="#252525")+
  scale_fill_gradient("Net Water Balance (MGD)", 
                      high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "2010-2017 Ave Net Water Balance (MGD) by HUC8 Watershed")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#---Consumption---#

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_t_ave),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "2010-2017 Ave Consumptive Use (%) by HUC8 Watershed")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_t_ave),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", na.value="#252525",space = "Lab", guide="colourbar")+
  labs(title = "2010-2017 Ave Consumptive Use (%) by HUC8 Watershed")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Net Water Balance-------#

#--2010--#

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2010_t),color="#252525")+
  scale_fill_gradient("Net Water Balance (MGD)", 
                      high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2011_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2012_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2013--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2013_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2014_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2015_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2016--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2016_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#636363", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2017--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2017_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#f0f0f0", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Consumption Plots-------#

#--2010--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2010_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2010_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2011_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2011_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2012_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2012_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2013--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2013_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2013_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2014_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2014_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2015_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2015_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2016--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2016_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2016_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2017--#
ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2017_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

ggplot()+
  geom_polygon(
    data=HUC8.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2017_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient", limits=c(0,1),
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC8 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


#Plot consumptive use by first assigning data to the cropped HUC boundary and then by running the plot and legend commands
HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")


######################################################################################################################
#------------------------------------------Separate by Water Use Sector----------------------------------------------#



