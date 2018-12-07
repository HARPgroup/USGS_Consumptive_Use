###########################################################################################################################################
#########################Estimating Consumptive Use in Virginia Counties##########################################################

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
library(scales)
library(ggrepel)
library(GISTools)
library(maps)
library(grid)

options(scipen=999) #Disable scientific notation
options(digits = 9)
memory.limit(size=100000000)

###########################################################################################################################################
####County Shapefile Manipulation####

#Load databases and extract required layers
VA_Counties<-readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/tlgdb_2015_a_51_va.gdb',layer="County")

#Reproject shapefiles to NAD83=EPSG Code of 4269
VA_Counties<-spTransform(VA_Counties, CRS("+init=epsg:4269"))
names(VA_Counties@data)[2]<-c("FIPS")
names(VA_Counties@data)[3]<-c("County")

#Create County Dataframe that will be used in future overlay processes
VA_Counties_Overlay<-VA_Counties #Keep integrity of spatial dataframe
VA_Counties_Overlay@data<-VA_Counties_Overlay@data[,c(2,3)] 
names(VA_Counties_Overlay@data)<-c("FIPS","County")

VA_FIPS<-VA_Counties_Overlay@data
###########################################################################################################################################
###########################################Calculating Transfers###########################################################################
#There are two different types of Transfers: Deliveries and Releases (both have TO and FROM components)
#This is because VA Hydro stores transfers at both the recieving and departing facilities
#Therefore, we will keep them separate during calculations and refer to transfers as deliveries and releases in both the TO and FROM lists

###########################################################################################################################################
####Delivery Transfers (TO)####
#Monthly Deliveries from 01/01/2010-12/31/2017
#fstatus
#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_4
GET('http://deq1.bse.vt.edu/d.bet/echo_transfer_to',write_disk(temp <- tempfile(fileext = ".csv")))
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

####Create Spatial Data Frame from Points and Overlay on Clipped VA_Counties Shapefile####
####FROM Delivery Transfers####
#Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
#Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
dFrom<-deliveries[!(is.na(deliveries$geomFlat)&is.na(deliveries$geomFlon)),]
dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
VA_Counties_Facilities<-over(dFrom,VA_Counties_Overlay)#Spatial overlay
dFrom@data$County<-VA_Counties_Facilities$County
dFrom@data$FIPS<-VA_Counties_Facilities$FIPS

####TO Delivery Transfers####
#Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
#Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
VA_Counties_Facilities<-over(dTo,VA_Counties_Overlay)#Spatial overlay
dTo@data$County<-VA_Counties_Facilities$County
dTo@data$FIPS<-VA_Counties_Facilities$FIPS

####Determine if TO Transfers are leaving watershed boundaries####
#Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
#We are only concerned with intercounty transfers and need to identify these with the following code
dTo@data$intercounty<-NA
dFrom@data$intercounty<-NA
#Check each transfer in the delivery VA Hydro transfers to see if its FROM County is different than its TO County

for (i in 1:length(dTo@data$hydroid)){
  ToCounty<-as.character(dTo@data$FIPS[i])
  FromCounty<-as.character(dFrom@data$FIPS[i])
  if(is.na(ToCounty)){
    ToCounty<-'Null County'
  }
  if(is.na(FromCounty)){
    FromCounty<-'Null County' 
  }
  intercounty<-0
  if(ToCounty!=FromCounty){ #if the County does not match, mark as intercounty delivery
    intercounty<-1
  }
  dTo@data$intercounty[i]<-intercounty
  dFrom@data$intercounty[i]<-intercounty
}

####Sum Net Water In and Out for each County####
###FROM Deliveries###
delf<-dFrom@data
delf<-delf[delf$intercounty==1,] #if intercounty is equal to 1, it is crossing boundaries---so just include those

delf<-delf%>%
  dplyr::group_by(County, Year)%>%
  dplyr::summarize(FIPS=first(FIPS),intercounty=sum(intercounty), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD

delf$County<-as.character(delf$County)
delf$County[is.na(delf$County)]<-'Fell Outside County Limits'

###TO Deliveries###
delt<-dTo@data
delt<-delt[delt$intercounty==1,] #narrow down to deliveries happening across borders

delt<-
  delt%>%
  dplyr::group_by(County, Year)%>%
  dplyr::summarize(FIPS=first(FIPS),intercounty=sum(intercounty), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD


delt$County<-as.character(delt$County)
delt$County[is.na(delt$County)]<-'Fell Outside County Limits'
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

####Create Spatial Data Frame from Points and Overlay on Clipped County Shapefile####
####FROM Release Transfers####
rFrom<-releases[!(is.na(releases$geomFlat)&is.na(releases$geomFlon)),]
rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
VA_Counties_Facilities<-over(rFrom,VA_Counties_Overlay)
rFrom@data$FIPS<-VA_Counties_Facilities$FIPS
rFrom@data$County<-VA_Counties_Facilities$County
####TO Release Transfers####
rTo<-releases[!(is.na(releases$geomTlat)&is.na(releases$geomTlon)),]
rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
VA_Counties_Facilities<-over(rTo,VA_Counties_Overlay)
rTo@data$FIPS<-VA_Counties_Facilities$FIPS
rTo@data$County<-VA_Counties_Facilities$County

####Determine if Release FROM Transfers are leaving County boundaries####
rTo@data$intercounty<-NA
rFrom@data$intercounty<-NA

for (i in 1:length(rTo@data$hydroid)){
  ToCounty<-as.character(rTo@data$FIPS[i])
  FromCounty<-as.character(rFrom@data$FIPS[i])
  if(is.na(ToCounty)){ #if the HUC code is NA
    ToCounty<-'Null FIPS'
  }
  if(is.na(FromCounty)){
    FromCounty<-'Null FIPS' 
  }
  intercounty<-0
  if(ToCounty!=FromCounty){
    intercounty<-1
  }
  rTo@data$intercounty[i]<-intercounty #1 indicating transfer is crossing watershed boundaries
  rFrom@data$intercounty[i]<-intercounty
}

####Sum Net Water In and Out for each County####
###FROM Releases###
relf<-rFrom@data
relf<-relf[relf$intercounty==1,]#remember intratransfers are indicated with a 0

relf<-relf%>% #Summarise by FIPS and year
  dplyr::group_by(County,Year)%>%
  dplyr::summarise(FIPS=first(FIPS), intercounty=sum(intercounty), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)  #MGM to MGD

relf$County<-as.character(relf$County)
relf$County[is.na(relf$County)]<-'Fell Outside County Limits'

###TO Releases###
relt<-rTo@data
relt<-relt[relt$intercounty==1,]

relt<-relt%>%
  dplyr::group_by(County,Year)%>%
  dplyr::summarise(FIPS=first(FIPS), intercounty=sum(intercounty), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)

relt$County<-as.character(relt$County)
relt$County[is.na(relt$County)]<-'Fell Outside County Limits'
###########################################################################################################################################
####Calculate Net Transfers for Each FIPS Watershed####
#Loop through each HUC 8 and check for summed releases and deliveries
#Water out is defined as the "from's" and Water in are the "to's"
#This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

County_waterout<-as.data.table(rbind(relf,delf))
County_waterout<-County_waterout[, lapply(.SD,sum), by=list(County, Year, FIPS), .SDcols=c(4,5)]

County_waterin<-as.data.table(rbind(relt,delt))
County_waterin<-County_waterin[, lapply(.SD,sum), by=list(County, Year, FIPS), .SDcols=c(4,5)]

#---Year 2010---#
VA_Counties@data$waterout_2010<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2010"],-County_waterout$waterout[County_waterout$Year=="2010"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2010"])],NA)
VA_Counties@data$waterin_2010<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2010"],County_waterin$waterin[County_waterin$Year=="2010"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2010"])],NA)
VA_Counties@data$transferred_2010<- (rowSums(VA_Counties@data[,(10:11)],na.rm=T))
VA_Counties@data$transferred_2010<-ifelse(is.na(VA_Counties@data$waterin_2010)&is.na(VA_Counties@data$waterout_2010),NA,VA_Counties@data$transferred_2010)

#---Year 2011---#
VA_Counties@data$waterout_2011<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2011"],-County_waterout$waterout[County_waterout$Year=="2011"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2011"])],NA)
VA_Counties@data$waterin_2011<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2011"],County_waterin$waterin[County_waterin$Year=="2011"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2011"])],NA)
VA_Counties@data$transferred_2011<- (rowSums(VA_Counties@data[,(13:14)],na.rm=T))
VA_Counties@data$transferred_2011<-ifelse(is.na(VA_Counties@data$waterin_2011)&is.na(VA_Counties@data$waterout_2011),NA,VA_Counties@data$transferred_2011)

#---Year 2012---#
VA_Counties@data$waterout_2012<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2012"],-County_waterout$waterout[County_waterout$Year=="2012"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2012"])],NA)
VA_Counties@data$waterin_2012<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2012"],County_waterin$waterin[County_waterin$Year=="2012"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2012"])],NA)
VA_Counties@data$transferred_2012<- (rowSums(VA_Counties@data[,(16:17)],na.rm=T))
VA_Counties@data$transferred_2012<-ifelse(is.na(VA_Counties@data$waterin_2012)&is.na(VA_Counties@data$waterout_2012),NA,VA_Counties@data$transferred_2012)

#---Year 2013---#
VA_Counties@data$waterout_2013<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2013"],-County_waterout$waterout[County_waterout$Year=="2013"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2013"])],NA)
VA_Counties@data$waterin_2013<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2013"],County_waterin$waterin[County_waterin$Year=="2013"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2013"])],NA)
VA_Counties@data$transferred_2013<- (rowSums(VA_Counties@data[,(19:20)],na.rm=T))
VA_Counties@data$transferred_2013<-ifelse(is.na(VA_Counties@data$waterin_2013)&is.na(VA_Counties@data$waterout_2013),NA,VA_Counties@data$transferred_2013)

#---Year 2014---#
VA_Counties@data$waterout_2014<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2014"],-County_waterout$waterout[County_waterout$Year=="2014"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2014"])],NA)
VA_Counties@data$waterin_2014<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2014"],County_waterin$waterin[County_waterin$Year=="2014"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2014"])],NA)
VA_Counties@data$transferred_2014<- (rowSums(VA_Counties@data[,(22:23)],na.rm=T))
VA_Counties@data$transferred_2014<-ifelse(is.na(VA_Counties@data$waterin_2014)&is.na(VA_Counties@data$waterout_2014),NA,VA_Counties@data$transferred_2014)

#---Year 2015---#
VA_Counties@data$waterout_2015<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2015"],-County_waterout$waterout[County_waterout$Year=="2015"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2015"])],NA)
VA_Counties@data$waterin_2015<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2015"],County_waterin$waterin[County_waterin$Year=="2015"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2015"])],NA)
VA_Counties@data$transferred_2015<- (rowSums(VA_Counties@data[,(25:26)],na.rm=T))
VA_Counties@data$transferred_2015<-ifelse(is.na(VA_Counties@data$waterin_2015)&is.na(VA_Counties@data$waterout_2015),NA,VA_Counties@data$transferred_2015)

#---Year 2016---#
VA_Counties@data$waterout_2016<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2016"],-County_waterout$waterout[County_waterout$Year=="2016"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2016"])],NA)
VA_Counties@data$waterin_2016<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2016"],County_waterin$waterin[County_waterin$Year=="2016"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2016"])],NA)
VA_Counties@data$transferred_2016<- (rowSums(VA_Counties@data[,(28:29)],na.rm=T))
VA_Counties@data$transferred_2016<-ifelse(is.na(VA_Counties@data$waterin_2016)&is.na(VA_Counties@data$waterout_2016),NA,VA_Counties@data$transferred_2016)

#---Year 2017---#
VA_Counties@data$waterout_2017<-ifelse(VA_Counties@data$FIPS%in%County_waterout$FIPS[County_waterout$Year=="2017"],-County_waterout$waterout[County_waterout$Year=="2017"][match(VA_Counties@data$FIPS,County_waterout$FIPS[County_waterout$Year=="2017"])],NA)
VA_Counties@data$waterin_2017<-ifelse(VA_Counties@data$FIPS%in%County_waterin$FIPS[County_waterin$Year=="2017"],County_waterin$waterin[County_waterin$Year=="2017"][match(VA_Counties@data$FIPS,County_waterin$FIPS[County_waterin$Year=="2017"])],NA)
VA_Counties@data$transferred_2017<- (rowSums(VA_Counties@data[,(31:32)],na.rm=T))
VA_Counties@data$transferred_2017<-ifelse(is.na(VA_Counties@data$waterin_2017)&is.na(VA_Counties@data$waterout_2017),NA,VA_Counties@data$transferred_2017)


County_Transfers<-data.frame(County=VA_Counties@data$County,
                           FIPS=VA_Counties@data$FIPS,
                           Transfers_2010_MGD=VA_Counties@data$transferred_2010,
                           Transfers_2011_MGD=VA_Counties@data$transferred_2011,
                           Transfers_2012_MGD=VA_Counties@data$transferred_2012,
                           Transfers_2013_MGD=VA_Counties@data$transferred_2013,
                           Transfers_2014_MGD=VA_Counties@data$transferred_2014,
                           Transfers_2015_MGD=VA_Counties@data$transferred_2015,
                           Transfers_2016_MGD=VA_Counties@data$transferred_2016,
                           Transfers_2017_MGD=VA_Counties@data$transferred_2017)

County_Transfers<-County_Transfers[order(County_Transfers$HUC8_Name,decreasing=F),]

#rm(delf,delt,dFrom,dTo,relf,relt,rFrom,rTo)
###########################################################################################################################################
####################################################Calculating Discharges#################################################################

#####Load in Discharge Data####

ECHO_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC.txt", sep="\t", header=T)

ECHO_2010_2017%>%
  summarise(Facilities=n_distinct(Facility.ID),
            Outfalls=n_distinct(OutfallID),
            Summed_D=sum(Measured_Effluent,na.rm=T),
            Summed_D_QAQC=sum(Resolved_Measured_Effluent_Med,na.rm=T),
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

ECHO_2010_2017<-subset(ECHO_2010_2017, subset=!ECHO_2010_2017$Reclass_Use_Type=="Energy")
ECHO_2010_2017<-SpatialPointsDataFrame(data.frame(Outfall_Longitude=ECHO_2010_2017$Outfall_Longitude,Outfall_Latitude=ECHO_2010_2017$Outfall_Latitude),ECHO_2010_2017,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83

#----Set Data Type----#
ECHO_2010_2017@data$FacilityName<-as.character(ECHO_2010_2017@data$FacilityName)
ECHO_2010_2017@data$Resolved_Measured_Effluent_NA<-as.numeric(ECHO_2010_2017@data$Resolved_Measured_Effluent_NA)#after QA/QC
ECHO_2010_2017@data$Resolved_Measured_Effluent_Med<-as.numeric(ECHO_2010_2017@data$Resolved_Measured_Effluent_Med)#after QA/QC
ECHO_2010_2017@data$Measured_Effluent<-as.numeric(ECHO_2010_2017@data$Measured_Effluent)#Before QA/QC

####Overlay with County Shapefile####
VA_Counties_Facilities<-over(ECHO_2010_2017,VA_Counties_Overlay)
ECHO_2010_2017@data$FIPS<-VA_Counties_Facilities$FIPS
ECHO_2010_2017@data$County<-VA_Counties_Facilities$County

####Sum Discharges in the Counties####
ECHO_2010_2017.test<-as.data.frame(ECHO_2010_2017@data)
ECHO_Resol_Mean_NA<-ECHO_2010_2017.test%>%dplyr::group_by(OutfallID)%>%dplyr::summarise(Resolved_Mean_NA=mean(Resolved_Measured_Effluent_NA,na.rm=T))
ECHO_Resol_Mean_Med<-ECHO_2010_2017.test%>%dplyr::group_by(OutfallID)%>%dplyr::summarise(Resolved_Mean_Med=mean(Resolved_Measured_Effluent_Med,na.rm=T))
ECHO_2010_2017.test<-merge(ECHO_2010_2017.test,ECHO_Resol_Mean_NA,by="OutfallID",all.x=T)
ECHO_2010_2017.test<-merge(ECHO_2010_2017.test,ECHO_Resol_Mean_Med,by="OutfallID",all.x=T)

Outfall_Discharges<-ECHO_2010_2017@data%>%
  dplyr::group_by(OutfallID,Year)%>%
  dplyr::summarise(Facility.ID=first(Facility.ID),
                   Facility_Name=first(FacilityName),
                   Mon_Reported=first(Mon_Reported),
                   Discharges_MGD=sum(Resolved_Measured_Effluent_Med, na.rm=T)/first(Mon_Reported),
                   Sector=first(Reclass_Use_Type),
                   County=first(County),
                   FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))


County_Discharges<-Outfall_Discharges%>%
  dplyr::group_by(County,Year)%>%
  dplyr::summarise(FIPS=first(FIPS),Discharge_MGD=sum(Discharges_MGD,na.rm=T))

###########################################################################################################################################
####################################################Calculating Withdrawals################################################################

load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017.RData")


#---Separate by Water Use Sector---# Go through individually---don't do scenario with transfers
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Agriculture/Irrigation")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Commercial")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Energy")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Industrial")
VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Reclass_Use_Type=="Municipal")

VWUDS_2010_2017<-subset(VWUDS_2010_2017, subset=!VWUDS_2010_2017$Reclass_Use_Type=="Energy")
#Convert these facilities into a spatial dataframe and overlay with HUC 8s. Then, summarize the data by HUC 8.
#Create a spatial dataframe of all facilities with real geometry
VWUDS_2010_2017<-VWUDS_2010_2017[!(is.na(VWUDS_2010_2017$Corrected_Latitude)&is.na(VWUDS_2010_2017$Corrected_Longitude)),]
VWUDS_2010_2017<-SpatialPointsDataFrame(data.frame(Longitude=VWUDS_2010_2017$Corrected_Longitude,Latitude=VWUDS_2010_2017$Corrected_Latitude),VWUDS_2010_2017,proj4string = CRS("+init=epsg:4269"))
VWUDS_2010_2017@data$Withdrawals_MGD<-as.numeric(VWUDS_2010_2017@data$Withdrawals_MGD)
#Overlay with HUC names
County_VWUDS<-over(VWUDS_2010_2017,VA_Counties_Overlay)
VWUDS_2010_2017@data$FIPS<-County_VWUDS$FIPS
VWUDS_2010_2017@data$County<-County_VWUDS$County

VWUDS_2010_2017.test<-VWUDS_2010_2017@data
#Summarize by HUC to find the total withdrawal occurring in each HUC


Source_Withdrawals<-VWUDS_2010_2017@data%>%
  dplyr::group_by(DEQ.ID.of.Source,Year)%>%
  dplyr::summarise(Facility.ID=first(Facility.ID),
                   Facility_Name=first(Facility),
                   Mon_Reported=first(Mon_Reported),
                   Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                   Sector=first(Reclass_Use_Type),
                   County=first(County),
                   FIPS=first(FIPS))%>%arrange(desc(Withdrawals_MGD))

County_Withdrawals<-Source_Withdrawals%>%
  dplyr::group_by(County,Year)%>%
  dplyr::summarise(FIPS=first(FIPS),Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T))

###########################################################################################################################################
####################################################Put Withdrawals and Discahrges into HUC8 Spatial Dataframe#############################

#---Year 2010----#
VA_Counties@data$Discharges_2010<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2010"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2010"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2010"])],NA)
VA_Counties@data$Withdrawals_2010<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2010"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2010"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2010"])],NA)

#---Year 2011----#
VA_Counties@data$Discharges_2011<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2011"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2011"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2011"])],NA)
VA_Counties@data$Withdrawals_2011<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2011"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2011"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2011"])],NA)

#---Year 2012----#
VA_Counties@data$Discharges_2012<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2012"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2012"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2012"])],NA)
VA_Counties@data$Withdrawals_2012<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2012"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2012"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2012"])],NA)

#---Year 2013----#
VA_Counties@data$Discharges_2013<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2013"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2013"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2013"])],NA)
VA_Counties@data$Withdrawals_2013<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2013"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2013"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2013"])],NA)

#---Year 2014----#
VA_Counties@data$Discharges_2014<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2014"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2014"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2014"])],NA)
VA_Counties@data$Withdrawals_2014<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2014"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2014"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2014"])],NA)

#---Year 2015----#
VA_Counties@data$Discharges_2015<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2015"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2015"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2015"])],NA)
VA_Counties@data$Withdrawals_2015<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2015"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2015"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2015"])],NA)

#---Year 2016----#
VA_Counties@data$Discharges_2016<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2016"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2016"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2016"])],NA)
VA_Counties@data$Withdrawals_2016<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2016"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2016"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2016"])],NA)

#---Year 2017----#
VA_Counties@data$Discharges_2017<-ifelse(VA_Counties@data$FIPS%in%County_Discharges$FIPS[County_Discharges$Year=="2017"],County_Discharges$Discharge_MGD[County_Discharges$Year=="2017"][match(VA_Counties@data$FIPS,County_Discharges$FIPS[County_Discharges$Year=="2017"])],NA)
VA_Counties@data$Withdrawals_2017<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals$FIPS[County_Withdrawals$Year=="2017"],County_Withdrawals$Withdrawals_MGD[County_Withdrawals$Year=="2017"][match(VA_Counties@data$FIPS,County_Withdrawals$FIPS[County_Withdrawals$Year=="2017"])],NA)

###########################################################################################################################################
#######################################################Net Water Balance and Consumtpive Use###############################################

#---Year 2010----#

#--With Transfers---#
#transfers have sign--negative mean water is leaving, positive means water is coming in 

VA_Counties@data$NetWB_2010_t<-(ifelse(is.na(VA_Counties@data$Discharges_2010),0,VA_Counties@data$Discharges_2010))+(ifelse(is.na(VA_Counties@data$transferred_2010),0,VA_Counties@data$transferred_2010))-(ifelse(is.na(VA_Counties@data$Withdrawals_2010),0,VA_Counties@data$Withdrawals_2010))

VA_Counties@data$NetWB_2010_t<-ifelse(is.na(VA_Counties@data$Discharges_2010)&is.na(VA_Counties@data$Withdrawals_2010)&is.na(VA_Counties@data$transferred_2010),NA,VA_Counties@data$NetWB_2010_t)

VA_Counties@data$Consumption_2010_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2010),0,VA_Counties@data$Withdrawals_2010))+(ifelse(is.na(VA_Counties@data$waterout_2010),0,-VA_Counties@data$waterout_2010)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2010),0,VA_Counties@data$Discharges_2010))+(ifelse(is.na(VA_Counties@data$waterin_2010),0,VA_Counties@data$waterin_2010)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2010),0,VA_Counties@data$Withdrawals_2010))+(ifelse(is.na(VA_Counties@data$waterout_2010),0,-VA_Counties@data$waterout_2010)))

VA_Counties@data$Consumption_2010_t<-ifelse(is.nan(VA_Counties@data$Consumption_2010_t)|is.infinite(VA_Counties@data$Consumption_2010_t),NA,VA_Counties@data$Consumption_2010_t)
#--Without Transfers---#

VA_Counties@data$NetWB_2010<-(ifelse(is.na(VA_Counties@data$Discharges_2010),0,VA_Counties@data$Discharges_2010))-(ifelse(is.na(VA_Counties@data$Withdrawals_2010),0,VA_Counties@data$Withdrawals_2010))

VA_Counties@data$NetWB_2010<-ifelse(is.na(VA_Counties@data$Discharges_2010)&is.na(VA_Counties@data$Withdrawals_2010),NA,VA_Counties@data$NetWB_2010)

VA_Counties@data$Consumption_2010<-((ifelse(is.na(VA_Counties@data$Withdrawals_2010),0,VA_Counties@data$Withdrawals_2010))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2010),0,VA_Counties@data$Discharges_2010)))/(ifelse(is.na(VA_Counties@data$Withdrawals_2010),0,VA_Counties@data$Withdrawals_2010))

VA_Counties@data$Consumption_2010<-ifelse(is.nan(VA_Counties@data$Consumption_2010)|is.infinite(VA_Counties@data$Consumption_2010),NA,VA_Counties@data$Consumption_2010)
#---Year 2011----#

#----With transfers---#
VA_Counties@data$NetWB_2011_t<-(ifelse(is.na(VA_Counties@data$Discharges_2011),0,VA_Counties@data$Discharges_2011))+(ifelse(is.na(VA_Counties@data$transferred_2011),0,VA_Counties@data$transferred_2011))-(ifelse(is.na(VA_Counties@data$Withdrawals_2011),0,VA_Counties@data$Withdrawals_2011))

VA_Counties@data$NetWB_2011_t<-ifelse(is.na(VA_Counties@data$Discharges_2011)&is.na(VA_Counties@data$Withdrawals_2011)&is.na(VA_Counties@data$transferred_2011),NA,VA_Counties@data$NetWB_2011_t)

VA_Counties@data$Consumption_2011_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2011),0,VA_Counties@data$Withdrawals_2011))+(ifelse(is.na(VA_Counties@data$waterout_2011),0,-VA_Counties@data$waterout_2011)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2011),0,VA_Counties@data$Discharges_2011))+(ifelse(is.na(VA_Counties@data$waterin_2011),0,VA_Counties@data$waterin_2011)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2011),0,VA_Counties@data$Withdrawals_2011))+(ifelse(is.na(VA_Counties@data$waterout_2011),0,-VA_Counties@data$waterout_2011)))

VA_Counties@data$Consumption_2011_t<-ifelse(is.nan(VA_Counties@data$Consumption_2011_t)|is.infinite(VA_Counties@data$Consumption_2011_t),NA,VA_Counties@data$Consumption_2011_t)
#---Without Transfers----#
VA_Counties@data$NetWB_2011<-(ifelse(is.na(VA_Counties@data$Discharges_2011),0,VA_Counties@data$Discharges_2011))-(ifelse(is.na(VA_Counties@data$Withdrawals_2011),0,VA_Counties@data$Withdrawals_2011))

VA_Counties@data$NetWB_2011<-ifelse(is.na(VA_Counties@data$Discharges_2011)&is.na(VA_Counties@data$Withdrawals_2011),NA,VA_Counties@data$NetWB_2011)

VA_Counties@data$Consumption_2011<-((ifelse(is.na(VA_Counties@data$Withdrawals_2011),0,VA_Counties@data$Withdrawals_2011))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2011),0,VA_Counties@data$Discharges_2011)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2011),0,VA_Counties@data$Withdrawals_2011))

VA_Counties@data$Consumption_2011<-ifelse(is.nan(VA_Counties@data$Consumption_2011)|is.infinite(VA_Counties@data$Consumption_2011),NA,VA_Counties@data$Consumption_2011)
#---Year 2012----#

#---With Transfers---#
VA_Counties@data$NetWB_2012_t<-(ifelse(is.na(VA_Counties@data$Discharges_2012),0,VA_Counties@data$Discharges_2012))+(ifelse(is.na(VA_Counties@data$transferred_2012),0,VA_Counties@data$transferred_2012))-(ifelse(is.na(VA_Counties@data$Withdrawals_2012),0,VA_Counties@data$Withdrawals_2012))

VA_Counties@data$NetWB_2012_t<-ifelse(is.na(VA_Counties@data$Discharges_2012)&is.na(VA_Counties@data$Withdrawals_2012)&is.na(VA_Counties@data$transferred_2012),NA,VA_Counties@data$NetWB_2012_t)

VA_Counties@data$Consumption_2012_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2012),0,VA_Counties@data$Withdrawals_2012))+(ifelse(is.na(VA_Counties@data$waterout_2012),0,-VA_Counties@data$waterout_2012)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2012),0,VA_Counties@data$Discharges_2012))+(ifelse(is.na(VA_Counties@data$waterin_2012),0,VA_Counties@data$waterin_2012)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2012),0,VA_Counties@data$Withdrawals_2012))+(ifelse(is.na(VA_Counties@data$waterout_2012),0,-VA_Counties@data$waterout_2012)))


VA_Counties@data$Consumption_2012_t<-ifelse(is.nan(VA_Counties@data$Consumption_2012_t)|is.infinite(VA_Counties@data$Consumption_2012_t),NA,VA_Counties@data$Consumption_2012_t)

#---Without Transfers---#
VA_Counties@data$NetWB_2012<-(ifelse(is.na(VA_Counties@data$Discharges_2012),0,VA_Counties@data$Discharges_2012))-(ifelse(is.na(VA_Counties@data$Withdrawals_2012),0,VA_Counties@data$Withdrawals_2012))

VA_Counties@data$NetWB_2012<-ifelse(is.na(VA_Counties@data$Discharges_2012)&is.na(VA_Counties@data$Withdrawals_2012),NA,VA_Counties@data$NetWB_2012)

VA_Counties@data$Consumption_2012<-((ifelse(is.na(VA_Counties@data$Withdrawals_2012),0,VA_Counties@data$Withdrawals_2012))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2012),0,VA_Counties@data$Discharges_2012)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2012),0,VA_Counties@data$Withdrawals_2012))

VA_Counties@data$Consumption_2012<-ifelse(is.nan(VA_Counties@data$Consumption_2012)|is.infinite(VA_Counties@data$Consumption_2012),NA,VA_Counties@data$Consumption_2012)
#---Year 2013----#

#---With Transfers---#
VA_Counties@data$NetWB_2013_t<-(ifelse(is.na(VA_Counties@data$Discharges_2013),0,VA_Counties@data$Discharges_2013))+(ifelse(is.na(VA_Counties@data$transferred_2013),0,VA_Counties@data$transferred_2013))-(ifelse(is.na(VA_Counties@data$Withdrawals_2013),0,VA_Counties@data$Withdrawals_2013))

VA_Counties@data$NetWB_2013_t<-ifelse(is.na(VA_Counties@data$Discharges_2013)&is.na(VA_Counties@data$Withdrawals_2013)&is.na(VA_Counties@data$transferred_2013),NA,VA_Counties@data$NetWB_2013_t)

VA_Counties@data$Consumption_2013_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2013),0,VA_Counties@data$Withdrawals_2013))+(ifelse(is.na(VA_Counties@data$waterout_2013),0,-VA_Counties@data$waterout_2013)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2013),0,VA_Counties@data$Discharges_2013))+(ifelse(is.na(VA_Counties@data$waterin_2013),0,VA_Counties@data$waterin_2013)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2013),0,VA_Counties@data$Withdrawals_2013))+(ifelse(is.na(VA_Counties@data$waterout_2013),0,-VA_Counties@data$waterout_2013)))

VA_Counties@data$Consumption_2013_t<-ifelse(is.nan(VA_Counties@data$Consumption_2013_t)|is.infinite(VA_Counties@data$Consumption_2013_t),NA,VA_Counties@data$Consumption_2013_t)

#---Without Transfers---#
VA_Counties@data$NetWB_2013<-(ifelse(is.na(VA_Counties@data$Discharges_2013),0,VA_Counties@data$Discharges_2013))-(ifelse(is.na(VA_Counties@data$Withdrawals_2013),0,VA_Counties@data$Withdrawals_2013))

VA_Counties@data$NetWB_2013<-ifelse(is.na(VA_Counties@data$Discharges_2013)&is.na(VA_Counties@data$Withdrawals_2013),NA,VA_Counties@data$NetWB_2013)

VA_Counties@data$Consumption_2013<-((ifelse(is.na(VA_Counties@data$Withdrawals_2013),0,VA_Counties@data$Withdrawals_2013))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2013),0,VA_Counties@data$Discharges_2013)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2013),0,VA_Counties@data$Withdrawals_2013))

VA_Counties@data$Consumption_2013<-ifelse(is.nan(VA_Counties@data$Consumption_2013)|is.infinite(VA_Counties@data$Consumption_2013),NA,VA_Counties@data$Consumption_2013)

#---Year 2014----#

#---With Transfers---#
VA_Counties@data$NetWB_2014_t<-(ifelse(is.na(VA_Counties@data$Discharges_2014),0,VA_Counties@data$Discharges_2014))+(ifelse(is.na(VA_Counties@data$transferred_2014),0,VA_Counties@data$transferred_2014))-(ifelse(is.na(VA_Counties@data$Withdrawals_2014),0,VA_Counties@data$Withdrawals_2014))

VA_Counties@data$NetWB_2014_t<-ifelse(is.na(VA_Counties@data$Discharges_2014)&is.na(VA_Counties@data$Withdrawals_2014)&is.na(VA_Counties@data$transferred_2014),NA,VA_Counties@data$NetWB_2014_t)

VA_Counties@data$Consumption_2014_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2014),0,VA_Counties@data$Withdrawals_2014))+(ifelse(is.na(VA_Counties@data$waterout_2014),0,-VA_Counties@data$waterout_2014)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2014),0,VA_Counties@data$Discharges_2014))+(ifelse(is.na(VA_Counties@data$waterin_2014),0,VA_Counties@data$waterin_2014)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2014),0,VA_Counties@data$Withdrawals_2014))+(ifelse(is.na(VA_Counties@data$waterout_2014),0,-VA_Counties@data$waterout_2014)))

VA_Counties@data$Consumption_2014_t<-ifelse(is.nan(VA_Counties@data$Consumption_2014_t)|is.infinite(VA_Counties@data$Consumption_2014_t),NA,VA_Counties@data$Consumption_2014_t)

#---Without Transfers---#
VA_Counties@data$NetWB_2014<-(ifelse(is.na(VA_Counties@data$Discharges_2014),0,VA_Counties@data$Discharges_2014))-(ifelse(is.na(VA_Counties@data$Withdrawals_2014),0,VA_Counties@data$Withdrawals_2014))

VA_Counties@data$NetWB_2014<-ifelse(is.na(VA_Counties@data$Discharges_2014)&is.na(VA_Counties@data$Withdrawals_2014),NA,VA_Counties@data$NetWB_2014)

VA_Counties@data$Consumption_2014<-((ifelse(is.na(VA_Counties@data$Withdrawals_2014),0,VA_Counties@data$Withdrawals_2014))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2014),0,VA_Counties@data$Discharges_2014)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2014),0,VA_Counties@data$Withdrawals_2014))

VA_Counties@data$Consumption_2014<-ifelse(is.nan(VA_Counties@data$Consumption_2014)|is.infinite(VA_Counties@data$Consumption_2014),NA,VA_Counties@data$Consumption_2014)
#---Year 2015----#

#---With Transfers---#
VA_Counties@data$NetWB_2015_t<-(ifelse(is.na(VA_Counties@data$Discharges_2015),0,VA_Counties@data$Discharges_2015))+(ifelse(is.na(VA_Counties@data$transferred_2015),0,VA_Counties@data$transferred_2015))-(ifelse(is.na(VA_Counties@data$Withdrawals_2015),0,VA_Counties@data$Withdrawals_2015))

VA_Counties@data$NetWB_2015_t<-ifelse(is.na(VA_Counties@data$Discharges_2015)&is.na(VA_Counties@data$Withdrawals_2015)&is.na(VA_Counties@data$transferred_2015),NA,VA_Counties@data$NetWB_2015_t)

VA_Counties@data$Consumption_2015_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2015),0,VA_Counties@data$Withdrawals_2015))+(ifelse(is.na(VA_Counties@data$waterout_2015),0,-VA_Counties@data$waterout_2015)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2015),0,VA_Counties@data$Discharges_2015))+(ifelse(is.na(VA_Counties@data$waterin_2015),0,VA_Counties@data$waterin_2015)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2015),0,VA_Counties@data$Withdrawals_2015))+(ifelse(is.na(VA_Counties@data$waterout_2015),0,-VA_Counties@data$waterout_2015)))

VA_Counties@data$Consumption_2015_t<-ifelse(is.nan(VA_Counties@data$Consumption_2015_t)|is.infinite(VA_Counties@data$Consumption_2015_t),NA,VA_Counties@data$Consumption_2015_t)

#---Without Transfers---#
VA_Counties@data$NetWB_2015<-(ifelse(is.na(VA_Counties@data$Discharges_2015),0,VA_Counties@data$Discharges_2015))-(ifelse(is.na(VA_Counties@data$Withdrawals_2015),0,VA_Counties@data$Withdrawals_2015))

VA_Counties@data$NetWB_2015<-ifelse(is.na(VA_Counties@data$Discharges_2015)&is.na(VA_Counties@data$Withdrawals_2015),NA,VA_Counties@data$NetWB_2015)

VA_Counties@data$Consumption_2015<-((ifelse(is.na(VA_Counties@data$Withdrawals_2015),0,VA_Counties@data$Withdrawals_2015))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2015),0,VA_Counties@data$Discharges_2015)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2015),0,VA_Counties@data$Withdrawals_2015))

VA_Counties@data$Consumption_2015<-ifelse(is.nan(VA_Counties@data$Consumption_2015)|is.infinite(VA_Counties@data$Consumption_2015),NA,VA_Counties@data$Consumption_2015)

#---Year 2016----#

#---With Transfers---#
VA_Counties@data$NetWB_2016_t<-(ifelse(is.na(VA_Counties@data$Discharges_2016),0,VA_Counties@data$Discharges_2016))+(ifelse(is.na(VA_Counties@data$transferred_2016),0,VA_Counties@data$transferred_2016))-(ifelse(is.na(VA_Counties@data$Withdrawals_2016),0,VA_Counties@data$Withdrawals_2016))

VA_Counties@data$NetWB_2016_t<-ifelse(is.na(VA_Counties@data$Discharges_2016)&is.na(VA_Counties@data$Withdrawals_2016)&is.na(VA_Counties@data$transferred_2016),NA,VA_Counties@data$NetWB_2016_t)

VA_Counties@data$Consumption_2016_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2016),0,VA_Counties@data$Withdrawals_2016))+(ifelse(is.na(VA_Counties@data$waterout_2016),0,-VA_Counties@data$waterout_2016)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2016),0,VA_Counties@data$Discharges_2016))+(ifelse(is.na(VA_Counties@data$waterin_2016),0,VA_Counties@data$waterin_2016)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2016),0,VA_Counties@data$Withdrawals_2016))+(ifelse(is.na(VA_Counties@data$waterout_2016),0,-VA_Counties@data$waterout_2016)))

VA_Counties@data$Consumption_2016_t<-ifelse(is.nan(VA_Counties@data$Consumption_2016_t)|is.infinite(VA_Counties@data$Consumption_2016_t),NA,VA_Counties@data$Consumption_2016_t)

#---Without Transfers---#
VA_Counties@data$NetWB_2016<-(ifelse(is.na(VA_Counties@data$Discharges_2016),0,VA_Counties@data$Discharges_2016))-(ifelse(is.na(VA_Counties@data$Withdrawals_2016),0,VA_Counties@data$Withdrawals_2016))

VA_Counties@data$NetWB_2016<-ifelse(is.na(VA_Counties@data$Discharges_2016)&is.na(VA_Counties@data$Withdrawals_2016),NA,VA_Counties@data$NetWB_2016)

VA_Counties@data$Consumption_2016<-((ifelse(is.na(VA_Counties@data$Withdrawals_2016),0,VA_Counties@data$Withdrawals_2016))-
                               (ifelse(is.na(VA_Counties@data$Discharges_2016),0,VA_Counties@data$Discharges_2016)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2016),0,VA_Counties@data$Withdrawals_2016))

VA_Counties@data$Consumption_2016<-ifelse(is.nan(VA_Counties@data$Consumption_2016)|is.infinite(VA_Counties@data$Consumption_2016),NA,VA_Counties@data$Consumption_2016)

#---Year 2017----#

#---With Transfers---#
VA_Counties@data$NetWB_2017_t<-(ifelse(is.na(VA_Counties@data$Discharges_2017),0,VA_Counties@data$Discharges_2017))+(ifelse(is.na(VA_Counties@data$transferred_2017),0,VA_Counties@data$transferred_2017))-(ifelse(is.na(VA_Counties@data$Withdrawals_2017),0,VA_Counties@data$Withdrawals_2017))

VA_Counties@data$NetWB_2017_t<-ifelse(is.na(VA_Counties@data$Discharges_2017)&is.na(VA_Counties@data$Withdrawals_2017)&is.na(VA_Counties@data$transferred_2017),NA,VA_Counties@data$NetWB_2017_t)

VA_Counties@data$Consumption_2017_t<-(((ifelse(is.na(VA_Counties@data$Withdrawals_2017),0,VA_Counties@data$Withdrawals_2017))+(ifelse(is.na(VA_Counties@data$waterout_2017),0,-VA_Counties@data$waterout_2017)))-
                                 (((ifelse(is.na(VA_Counties@data$Discharges_2017),0,VA_Counties@data$Discharges_2017))+(ifelse(is.na(VA_Counties@data$waterin_2017),0,VA_Counties@data$waterin_2017)))))/
  ((ifelse(is.na(VA_Counties@data$Withdrawals_2017),0,VA_Counties@data$Withdrawals_2017))+(ifelse(is.na(VA_Counties@data$waterout_2017),0,-VA_Counties@data$waterout_2017)))

VA_Counties@data$Consumption_2017_t<-ifelse(is.nan(VA_Counties@data$Consumption_2017_t)|is.infinite(VA_Counties@data$Consumption_2017_t),NA,VA_Counties@data$Consumption_2017_t)

#---Without Transfers---#
VA_Counties@data$NetWB_2017<-(ifelse(is.na(VA_Counties@data$Discharges_2017),0,VA_Counties@data$Discharges_2017))-(ifelse(is.na(VA_Counties@data$Withdrawals_2017),0,VA_Counties@data$Withdrawals_2017))

VA_Counties@data$NetWB_2017<-ifelse(is.na(VA_Counties@data$Discharges_2017)&is.na(VA_Counties@data$Withdrawals_2017),NA,VA_Counties@data$NetWB_2017)

VA_Counties@data$Consumption_2017<-((ifelse(is.na(VA_Counties@data$Withdrawals_2017),0,VA_Counties@data$Withdrawals_2017))-(ifelse(is.na(VA_Counties@data$Discharges_2017),0,VA_Counties@data$Discharges_2017)))/
  (ifelse(is.na(VA_Counties@data$Withdrawals_2017),0,VA_Counties@data$Withdrawals_2017))

VA_Counties@data$Consumption_2017<-ifelse(is.nan(VA_Counties@data$Consumption_2017)|is.infinite(VA_Counties@data$Consumption_2017),NA,VA_Counties@data$Consumption_2017)

VA_Counties_glimpse<-VA_Counties@data

#################################################################################################
#-------------Long Term Averages 2010-2017--------------#

#----Discharges-----#

#Be careful for NA values#
for (i in 1:length(VA_Counties@data$COUNTYNS)){
  VA_Counties@data$Discharge_sum[i]<-(ifelse(is.na(VA_Counties@data$Discharges_2010[i]),0,VA_Counties@data$Discharges_2010[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2011[i]),0,VA_Counties@data$Discharges_2011[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2012[i]),0,VA_Counties@data$Discharges_2012[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2013[i]),0,VA_Counties@data$Discharges_2013[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2014[i]),0,VA_Counties@data$Discharges_2014[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2015[i]),0,VA_Counties@data$Discharges_2015[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2016[i]),0,VA_Counties@data$Discharges_2016[i])+
                                 ifelse(is.na(VA_Counties@data$Discharges_2017[i]),0,VA_Counties@data$Discharges_2017[i]))
}
VA_Counties@data$Discharge_ave<-VA_Counties@data$Discharge_sum/8
VA_Counties@data$Discharge_ave<-ifelse(VA_Counties@data$Discharge_ave==0,NA,VA_Counties@data$Discharge_ave)

#----Withdrawals----#
for (i in 1:length(VA_Counties@data$COUNTYNS)){
  VA_Counties@data$Withdrawal_sum[i]<-(ifelse(is.na(VA_Counties@data$Withdrawals_2010[i]),0,VA_Counties@data$Withdrawals_2010[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2011[i]),0,VA_Counties@data$Withdrawals_2011[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2012[i]),0,VA_Counties@data$Withdrawals_2012[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2013[i]),0,VA_Counties@data$Withdrawals_2013[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2014[i]),0,VA_Counties@data$Withdrawals_2014[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2015[i]),0,VA_Counties@data$Withdrawals_2015[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2016[i]),0,VA_Counties@data$Withdrawals_2016[i])+
                                  ifelse(is.na(VA_Counties@data$Withdrawals_2017[i]),0,VA_Counties@data$Withdrawals_2017[i]))
}
VA_Counties@data$Withdrawal_ave<-VA_Counties@data$Withdrawal_sum/8
VA_Counties@data$Withdrawal_ave<-ifelse(VA_Counties@data$Withdrawal_ave==0,NA,VA_Counties@data$Withdrawal_ave)


#----Net Water Balance-----#
for (i in 1:length(VA_Counties@data$COUNTYNS)){
  VA_Counties@data$NetWB_sum[i]<-(ifelse(is.na(VA_Counties@data$NetWB_2010[i]),0,VA_Counties@data$NetWB_2010[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2011[i]),0,VA_Counties@data$NetWB_2011[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2012[i]),0,VA_Counties@data$NetWB_2012[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2013[i]),0,VA_Counties@data$NetWB_2013[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2014[i]),0,VA_Counties@data$NetWB_2014[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2015[i]),0,VA_Counties@data$NetWB_2015[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2016[i]),0,VA_Counties@data$NetWB_2016[i])+
                             ifelse(is.na(VA_Counties@data$NetWB_2017[i]),0,VA_Counties@data$NetWB_2017[i]))
}
VA_Counties@data$NetWB_ave<-VA_Counties@data$NetWB_sum/8
VA_Counties@data$NetWB_ave<-ifelse(VA_Counties@data$NetWB_ave==0,NA,VA_Counties@data$NetWB_ave)

#----Consumption----#

for (i in 1:length(VA_Counties@data$COUNTYNS)){
  VA_Counties@data$Consumption_sum[i]<-(ifelse(is.na(VA_Counties@data$Consumption_2010[i]),0,VA_Counties@data$Consumption_2010[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2011[i]),0,VA_Counties@data$Consumption_2011[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2012[i]),0,VA_Counties@data$Consumption_2012[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2013[i]),0,VA_Counties@data$Consumption_2013[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2014[i]),0,VA_Counties@data$Consumption_2014[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2015[i]),0,VA_Counties@data$Consumption_2015[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2016[i]),0,VA_Counties@data$Consumption_2016[i])+
                                   ifelse(is.na(VA_Counties@data$Consumption_2017[i]),0,VA_Counties@data$Consumption_2017[i]))
}

VA_Counties@data$Consumption_ave<-VA_Counties@data$Consumption_sum/8
VA_Counties@data$Consumption_ave<-ifelse(VA_Counties@data$Consumption_ave==0,NA,VA_Counties@data$Consumption_ave)

#-----------------With Transfers-------------------------#
for (i in 1:length(VA_Counties@data$COUNTYNS)){
  VA_Counties@data$NetWB_t_sum[i]<-(ifelse(is.na(VA_Counties@data$NetWB_2010_t[i]),0,VA_Counties@data$NetWB_2010_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2011_t[i]),0,VA_Counties@data$NetWB_2011_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2012_t[i]),0,VA_Counties@data$NetWB_2012_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2013_t[i]),0,VA_Counties@data$NetWB_2013_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2014_t[i]),0,VA_Counties@data$NetWB_2014_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2015_t[i]),0,VA_Counties@data$NetWB_2015_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2016_t[i]),0,VA_Counties@data$NetWB_2016_t[i])+
                               ifelse(is.na(VA_Counties@data$NetWB_2017_t[i]),0,VA_Counties@data$NetWB_2017_t[i]))
}
VA_Counties@data$NetWB_t_ave<-VA_Counties@data$NetWB_t_sum/8
VA_Counties@data$NetWB_t_ave<-ifelse(VA_Counties@data$NetWB_t_ave==0,NA,VA_Counties@data$NetWB_t_ave)

for (i in 1:length(VA_Counties@data$COUNTYNS)){
  VA_Counties@data$Consumption_t_sum[i]<-(ifelse(is.na(VA_Counties@data$Consumption_2010_t[i]),0,VA_Counties@data$Consumption_2010_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2011_t[i]),0,VA_Counties@data$Consumption_2011_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2012_t[i]),0,VA_Counties@data$Consumption_2012_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2013_t[i]),0,VA_Counties@data$Consumption_2013_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2014_t[i]),0,VA_Counties@data$Consumption_2014_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2015_t[i]),0,VA_Counties@data$Consumption_2015_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2016_t[i]),0,VA_Counties@data$Consumption_2016_t[i])+
                                     ifelse(is.na(VA_Counties@data$Consumption_2017_t[i]),0,VA_Counties@data$Consumption_2017_t[i]))
}

VA_Counties@data$Consumption_t_ave<-VA_Counties@data$Consumption_t_sum/8
VA_Counties@data$Consumption_t_ave<-ifelse(VA_Counties@data$Consumption_t_ave==0,NA,VA_Counties@data$Consumption_t_ave)

VA_Counties_glimpse<-VA_Counties@data
VA_Counties_glimpse[10:93]<-sapply(VA_Counties_glimpse[10:93],as.numeric)
VA_Counties_glimpse[10:93]<-round(VA_Counties_glimpse[10:93], digits=2)

rm(VA_Counties_Facilities)

#####################################################################################
#-----------------------------Visualization-----------------------------------------#
VA_Counties.df<-broom::tidy(VA_Counties)
VA_Counties$polyID<-sapply(slot(VA_Counties,"polygons"), function(x) slot(x, "ID"))
VA_Counties.df<-merge(VA_Counties.df, VA_Counties, by.x="id", by.y="polyID")

#----------------------------------------------------------------------------------------------------------#

#-----County Labels-----#
VA_Counties_Centroids<-as.data.frame(coordinates(VA_Counties))
names(VA_Counties_Centroids)<-c("Longitude","Latitude")
VA_Counties_Centroids$FIPS<-VA_Counties_glimpse$FIPS
VA_Counties_glimpse<-subset(VA_Counties_glimpse,select=-c(1,4,5,6,7,8,9))
VA_Counties_Centroids<-merge(VA_Counties_Centroids,VA_Counties_glimpse,by="FIPS")

ggplot()+
  geom_polygon(data=VA_Counties,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  geom_text(data=VA_Counties_Centroids,aes(x=Longitude,y=Latitude,label=County),size=1)+
  scale_colour_manual(values=c("#252525"))+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


#---Distribution of Discharging Outfalls in County-----#

cut(VA_Counties.df$Discharge_ave,breaks=c(0,
                                          (quantile(VA_Counties.df$Discharge_ave,c(1/6),na.rm=T)),
                                          (quantile(VA_Counties.df$Discharge_ave,c(2/6),na.rm=T)),
                                          (quantile(VA_Counties.df$Discharge_ave,c(3/6),na.rm=T)),
                                          (quantile(VA_Counties.df$Discharge_ave,c(4/6),na.rm=T)),
                                          (quantile(VA_Counties.df$Discharge_ave,c(5/6),na.rm=T)),
                                          (quantile(VA_Counties.df$Discharge_ave,c(1),na.rm=T))),include.lowest=T)

Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")

ggplot()+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group,
        fill= cut(VA_Counties.df$Discharge_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Discharge_ave,na.rm=T)),include.lowest=T), colour=""))+
  scale_fill_manual(name="Summed Discharge (MGD)", values=(Dis_Discrete), 
                    labels=c(paste("<",1),
                             paste(1,"-",5),
                             paste(5,"-",10),
                             paste(10,"-",25),
                             paste(25,"-",50),
                             paste(50,"<")),
                    na.value="transparent",drop=FALSE)+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude, shape="Outfall"),
             size=1.25,colour="#252525")+
  scale_shape_manual(name="", values=17)+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
  labs(title = "Average Annual Total Discharge (MGD) 2010-2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)

#---Distribution of Withdrawing Sources in HUC8-----#
VWUDS_2010_2017.test<-VWUDS_2010_2017@data
Mean_Withdrawal<-VWUDS_2010_2017.test%>%group_by(DEQ.ID.of.Source)%>%summarise(Mean_Withdrawal=mean(Withdrawals_MGD, na.rm=T))
VWUDS_2010_2017.test<-merge(VWUDS_2010_2017.test,Mean_Withdrawal, by="DEQ.ID.of.Source",all.x=T)


With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")

cut(VA_Counties.df$Withdrawal_ave,breaks=c(0,
                                    (quantile(VA_Counties.df$Withdrawal_ave,c(1/6),na.rm=T)),
                                    (quantile(VA_Counties.df$Withdrawal_ave,c(2/6),na.rm=T)),
                                    (quantile(VA_Counties.df$Withdrawal_ave,c(3/6),na.rm=T)),
                                    (quantile(VA_Counties.df$Withdrawal_ave,c(4/6),na.rm=T)),
                                    (quantile(VA_Counties.df$Withdrawal_ave,c(5/6),na.rm=T)),
                                    (quantile(VA_Counties.df$Withdrawal_ave,c(1),na.rm=T))),include.lowest=T)

ggplot()+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group,
        fill= cut(VA_Counties.df$Withdrawal_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Withdrawal_ave,na.rm=TRUE)),include.lowest=T), colour=""))+
  scale_fill_manual(name="Summed Withdrawal (MGD)", values=(With_Discrete),
                    labels=c(paste("<",1),
                             paste(1,"-",5),
                             paste(5,"-",10),
                             paste(10,"-",25),
                             paste(25,"-",50),
                             paste(50,"<")),
                    na.value="transparent",drop=FALSE)+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude, shape="Point Source"),
             size=1.54,colour="#252525")+
  scale_shape_manual(name="", values=20)+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
  labs(title = "Average Annual Summed Withdrawal (MGD) 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#######################################################################################################################
#-----------------------------Without Transfers-----------------------------------------------------------------------#
#Convert Spatialpolygonsdataframe to dataframe 

cut(VA_Counties.df$NetWB_ave,breaks=c(quantile(VA_Counties.df$NetWB_ave,c(0.0),na.rm=T),
                               quantile(VA_Counties.df$NetWB_ave,c(0.20),na.rm=T),
                               quantile(VA_Counties.df$NetWB_ave,c(0.40),na.rm=T),
                               quantile(VA_Counties.df$NetWB_ave,c(0.70),na.rm=T),
                               0,
                               quantile(VA_Counties.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T)

NWB_Discrete<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#2b8cbe")

ggplot()+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group,
        fill= cut(VA_Counties.df$NetWB_ave,breaks=c(quantile(VA_Counties.df$NetWB_ave,c(0),na.rm=T),-250,-150,-10,0,quantile(VA_Counties.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
  scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete,
                    labels=c(paste(-1250,"-",-250),
                             paste(-250,"-",-150),
                             paste(-150,"-",-10),
                             paste(-10,"-",0),
                             paste(0,"<")),
                    na.value="transparent")+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
  labs(title = "Average Net Water Balance (MGD) 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


#-----Consumption-----#

CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")

CU<-ggplot()+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group,
        fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
  scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                    labels=c(paste(round(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),digits=2),"-",0),
                             paste(0,"-",0.25),
                             paste(0.25,"-",0.5),
                             paste(0.5,"-",0.75),
                             paste(0.75,"-",1)),
                    na.value="transparent", drop=FALSE)+
  geom_polygon(
    data=VA_Counties.df,
    aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude, shape="Outfall"),
             size=1.75,colour="#252525")+
  scale_shape_manual(name="", values=17)+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude, size="Withdrawing Source"),
             colour="#252525")+
  scale_size_manual(name="", values=1.75)+
  guides(fill=guide_legend(order=1),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
         shape=guide_legend(order=3),
         size=guide_legend(order=4))+
  labs(title = "Average Consumption Coefficient 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#######################################################################################################################
#-----------------------------Transfers----------------------------#

