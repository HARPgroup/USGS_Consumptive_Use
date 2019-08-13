###########################################################################################################################################
#-------------------------------------------Estimating Consumptive Use in Virginia Counties-----------------------------------------------#

##This code calculates consumptive use by performing a water balance (In-Out=Change in Storage)
#Inputs are defined as the discharges/return flows from NPDES permitted facilities (ECHO Database)
#Outputs are the withdrawals from surface water found in the Virginia Wateruse Data System (VWUDS)
#The change in storage refers to transfers between facilities (releases and deliveries)--This is calculated in this script

###########################################################################################################################################
#-----------------------------------------------------------Library Load------------------------------------------------------------------#

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
library(ggrepel)
library(ggpubr)

options(scipen=999) #Disable scientific notation
options(digits = 9)
memory.limit(size=100000000)

###########################################################################################################################################
#-------------------------------------------County and Virginia Shapefile Manipulation----------------------------------------------------#

#Load databases and extract required layers
VA_Counties<-rgdal::readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/tlgdb_2015_a_51_va.gdb',layer="County")

#Reproject shapefiles to NAD83=EPSG Code of 4269
VA_Counties<-sp::spTransform(VA_Counties, CRS("+init=epsg:4269"))
names(VA_Counties@data)[2]<-c("FIPS")
names(VA_Counties@data)[3]<-c("County")

#Create County Dataframe that will be used in future overlay processes
VA_Counties_Overlay<-VA_Counties #Keep integrity of spatial dataframe
VA_Counties_Overlay@data<-VA_Counties_Overlay@data[,c(2,3)] 
names(VA_Counties_Overlay@data)<-c("FIPS","County")


###########################################################################################################################################
#---------------------------------------------------Calculating Transfers-----------------------------------------------------------------#

# There are two different types of Transfers: Deliveries and Releases (both have TO and FROM components)
# This is because VA Hydro stores transfers at both the recieving and departing facilities
# Therefore, we will keep them separate during calculations and refer to transfers as deliveries and releases in both the TO and FROM lists

#-----------------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------Delivery Transfers (TO)----------------------------------------------------------------#

# Monthly Deliveries from 01/01/2010-12/31/2017
# http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_4

GET('http://deq1.bse.vt.edu/d.bet/echo_transfer_to',write_disk(del <- tempfile(fileext = ".csv")))

deliveries_func<- function(temp_dir){
  deliveries<-read.csv(temp_dir)
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
  #Spatially overlays County boundaries on dFrom such that each FROM transfer is labeled by origin county
  dFrom<-deliveries[!(is.na(deliveries$geomFlat)&is.na(deliveries$geomFlon)),]
  dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
  VA_Counties_Facilities<-over(dFrom,VA_Counties_Overlay)#Spatial overlay
  dFrom@data$County<-VA_Counties_Facilities$County
  dFrom@data$FIPS<-VA_Counties_Facilities$FIPS
  
  ####TO Delivery Transfers####
  #Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
  #Spatially overlays county boundaries on dTo such that each TO transfer is labeled by origin county
  dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
  dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
  VA_Counties_Facilities<-over(dTo,VA_Counties_Overlay)#Spatial overlay
  dTo@data$County<-VA_Counties_Facilities$County
  dTo@data$FIPS<-VA_Counties_Facilities$FIPS
  
  ####Determine if TO Transfers are leaving watershed boundaries####
  #Need to identify if transfers are leaving and entering the same county. If so, these may be ignored
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
  
  assign("delf",delf,envir=.GlobalEnv)
  assign("delt",delt,envir=.GlobalEnv)
  assign("deliveries",deliveries,envir=.GlobalEnv)
}
deliveries_func(del)

#-----------------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------Release Transfers (FROM)---------------------------------------------------------------#

# Ignoring repeat hydroids (same transfer reported from both recieving and transfer facility),
# repeat the above steps using the remaining FROM transfers available at:
# http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_5

#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_5
GET('http://deq1.bse.vt.edu/d.bet/echo_transfer_from',write_disk(rel <- tempfile(fileext = ".csv")))

releases_func<- function(temp_dir){
  
  #Ignoring repeat hydroids (same transfer reported from both recieving and transfer facility),
  #repeat the above steps using the remaining FROM transfers available at:
  releases<-read.csv(temp_dir)
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
    if(is.na(ToCounty)){ #if the County code is NA
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
  
  assign("relf",relf,envir=.GlobalEnv)
  assign("relt",relt,envir=.GlobalEnv)
  
}
releases_func(rel)

#-----------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------Calculate Net Transfers for Each FIPS Watershed-------------------------------------------------#

# Loop through each county and check for summed releases and deliveries
# Water out is defined as the "from's" and Water in are the "to's"
# This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

transfers<- function(relf,delf,relt,delt){
  
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
  
  assign("VA_Counties",VA_Counties,envir = .GlobalEnv)
  assign("County_Transfers",County_Transfers,envir = .GlobalEnv)
  
  rm(relf,delf,relt,delt,deliveries,envir = .GlobalEnv)
}
transfers(relf,delf,relt,delt)

###########################################################################################################################################
#----------------------------------------------------Calculating Discharges---------------------------------------------------------------#

#---Load in Discharge Data previously cleaned---#

ECHO_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC_statewide.txt", sep="\t", header=T)

ECHO_2010_2017%>%dplyr::summarise(Facilities=n_distinct(VPDES.Facility.ID),Outfalls=n_distinct(OutfallID))

#---Load in fully matched Discharge Data previously cleaned---#

full_match_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/full_match_2010_2017.txt", sep="\t", header=T)

#-------------Sector Analysis---------------#

# All of these functions are independent. So you can run each of them.
#---Option to Analyze all Sectors at once---#
all_discharge<- function(discharge_db,label){

  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(discharge_db,VA_Counties_Overlay)
  discharge_db@data$FIPS<-VA_Counties_Facilities$FIPS
  discharge_db@data$County<-VA_Counties_Facilities$County
  
  
  #--Sum Discharges in the Counties--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))

  County_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     VPDES.Facility.ID=first(VPDES.Facility.ID),
                     Outfalls=n_distinct(OutfallID),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD,na.rm=T)/first(Mon_Reported),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))

  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Outfalls=sum(Outfalls),
                     Ave_Mon_Reported=mean(Ave_Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=median(Discharges_MGD,na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))
  
  assign(paste0("County_Discharges",label),County_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
all_discharge(ECHO_2010_2017,"") # All Facilities

all_discharge<- function(discharge_db,label){
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(discharge_db,VA_Counties_Overlay)
  discharge_db@data$FIPS<-VA_Counties_Facilities$FIPS
  discharge_db@data$County<-VA_Counties_Facilities$County
  
  
  #--Sum Discharges in the Counties--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Facility_Discharges<-discharge_db@data%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))
  
  County_Discharges<-Facility_Discharges%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))

  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=median(Discharges_MGD,na.rm=T),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  assign(paste0("County_Discharges",label),County_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
all_discharge(full_match_2010_2017,"_matched") # Fully Matched Facilities

#---Option to separate by Water Use Sector---#
sector_discharge<- function(discharge_db,sector,label){
  
  discharge_db<-subset(discharge_db,subset=discharge_db$Use.Type==sector)
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(discharge_db,VA_Counties_Overlay)
  discharge_db@data$FIPS<-VA_Counties_Facilities$FIPS
  discharge_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))
  
  
  County_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     VPDES.Facility.ID=first(VPDES.Facility.ID),
                     Outfalls=n_distinct(OutfallID),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD,na.rm=T)/first(Mon_Reported),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Outfalls=sum(Outfalls),
                     Ave_Mon_Reported=mean(Ave_Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=median(Discharges_MGD,na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))
  
  assign(paste0("County_Discharges",label),County_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
sector_discharge(ECHO_2010_2017,"Energy","_energy")
sector_discharge(ECHO_2010_2017,"Aquaculture","_aq")
sector_discharge(ECHO_2010_2017,"Commercial","_commercial")
sector_discharge(ECHO_2010_2017,"Industrial","_industrial")
sector_discharge(ECHO_2010_2017,"Municipal","_municipal")
#sector_discharge(ECHO_2010_2017,"Agriculture/Irrigation","_ag")

sector_discharge<- function(discharge_db,sector,label){
  
  discharge_db<-subset(discharge_db,subset=discharge_db$Use.Type==sector)
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(discharge_db,VA_Counties_Overlay)
  discharge_db@data$FIPS<-VA_Counties_Facilities$FIPS
  discharge_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Facility_Discharges<-discharge_db@data%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))
  
  County_Discharges<-Facility_Discharges%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  
  #-----For later Use-------#
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=median(Discharges_MGD,na.rm=T),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  assign(paste0("County_Discharges",label),County_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
sector_discharge(full_match_2010_2017,"Energy","_match_energy")
sector_discharge(full_match_2010_2017,"Aquaculture","_match_aq")
sector_discharge(full_match_2010_2017,"Commercial","_match_commercial")
sector_discharge(full_match_2010_2017,"Industrial","_match_industrial")
# sector_discharge(full_match_2010_2017,"Municipal","_match_municipal")
# sector_discharge(full_match_2010_2017,"Agriculture/Irrigation","_match_ag")

#--Option to look at Non-Energy Sectors--#
nonenergy_discharge<- function(discharge_db,label){
  
  discharge_db<-subset(discharge_db,subset=!discharge_db$Use.Type=="Energy")
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(discharge_db,VA_Counties_Overlay)
  discharge_db@data$FIPS<-VA_Counties_Facilities$FIPS
  discharge_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))
  
  
  County_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     VPDES.Facility.ID=first(VPDES.Facility.ID),
                     Outfalls=n_distinct(OutfallID),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD,na.rm=T)/first(Mon_Reported),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Outfalls=sum(Outfalls),
                     Ave_Mon_Reported=mean(Ave_Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=median(Discharges_MGD,na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))
  
  assign(paste0("County_Discharges",label),County_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
nonenergy_discharge(ECHO_2010_2017,"_nonenergy")

nonenergy_discharge<- function(discharge_db,label){
  
  discharge_db<-subset(discharge_db,subset=!discharge_db$Use.Type=="Energy")
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(discharge_db,VA_Counties_Overlay)
  discharge_db@data$FIPS<-VA_Counties_Facilities$FIPS
  discharge_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Facility_Discharges<-discharge_db@data%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))%>%arrange(desc(Discharges_MGD))
  
  County_Discharges<-Facility_Discharges%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  
  discharge_db.test<-discharge_db.test%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     Fac.Lat=first(Fac.Lat),
                     Fac.Long=first(Fac.Long),
                     Discharges_MGD=median(Discharges_MGD,na.rm=T),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Fuel_Type=first(Fuel_Type),
                     Use.Type=first(Use.Type))%>%arrange(desc(Discharges_MGD))
  
  assign(paste0("County_Discharges",label),County_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
nonenergy_discharge(full_match_2010_2017,"_match_nonenergy")


###########################################################################################################################################
#----------------------------------------------------Calculating Withdrawals--------------------------------------------------------------#

#---Load in Withdrawal Data previously cleaned in VWUDS_QAQC.R---#
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017_statewide.RData")
VWUDS_2010_2017%>%dplyr::summarise(Facilities=n_distinct(VWUDS.Facility.ID),Sources=n_distinct(DEQ.ID.of.Source),Summed_W=sum(Withdrawals_MGD,na.rm=T))
colnames(VWUDS_2010_2017)[18:19]<-c("VWUDS.Lat","VWUDS.Long")
colnames(VWUDS_2010_2017)[6]<-c("VWUDS.Name")

#-------------Sector Analysis---------------#

# All of these functions are independent. So you can run each of them.

#---Option to Analyze all Sectors at once---#
all_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                                       withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(withdrawal_db,VA_Counties_Overlay)
  withdrawal_db@data$FIPS<-VA_Counties_Facilities$FIPS
  withdrawal_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))
  
  
  County_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(DEQ.ID.of.Source,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     VWUDS.Facility.ID=first(VWUDS.Facility.ID),
                     Sources=n_distinct(DEQ.ID.of.Source),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Sources=sum(Sources),
                     Ave_Mon_Reported=mean(Ave_Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("County_Withdrawals",label),County_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
all_withdrawal(VWUDS_2010_2017,"") # All Facilities

all_withdrawal<- function(withdrawal_db,label){
  
  # use same location as matched discharge facility
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$Fac.Long,
                                                       Facility_Latitude=withdrawal_db$Fac.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(withdrawal_db,VA_Counties_Overlay)
  withdrawal_db@data$FIPS<-VA_Counties_Facilities$FIPS
  withdrawal_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))
  
  
  County_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("County_Withdrawals",label),County_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
all_withdrawal(full_match_2010_2017,"_matched") # Fully Matched Facilities

#---Option to separate by Water Use Sector---#
sector_withdrawal<- function(withdrawal_db,sector,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=withdrawal_db$Use.Type==sector)
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(withdrawal_db,VA_Counties_Overlay)
  withdrawal_db@data$FIPS<-VA_Counties_Facilities$FIPS
  withdrawal_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))
  
  
  County_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(DEQ.ID.of.Source,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     VWUDS.Facility.ID=first(VWUDS.Facility.ID),
                     Sources=n_distinct(DEQ.ID.of.Source),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Sources=sum(Sources),
                     Ave_Mon_Reported=mean(Ave_Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("County_Withdrawals",label),County_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
sector_withdrawal(VWUDS_2010_2017,"Energy","_energy")
sector_withdrawal(VWUDS_2010_2017,"Agriculture/Irrigation","_ag")
sector_withdrawal(VWUDS_2010_2017,"Aquaculture","_aq")
sector_withdrawal(VWUDS_2010_2017,"Commercial","_commercial")
sector_withdrawal(VWUDS_2010_2017,"Industrial","_industrial")
sector_withdrawal(VWUDS_2010_2017,"Municipal","_municipal")


sector_withdrawal<- function(withdrawal_db,sector,label){
  
  # use one facility location for matched facilities
  withdrawal_db<-subset(withdrawal_db,subset=withdrawal_db$Use.Type==sector)
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$Fac.Long,
                                                       Facility_Latitude=withdrawal_db$Fac.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(withdrawal_db,VA_Counties_Overlay)
  withdrawal_db@data$FIPS<-VA_Counties_Facilities$FIPS
  withdrawal_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))
  
  
  County_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("County_Withdrawals",label),County_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
sector_withdrawal(full_match_2010_2017,"Energy","_match_energy")
# sector_withdrawal(full_match_2010_2017,"Agriculture/Irrigation","_match_ag")
sector_withdrawal(full_match_2010_2017,"Aquaculture","_match_aq")
sector_withdrawal(full_match_2010_2017,"Commercial","_match_commercial")
sector_withdrawal(full_match_2010_2017,"Industrial","_match_industrial")
# sector_withdrawal(full_match_2010_2017,"Municipal","_match_municipal")

#--Option to look at Non-Energy Sectors--#
nonenergy_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=!withdrawal_db$Use.Type=="Energy")
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(withdrawal_db,VA_Counties_Overlay)
  withdrawal_db@data$FIPS<-VA_Counties_Facilities$FIPS
  withdrawal_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))
  
  
  County_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(DEQ.ID.of.Source,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     VWUDS.Facility.ID=first(VWUDS.Facility.ID),
                     Sources=n_distinct(DEQ.ID.of.Source),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Sources=sum(Sources),
                     Ave_Mon_Reported=mean(Ave_Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("County_Withdrawals",label),County_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(VWUDS_2010_2017,"_nonenergy")

nonenergy_withdrawal<- function(withdrawal_db,label){
  
  # Use same location of matched VPDES facility
  withdrawal_db<-subset(withdrawal_db,subset=!withdrawal_db$Use.Type=="Energy")
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$Fac.Long,
                                                       Facility_Latitude=withdrawal_db$Fac.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with County Shapefile--#
  VA_Counties_Facilities<-over(withdrawal_db,VA_Counties_Overlay)
  withdrawal_db@data$FIPS<-VA_Counties_Facilities$FIPS
  withdrawal_db@data$County<-VA_Counties_Facilities$County
  
  #--Sum Discharges in the Counties--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     County=first(County),
                     FIPS=first(FIPS))
  
  
  County_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(County,Year)%>%
    dplyr::summarise(FIPS=first(FIPS),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Ave_Mon_Reported=mean(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("County_Withdrawals",label),County_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(full_match_2010_2017,"_match_nonenergy")

###########################################################################################################################################
#------------------------------------------Apply Withdrawals and Discharges into County Spatial Dataframe-----------------------------------#

county_discharge_withdrawal<- function(County_Discharges_db,County_Withdrawals_db,label){
#---Year 2010----#
VA_Counties@data$Discharges_2010<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2010"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2010"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2010"])],NA)
VA_Counties@data$Withdrawals_2010<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2010"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2010"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2010"])],NA)

#---Year 2011----#
VA_Counties@data$Discharges_2011<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2011"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2011"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2011"])],NA)
VA_Counties@data$Withdrawals_2011<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2011"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2011"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2011"])],NA)

#---Year 2012----#
VA_Counties@data$Discharges_2012<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2012"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2012"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2012"])],NA)
VA_Counties@data$Withdrawals_2012<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2012"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2012"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2012"])],NA)

#---Year 2013----#
VA_Counties@data$Discharges_2013<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2013"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2013"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2013"])],NA)
VA_Counties@data$Withdrawals_2013<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2013"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2013"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2013"])],NA)

#---Year 2014----#
VA_Counties@data$Discharges_2014<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2014"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2014"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2014"])],NA)
VA_Counties@data$Withdrawals_2014<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2014"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2014"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2014"])],NA)

#---Year 2015----#
VA_Counties@data$Discharges_2015<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2015"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2015"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2015"])],NA)
VA_Counties@data$Withdrawals_2015<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2015"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2015"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2015"])],NA)

#---Year 2016----#
VA_Counties@data$Discharges_2016<-ifelse(VA_Counties@data$FIPS%in%County_Discharges_db$FIPS[County_Discharges_db$Year=="2016"],County_Discharges_db$Discharge_MGD[County_Discharges_db$Year=="2016"][match(VA_Counties@data$FIPS,County_Discharges_db$FIPS[County_Discharges_db$Year=="2016"])],NA)
VA_Counties@data$Withdrawals_2016<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2016"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2016"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2016"])],NA)

assign(paste0("VA_Counties",label),VA_Counties,envir = .GlobalEnv)


}

#----------------------------------------------------------#
#-------------------All Available Data---------------------#

#---All Sectors---#
county_discharge_withdrawal(County_Discharges,County_Withdrawals,"_all")

#--Non-Energy--#
county_discharge_withdrawal(County_Discharges_nonenergy,County_Withdrawals_nonenergy,"_nonenergy")

county_discharge_withdrawal(County_Discharges_energy,County_Withdrawals_energy,"_energy")
county_discharge_withdrawal(County_Discharges_commercial,County_Withdrawals_commercial,"_commercial")
county_discharge_withdrawal(County_Discharges_industrial,County_Withdrawals_industrial,"_industrial")
county_discharge_withdrawal(County_Discharges_municipal,County_Withdrawals_municipal,"_municipal")
county_discharge_withdrawal(County_Discharges_aq,County_Withdrawals_aq,"_aq")

#--Agriculture--#
ag.county_discharge_withdrawal<- function(County_Withdrawals_db,label){
  #---Year 2010----#
  VA_Counties@data$Withdrawals_2010<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2010"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2010"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2010"])],NA)
  
  #---Year 2011----#
  VA_Counties@data$Withdrawals_2011<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2011"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2011"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2011"])],NA)
  
  #---Year 2012----#
  VA_Counties@data$Withdrawals_2012<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2012"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2012"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2012"])],NA)
  
  #---Year 2013----#
  VA_Counties@data$Withdrawals_2013<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2013"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2013"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2013"])],NA)
  
  #---Year 2014----#
  VA_Counties@data$Withdrawals_2014<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2014"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2014"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2014"])],NA)
  
  #---Year 2015----#
  VA_Counties@data$Withdrawals_2015<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2015"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2015"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2015"])],NA)
  
  #---Year 2016----#
  VA_Counties@data$Withdrawals_2016<-ifelse(VA_Counties@data$FIPS%in%County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2016"],County_Withdrawals_db$Withdrawals_MGD[County_Withdrawals_db$Year=="2016"][match(VA_Counties@data$FIPS,County_Withdrawals_db$FIPS[County_Withdrawals_db$Year=="2016"])],NA)
  
  assign(paste0("VA_Counties",label),VA_Counties,envir = .GlobalEnv)
}
ag.county_discharge_withdrawal(County_Withdrawals_ag,"_ag")

#----------------------------------------------------------#
#-------------------Matched Facility Data------------------#

#-All Sectors-#
county_discharge_withdrawal(County_Discharges_matched,County_Withdrawals_matched,"_matched")

#--Non-Energy--#
county_discharge_withdrawal(County_Discharges_match_nonenergy,County_Withdrawals_match_nonenergy,"_match_nonenergy")

county_discharge_withdrawal(County_Discharges_match_energy,County_Withdrawals_match_energy,"_match_energy")
county_discharge_withdrawal(County_Discharges_match_aq,County_Withdrawals_match_aq,"_match_aq")
county_discharge_withdrawal(County_Discharges_match_commercial,County_Withdrawals_match_commercial,"_match_commercial")
county_discharge_withdrawal(County_Discharges_match_industrial,County_Withdrawals_match_industrial,"_match_industrial")

rm(County_Discharges,County_Discharges_aq,County_Discharges_match_aq,County_Discharges_commercial,County_Discharges_energy,County_Discharges_industrial,County_Discharges_match_ag,
   County_Discharges_match_commercial,County_Discharges_match_energy,County_Discharges_match_industrial,County_Discharges_match_municipal,County_Discharges_match_nonenergy,
   County_Discharges_matched,County_Withdrawals,County_Withdrawals_ag,County_Withdrawals_commercial,County_Withdrawals_energy,County_Withdrawals_industrial,
   County_Withdrawals_match_ag,County_Withdrawals_match_commercial,County_Withdrawals_match_energy,County_Withdrawals_match_industrial,County_Withdrawals_match_municipal,
   County_Withdrawals_match_aq,County_Withdrawals_aq,County_Withdrawals_match_nonenergy,County_Discharges_municipal,County_Discharges_nonenergy,County_Withdrawals_matched,County_Withdrawals_municipal,County_Withdrawals_nonenergy)

###########################################################################################################################################
#-----------------------------------------------Net Water Balance and Consumtpive Use-----------------------------------------------------#

NWB_CU<- function(VA_Counties_db,label){
#---Year 2010----#

VA_Counties_db@data$NetWB_2010<-(ifelse(is.na(VA_Counties_db@data$Discharges_2010),0,VA_Counties_db@data$Discharges_2010))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),0,VA_Counties_db@data$Withdrawals_2010))
VA_Counties_db@data$NetWB_2010<-ifelse(is.na(VA_Counties_db@data$Discharges_2010)&is.na(VA_Counties_db@data$Withdrawals_2010),NA,VA_Counties_db@data$NetWB_2010)
VA_Counties_db@data$Consumption_2010<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),0,VA_Counties_db@data$Withdrawals_2010))-(ifelse(is.na(VA_Counties_db@data$Discharges_2010),0,VA_Counties_db@data$Discharges_2010)))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),0,VA_Counties_db@data$Withdrawals_2010))
VA_Counties_db@data$Consumption_2010<-ifelse(is.nan(VA_Counties_db@data$Consumption_2010)|is.infinite(VA_Counties_db@data$Consumption_2010),NA,VA_Counties_db@data$Consumption_2010)

#---Year 2011----#
VA_Counties_db@data$NetWB_2011<-(ifelse(is.na(VA_Counties_db@data$Discharges_2011),0,VA_Counties_db@data$Discharges_2011))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),0,VA_Counties_db@data$Withdrawals_2011))

VA_Counties_db@data$NetWB_2011<-ifelse(is.na(VA_Counties_db@data$Discharges_2011)&is.na(VA_Counties_db@data$Withdrawals_2011),NA,VA_Counties_db@data$NetWB_2011)

VA_Counties_db@data$Consumption_2011<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),0,VA_Counties_db@data$Withdrawals_2011))-
                               (ifelse(is.na(VA_Counties_db@data$Discharges_2011),0,VA_Counties_db@data$Discharges_2011)))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),0,VA_Counties_db@data$Withdrawals_2011))

VA_Counties_db@data$Consumption_2011<-ifelse(is.nan(VA_Counties_db@data$Consumption_2011)|is.infinite(VA_Counties_db@data$Consumption_2011),NA,VA_Counties_db@data$Consumption_2011)
#---Year 2012----#
VA_Counties_db@data$NetWB_2012<-(ifelse(is.na(VA_Counties_db@data$Discharges_2012),0,VA_Counties_db@data$Discharges_2012))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),0,VA_Counties_db@data$Withdrawals_2012))

VA_Counties_db@data$NetWB_2012<-ifelse(is.na(VA_Counties_db@data$Discharges_2012)&is.na(VA_Counties_db@data$Withdrawals_2012),NA,VA_Counties_db@data$NetWB_2012)

VA_Counties_db@data$Consumption_2012<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),0,VA_Counties_db@data$Withdrawals_2012))-(ifelse(is.na(VA_Counties_db@data$Discharges_2012),0,VA_Counties_db@data$Discharges_2012)))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),0,VA_Counties_db@data$Withdrawals_2012))

VA_Counties_db@data$Consumption_2012<-ifelse(is.nan(VA_Counties_db@data$Consumption_2012)|is.infinite(VA_Counties_db@data$Consumption_2012),NA,VA_Counties_db@data$Consumption_2012)
#---Year 2013----#
VA_Counties_db@data$NetWB_2013<-(ifelse(is.na(VA_Counties_db@data$Discharges_2013),0,VA_Counties_db@data$Discharges_2013))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),0,VA_Counties_db@data$Withdrawals_2013))

VA_Counties_db@data$NetWB_2013<-ifelse(is.na(VA_Counties_db@data$Discharges_2013)&is.na(VA_Counties_db@data$Withdrawals_2013),NA,VA_Counties_db@data$NetWB_2013)

VA_Counties_db@data$Consumption_2013<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),0,VA_Counties_db@data$Withdrawals_2013))-
                               (ifelse(is.na(VA_Counties_db@data$Discharges_2013),0,VA_Counties_db@data$Discharges_2013)))/
  (ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),0,VA_Counties_db@data$Withdrawals_2013))

VA_Counties_db@data$Consumption_2013<-ifelse(is.nan(VA_Counties_db@data$Consumption_2013)|is.infinite(VA_Counties_db@data$Consumption_2013),NA,VA_Counties_db@data$Consumption_2013)

#---Year 2014----#
VA_Counties_db@data$NetWB_2014<-(ifelse(is.na(VA_Counties_db@data$Discharges_2014),0,VA_Counties_db@data$Discharges_2014))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),0,VA_Counties_db@data$Withdrawals_2014))

VA_Counties_db@data$NetWB_2014<-ifelse(is.na(VA_Counties_db@data$Discharges_2014)&is.na(VA_Counties_db@data$Withdrawals_2014),NA,VA_Counties_db@data$NetWB_2014)

VA_Counties_db@data$Consumption_2014<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),0,VA_Counties_db@data$Withdrawals_2014))-
                               (ifelse(is.na(VA_Counties_db@data$Discharges_2014),0,VA_Counties_db@data$Discharges_2014)))/
  (ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),0,VA_Counties_db@data$Withdrawals_2014))

VA_Counties_db@data$Consumption_2014<-ifelse(is.nan(VA_Counties_db@data$Consumption_2014)|is.infinite(VA_Counties_db@data$Consumption_2014),NA,VA_Counties_db@data$Consumption_2014)
#---Year 2015----#
VA_Counties_db@data$NetWB_2015<-(ifelse(is.na(VA_Counties_db@data$Discharges_2015),0,VA_Counties_db@data$Discharges_2015))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),0,VA_Counties_db@data$Withdrawals_2015))

VA_Counties_db@data$NetWB_2015<-ifelse(is.na(VA_Counties_db@data$Discharges_2015)&is.na(VA_Counties_db@data$Withdrawals_2015),NA,VA_Counties_db@data$NetWB_2015)

VA_Counties_db@data$Consumption_2015<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),0,VA_Counties_db@data$Withdrawals_2015))-
                               (ifelse(is.na(VA_Counties_db@data$Discharges_2015),0,VA_Counties_db@data$Discharges_2015)))/
  (ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),0,VA_Counties_db@data$Withdrawals_2015))

VA_Counties_db@data$Consumption_2015<-ifelse(is.nan(VA_Counties_db@data$Consumption_2015)|is.infinite(VA_Counties_db@data$Consumption_2015),NA,VA_Counties_db@data$Consumption_2015)

#---Year 2016----#
VA_Counties_db@data$NetWB_2016<-(ifelse(is.na(VA_Counties_db@data$Discharges_2016),0,VA_Counties_db@data$Discharges_2016))-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),0,VA_Counties_db@data$Withdrawals_2016))

VA_Counties_db@data$NetWB_2016<-ifelse(is.na(VA_Counties_db@data$Discharges_2016)&is.na(VA_Counties_db@data$Withdrawals_2016),NA,VA_Counties_db@data$NetWB_2016)

VA_Counties_db@data$Consumption_2016<-((ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),0,VA_Counties_db@data$Withdrawals_2016))-
                               (ifelse(is.na(VA_Counties_db@data$Discharges_2016),0,VA_Counties_db@data$Discharges_2016)))/
  (ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),0,VA_Counties_db@data$Withdrawals_2016))

VA_Counties_db@data$Consumption_2016<-ifelse(is.nan(VA_Counties_db@data$Consumption_2016)|is.infinite(VA_Counties_db@data$Consumption_2016),NA,VA_Counties_db@data$Consumption_2016)


VA_Counties_glimpse<-VA_Counties_db@data

assign("VA_Counties_glimpse",VA_Counties_glimpse,envir = .GlobalEnv)
assign(paste0("VA_Counties",label),VA_Counties_db,envir = .GlobalEnv)

}

#---All Sectors---#

#-All Facilities-#
NWB_CU(VA_Counties_all,"_all")
#-Fully Matched Facilities-#
NWB_CU(VA_Counties_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
NWB_CU(VA_Counties_energy,"_energy")
NWB_CU(VA_Counties_aq,"_aq")

NWB_CU(VA_Counties_commercial,"_commercial")
NWB_CU(VA_Counties_industrial,"_industrial")
NWB_CU(VA_Counties_municipal,"_municipal")

NWB_CU.ag<- function(VA_Counties_db,label){
  #---Year 2010----#
  
  VA_Counties_db@data$NetWB_2010<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),0,VA_Counties_db@data$Withdrawals_2010))
  VA_Counties_db@data$NetWB_2010<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),NA,VA_Counties_db@data$NetWB_2010)
  
  VA_Counties_db@data$Consumption_2010<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),0,VA_Counties_db@data$Withdrawals_2010))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2010),0,VA_Counties_db@data$Withdrawals_2010))
  VA_Counties_db@data$Consumption_2010<-ifelse(is.nan(VA_Counties_db@data$Consumption_2010)|is.infinite(VA_Counties_db@data$Consumption_2010),NA,VA_Counties_db@data$Consumption_2010)
  #---Year 2011----#
  
  VA_Counties_db@data$NetWB_2011<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),0,VA_Counties_db@data$Withdrawals_2011))
  
  VA_Counties_db@data$NetWB_2011<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),NA,VA_Counties_db@data$NetWB_2011)
  
  VA_Counties_db@data$Consumption_2011<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),0,VA_Counties_db@data$Withdrawals_2011))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2011),0,VA_Counties_db@data$Withdrawals_2011))
  
  VA_Counties_db@data$Consumption_2011<-ifelse(is.nan(VA_Counties_db@data$Consumption_2011)|is.infinite(VA_Counties_db@data$Consumption_2011),NA,VA_Counties_db@data$Consumption_2011)
 
  #---Year 2012----#

  VA_Counties_db@data$NetWB_2012<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),0,VA_Counties_db@data$Withdrawals_2012))
  
  VA_Counties_db@data$NetWB_2012<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),NA,VA_Counties_db@data$NetWB_2012)
  
  VA_Counties_db@data$Consumption_2012<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),0,VA_Counties_db@data$Withdrawals_2012))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2012),0,VA_Counties_db@data$Withdrawals_2012))
  
  VA_Counties_db@data$Consumption_2012<-ifelse(is.nan(VA_Counties_db@data$Consumption_2012)|is.infinite(VA_Counties_db@data$Consumption_2012),NA,VA_Counties_db@data$Consumption_2012)
  #---Year 2013----#
  VA_Counties_db@data$NetWB_2013<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),0,VA_Counties_db@data$Withdrawals_2013))
  
  VA_Counties_db@data$NetWB_2013<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),NA,VA_Counties_db@data$NetWB_2013)
  
  VA_Counties_db@data$Consumption_2013<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),0,VA_Counties_db@data$Withdrawals_2013))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2013),0,VA_Counties_db@data$Withdrawals_2013))
  VA_Counties_db@data$Consumption_2013<-ifelse(is.nan(VA_Counties_db@data$Consumption_2013)|is.infinite(VA_Counties_db@data$Consumption_2013),NA,VA_Counties_db@data$Consumption_2013)
  
  #---Year 2014----#

  #---Without Transfers---#
  VA_Counties_db@data$NetWB_2014<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),0,VA_Counties_db@data$Withdrawals_2014))
  
  VA_Counties_db@data$NetWB_2014<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),NA,VA_Counties_db@data$NetWB_2014)
  
  VA_Counties_db@data$Consumption_2014<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),0,VA_Counties_db@data$Withdrawals_2014))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2014),0,VA_Counties_db@data$Withdrawals_2014))
  
  VA_Counties_db@data$Consumption_2014<-ifelse(is.nan(VA_Counties_db@data$Consumption_2014)|is.infinite(VA_Counties_db@data$Consumption_2014),NA,VA_Counties_db@data$Consumption_2014)
  #---Year 2015----#

  #---Without Transfers---#
  VA_Counties_db@data$NetWB_2015<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),0,VA_Counties_db@data$Withdrawals_2015))
  
  VA_Counties_db@data$NetWB_2015<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),NA,VA_Counties_db@data$NetWB_2015)
  
  VA_Counties_db@data$Consumption_2015<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),0,VA_Counties_db@data$Withdrawals_2015))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2015),0,VA_Counties_db@data$Withdrawals_2015))
  
  VA_Counties_db@data$Consumption_2015<-ifelse(is.nan(VA_Counties_db@data$Consumption_2015)|is.infinite(VA_Counties_db@data$Consumption_2015),NA,VA_Counties_db@data$Consumption_2015)
  
  #---Year 2016----#
  
  #---Without Transfers---#
  VA_Counties_db@data$NetWB_2016<- -(ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),0,VA_Counties_db@data$Withdrawals_2016))
  
  VA_Counties_db@data$NetWB_2016<-ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),NA,VA_Counties_db@data$NetWB_2016)
  
  VA_Counties_db@data$Consumption_2016<-(ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),0,VA_Counties_db@data$Withdrawals_2016))/(ifelse(is.na(VA_Counties_db@data$Withdrawals_2016),0,VA_Counties_db@data$Withdrawals_2016))
  
  VA_Counties_db@data$Consumption_2016<-ifelse(is.nan(VA_Counties_db@data$Consumption_2016)|is.infinite(VA_Counties_db@data$Consumption_2016),NA,VA_Counties_db@data$Consumption_2016)
  
  VA_Counties_glimpse<-VA_Counties_db@data
  
  assign("VA_Counties_glimpse",VA_Counties_glimpse,envir = .GlobalEnv)
  assign(paste0("VA_Counties",label),VA_Counties_db,envir = .GlobalEnv)
  
}
NWB_CU.ag(VA_Counties_ag,"_ag")

#-Fully Matched Facilities-#
NWB_CU(VA_Counties_match_energy,"_match_energy")
NWB_CU(VA_Counties_match_aq,"_match_aq")
NWB_CU(VA_Counties_match_commercial,"_match_commercial")
NWB_CU(VA_Counties_match_industrial,"_match_industrial")

#---Non-Energy Sectors---#

#-All Facilities-#
NWB_CU(VA_Counties_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
NWB_CU(VA_Counties_match_nonenergy,"_match_nonenergy")

###########################################################################################################################################
#--------------------------------Long Term Average (2010-2017) Net Water Balance and Consumtpive Use--------------------------------------#

Ave_NWB_CU<- function(VA_Counties_db,label){
  
  #----Discharges-----#
  VA_Counties_db@data$Discharge_ave<-apply(VA_Counties_db@data[,c("Discharges_2010","Discharges_2011","Discharges_2012",
                                                            "Discharges_2013","Discharges_2014","Discharges_2015",
                                                            "Discharges_2016")],1,median,na.rm=T)

  #----Withdrawals----#
  VA_Counties_db@data$Withdrawal_ave<-apply(VA_Counties_db@data[,c("Withdrawals_2010","Withdrawals_2011","Withdrawals_2012",
                                                             "Withdrawals_2013","Withdrawals_2014","Withdrawals_2015",
                                                             "Withdrawals_2016")],1,median,na.rm=T)

  #----Net Water Balance-----#
  VA_Counties_db@data$NetWB_ave<-apply(VA_Counties_db@data[,c("NetWB_2010","NetWB_2011","NetWB_2012",
                                                             "NetWB_2013","NetWB_2014","NetWB_2015",
                                                             "NetWB_2016")],1,median,na.rm=T)

  #----Consumption----#
   VA_Counties_db@data$Consumption_ave<-apply(VA_Counties_db@data[,c("Consumption_2010","Consumption_2011","Consumption_2012",
                                                               "Consumption_2013","Consumption_2014","Consumption_2015",
                                                              "Consumption_2016")],1,median,na.rm=T)
  
  VA_Counties_db@data$Consumption_ave<-ifelse(is.infinite(VA_Counties_db@data$Consumption_ave),NA,VA_Counties_db@data$Consumption_ave)

  VA_Counties_glimpse<-VA_Counties_db@data
  assign(paste0("VA_Counties",label),VA_Counties_db,envir = .GlobalEnv)
  assign("VA_Counties_glimpse",VA_Counties_glimpse,envir = .GlobalEnv)
}

#---All Sectors---#

#-All Facilities-#
Ave_NWB_CU(VA_Counties_all,"")
#-Fully Matched Facilities-#
Ave_NWB_CU(VA_Counties_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
Ave_NWB_CU(VA_Counties_energy,"_energy")


Ave_NWB_CU.ag<- function(VA_Counties_db,label){
  
  #----Withdrawals----#
  VA_Counties_db@data$Withdrawal_ave<-apply(VA_Counties_db@data[,c("Withdrawals_2010","Withdrawals_2011","Withdrawals_2012",
                                                             "Withdrawals_2013","Withdrawals_2014","Withdrawals_2015",
                                                             "Withdrawals_2016")],1,median,na.rm=T)
  
  #----Consumption----#
  VA_Counties_db@data$Consumption_ave<-apply(VA_Counties_db@data[,c("Consumption_2010","Consumption_2011","Consumption_2012",
                                                              "Consumption_2013","Consumption_2014","Consumption_2015",
                                                              "Consumption_2016")],1,median,na.rm=T)
  
  VA_Counties_db@data$Consumption_ave<-ifelse(is.infinite(VA_Counties_db@data$Consumption_ave),NA,VA_Counties_db@data$Consumption_ave)
  
  
  VA_Counties_glimpse<-VA_Counties_db@data
  assign(paste0("VA_Counties",label),VA_Counties_db,envir = .GlobalEnv)
  assign("VA_Counties_glimpse",VA_Counties_glimpse,envir = .GlobalEnv)
}
Ave_NWB_CU.ag(VA_Counties_ag,"_ag")

Ave_NWB_CU(VA_Counties_aq,"_aq")
Ave_NWB_CU(VA_Counties_commercial,"_commercial")
Ave_NWB_CU(VA_Counties_industrial,"_industrial")
Ave_NWB_CU(VA_Counties_municipal,"_municipal")

#-Fully Matched Facilities-#
Ave_NWB_CU(VA_Counties_match_energy,"_match_energy")
Ave_NWB_CU(VA_Counties_match_aq,"_match_aq")
Ave_NWB_CU(VA_Counties_match_commercial,"_match_commercial")
Ave_NWB_CU(VA_Counties_match_industrial,"_match_industrial")

#---Non-Energy Sectors---#

#-All Facilities-#
Ave_NWB_CU(VA_Counties_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
Ave_NWB_CU(VA_Counties_match_nonenergy,"_match_nonenergy")

###########################################################################################################################################
#----------------------------------------------------------Visualization------------------------------------------------------------------#

#---Functions to Create Scale Bar and North Arrow---#
create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}
create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}
scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}

#-----County Labels-----#
county_labels<- function(VA_Counties){
VA_Counties_Centroids<-as.data.frame(coordinates(VA_Counties))
names(VA_Counties_Centroids)<-c("Longitude","Latitude")
VA_Counties_Centroids$FIPS<-VA_Counties_glimpse$FIPS
County_Names<-subset(VA_Counties_glimpse,select=c(2,3))
VA_Counties_Centroids<-merge(VA_Counties_Centroids,County_Names,by="FIPS")

ggplot()+
  geom_polygon(data=VA_Counties,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  geom_text(data=VA_Counties_Centroids,aes(x=Longitude,y=Latitude,label=County),size=1)+
  scale_colour_manual(values=c("#252525"))+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

}
county_labels(VA_Counties)

###########################################################################################################################################
#-------------------------------------------Distribution of Discharging Facilities in County----------------------------------------------#

county_discharge<- function(VA_Counties_db,ECHO_points, label,fileext){

  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill= cut(VA_Counties.df$Discharge_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Discharge_ave,na.rm=T)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Discharge by County (MGD)", values=(Dis_Discrete), 
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),colour="#252525",size=3.0)+
    scale_shape_manual(name="", values=17)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
    #labs(title = paste0("Average Annual Total Discharge (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
    # scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              # arrow_length=75, arrow_distance = 60, arrow_north_size = 6)

  # saves plot in pdf format to folder of your choosing. Dont want to do this? Comment it out
    
  assign(paste0("county_dis_",fileext),plot1,envir = .GlobalEnv) 
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Discharges/"
  
  #ggsave(filename=paste0(path,"county_discharge_",fileext,".pdf"), 
         #plot=plot1, 
         #width=13.33, height=6.66,units="in")
  
  
  
  
  return(list(plot1))
  

}

#---All Sectors---#

#-All Facilities-#
county_discharge(VA_Counties_all,ECHO.test,"All Reporting Facilities in All Sectors","all_fac_all_sec")
#-Fully Matched Facilities-#
county_discharge(VA_Counties_matched,ECHO.test_matched,"Matched Facilities in All Sectors","match_all_sector")

#---Subsetted by Sector---#

#----Sizes of points change with median discharge----#
county_discharge.energy<- function(VA_Counties_db,ECHO_points, label,fileext){
  
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")

  ECHO_points<-mutate_if(ECHO_points,is.factor,as.character)
  ECHO_points$Fuel_Type<-trimws(ECHO_points$Fuel_Type, which=c("right"))
  
  # ECHO_points$Fuel_Type<-ifelse(ECHO_points$Fuel_Type=="Natural Gas","Combination Fossil",ECHO_points$Fuel_Type)
  
  Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill= cut(VA_Counties.df$Discharge_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Discharge_ave,na.rm=T)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Discharge by County (MGD)", values=(Dis_Discrete), 
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape=ECHO_points$Fuel_Type, size=ECHO_points$Discharges_MGD),colour="#252525")+
    scale_size(name="Facility Discharge (MGD)", 
               breaks=c(0,1,5,50,500),
               trans="sqrt",
               labels=c(paste("<",1),
                        paste(1,"-",5),
                        paste(5,"-",50),
                        paste(50,"-",500),
                        paste(500,"<")),
               range=c(1,8))+
    scale_shape_manual(name="Fuel Type",
                       values=c("Coal"=15,"Nuclear"=16,"Combination Fossil"=17,
                                "Biomass"=18,"Hybrid"=8))+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes = list(size=3), order=3),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2),
           size=guide_legend(order=4))+
    #labs(title = paste0("Average Annual Total Discharge (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  # saves plot in pdf format to folder of your choosing. Dont want to do this? Comment it out
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Discharges/"
  
  ggsave(filename=paste0(path,"county_discharge_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  
  assign(paste0("county_dis_",fileext),plot1,envir = .GlobalEnv)
  
  return(list(plot1))
  
  
}
county_discharge.energy(VA_Counties_energy,ECHO.test_energy,"Energy Facilities","all_energy_size")

county_discharge.nonenergy<- function(VA_Counties_db,ECHO_points, label,fileext){
  
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill= cut(VA_Counties.df$Discharge_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Discharge_ave,na.rm=T)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Discharge by County (MGD)", values=(Dis_Discrete), 
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility",size=ECHO_points$Discharges_MGD),colour="#252525")+
    scale_size(name="Facility Discharge (MGD)", 
               breaks=c(0,0.1,0.5,1,5),
               trans="sqrt",
               labels=c(paste("<",0.1),
                        paste(0.1,"-",0.5),
                        paste(0.5,"-",1),
                        paste(1,"-",5),
                        paste(5,"<")),
               range=c(1,8))+
    scale_shape_manual(name="", values=17)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=3),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2),
           size=guide_legend(override.aes=list(shape=17)))+
    #labs(title = paste0("Average Annual Total Discharge (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  # saves plot in pdf format to folder of your choosing. Dont want to do this? Comment it out
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Discharges/"
  
  ggsave(filename=paste0(path,"county_discharge_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  
  assign(paste0("county_dis_",fileext),plot1,envir = .GlobalEnv)
  return(list(plot1))
  
  
}
county_discharge.nonenergy(VA_Counties_nonenergy,ECHO.test_nonenergy,"Non-Energy Facilities","all_nonenergy_size")


#----Just aggregated discharge on map, sizes of points are universal----#
county_discharge(VA_Counties_energy,ECHO.test_energy,"Energy Facilities","all_energy")
county_discharge(VA_Counties_aq,ECHO.test_aq,"Aquaculture Facilities","all_aq")
county_discharge(VA_Counties_commercial,ECHO.test_commercial,"Commercial Facilities","all_commercial")
county_discharge(VA_Counties_industrial,ECHO.test_industrial,"Industrial Facilities","all_industrial")
county_discharge(VA_Counties_municipal,ECHO.test_municipal,"Municipal Facilities","all_municipal")
county_discharge(VA_Counties_nonenergy,ECHO.test_nonenergy,"Non-Energy Facilities","all_nonenergy")

discharge_subplot_all<- function(){
  
  subplot<-ggpubr::ggarrange(county_dis_all_energy,county_dis_all_nonenergy,county_dis_all_industrial,county_dis_all_commercial,
                             county_dis_all_aq,county_dis_all_municipal,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture","(f) Municipal"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("Visualizing Median County Discharge 2010-2016: All Reporting VPDES Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"County_Dis_All_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
discharge_subplot_all()

#-Fully Matched Facilities-#
county_discharge(VA_Counties_match_energy,ECHO.test_match_energy,"Matched Energy Facilities","match_energy")
county_discharge(VA_Counties_match_aq,ECHO.test_match_aq,"Aquaculture Facilities","match_aq")
county_discharge(VA_Counties_match_commercial,ECHO.test_match_commercial,"Matched Commercial Facilities","match_commercial")
county_discharge(VA_Counties_match_industrial,ECHO.test_match_industrial,"Matched Industrial Facilities","match_industrial")
county_discharge(VA_Counties_match_nonenergy,ECHO.test_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")

discharge_subplot_match<- function(){
  
  subplot<-ggpubr::ggarrange(county_dis_match_energy,county_dis_match_nonenergy,county_dis_match_industrial,county_dis_match_commercial,
                             county_dis_match_aq,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("Visualizing Median County Discharge 2010-2016: Matched VPDES Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"County_Dis_Matched_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
discharge_subplot_match()

###########################################################################################################################################
#-----------------------------------------Distribution of Withdrawing Facilities in County------------------------------------------------#

county_withdrawal<- function(VA_Counties_db,VWUDS_points,label,fileext){
  
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill= cut(VA_Counties.df$Withdrawal_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Withdrawal_ave,na.rm=TRUE)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Withdrawal by County (MGD)", values=(With_Discrete),
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
    geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, shape="VWUDS Facility"),
               size=3.0,colour="#252525")+
    scale_shape_manual(name="", values=20)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
    # labs(title = paste0("Average Annual Summed Withdrawal (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
    # scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              # arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
    
  assign(paste0("county_with_",fileext),plot1,envir = .GlobalEnv) 
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Withdrawals/"
  
  #ggsave(filename=paste0(path,"county_withdrawal_",fileext,".pdf"), 
         #plot=plot1, 
         #width=13.33, height=6.66,units="in")
  
  # return(list(plot1))
}

#-All Facilities-#
county_withdrawal.energy<- function(VA_Counties_db,VWUDS_points,label,fileext){
  
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  Fuel_Type<-read.csv("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Energy_Fuel_Types.csv",header=T)
  VWUDS_points$Fuel_Type<-(Fuel_Type$Fuel.Type[match(VWUDS_points$VWUDS.Facility.ID,Fuel_Type$VWUDS)])
  
  VWUDS_points<-mutate_if(VWUDS_points,is.factor,as.character)
  VWUDS_points$Fuel_Type<-trimws(VWUDS_points$Fuel_Type, which=c("right"))
  
  
  With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill= cut(VA_Counties.df$Withdrawal_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Withdrawal_ave,na.rm=TRUE)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Withdrawal by County (MGD)", values=(With_Discrete),
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
    geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, shape=VWUDS_points$Fuel_Type,size=VWUDS_points$Withdrawals_MGD),colour="#252525")+
    scale_size(name="Facility Withdrawal (MGD)", 
               breaks=c(0,1,5,50,500),
               trans="sqrt",
               labels=c(paste("<",1),
                        paste(1,"-",5),
                        paste(5,"-",50),
                        paste(50,"-",500),
                        paste(500,"<")),
               range=c(1,8))+
    scale_shape_manual(name="Fuel Type",
                       values=c("Coal"=15,"Nuclear"=16,"Combination Fossil"=17,
                                "Biomass"=18,"Hybrid"=8))+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes = list(size=3), order=3),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2),
           size=guide_legend(override.aes=list(shape=20)))+
    # labs(title = paste0("Average Annual Summed Withdrawal (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Withdrawals/"
  
  ggsave(filename=paste0(path,"county_withdrawal_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  return(list(plot1))
}
county_withdrawal.energy(VA_Counties_energy,VWUDS.test_energy,"Energy Facilities","all_energy")

county_withdrawal.nonenergy<- function(VA_Counties_db,VWUDS_points,label,fileext){
  
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill= cut(VA_Counties.df$Withdrawal_ave,breaks=c(0,1,5,10,25,50,max(VA_Counties.df$Withdrawal_ave,na.rm=TRUE)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Withdrawal by County (MGD)", values=(With_Discrete),
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
    geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, shape="VWUDS Facility",size=VWUDS_points$Withdrawals_MGD),colour="#252525")+
    scale_size(name="Facility Withdrawal (MGD)", 
               breaks=c(0,0.1,0.5,1,5),
               trans="sqrt",
               labels=c(paste("<",0.1),
                        paste(0.1,"-",0.5),
                        paste(0.5,"-",1),
                        paste(1,"-",5),
                        paste(5,"<")),
               range=c(1,8))+
    scale_shape_manual(name="", values=20)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=3),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2),
           size=guide_legend(override.aes=list(shape=20)))+
    # labs(title = paste0("Average Annual Summed Withdrawal (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Withdrawals/"
  
  ggsave(filename=paste0(path,"county_withdrawal_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  return(list(plot1))
}
county_withdrawal.nonenergy(VA_Counties_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities","all_nonenergy")

#--All Sectors--#
county_withdrawal(VA_Counties_all,VWUDS.test,"All Reporting Facilities in All Sectors","all_fac_all_sec")

#-Fully Matched Facilities-#
county_withdrawal(VA_Counties_matched,VWUDS.test_matched,"Matched Facilities in All Sectors","match_all_sector")

#---Subsetted by Sector---#

#-All Facilities-#
county_withdrawal(VA_Counties_energy,VWUDS.test_energy,"Energy Facilities","all_energy")
county_withdrawal(VA_Counties_ag,VWUDS.test_ag,"Agriculture/Irrigation Facilities","all_ag")
county_withdrawal(VA_Counties_aq,VWUDS.test_aq,"Aquaculture Facilities","all_aq")
county_withdrawal(VA_Counties_commercial,VWUDS.test_commercial,"Commercial Facilities","all_commercial")
county_withdrawal(VA_Counties_industrial,VWUDS.test_industrial,"Industrial Facilities","all_industrial")
county_withdrawal(VA_Counties_municipal,VWUDS.test_municipal,"Municipal Facilities","all_municipal")
county_withdrawal(VA_Counties_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities","all_nonenergy")

withdrawal_subplot_all<- function(){
  
  subplot<-ggpubr::ggarrange(county_with_all_energy,county_with_all_nonenergy,county_with_all_industrial,county_with_all_commercial,
                             county_with_all_aq,county_with_all_ag,county_with_all_municipal,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture","(f) Agriculture/Irrigation","(g) Municipal"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=4)%>%annotate_figure(
                               top = text_grob("Visualizing Median County Withdrawal 2010-2016: All Reporting VWUDS Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"County_With_All_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
withdrawal_subplot_all()

#-Fully Matched Facilities-#
county_withdrawal(VA_Counties_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities","match_energy")
county_withdrawal(VA_Counties_match_aq,VWUDS.test_match_aq,"Matched Aquaculture Facilities","match_aq")
county_withdrawal(VA_Counties_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities","match_commercial")
county_withdrawal(VA_Counties_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities","match_industrial")
county_withdrawal(VA_Counties_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")

withdrawal_subplot_match<- function(){
  
  subplot<-ggpubr::ggarrange(county_with_match_energy,county_with_match_nonenergy,county_with_match_industrial,
                             county_with_match_commercial,
                             county_with_match_aq,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("Visualizing Median County Withdrawal 2010-2016: Matched VWUDS Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"County_With_Matched_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
withdrawal_subplot_match()


###########################################################################################################################################
#---------------------------------------------------Consumption over Counties-------------------------------------------------------------#

county_consumption<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),
                   include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
    # scale_shape_manual(name="", values=17)+
    # geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, size="VWUDS Facility"),colour="#252525")+
    # scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
    #scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              #arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
          #plot=plot1, 
          #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}

#---------------------------All Reporting Facilities--------------------------#
county_consumption(VA_Counties,ECHO.test,VWUDS.test,"All Reporting Facilities in All Sectors","all_fac_all_sec")

county_consumption(VA_Counties_commercial,ECHO.test_commercial,VWUDS.test_commercial,"Commercial Facilities","all_commercial")
county_consumption(VA_Counties_industrial,ECHO.test_industrial,VWUDS.test_industrial,"Industrial Facilities","all_industrial")
county_consumption(VA_Counties_municipal,ECHO.test_municipal,VWUDS.test_municipal,"Municipal Facilities","all_municipal")
county_consumption(VA_Counties_nonenergy,ECHO.test_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities","county_all_nonenergy")

county_consumption.energy<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  ECHO_points<-mutate_if(ECHO_points,is.factor,as.character)
  ECHO_points$Fuel_Type<-trimws(ECHO_points$Fuel_Type, which=c("right"))
  
  Fuel_Type<-read.csv("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Energy_Fuel_Types.csv",header=T)
  VWUDS_points$Fuel_Type<-(Fuel_Type$Fuel.Type[match(VWUDS_points$VWUDS.Facility.ID,Fuel_Type$VWUDS)])
  
  VWUDS_points<-mutate_if(VWUDS_points,is.factor,as.character)
  VWUDS_points$Fuel_Type<-trimws(VWUDS_points$Fuel_Type, which=c("right"))
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),
                   include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
    # scale_shape_manual(name="", values=17)+
    # geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, size="VWUDS Facility"),colour="#252525")+
    # scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
    #scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              #arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         #plot=plot1, 
         #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}
county_consumption.energy(VA_Counties_energy,ECHO.test_energy,VWUDS.test_energy,"Energy Facilities","all_energy")

county_consumption_ag<- function(VA_Counties_db,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(0,1)),colour=""))+
    scale_fill_manual(name="Consumption Coefficient",
                      values=CU_Discrete, 
                      labels=c(1),
                      na.value="transparent")+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    scale_shape_manual(name="", values=17)+
    #geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, size="VWUDS Facility"),colour="#252525")+
    #scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          # legend.position = "none",
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
    #scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              #arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(fileext,plot1,envir = .GlobalEnv)
   #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
   #ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
          #plot=plot1, 
          #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}
county_consumption_ag(VA_Counties_ag,VWUDS.test_ag,"Agriculture/Irrigation Facilities","all_ag")

county_consumption.aq<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(0,0.25,0.5,0.75,1),
                   include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
    # scale_shape_manual(name="", values=17)+
    # geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, size="VWUDS Facility"),colour="#252525")+
    # scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
    #scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              #arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         #plot=plot1, 
         #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}
county_consumption.aq(VA_Counties_aq,ECHO.test_aq,VWUDS.test_aq,"Aquaculture Facilities","all_aq")

all_subplot<- function(){
  subplot<-ggpubr::ggarrange(all_energy,all_nonenergy,all_industrial,
                             all_aq,all_commercial,all_ag,all_municipal,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Aquaculture",
                                      "(e) Commercial","(f) Agriculture","(g) Municipal"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=4)%>%annotate_figure(top = text_grob("Visualizing County Consumption: All Reporting Facilities", color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"County_CU_All_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
all_subplot()

#-----------------Matched --------------------------#
county_consumption<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
     # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
     # scale_shape_manual(name="", values=17)+
     # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, size="VWUDS Facility"), colour="#252525")+
     # scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
     #labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  
  assign(fileext,plot1,envir = .GlobalEnv)
   #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
   #ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         #plot=plot1, 
         #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}

county_consumption(VA_Counties_match_energy,ECHO.test_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities","match_energy")
county_consumption(VA_Counties_match_nonenergy,ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities","county_match_nonenergy")
county_consumption(VA_Counties_match_commercial,ECHO.test_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities","match_commercial")
county_consumption(VA_Counties_match_industrial,ECHO.test_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities","match_industrial")

county_consumption_aq<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
     geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
     scale_shape_manual(name="", values=17)+
     geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, size="VWUDS Facility"),colour="#252525")+
     scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
     #labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  
  assign(fileext,plot1,envir = .GlobalEnv)
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
   #ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
   #plot=plot1, 
   #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}
county_consumption_aq(VA_Counties_match_aq,ECHO.test_match_aq,VWUDS.test_match_aq,"Matched Aquaculture Facilities","match_aq")

#-----Compile into Subplots on one page-------#

matched_subplot<- function(){
  
  subplot<-ggpubr::ggarrange(match_energy,match_nonenergy,match_industrial,
                             match_aq,match_commercial,labels=c("(a) Energy","(b) Non-Energy","(c) Industrial",
                                                                "(d) Aquaculture","(e) Commercial"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("2010-2016 Median Annual County Consumption Considering Matched Facility Data", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"County_CU_Matched_Subplot_points.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
matched_subplot()

nonenergy_subplot<- function(){
  load("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/McCarthy Thesis/HUC6_match_nonenergy.RData")
  load("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/McCarthy Thesis/HUC6_all_nonenergy.RData")
  
  subplot<-ggpubr::ggarrange(HUC6_all_nonenergy,HUC6_match_nonenergy,county_all_nonenergy,
                             county_match_nonenergy,labels=c("(a) HUC 6 Watershed: All-Available Data","(b) HUC 6 Watershed: Matched Data",
                                                             "(c) County: All-Available Data","(d) County: Matched Data"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=2)%>%annotate_figure(
                               top = text_grob("2010-2016 Median Annual Non-Energy Consumption Using Total Volume", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/"
  
  ggexport(filename=paste0(path,"Nonenergy_spatial_subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
nonenergy_subplot()
#------------------------------------------------------------------------------------#
#-----------------------------Matched Facilities-------------------------------------#

# These functions are a little different because I paired the locations of tyhe matched facilities
# So i went ahead and added size of points to represent the quantity of water being consumed along with the coefficient

#---Non-Energy Sectors---#
county_consumption_non_energy<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, 
                                     size=ECHO_points$Ave_CU),shape=20,colour="#252525")+
    scale_size(name="Average Consumption (MGD)", 
               breaks=c(min(ECHO_points$Ave_CU,na.rm=T),0,0.001,0.05,0.5,1),
               labels=c(paste("<",0),
                        paste(0,"-",0.001),
                        paste(0.001,"-",0.05),
                        paste(0.05,"-",0.5),
                        paste(0.5,"-",1),
                        paste(1,"<")),
               range=c(2,11),
               trans="sqrt"
    )+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           size=guide_legend(order=4))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
county_consumption_non_energy(VA_Counties_match_nonenergy,ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")

#--Aquaculture--#
county_consumption_aquaculture<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(-1,0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("\u2264",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, 
                                     size=ECHO_points$Ave_CU),shape=20,colour="#252525")+
    scale_size(name="Average Consumption (MGD)", 
               breaks=c(0,0.05,0.5,1),
               labels=c(paste(0,"-",0.05),
                        paste(0.05,"-",0.5),
                        paste(0.5,"-",1),
                        paste(1,"<")),
               range=c(2,11),
               trans="sqrt"
    )+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           size=guide_legend(order=4))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
county_consumption_aquaculture(VA_Counties_match_aq,ECHO.test_match_aq,VWUDS.test_match_aq,"Matched Aquaculture Facilities","match_aq")

#--Commercial--#
county_consumption_commercial<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, 
                                     size=ECHO_points$Ave_CU),shape=20,colour="#252525")+
    scale_size(name="Average Consumption (MGD)", 
               breaks=c(min(ECHO_points$Ave_CU,na.rm=T),0,0.05,0.5,1),
               labels=c(paste("<",0),
                        paste(0,"-",0.05),
                        paste(0.05,"-",0.5),
                        paste(0.5,"-",1),
                        paste(1,"<")),
               range=c(2,11),
               trans="sqrt"
    )+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           size=guide_legend(order=4))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
county_consumption_commercial(VA_Counties_match_commercial,ECHO.test_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities","match_commercial")

#--Industrial--#
county_consumption_industrial<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, 
                                     size=ECHO_points$Ave_CU),shape=20,colour="#252525")+
    scale_size(name="Average Consumption (MGD)", 
               breaks=c(min(ECHO_points$Ave_CU,na.rm=T),0,0.001,0.05,0.5,1),
               labels=c(paste("<",0),
                        paste(0,"-",0.001),
                        paste(0.001,"-",0.05),
                        paste(0.05,"-",0.5),
                        paste(0.5,"-",1),
                        paste(1,"<")),
               range=c(2,11),
               trans="sqrt"
    )+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           size=guide_legend(order=4))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
county_consumption_industrial(VA_Counties_match_industrial,ECHO.test_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities","match_industrial")

#--Energy--#
county_consumption_energy<- function(VA_Counties_db, ECHO_points ,VWUDS_points, label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties_db)
  VA_Counties_db$polyID<-sapply(slot(VA_Counties_db,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties_db, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T),Fuel_Type=first(Fuel_Type))
  
  ECHO_points<-mutate_if(ECHO_points,is.factor,as.character)
  
  ECHO_points$Fuel_Type<-ifelse(ECHO_points$Fuel_Type=="Natural Gas","Combination Fossil",ECHO_points$Fuel_Type)
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=VA_Counties.df,
      aes(x=long, y= lat, group=group,
          fill=cut(VA_Counties.df$Consumption_ave,breaks=c(quantile(VA_Counties.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, 
              size=ECHO_points$Ave_CU,shape=ECHO_points$Fuel_Type),colour="#252525")+
    scale_size(name="Average Consumption (MGD)", 
               breaks=c(min(ECHO_points$Ave_CU,na.rm=T),0,0.5,1,5,10),
               trans="sqrt",
                 labels=c(paste("<",0),
                          paste(0,"-",0.5),
                          paste(0.5,"-",1.0),
                          paste(1.0,"-",5.0),
                          paste(5.0,"-",10.0),
                          paste(10,"<")),
               range=c(2,12))+
    scale_shape_manual(name="Fuel Type",
                       values=c("Coal"=15,"Nuclear"=16,"Combination Fossil"=17,
                                "Biomass"=18))+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           size=guide_legend(order=4),
           shape=guide_legend(override.aes = list(size=3), order=3))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"county_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
county_consumption_energy(VA_Counties_match_energy,ECHO.test_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities","match_energy")


###########################################################################################################################################
#-----------------------------------------Consumption over Counties (without points)------------------------------------------------------#
# Don't want locations of outfalls and withdrawing sources? No problem, here's a function for that. 

county_consumption_nopnt<- function(VA_Counties,label,fileext){
  VA_Counties.df<-broom::tidy(VA_Counties)
  VA_Counties$polyID<-sapply(slot(VA_Counties,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
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
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           shape=guide_legend(order=3),
           size=guide_legend(order=4))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/County Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"county_CU_nopnt_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}

#-All Facilities-#
county_consumption_nopnt(VA_Counties,"All Reporting Facilities in All Sectors","all_fac_all_sec")
#-Fully Matched Facilities-#
county_consumption_nopnt(VA_Counties_matched,"Matched Facilities in All Sectors","match_all_sector")

#---Subsetted by Sector---#

#-All Facilities-#
county_consumption_nopnt(VA_Counties_energy,"Energy Facilities","all_energy")
county_consumption_nopnt(VA_Counties_ag,"Agriculture/Irrigation Facilities","all_ag")
county_consumption_nopnt(VA_Counties_commercial,"Commercial Facilities","all_commercial")
county_consumption_nopnt(VA_Counties_industrial,"Industrial Facilities","all_industrial")
county_consumption_nopnt(VA_Counties_municipal,"Municipal Facilities","all_municipal")

#-Fully Matched Facilities-#
county_consumption_nopnt(VA_Counties_match_energy,"Matched Energy Facilities","match_energy")
county_consumption_nopnt(VA_Counties_match_ag,"Matched Agriculture/Irrigation Facilities","match_ag")
county_consumption_nopnt(VA_Counties_match_commercial,"Matched Commercial Facilities","match_commercial")
county_consumption_nopnt(VA_Counties_match_industrial,"Matched Industrial Facilities","match_industrial")
county_consumption_nopnt(VA_Counties_match_municipal,"Matched Municipal Facilities","match_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
county_consumption_nopnt(VA_Counties_nonenergy,"Non-Energy Facilities","all_nonenergy")

#-Fully Matched Facilities-#
county_consumption_nopnt(VA_Counties_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")


###########################################################################################################################################
#-----------------------------------------------Case Study Facility Locations-------------------------------------------------------------#

full_match_2010_2017_summary<-full_match_2010_2017%>%dplyr::group_by(VPDES.Facility.ID)%>%
  dplyr::summarise(VWUDS.Facility.ID=first(VWUDS.Facility.ID),OutfallID=first(OutfallID),VWUDS.Name=first(VWUDS.Name),VPDES.Name=first(VPDES.Name),
                   Use.Type=first(Use.Type), VPDES.Fac.Lat=first(Fac.Lat),VPDES.Fac.Long=first(Fac.Long),VWUDS.Fac.Lat=first(VWUDS.Lat),VWUDS.Fac.Long=first(VWUDS.Long),
                   Ave_Withdrawal_mgd=median(Withdrawals_MGD,na.rm=T),Ave_Discharge_mgd=median(Discharges_MGD,na.rm=T),County=first(County),Waterbody=first(Waterbody))

VA_River<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/VA_Rivers_Clip.shp")
VA_River<-sp::spTransform(VA_River, CRS("+init=epsg:4269"))#Reproject shapefiles to NAD83=EPSG Code of 4269
VA_River.df<-fortify(VA_River)

case.study.location<- function(VPDES.ID,VWUDS.ID){
  
  full_match_2010_2017_summary$VPDES_County<-ECHO.test_matched$County[match(full_match_2010_2017_summary$VPDES.Facility.ID,ECHO.test_matched$VPDES.Facility.ID)]
  full_match_2010_2017_summary$VWUDS_County<-VWUDS.test_matched$County[match(full_match_2010_2017_summary$VWUDS.Facility.ID,VWUDS.test_matched$VWUDS.Facility.ID)]
  
  VA_Counties.df<-broom::tidy(VA_Counties)
  VA_Counties$polyID<-sapply(slot(VA_Counties,"polygons"), function(x) slot(x, "ID"))
  VA_Counties.df<-merge(VA_Counties.df, VA_Counties, by.x="id", by.y="polyID")
  
  ggplot()+
    geom_polygon(data=VA_Counties.df,aes(x=long, y= lat, group=group),color="#252525",fill="transparent",alpha=0.5)+
    geom_path(data=VA_River.df, aes(x=long,y=lat,group=group,linetype=paste0("Rivers & Streams")), color="#9ecae1", size=1, alpha=0.5)+
    scale_linetype_manual(name="",values=c(1))+
    geom_point(data=subset(full_match_2010_2017_summary,full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID), aes(x=VPDES.Fac.Long, y=VPDES.Fac.Lat,shape="VPDES Facility"),size=3,colour="#034e7b")+
    geom_label_repel(aes(label=full_match_2010_2017_summary$VPDES_County[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID],
                   x=full_match_2010_2017_summary$VPDES.Fac.Long[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID],
                   y=full_match_2010_2017_summary$VPDES.Fac.Lat[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID]))+
    scale_shape_manual(name="", values=17)+
    geom_point(data=subset(full_match_2010_2017_summary,full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID), aes(x=VWUDS.Fac.Long, y=VWUDS.Fac.Lat, size="VWUDS Facility"),colour="#99000d")+
    geom_label_repel(aes(label=full_match_2010_2017_summary$VWUDS_County[full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID], 
                   x=full_match_2010_2017_summary$VWUDS.Fac.Long[full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID],
                   y=full_match_2010_2017_summary$VWUDS.Fac.Lat[full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID]))+
    scale_size_manual(name="", values=3)+
    guides(linetype=guide_legend(override.aes = list(linetype=c("solid"),colour="#9ecae1"),order=1))+
    labs(title = paste0(full_match_2010_2017_summary$VPDES.Name[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID]))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
}

for (i in 1:length(full_match_2010_2017_summary$VPDES.Facility.ID)){
  print(paste("Matched Facility ",i," of ", length(full_match_2010_2017_summary$VPDES.Facility.ID)))
  full_match_2010_2017_summary<-full_match_2010_2017_summary%>%arrange(desc(Ave_Withdrawal_mgd))
  case.study.location(full_match_2010_2017_summary$VPDES.Facility.ID[i],
                      full_match_2010_2017_summary$VWUDS.Facility.ID[i])
  }


###########################################################################################################################################
#-----------------------------------------------------Mann Kendall TS Compile-------------------------------------------------------------#

MK_County_Compile<- function(ECHO_2010_2017,VWUDS_2010_2017,label){
  
FIPS<-as.vector(VA_Counties@data$FIPS)

TS_County_Discharges<-ECHO_2010_2017%>%
  dplyr::group_by(Date,FIPS)%>%
  dplyr::summarise(Discharge=sum(Discharges_MGD,na.rm=T))%>%
  tidyr::spread(FIPS, Discharge)

Missing<-setdiff(FIPS,names(TS_County_Discharges))
TS_County_Discharges[Missing]<-NA
TS_County_Discharges<-TS_County_Discharges[FIPS]

TS_County_Withdrawals<-VWUDS_2010_2017%>%
  dplyr::group_by(Date,FIPS)%>%
  dplyr::summarise(Withdrawal=sum(Withdrawals_MGD,na.rm=T))%>%
  tidyr::spread(FIPS, Withdrawal)

Missing<-setdiff(FIPS,names(TS_County_Withdrawals))
TS_County_Withdrawals[Missing]<-NA
TS_County_Withdrawals<-TS_County_Withdrawals[FIPS]

# Timeseries Consumption in Counties for Mann Kendall Analysis
CU_Function<-function(x,y) (ifelse(is.na(x),0,x)-ifelse(is.na(y),0,y))/(ifelse(is.na(x),0,x))
TS_County_Withdrawals<-TS_County_Withdrawals[,order(names(TS_County_Withdrawals))]
TS_County_Discharges<-TS_County_Discharges[,order(names(TS_County_Discharges))]

TS_CU_County<-data.frame(Date=unique(VWUDS_2010_2017$Date),mapply(CU_Function,TS_County_Withdrawals,TS_County_Discharges),stringsAsFactors = F)

colnames(TS_CU_County)<-gsub("X","",colnames(TS_CU_County))

assign(label,TS_CU_County,envir = .GlobalEnv)

save(TS_CU_County,file=paste0("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/county_",label,".RData"))

}

#---All Sectors---#
#-All Facilities-#
MK_County_Compile(ECHO.test,VWUDS.test,"TS_CU_All")
#-Fully Matched Facilities-#
MK_County_Compile(ECHO.test_matched,VWUDS.test_matched,"TS_CU_Matched")

#---Subsetted by Sector---#

#-All Facilities-#
MK_County_Compile(ECHO.test_energy,VWUDS.test_energy,"TS_CU_All_Energy")
MK_County_Compile(ECHO.test_ag,VWUDS.test_ag,"TS_CU_All_Ag")
MK_County_Compile(ECHO.test_aq,VWUDS.test_aq,"TS_CU_All_Aq")
MK_County_Compile(ECHO.test_commercial,VWUDS.test_commercial,"TS_CU_All_Commercial")
MK_County_Compile(ECHO.test_industrial,VWUDS.test_industrial,"TS_CU_All_Industrial")
MK_County_Compile(ECHO.test_municipal,VWUDS.test_municipal,"TS_CU_All_Municipal")

#-Fully Matched Facilities-#
MK_County_Compile(ECHO.test_match_energy,VWUDS.test_match_energy,"TS_CU_Match_Energy")
MK_County_Compile(ECHO.test_match_ag,VWUDS.test_match_ag,"TS_CU_Match_Ag")
MK_County_Compile(ECHO.test_match_aq,VWUDS.test_match_aq,"TS_CU_Match_Aq")
MK_County_Compile(ECHO.test_match_commercial,VWUDS.test_match_commercial,"TS_CU_Match_Commercial")
MK_County_Compile(ECHO.test_match_industrial,VWUDS.test_match_industrial,"TS_CU_Match_Industrial")
MK_County_Compile(ECHO.test_match_municipal,VWUDS.test_match_municipal,"TS_CU_Match_Municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
MK_County_Compile(ECHO.test_nonenergy,VWUDS.test_match_nonenergy,"TS_CU_All_NonEnergy")

#-Fully Matched Facilities-#
MK_County_Compile(ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"TS_CU_Match_NonEnergy")
