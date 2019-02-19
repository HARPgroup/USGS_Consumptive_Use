###########################################################################################################################################
#-------------------------------------------Estimating Consumptive Use in Virginian HUC 6 Watersheds--------------------------------------#

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

options(scipen=999) #Disable scientific notation
options(digits = 9)
memory.limit(size=100000000)

###########################################################################################################################################
#-------------------------------------------HUC 6 and Virginia Shapefile Manipulation-----------------------------------------------------#

#Load databases and extract required layers
HUC6<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/HUC.gdb",layer='WBDHU6')
VA<-readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC6<-sp::spTransform(HUC6, CRS("+init=epsg:4269"))
VA<-sp::spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC6_Clipped<-gIntersection(HUC6,VA,id=as.character(HUC6@data$HUC6),byid=TRUE,drop_lower_td=TRUE)

#Create HUC6 Dataframe that will be used in future overlay processes
HUC6_Overlay<-HUC6 #Keep integrity of spatial dataframe
HUC6_Overlay@data<-HUC6_Overlay@data[,c(11,12)] 
names(HUC6_Overlay@data)<-c("HUC6","HUC6Name")


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
  
  ####Create Spatial Data Frame from Points and Overlay on Clipped HUC6 Shapefile####
  ####FROM Delivery Transfers####
  #Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
  #Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
  dFrom<-deliveries[!(is.na(deliveries$geomFlat)&is.na(deliveries$geomFlon)),]
  dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
  HUC6_Facilities<-over(dFrom,HUC6_Overlay)#Spatial overlay
  dFrom@data$HUC6Name<-HUC6_Facilities$HUC6Name
  dFrom@data$HUC6<-HUC6_Facilities$HUC6
  
  ####TO Delivery Transfers####
  #Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
  #Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
  dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
  dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
  HUC6_Facilities<-over(dTo,HUC6_Overlay)#Spatial overlay
  dTo@data$HUC6Name<-HUC6_Facilities$HUC6Name
  dTo@data$HUC6<-HUC6_Facilities$HUC6
  
  ####Determine if TO Transfers are leaving watershed boundaries####
  #Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
  #We are only concerned with interbasin transfers and need to identify these with the following code
  dTo@data$interbasin<-NA
  dFrom@data$interbasin<-NA
  #Check each transfer in the delivery VA Hydro transfers to see if its FROM HUC6 is different than its TO HUC6
  
  for (i in 1:length(dTo@data$hydroid)){
    ToHUC<-as.character(dTo@data$HUC6[i])
    FromHUC<-as.character(dFrom@data$HUC6[i])
    if(is.na(ToHUC)){
      ToHUC<-'Null HUC6'
    }
    if(is.na(FromHUC)){
      FromHUC<-'Null HUC6' 
    }
    interbasin<-0
    if(ToHUC!=FromHUC){ #if the HUC6 does not match, mark as interbasin delivery
      interbasin<-1
    }
    dTo@data$interbasin[i]<-interbasin
    dFrom@data$interbasin[i]<-interbasin
  }
  
  ####Sum Net Water In and Out for each HUC6####
  ###FROM Deliveries###
  delf<-dFrom@data
  delf<-delf[delf$interbasin==1,] #if interbasin is equal to 1, it is crossing boundaries---so just include those
  
  delf<-delf%>%
    dplyr::group_by(HUC6Name, Year)%>%
    dplyr::summarize(HUC6=first(HUC6),interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD
  
  delf$HUC6Name<-as.character(delf$HUC6Name)
  delf$HUC6Name[is.na(delf$HUC6Name)]<-'Fell Outside HUC6 Limits'
  
  ###TO Deliveries###
  delt<-dTo@data
  delt<-delt[delt$interbasin==1,] #narrow down to deliveries happening across borders
  
  delt<-
    delt%>%
    dplyr::group_by(HUC6Name, Year)%>%
    dplyr::summarize(HUC6=first(HUC6),interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD
  
  
  delt$HUC6Name<-as.character(delt$HUC6Name)
  delt$HUC6Name[is.na(delt$HUC6Name)]<-'Fell Outside HUC6 Limits'
  
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
  
  ####Create Spatial Data Frame from Points and Overlay on Clipped HUC6 Shapefile####
  ####FROM Release Transfers####
  rFrom<-releases[!(is.na(releases$geomFlat)&is.na(releases$geomFlon)),]
  rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
  HUC6_Facilities<-over(rFrom,HUC6_Overlay)
  rFrom@data$HUC6<-HUC6_Facilities$HUC6
  rFrom@data$HUC6Name<-HUC6_Facilities$HUC6Name
  ####TO Release Transfers####
  rTo<-releases[!(is.na(releases$geomTlat)&is.na(releases$geomTlon)),]
  rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
  HUC6_Facilities<-over(rTo,HUC6_Overlay)
  rTo@data$HUC6<-HUC6_Facilities$HUC6
  rTo@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  ####Determine if Release FROM Transfers are leaving HUC6 boundaries####
  rTo@data$interbasin<-NA
  rFrom@data$interbasin<-NA
  
  for (i in 1:length(rTo@data$hydroid)){
    ToHUC<-as.character(rTo@data$HUC6[i])
    FromHUC<-as.character(rFrom@data$HUC6[i])
    if(is.na(ToHUC)){ #if the HUC code is NA
      ToHUC<-'Null HUC6'
    }
    if(is.na(FromHUC)){
      FromHUC<-'Null HUC6' 
    }
    interbasin<-0
    if(ToHUC!=FromHUC){
      interbasin<-1
    }
    rTo@data$interbasin[i]<-interbasin #1 indicating transfer is crossing watershed boundaries
    rFrom@data$interbasin[i]<-interbasin
  }
  
  ####Sum Net Water In and Out for each HUC6####
  ###FROM Releases###
  relf<-rFrom@data
  relf<-relf[relf$interbasin==1,]#remember intratransfers are indicated with a 0
  
  relf<-relf%>% #Summarise by HUC6 and year
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6), interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)  #MGM to MGD
  
  relf$HUC6Name<-as.character(relf$HUC6Name)
  relf$HUC6Name[is.na(relf$HUC6Name)]<-'Fell Outside HUC6 Limits'
  
  ###TO Releases###
  relt<-rTo@data
  relt<-relt[relt$interbasin==1,]
  
  relt<-relt%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6), interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)
  
  relt$HUC6Name<-as.character(relt$HUC6Name)
  relt$HUC6Name[is.na(relt$HUC6Name)]<-'Fell Outside HUC6 Limits'
  
  assign("relf",relf,envir=.GlobalEnv)
  assign("relt",relt,envir=.GlobalEnv)
  
}
releases_func(rel)

#-----------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------Calculate Net Transfers for Each HUC6 Watershed-------------------------------------------------#

# Loop through each HUC6 and check for summed releases and deliveries
# Water out is defined as the "from's" and Water in are the "to's"
# This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

transfers<- function(relf,delf,relt,delt){
  
  HUC6_waterout<-as.data.table(rbind(relf,delf))
  HUC6_waterout<-HUC6_waterout[, lapply(.SD,sum), by=list(HUC6Name, Year, HUC6), .SDcols=c(4,5)]
  
  HUC6_waterin<-as.data.table(rbind(relt,delt))
  HUC6_waterin<-HUC6_waterin[, lapply(.SD,sum), by=list(HUC6Name, Year, HUC6), .SDcols=c(4,5)]
  
  #---Year 2010---#
  HUC6@data$waterout_2010<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2010"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2010"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2010"])],NA)
  HUC6@data$waterin_2010<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2010"],HUC6_waterin$waterin[HUC6_waterin$Year=="2010"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2010"])],NA)
  HUC6@data$transferred_2010<- (rowSums(HUC6@data[,(10:11)],na.rm=T))
  HUC6@data$transferred_2010<-ifelse(is.na(HUC6@data$waterin_2010)&is.na(HUC6@data$waterout_2010),NA,HUC6@data$transferred_2010)
  
  #---Year 2011---#
  HUC6@data$waterout_2011<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2011"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2011"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2011"])],NA)
  HUC6@data$waterin_2011<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2011"],HUC6_waterin$waterin[HUC6_waterin$Year=="2011"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2011"])],NA)
  HUC6@data$transferred_2011<- (rowSums(HUC6@data[,(13:14)],na.rm=T))
  HUC6@data$transferred_2011<-ifelse(is.na(HUC6@data$waterin_2011)&is.na(HUC6@data$waterout_2011),NA,HUC6@data$transferred_2011)
  
  #---Year 2012---#
  HUC6@data$waterout_2012<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2012"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2012"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2012"])],NA)
  HUC6@data$waterin_2012<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2012"],HUC6_waterin$waterin[HUC6_waterin$Year=="2012"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2012"])],NA)
  HUC6@data$transferred_2012<- (rowSums(HUC6@data[,(16:17)],na.rm=T))
  HUC6@data$transferred_2012<-ifelse(is.na(HUC6@data$waterin_2012)&is.na(HUC6@data$waterout_2012),NA,HUC6@data$transferred_2012)
  
  #---Year 2013---#
  HUC6@data$waterout_2013<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2013"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2013"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2013"])],NA)
  HUC6@data$waterin_2013<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2013"],HUC6_waterin$waterin[HUC6_waterin$Year=="2013"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2013"])],NA)
  HUC6@data$transferred_2013<- (rowSums(HUC6@data[,(19:20)],na.rm=T))
  HUC6@data$transferred_2013<-ifelse(is.na(HUC6@data$waterin_2013)&is.na(HUC6@data$waterout_2013),NA,HUC6@data$transferred_2013)
  
  #---Year 2014---#
  HUC6@data$waterout_2014<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2014"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2014"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2014"])],NA)
  HUC6@data$waterin_2014<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2014"],HUC6_waterin$waterin[HUC6_waterin$Year=="2014"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2014"])],NA)
  HUC6@data$transferred_2014<- (rowSums(HUC6@data[,(22:23)],na.rm=T))
  HUC6@data$transferred_2014<-ifelse(is.na(HUC6@data$waterin_2014)&is.na(HUC6@data$waterout_2014),NA,HUC6@data$transferred_2014)
  
  #---Year 2015---#
  HUC6@data$waterout_2015<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2015"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2015"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2015"])],NA)
  HUC6@data$waterin_2015<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2015"],HUC6_waterin$waterin[HUC6_waterin$Year=="2015"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2015"])],NA)
  HUC6@data$transferred_2015<- (rowSums(HUC6@data[,(25:26)],na.rm=T))
  HUC6@data$transferred_2015<-ifelse(is.na(HUC6@data$waterin_2015)&is.na(HUC6@data$waterout_2015),NA,HUC6@data$transferred_2015)
  
  #---Year 2016---#
  HUC6@data$waterout_2016<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2016"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2016"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2016"])],NA)
  HUC6@data$waterin_2016<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2016"],HUC6_waterin$waterin[HUC6_waterin$Year=="2016"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2016"])],NA)
  HUC6@data$transferred_2016<- (rowSums(HUC6@data[,(28:29)],na.rm=T))
  HUC6@data$transferred_2016<-ifelse(is.na(HUC6@data$waterin_2016)&is.na(HUC6@data$waterout_2016),NA,HUC6@data$transferred_2016)
  
  #---Year 2017---#
  HUC6@data$waterout_2017<-ifelse(HUC6@data$HUC6%in%HUC6_waterout$HUC6[HUC6_waterout$Year=="2017"],-HUC6_waterout$waterout[HUC6_waterout$Year=="2017"][match(HUC6@data$HUC6,HUC6_waterout$HUC6[HUC6_waterout$Year=="2017"])],NA)
  HUC6@data$waterin_2017<-ifelse(HUC6@data$HUC6%in%HUC6_waterin$HUC6[HUC6_waterin$Year=="2017"],HUC6_waterin$waterin[HUC6_waterin$Year=="2017"][match(HUC6@data$HUC6,HUC6_waterin$HUC6[HUC6_waterin$Year=="2017"])],NA)
  HUC6@data$transferred_2017<- (rowSums(HUC6@data[,(31:32)],na.rm=T))
  HUC6@data$transferred_2017<-ifelse(is.na(HUC6@data$waterin_2017)&is.na(HUC6@data$waterout_2017),NA,HUC6@data$transferred_2017)
  
  
  HUC6_Transfers<-data.frame(HUC6Name=HUC6@data$HUC6Name,
                               HUC6=HUC6@data$HUC6,
                               Transfers_2010_MGD=HUC6@data$transferred_2010,
                               Transfers_2011_MGD=HUC6@data$transferred_2011,
                               Transfers_2012_MGD=HUC6@data$transferred_2012,
                               Transfers_2013_MGD=HUC6@data$transferred_2013,
                               Transfers_2014_MGD=HUC6@data$transferred_2014,
                               Transfers_2015_MGD=HUC6@data$transferred_2015,
                               Transfers_2016_MGD=HUC6@data$transferred_2016,
                               Transfers_2017_MGD=HUC6@data$transferred_2017)
  
  assign("HUC6",HUC6,envir = .GlobalEnv)
  assign("HUC6_Transfers",HUC6_Transfers,envir = .GlobalEnv)
  
  
}
transfers(relf,delf,relt,delt)
rm(relf,delf,relt,delt,deliveries)

###########################################################################################################################################
#----------------------------------------------------Calculating Discharges---------------------------------------------------------------#

#---Load in Discharge Data previously cleaned---#

ECHO_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC_statewide.txt", sep="\t", header=T)
ECHO_2010_2017%>%dplyr::summarise(Facilities=n_distinct(VPDES.Facility.ID),Outfalls=n_distinct(OutfallID),Summed_D=sum(Measured_Effluent,na.rm=T),Summed_D_QAQC=sum(Discharges_MGD,na.rm=T))

#---Load in fully matched Discharge Data previously cleaned---#

full_match_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/full_match_2010_2017.txt", sep="\t", header=T)
full_match_2010_2017%>%dplyr::summarise(Facilities=n_distinct(VPDES.Facility.ID),Outfalls=n_distinct(OutfallID))

#-------------Sector Analysis---------------#

# All of these functions are independent. So you can run each of them.

#---Option to Analyze all Sectors at once---#
all_discharge<- function(discharge_db,label){
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC6 Shapefile--#
  HUC6_Facilities<-over(discharge_db,HUC6_Overlay)
  discharge_db@data$HUC6<-HUC6_Facilities$HUC6
  discharge_db@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  ECHO_Resol_Mean_Med<-discharge_db.test%>%dplyr::group_by(OutfallID,Year)%>%dplyr::summarise(Resolved_Mean_Med=mean(Discharges_MGD,na.rm=T))
  discharge_db.test<-merge(discharge_db.test,ECHO_Resol_Mean_Med,by="OutfallID",all.x=T)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC6Name=first(HUC6Name),
                     HUC6=first(HUC6))%>%arrange(desc(Discharges_MGD))
  
  
  HUC6_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  assign(paste0("HUC6_Discharges",label),HUC6_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
all_discharge(ECHO_2010_2017,"") # All Facilities
all_discharge(full_match_2010_2017,"_matched") # Fully Matched Facilities

#---Option to separate by Water Use Sector---#
sector_discharge<- function(discharge_db,sector,label){
  
  discharge_db<-subset(discharge_db,subset=discharge_db$Use.Type==sector)
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC6 Shapefile--#
  HUC6_Facilities<-over(discharge_db,HUC6_Overlay)
  discharge_db@data$HUC6<-HUC6_Facilities$HUC6
  discharge_db@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  ECHO_Resol_Mean_Med<-discharge_db.test%>%dplyr::group_by(OutfallID,Year)%>%dplyr::summarise(Resolved_Mean_Med=mean(Discharges_MGD,na.rm=T))
  discharge_db.test<-merge(discharge_db.test,ECHO_Resol_Mean_Med,by="OutfallID",all.x=T)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC6Name=first(HUC6Name),
                     HUC6=first(HUC6))%>%arrange(desc(Discharges_MGD))
  
  
  HUC6_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  assign(paste0("HUC6_Discharges",label),HUC6_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
sector_discharge(ECHO_2010_2017,"Energy","_energy")
sector_discharge(ECHO_2010_2017,"Agriculture/Irrigation","_ag")
sector_discharge(ECHO_2010_2017,"Commercial","_commercial")
sector_discharge(ECHO_2010_2017,"Industrial","_industrial")
sector_discharge(ECHO_2010_2017,"Municipal","_municipal")

sector_discharge(full_match_2010_2017,"Energy","_match_energy")
sector_discharge(full_match_2010_2017,"Agriculture/Irrigation","_match_ag")
sector_discharge(full_match_2010_2017,"Commercial","_match_commercial")
sector_discharge(full_match_2010_2017,"Industrial","_match_industrial")
sector_discharge(full_match_2010_2017,"Municipal","_match_municipal")

#--Option to look at Non-Energy Sectors--#
nonenergy_discharge<- function(discharge_db,label){
  
  discharge_db<-subset(discharge_db,subset=!discharge_db$Use.Type=="Energy")
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC6 Shapefile--#
  HUC6_Facilities<-over(discharge_db,HUC6_Overlay)
  discharge_db@data$HUC6<-HUC6_Facilities$HUC6
  discharge_db@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  ECHO_Resol_Mean_Med<-discharge_db.test%>%dplyr::group_by(OutfallID,Year)%>%dplyr::summarise(Resolved_Mean_Med=mean(Discharges_MGD,na.rm=T))
  discharge_db.test<-merge(discharge_db.test,ECHO_Resol_Mean_Med,by="OutfallID",all.x=T)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC6Name=first(HUC6Name),
                     HUC6=first(HUC6))%>%arrange(desc(Discharges_MGD))
  
  
  HUC6_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  assign(paste0("HUC6_Discharges",label),HUC6_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
nonenergy_discharge(ECHO_2010_2017,"_nonenergy")
nonenergy_discharge(full_match_2010_2017,"_match_nonenergy")


###########################################################################################################################################
#----------------------------------------------------Calculating Withdrawals--------------------------------------------------------------#

#---Load in Withdrawal Data previously cleaned in VWUDS_QAQC.R---#
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017.RData")
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
  
  #--Overlay with HUC6 Shapefile--#
  HUC6_Facilities<-over(withdrawal_db,HUC6_Overlay)
  withdrawal_db@data$HUC6<-HUC6_Facilities$HUC6
  withdrawal_db@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC6Name=first(HUC6Name),
                     HUC6=first(HUC6))
  
  
  HUC6_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  assign(paste0("HUC6_Withdrawals",label),HUC6_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
all_withdrawal(VWUDS_2010_2017,"") # All Facilities
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
  
  #--Overlay with HUC6 Shapefile--#
  HUC6_Facilities<-over(withdrawal_db,HUC6_Overlay)
  withdrawal_db@data$HUC6<-HUC6_Facilities$HUC6
  withdrawal_db@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC6Name=first(HUC6Name),
                     HUC6=first(HUC6))
  
  
  HUC6_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  assign(paste0("HUC6_Withdrawals",label),HUC6_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
sector_withdrawal(VWUDS_2010_2017,"Energy","_energy")
sector_withdrawal(VWUDS_2010_2017,"Agriculture/Irrigation","_ag")
sector_withdrawal(VWUDS_2010_2017,"Commercial","_commercial")
sector_withdrawal(VWUDS_2010_2017,"Industrial","_industrial")
sector_withdrawal(VWUDS_2010_2017,"Municipal","_municipal")

sector_withdrawal(full_match_2010_2017,"Energy","_match_energy")
sector_withdrawal(full_match_2010_2017,"Agriculture/Irrigation","_match_ag")
sector_withdrawal(full_match_2010_2017,"Commercial","_match_commercial")
sector_withdrawal(full_match_2010_2017,"Industrial","_match_industrial")
sector_withdrawal(full_match_2010_2017,"Municipal","_match_municipal")

#--Option to look at Non-Energy Sectors--#
nonenergy_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=!withdrawal_db$Use.Type=="Energy")
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                                       withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC6 Shapefile--#
  HUC6_Facilities<-over(withdrawal_db,HUC6_Overlay)
  withdrawal_db@data$HUC6<-HUC6_Facilities$HUC6
  withdrawal_db@data$HUC6Name<-HUC6_Facilities$HUC6Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC6Name=first(HUC6Name),
                     HUC6=first(HUC6))
  
  
  HUC6_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC6Name,Year)%>%
    dplyr::summarise(HUC6=first(HUC6),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  assign(paste0("HUC6_Withdrawals",label),HUC6_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(VWUDS_2010_2017,"_nonenergy")
nonenergy_withdrawal(full_match_2010_2017,"_match_nonenergy")

###########################################################################################################################################
#------------------------------------------Apply Withdrawals and Discharges into HUC6 Spatial Dataframe-----------------------------------#

HUC6_discharge_withdrawal<- function(HUC6_Discharges,HUC6_Withdrawals,label){
  #---Year 2010----#
  HUC6@data$Discharges_2010<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2010"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2010"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2010"])],NA)
  HUC6@data$Withdrawals_2010<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2010"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2010"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2010"])],NA)
  
  #---Year 2011----#
  HUC6@data$Discharges_2011<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2011"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2011"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2011"])],NA)
  HUC6@data$Withdrawals_2011<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2011"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2011"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2011"])],NA)
  
  #---Year 2012----#
  HUC6@data$Discharges_2012<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2012"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2012"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2012"])],NA)
  HUC6@data$Withdrawals_2012<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2012"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2012"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2012"])],NA)
  
  #---Year 2013----#
  HUC6@data$Discharges_2013<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2013"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2013"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2013"])],NA)
  HUC6@data$Withdrawals_2013<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2013"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2013"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2013"])],NA)
  
  #---Year 2014----#
  HUC6@data$Discharges_2014<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2014"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2014"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2014"])],NA)
  HUC6@data$Withdrawals_2014<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2014"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2014"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2014"])],NA)
  
  #---Year 2015----#
  HUC6@data$Discharges_2015<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2015"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2015"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2015"])],NA)
  HUC6@data$Withdrawals_2015<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2015"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2015"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2015"])],NA)
  
  #---Year 2016----#
  HUC6@data$Discharges_2016<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2016"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2016"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2016"])],NA)
  HUC6@data$Withdrawals_2016<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2016"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2016"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2016"])],NA)
  
  #---Year 2017----#
  HUC6@data$Discharges_2017<-ifelse(HUC6@data$HUC6%in%HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2017"],HUC6_Discharges$Discharge_MGD[HUC6_Discharges$Year=="2017"][match(HUC6@data$HUC6,HUC6_Discharges$HUC6[HUC6_Discharges$Year=="2017"])],NA)
  HUC6@data$Withdrawals_2017<-ifelse(HUC6@data$HUC6%in%HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2017"],HUC6_Withdrawals$Withdrawals_MGD[HUC6_Withdrawals$Year=="2017"][match(HUC6@data$HUC6,HUC6_Withdrawals$HUC6[HUC6_Withdrawals$Year=="2017"])],NA)
  
  assign(paste0("HUC6",label),HUC6,envir = .GlobalEnv)
}

#---All Sectors---#

#-All Facilities-#
HUC6_discharge_withdrawal(HUC6_Discharges,HUC6_Withdrawals,"")

#-Fully Matched Facilities-#
HUC6_discharge_withdrawal(HUC6_Discharges_matched,HUC6_Withdrawals_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
HUC6_discharge_withdrawal(HUC6_Discharges_energy,HUC6_Withdrawals_energy,"_energy")
HUC6_discharge_withdrawal(HUC6_Discharges_ag,HUC6_Withdrawals_ag,"_ag")
HUC6_discharge_withdrawal(HUC6_Discharges_commercial,HUC6_Withdrawals_commercial,"_commercial")
HUC6_discharge_withdrawal(HUC6_Discharges_industrial,HUC6_Withdrawals_industrial,"_industrial")
HUC6_discharge_withdrawal(HUC6_Discharges_municipal,HUC6_Withdrawals_municipal,"_municipal")

#-Fully Matched Facilities-#
HUC6_discharge_withdrawal(HUC6_Discharges_match_energy,HUC6_Withdrawals_match_energy,"_match_energy")
HUC6_discharge_withdrawal(HUC6_Discharges_match_ag,HUC6_Withdrawals_match_ag,"_match_ag")
HUC6_discharge_withdrawal(HUC6_Discharges_match_commercial,HUC6_Withdrawals_match_commercial,"_match_commercial")
HUC6_discharge_withdrawal(HUC6_Discharges_match_industrial,HUC6_Withdrawals_match_industrial,"_match_industrial")
HUC6_discharge_withdrawal(HUC6_Discharges_match_municipal,HUC6_Withdrawals_match_municipal,"_match_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
HUC6_discharge_withdrawal(HUC6_Discharges_nonenergy,HUC6_Withdrawals_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
HUC6_discharge_withdrawal(HUC6_Discharges_match_nonenergy,HUC6_Withdrawals_match_nonenergy,"_match_nonenergy")


rm(HUC6_Discharges,HUC6_Discharges_ag,HUC6_Discharges_commercial,HUC6_Discharges_energy,HUC6_Discharges_industrial,HUC6_Discharges_match_ag,
   HUC6_Discharges_match_commercial,HUC6_Discharges_match_energy,HUC6_Discharges_match_industrial,HUC6_Discharges_match_municipal,HUC6_Discharges_match_nonenergy,
   HUC6_Discharges_matched,HUC6_Withdrawals,HUC6_Withdrawals_ag,HUC6_Withdrawals_commercial,HUC6_Withdrawals_energy,HUC6_Withdrawals_industrial,
   HUC6_Withdrawals_match_ag,HUC6_Withdrawals_match_commercial,HUC6_Withdrawals_match_energy,HUC6_Withdrawals_match_industrial,HUC6_Withdrawals_match_municipal,
   HUC6_Withdrawals_match_nonenergy,HUC6_Discharges_municipal,HUC6_Discharges_nonenergy,HUC6_Withdrawals_matched,HUC6_Withdrawals_municipal,HUC6_Withdrawals_nonenergy)

###########################################################################################################################################
#-----------------------------------------------Net Water Balance and Consumtpive Use-----------------------------------------------------#

NWB_CU<- function(HUC6,label){
  #---Year 2010----#
  
  #--With Transfers---#
  #transfers have sign--negative mean water is leaving, positive means water is coming in 
  
  HUC6@data$NetWB_2010_t<-(ifelse(is.na(HUC6@data$Discharges_2010),0,HUC6@data$Discharges_2010))+(ifelse(is.na(HUC6@data$transferred_2010),0,HUC6@data$transferred_2010))-(ifelse(is.na(HUC6@data$Withdrawals_2010),0,HUC6@data$Withdrawals_2010))
  
  HUC6@data$NetWB_2010_t<-ifelse(is.na(HUC6@data$Discharges_2010)&is.na(HUC6@data$Withdrawals_2010)&is.na(HUC6@data$transferred_2010),NA,HUC6@data$NetWB_2010_t)
  
  HUC6@data$Consumption_2010_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2010),0,HUC6@data$Withdrawals_2010))+(ifelse(is.na(HUC6@data$waterout_2010),0,-HUC6@data$waterout_2010)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2010),0,HUC6@data$Discharges_2010))+(ifelse(is.na(HUC6@data$waterin_2010),0,HUC6@data$waterin_2010)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2010),0,HUC6@data$Withdrawals_2010))+(ifelse(is.na(HUC6@data$waterout_2010),0,-HUC6@data$waterout_2010)))
  
  HUC6@data$Consumption_2010_t<-ifelse(is.nan(HUC6@data$Consumption_2010_t)|is.infinite(HUC6@data$Consumption_2010_t),NA,HUC6@data$Consumption_2010_t)
  #--Without Transfers---#
  
  HUC6@data$NetWB_2010<-(ifelse(is.na(HUC6@data$Discharges_2010),0,HUC6@data$Discharges_2010))-(ifelse(is.na(HUC6@data$Withdrawals_2010),0,HUC6@data$Withdrawals_2010))
  
  HUC6@data$NetWB_2010<-ifelse(is.na(HUC6@data$Discharges_2010)&is.na(HUC6@data$Withdrawals_2010),NA,HUC6@data$NetWB_2010)
  
  HUC6@data$Consumption_2010<-((ifelse(is.na(HUC6@data$Withdrawals_2010),0,HUC6@data$Withdrawals_2010))-
                                        (ifelse(is.na(HUC6@data$Discharges_2010),0,HUC6@data$Discharges_2010)))/(ifelse(is.na(HUC6@data$Withdrawals_2010),0,HUC6@data$Withdrawals_2010))
  
  HUC6@data$Consumption_2010<-ifelse(is.nan(HUC6@data$Consumption_2010)|is.infinite(HUC6@data$Consumption_2010),NA,HUC6@data$Consumption_2010)
  #---Year 2011----#
  
  #----With transfers---#
  HUC6@data$NetWB_2011_t<-(ifelse(is.na(HUC6@data$Discharges_2011),0,HUC6@data$Discharges_2011))+(ifelse(is.na(HUC6@data$transferred_2011),0,HUC6@data$transferred_2011))-(ifelse(is.na(HUC6@data$Withdrawals_2011),0,HUC6@data$Withdrawals_2011))
  
  HUC6@data$NetWB_2011_t<-ifelse(is.na(HUC6@data$Discharges_2011)&is.na(HUC6@data$Withdrawals_2011)&is.na(HUC6@data$transferred_2011),NA,HUC6@data$NetWB_2011_t)
  
  HUC6@data$Consumption_2011_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2011),0,HUC6@data$Withdrawals_2011))+(ifelse(is.na(HUC6@data$waterout_2011),0,-HUC6@data$waterout_2011)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2011),0,HUC6@data$Discharges_2011))+(ifelse(is.na(HUC6@data$waterin_2011),0,HUC6@data$waterin_2011)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2011),0,HUC6@data$Withdrawals_2011))+(ifelse(is.na(HUC6@data$waterout_2011),0,-HUC6@data$waterout_2011)))
  
  HUC6@data$Consumption_2011_t<-ifelse(is.nan(HUC6@data$Consumption_2011_t)|is.infinite(HUC6@data$Consumption_2011_t),NA,HUC6@data$Consumption_2011_t)
  #---Without Transfers----#
  HUC6@data$NetWB_2011<-(ifelse(is.na(HUC6@data$Discharges_2011),0,HUC6@data$Discharges_2011))-(ifelse(is.na(HUC6@data$Withdrawals_2011),0,HUC6@data$Withdrawals_2011))
  
  HUC6@data$NetWB_2011<-ifelse(is.na(HUC6@data$Discharges_2011)&is.na(HUC6@data$Withdrawals_2011),NA,HUC6@data$NetWB_2011)
  
  HUC6@data$Consumption_2011<-((ifelse(is.na(HUC6@data$Withdrawals_2011),0,HUC6@data$Withdrawals_2011))-
                                        (ifelse(is.na(HUC6@data$Discharges_2011),0,HUC6@data$Discharges_2011)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2011),0,HUC6@data$Withdrawals_2011))
  
  HUC6@data$Consumption_2011<-ifelse(is.nan(HUC6@data$Consumption_2011)|is.infinite(HUC6@data$Consumption_2011),NA,HUC6@data$Consumption_2011)
  #---Year 2012----#
  
  #---With Transfers---#
  HUC6@data$NetWB_2012_t<-(ifelse(is.na(HUC6@data$Discharges_2012),0,HUC6@data$Discharges_2012))+(ifelse(is.na(HUC6@data$transferred_2012),0,HUC6@data$transferred_2012))-(ifelse(is.na(HUC6@data$Withdrawals_2012),0,HUC6@data$Withdrawals_2012))
  
  HUC6@data$NetWB_2012_t<-ifelse(is.na(HUC6@data$Discharges_2012)&is.na(HUC6@data$Withdrawals_2012)&is.na(HUC6@data$transferred_2012),NA,HUC6@data$NetWB_2012_t)
  
  HUC6@data$Consumption_2012_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2012),0,HUC6@data$Withdrawals_2012))+(ifelse(is.na(HUC6@data$waterout_2012),0,-HUC6@data$waterout_2012)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2012),0,HUC6@data$Discharges_2012))+(ifelse(is.na(HUC6@data$waterin_2012),0,HUC6@data$waterin_2012)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2012),0,HUC6@data$Withdrawals_2012))+(ifelse(is.na(HUC6@data$waterout_2012),0,-HUC6@data$waterout_2012)))
  
  
  HUC6@data$Consumption_2012_t<-ifelse(is.nan(HUC6@data$Consumption_2012_t)|is.infinite(HUC6@data$Consumption_2012_t),NA,HUC6@data$Consumption_2012_t)
  
  #---Without Transfers---#
  HUC6@data$NetWB_2012<-(ifelse(is.na(HUC6@data$Discharges_2012),0,HUC6@data$Discharges_2012))-(ifelse(is.na(HUC6@data$Withdrawals_2012),0,HUC6@data$Withdrawals_2012))
  
  HUC6@data$NetWB_2012<-ifelse(is.na(HUC6@data$Discharges_2012)&is.na(HUC6@data$Withdrawals_2012),NA,HUC6@data$NetWB_2012)
  
  HUC6@data$Consumption_2012<-((ifelse(is.na(HUC6@data$Withdrawals_2012),0,HUC6@data$Withdrawals_2012))-
                                        (ifelse(is.na(HUC6@data$Discharges_2012),0,HUC6@data$Discharges_2012)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2012),0,HUC6@data$Withdrawals_2012))
  
  HUC6@data$Consumption_2012<-ifelse(is.nan(HUC6@data$Consumption_2012)|is.infinite(HUC6@data$Consumption_2012),NA,HUC6@data$Consumption_2012)
  #---Year 2013----#
  
  #---With Transfers---#
  HUC6@data$NetWB_2013_t<-(ifelse(is.na(HUC6@data$Discharges_2013),0,HUC6@data$Discharges_2013))+(ifelse(is.na(HUC6@data$transferred_2013),0,HUC6@data$transferred_2013))-(ifelse(is.na(HUC6@data$Withdrawals_2013),0,HUC6@data$Withdrawals_2013))
  
  HUC6@data$NetWB_2013_t<-ifelse(is.na(HUC6@data$Discharges_2013)&is.na(HUC6@data$Withdrawals_2013)&is.na(HUC6@data$transferred_2013),NA,HUC6@data$NetWB_2013_t)
  
  HUC6@data$Consumption_2013_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2013),0,HUC6@data$Withdrawals_2013))+(ifelse(is.na(HUC6@data$waterout_2013),0,-HUC6@data$waterout_2013)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2013),0,HUC6@data$Discharges_2013))+(ifelse(is.na(HUC6@data$waterin_2013),0,HUC6@data$waterin_2013)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2013),0,HUC6@data$Withdrawals_2013))+(ifelse(is.na(HUC6@data$waterout_2013),0,-HUC6@data$waterout_2013)))
  
  HUC6@data$Consumption_2013_t<-ifelse(is.nan(HUC6@data$Consumption_2013_t)|is.infinite(HUC6@data$Consumption_2013_t),NA,HUC6@data$Consumption_2013_t)
  
  #---Without Transfers---#
  HUC6@data$NetWB_2013<-(ifelse(is.na(HUC6@data$Discharges_2013),0,HUC6@data$Discharges_2013))-(ifelse(is.na(HUC6@data$Withdrawals_2013),0,HUC6@data$Withdrawals_2013))
  
  HUC6@data$NetWB_2013<-ifelse(is.na(HUC6@data$Discharges_2013)&is.na(HUC6@data$Withdrawals_2013),NA,HUC6@data$NetWB_2013)
  
  HUC6@data$Consumption_2013<-((ifelse(is.na(HUC6@data$Withdrawals_2013),0,HUC6@data$Withdrawals_2013))-
                                        (ifelse(is.na(HUC6@data$Discharges_2013),0,HUC6@data$Discharges_2013)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2013),0,HUC6@data$Withdrawals_2013))
  
  HUC6@data$Consumption_2013<-ifelse(is.nan(HUC6@data$Consumption_2013)|is.infinite(HUC6@data$Consumption_2013),NA,HUC6@data$Consumption_2013)
  
  #---Year 2014----#
  
  #---With Transfers---#
  HUC6@data$NetWB_2014_t<-(ifelse(is.na(HUC6@data$Discharges_2014),0,HUC6@data$Discharges_2014))+(ifelse(is.na(HUC6@data$transferred_2014),0,HUC6@data$transferred_2014))-(ifelse(is.na(HUC6@data$Withdrawals_2014),0,HUC6@data$Withdrawals_2014))
  
  HUC6@data$NetWB_2014_t<-ifelse(is.na(HUC6@data$Discharges_2014)&is.na(HUC6@data$Withdrawals_2014)&is.na(HUC6@data$transferred_2014),NA,HUC6@data$NetWB_2014_t)
  
  HUC6@data$Consumption_2014_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2014),0,HUC6@data$Withdrawals_2014))+(ifelse(is.na(HUC6@data$waterout_2014),0,-HUC6@data$waterout_2014)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2014),0,HUC6@data$Discharges_2014))+(ifelse(is.na(HUC6@data$waterin_2014),0,HUC6@data$waterin_2014)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2014),0,HUC6@data$Withdrawals_2014))+(ifelse(is.na(HUC6@data$waterout_2014),0,-HUC6@data$waterout_2014)))
  
  HUC6@data$Consumption_2014_t<-ifelse(is.nan(HUC6@data$Consumption_2014_t)|is.infinite(HUC6@data$Consumption_2014_t),NA,HUC6@data$Consumption_2014_t)
  
  #---Without Transfers---#
  HUC6@data$NetWB_2014<-(ifelse(is.na(HUC6@data$Discharges_2014),0,HUC6@data$Discharges_2014))-(ifelse(is.na(HUC6@data$Withdrawals_2014),0,HUC6@data$Withdrawals_2014))
  
  HUC6@data$NetWB_2014<-ifelse(is.na(HUC6@data$Discharges_2014)&is.na(HUC6@data$Withdrawals_2014),NA,HUC6@data$NetWB_2014)
  
  HUC6@data$Consumption_2014<-((ifelse(is.na(HUC6@data$Withdrawals_2014),0,HUC6@data$Withdrawals_2014))-
                                        (ifelse(is.na(HUC6@data$Discharges_2014),0,HUC6@data$Discharges_2014)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2014),0,HUC6@data$Withdrawals_2014))
  
  HUC6@data$Consumption_2014<-ifelse(is.nan(HUC6@data$Consumption_2014)|is.infinite(HUC6@data$Consumption_2014),NA,HUC6@data$Consumption_2014)
  #---Year 2015----#
  
  #---With Transfers---#
  HUC6@data$NetWB_2015_t<-(ifelse(is.na(HUC6@data$Discharges_2015),0,HUC6@data$Discharges_2015))+(ifelse(is.na(HUC6@data$transferred_2015),0,HUC6@data$transferred_2015))-(ifelse(is.na(HUC6@data$Withdrawals_2015),0,HUC6@data$Withdrawals_2015))
  
  HUC6@data$NetWB_2015_t<-ifelse(is.na(HUC6@data$Discharges_2015)&is.na(HUC6@data$Withdrawals_2015)&is.na(HUC6@data$transferred_2015),NA,HUC6@data$NetWB_2015_t)
  
  HUC6@data$Consumption_2015_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2015),0,HUC6@data$Withdrawals_2015))+(ifelse(is.na(HUC6@data$waterout_2015),0,-HUC6@data$waterout_2015)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2015),0,HUC6@data$Discharges_2015))+(ifelse(is.na(HUC6@data$waterin_2015),0,HUC6@data$waterin_2015)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2015),0,HUC6@data$Withdrawals_2015))+(ifelse(is.na(HUC6@data$waterout_2015),0,-HUC6@data$waterout_2015)))
  
  HUC6@data$Consumption_2015_t<-ifelse(is.nan(HUC6@data$Consumption_2015_t)|is.infinite(HUC6@data$Consumption_2015_t),NA,HUC6@data$Consumption_2015_t)
  
  #---Without Transfers---#
  HUC6@data$NetWB_2015<-(ifelse(is.na(HUC6@data$Discharges_2015),0,HUC6@data$Discharges_2015))-(ifelse(is.na(HUC6@data$Withdrawals_2015),0,HUC6@data$Withdrawals_2015))
  
  HUC6@data$NetWB_2015<-ifelse(is.na(HUC6@data$Discharges_2015)&is.na(HUC6@data$Withdrawals_2015),NA,HUC6@data$NetWB_2015)
  
  HUC6@data$Consumption_2015<-((ifelse(is.na(HUC6@data$Withdrawals_2015),0,HUC6@data$Withdrawals_2015))-
                                        (ifelse(is.na(HUC6@data$Discharges_2015),0,HUC6@data$Discharges_2015)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2015),0,HUC6@data$Withdrawals_2015))
  
  HUC6@data$Consumption_2015<-ifelse(is.nan(HUC6@data$Consumption_2015)|is.infinite(HUC6@data$Consumption_2015),NA,HUC6@data$Consumption_2015)
  
  #---Year 2016----#
  
  #---With Transfers---#
  HUC6@data$NetWB_2016_t<-(ifelse(is.na(HUC6@data$Discharges_2016),0,HUC6@data$Discharges_2016))+(ifelse(is.na(HUC6@data$transferred_2016),0,HUC6@data$transferred_2016))-(ifelse(is.na(HUC6@data$Withdrawals_2016),0,HUC6@data$Withdrawals_2016))
  
  HUC6@data$NetWB_2016_t<-ifelse(is.na(HUC6@data$Discharges_2016)&is.na(HUC6@data$Withdrawals_2016)&is.na(HUC6@data$transferred_2016),NA,HUC6@data$NetWB_2016_t)
  
  HUC6@data$Consumption_2016_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2016),0,HUC6@data$Withdrawals_2016))+(ifelse(is.na(HUC6@data$waterout_2016),0,-HUC6@data$waterout_2016)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2016),0,HUC6@data$Discharges_2016))+(ifelse(is.na(HUC6@data$waterin_2016),0,HUC6@data$waterin_2016)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2016),0,HUC6@data$Withdrawals_2016))+(ifelse(is.na(HUC6@data$waterout_2016),0,-HUC6@data$waterout_2016)))
  
  HUC6@data$Consumption_2016_t<-ifelse(is.nan(HUC6@data$Consumption_2016_t)|is.infinite(HUC6@data$Consumption_2016_t),NA,HUC6@data$Consumption_2016_t)
  
  #---Without Transfers---#
  HUC6@data$NetWB_2016<-(ifelse(is.na(HUC6@data$Discharges_2016),0,HUC6@data$Discharges_2016))-(ifelse(is.na(HUC6@data$Withdrawals_2016),0,HUC6@data$Withdrawals_2016))
  
  HUC6@data$NetWB_2016<-ifelse(is.na(HUC6@data$Discharges_2016)&is.na(HUC6@data$Withdrawals_2016),NA,HUC6@data$NetWB_2016)
  
  HUC6@data$Consumption_2016<-((ifelse(is.na(HUC6@data$Withdrawals_2016),0,HUC6@data$Withdrawals_2016))-
                                        (ifelse(is.na(HUC6@data$Discharges_2016),0,HUC6@data$Discharges_2016)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2016),0,HUC6@data$Withdrawals_2016))
  
  HUC6@data$Consumption_2016<-ifelse(is.nan(HUC6@data$Consumption_2016)|is.infinite(HUC6@data$Consumption_2016),NA,HUC6@data$Consumption_2016)
  
  #---Year 2017----#
  
  #---With Transfers---#
  HUC6@data$NetWB_2017_t<-(ifelse(is.na(HUC6@data$Discharges_2017),0,HUC6@data$Discharges_2017))+(ifelse(is.na(HUC6@data$transferred_2017),0,HUC6@data$transferred_2017))-(ifelse(is.na(HUC6@data$Withdrawals_2017),0,HUC6@data$Withdrawals_2017))
  
  HUC6@data$NetWB_2017_t<-ifelse(is.na(HUC6@data$Discharges_2017)&is.na(HUC6@data$Withdrawals_2017)&is.na(HUC6@data$transferred_2017),NA,HUC6@data$NetWB_2017_t)
  
  HUC6@data$Consumption_2017_t<-(((ifelse(is.na(HUC6@data$Withdrawals_2017),0,HUC6@data$Withdrawals_2017))+(ifelse(is.na(HUC6@data$waterout_2017),0,-HUC6@data$waterout_2017)))-
                                          (((ifelse(is.na(HUC6@data$Discharges_2017),0,HUC6@data$Discharges_2017))+(ifelse(is.na(HUC6@data$waterin_2017),0,HUC6@data$waterin_2017)))))/
    ((ifelse(is.na(HUC6@data$Withdrawals_2017),0,HUC6@data$Withdrawals_2017))+(ifelse(is.na(HUC6@data$waterout_2017),0,-HUC6@data$waterout_2017)))
  
  HUC6@data$Consumption_2017_t<-ifelse(is.nan(HUC6@data$Consumption_2017_t)|is.infinite(HUC6@data$Consumption_2017_t),NA,HUC6@data$Consumption_2017_t)
  
  #---Without Transfers---#
  HUC6@data$NetWB_2017<-(ifelse(is.na(HUC6@data$Discharges_2017),0,HUC6@data$Discharges_2017))-(ifelse(is.na(HUC6@data$Withdrawals_2017),0,HUC6@data$Withdrawals_2017))
  
  HUC6@data$NetWB_2017<-ifelse(is.na(HUC6@data$Discharges_2017)&is.na(HUC6@data$Withdrawals_2017),NA,HUC6@data$NetWB_2017)
  
  HUC6@data$Consumption_2017<-((ifelse(is.na(HUC6@data$Withdrawals_2017),0,HUC6@data$Withdrawals_2017))-(ifelse(is.na(HUC6@data$Discharges_2017),0,HUC6@data$Discharges_2017)))/
    (ifelse(is.na(HUC6@data$Withdrawals_2017),0,HUC6@data$Withdrawals_2017))
  
  HUC6@data$Consumption_2017<-ifelse(is.nan(HUC6@data$Consumption_2017)|is.infinite(HUC6@data$Consumption_2017),NA,HUC6@data$Consumption_2017)
  
  
  HUC6_glimpse<<-HUC6@data
  
  assign(paste0("HUC6",label),HUC6,envir = .GlobalEnv)
  
}

#---All Sectors---#

#-All Facilities-#
NWB_CU(HUC6,"")
#-Fully Matched Facilities-#
NWB_CU(HUC6_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
NWB_CU(HUC6_energy,"_energy")
NWB_CU(HUC6_ag,"_ag")
NWB_CU(HUC6_commercial,"_commercial")
NWB_CU(HUC6_industrial,"_industrial")
NWB_CU(HUC6_municipal,"_municipal")

#-Fully Matched Facilities-#
NWB_CU(HUC6_match_energy,"_match_energy")
NWB_CU(HUC6_match_ag,"_match_ag")
NWB_CU(HUC6_match_commercial,"_match_commercial")
NWB_CU(HUC6_match_industrial,"_match_industrial")
NWB_CU(HUC6_match_municipal,"_match_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
NWB_CU(HUC6_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
NWB_CU(HUC6_match_nonenergy,"_match_nonenergy")


###########################################################################################################################################
#--------------------------------Long Term Average (2010-2017) Net Water Balance and Consumtpive Use--------------------------------------#

Ave_NWB_CU<- function(HUC6,label){
  
  #----Discharges-----#
  
  #Be careful for NA values#
  for (i in 1:length(HUC6@data$TNMID)){
    HUC6@data$Discharge_sum[i]<-(ifelse(is.na(HUC6@data$Discharges_2010[i]),0,HUC6@data$Discharges_2010[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2011[i]),0,HUC6@data$Discharges_2011[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2012[i]),0,HUC6@data$Discharges_2012[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2013[i]),0,HUC6@data$Discharges_2013[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2014[i]),0,HUC6@data$Discharges_2014[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2015[i]),0,HUC6@data$Discharges_2015[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2016[i]),0,HUC6@data$Discharges_2016[i])+
                                          ifelse(is.na(HUC6@data$Discharges_2017[i]),0,HUC6@data$Discharges_2017[i]))
  }
  HUC6@data$Discharge_ave<-HUC6@data$Discharge_sum/8
  HUC6@data$Discharge_ave<-ifelse(HUC6@data$Discharge_ave==0,NA,HUC6@data$Discharge_ave)
  
  #----Withdrawals----#
  for (i in 1:length(HUC6@data$TNMID)){
    HUC6@data$Withdrawal_sum[i]<-(ifelse(is.na(HUC6@data$Withdrawals_2010[i]),0,HUC6@data$Withdrawals_2010[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2011[i]),0,HUC6@data$Withdrawals_2011[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2012[i]),0,HUC6@data$Withdrawals_2012[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2013[i]),0,HUC6@data$Withdrawals_2013[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2014[i]),0,HUC6@data$Withdrawals_2014[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2015[i]),0,HUC6@data$Withdrawals_2015[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2016[i]),0,HUC6@data$Withdrawals_2016[i])+
                                           ifelse(is.na(HUC6@data$Withdrawals_2017[i]),0,HUC6@data$Withdrawals_2017[i]))
  }
  HUC6@data$Withdrawal_ave<-HUC6@data$Withdrawal_sum/8
  HUC6@data$Withdrawal_ave<-ifelse(HUC6@data$Withdrawal_ave==0,NA,HUC6@data$Withdrawal_ave)
  
  
  #----Net Water Balance-----#
  for (i in 1:length(HUC6@data$TNMID)){
    HUC6@data$NetWB_sum[i]<-(ifelse(is.na(HUC6@data$NetWB_2010[i]),0,HUC6@data$NetWB_2010[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2011[i]),0,HUC6@data$NetWB_2011[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2012[i]),0,HUC6@data$NetWB_2012[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2013[i]),0,HUC6@data$NetWB_2013[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2014[i]),0,HUC6@data$NetWB_2014[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2015[i]),0,HUC6@data$NetWB_2015[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2016[i]),0,HUC6@data$NetWB_2016[i])+
                                      ifelse(is.na(HUC6@data$NetWB_2017[i]),0,HUC6@data$NetWB_2017[i]))
  }
  HUC6@data$NetWB_ave<-HUC6@data$NetWB_sum/8
  HUC6@data$NetWB_ave<-ifelse(HUC6@data$NetWB_ave==0,NA,HUC6@data$NetWB_ave)
  
  #----Consumption----#
  
  for (i in 1:length(HUC6@data$TNMID)){
    HUC6@data$Consumption_sum[i]<-(ifelse(is.na(HUC6@data$Consumption_2010[i]),0,HUC6@data$Consumption_2010[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2011[i]),0,HUC6@data$Consumption_2011[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2012[i]),0,HUC6@data$Consumption_2012[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2013[i]),0,HUC6@data$Consumption_2013[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2014[i]),0,HUC6@data$Consumption_2014[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2015[i]),0,HUC6@data$Consumption_2015[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2016[i]),0,HUC6@data$Consumption_2016[i])+
                                            ifelse(is.na(HUC6@data$Consumption_2017[i]),0,HUC6@data$Consumption_2017[i]))
  }
  
  HUC6@data$Consumption_ave<-HUC6@data$Consumption_sum/8
  HUC6@data$Consumption_ave<-ifelse(HUC6@data$Consumption_ave==0,NA,HUC6@data$Consumption_ave)
  
  #-----------------With Transfers-------------------------#
  for (i in 1:length(HUC6@data$TNMID)){
    HUC6@data$NetWB_t_sum[i]<-(ifelse(is.na(HUC6@data$NetWB_2010_t[i]),0,HUC6@data$NetWB_2010_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2011_t[i]),0,HUC6@data$NetWB_2011_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2012_t[i]),0,HUC6@data$NetWB_2012_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2013_t[i]),0,HUC6@data$NetWB_2013_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2014_t[i]),0,HUC6@data$NetWB_2014_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2015_t[i]),0,HUC6@data$NetWB_2015_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2016_t[i]),0,HUC6@data$NetWB_2016_t[i])+
                                        ifelse(is.na(HUC6@data$NetWB_2017_t[i]),0,HUC6@data$NetWB_2017_t[i]))
  }
  HUC6@data$NetWB_t_ave<-HUC6@data$NetWB_t_sum/8
  HUC6@data$NetWB_t_ave<-ifelse(HUC6@data$NetWB_t_ave==0,NA,HUC6@data$NetWB_t_ave)
  
  for (i in 1:length(HUC6@data$TNMID)){
    HUC6@data$Consumption_t_sum[i]<-(ifelse(is.na(HUC6@data$Consumption_2010_t[i]),0,HUC6@data$Consumption_2010_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2011_t[i]),0,HUC6@data$Consumption_2011_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2012_t[i]),0,HUC6@data$Consumption_2012_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2013_t[i]),0,HUC6@data$Consumption_2013_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2014_t[i]),0,HUC6@data$Consumption_2014_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2015_t[i]),0,HUC6@data$Consumption_2015_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2016_t[i]),0,HUC6@data$Consumption_2016_t[i])+
                                              ifelse(is.na(HUC6@data$Consumption_2017_t[i]),0,HUC6@data$Consumption_2017_t[i]))
  }
  
  HUC6@data$Consumption_t_ave<-HUC6@data$Consumption_t_sum/8
  HUC6@data$Consumption_t_ave<-ifelse(HUC6@data$Consumption_t_ave==0,NA,HUC6@data$Consumption_t_ave)
  
  HUC6_glimpse<-HUC6@data
  HUC6_glimpse[10:93]<-sapply(HUC6_glimpse[10:93],as.numeric)
  HUC6_glimpse[10:93]<<-round(HUC6_glimpse[10:93], digits=2)
  
  assign(paste0("HUC6",label),HUC6,envir = .GlobalEnv)
  
}


#---All Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC6,"")
#-Fully Matched Facilities-#
Ave_NWB_CU(HUC6_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
Ave_NWB_CU(HUC6_energy,"_energy")
Ave_NWB_CU(HUC6_ag,"_ag")
Ave_NWB_CU(HUC6_commercial,"_commercial")
Ave_NWB_CU(HUC6_industrial,"_industrial")
Ave_NWB_CU(HUC6_municipal,"_municipal")

#-Fully Matched Facilities-#
Ave_NWB_CU(HUC6_match_energy,"_match_energy")
Ave_NWB_CU(HUC6_match_ag,"_match_ag")
Ave_NWB_CU(HUC6_match_commercial,"_match_commercial")
Ave_NWB_CU(HUC6_match_industrial,"_match_industrial")
Ave_NWB_CU(HUC6_match_municipal,"_match_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC6_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
Ave_NWB_CU(HUC6_match_nonenergy,"_match_nonenergy")

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


#-----HUC6 Labels-----#
HUC6_labels<- function(HUC6){
  HUC6_Centroids<-as.data.frame(coordinates(HUC6))
  names(HUC6_Centroids)<-c("Longitude","Latitude")
  HUC6_Centroids$HUC6<-HUC6_glimpse$HUC6
  HUC6_Names<-subset(HUC6_glimpse,select=c(2,3))
  HUC6_Centroids<-merge(HUC6_Centroids,HUC6_Names,by="HUC6")
  
  ggplot()+
    geom_polygon(data=HUC6,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
    geom_text(data=HUC6_Centroids,aes(x=Longitude,y=Latitude,label=HUC6Name),size=1)+
    scale_colour_manual(values=c("#252525"))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
}
HUC6_labels(HUC6)


###########################################################################################################################################
#-------------------------------------------Distribution of Discharging Facilities in HUC6----------------------------------------------#

HUC6_discharge<- function(HUC6,ECHO_points, label){
  
  HUC6.df<-broom::tidy(HUC6)
  HUC6$polyID<-sapply(slot(HUC6,"polygons"), function(x) slot(x, "ID"))
  HUC6.df<-merge(HUC6.df, HUC6, by.x="id", by.y="polyID")
  
  Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
  
  ggplot()+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group,
          fill= cut(HUC6.df$Discharge_ave,breaks=c(0,1,5,10,25,50,max(HUC6.df$Discharge_ave,na.rm=T)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Summed Discharge (MGD)", values=(Dis_Discrete), 
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="Outfall"),
               size=1.25,colour="#252525")+
    scale_shape_manual(name="", values=17)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
    labs(title = paste0("Average Annual Total Discharge (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  
}

#---All Sectors---#

#-All Facilities-#
HUC6_discharge(HUC6,ECHO.test,"All Reporting Facilities in All Sectors")
#-Fully Matched Facilities-#
HUC6_discharge(HUC6_matched,ECHO.test_matched,"Matched Facilities in All Sectors")

#---Subsetted by Sector---#

#-All Facilities-#
HUC6_discharge(HUC6_energy,ECHO.test_energy,"Energy Facilities")
HUC6_discharge(HUC6_ag,ECHO.test_ag,"Agriculture/Irrigation Facilities")
HUC6_discharge(HUC6_commercial,ECHO.test_commercial,"Commercial Facilities")
HUC6_discharge(HUC6_industrial,ECHO.test_industrial,"Industrial Facilities")
HUC6_discharge(HUC6_municipal,ECHO.test_municipal,"Municipal Facilities")

#-Fully Matched Facilities-#
HUC6_discharge(HUC6_match_energy,ECHO.test_match_energy,"Matched Energy Facilities")
HUC6_discharge(HUC6_match_ag,ECHO.test_match_ag,"Matched Agriculture/Irrigation Facilities")
HUC6_discharge(HUC6_match_commercial,ECHO.test_match_commercial,"Matched Commercial Facilities")
HUC6_discharge(HUC6_match_industrial,ECHO.test_match_industrial,"Matched Industrial Facilities")
HUC6_discharge(HUC6_match_municipal,ECHO.test_match_municipal,"Matched Municipal Facilities")

#---Non-Energy Sectors---#

#-All Facilities-#
HUC6_discharge(HUC6_nonenergy,ECHO.test_nonenergy,"Non-Energy Facilities")

#-Fully Matched Facilities-#
HUC6_discharge(HUC6_nonenergy,ECHO.test_match_nonenergy,"Matched Non-Energy Facilities")

###########################################################################################################################################
#-----------------------------------------Distribution of Withdrawing Facilities in HUC6------------------------------------------------#

HUC6_withdrawal<- function(HUC6,VWUDS_points,label){
  
  HUC6.df<-broom::tidy(HUC6)
  HUC6$polyID<-sapply(slot(HUC6,"polygons"), function(x) slot(x, "ID"))
  HUC6.df<-merge(HUC6.df, HUC6, by.x="id", by.y="polyID")
  
  With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")
  
  ggplot()+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group,
          fill= cut(HUC6.df$Withdrawal_ave,breaks=c(0,1,5,10,25,50,max(HUC6.df$Withdrawal_ave,na.rm=TRUE)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Summed Withdrawal (MGD)", values=(With_Discrete),
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, shape="Point Source"),
               size=1.54,colour="#252525")+
    scale_shape_manual(name="", values=20)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
    labs(title = paste0("Average Annual Summed Withdrawal (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
}

#---All Sectors---#

#-All Facilities-#
HUC6_withdrawal(HUC6,VWUDS.test,"All Reporting Facilities in All Sectors")
#-Fully Matched Facilities-#
HUC6_withdrawal(HUC6_matched,VWUDS.test_matched,"Matched Facilities in All Sectors")

#---Subsetted by Sector---#

#-All Facilities-#
HUC6_withdrawal(HUC6_energy,VWUDS.test_energy,"Energy Facilities")
HUC6_withdrawal(HUC6_ag,VWUDS.test_ag,"Agriculture/Irrigation Facilities")
HUC6_withdrawal(HUC6_commercial,VWUDS.test_commercial,"Commercial Facilities")
HUC6_withdrawal(HUC6_industrial,VWUDS.test_industrial,"Industrial Facilities")
HUC6_withdrawal(HUC6_municipal,VWUDS.test_municipal,"Municipal Facilities")

#-Fully Matched Facilities-#
HUC6_withdrawal(HUC6_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities")
HUC6_withdrawal(HUC6_match_ag,VWUDS.test_match_ag,"Matched Agriculture/Irrigation Facilities")
HUC6_withdrawal(HUC6_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities")
HUC6_withdrawal(HUC6_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities")
HUC6_withdrawal(HUC6_match_municipal,VWUDS.test_match_municipal,"Matched Municipal Facilities")

#---Non-Energy Sectors---#

#-All Facilities-#
HUC6_withdrawal(HUC6_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities")

#-Fully Matched Facilities-#
HUC6_withdrawal(HUC6_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities")


###########################################################################################################################################
#---------------------------------------------------Consumption over HUC 6 Watersheds-------------------------------------------------------------#

HUC6_consumption<- function(HUC6, ECHO_points ,VWUDS_points, label){
  HUC6.df<-broom::tidy(HUC6)
  HUC6$polyID<-sapply(slot(HUC6,"polygons"), function(x) slot(x, "ID"))
  HUC6.df<-merge(HUC6.df, HUC6, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ggplot()+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC6.df$Consumption_ave,breaks=c(quantile(HUC6.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(round(quantile(HUC6.df$Consumption_ave,c(0.0),na.rm=T),digits=2),"-",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="Outfall"),
               size=1.75,colour="#252525")+
    scale_shape_manual(name="", values=17)+
    geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, size="Withdrawing Source"),
               colour="#252525")+
    scale_size_manual(name="", values=1.75)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           shape=guide_legend(order=3),
           size=guide_legend(order=4))+
    labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  
}

#-All Facilities-#
HUC6_consumption(HUC6,ECHO.test,VWUDS.test,"All Reporting Facilities in All Sectors")
#-Fully Matched Facilities-#
HUC6_consumption(HUC6_matched,ECHO.test_matched,VWUDS.test_matched,"Matched Facilities in All Sectors")

#---Subsetted by Sector---#

#-All Facilities-#
HUC6_consumption(HUC6_energy,ECHO.test_energy,VWUDS.test_energy,"Energy Facilities")
HUC6_consumption(HUC6_ag,ECHO.test_ag,VWUDS.test_ag,"Agriculture/Irrigation Facilities")
HUC6_consumption(HUC6_commercial,ECHO.test_commercial,VWUDS.test_commercial,"Commercial Facilities")
HUC6_consumption(HUC6_industrial,ECHO.test_industrial,VWUDS.test_industrial,"Industrial Facilities")
HUC6_consumption(HUC6_municipal,ECHO.test_municipal,VWUDS.test_municipal,"Municipal Facilities")

#-Fully Matched Facilities-#
HUC6_consumption(HUC6_match_energy,ECHO.test_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities")
HUC6_consumption(HUC6_match_ag,ECHO.test_match_ag,VWUDS.test_match_ag,"Matched Agriculture/Irrigation Facilities")
HUC6_consumption(HUC6_match_commercial,ECHO.test_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities")
HUC6_consumption(HUC6_match_industrial,ECHO.test_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities")
HUC6_consumption(HUC6_match_municipal,ECHO.test_match_municipal,VWUDS.test_match_municipal,"Matched Municipal Facilities")

#---Non-Energy Sectors---#

#-All Facilities-#
HUC6_consumption(HUC6_nonenergy,ECHO.test_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities")

#-Fully Matched Facilities-#
HUC6_consumption(HUC6_nonenergy,ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities")

###########################################################################################################################################
#-----------------------------------------Consumption over HUC 6 Watersheds (without points)------------------------------------------------------#

HUC6_consumption_nopnt<- function(HUC6, label){
  HUC6.df<-broom::tidy(HUC6)
  HUC6$polyID<-sapply(slot(HUC6,"polygons"), function(x) slot(x, "ID"))
  HUC6.df<-merge(HUC6.df, HUC6, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ggplot()+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC6.df$Consumption_ave,breaks=c(quantile(HUC6.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(round(quantile(HUC6.df$Consumption_ave,c(0.0),na.rm=T),digits=2),"-",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)),
           shape=guide_legend(order=3),
           size=guide_legend(order=4))+
    labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  
}

#-All Facilities-#
HUC6_consumption_nopnt(HUC6,"All Reporting Facilities in All Sectors")
#-Fully Matched Facilities-#
HUC6_consumption_nopnt(HUC6_matched,"Matched Facilities in All Sectors")

#---Subsetted by Sector---#

#-All Facilities-#
HUC6_consumption_nopnt(HUC6_energy,"Energy Facilities")
HUC6_consumption_nopnt(HUC6_ag,"Agriculture/Irrigation Facilities")
HUC6_consumption_nopnt(HUC6_commercial,"Commercial Facilities")
HUC6_consumption_nopnt(HUC6_industrial,"Industrial Facilities")
HUC6_consumption_nopnt(HUC6_municipal,"Municipal Facilities")

#-Fully Matched Facilities-#
HUC6_consumption_nopnt(HUC6_match_energy,"Matched Energy Facilities")
HUC6_consumption_nopnt(HUC6_match_ag,"Matched Agriculture/Irrigation Facilities")
HUC6_consumption_nopnt(HUC6_match_commercial,"Matched Commercial Facilities")
HUC6_consumption_nopnt(HUC6_match_industrial,"Matched Industrial Facilities")
HUC6_consumption_nopnt(HUC6_match_municipal,"Matched Municipal Facilities")

#---Non-Energy Sectors---#

#-All Facilities-#
HUC6_consumption_nopnt(HUC6_nonenergy,"Non-Energy Facilities")

#-Fully Matched Facilities-#
HUC6_consumption_nopnt(HUC6_nonenergy,"Matched Non-Energy Facilities")


###########################################################################################################################################
#---------------------------------------------Net Water Balance over HUC 6 Watersheds-------------------------------------------------------------#


HUC6_NWB<- function(HUC6,label){
  
  HUC6.df<-broom::tidy(HUC6)
  HUC6$polyID<-sapply(slot(HUC6,"polygons"), function(x) slot(x, "ID"))
  HUC6.df<-merge(HUC6.df, HUC6, by.x="id", by.y="polyID")
  
  NWB_Discrete<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#2b8cbe")
  
  ggplot()+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group,
          fill= cut(HUC6.df$NetWB_ave,breaks=c(quantile(HUC6.df$NetWB_ave,c(0),na.rm=T),-250,-150,-10,0,quantile(HUC6.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
    scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete,
                      labels=c(paste(-1250,"-",-250),
                               paste(-250,"-",-150),
                               paste(-150,"-",-10),
                               paste(-10,"-",0),
                               paste(0,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(
      data=HUC6.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    labs(title = paste0("Average Net Water Balance (MGD) 2010-2017: ",label))+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  
  
}


###########################################################################################################################################
#-----------------------------------------------Case Study Facility Locations-------------------------------------------------------------#

full_match_2010_2017_summary<-full_match_2010_2017%>%dplyr::group_by(VPDES.Facility.ID)%>%
  dplyr::summarise(VWUDS.Facility.ID=first(VWUDS.Facility.ID),OutfallID=first(OutfallID),VWUDS.Name=first(VWUDS.Name),VPDES.Name=first(VPDES.Name),
                   Use.Type=first(Use.Type), VPDES.Fac.Lat=first(Fac.Lat),VPDES.Fac.Long=first(Fac.Long),VWUDS.Fac.Lat=first(VWUDS.Lat),VWUDS.Fac.Long=first(VWUDS.Long),
                   Ave_Withdrawal_mgd=mean(Withdrawals_MGD,na.rm=T),Ave_Discharge_mgd=mean(Discharges_MGD,na.rm=T),HUC6Name=first(HUC6Name),Waterbody=first(Waterbody))

VA_River<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/VA_Rivers_Clip.shp")
VA_River<-sp::spTransform(VA_River, CRS("+init=epsg:4269"))#Reproject shapefiles to NAD83=EPSG Code of 4269
VA_River.df<-fortify(VA_River)

case.study.location<- function(VPDES.ID,VWUDS.ID){
  
  full_match_2010_2017_summary$VPDES_HUC6<-ECHO.test_matched$HUC6Name[match(full_match_2010_2017_summary$VPDES.Facility.ID,ECHO.test_matched$VPDES.Facility.ID)]
  full_match_2010_2017_summary$VWUDS_HUC6<-VWUDS.test_matched$HUC6Name[match(full_match_2010_2017_summary$VWUDS.Facility.ID,VWUDS.test_matched$VWUDS.Facility.ID)]
  
  HUC6.df<-broom::tidy(HUC6)
  HUC6$polyID<-sapply(slot(HUC6,"polygons"), function(x) slot(x, "ID"))
  HUC6.df<-merge(HUC6.df, HUC6, by.x="id", by.y="polyID")
  
  ggplot()+
    geom_polygon(data=HUC6.df,aes(x=long, y= lat, group=group),color="#252525",fill="transparent",alpha=0.5)+
    geom_path(data=VA_River.df, aes(x=long,y=lat,group=group,linetype=paste0("Rivers & Streams")), color="#9ecae1", size=1, alpha=0.5)+
    scale_linetype_manual(name="",values=c(1))+
    geom_point(data=subset(full_match_2010_2017_summary,full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID), aes(x=VPDES.Fac.Long, y=VPDES.Fac.Lat,shape="VPDES Facility"),size=3,colour="#034e7b")+
    geom_label_repel(aes(label=full_match_2010_2017_summary$VPDES_HUC6[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID],
                         x=full_match_2010_2017_summary$VPDES.Fac.Long[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID],
                         y=full_match_2010_2017_summary$VPDES.Fac.Lat[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID]))+
    scale_shape_manual(name="", values=17)+
    geom_point(data=subset(full_match_2010_2017_summary,full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID), aes(x=VWUDS.Fac.Long, y=VWUDS.Fac.Lat, size="VWUDS Facility"),colour="#99000d")+
    geom_label_repel(aes(label=full_match_2010_2017_summary$VWUDS_HUC6[full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID], 
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
  case.study.location(full_match_2010_2017_summary$VPDES.Facility.ID[i],full_match_2010_2017_summary$VWUDS.Facility.ID[i])
}


###########################################################################################################################################
#-----------------------------------------------------Mann Kendall TS Compile-------------------------------------------------------------#

MK_HUC6_Compile<- function(ECHO_2010_2017,VWUDS_2010_2017,label){
  
  HUC6<-as.vector(HUC6@data$HUC6)
  
  TS_HUC6_Discharges<-ECHO_2010_2017%>%
    dplyr::group_by(Date,HUC6)%>%
    dplyr::summarise(Discharge=sum(Discharges_MGD,na.rm=T))%>%
    tidyr::spread(HUC6, Discharge)
  
  Missing<-setdiff(HUC6,names(TS_HUC6_Discharges))
  TS_HUC6_Discharges[Missing]<-NA
  TS_HUC6_Discharges<-TS_HUC6_Discharges[HUC6]
  
  TS_HUC6_Withdrawals<-VWUDS_2010_2017%>%
    dplyr::group_by(Date,HUC6)%>%
    dplyr::summarise(Withdrawal=sum(Withdrawals_MGD,na.rm=T))%>%
    tidyr::spread(HUC6, Withdrawal)
  
  Missing<-setdiff(HUC6,names(TS_HUC6_Withdrawals))
  TS_HUC6_Withdrawals[Missing]<-NA
  TS_HUC6_Withdrawals<-TS_HUC6_Withdrawals[HUC6]
  
  # Timeseries Consumption in HUC8 for Mann Kendall Analysis
  CU_Function<-function(x,y) (ifelse(is.na(x),0,x)-ifelse(is.na(y),0,y))/(ifelse(is.na(x),0,x))
  TS_HUC6_Withdrawals<-TS_HUC6_Withdrawals[,order(names(TS_HUC6_Withdrawals))]
  TS_HUC6_Discharges<-TS_HUC6_Discharges[,order(names(TS_HUC6_Discharges))]
  
  TS_CU_HUC6<-data.frame(Date=unique(VWUDS_2010_2017$Date),mapply(CU_Function,TS_HUC6_Withdrawals,TS_HUC6_Discharges),stringsAsFactors = F)
  
  colnames(TS_CU_HUC6)<-gsub("X","",colnames(TS_CU_HUC6))
  
  assign(label,TS_CU_HUC6,envir = .GlobalEnv)
  
  save(TS_CU_HUC6,file=paste0("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/",label,".RData"))
  
}

#---All Sectors---#
#-All Facilities-#
MK_HUC6_Compile(ECHO.test,VWUDS.test,"TS_CU_All")
#-Fully Matched Facilities-#
MK_HUC6_Compile(ECHO.test_matched,VWUDS.test_matched,"TS_CU_Matched")

#---Subsetted by Sector---#

#-All Facilities-#
MK_HUC6_Compile(ECHO.test_energy,VWUDS.test_energy,"TS_CU_All_Energy")
MK_HUC6_Compile(ECHO.test_ag,VWUDS.test_ag,"TS_CU_All_Ag")
MK_HUC6_Compile(ECHO.test_commercial,VWUDS.test_commercial,"TS_CU_All_Commercial")
MK_HUC6_Compile(ECHO.test_industrial,VWUDS.test_industrial,"TS_CU_All_Industrial")
MK_HUC6_Compile(ECHO.test_municipal,VWUDS.test_municipal,"TS_CU_All_Municipal")

#-Fully Matched Facilities-#
MK_HUC6_Compile(ECHO.test_match_energy,VWUDS.test_match_energy,"TS_CU_Match_Energy")
MK_HUC6_Compile(ECHO.test_match_ag,VWUDS.test_match_ag,"TS_CU_Match_Ag")
MK_HUC6_Compile(ECHO.test_match_commercial,VWUDS.test_match_commercial,"TS_CU_Match_Commercial")
MK_HUC6_Compile(ECHO.test_match_industrial,VWUDS.test_match_industrial,"TS_CU_Match_Industrial")
MK_HUC6_Compile(ECHO.test_match_municipal,VWUDS.test_match_municipal,"TS_CU_Match_Municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
MK_HUC6_Compile(ECHO.test_nonenergy,VWUDS.test_match_nonenergy,"TS_CU_All_NonEnergy")

#-Fully Matched Facilities-#
MK_HUC6_Compile(ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"TS_CU_Match_NonEnergy")
