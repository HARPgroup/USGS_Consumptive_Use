###########################################################################################################################################
#-------------------------------------------Estimating Consumptive Use in Virginian HUC 8 Watersheds--------------------------------------#

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
library(ggpubr)


options(scipen=999) #Disable scientific notation
options(digits = 9)
memory.limit(size=100000000)

###########################################################################################################################################
#-------------------------------------------HUC 8 and Virginia Shapefile Manipulation-----------------------------------------------------#

#Load databases and extract required layers
HUC8<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/HUC.gdb",layer='WBDHU8')
VA<-readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC8<-sp::spTransform(HUC8, CRS("+init=epsg:4269"))
VA<-sp::spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)

#Create HUC8 Dataframe that will be used in future overlay processes
HUC8_Overlay<-HUC8 #Keep integrity of spatial dataframe
HUC8_Overlay@data<-HUC8_Overlay@data[,c(11,12)] 
names(HUC8_Overlay@data)<-c("HUC8","HUC8Name")


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
  
  ####Create Spatial Data Frame from Points and Overlay on Clipped HUC8 Shapefile####
  ####FROM Delivery Transfers####
  #Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
  #Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
  dFrom<-deliveries[!(is.na(deliveries$geomFlat)&is.na(deliveries$geomFlon)),]
  dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
  HUC8_Facilities<-over(dFrom,HUC8_Overlay)#Spatial overlay
  dFrom@data$HUC8Name<-HUC8_Facilities$HUC8Name
  dFrom@data$HUC8<-HUC8_Facilities$HUC8
  
  ####TO Delivery Transfers####
  #Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
  #Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
  dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
  dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
  HUC8_Facilities<-over(dTo,HUC8_Overlay)#Spatial overlay
  dTo@data$HUC8Name<-HUC8_Facilities$HUC8Name
  dTo@data$HUC8<-HUC8_Facilities$HUC8
  
  ####Determine if TO Transfers are leaving watershed boundaries####
  #Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
  #We are only concerned with interbasin transfers and need to identify these with the following code
  dTo@data$interbasin<-NA
  dFrom@data$interbasin<-NA
  #Check each transfer in the delivery VA Hydro transfers to see if its FROM HUC8 is different than its TO HUC8
  
  for (i in 1:length(dTo@data$hydroid)){
    ToHUC<-as.character(dTo@data$HUC8[i])
    FromHUC<-as.character(dFrom@data$HUC8[i])
    if(is.na(ToHUC)){
      ToHUC<-'Null HUC8'
    }
    if(is.na(FromHUC)){
      FromHUC<-'Null HUC8' 
    }
    interbasin<-0
    if(ToHUC!=FromHUC){ #if the HUC8 does not match, mark as interbasin delivery
      interbasin<-1
    }
    dTo@data$interbasin[i]<-interbasin
    dFrom@data$interbasin[i]<-interbasin
  }
  
  ####Sum Net Water In and Out for each HUC8####
  ###FROM Deliveries###
  delf<-dFrom@data
  delf<-delf[delf$interbasin==1,] #if interbasin is equal to 1, it is crossing boundaries---so just include those
  
  delf<-delf%>%
    dplyr::group_by(HUC8Name, Year)%>%
    dplyr::summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD
  
  delf$HUC8Name<-as.character(delf$HUC8Name)
  delf$HUC8Name[is.na(delf$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
  ###TO Deliveries###
  delt<-dTo@data
  delt<-delt[delt$interbasin==1,] #narrow down to deliveries happening across borders
  
  delt<-
    delt%>%
    dplyr::group_by(HUC8Name, Year)%>%
    dplyr::summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD
  
  
  delt$HUC8Name<-as.character(delt$HUC8Name)
  delt$HUC8Name[is.na(delt$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
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
  
  ####Determine if Release FROM Transfers are leaving HUC8 boundaries####
  rTo@data$interbasin<-NA
  rFrom@data$interbasin<-NA
  
  for (i in 1:length(rTo@data$hydroid)){
    ToHUC<-as.character(rTo@data$HUC8[i])
    FromHUC<-as.character(rFrom@data$HUC8[i])
    if(is.na(ToHUC)){ #if the HUC code is NA
      ToHUC<-'Null HUC8'
    }
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
  
  ####Sum Net Water In and Out for each HUC8####
  ###FROM Releases###
  relf<-rFrom@data
  relf<-relf[relf$interbasin==1,]#remember intratransfers are indicated with a 0
  
  relf<-relf%>% #Summarise by HUC8 and year
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8), interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)  #MGM to MGD
  
  relf$HUC8Name<-as.character(relf$HUC8Name)
  relf$HUC8Name[is.na(relf$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
  ###TO Releases###
  relt<-rTo@data
  relt<-relt[relt$interbasin==1,]
  
  relt<-relt%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8), interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)
  
  relt$HUC8Name<-as.character(relt$HUC8Name)
  relt$HUC8Name[is.na(relt$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
  assign("relf",relf,envir=.GlobalEnv)
  assign("relt",relt,envir=.GlobalEnv)
  
}
releases_func(rel)

#-----------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------Calculate Net Transfers for Each HUC8 Watershed-------------------------------------------------#

# Loop through each HUC8 and check for summed releases and deliveries
# Water out is defined as the "from's" and Water in are the "to's"
# This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

transfers<- function(relf,delf,relt,delt){
  
  HUC8_waterout<-as.data.table(rbind(relf,delf))
  HUC8_waterout<-HUC8_waterout[, lapply(.SD,sum), by=list(HUC8Name, Year, HUC8), .SDcols=c(4,5)]
  
  HUC8_waterin<-as.data.table(rbind(relt,delt))
  HUC8_waterin<-HUC8_waterin[, lapply(.SD,sum), by=list(HUC8Name, Year, HUC8), .SDcols=c(4,5)]
  
  #---Year 2010---#
  HUC8@data$waterout_2010<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2010"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2010"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2010"])],NA)
  HUC8@data$waterin_2010<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2010"],HUC8_waterin$waterin[HUC8_waterin$Year=="2010"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2010"])],NA)
  HUC8@data$transferred_2010<- (rowSums(HUC8@data[,(15:16)],na.rm=T))
  HUC8@data$transferred_2010<-ifelse(is.na(HUC8@data$waterin_2010)&is.na(HUC8@data$waterout_2010),NA,HUC8@data$transferred_2010)
  
  #---Year 2011---#
  HUC8@data$waterout_2011<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2011"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2011"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2011"])],NA)
  HUC8@data$waterin_2011<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2011"],HUC8_waterin$waterin[HUC8_waterin$Year=="2011"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2011"])],NA)
  HUC8@data$transferred_2011<- (rowSums(HUC8@data[,(18:19)],na.rm=T))
  HUC8@data$transferred_2011<-ifelse(is.na(HUC8@data$waterin_2011)&is.na(HUC8@data$waterout_2011),NA,HUC8@data$transferred_2011)
  
  #---Year 2012---#
  HUC8@data$waterout_2012<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2012"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2012"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2012"])],NA)
  HUC8@data$waterin_2012<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2012"],HUC8_waterin$waterin[HUC8_waterin$Year=="2012"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2012"])],NA)
  HUC8@data$transferred_2012<- (rowSums(HUC8@data[,(21:22)],na.rm=T))
  HUC8@data$transferred_2012<-ifelse(is.na(HUC8@data$waterin_2012)&is.na(HUC8@data$waterout_2012),NA,HUC8@data$transferred_2012)
  
  #---Year 2013---#
  HUC8@data$waterout_2013<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2013"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2013"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2013"])],NA)
  HUC8@data$waterin_2013<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2013"],HUC8_waterin$waterin[HUC8_waterin$Year=="2013"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2013"])],NA)
  HUC8@data$transferred_2013<- (rowSums(HUC8@data[,(24:25)],na.rm=T))
  HUC8@data$transferred_2013<-ifelse(is.na(HUC8@data$waterin_2013)&is.na(HUC8@data$waterout_2013),NA,HUC8@data$transferred_2013)
  
  #---Year 2014---#
  HUC8@data$waterout_2014<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2014"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2014"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2014"])],NA)
  HUC8@data$waterin_2014<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2014"],HUC8_waterin$waterin[HUC8_waterin$Year=="2014"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2014"])],NA)
  HUC8@data$transferred_2014<- (rowSums(HUC8@data[,(27:28)],na.rm=T))
  HUC8@data$transferred_2014<-ifelse(is.na(HUC8@data$waterin_2014)&is.na(HUC8@data$waterout_2014),NA,HUC8@data$transferred_2014)
  
  #---Year 2015---#
  HUC8@data$waterout_2015<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2015"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2015"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2015"])],NA)
  HUC8@data$waterin_2015<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2015"],HUC8_waterin$waterin[HUC8_waterin$Year=="2015"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2015"])],NA)
  HUC8@data$transferred_2015<- (rowSums(HUC8@data[,(30:31)],na.rm=T))
  HUC8@data$transferred_2015<-ifelse(is.na(HUC8@data$waterin_2015)&is.na(HUC8@data$waterout_2015),NA,HUC8@data$transferred_2015)
  
  #---Year 2016---#
  HUC8@data$waterout_2016<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2016"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2016"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2016"])],NA)
  HUC8@data$waterin_2016<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2016"],HUC8_waterin$waterin[HUC8_waterin$Year=="2016"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2016"])],NA)
  HUC8@data$transferred_2016<- (rowSums(HUC8@data[,(33:34)],na.rm=T))
  HUC8@data$transferred_2016<-ifelse(is.na(HUC8@data$waterin_2016)&is.na(HUC8@data$waterout_2016),NA,HUC8@data$transferred_2016)
  
  #---Year 2017---#
  HUC8@data$waterout_2017<-ifelse(HUC8@data$HUC8%in%HUC8_waterout$HUC8[HUC8_waterout$Year=="2017"],-HUC8_waterout$waterout[HUC8_waterout$Year=="2017"][match(HUC8@data$HUC8,HUC8_waterout$HUC8[HUC8_waterout$Year=="2017"])],NA)
  HUC8@data$waterin_2017<-ifelse(HUC8@data$HUC8%in%HUC8_waterin$HUC8[HUC8_waterin$Year=="2017"],HUC8_waterin$waterin[HUC8_waterin$Year=="2017"][match(HUC8@data$HUC8,HUC8_waterin$HUC8[HUC8_waterin$Year=="2017"])],NA)
  HUC8@data$transferred_2017<- (rowSums(HUC8@data[,(36:37)],na.rm=T))
  HUC8@data$transferred_2017<-ifelse(is.na(HUC8@data$waterin_2017)&is.na(HUC8@data$waterout_2017),NA,HUC8@data$transferred_2017)
  
  
  HUC8_Transfers<-data.frame(HUC8Name=HUC8@data$Name,
                             HUC8=HUC8@data$HUC8,
                             Transfers_2010_MGD=HUC8@data$transferred_2010,
                             Transfers_2011_MGD=HUC8@data$transferred_2011,
                             Transfers_2012_MGD=HUC8@data$transferred_2012,
                             Transfers_2013_MGD=HUC8@data$transferred_2013,
                             Transfers_2014_MGD=HUC8@data$transferred_2014,
                             Transfers_2015_MGD=HUC8@data$transferred_2015,
                             Transfers_2016_MGD=HUC8@data$transferred_2016,
                             Transfers_2017_MGD=HUC8@data$transferred_2017)
  
  assign("HUC8",HUC8,envir = .GlobalEnv)
  assign("HUC8_Transfers",HUC8_Transfers,envir = .GlobalEnv)
  
  rm(relf,delf,relt,delt,deliveries,envir = .GlobalEnv)
  
}
transfers(relf,delf,relt,delt)

###########################################################################################################################################
#----------------------------------------------------Calculating Discharges---------------------------------------------------------------#

#---Load in Discharge Data previously cleaned---#

ECHO_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC_statewide.txt", sep="\t", header=T)

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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  
  HUC8_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Discharges",label),HUC8_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
all_discharge(ECHO_2010_2017,"") # All Facilities

all_discharge<- function(discharge_db,label){
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Facility_Discharges<-discharge_db@data%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  
  HUC8_Discharges<-Facility_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Discharges",label),HUC8_Discharges,envir = .GlobalEnv)
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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  
  HUC8_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Discharges",label),HUC8_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
sector_discharge(ECHO_2010_2017,"Energy","_energy")
sector_discharge(ECHO_2010_2017,"Aquaculture","_aq")
sector_discharge(ECHO_2010_2017,"Commercial","_commercial")
sector_discharge(ECHO_2010_2017,"Industrial","_industrial")
sector_discharge(ECHO_2010_2017,"Municipal","_municipal")

sector_discharge<- function(discharge_db,sector,label){
  
  discharge_db<-subset(discharge_db,subset=discharge_db$Use.Type==sector)
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Facility_Discharges<-discharge_db@data%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  
  HUC8_Discharges<-Facility_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
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
  
  assign(paste0("HUC8_Discharges",label),HUC8_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
sector_discharge(full_match_2010_2017,"Energy","_match_energy")
sector_discharge(full_match_2010_2017,"Aquaculture","_match_aq")
sector_discharge(full_match_2010_2017,"Commercial","_match_commercial")
sector_discharge(full_match_2010_2017,"Industrial","_match_industrial")

#--Option to look at Non-Energy Sectors--#
nonenergy_discharge<- function(discharge_db,label){
  
  discharge_db<-subset(discharge_db,subset=!discharge_db$Use.Type=="Energy")
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  
  HUC8_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Discharges",label),HUC8_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
nonenergy_discharge(ECHO_2010_2017,"_nonenergy")

nonenergy_discharge<- function(discharge_db,label){
  
  discharge_db<-subset(discharge_db,subset=!discharge_db$Use.Type=="Energy")
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Facility_Discharges<-discharge_db@data%>%
    dplyr::group_by(VPDES.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  
  HUC8_Discharges<-Facility_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Discharges",label),HUC8_Discharges,envir = .GlobalEnv)
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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
all_withdrawal(VWUDS_2010_2017,"") # All Facilities

all_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$Fac.Long,
                                                       Facility_Latitude=withdrawal_db$Fac.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
} #Want to use one facility location for matched facilities
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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
                     Ave_Mon_Reported=median(Ave_Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  withdrawal_db.test<-withdrawal_db.test%>%
    dplyr::group_by(VWUDS.Facility.ID)%>%
    dplyr::summarise(Facility_Name=first(Facility_Name),
                     VWUDS.Lat=first(VWUDS.Lat),
                     VWUDS.Long=first(VWUDS.Long),
                     Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T),
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
sector_withdrawal(VWUDS_2010_2017,"Energy","_energy")
sector_withdrawal(VWUDS_2010_2017,"Agriculture/Irrigation","_ag")
sector_withdrawal(VWUDS_2010_2017,"Aquaculture","_aq")
sector_withdrawal(VWUDS_2010_2017,"Commercial","_commercial")
sector_withdrawal(VWUDS_2010_2017,"Industrial","_industrial")
sector_withdrawal(VWUDS_2010_2017,"Municipal","_municipal")

# Separate function that uses coordinates of VPDES counterpart
sector_withdrawal<- function(withdrawal_db,sector,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=withdrawal_db$Use.Type==sector)
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$Fac.Long,
                                                       Facility_Latitude=withdrawal_db$Fac.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
sector_withdrawal(full_match_2010_2017,"Energy","_match_energy")
sector_withdrawal(full_match_2010_2017,"Aquaculture","_match_aq")
sector_withdrawal(full_match_2010_2017,"Commercial","_match_commercial")
sector_withdrawal(full_match_2010_2017,"Industrial","_match_industrial")

#--Option to look at Non-Energy Sectors--#
nonenergy_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=!withdrawal_db$Use.Type=="Energy")
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(VWUDS_2010_2017,"_nonenergy")

nonenergy_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=!withdrawal_db$Use.Type=="Energy")
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$Fac.Long,
                                                       Facility_Latitude=withdrawal_db$Fac.Lat),
                                            withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
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
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(full_match_2010_2017,"_match_nonenergy")

###########################################################################################################################################
#------------------------------------------Apply Withdrawals and Discharges into HUC8 Spatial Dataframe-----------------------------------#

HUC8_discharge_withdrawal<- function(HUC8_Discharges_db,HUC8_Withdrawals_db,label){
  #---Year 2010----#
  HUC8@data$Discharges_2010<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2010"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2010"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2010"])],NA)
  HUC8@data$Withdrawals_2010<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2010"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2010"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2010"])],NA)
  
  #---Year 2011----#
  HUC8@data$Discharges_2011<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2011"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2011"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2011"])],NA)
  HUC8@data$Withdrawals_2011<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2011"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2011"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2011"])],NA)
  
  #---Year 2012----#
  HUC8@data$Discharges_2012<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2012"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2012"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2012"])],NA)
  HUC8@data$Withdrawals_2012<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2012"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2012"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2012"])],NA)
  
  #---Year 2013----#
  HUC8@data$Discharges_2013<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2013"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2013"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2013"])],NA)
  HUC8@data$Withdrawals_2013<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2013"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2013"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2013"])],NA)
  
  #---Year 2014----#
  HUC8@data$Discharges_2014<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2014"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2014"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2014"])],NA)
  HUC8@data$Withdrawals_2014<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2014"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2014"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2014"])],NA)
  
  #---Year 2015----#
  HUC8@data$Discharges_2015<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2015"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2015"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2015"])],NA)
  HUC8@data$Withdrawals_2015<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2015"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2015"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2015"])],NA)
  
  #---Year 2016----#
  HUC8@data$Discharges_2016<-ifelse(HUC8@data$HUC8%in%HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2016"],HUC8_Discharges_db$Discharge_MGD[HUC8_Discharges_db$Year=="2016"][match(HUC8@data$HUC8,HUC8_Discharges_db$HUC8[HUC8_Discharges_db$Year=="2016"])],NA)
  HUC8@data$Withdrawals_2016<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2016"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2016"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2016"])],NA)
  
  assign(paste0("HUC8",label),HUC8,envir = .GlobalEnv)
}

#----------------------------------------------------------#
#-------------------All Available Data---------------------#

#--All Sectors--#
HUC8_discharge_withdrawal(HUC8_Discharges,HUC8_Withdrawals,"_all")

#--Non-Energy--#
HUC8_discharge_withdrawal(HUC8_Discharges_nonenergy,HUC8_Withdrawals_nonenergy,"_nonenergy")
HUC8_discharge_withdrawal(HUC8_Discharges_energy,HUC8_Withdrawals_energy,"_energy")
HUC8_discharge_withdrawal(HUC8_Discharges_aq,HUC8_Withdrawals_aq,"_aq")
HUC8_discharge_withdrawal(HUC8_Discharges_commercial,HUC8_Withdrawals_commercial,"_commercial")
HUC8_discharge_withdrawal(HUC8_Discharges_industrial,HUC8_Withdrawals_industrial,"_industrial")
HUC8_discharge_withdrawal(HUC8_Discharges_municipal,HUC8_Withdrawals_municipal,"_municipal")

#--Agriculture--#
ag.HUC8_discharge_withdrawal<- function(HUC8_Withdrawals_db,label){
  #---Year 2010----#
  HUC8@data$Withdrawals_2010<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2010"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2010"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2010"])],NA)
  
  #---Year 2011----#
  HUC8@data$Withdrawals_2011<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2011"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2011"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2011"])],NA)
  
  #---Year 2012----#
  HUC8@data$Withdrawals_2012<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2012"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2012"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2012"])],NA)
  
  #---Year 2013----#
  HUC8@data$Withdrawals_2013<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2013"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2013"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2013"])],NA)
  
  #---Year 2014----#
  HUC8@data$Withdrawals_2014<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2014"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2014"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2014"])],NA)
  
  #---Year 2015----#
  HUC8@data$Withdrawals_2015<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2015"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2015"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2015"])],NA)
  
  #---Year 2016----#
  HUC8@data$Withdrawals_2016<-ifelse(HUC8@data$HUC8%in%HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2016"],HUC8_Withdrawals_db$Withdrawals_MGD[HUC8_Withdrawals_db$Year=="2016"][match(HUC8@data$HUC8,HUC8_Withdrawals_db$HUC8[HUC8_Withdrawals_db$Year=="2016"])],NA)
  
  assign(paste0("HUC8",label),HUC8,envir = .GlobalEnv)
  
}
ag.HUC8_discharge_withdrawal(HUC8_Withdrawals_ag,"_ag")

#----------------------------------------------------------#
#-------------------Matched Facility Data------------------#

#--All Sectors--#
HUC8_discharge_withdrawal(HUC8_Discharges_matched,HUC8_Withdrawals_matched,"_matched")

#--Non-Energy--#
HUC8_discharge_withdrawal(HUC8_Discharges_match_nonenergy,HUC8_Withdrawals_match_nonenergy,"_match_nonenergy")
HUC8_discharge_withdrawal(HUC8_Discharges_match_energy,HUC8_Withdrawals_match_energy,"_match_energy")
HUC8_discharge_withdrawal(HUC8_Discharges_match_aq,HUC8_Withdrawals_match_aq,"_match_aq")
HUC8_discharge_withdrawal(HUC8_Discharges_match_commercial,HUC8_Withdrawals_match_commercial,"_match_commercial")
HUC8_discharge_withdrawal(HUC8_Discharges_match_industrial,HUC8_Withdrawals_match_industrial,"_match_industrial")

rm(HUC8_Discharges,HUC8_Discharges_commercial,HUC8_Discharges_energy,HUC8_Discharges_industrial,HUC8_Discharges_match_ag,
   HUC8_Discharges_match_commercial,HUC8_Discharges_match_energy,HUC8_Discharges_match_industrial,HUC8_Discharges_match_municipal,HUC8_Discharges_match_nonenergy,
   HUC8_Discharges_matched,HUC8_Withdrawals,HUC8_Withdrawals_ag,HUC8_Withdrawals_commercial,HUC8_Withdrawals_energy,HUC8_Withdrawals_industrial,
   HUC8_Withdrawals_match_ag,HUC8_Withdrawals_match_commercial,HUC8_Withdrawals_match_energy,HUC8_Withdrawals_match_industrial,HUC8_Withdrawals_match_municipal,
   HUC8_Withdrawals_match_nonenergy,HUC8_Discharges_municipal,HUC8_Discharges_nonenergy,HUC8_Withdrawals_matched,HUC8_Withdrawals_municipal,HUC8_Withdrawals_nonenergy)

###########################################################################################################################################
#-----------------------------------------------Net Water Balance and Consumptive Use-----------------------------------------------------#

NWB_CU<- function(HUC8_db,label){
  #---Year 2010----#
  HUC8_db@data$NetWB_2010<-(ifelse(is.na(HUC8_db@data$Discharges_2010),0,HUC8_db@data$Discharges_2010))-(ifelse(is.na(HUC8_db@data$Withdrawals_2010),0,HUC8_db@data$Withdrawals_2010))
  HUC8_db@data$NetWB_2010<-ifelse(is.na(HUC8_db@data$Discharges_2010)&is.na(HUC8_db@data$Withdrawals_2010),NA,HUC8_db@data$NetWB_2010)
  
  HUC8_db@data$Consumption_2010<-((ifelse(is.na(HUC8_db@data$Withdrawals_2010),0,HUC8_db@data$Withdrawals_2010))-(ifelse(is.na(HUC8_db@data$Discharges_2010),0,HUC8_db@data$Discharges_2010)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2010),0,HUC8_db@data$Withdrawals_2010))
  HUC8_db@data$Consumption_2010<-ifelse(is.nan(HUC8_db@data$Consumption_2010)|is.infinite(HUC8_db@data$Consumption_2010),NA,HUC8_db@data$Consumption_2010)
  
  #---Year 2011----#
  
  HUC8_db@data$NetWB_2011<-(ifelse(is.na(HUC8_db@data$Discharges_2011),0,HUC8_db@data$Discharges_2011))-(ifelse(is.na(HUC8_db@data$Withdrawals_2011),0,HUC8_db@data$Withdrawals_2011))
  HUC8_db@data$NetWB_2011<-ifelse(is.na(HUC8_db@data$Discharges_2011)&is.na(HUC8_db@data$Withdrawals_2011),NA,HUC8_db@data$NetWB_2011)
  
  HUC8_db@data$Consumption_2011<-((ifelse(is.na(HUC8_db@data$Withdrawals_2011),0,HUC8_db@data$Withdrawals_2011))-(ifelse(is.na(HUC8_db@data$Discharges_2011),0,HUC8_db@data$Discharges_2011)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2011),0,HUC8_db@data$Withdrawals_2011))
  HUC8_db@data$Consumption_2011<-ifelse(is.nan(HUC8_db@data$Consumption_2011)|is.infinite(HUC8_db@data$Consumption_2011),NA,HUC8_db@data$Consumption_2011)
  
  #---Year 2012----#
  HUC8_db@data$NetWB_2012<-(ifelse(is.na(HUC8_db@data$Discharges_2012),0,HUC8_db@data$Discharges_2012))-(ifelse(is.na(HUC8_db@data$Withdrawals_2012),0,HUC8_db@data$Withdrawals_2012))
  HUC8_db@data$NetWB_2012<-ifelse(is.na(HUC8_db@data$Discharges_2012)&is.na(HUC8_db@data$Withdrawals_2012),NA,HUC8_db@data$NetWB_2012)
  
  HUC8_db@data$Consumption_2012<-((ifelse(is.na(HUC8_db@data$Withdrawals_2012),0,HUC8_db@data$Withdrawals_2012))-(ifelse(is.na(HUC8_db@data$Discharges_2012),0,HUC8_db@data$Discharges_2012)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2012),0,HUC8_db@data$Withdrawals_2012))
  HUC8_db@data$Consumption_2012<-ifelse(is.nan(HUC8_db@data$Consumption_2012)|is.infinite(HUC8_db@data$Consumption_2012),NA,HUC8_db@data$Consumption_2012)
  
  #---Year 2013----#
  HUC8_db@data$NetWB_2013<-(ifelse(is.na(HUC8_db@data$Discharges_2013),0,HUC8_db@data$Discharges_2013))-(ifelse(is.na(HUC8_db@data$Withdrawals_2013),0,HUC8_db@data$Withdrawals_2013))
  HUC8_db@data$NetWB_2013<-ifelse(is.na(HUC8_db@data$Discharges_2013)&is.na(HUC8_db@data$Withdrawals_2013),NA,HUC8_db@data$NetWB_2013)
  
  HUC8_db@data$Consumption_2013<-((ifelse(is.na(HUC8_db@data$Withdrawals_2013),0,HUC8_db@data$Withdrawals_2013))-(ifelse(is.na(HUC8_db@data$Discharges_2013),0,HUC8_db@data$Discharges_2013)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2013),0,HUC8_db@data$Withdrawals_2013))
  HUC8_db@data$Consumption_2013<-ifelse(is.nan(HUC8_db@data$Consumption_2013)|is.infinite(HUC8_db@data$Consumption_2013),NA,HUC8_db@data$Consumption_2013)
  
  #---Year 2014----#
  HUC8_db@data$NetWB_2014<-(ifelse(is.na(HUC8_db@data$Discharges_2014),0,HUC8_db@data$Discharges_2014))-(ifelse(is.na(HUC8_db@data$Withdrawals_2014),0,HUC8_db@data$Withdrawals_2014))
  HUC8_db@data$NetWB_2014<-ifelse(is.na(HUC8_db@data$Discharges_2014)&is.na(HUC8_db@data$Withdrawals_2014),NA,HUC8_db@data$NetWB_2014)
  
  HUC8_db@data$Consumption_2014<-((ifelse(is.na(HUC8_db@data$Withdrawals_2014),0,HUC8_db@data$Withdrawals_2014))-(ifelse(is.na(HUC8_db@data$Discharges_2014),0,HUC8_db@data$Discharges_2014)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2014),0,HUC8_db@data$Withdrawals_2014))
  HUC8_db@data$Consumption_2014<-ifelse(is.nan(HUC8_db@data$Consumption_2014)|is.infinite(HUC8_db@data$Consumption_2014),NA,HUC8_db@data$Consumption_2014)
  
  #---Year 2015----#
  HUC8_db@data$NetWB_2015<-(ifelse(is.na(HUC8_db@data$Discharges_2015),0,HUC8_db@data$Discharges_2015))-(ifelse(is.na(HUC8_db@data$Withdrawals_2015),0,HUC8_db@data$Withdrawals_2015))
  HUC8_db@data$NetWB_2015<-ifelse(is.na(HUC8_db@data$Discharges_2015)&is.na(HUC8_db@data$Withdrawals_2015),NA,HUC8_db@data$NetWB_2015)
  
  HUC8_db@data$Consumption_2015<-((ifelse(is.na(HUC8_db@data$Withdrawals_2015),0,HUC8_db@data$Withdrawals_2015))-(ifelse(is.na(HUC8_db@data$Discharges_2015),0,HUC8_db@data$Discharges_2015)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2015),0,HUC8_db@data$Withdrawals_2015))
  HUC8_db@data$Consumption_2015<-ifelse(is.nan(HUC8_db@data$Consumption_2015)|is.infinite(HUC8_db@data$Consumption_2015),NA,HUC8_db@data$Consumption_2015)
  
  #---Year 2016----#
  HUC8_db@data$NetWB_2016<-(ifelse(is.na(HUC8_db@data$Discharges_2016),0,HUC8_db@data$Discharges_2016))-(ifelse(is.na(HUC8_db@data$Withdrawals_2016),0,HUC8_db@data$Withdrawals_2016))
  HUC8_db@data$NetWB_2016<-ifelse(is.na(HUC8_db@data$Discharges_2016)&is.na(HUC8_db@data$Withdrawals_2016),NA,HUC8_db@data$NetWB_2016)
  
  HUC8_db@data$Consumption_2016<-((ifelse(is.na(HUC8_db@data$Withdrawals_2016),0,HUC8_db@data$Withdrawals_2016))-(ifelse(is.na(HUC8_db@data$Discharges_2016),0,HUC8_db@data$Discharges_2016)))/(ifelse(is.na(HUC8_db@data$Withdrawals_2016),0,HUC8_db@data$Withdrawals_2016))
  HUC8_db@data$Consumption_2016<-ifelse(is.nan(HUC8_db@data$Consumption_2016)|is.infinite(HUC8_db@data$Consumption_2016),NA,HUC8_db@data$Consumption_2016)
  
  
  HUC8_glimpse<-HUC8_db@data
  assign("HUC8_glimpse",HUC8_glimpse,envir = .GlobalEnv)
  assign(paste0("HUC8",label),HUC8_db,envir = .GlobalEnv)
  
}

#---All Sectors---#

#-All Facilities-#
NWB_CU(HUC8_all,"_all")
#-Fully Matched Facilities-#
NWB_CU(HUC8_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
NWB_CU(HUC8_energy,"_energy")
NWB_CU(HUC8_aq,"_aq")
NWB_CU(HUC8_commercial,"_commercial")
NWB_CU(HUC8_industrial,"_industrial")
NWB_CU(HUC8_municipal,"_municipal")

NWB_CU.ag<- function(HUC8_db,label){
  #---Year 2010----#
  
  HUC8_db@data$NetWB_2010<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2010),0,HUC8_db@data$Withdrawals_2010))
  HUC8_db@data$NetWB_2010<-ifelse(is.na(HUC8_db@data$Withdrawals_2010),NA,HUC8_db@data$NetWB_2010)
  
  HUC8_db@data$Consumption_2010<-(ifelse(is.na(HUC8_db@data$Withdrawals_2010),0,HUC8_db@data$Withdrawals_2010))/(ifelse(is.na(HUC8_db@data$Withdrawals_2010),0,HUC8_db@data$Withdrawals_2010))
  HUC8_db@data$Consumption_2010<-ifelse(is.nan(HUC8_db@data$Consumption_2010)|is.infinite(HUC8_db@data$Consumption_2010),NA,HUC8_db@data$Consumption_2010)
  #---Year 2011----#
  
  HUC8_db@data$NetWB_2011<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2011),0,HUC8_db@data$Withdrawals_2011))
  
  HUC8_db@data$NetWB_2011<-ifelse(is.na(HUC8_db@data$Withdrawals_2011),NA,HUC8_db@data$NetWB_2011)
  
  HUC8_db@data$Consumption_2011<-(ifelse(is.na(HUC8_db@data$Withdrawals_2011),0,HUC8_db@data$Withdrawals_2011))/(ifelse(is.na(HUC8_db@data$Withdrawals_2011),0,HUC8_db@data$Withdrawals_2011))
  
  HUC8_db@data$Consumption_2011<-ifelse(is.nan(HUC8_db@data$Consumption_2011)|is.infinite(HUC8_db@data$Consumption_2011),NA,HUC8_db@data$Consumption_2011)
  
  #---Year 2012----#
  
  HUC8_db@data$NetWB_2012<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2012),0,HUC8_db@data$Withdrawals_2012))
  
  HUC8_db@data$NetWB_2012<-ifelse(is.na(HUC8_db@data$Withdrawals_2012),NA,HUC8_db@data$NetWB_2012)
  
  HUC8_db@data$Consumption_2012<-(ifelse(is.na(HUC8_db@data$Withdrawals_2012),0,HUC8_db@data$Withdrawals_2012))/(ifelse(is.na(HUC8_db@data$Withdrawals_2012),0,HUC8_db@data$Withdrawals_2012))
  
  HUC8_db@data$Consumption_2012<-ifelse(is.nan(HUC8_db@data$Consumption_2012)|is.infinite(HUC8_db@data$Consumption_2012),NA,HUC8_db@data$Consumption_2012)
  #---Year 2013----#
  HUC8_db@data$NetWB_2013<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2013),0,HUC8_db@data$Withdrawals_2013))
  
  HUC8_db@data$NetWB_2013<-ifelse(is.na(HUC8_db@data$Withdrawals_2013),NA,HUC8_db@data$NetWB_2013)
  
  HUC8_db@data$Consumption_2013<-(ifelse(is.na(HUC8_db@data$Withdrawals_2013),0,HUC8_db@data$Withdrawals_2013))/(ifelse(is.na(HUC8_db@data$Withdrawals_2013),0,HUC8_db@data$Withdrawals_2013))
  HUC8_db@data$Consumption_2013<-ifelse(is.nan(HUC8_db@data$Consumption_2013)|is.infinite(HUC8_db@data$Consumption_2013),NA,HUC8_db@data$Consumption_2013)
  
  #---Year 2014----#
  
  #---Without Transfers---#
  HUC8_db@data$NetWB_2014<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2014),0,HUC8_db@data$Withdrawals_2014))
  
  HUC8_db@data$NetWB_2014<-ifelse(is.na(HUC8_db@data$Withdrawals_2014),NA,HUC8_db@data$NetWB_2014)
  
  HUC8_db@data$Consumption_2014<-(ifelse(is.na(HUC8_db@data$Withdrawals_2014),0,HUC8_db@data$Withdrawals_2014))/(ifelse(is.na(HUC8_db@data$Withdrawals_2014),0,HUC8_db@data$Withdrawals_2014))
  
  HUC8_db@data$Consumption_2014<-ifelse(is.nan(HUC8_db@data$Consumption_2014)|is.infinite(HUC8_db@data$Consumption_2014),NA,HUC8_db@data$Consumption_2014)
  #---Year 2015----#
  
  #---Without Transfers---#
  HUC8_db@data$NetWB_2015<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2015),0,HUC8_db@data$Withdrawals_2015))
  
  HUC8_db@data$NetWB_2015<-ifelse(is.na(HUC8_db@data$Withdrawals_2015),NA,HUC8_db@data$NetWB_2015)
  
  HUC8_db@data$Consumption_2015<-(ifelse(is.na(HUC8_db@data$Withdrawals_2015),0,HUC8_db@data$Withdrawals_2015))/(ifelse(is.na(HUC8_db@data$Withdrawals_2015),0,HUC8_db@data$Withdrawals_2015))
  
  HUC8_db@data$Consumption_2015<-ifelse(is.nan(HUC8_db@data$Consumption_2015)|is.infinite(HUC8_db@data$Consumption_2015),NA,HUC8_db@data$Consumption_2015)
  
  #---Year 2016----#
  
  #---Without Transfers---#
  HUC8_db@data$NetWB_2016<- -(ifelse(is.na(HUC8_db@data$Withdrawals_2016),0,HUC8_db@data$Withdrawals_2016))
  
  HUC8_db@data$NetWB_2016<-ifelse(is.na(HUC8_db@data$Withdrawals_2016),NA,HUC8_db@data$NetWB_2016)
  
  HUC8_db@data$Consumption_2016<-(ifelse(is.na(HUC8_db@data$Withdrawals_2016),0,HUC8_db@data$Withdrawals_2016))/(ifelse(is.na(HUC8_db@data$Withdrawals_2016),0,HUC8_db@data$Withdrawals_2016))
  
  HUC8_db@data$Consumption_2016<-ifelse(is.nan(HUC8_db@data$Consumption_2016)|is.infinite(HUC8_db@data$Consumption_2016),NA,HUC8_db@data$Consumption_2016)
  
  HUC8_glimpse<-HUC8_db@data
  assign("HUC8_glimpse",HUC8_glimpse,envir = .GlobalEnv)
  assign(paste0("HUC8",label),HUC8_db,envir = .GlobalEnv)
  
}
NWB_CU.ag(HUC8_ag,"_ag")

#-Fully Matched Facilities-#
NWB_CU(HUC8_match_energy,"_match_energy")
NWB_CU(HUC8_match_aq,"_match_aq")
NWB_CU(HUC8_match_commercial,"_match_commercial")
NWB_CU(HUC8_match_industrial,"_match_industrial")

#---Non-Energy Sectors---#
#-All Facilities-#
NWB_CU(HUC8_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
NWB_CU(HUC8_match_nonenergy,"_match_nonenergy")

###########################################################################################################################################
#--------------------------------Long Term Average (2010-2017) Net Water Balance and Consumptive Use--------------------------------------#
# median is used for all averages
Ave_NWB_CU<- function(HUC8,label){
  
  #----Discharges-----#
  HUC8@data$Discharge_ave<-apply(HUC8@data[,c("Discharges_2010","Discharges_2011","Discharges_2012",
                                              "Discharges_2013","Discharges_2014","Discharges_2015",
                                              "Discharges_2016")],1,median,na.rm=T)
  
  #----Withdrawals----#
  HUC8@data$Withdrawal_ave<-apply(HUC8@data[,c("Withdrawals_2010","Withdrawals_2011","Withdrawals_2012",
                                               "Withdrawals_2013","Withdrawals_2014","Withdrawals_2015",
                                               "Withdrawals_2016")],1,median,na.rm=T)
  
  #----Net Water Balance-----#
  HUC8@data$NetWB_ave<-apply(HUC8@data[,c("NetWB_2010","NetWB_2011","NetWB_2012",
                                          "NetWB_2013","NetWB_2014","NetWB_2015",
                                          "NetWB_2016")],1,median,na.rm=T)
  
  #----Consumption----#
  HUC8@data$Consumption_ave<-apply(HUC8@data[,c("Consumption_2010","Consumption_2011","Consumption_2012",
                                                "Consumption_2013","Consumption_2014","Consumption_2015",
                                                "Consumption_2016")],1,median,na.rm=T)
  
  HUC8@data$Consumption_ave<-ifelse(is.infinite(HUC8@data$Consumption_ave),NA,HUC8@data$Consumption_ave)
  
  HUC8_glimpse<-HUC8@data
  assign(paste0("HUC8",label),HUC8,envir = .GlobalEnv)
  assign("HUC8_glimpse",HUC8_glimpse,envir = .GlobalEnv)
}

#---All Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC8_all,"")
#-Fully Matched Facilities-#
Ave_NWB_CU(HUC8_matched,"_matched")

#---Subsetted by Sector---#

#-All Facilities-#
Ave_NWB_CU(HUC8_energy,"_energy")
Ave_NWB_CU(HUC8_aq,"_aq")
Ave_NWB_CU(HUC8_commercial,"_commercial")
Ave_NWB_CU(HUC8_industrial,"_industrial")
Ave_NWB_CU(HUC8_municipal,"_municipal")

Ave_NWB_CU.ag<- function(HUC8_db,label){
  
  #----Withdrawals----#
  HUC8_db@data$Withdrawal_ave<-apply(HUC8_db@data[,c("Withdrawals_2010","Withdrawals_2011","Withdrawals_2012",
                                                     "Withdrawals_2013","Withdrawals_2014","Withdrawals_2015",
                                                     "Withdrawals_2016")],1,median,na.rm=T)
  
  #----Consumption----#
  HUC8_db@data$Consumption_ave<-apply(HUC8_db@data[,c("Consumption_2010","Consumption_2011","Consumption_2012",
                                                      "Consumption_2013","Consumption_2014","Consumption_2015",
                                                      "Consumption_2016")],1,median,na.rm=T)
  
  HUC8_db@data$Consumption_ave<-ifelse(is.infinite(HUC8_db@data$Consumption_ave),NA,HUC8_db@data$Consumption_ave)
  
  
  HUC8_glimpse<-HUC8_db@data
  assign(paste0("HUC8",label),HUC8_db,envir = .GlobalEnv)
  assign("HUC8_glimpse",HUC8_glimpse,envir = .GlobalEnv)
}
Ave_NWB_CU.ag(HUC8_ag,"_ag")

#-Fully Matched Facilities-#
Ave_NWB_CU(HUC8_match_energy,"_match_energy")
Ave_NWB_CU(HUC8_match_aq,"_match_aq")
Ave_NWB_CU(HUC8_match_commercial,"_match_commercial")
Ave_NWB_CU(HUC8_match_industrial,"_match_industrial")

#---Non-Energy Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC8_nonenergy,"_nonenergy")

#-Fully Matched Facilities-#
Ave_NWB_CU(HUC8_match_nonenergy,"_match_nonenergy")

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


###########################################################################################################################################
#-------------------------------------------Distribution of Discharging Facilities in HUC8----------------------------------------------#

HUC8_discharge<- function(HUC8_db,ECHO_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  
  Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill= cut(HUC8.df$Discharge_ave,breaks=c(0,1,5,10,25,50,max(HUC8.df$Discharge_ave,na.rm=T)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Discharge by HUC8 (MGD)", values=(Dis_Discrete), 
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
    scale_shape_manual(name="", values=17)+
    scale_colour_manual(values=NA)+
    guides(fill=guide_legend(order=1),
           shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
    # labs(title = paste0("Average Annual Total Discharge (MGD) 2010-2017: ",label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  #scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
  #arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(paste0("huc_dis_",fileext),plot1,envir = .GlobalEnv) 
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Discharges/"
  
  #ggsave(filename=paste0(path,"HUC8_discharge_",fileext,".pdf"), 
  #plot=plot1, 
  #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}

#---All Sectors---#

#-All Facilities-#
HUC8_discharge(HUC8_all,ECHO.test,"All Reporting Facilities in All Sectors","all_fac_all_sec")
#-Fully Matched Facilities-#
HUC8_discharge(HUC8_matched,ECHO.test_matched,"Matched Facilities in All Sectors","match_all_sector")

#---Subsetted by Sector---#

#-All Facilities-#
HUC8_discharge(HUC8_energy,ECHO.test_energy,"Energy Facilities","all_energy")
HUC8_discharge(HUC8_aq,ECHO.test_aq,"Aquaculture Facilities","all_aq")
HUC8_discharge(HUC8_commercial,ECHO.test_commercial,"Commercial Facilities","all_commercial")
HUC8_discharge(HUC8_industrial,ECHO.test_industrial,"Industrial Facilities","all_industrial")
HUC8_discharge(HUC8_municipal,ECHO.test_municipal,"Municipal Facilities","all_municipal")
HUC8_discharge(HUC8_nonenergy,ECHO.test_nonenergy,"Non-Energy Facilities","all_nonenergy")

discharge_subplot_all<- function(){
  
  subplot<-ggpubr::ggarrange(huc_dis_all_energy,huc_dis_all_nonenergy,huc_dis_all_industrial,huc_dis_all_commercial,
                             huc_dis_all_aq,huc_dis_all_municipal,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture","(f) Municipal"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("Visualizing Median Watershed Discharge 2010-2016: All Reporting VPDES Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"HUC8_Dis_All_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
discharge_subplot_all()

#-Fully Matched Facilities-#
HUC8_discharge(HUC8_match_energy,ECHO.test_match_energy,"Matched Energy Facilities","match_energy")
HUC8_discharge(HUC8_match_aq,ECHO.test_match_aq,"Matched Aquaculture Facilities","match_aq")
HUC8_discharge(HUC8_match_commercial,ECHO.test_match_commercial,"Matched Commercial Facilities","match_commercial")
HUC8_discharge(HUC8_match_industrial,ECHO.test_match_industrial,"Matched Industrial Facilities","match_industrial")
HUC8_discharge(HUC8_match_nonenergy,ECHO.test_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")

discharge_subplot_match<- function(){
  
  subplot<-ggpubr::ggarrange(huc_dis_match_energy,huc_dis_match_nonenergy,huc_dis_match_industrial,huc_dis_match_commercial,
                             huc_dis_match_aq,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("Visualizing Median Watershed Discharge 2010-2016: Matched VPDES Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"HUC8_Dis_Matched_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
discharge_subplot_match()

###########################################################################################################################################
#-----------------------------------------Distribution of Withdrawing Facilities in HUC8------------------------------------------------#

HUC8_withdrawal<- function(HUC8_db,VWUDS_points,label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill= cut(HUC8.df$Withdrawal_ave,breaks=c(0,1,5,10,25,50,max(HUC8.df$Withdrawal_ave,na.rm=TRUE)),include.lowest=T), colour=""))+
    scale_fill_manual(name="Withdrawal by HUC8 (MGD)", values=(With_Discrete),
                      labels=c(paste("<",1),
                               paste(1,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",25),
                               paste(25,"-",50),
                               paste(50,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
    geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, shape="VWUDS Facility"),size=3.0,colour="#252525")+
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
  #scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
  #arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  
  assign(paste0("huc_with_",fileext),plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Withdrawals/"
  
  #ggsave(filename=paste0(path,"HUC8_withdrawal_",fileext,".pdf"), 
  #plot=plot1, 
  #width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}

#---All Sectors---#

#-All Facilities-#
HUC8_withdrawal(HUC8_all,VWUDS.test,"All Reporting Facilities in All Sectors","all_fac_all_sec")
#-Fully Matched Facilities-#
HUC8_withdrawal(HUC8_matched,VWUDS.test_matched,"Matched Facilities in All Sectors","match_all_sector")

#---Subsetted by Sector---#

#-All Facilities-#
HUC8_withdrawal(HUC8_energy,VWUDS.test_energy,"Energy Facilities","all_energy")
HUC8_withdrawal(HUC8_ag,VWUDS.test_ag,"Agriculture/Irrigation Facilities","all_ag")
HUC8_withdrawal(HUC8_aq,VWUDS.test_aq,"Aquaculture Facilities","all_aq")
HUC8_withdrawal(HUC8_commercial,VWUDS.test_commercial,"Commercial Facilities","all_commercial")
HUC8_withdrawal(HUC8_industrial,VWUDS.test_industrial,"Industrial Facilities","all_industrial")
HUC8_withdrawal(HUC8_municipal,VWUDS.test_municipal,"Municipal Facilities","all_municipal")
HUC8_withdrawal(HUC8_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities","all_nonenergy")

withdrawal_subplot_all<- function(){
  
  subplot<-ggpubr::ggarrange(huc_with_all_energy,huc_with_all_nonenergy,huc_with_all_industrial,huc_with_all_commercial,
                             huc_with_all_aq,huc_with_all_ag,huc_with_all_municipal,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(d) Aquaculture","(e) Agriculture/Irrigation","(f) Municipal"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=4)%>%annotate_figure(
                               top = text_grob("Visualizing Median Watershed Withdrawal 2010-2016: All Reporting VWUDS Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"HUC8_With_All_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
withdrawal_subplot_all()

#-Fully Matched Facilities-#
HUC8_withdrawal(HUC8_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities","match_energy")
HUC8_withdrawal(HUC8_match_aq,VWUDS.test_match_aq,"Matched Aquaculture Facilities","match_aq")
HUC8_withdrawal(HUC8_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities","match_commercial")
HUC8_withdrawal(HUC8_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities","match_industrial")
HUC8_withdrawal(HUC8_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")

withdrawal_subplot_match<- function(){
  
  subplot<-ggpubr::ggarrange(huc_with_match_energy,huc_with_match_nonenergy,huc_with_match_industrial,
                             huc_with_match_commercial,
                             huc_with_match_aq,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Commercial",
                                      "(e) Aquaculture"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("Visualizing Median Watershed Withdrawal 2010-2016: Matched VWUDS Facilities", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"HUC8_With_Matched_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
withdrawal_subplot_match()

###########################################################################################################################################
#---------------------------------------------------Consumption over HUC8 Watersheds-------------------------------------------------------------#

HUC8_consumption<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  # path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
  #plot=plot1, 
  #width=13.33, height=6.66,units="in")
  
  # return(list(plot1))
}

#--------------------All Reporting Facilities---------------------#
HUC8_consumption(HUC8_all,ECHO.test,VWUDS.test,"All Reporting Facilities in All Sectors","all_fac_all_sector")

HUC8_consumption(HUC8_commercial,ECHO.test_commercial,VWUDS.test_commercial,"Commercial Facilities","all_commercial")
HUC8_consumption(HUC8_industrial,ECHO.test_industrial,VWUDS.test_industrial,"Industrial Facilities","all_industrial")
HUC8_consumption(HUC8_municipal,ECHO.test_municipal,VWUDS.test_municipal,"Municipal Facilities","all_municipal")
HUC8_consumption(HUC8_nonenergy,ECHO.test_nonenergy,VWUDS.test_nonenergy,"Non-Energy Facilities","HUC8_all_nonenergy")
HUC8_consumption(HUC8_energy,ECHO.test_energy,VWUDS.test_energy,"Energy Facilities","all_energy")

HUC8_consumption_ag<- function(HUC8_db,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(
                        paste(0,"-",0.25),
                        paste(0.25,"-",0.5),
                        paste(0.5,"-",0.75),
                        paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
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
  # scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
  # arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), plot=plot1, width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
}
HUC8_consumption_ag(HUC8_ag,VWUDS.test_ag,"Agriculture/Irrigation Facilities","all_ag")

HUC8_consumption_aq<- function(HUC8, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=2.0,colour="#252525")+
    # scale_shape_manual(name="", values=17)+
    # geom_point(data=VWUDS_points, aes(x=VWUDS.Long, y=VWUDS.Lat, size="VWUDS Facility"),colour="#252525")+
    # scale_size_manual(name="", values=2.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  # scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
  # arrow_length=75, arrow_distance = 60, arrow_north_size = 6)
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  # path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  # ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), plot=plot1, width=13.33, height=6.66,units="in")
  
}
HUC8_consumption_aq(HUC8_aq,ECHO.test_aq,VWUDS.test_aq,"Aquaculture Facilities","all_aq")

all_subplot<- function(){
  subplot<-ggpubr::ggarrange(all_energy,all_nonenergy,all_industrial,
                             all_aq,all_commercial,all_ag,all_municipal,
                             labels=c("(a) Energy","(b) Non-Energy","(c) Industrial","(d) Aquaculture",
                                      "(e) Commercial","(f) Agriculture/Irrigation","(g) Municipal"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=4)%>%annotate_figure(top = text_grob("Visualizing Watershed Consumption: All Reporting Facilities", color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"HUC8_CU_All_Subplot.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
all_subplot()

#--------------------Matched Facilities---------------------#
HUC8_consumption<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
    # scale_shape_manual(name="", values=17)+
    # geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, size="VWUDS Facility"),colour="#252525")+
    # scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
  # plot=plot1, 
  #width=13.33, height=6.66,units="in")
  
}

HUC8_consumption(HUC8_match_energy,ECHO.test_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities","match_energy")
HUC8_consumption(HUC8_match_commercial,ECHO.test_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities","match_commercial")
HUC8_consumption(HUC8_match_industrial,ECHO.test_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities","match_industrial")
HUC8_consumption(HUC8_match_nonenergy,ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities","HUC8_match_nonenergy")

HUC8_consumption_aq<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
    scale_colour_manual(values=NA)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, shape="VPDES Facility"),size=3.0,colour="#252525")+
    scale_shape_manual(name="", values=17)+
    geom_point(data=ECHO_points, aes(x=Fac.Long, y=Fac.Lat, size="VWUDS Facility"),colour="#252525")+
    scale_size_manual(name="", values=3.0)+
    guides(fill=guide_legend(order=1),
           colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
    # labs(title = paste0("Average Consumption Coefficient 2010-2017: ", label))+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()
  
  
  
  assign(fileext,plot1,envir = .GlobalEnv)
  
  #path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  #ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
  # plot=plot1, 
  #width=13.33, height=6.66,units="in")
}
HUC8_consumption_aq(HUC8_match_aq,ECHO.test_match_aq,VWUDS.test_match_aq,"Matched Aquaculture Facilities","match_aq")

matched_subplot<- function(){
  
  subplot<-ggpubr::ggarrange(match_energy,match_nonenergy,match_industrial,
                             match_aq,match_commercial,labels=c("(a) Energy","(b) Non-Energy","(c) Industrial",
                                                                "(d) Aquaculture","(e) Commercial"),
                             common.legend = T,
                             legend="bottom",
                             ncol=2,nrow=3)%>%annotate_figure(
                               top = text_grob("2010-2016 Median Annual HUC 8 Watershed Consumption Considering Matched Facility Data", 
                                               color = "black", face = "bold", size = 14))
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggexport(filename=paste0(path,"HUC8_CU_Matched_Subplot_points.pdf"), plot=subplot, width=13.33, height=6.66,units="in")
  
}
matched_subplot()



#------------------------------------------------------------------------------------#
#-----------------------------Matched Facilities-------------------------------------#

# With sizes of point corresponding to median annual consumption in MGD

#--Aquaculture--#
HUC8_consumption_aquaculture<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
HUC8_consumption_aquaculture(HUC8_match_aq,ECHO.test_match_aq,VWUDS.test_match_aq,"Matched Aquaculture Facilities","match_aq")

#--Commercial--#
HUC8_consumption_commercial<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
HUC8_consumption_commercial(HUC8_match_commercial,ECHO.test_match_commercial,VWUDS.test_match_commercial,"Matched Commercial Facilities","match_commercial")

#--Industrial--#
HUC8_consumption_industrial<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
HUC8_consumption_industrial(HUC8_match_industrial,ECHO.test_match_industrial,VWUDS.test_match_industrial,"Matched Industrial Facilities","match_industrial")


#---Non-Energy Sectors---#
HUC8_consumption_non_energy<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T))
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
HUC8_consumption_non_energy(HUC8_match_nonenergy,ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")

#--Energy--#
HUC8_consumption_energy<- function(HUC8_db, ECHO_points ,VWUDS_points, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8_db,VA,id=as.character(HUC8_db@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8_db@data[as.character(HUC8_db@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  ECHO_points$CU<-ECHO_points$Withdrawals_MGD-ECHO_points$Discharges_MGD
  
  ECHO_points<-ECHO_points%>%
    dplyr::group_by(VPDES.Facility.ID)%>%
    dplyr::summarise(Fac.Lat=first(Fac.Lat),Fac.Long=first(Fac.Long),
                     Ave_CU=median(CU,na.rm=T),Fuel_Type=first(Fuel_Type))
  
  ECHO_points<-dplyr::mutate_if(ECHO_points,is.factor,as.character)
  
  ECHO_points$Fuel_Type<-ifelse(ECHO_points$Fuel_Type=="Natural Gas","Combination Fossil",ECHO_points$Fuel_Type)
  
  plot1<-ggplot2::ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste("<",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"HUC8_CU_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  assign("ECHO_points",ECHO_points,envir = .GlobalEnv)
  
  return(list(plot1))
  
}
HUC8_consumption_energy(HUC8_match_energy,ECHO.test_match_energy,VWUDS.test_match_energy,"Matched Energy Facilities","match_energy")

###########################################################################################################################################
#-----------------------------------------Consumption over HUC8 Watersheds (without points)------------------------------------------------------#

HUC8_consumption_nopnt<- function(HUC8, label,fileext){
  
  HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  plot1<-ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC8.df$Consumption_ave,breaks=c(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
    scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                      labels=c(paste(round(quantile(HUC8.df$Consumption_ave,c(0.0),na.rm=T),digits=2),"-",0),
                               paste(0,"-",0.25),
                               paste(0.25,"-",0.5),
                               paste(0.5,"-",0.75),
                               paste(0.75,"-",1)),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
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
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/HUC8 Analysis/Consumption/"
  
  ggsave(filename=paste0(path,"HUC8_CU_nopnt_",fileext,".pdf"), 
         plot=plot1, 
         width=13.33, height=6.66,units="in")
  
  return(list(plot1))
  
  
}

#-All Facilities-#
HUC8_consumption_nopnt(HUC8,"All Reporting Facilities in All Sectors","all_fac_all_sector")
#-Fully Matched Facilities-#
HUC8_consumption_nopnt(HUC8_matched,"Matched Facilities in All Sectors","match_all_sector")

#---Subsetted by Sector---#

#-All Facilities-#
HUC8_consumption_nopnt(HUC8_energy,"Energy Facilities","all_energy")
HUC8_consumption_nopnt(HUC8_ag,"Agriculture/Irrigation Facilities","all_ag")
HUC8_consumption_nopnt(HUC8_commercial,"Commercial Facilities","all_commercial")
HUC8_consumption_nopnt(HUC8_industrial,"Industrial Facilities","all_industrial")
HUC8_consumption_nopnt(HUC8_municipal,"Municipal Facilities","all_municipal")

#-Fully Matched Facilities-#
HUC8_consumption_nopnt(HUC8_match_energy,"Matched Energy Facilities","match_energy")
HUC8_consumption_nopnt(HUC8_match_ag,"Matched Agriculture/Irrigation Facilities","match_ag")
HUC8_consumption_nopnt(HUC8_match_commercial,"Matched Commercial Facilities","match_commercial")
HUC8_consumption_nopnt(HUC8_match_industrial,"Matched Industrial Facilities","match_industrial")
HUC8_consumption_nopnt(HUC8_match_municipal,"Matched Municipal Facilities",",match_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
HUC8_consumption_nopnt(HUC8_nonenergy,"Non-Energy Facilities","all_nonenergy")

#-Fully Matched Facilities-#
HUC8_consumption_nopnt(HUC8_nonenergy,"Matched Non-Energy Facilities","match_nonenergy")


###########################################################################################################################################
#---------------------------------------------Net Water Balance over HUC8 Watersheds-------------------------------------------------------------#


HUC8_NWB<- function(HUC8,label){
  
  HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  NWB_Discrete<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#2b8cbe")
  
  ggplot()+
    geom_polygon(
      data=HUC8.df,
      aes(x=long, y= lat, group=group,
          fill= cut(HUC8.df$NetWB_ave,breaks=c(quantile(HUC8.df$NetWB_ave,c(0),na.rm=T),-250,-150,-10,0,quantile(HUC8.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
    scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete,
                      labels=c(paste(-1250,"-",-250),
                               paste(-250,"-",-150),
                               paste(-150,"-",-10),
                               paste(-10,"-",0),
                               paste(0,"<")),
                      na.value="transparent",drop=FALSE)+
    geom_polygon(
      data=HUC8.df,
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
                   Ave_Withdrawal_mgd=median(Withdrawals_MGD,na.rm=T),Ave_Discharge_mgd=median(Discharges_MGD,na.rm=T),HUC8Name=first(HUC8Name),Waterbody=first(Waterbody))

VA_River<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/VA_Rivers_Clip.shp")
VA_River<-sp::spTransform(VA_River, CRS("+init=epsg:4269"))#Reproject shapefiles to NAD83=EPSG Code of 4269
VA_River.df<-fortify(VA_River)

case.study.location<- function(VPDES.ID,VWUDS.ID){
  
  full_match_2010_2017_summary$VPDES_HUC8<-ECHO.test_matched$HUC8Name[match(full_match_2010_2017_summary$VPDES.Facility.ID,ECHO.test_matched$VPDES.Facility.ID)]
  full_match_2010_2017_summary$VWUDS_HUC8<-VWUDS.test_matched$HUC8Name[match(full_match_2010_2017_summary$VWUDS.Facility.ID,VWUDS.test_matched$VWUDS.Facility.ID)]
  
  HUC8_Clipped<-gIntersection(HUC8,VA,id=as.character(HUC8@data$HUC8),byid=TRUE,drop_lower_td=TRUE)
  HUC8_Clipped<-SpatialPolygonsDataFrame(HUC8_Clipped,HUC8@data[as.character(HUC8@data$HUC8)%in%names(HUC8_Clipped),],match.ID = "HUC8")
  
  HUC8.df<-broom::tidy(HUC8_Clipped)
  HUC8_Clipped$polyID<-sapply(slot(HUC8_Clipped,"polygons"), function(x) slot(x, "ID"))
  HUC8.df<-merge(HUC8.df, HUC8_Clipped, by.x="id", by.y="polyID")
  
  ggplot()+
    geom_polygon(data=HUC8.df,aes(x=long, y= lat, group=group),color="#252525",fill="transparent",alpha=0.5)+
    geom_path(data=VA_River.df, aes(x=long,y=lat,group=group,linetype=paste0("Rivers & Streams")), color="#9ecae1", size=1, alpha=0.5)+
    scale_linetype_manual(name="",values=c(1))+
    geom_point(data=subset(full_match_2010_2017_summary,full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID), aes(x=VPDES.Fac.Long, y=VPDES.Fac.Lat,shape="VPDES Facility"),size=3,colour="#034e7b")+
    geom_label_repel(aes(label=full_match_2010_2017_summary$VPDES_HUC8[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID],
                         x=full_match_2010_2017_summary$VPDES.Fac.Long[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID],
                         y=full_match_2010_2017_summary$VPDES.Fac.Lat[full_match_2010_2017_summary$VPDES.Facility.ID==VPDES.ID]))+
    scale_shape_manual(name="", values=17)+
    geom_point(data=subset(full_match_2010_2017_summary,full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID), aes(x=VWUDS.Fac.Long, y=VWUDS.Fac.Lat, size="VWUDS Facility"),colour="#99000d")+
    geom_label_repel(aes(label=full_match_2010_2017_summary$VWUDS_HUC8[full_match_2010_2017_summary$VWUDS.Facility.ID==VWUDS.ID], 
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

MK_HUC8_Compile<- function(ECHO_2010_2017,VWUDS_2010_2017,label){
  
  HUC8<-as.vector(HUC8@data$HUC8)
  
  TS_HUC8_Discharges<-ECHO_2010_2017%>%
    dplyr::group_by(Date,HUC8)%>%
    dplyr::summarise(Discharge=sum(Discharges_MGD,na.rm=T))%>%
    tidyr::spread(HUC8, Discharge)
  
  Missing<-setdiff(HUC8,names(TS_HUC8_Discharges))
  TS_HUC8_Discharges[Missing]<-NA
  TS_HUC8_Discharges<-TS_HUC8_Discharges[HUC8]
  
  TS_HUC8_Withdrawals<-VWUDS_2010_2017%>%
    dplyr::group_by(Date,HUC8)%>%
    dplyr::summarise(Withdrawal=sum(Withdrawals_MGD,na.rm=T))%>%
    tidyr::spread(HUC8, Withdrawal)
  
  Missing<-setdiff(HUC8,names(TS_HUC8_Withdrawals))
  TS_HUC8_Withdrawals[Missing]<-NA
  TS_HUC8_Withdrawals<-TS_HUC8_Withdrawals[HUC8]
  
  # Timeseries Consumption in HUC8 for Mann Kendall Analysis
  CU_Function<-function(x,y) (ifelse(is.na(x),0,x)-ifelse(is.na(y),0,y))/(ifelse(is.na(x),0,x))
  TS_HUC8_Withdrawals<-TS_HUC8_Withdrawals[,order(names(TS_HUC8_Withdrawals))]
  TS_HUC8_Discharges<-TS_HUC8_Discharges[,order(names(TS_HUC8_Discharges))]
  
  TS_CU_HUC8<-data.frame(Date=unique(VWUDS_2010_2017$Date),mapply(CU_Function,TS_HUC8_Withdrawals,TS_HUC8_Discharges),stringsAsFactors = F)
  
  colnames(TS_CU_HUC8)<-gsub("X","",colnames(TS_CU_HUC8))
  
  assign(label,TS_CU_HUC8,envir = .GlobalEnv)
  
  save(TS_CU_HUC8,file=paste0("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/HUC8_",label,".RData"))
  
}

#---All Sectors---#
#-All Facilities-#
MK_HUC8_Compile(ECHO.test,VWUDS.test,"TS_CU_All")
#-Fully Matched Facilities-#
MK_HUC8_Compile(ECHO.test_matched,VWUDS.test_matched,"TS_CU_Matched")

#---Subsetted by Sector---#

#-All Facilities-#
MK_HUC8_Compile(ECHO.test_energy,VWUDS.test_energy,"TS_CU_All_Energy")
MK_HUC8_Compile(ECHO.test_ag,VWUDS.test_ag,"TS_CU_All_Ag")
MK_HUC8_Compile(ECHO.test_commercial,VWUDS.test_commercial,"TS_CU_All_Commercial")
MK_HUC8_Compile(ECHO.test_industrial,VWUDS.test_industrial,"TS_CU_All_Industrial")
MK_HUC8_Compile(ECHO.test_municipal,VWUDS.test_municipal,"TS_CU_All_Municipal")

#-Fully Matched Facilities-#
MK_HUC8_Compile(ECHO.test_match_energy,VWUDS.test_match_energy,"TS_CU_Match_Energy")
MK_HUC8_Compile(ECHO.test_match_ag,VWUDS.test_match_ag,"TS_CU_Match_Ag")
MK_HUC8_Compile(ECHO.test_match_commercial,VWUDS.test_match_commercial,"TS_CU_Match_Commercial")
MK_HUC8_Compile(ECHO.test_match_industrial,VWUDS.test_match_industrial,"TS_CU_Match_Industrial")
MK_HUC8_Compile(ECHO.test_match_municipal,VWUDS.test_match_municipal,"TS_CU_Match_Municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
MK_HUC8_Compile(ECHO.test_nonenergy,VWUDS.test_match_nonenergy,"TS_CU_All_NonEnergy")

#-Fully Matched Facilities-#
MK_HUC8_Compile(ECHO.test_match_nonenergy,VWUDS.test_match_nonenergy,"TS_CU_Match_NonEnergy")
