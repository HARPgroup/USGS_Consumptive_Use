###########################################################################################################################################
#-------------------------------------------Estimating Consumptive Use in Virginian HUC 10 Watersheds--------------------------------------#

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
#-------------------------------------------HUC 10 and Virginia Shapefile Manipulation-----------------------------------------------------#

#Load databases and extract required layers
HUC10<-readOGR('G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/GithubFiles/GithubFiles/Spatial Analysis/HUC.gdb',layer='WBDHU10')
VA<-readOGR('G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/GithubFiles/GithubFiles/Spatial Analysis/EvapInputs.gdb',layer="VA")

#Reproject shapefiles to NAD83=EPSG Code of 4269
HUC10<-sp::spTransform(HUC10, CRS("+init=epsg:4269"))
VA<-sp::spTransform(VA, CRS("+init=epsg:4269"))

#Crop Watersheds to Virginia State Boundaries
HUC10_Clipped<-gIntersection(HUC10,VA,id=as.character(HUC10@data$HUC10),byid=TRUE,drop_lower_td=TRUE)

#Create HUC10 Dataframe that will be used in future overlay processes
HUC10_Overlay<-HUC10 #Keep integrity of spatial dataframe
HUC10_Overlay@data<-HUC10_Overlay@data[,c(11,12)] 
names(HUC10_Overlay@data)<-c("HUC10","HUC10Name")


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
  
  ####Create Spatial Data Frame from Points and Overlay on Clipped HUC10 Shapefile####
  ####FROM Delivery Transfers####
  #Looks at all FROM delivery transfers with real geometry and creates a spatial dataframe 'dFrom'
  #Spatially overlays HUC boundaries on dFrom such that each FROM transfer is labeled by origin HUC
  dFrom<-deliveries[!(is.na(deliveries$geomFlat)&is.na(deliveries$geomFlon)),]
  dFrom<-SpatialPointsDataFrame(data.frame(lon=dFrom$geomFlon,lat=dFrom$geomFlat),dFrom,proj4string = CRS("+init=epsg:4269")) #Making data spatial
  HUC10_Facilities<-over(dFrom,HUC10_Overlay)#Spatial overlay
  dFrom@data$HUC10Name<-HUC10_Facilities$HUC10Name
  dFrom@data$HUC10<-HUC10_Facilities$HUC10
  
  ####TO Delivery Transfers####
  #Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
  #Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
  dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
  dTo<-SpatialPointsDataFrame(data.frame(lon=dTo$geomTlon,lat=dTo$geomTlat),dTo,proj4string = CRS("+init=epsg:4269"))
  HUC10_Facilities<-over(dTo,HUC10_Overlay)#Spatial overlay
  dTo@data$HUC10Name<-HUC10_Facilities$HUC10Name
  dTo@data$HUC10<-HUC10_Facilities$HUC10
  
  ####Determine if TO Transfers are leaving watershed boundaries####
  #Need to identify if transfers are leaving and entering the same HUC. If so, these may be ignored
  #We are only concerned with interbasin transfers and need to identify these with the following code
  dTo@data$interbasin<-NA
  dFrom@data$interbasin<-NA
  #Check each transfer in the delivery VA Hydro transfers to see if its FROM HUC10 is different than its TO HUC10
  
  for (i in 1:length(dTo@data$hydroid)){
    ToHUC<-as.character(dTo@data$HUC10[i])
    FromHUC<-as.character(dFrom@data$HUC10[i])
    if(is.na(ToHUC)){
      ToHUC<-'Null HUC10'
    }
    if(is.na(FromHUC)){
      FromHUC<-'Null HUC10' 
    }
    interbasin<-0
    if(ToHUC!=FromHUC){ #if the HUC10 does not match, mark as interbasin delivery
      interbasin<-1
    }
    dTo@data$interbasin[i]<-interbasin
    dFrom@data$interbasin[i]<-interbasin
  }
  
  ####Sum Net Water In and Out for each HUC10####
  ###FROM Deliveries###
  delf<-dFrom@data
  delf<-delf[delf$interbasin==1,] #if interbasin is equal to 1, it is crossing boundaries---so just include those
  
  delf<-delf%>%
    dplyr::group_by(HUC10Name, Year)%>%
    dplyr::summarize(HUC10=first(HUC10),interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD
  
  delf$HUC10Name<-as.character(delf$HUC10Name)
  delf$HUC10Name[is.na(delf$HUC10Name)]<-'Fell Outside HUC10 Limits'
  
  ###TO Deliveries###
  delt<-dTo@data
  delt<-delt[delt$interbasin==1,] #narrow down to deliveries happening across borders
  
  delt<-
    delt%>%
    dplyr::group_by(HUC10Name, Year)%>%
    dplyr::summarize(HUC10=first(HUC10),interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)#units in MGM to MGD
  
  
  delt$HUC10Name<-as.character(delt$HUC10Name)
  delt$HUC10Name[is.na(delt$HUC10Name)]<-'Fell Outside HUC10 Limits'
  
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
  
  ####Create Spatial Data Frame from Points and Overlay on Clipped HUC10 Shapefile####
  ####FROM Release Transfers####
  rFrom<-releases[!(is.na(releases$geomFlat)&is.na(releases$geomFlon)),]
  rFrom<-SpatialPointsDataFrame(data.frame(lon=rFrom$geomFlon,lat=rFrom$geomFlat),rFrom,proj4string = CRS("+init=epsg:4269"))
  HUC10_Facilities<-over(rFrom,HUC10_Overlay)
  rFrom@data$HUC10<-HUC10_Facilities$HUC10
  rFrom@data$HUC10Name<-HUC10_Facilities$HUC10Name
  ####TO Release Transfers####
  rTo<-releases[!(is.na(releases$geomTlat)&is.na(releases$geomTlon)),]
  rTo<-SpatialPointsDataFrame(data.frame(lon=rTo$geomTlon,lat=rTo$geomTlat),rTo,proj4string = CRS("+init=epsg:4269"))
  HUC10_Facilities<-over(rTo,HUC10_Overlay)
  rTo@data$HUC10<-HUC10_Facilities$HUC10
  rTo@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  ####Determine if Release FROM Transfers are leaving HUC10 boundaries####
  rTo@data$interbasin<-NA
  rFrom@data$interbasin<-NA
  
  for (i in 1:length(rTo@data$hydroid)){
    ToHUC<-as.character(rTo@data$HUC10[i])
    FromHUC<-as.character(rFrom@data$HUC10[i])
    if(is.na(ToHUC)){ #if the HUC code is NA
      ToHUC<-'Null HUC10'
    }
    if(is.na(FromHUC)){
      FromHUC<-'Null HUC10' 
    }
    interbasin<-0
    if(ToHUC!=FromHUC){
      interbasin<-1
    }
    rTo@data$interbasin[i]<-interbasin #1 indicating transfer is crossing watershed boundaries
    rFrom@data$interbasin[i]<-interbasin
  }
  
  ####Sum Net Water In and Out for each HUC10####
  ###FROM Releases###
  relf<-rFrom@data
  relf<-relf[relf$interbasin==1,]#remember intratransfers are indicated with a 0
  
  relf<-relf%>% #Summarise by HUC10 and year
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10), interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)  #MGM to MGD
  
  relf$HUC10Name<-as.character(relf$HUC10Name)
  relf$HUC10Name[is.na(relf$HUC10Name)]<-'Fell Outside HUC10 Limits'
  
  ###TO Releases###
  relt<-rTo@data
  relt<-relt[relt$interbasin==1,]
  
  relt<-relt%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10), interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T)/12)
  
  relt$HUC10Name<-as.character(relt$HUC10Name)
  relt$HUC10Name[is.na(relt$HUC10Name)]<-'Fell Outside HUC10 Limits'
  
  assign("relf",relf,envir=.GlobalEnv)
  assign("relt",relt,envir=.GlobalEnv)
  
}
releases_func(rel)

#-----------------------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------Calculate Net Transfers for Each HUC10 Watershed-------------------------------------------------#

# Loop through each HUC10 and check for summed releases and deliveries
# Water out is defined as the "from's" and Water in are the "to's"
# This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

transfers<- function(relf,delf,relt,delt){
  
  HUC10_waterout<-as.data.table(rbind(relf,delf))
  HUC10_waterout<-HUC10_waterout[, lapply(.SD,sum), by=list(HUC10Name, Year, HUC10), .SDcols=c(4,5)]
  
  HUC10_waterin<-as.data.table(rbind(relt,delt))
  HUC10_waterin<-HUC10_waterin[, lapply(.SD,sum), by=list(HUC10Name, Year, HUC10), .SDcols=c(4,5)]
  
  #---Year 2010---#
  HUC10@data$waterout_2010<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2010"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2010"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2010"])],NA)
  HUC10@data$waterin_2010<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2010"],HUC10_waterin$waterin[HUC10_waterin$Year=="2010"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2010"])],NA)
  HUC10@data$transferred_2010<- (rowSums(HUC10@data[,(15:16)],na.rm=T))
  HUC10@data$transferred_2010<-ifelse(is.na(HUC10@data$waterin_2010)&is.na(HUC10@data$waterout_2010),NA,HUC10@data$transferred_2010)
  
  #---Year 2011---#
  HUC10@data$waterout_2011<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2011"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2011"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2011"])],NA)
  HUC10@data$waterin_2011<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2011"],HUC10_waterin$waterin[HUC10_waterin$Year=="2011"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2011"])],NA)
  HUC10@data$transferred_2011<- (rowSums(HUC10@data[,(18:19)],na.rm=T))
  HUC10@data$transferred_2011<-ifelse(is.na(HUC10@data$waterin_2011)&is.na(HUC10@data$waterout_2011),NA,HUC10@data$transferred_2011)
  
  #---Year 2012---#
  HUC10@data$waterout_2012<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2012"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2012"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2012"])],NA)
  HUC10@data$waterin_2012<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2012"],HUC10_waterin$waterin[HUC10_waterin$Year=="2012"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2012"])],NA)
  HUC10@data$transferred_2012<- (rowSums(HUC10@data[,(21:22)],na.rm=T))
  HUC10@data$transferred_2012<-ifelse(is.na(HUC10@data$waterin_2012)&is.na(HUC10@data$waterout_2012),NA,HUC10@data$transferred_2012)
  
  #---Year 2013---#
  HUC10@data$waterout_2013<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2013"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2013"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2013"])],NA)
  HUC10@data$waterin_2013<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2013"],HUC10_waterin$waterin[HUC10_waterin$Year=="2013"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2013"])],NA)
  HUC10@data$transferred_2013<- (rowSums(HUC10@data[,(24:25)],na.rm=T))
  HUC10@data$transferred_2013<-ifelse(is.na(HUC10@data$waterin_2013)&is.na(HUC10@data$waterout_2013),NA,HUC10@data$transferred_2013)
  
  #---Year 2014---#
  HUC10@data$waterout_2014<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2014"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2014"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2014"])],NA)
  HUC10@data$waterin_2014<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2014"],HUC10_waterin$waterin[HUC10_waterin$Year=="2014"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2014"])],NA)
  HUC10@data$transferred_2014<- (rowSums(HUC10@data[,(27:28)],na.rm=T))
  HUC10@data$transferred_2014<-ifelse(is.na(HUC10@data$waterin_2014)&is.na(HUC10@data$waterout_2014),NA,HUC10@data$transferred_2014)
  
  #---Year 2015---#
  HUC10@data$waterout_2015<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2015"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2015"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2015"])],NA)
  HUC10@data$waterin_2015<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2015"],HUC10_waterin$waterin[HUC10_waterin$Year=="2015"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2015"])],NA)
  HUC10@data$transferred_2015<- (rowSums(HUC10@data[,(30:31)],na.rm=T))
  HUC10@data$transferred_2015<-ifelse(is.na(HUC10@data$waterin_2015)&is.na(HUC10@data$waterout_2015),NA,HUC10@data$transferred_2015)
  
  #---Year 2016---#
  HUC10@data$waterout_2016<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2016"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2016"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2016"])],NA)
  HUC10@data$waterin_2016<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2016"],HUC10_waterin$waterin[HUC10_waterin$Year=="2016"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2016"])],NA)
  HUC10@data$transferred_2016<- (rowSums(HUC10@data[,(33:34)],na.rm=T))
  HUC10@data$transferred_2016<-ifelse(is.na(HUC10@data$waterin_2016)&is.na(HUC10@data$waterout_2016),NA,HUC10@data$transferred_2016)
  
  #---Year 2017---#
  HUC10@data$waterout_2017<-ifelse(HUC10@data$HUC10%in%HUC10_waterout$HUC10[HUC10_waterout$Year=="2017"],-HUC10_waterout$waterout[HUC10_waterout$Year=="2017"][match(HUC10@data$HUC10,HUC10_waterout$HUC10[HUC10_waterout$Year=="2017"])],NA)
  HUC10@data$waterin_2017<-ifelse(HUC10@data$HUC10%in%HUC10_waterin$HUC10[HUC10_waterin$Year=="2017"],HUC10_waterin$waterin[HUC10_waterin$Year=="2017"][match(HUC10@data$HUC10,HUC10_waterin$HUC10[HUC10_waterin$Year=="2017"])],NA)
  HUC10@data$transferred_2017<- (rowSums(HUC10@data[,(36:37)],na.rm=T))
  HUC10@data$transferred_2017<-ifelse(is.na(HUC10@data$waterin_2017)&is.na(HUC10@data$waterout_2017),NA,HUC10@data$transferred_2017)
  
  
  HUC10_Transfers<-data.frame(HUC10Name=HUC10@data$Name,
                               HUC10=HUC10@data$HUC10,
                               Transfers_2010_MGD=HUC10@data$transferred_2010,
                               Transfers_2011_MGD=HUC10@data$transferred_2011,
                               Transfers_2012_MGD=HUC10@data$transferred_2012,
                               Transfers_2013_MGD=HUC10@data$transferred_2013,
                               Transfers_2014_MGD=HUC10@data$transferred_2014,
                               Transfers_2015_MGD=HUC10@data$transferred_2015,
                               Transfers_2016_MGD=HUC10@data$transferred_2016,
                               Transfers_2017_MGD=HUC10@data$transferred_2017)
  
  assign("HUC10",HUC10,envir = .GlobalEnv)
  assign("HUC10_Transfers",HUC10_Transfers,envir = .GlobalEnv)
  
  rm(relf,delf,relt,delt,deliveries,envir = .GlobalEnv)
  
}
transfers(relf,delf,relt,delt)

###########################################################################################################################################
#----------------------------------------------------Calculating Discharges---------------------------------------------------------------#

#---Load in Discharge Data previously cleaned---#

ECHO_2010_2017<-read.table("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/DataFiles/ECHO_2010_2017_QAQC_statewide.txt", sep="\t", header=T)

#---Load in fully matched Discharge Data previously cleaned---#
  ## NOTE LEAVE THIS OUT SINCE SPATIALLY AGGREGATED DATA IS FOCUSED ON UNMATCHED
  #full_match_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/full_match_2010_2017.txt", sep="\t", header=T)


#-------------Sector Analysis---------------#

# All of these functions are independent. So you can run each of them.

#---Option to Analyze all Sectors at once---#
all_discharge<- function(discharge_db,label){
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC10 Shapefile--#
  HUC10_Facilities<-over(discharge_db,HUC10_Overlay)
  discharge_db@data$HUC10<-HUC10_Facilities$HUC10
  discharge_db@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC10Name=first(HUC10Name),
                     HUC10=first(HUC10))%>%arrange(desc(Discharges_MGD))
  
  
  HUC10_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10),
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
  
  assign(paste0("HUC10_Discharges",label),HUC10_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
all_discharge(ECHO_2010_2017,"") # All Facilities

#---Option to separate by Water Use Sector---#
sector_discharge<- function(discharge_db,sector,label){
  
  discharge_db<-subset(discharge_db,subset=discharge_db$Use.Type==sector)
  
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC10 Shapefile--#
  HUC10_Facilities<-over(discharge_db,HUC10_Overlay)
  discharge_db@data$HUC10<-HUC10_Facilities$HUC10
  discharge_db@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC10Name=first(HUC10Name),
                     HUC10=first(HUC10))%>%arrange(desc(Discharges_MGD))
  
  
  HUC10_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10),
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
  
  assign(paste0("HUC10_Discharges",label),HUC10_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
sector_discharge(ECHO_2010_2017,"Energy","_energy")
sector_discharge(ECHO_2010_2017,"Aquaculture","_aq")
sector_discharge(ECHO_2010_2017,"Commercial","_commercial")
sector_discharge(ECHO_2010_2017,"Industrial","_industrial")
sector_discharge(ECHO_2010_2017,"Municipal","_municipal")

#--Option to look at Non-Energy Sectors--#
nonenergy_discharge<- function(discharge_db,label){
  
  discharge_db<-subset(discharge_db,subset=!discharge_db$Use.Type=="Energy")
  discharge_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=discharge_db$Fac.Long,Facility_Latitude=discharge_db$Fac.Lat),discharge_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  discharge_db@data$VPDES.Name<-as.character(discharge_db@data$VPDES.Name)
  discharge_db@data$Discharges_MGD<-as.numeric(discharge_db@data$Discharges_MGD)#after QA/QC
  
  #--Overlay with HUC10 Shapefile--#
  HUC10_Facilities<-over(discharge_db,HUC10_Overlay)
  discharge_db@data$HUC10<-HUC10_Facilities$HUC10
  discharge_db@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  discharge_db.test<-as.data.frame(discharge_db@data)
  
  Outfall_Discharges<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Year)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC10Name=first(HUC10Name),
                     HUC10=first(HUC10))%>%arrange(desc(Discharges_MGD))
  
  
  HUC10_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10),
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
  
  assign(paste0("HUC10_Discharges",label),HUC10_Discharges,envir = .GlobalEnv)
  assign(paste0("ECHO.test",label),discharge_db.test,envir = .GlobalEnv)
}
nonenergy_discharge(ECHO_2010_2017,"_nonenergy")

###########################################################################################################################################
#----------------------------------------------------Calculating Withdrawals--------------------------------------------------------------#

#---Load in Withdrawal Data previously cleaned in VWUDS_QAQC.R---#
load("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/DataFiles/VWUDS_2010_2017.RData")
VWUDS_2010_2017%>%dplyr::summarise(Facilities=n_distinct(Facility.ID),Sources=n_distinct(DEQ.ID.of.Source),Summed_W=sum(Withdrawals_MGD,na.rm=T))
colnames(VWUDS_2010_2017)[18:19]<-c("VWUDS.Lat","VWUDS.Long")
colnames(VWUDS_2010_2017)[6]<-c("VWUDS.Name")
colnames(VWUDS_2010_2017)[3]<-c("VWUDS.Facility.ID")
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
  
  #--Overlay with HUC10 Shapefile--#
  HUC10_Facilities<-over(withdrawal_db,HUC10_Overlay)
  withdrawal_db@data$HUC10<-HUC10_Facilities$HUC10
  withdrawal_db@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC10Name=first(HUC10Name),
                     HUC10=first(HUC10))
  
  
  HUC10_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10),
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
  
  assign(paste0("HUC10_Withdrawals",label),HUC10_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
all_withdrawal(VWUDS_2010_2017,"") # All Facilities

#---Option to separate by Water Use Sector---#
sector_withdrawal<- function(withdrawal_db,sector,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=withdrawal_db$Reclass_Use_Type==sector)
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                                       withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC10 Shapefile--#
  HUC10_Facilities<-over(withdrawal_db,HUC10_Overlay)
  withdrawal_db@data$HUC10<-HUC10_Facilities$HUC10
  withdrawal_db@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  #--Sum Discharges in the HUC 8 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC10Name=first(HUC10Name),
                     HUC10=first(HUC10))
  
  
  HUC10_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10),
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
  
  assign(paste0("HUC10_Withdrawals",label),HUC10_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
sector_withdrawal(VWUDS_2010_2017,"Energy","_energy")
sector_withdrawal(VWUDS_2010_2017,"Agriculture/Irrigation","_ag")
sector_withdrawal(VWUDS_2010_2017,"Aquaculture","_aq")
sector_withdrawal(VWUDS_2010_2017,"Commercial","_commercial")
sector_withdrawal(VWUDS_2010_2017,"Industrial","_industrial")
sector_withdrawal(VWUDS_2010_2017,"Municipal","_municipal")


#--Option to look at Non-Energy Sectors--#
nonenergy_withdrawal<- function(withdrawal_db,label){
  
  withdrawal_db<-subset(withdrawal_db,subset=!withdrawal_db$Reclass_Use_Type=="Energy")
  withdrawal_db<-sp::SpatialPointsDataFrame(data.frame(Facility_Longitude=withdrawal_db$VWUDS.Long,
                                                       Facility_Latitude=withdrawal_db$VWUDS.Lat),
                                                       withdrawal_db,proj4string = CRS("+init=epsg:4269"))#projecting to NAD83
  
  #----Set Data Type----#
  withdrawal_db@data$VWUDS.Name<-as.character(withdrawal_db@data$VWUDS.Name)
  withdrawal_db@data$Withdrawals_MGD<-as.numeric(withdrawal_db@data$Withdrawals_MGD)#after QA/QC
  
  #--Overlay with HUC10 Shapefile--#
  HUC10_Facilities<-over(withdrawal_db,HUC10_Overlay)
  withdrawal_db@data$HUC10<-HUC10_Facilities$HUC10
  withdrawal_db@data$HUC10Name<-HUC10_Facilities$HUC10Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC10Name=first(HUC10Name),
                     HUC10=first(HUC10))
  
  
  HUC10_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC10Name,Year)%>%
    dplyr::summarise(HUC10=first(HUC10),
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
  
  assign(paste0("HUC10_Withdrawals",label),HUC10_Withdrawals,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(VWUDS_2010_2017,"_nonenergy")

###########################################################################################################################################
#------------------------------------------Apply Withdrawals and Discharges into HUC10 Spatial Dataframe-----------------------------------#

HUC10_discharge_withdrawal<- function(HUC10_Discharges_db,HUC10_Withdrawals_db,label){
  #---Year 2010----#
  HUC10@data$Discharges_2010<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2010"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2010"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2010"])],NA)
  HUC10@data$Withdrawals_2010<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2010"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2010"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2010"])],NA)
  
  #---Year 2011----#
  HUC10@data$Discharges_2011<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2011"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2011"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2011"])],NA)
  HUC10@data$Withdrawals_2011<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2011"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2011"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2011"])],NA)
  
  #---Year 2012----#
  HUC10@data$Discharges_2012<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2012"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2012"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2012"])],NA)
  HUC10@data$Withdrawals_2012<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2012"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2012"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2012"])],NA)
  
  #---Year 2013----#
  HUC10@data$Discharges_2013<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2013"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2013"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2013"])],NA)
  HUC10@data$Withdrawals_2013<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2013"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2013"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2013"])],NA)
  
  #---Year 2014----#
  HUC10@data$Discharges_2014<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2014"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2014"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2014"])],NA)
  HUC10@data$Withdrawals_2014<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2014"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2014"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2014"])],NA)
  
  #---Year 2015----#
  HUC10@data$Discharges_2015<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2015"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2015"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2015"])],NA)
  HUC10@data$Withdrawals_2015<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2015"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2015"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2015"])],NA)
  
  #---Year 2016----#
  HUC10@data$Discharges_2016<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2016"],HUC10_Discharges_db$Discharge_MGD[HUC10_Discharges_db$Year=="2016"][match(HUC10@data$HUC10,HUC10_Discharges_db$HUC10[HUC10_Discharges_db$Year=="2016"])],NA)
  HUC10@data$Withdrawals_2016<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2016"],HUC10_Withdrawals_db$Withdrawals_MGD[HUC10_Withdrawals_db$Year=="2016"][match(HUC10@data$HUC10,HUC10_Withdrawals_db$HUC10[HUC10_Withdrawals_db$Year=="2016"])],NA)
  
  assign(paste0("HUC10",label),HUC10,envir = .GlobalEnv)
}

#----------------------------------------------------------#
#-------------------All Available Data---------------------#

#--All Sectors--#
HUC10_discharge_withdrawal(HUC10_Discharges,HUC10_Withdrawals,"_all")

#--Non-Energy--#
HUC10_discharge_withdrawal(HUC10_Discharges_nonenergy,HUC10_Withdrawals_nonenergy,"_nonenergy")
HUC10_discharge_withdrawal(HUC10_Discharges_energy,HUC10_Withdrawals_energy,"_energy")
HUC10_discharge_withdrawal(HUC10_Discharges_aq,HUC10_Withdrawals_aq,"_aq")
HUC10_discharge_withdrawal(HUC10_Discharges_commercial,HUC10_Withdrawals_commercial,"_commercial")
HUC10_discharge_withdrawal(HUC10_Discharges_industrial,HUC10_Withdrawals_industrial,"_industrial")
HUC10_discharge_withdrawal(HUC10_Discharges_municipal,HUC10_Withdrawals_municipal,"_municipal")

rm(HUC10_Discharges,HUC10_Discharges_commercial,HUC10_Discharges_energy,HUC10_Discharges_industrial,
   HUC10_Withdrawals,HUC10_Withdrawals_ag,HUC10_Withdrawals_commercial,HUC10_Withdrawals_energy,HUC10_Withdrawals_industrial,
   HUC10_Discharges_municipal,HUC10_Discharges_nonenergy,HUC10_Withdrawals_municipal,HUC10_Withdrawals_nonenergy)

###########################################################################################################################################
#-----------------------------------------------Net Water Balance and Consumptive Use-----------------------------------------------------#

NWB_CU<- function(HUC10_db,label){
  #---Year 2010----#
  HUC10_db@data$NetWB_2010<-(ifelse(is.na(HUC10_db@data$Discharges_2010),0,HUC10_db@data$Discharges_2010))-(ifelse(is.na(HUC10_db@data$Withdrawals_2010),0,HUC10_db@data$Withdrawals_2010))
  HUC10_db@data$NetWB_2010<-ifelse(is.na(HUC10_db@data$Discharges_2010)&is.na(HUC10_db@data$Withdrawals_2010),NA,HUC10_db@data$NetWB_2010)
  
  HUC10_db@data$Consumption_2010<-((ifelse(is.na(HUC10_db@data$Withdrawals_2010),0,HUC10_db@data$Withdrawals_2010))-(ifelse(is.na(HUC10_db@data$Discharges_2010),0,HUC10_db@data$Discharges_2010)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2010),0,HUC10_db@data$Withdrawals_2010))
  HUC10_db@data$Consumption_2010<-ifelse(is.nan(HUC10_db@data$Consumption_2010)|is.infinite(HUC10_db@data$Consumption_2010),NA,HUC10_db@data$Consumption_2010)
  
  #---Year 2011----#

  HUC10_db@data$NetWB_2011<-(ifelse(is.na(HUC10_db@data$Discharges_2011),0,HUC10_db@data$Discharges_2011))-(ifelse(is.na(HUC10_db@data$Withdrawals_2011),0,HUC10_db@data$Withdrawals_2011))
  HUC10_db@data$NetWB_2011<-ifelse(is.na(HUC10_db@data$Discharges_2011)&is.na(HUC10_db@data$Withdrawals_2011),NA,HUC10_db@data$NetWB_2011)
  
  HUC10_db@data$Consumption_2011<-((ifelse(is.na(HUC10_db@data$Withdrawals_2011),0,HUC10_db@data$Withdrawals_2011))-(ifelse(is.na(HUC10_db@data$Discharges_2011),0,HUC10_db@data$Discharges_2011)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2011),0,HUC10_db@data$Withdrawals_2011))
  HUC10_db@data$Consumption_2011<-ifelse(is.nan(HUC10_db@data$Consumption_2011)|is.infinite(HUC10_db@data$Consumption_2011),NA,HUC10_db@data$Consumption_2011)
  
  #---Year 2012----#
  HUC10_db@data$NetWB_2012<-(ifelse(is.na(HUC10_db@data$Discharges_2012),0,HUC10_db@data$Discharges_2012))-(ifelse(is.na(HUC10_db@data$Withdrawals_2012),0,HUC10_db@data$Withdrawals_2012))
  HUC10_db@data$NetWB_2012<-ifelse(is.na(HUC10_db@data$Discharges_2012)&is.na(HUC10_db@data$Withdrawals_2012),NA,HUC10_db@data$NetWB_2012)
  
  HUC10_db@data$Consumption_2012<-((ifelse(is.na(HUC10_db@data$Withdrawals_2012),0,HUC10_db@data$Withdrawals_2012))-(ifelse(is.na(HUC10_db@data$Discharges_2012),0,HUC10_db@data$Discharges_2012)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2012),0,HUC10_db@data$Withdrawals_2012))
  HUC10_db@data$Consumption_2012<-ifelse(is.nan(HUC10_db@data$Consumption_2012)|is.infinite(HUC10_db@data$Consumption_2012),NA,HUC10_db@data$Consumption_2012)
  
  #---Year 2013----#
  HUC10_db@data$NetWB_2013<-(ifelse(is.na(HUC10_db@data$Discharges_2013),0,HUC10_db@data$Discharges_2013))-(ifelse(is.na(HUC10_db@data$Withdrawals_2013),0,HUC10_db@data$Withdrawals_2013))
  HUC10_db@data$NetWB_2013<-ifelse(is.na(HUC10_db@data$Discharges_2013)&is.na(HUC10_db@data$Withdrawals_2013),NA,HUC10_db@data$NetWB_2013)
  
  HUC10_db@data$Consumption_2013<-((ifelse(is.na(HUC10_db@data$Withdrawals_2013),0,HUC10_db@data$Withdrawals_2013))-(ifelse(is.na(HUC10_db@data$Discharges_2013),0,HUC10_db@data$Discharges_2013)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2013),0,HUC10_db@data$Withdrawals_2013))
  HUC10_db@data$Consumption_2013<-ifelse(is.nan(HUC10_db@data$Consumption_2013)|is.infinite(HUC10_db@data$Consumption_2013),NA,HUC10_db@data$Consumption_2013)
  
  #---Year 2014----#
  HUC10_db@data$NetWB_2014<-(ifelse(is.na(HUC10_db@data$Discharges_2014),0,HUC10_db@data$Discharges_2014))-(ifelse(is.na(HUC10_db@data$Withdrawals_2014),0,HUC10_db@data$Withdrawals_2014))
  HUC10_db@data$NetWB_2014<-ifelse(is.na(HUC10_db@data$Discharges_2014)&is.na(HUC10_db@data$Withdrawals_2014),NA,HUC10_db@data$NetWB_2014)
  
  HUC10_db@data$Consumption_2014<-((ifelse(is.na(HUC10_db@data$Withdrawals_2014),0,HUC10_db@data$Withdrawals_2014))-(ifelse(is.na(HUC10_db@data$Discharges_2014),0,HUC10_db@data$Discharges_2014)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2014),0,HUC10_db@data$Withdrawals_2014))
  HUC10_db@data$Consumption_2014<-ifelse(is.nan(HUC10_db@data$Consumption_2014)|is.infinite(HUC10_db@data$Consumption_2014),NA,HUC10_db@data$Consumption_2014)
  
  #---Year 2015----#
  HUC10_db@data$NetWB_2015<-(ifelse(is.na(HUC10_db@data$Discharges_2015),0,HUC10_db@data$Discharges_2015))-(ifelse(is.na(HUC10_db@data$Withdrawals_2015),0,HUC10_db@data$Withdrawals_2015))
  HUC10_db@data$NetWB_2015<-ifelse(is.na(HUC10_db@data$Discharges_2015)&is.na(HUC10_db@data$Withdrawals_2015),NA,HUC10_db@data$NetWB_2015)
  
  HUC10_db@data$Consumption_2015<-((ifelse(is.na(HUC10_db@data$Withdrawals_2015),0,HUC10_db@data$Withdrawals_2015))-(ifelse(is.na(HUC10_db@data$Discharges_2015),0,HUC10_db@data$Discharges_2015)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2015),0,HUC10_db@data$Withdrawals_2015))
  HUC10_db@data$Consumption_2015<-ifelse(is.nan(HUC10_db@data$Consumption_2015)|is.infinite(HUC10_db@data$Consumption_2015),NA,HUC10_db@data$Consumption_2015)
  
  #---Year 2016----#
  HUC10_db@data$NetWB_2016<-(ifelse(is.na(HUC10_db@data$Discharges_2016),0,HUC10_db@data$Discharges_2016))-(ifelse(is.na(HUC10_db@data$Withdrawals_2016),0,HUC10_db@data$Withdrawals_2016))
  HUC10_db@data$NetWB_2016<-ifelse(is.na(HUC10_db@data$Discharges_2016)&is.na(HUC10_db@data$Withdrawals_2016),NA,HUC10_db@data$NetWB_2016)
  
  HUC10_db@data$Consumption_2016<-((ifelse(is.na(HUC10_db@data$Withdrawals_2016),0,HUC10_db@data$Withdrawals_2016))-(ifelse(is.na(HUC10_db@data$Discharges_2016),0,HUC10_db@data$Discharges_2016)))/(ifelse(is.na(HUC10_db@data$Withdrawals_2016),0,HUC10_db@data$Withdrawals_2016))
  HUC10_db@data$Consumption_2016<-ifelse(is.nan(HUC10_db@data$Consumption_2016)|is.infinite(HUC10_db@data$Consumption_2016),NA,HUC10_db@data$Consumption_2016)
  
  
  HUC10_glimpse<-HUC10_db@data
  assign("HUC10_glimpse",HUC10_glimpse,envir = .GlobalEnv)
  assign(paste0("HUC10",label),HUC10_db,envir = .GlobalEnv)
  
}

#---All Sectors---#

#-All Facilities-#
NWB_CU(HUC10_all,"_all")

#---Subsetted by Sector---#

#-All Facilities-#
NWB_CU(HUC10_energy,"_energy")
NWB_CU(HUC10_aq,"_aq")
NWB_CU(HUC10_commercial,"_commercial")
NWB_CU(HUC10_industrial,"_industrial")
NWB_CU(HUC10_municipal,"_municipal")

#---Non-Energy Sectors---#
#-All Facilities-#
NWB_CU(HUC10_nonenergy,"_nonenergy")

###########################################################################################################################################
#--------------------------------Long Term Average (2010-2017) Net Water Balance and Consumptive Use--------------------------------------#
# median is used for all averages
Ave_NWB_CU<- function(HUC10,label){
  
  #----Discharges-----#
  HUC10@data$Discharge_ave<-apply(HUC10@data[,c("Discharges_2010","Discharges_2011","Discharges_2012",
                                                            "Discharges_2013","Discharges_2014","Discharges_2015",
                                                            "Discharges_2016")],1,median,na.rm=T)
  
  #----Withdrawals----#
  HUC10@data$Withdrawal_ave<-apply(HUC10@data[,c("Withdrawals_2010","Withdrawals_2011","Withdrawals_2012",
                                                             "Withdrawals_2013","Withdrawals_2014","Withdrawals_2015",
                                                             "Withdrawals_2016")],1,median,na.rm=T)
  
  #----Net Water Balance-----#
  HUC10@data$NetWB_ave<-apply(HUC10@data[,c("NetWB_2010","NetWB_2011","NetWB_2012",
                                                        "NetWB_2013","NetWB_2014","NetWB_2015",
                                                        "NetWB_2016")],1,median,na.rm=T)
  
  #----Consumption----#
   HUC10@data$Consumption_ave<-apply(HUC10@data[,c("Consumption_2010","Consumption_2011","Consumption_2012",
                                                               "Consumption_2013","Consumption_2014","Consumption_2015",
                                                               "Consumption_2016")],1,median,na.rm=T)
   # Consumption based on long-term withdrawal and discharge
   HUC10@data$Consumption_ave2<-(HUC10@data$Withdrawal_ave - HUC10@data$Discharge_ave)/HUC10@data$Withdrawal_ave  
   
  HUC10@data$Consumption_ave<-ifelse(is.infinite(HUC10@data$Consumption_ave),NA,HUC10@data$Consumption_ave)
  HUC10@data$Consumption_ave2<-ifelse(is.infinite(HUC10@data$Consumption_ave2),NA,HUC10@data$Consumption_ave2)
  
  HUC10_glimpse<-HUC10@data
  assign(paste0("HUC10",label),HUC10,envir = .GlobalEnv)
  assign("HUC10_glimpse",HUC10_glimpse,envir = .GlobalEnv)
}

#---All Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC10_all,"")

#---Subsetted by Sector---#

#-All Facilities-#
Ave_NWB_CU(HUC10_energy,"_energy")
Ave_NWB_CU(HUC10_aq,"_aq")
Ave_NWB_CU(HUC10_commercial,"_commercial")
Ave_NWB_CU(HUC10_industrial,"_industrial")
Ave_NWB_CU(HUC10_municipal,"_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC10_nonenergy,"_nonenergy")

save.image("G:\\My Drive\\VT\\Research\\Virginia\\USGS_WUDR_ConsumptiveUse\\Analysis\\Spatial Analysis\\HUC10_JES.RData")

###########################################################################################################################################
#-----------------------------------------Plot maps of NWB from HUC10_Clipped------------------------------------------------------#

# load workspace if reopening script
load("G:\\My Drive\\VT\\Research\\Virginia\\USGS_WUDR_ConsumptiveUse\\Analysis\\Spatial Analysis\\HUC10_JES.RData")

setwd("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis/Spatial Analysis/Figures")

## datsets of interest - all data (HUC10), all non-energy (HUC10_nonenergy), and municipal only (HUC10_municipal)

## All  including energy
HUC10_Clipped<-gIntersection(HUC10,VA,id=as.character(HUC10@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10@data[as.character(HUC10@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

ppi<-300
png(file="NWB_all_HUC10.png",width=4*ppi,height=3*ppi,res=ppi)		
plot.data<-HUC10_Clipped@data$NetWB_ave
cut.seq<-quantile(plot.data, na.rm=TRUE, probs=c(0, 0.10, 0.20, 0.8, 0.9, 1.0) ) 
color.bins <- cut(plot.data, cut.seq, include.lowest = TRUE, dig.lab = 2)
n.col<-length(levels(color.bins))
lev<-levels(color.bins)	
lev2 <- gsub("\\,", " to ", lev)
lev3 <- gsub("\\]$", "", lev2)
lev4 <- gsub("\\(|\\)", "", lev3)
lev5 <- gsub("^\\[", "", lev4)
lev5[1]<- "-320 to -7.3"
lev5[5]<- "0.46 to 100"
my.levels <- lev5
Palette <- colorRampPalette(c('red','blue'))
colors<-Palette(n.col)[as.numeric(cut(plot.data, cut.seq, include.lowest = TRUE ))]
plot(HUC10_Clipped,col=colors,main="All Facilities",cex.main=0.8)
legend("topleft",fill=Palette(n.col),legend=my.levels,col=Palette(n.col),cex=0.55 )
dev.off()

## All non-energy
HUC10_Clipped<-gIntersection(HUC10_nonenergy,VA,id=as.character(HUC10_nonenergy@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10_nonenergy@data[as.character(HUC10_nonenergy@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

png(file="NWB_nonenergy_HUC10.png",width=4*ppi,height=3*ppi,res=ppi)		
plot.data<-HUC10_Clipped@data$NetWB_ave
cut.seq<-quantile(plot.data, na.rm=TRUE, probs=c(0, 0.10, 0.20, 0.8, 0.9, 1.0) ) 
color.bins <- cut(plot.data, cut.seq, include.lowest = TRUE, dig.lab = 2)
n.col<-length(levels(color.bins))
lev<-levels(color.bins)	
lev2 <- gsub("\\,", " to ", lev)
lev3 <- gsub("\\]$", "", lev2)
lev4 <- gsub("\\(|\\)", "", lev3)
lev5 <- gsub("^\\[", "", lev4)
lev5[1]<- "-140 to -5.9"
lev5[5]<- "0.46 to 100"
my.levels <- lev5
Palette <- colorRampPalette(c('red','blue'))
colors<-Palette(n.col)[as.numeric(cut(plot.data, cut.seq, include.lowest = TRUE ))]
plot(HUC10_Clipped,col=colors,main="All Nonenergy",cex.main=0.8)
legend("topleft",fill=Palette(n.col),legend=my.levels,col=Palette(n.col),cex=0.55 )
dev.off()

## Municipal
HUC10_Clipped<-gIntersection(HUC10_municipal,VA,id=as.character(HUC10_municipal@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10_municipal@data[as.character(HUC10_municipal@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

png(file="NWB_municipal_HUC10.png",width=4*ppi,height=3*ppi,res=ppi)		
plot.data<-HUC10_Clipped@data$NetWB_ave
cut.seq<-quantile(plot.data, na.rm=TRUE, probs=c(0, 0.10, 0.20, 0.8, 0.9, 1.0) ) 
cut.seq<-c(cut.seq[1:3],0,cut.seq[5:6])
color.bins <- cut(plot.data, cut.seq, include.lowest = TRUE, dig.lab = 2)
n.col<-length(levels(color.bins))
lev<-levels(color.bins)	
lev2 <- gsub("\\,", " to ", lev)
lev3 <- gsub("\\]$", "", lev2)
lev4 <- gsub("\\(|\\)", "", lev3)
lev5 <- gsub("^\\[", "", lev4)
lev5[1]<- "-140 to -3.4"
lev5[5]<- "1.4 to 110"
my.levels <- lev5
Palette <- colorRampPalette(c('red','blue'))
colors<-Palette(n.col)[as.numeric(cut(plot.data, cut.seq, include.lowest = TRUE ))]
plot(HUC10_Clipped,col=colors,main="Municipal",cex.main=0.8)
legend("topleft",fill=Palette(n.col),legend=my.levels,col=Palette(n.col),cex=0.55 )
dev.off()

###########################################################################################################################################
#-----------------------------------------Plot maps of withdrawal from HUC10_Clipped------------------------------------------------------#
## Do "hotspots" of withdrawal vary compared to hotspots of consumption?

## All  including energy
HUC10_Clipped<-gIntersection(HUC10,VA,id=as.character(HUC10@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10@data[as.character(HUC10@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

ppi<-300
png(file="Withdrawals_all_HUC10.png",width=4*ppi,height=3*ppi,res=ppi)		
plot.data<-HUC10_Clipped@data$Withdrawal_ave
cut.seq<-quantile(plot.data, na.rm=TRUE, probs=c(0, 0.20, 0.60, 0.80, 0.90, 1.0) ) 
color.bins <- cut(plot.data, cut.seq, include.lowest = TRUE, dig.lab = 2)
n.col<-length(levels(color.bins))
lev<-levels(color.bins)	
lev2 <- gsub("\\,", " to ", lev)
lev3 <- gsub("\\]$", "", lev2)
lev4 <- gsub("\\(|\\)", "", lev3)
lev5 <- gsub("^\\[", "", lev4)
lev5[5]<- "12 to 2000"
my.levels <- lev5
Palette <- colorRampPalette(c('blue','red'))
colors<-Palette(n.col)[as.numeric(cut(plot.data, cut.seq, include.lowest = TRUE ))]
plot(HUC10_Clipped,col=colors,main="All Facilities",cex.main=0.8)
legend("topleft",fill=Palette(n.col),legend=my.levels,col=Palette(n.col),cex=0.55 )
dev.off()

## All non-energy
HUC10_Clipped<-gIntersection(HUC10_nonenergy,VA,id=as.character(HUC10_nonenergy@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10_nonenergy@data[as.character(HUC10_nonenergy@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

png(file="Withdrawals_nonenergy_HUC10.png",width=4*ppi,height=3*ppi,res=ppi)		
plot.data<-HUC10_Clipped@data$Withdrawal_ave
cut.seq<-quantile(plot.data, na.rm=TRUE, probs=c(0, 0.20, 0.60, 0.80, 0.90, 1.0) ) 
color.bins <- cut(plot.data, cut.seq, include.lowest = TRUE, dig.lab = 2)
n.col<-length(levels(color.bins))
lev<-levels(color.bins)	
lev2 <- gsub("\\,", " to ", lev)
lev3 <- gsub("\\]$", "", lev2)
lev4 <- gsub("\\(|\\)", "", lev3)
lev5 <- gsub("^\\[", "", lev4)
lev5[5]<- "9.7 to 140"
my.levels <- lev5
Palette <- colorRampPalette(c('blue','red'))
colors<-Palette(n.col)[as.numeric(cut(plot.data, cut.seq, include.lowest = TRUE ))]
plot(HUC10_Clipped,col=colors,main="All Nonenergy",cex.main=0.8)
legend("topleft",fill=Palette(n.col),legend=my.levels,col=Palette(n.col),cex=0.55 )
dev.off()

## Municipal
HUC10_Clipped<-gIntersection(HUC10_municipal,VA,id=as.character(HUC10_municipal@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10_municipal@data[as.character(HUC10_municipal@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

png(file="Withdrawals_municipal_HUC10.png",width=4*ppi,height=3*ppi,res=ppi)	
plot.data<-HUC10_Clipped@data$Withdrawal_ave
cut.seq<-quantile(plot.data, na.rm=TRUE, probs=c(0, 0.20, 0.60, 0.80, 0.90, 1.0) ) 
color.bins <- cut(plot.data, cut.seq, include.lowest = TRUE, dig.lab = 2)
n.col<-length(levels(color.bins))
lev<-levels(color.bins)	
lev2 <- gsub("\\,", " to ", lev)
lev3 <- gsub("\\]$", "", lev2)
lev4 <- gsub("\\(|\\)", "", lev3)
lev5 <- gsub("^\\[", "", lev4)
lev5[5]<- "6.2 to 140"
my.levels <- lev5
Palette <- colorRampPalette(c('blue','red'))
colors<-Palette(n.col)[as.numeric(cut(plot.data, cut.seq, include.lowest = TRUE ))]
plot(HUC10_Clipped,col=colors,main="Municipal",cex.main=0.8)
legend("topleft",fill=Palette(n.col),legend=my.levels,col=Palette(n.col),cex=0.55 )
dev.off()

save.image("G:\\My Drive\\VT\\Research\\Virginia\\USGS_WUDR_ConsumptiveUse\\Analysis\\Spatial Analysis\\HUC10_JES.RData")

