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
library(DescTools)

options(scipen=999) #Disable scientific notation
options(digits = 9)
memory.limit(size=100000000)

###########################################################################################################################################
#-------------------------------------------HUC 6 and Virginia Shapefile Manipulation-----------------------------------------------------#

#Load databases and extract required layers
HUC8<-readOGR('G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/GithubFiles/GithubFiles/Spatial Analysis/HUC.gdb',layer='WBDHU8')
VA<-readOGR('G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/GithubFiles/GithubFiles/Spatial Analysis/EvapInputs.gdb',layer="VA")

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
  deliveries$Month<-as.numeric(substring(deliveries$tstime,6,7))
  deliveries$Date<-as.character(deliveries$tstime)
  deliveries$Date<-as.Date(deliveries$Date, format="%Y/%m/%d" )
  deliveries$Date<-format(deliveries$Date, format="%Y-%m")
  
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
  
  ####Monthly Sum Net Water In and Out for each HUC8####
  ###FROM Deliveries###
  delf.month<-dFrom@data
  delf.month<-delf.month[delf.month$interbasin==1,] #if interbasin is equal to 1, it is crossing boundaries---so just include those
  
  delf.month<-delf.month%>%
    dplyr::group_by(HUC8Name, Date)%>%
    dplyr::summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))#units in MGM to MGD
  
  delf.month$HUC8Name<-as.character(delf.month$HUC8Name)
  delf.month$HUC8Name[is.na(delf.month$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
  ###TO Deliveries###
  delt.month<-dTo@data
  delt.month<-delt.month[delt.month$interbasin==1,] #narrow down to deliveries happening across borders
  
  delt.month<-
    delt.month%>%
    dplyr::group_by(HUC8Name, Date)%>%
    dplyr::summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))#units in MGM to MGD
  
  delt.month$HUC8Name<-as.character(delt.month$HUC8Name)
  delt.month$HUC8Name[is.na(delt.month$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
  assign("delf",delf,envir=.GlobalEnv)
  assign("delt",delt,envir=.GlobalEnv)
  assign("delf.month",delf.month,envir=.GlobalEnv)
  assign("delt.month",delt.month,envir=.GlobalEnv)
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
  releases$Month<-as.numeric(substring(releases$tstime,6,7))
  releases$Date<-as.character(releases$tstime)
  releases$Date<-as.Date(releases$Date, format="%Y/%m/%d" )
  releases$Date<-format(releases$Date, format="%Y-%m")
  
  
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
  
  ####Monthly Sum Net Water In and Out for each HUC8####
  ###FROM Releases###
  relf.month<-rFrom@data
  relf.month<-relf.month[relf.month$interbasin==1,] #if interbasin is equal to 1, it is crossing boundaries---so just include those
  
  relf.month<-relf.month%>%
    dplyr::group_by(HUC8Name, Date)%>%
    dplyr::summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterout=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))#units in MGM to MGD
  
  relf.month$HUC8Name<-as.character(relf.month$HUC8Name)
  relf.month$HUC8Name[is.na(relf.month$HUC8Name)]<-'Fell Outside HUC8 Limits'
  
  ###TO Releases###
  relt.month<-rTo@data
  relt.month<-relt.month[relt.month$interbasin==1,] #narrow down to reliveries happening across borders
  
  relt.month<-
    relt.month%>%
    dplyr::group_by(HUC8Name, Date)%>%
    dplyr::summarize(HUC8=first(HUC8),interbasin=sum(interbasin), waterin=sum(as.numeric(tsvalue)/30.4166667,na.rm=T))#units in MGM to MGD
  
  relt.month$HUC8Name<-as.character(relt.month$HUC8Name)
  relt.month$HUC8Name[is.na(relt.month$HUC8Name)]<-'Fell Outside HUC8 Limits'

  assign("relf",relf,envir=.GlobalEnv)
  assign("relt",relt,envir=.GlobalEnv)
  assign("relf.month",relf.month,envir=.GlobalEnv)
  assign("relt.month",relt.month,envir=.GlobalEnv)
  assign("releases",deliveries,envir=.GlobalEnv)
  
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

# monthly data - just do 2016
transfers.month<- function(relf.month,delf.month,relt.month,delt.month){
  # copy spatial dataframe and remove annual data
  HUC8.month<-HUC8
  HUC8.month@data<-HUC8.month@data[,1:14]
  
  # extract monthly transfer data for 2016
  HUC8_waterout.month<-as.data.table(rbind(relf.month,delf.month))
  HUC8_waterout.month<-HUC8_waterout.month[, lapply(.SD,sum), by=list(HUC8Name, Date, HUC8), .SDcols=c(4,5)]
  HUC8_waterout.month$Year<-as.numeric(substring(HUC8_waterout.month$Date,1,4))
  HUC8_waterout.month$Month<-as.numeric(substring(HUC8_waterout.month$Date,6,7))
  HUC8_waterout.month<-HUC8_waterout.month[HUC8_waterout.month$Year == 2016, ] # trim to 2016
  
  HUC8_waterin.month<-as.data.table(rbind(relt.month,delt.month))
  HUC8_waterin.month<-HUC8_waterin.month[, lapply(.SD,sum), by=list(HUC8Name, Date, HUC8), .SDcols=c(4,5)]
  HUC8_waterin.month$Year<-as.numeric(substring(HUC8_waterin.month$Date,1,4))
  HUC8_waterin.month$Month<-as.numeric(substring(HUC8_waterin.month$Date,6,7))
  HUC8_waterin.month<-HUC8_waterin.month[HUC8_waterin.month$Year == 2016, ] # trim to 2016
  
  
  #---January---#
  HUC8.month@data$waterout_1<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==1],
                                       -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==1]
                                       [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==1])],NA)
  HUC8.month@data$waterin_1<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==1],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==1]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==1])],NA)
  HUC8.month@data$transferred_1<- (rowSums(HUC8.month@data[,(15:16)],na.rm=T))
  HUC8.month@data$transferred_1<-ifelse(is.na(HUC8.month@data$waterin_1)&is.na(HUC8.month@data$waterout_1),NA,HUC8.month@data$transferred_1)
  
  #---February---#
  HUC8.month@data$waterout_2<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==2],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==2]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==2])],NA)
  HUC8.month@data$waterin_2<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==2],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==2]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==2])],NA)
  HUC8.month@data$transferred_2<- (rowSums(HUC8.month@data[,(18:19)],na.rm=T))
  HUC8.month@data$transferred_2<-ifelse(is.na(HUC8.month@data$waterin_2)&is.na(HUC8.month@data$waterout_2),NA,HUC8.month@data$transferred_2)
  
  #---March---#
  HUC8.month@data$waterout_3<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==3],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==3]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==3])],NA)
  HUC8.month@data$waterin_3<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==3],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==3]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==3])],NA)
  HUC8.month@data$transferred_3<- (rowSums(HUC8.month@data[,(21:22)],na.rm=T))
  HUC8.month@data$transferred_3<-ifelse(is.na(HUC8.month@data$waterin_3)&is.na(HUC8.month@data$waterout_3),NA,HUC8.month@data$transferred_3)
  
  #---April---#
  HUC8.month@data$waterout_4<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==4],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==4]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==4])],NA)
  HUC8.month@data$waterin_4<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==4],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==4]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==4])],NA)
  HUC8.month@data$transferred_4<- (rowSums(HUC8.month@data[,(24:25)],na.rm=T))
  HUC8.month@data$transferred_4<-ifelse(is.na(HUC8.month@data$waterin_4)&is.na(HUC8.month@data$waterout_4),NA,HUC8.month@data$transferred_4)
  
  #---May---#
  HUC8.month@data$waterout_5<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==5],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==5]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==5])],NA)
  HUC8.month@data$waterin_5<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==5],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==5]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==5])],NA)
  HUC8.month@data$transferred_5<- (rowSums(HUC8.month@data[,(27:28)],na.rm=T))
  HUC8.month@data$transferred_5<-ifelse(is.na(HUC8.month@data$waterin_5)&is.na(HUC8.month@data$waterout_5),NA,HUC8.month@data$transferred_5)

  #---June---#
  HUC8.month@data$waterout_6<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==6],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==6]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==6])],NA)
  HUC8.month@data$waterin_6<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==6],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==6]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==6])],NA)
  HUC8.month@data$transferred_6<- (rowSums(HUC8.month@data[,(30:31)],na.rm=T))
  HUC8.month@data$transferred_6<-ifelse(is.na(HUC8.month@data$waterin_6)&is.na(HUC8.month@data$waterout_6),NA,HUC8.month@data$transferred_6)
  
  #---July---#
  HUC8.month@data$waterout_7<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==7],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==7]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==7])],NA)
  HUC8.month@data$waterin_7<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==7],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==7]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==7])],NA)
  HUC8.month@data$transferred_7<- (rowSums(HUC8.month@data[,(33:34)],na.rm=T))
  HUC8.month@data$transferred_7<-ifelse(is.na(HUC8.month@data$waterin_7)&is.na(HUC8.month@data$waterout_7),NA,HUC8.month@data$transferred_7)
  
  #---August---#
  HUC8.month@data$waterout_8<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==8],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==8]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==8])],NA)
  HUC8.month@data$waterin_8<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==8],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==8]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==8])],NA)
  HUC8.month@data$transferred_8<- (rowSums(HUC8.month@data[,(36:37)],na.rm=T))
  HUC8.month@data$transferred_8<-ifelse(is.na(HUC8.month@data$waterin_8)&is.na(HUC8.month@data$waterout_8),NA,HUC8.month@data$transferred_8)
  
  #---September---#
  HUC8.month@data$waterout_9<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==9],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==9]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==9])],NA)
  HUC8.month@data$waterin_9<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==9],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==9]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==9])],NA)
  HUC8.month@data$transferred_9<- (rowSums(HUC8.month@data[,(39:40)],na.rm=T))
  HUC8.month@data$transferred_9<-ifelse(is.na(HUC8.month@data$waterin_9)&is.na(HUC8.month@data$waterout_9),NA,HUC8.month@data$transferred_9)
  
  #---October---#
  HUC8.month@data$waterout_10<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==10],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==10]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==10])],NA)
  HUC8.month@data$waterin_10<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==10],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==10]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==10])],NA)
  HUC8.month@data$transferred_10<- (rowSums(HUC8.month@data[,(42:43)],na.rm=T))
  HUC8.month@data$transferred_10<-ifelse(is.na(HUC8.month@data$waterin_10)&is.na(HUC8.month@data$waterout_10),NA,HUC8.month@data$transferred_10)
  
  #---November---#
  HUC8.month@data$waterout_11<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==11],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==11]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==11])],NA)
  HUC8.month@data$waterin_11<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==11],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==11]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==11])],NA)
  HUC8.month@data$transferred_11<- (rowSums(HUC8.month@data[,(45:46)],na.rm=T))
  HUC8.month@data$transferred_11<-ifelse(is.na(HUC8.month@data$waterin_11)&is.na(HUC8.month@data$waterout_11),NA,HUC8.month@data$transferred_11)
  
  #---December---#
  HUC8.month@data$waterout_12<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==12],
                                     -HUC8_waterout.month$waterout[HUC8_waterout.month$Month==12]
                                     [match(HUC8.month@data$HUC8,HUC8_waterout.month$HUC8[HUC8_waterout.month$Month==12])],NA)
  HUC8.month@data$waterin_12<-ifelse(HUC8.month@data$HUC8 %in% HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==12],
                                    HUC8_waterin.month$waterin[HUC8_waterin.month$Month==12]
                                    [match(HUC8.month@data$HUC8,HUC8_waterin.month$HUC8[HUC8_waterin.month$Month==12])],NA)
  HUC8.month@data$transferred_12<- (rowSums(HUC8.month@data[,(48:49)],na.rm=T))
  HUC8.month@data$transferred_12<-ifelse(is.na(HUC8.month@data$waterin_12)&is.na(HUC8.month@data$waterout_12),NA,HUC8.month@data$transferred_12)
  
  HUC8_Transfers.month<-data.frame(HUC8Name=HUC8.month@data$Name,
                             HUC8=HUC8.month@data$HUC8,
                             Transfers_1_MGD=HUC8.month@data$transferred_1,
                             Transfers_2_MGD=HUC8.month@data$transferred_2,
                             Transfers_3_MGD=HUC8.month@data$transferred_3,
                             Transfers_4_MGD=HUC8.month@data$transferred_4,
                             Transfers_5_MGD=HUC8.month@data$transferred_5,
                             Transfers_6_MGD=HUC8.month@data$transferred_6,
                             Transfers_7_MGD=HUC8.month@data$transferred_7,
                             Transfers_8_MGD=HUC8.month@data$transferred_8,
                             Transfers_9_MGD=HUC8.month@data$transferred_9,
                             Transfers_10_MGD=HUC8.month@data$transferred_10,
                             Transfers_11_MGD=HUC8.month@data$transferred_11,
                             Transfers_12_MGD=HUC8.month@data$transferred_12)
  
  assign("HUC8.month",HUC8.month,envir = .GlobalEnv)
  assign("HUC8_Transfers.month",HUC8_Transfers.month,envir = .GlobalEnv)
  
  #rm(relf.month,delf.month,relt.month,delt.month,envir = .GlobalEnv)
  
}
transfers.month(relf.month,delf.month,relt.month,delt.month)

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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #-- Add date column (year/month) to discharge_db
  discharge_db@data$Month<-as.numeric(substring(as.character(discharge_db@data$Date),6,7))
  discharge_db@data$Date<-as.character(discharge_db@data$Date)
  discharge_db@data$Date<-as.Date(discharge_db@data$Date, format="%Y-%m-%d" )
  discharge_db@data$Date<-format(discharge_db@data$Date, format="%Y-%m")
  
  ## Nonmonthly reports
  #discharge_db.test<-as.data.frame(discharge_db@data)
  #discharge_db_nonmonthly<-discharge_db.test[discharge_db.test$Mon_Reported < 12,]
  #discharge_db_nonmonthly_2016<-subset(discharge_db_nonmonthly,Year == 2016)
  
  #--Sum Discharges in the HUC 6 Watersheds--#
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
  
  Outfall_Discharges.month<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Date)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  HUC8_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  HUC8_Discharges.month<-Outfall_Discharges.month%>%
    dplyr::group_by(HUC8Name,Date)%>%
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
  assign(paste0("HUC8_Discharges.month",label),HUC8_Discharges.month,envir = .GlobalEnv)
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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #-- Add date column (year/month) to discharge_db
  discharge_db@data$Month<-as.numeric(substring(as.character(discharge_db@data$Date),6,7))
  discharge_db@data$Date<-as.character(discharge_db@data$Date)
  discharge_db@data$Date<-as.Date(discharge_db@data$Date, format="%Y-%m-%d" )
  discharge_db@data$Date<-format(discharge_db@data$Date, format="%Y-%m")
  
  #--Sum Discharges in the HUC 6 Watersheds--#
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
  
  Outfall_Discharges.month<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Date)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  HUC8_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  HUC8_Discharges.month<-Outfall_Discharges.month%>%
    dplyr::group_by(HUC8Name,Date)%>%
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
  assign(paste0("HUC8_Discharges.month",label),HUC8_Discharges.month,envir = .GlobalEnv)
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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(discharge_db,HUC8_Overlay)
  discharge_db@data$HUC8<-HUC8_Facilities$HUC8
  discharge_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #-- Add date column (year/month) to discharge_db
  discharge_db@data$Month<-as.numeric(substring(as.character(discharge_db@data$Date),6,7))
  discharge_db@data$Date<-as.character(discharge_db@data$Date)
  discharge_db@data$Date<-as.Date(discharge_db@data$Date, format="%Y-%m-%d" )
  discharge_db@data$Date<-format(discharge_db@data$Date, format="%Y-%m")
  
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
  
  Outfall_Discharges.month<-discharge_db@data%>%
    dplyr::group_by(OutfallID,Date)%>%
    dplyr::summarise(Facility.ID=first(VPDES.Facility.ID),
                     Facility_Name=first(VPDES.Name),
                     Mon_Reported=first(Mon_Reported),
                     Discharges_MGD=sum(Discharges_MGD, na.rm=T),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))%>%arrange(desc(Discharges_MGD))
  
  HUC8_Discharges<-Outfall_Discharges%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Discharge_MGD=sum(Discharges_MGD,na.rm=T))
  
  HUC8_Discharges.month<-Outfall_Discharges.month%>%
    dplyr::group_by(HUC8Name,Date)%>%
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
  assign(paste0("HUC8_Discharges.month",label),HUC8_Discharges.month,envir = .GlobalEnv)
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

## Format dates and add month column
summary(VWUDS_2010_2017$Date)
orig.date<-VWUDS_2010_2017$Date
VWUDS_2010_2017$Date<-format(VWUDS_2010_2017$Date, format="%Y-%m")
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
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  Facility_Withdrawals.month<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Date)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  HUC8_Withdrawals.month<-Facility_Withdrawals.month%>%
    dplyr::group_by(HUC8Name,Date)%>%
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
  assign(paste0("HUC8_Withdrawals.month",label),HUC8_Withdrawals.month,envir = .GlobalEnv)
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
  
  Facility_Withdrawals.month<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Date)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  HUC8_Withdrawals.month<-Facility_Withdrawals.month%>%
    dplyr::group_by(HUC8Name,Date)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  "withdrawal_db.test<-withdrawal_db.test%>%
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
                     Use.Type=first(Use.Type))%>%arrange(desc(Withdrawals_MGD))"
  
  assign(paste0("HUC8_Withdrawals",label),HUC8_Withdrawals,envir = .GlobalEnv)
  assign(paste0("HUC8_Withdrawals.month",label),HUC8_Withdrawals.month,envir = .GlobalEnv)
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
  
  #--Overlay with HUC8 Shapefile--#
  HUC8_Facilities<-over(withdrawal_db,HUC8_Overlay)
  withdrawal_db@data$HUC8<-HUC8_Facilities$HUC8
  withdrawal_db@data$HUC8Name<-HUC8_Facilities$HUC8Name
  
  #--Sum Discharges in the HUC 6 Watersheds--#
  withdrawal_db.test<-as.data.frame(withdrawal_db@data)
  
  Facility_Withdrawals<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Year)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  Facility_Withdrawals.month<-withdrawal_db@data%>%
    dplyr::group_by(VWUDS.Facility.ID,Date)%>%
    dplyr::summarise(Facility_Name=first(VWUDS.Name),
                     Mon_Reported=first(Mon_Reported),
                     Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T),
                     Sector=first(Use.Type),
                     HUC8Name=first(HUC8Name),
                     HUC8=first(HUC8))
  
  HUC8_Withdrawals<-Facility_Withdrawals%>%
    dplyr::group_by(HUC8Name,Year)%>%
    dplyr::summarise(HUC8=first(HUC8),
                     Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  
  HUC8_Withdrawals.month<-Facility_Withdrawals.month%>%
    dplyr::group_by(HUC8Name,Date)%>%
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
  assign(paste0("HUC8_Withdrawals.month",label),HUC8_Withdrawals.month,envir = .GlobalEnv)
  assign(paste0("VWUDS.test",label),withdrawal_db.test,envir=.GlobalEnv)
}
nonenergy_withdrawal(VWUDS_2010_2017,"_nonenergy")

###########################################################################################################################################
#------------------------------------------Apply Withdrawals and Discharges into HUC8 Spatial Dataframe-----------------------------------#

## Annual
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

rm(HUC8_Discharges,HUC8_Discharges_aq, HUC8_Discharges_commercial,HUC8_Discharges_energy,HUC8_Discharges_industrial,
   HUC8_Withdrawals,HUC8_Withdrawals_aq,HUC8_Withdrawals_ag,HUC8_Withdrawals_commercial,HUC8_Withdrawals_energy,HUC8_Withdrawals_industrial,
   HUC8_Discharges_municipal,HUC8_Discharges_nonenergy,HUC8_Withdrawals_municipal,HUC8_Withdrawals_nonenergy)

#######################################################################
## Monthly for 2016 (add columns to HUC8.month spatial dataframe)
#######################################################################
HUC8_discharge_withdrawal_monthly<- function(HUC8_Discharges_db,HUC8_Withdrawals_db,label){
  
  # Extract date information and subset to 2016 years
  withdrawal.yr<-as.numeric(substring(HUC8_Withdrawals_db$Date,1,4))
  discharge.yr<-as.numeric(substring(HUC8_Discharges_db$Date,1,4))
  withdrawal.2016<-HUC8_Withdrawals_db[withdrawal.yr == 2016 ,]
  discharge.2016<-HUC8_Discharges_db[discharge.yr == 2016 ,]
  withdrawal.2016$mnth<-as.numeric(substring(withdrawal.2016$Date,6,7))
  discharge.2016$mnth<-as.numeric(substring(discharge.2016$Date,6,7))
  
  # Loop through months and apply to spatial dataframe
  for (m in 1:12){
    m.discharge<-ifelse(HUC8.month@data$HUC8 %in% discharge.2016$HUC8[discharge.2016$mnth==m],
                 discharge.2016$Discharge_MGD[discharge.2016$mnth==m]
                 [match(HUC8.month@data$HUC8,discharge.2016$HUC8[discharge.2016$mnth==m])],NA)
    m.withdrawal<-ifelse(HUC8.month@data$HUC8 %in% withdrawal.2016$HUC8[withdrawal.2016$mnth==m],
                      withdrawal.2016$Withdrawals_MGD[withdrawal.2016$mnth==m]
                      [match(HUC8.month@data$HUC8,withdrawal.2016$HUC8[withdrawal.2016$mnth==m])],NA)
    HUC8.month@data$Discharges <- m.discharge
    HUC8.month@data$Withdrawals <- m.withdrawal
    colname.d<-paste("Discharges_",as.character(m),sep="")
    colname.w<-paste("Withdrawals_",as.character(m),sep="")
    colnames(HUC8.month@data)[dim(HUC8.month@data)[2] - 1]<- colname.d
    colnames(HUC8.month@data)[dim(HUC8.month@data)[2] ]<- colname.w
  }
 
  assign(paste0("HUC8.month",label),HUC8.month,envir = .GlobalEnv)
}

#--All Sectors--#
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month,HUC8_Withdrawals.month,"_all")

#--Non-Energy--#
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month_nonenergy,HUC8_Withdrawals.month_nonenergy,"_nonenergy")
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month_energy,HUC8_Withdrawals.month_energy,"_energy")
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month_aq,HUC8_Withdrawals.month_aq,"_aq")
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month_commercial,HUC8_Withdrawals.month_commercial,"_commercial")
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month_industrial,HUC8_Withdrawals.month_industrial,"_industrial")
HUC8_discharge_withdrawal_monthly(HUC8_Discharges.month_municipal,HUC8_Withdrawals.month_municipal,"_municipal")


rm(HUC8_Discharges.month,HUC8_Discharges.month_aq,HUC8_Discharges.month_commercial,HUC8_Discharges.month_energy,HUC8_Discharges.month_industrial,
   HUC8_Withdrawals.month,HUC8_Withdrawals.month_aq,HUC8_Withdrawals.month_ag,HUC8_Withdrawals.month_commercial,HUC8_Withdrawals.month_energy,HUC8_Withdrawals.month_industrial,
   HUC8_Discharges.month_municipal,HUC8_Discharges.month_nonenergy,HUC8_Withdrawals.month_municipal,HUC8_Withdrawals.month_nonenergy)

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

#---Subsetted by Sector---#

#-All Facilities-#
NWB_CU(HUC8_energy,"_energy")
NWB_CU(HUC8_aq,"_aq")
NWB_CU(HUC8_commercial,"_commercial")
NWB_CU(HUC8_industrial,"_industrial")
NWB_CU(HUC8_municipal,"_municipal")

#---Non-Energy Sectors---#
#-All Facilities-#
NWB_CU(HUC8_nonenergy,"_nonenergy")


#######################################
## Calculate Seasonal Consumptive Use
#######################################

warm.wd<-c("Withdrawals_4","Withdrawals_5","Withdrawals_6","Withdrawals_7","Withdrawals_8","Withdrawals_9")
cool.wd<-c("Withdrawals_1","Withdrawals_2","Withdrawals_3","Withdrawals_10","Withdrawals_11","Withdrawals_12")
warm.dc<-c("Discharges_4","Discharges_5","Discharges_6","Discharges_7","Discharges_8","Discharges_9")
cool.dc<-c("Discharges_1","Discharges_2","Discharges_3","Discharges_10","Discharges_11","Discharges_12")

CU.season<- function(HUC8_db,label){
  HUC8.wd.warm<-HUC8_db@data[, colnames(HUC8_db@data) %in% warm.wd ]
  HUC8.wd.cool<-HUC8_db@data[, colnames(HUC8_db@data) %in% cool.wd ]
  HUC8.dc.warm<-HUC8_db@data[, colnames(HUC8_db@data) %in% warm.dc ]
  HUC8.dc.cool<-HUC8_db@data[, colnames(HUC8_db@data) %in% cool.dc ]
  
  HUC8_db@data$Withdrawals_warm<-apply(HUC8.wd.warm, 1, mean, na.rm=TRUE)
  HUC8_db@data$Withdrawals_cool<-apply(HUC8.wd.cool , 1, mean, na.rm=TRUE)
  
  HUC8_db@data$Discharges_warm<-apply(HUC8.dc.warm , 1, mean, na.rm=TRUE)
  HUC8_db@data$Discharges_cool<-apply(HUC8.dc.cool , 1, mean, na.rm=TRUE)
  
  HUC8_db@data$CU_warm<-(HUC8_db@data$Withdrawals_warm - HUC8_db@data$Discharges_warm)/HUC8_db@data$Withdrawals_warm
  HUC8_db@data$CU_cool<-(HUC8_db@data$Withdrawals_cool - HUC8_db@data$Discharges_cool)/HUC8_db@data$Withdrawals_cool
  
  HUC8_db@data$CU_annual<- ((HUC8_db@data$Withdrawals_warm + HUC8_db@data$Withdrawals_cool) - 
                           (HUC8_db@data$Discharges_warm + HUC8_db@data$Discharges_cool) )/
                           (HUC8_db@data$Withdrawals_warm + HUC8_db@data$Withdrawals_cool)
  
  assign(paste0("HUC8.month",label),HUC8_db,envir = .GlobalEnv)
 }
  
#-All Facilities-#
CU.season(HUC8.month_all,"_all")

#---Subsetted by Sector---#
CU.season(HUC8.month_energy,"_energy")
CU.season(HUC8.month_aq,"_aq")
CU.season(HUC8.month_commercial,"_commercial")
CU.season(HUC8.month_industrial,"_industrial")
CU.season(HUC8.month_municipal,"_municipal")
CU.season(HUC8.month_nonenergy,"_nonenergy")

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
   
   # Consumption based on long-term withdrawal and discharge
   HUC8@data$Consumption_ave2<-(HUC8@data$Withdrawal_ave - HUC8@data$Discharge_ave)/HUC8@data$Withdrawal_ave  
   
  HUC8@data$Consumption_ave<-ifelse(is.infinite(HUC8@data$Consumption_ave),NA,HUC8@data$Consumption_ave)
  HUC8@data$Consumption_ave2<-ifelse(is.infinite(HUC8@data$Consumption_ave2),NA,HUC8@data$Consumption_ave2)
  
  HUC8_glimpse<-HUC8@data
  assign(paste0("HUC8",label),HUC8,envir = .GlobalEnv)
  assign("HUC8_glimpse",HUC8_glimpse,envir = .GlobalEnv)
}

#---All Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC8_all,"")

#---Subsetted by Sector---#

#-All Facilities-#
Ave_NWB_CU(HUC8_energy,"_energy")
Ave_NWB_CU(HUC8_aq,"_aq")
Ave_NWB_CU(HUC8_commercial,"_commercial")
Ave_NWB_CU(HUC8_industrial,"_industrial")
Ave_NWB_CU(HUC8_municipal,"_municipal")

#---Non-Energy Sectors---#

#-All Facilities-#
Ave_NWB_CU(HUC8_nonenergy,"_nonenergy")

## Comparing results where average consumption is the median of annual consumption values 2010-2016
##  with consumption calculated based on average withdrawal (2010-2016) and discharge (2010-2016)
##  Using median of annual consumption values results in many values of 1.0 - years where reported withdrawal 
##  was much higher than reported discharge. However the actual volume of withdrawals were not that high - 
##  hence the much lower estimates of consumption. These seem to be skewing the results, so we will report
##  long-term consumption based on the process of first calculating average withdrawal and discharge over the 
##  study period, and then calculating consumption as (W-D)/W for that period. 

## Compare some results
##################################

comp.summary<-data.frame(All.Nonenergy=rep(NA,9),Energy=rep(NA,9),Industrial=rep(NA,9),Commercial=rep(NA,9),Aquaculture=rep(NA,9),Municipal=rep(NA,9))
row.names(comp.summary) <- c("correlation", "mean.MM", "mean.JS", "Q25.MM","Q25.JS", "median.MM","median.JS", "Q75.MM","Q75.JS")

JS.list<-cbind(HUC8.month_nonenergy@data$CU_annual, HUC8.month_energy@data$CU_annual,HUC8.month_industrial@data$CU_annual,
               HUC8.month_commercial@data$CU_annual, HUC8.month_aq@data$CU_annual, HUC8.month_municipal@data$CU_annual)
MM.list<-cbind(HUC8_nonenergy@data$Consumption_2016, HUC8_energy@data$Consumption_2016, HUC8_industrial@data$Consumption_2016,
               HUC8_commercial@data$Consumption_2016, HUC8_aq@data$Consumption_2016, HUC8_municipal@data$Consumption_2016)              

for (s in 1:6){
  comp.table<-data.frame(MM=MM.list[,s], JS=JS.list[,s])
  comp.table$MM<- ifelse(is.infinite(comp.table$MM),NA, comp.table$MM)
  comp.table$JS<- ifelse(is.infinite(comp.table$JS),NA, comp.table$JS)
  comp.table$MM<- ifelse(is.na(comp.table$JS),NA, comp.table$MM)
  comp.table$JS<- ifelse(is.na(comp.table$MM),NA, comp.table$JS)
  comp.summary[1,s]<-cor(comp.table$MM[!is.na(comp.table$MM)], comp.table$JS[!is.na(comp.table$JS)])
  comp.summary[2,s]<-mean(comp.table$MM, na.rm=TRUE)
  comp.summary[3,s]<-mean(comp.table$JS, na.rm=TRUE)
  comp.summary[c(4,6,8),s]<-quantile(comp.table$MM, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  comp.summary[c(5,7,9),s]<-quantile(comp.table$JS, probs=c(0.25,0.5,0.75), na.rm=TRUE)
}

## Look at what is going on in commercial sector - it's single value driving the distance
comp.table<-data.frame(HUC.name= HUC8.month_commercial@data$Name, HUC=HUC8.month_commercial@data$HUC8, 
                       MM=HUC8_commercial@data$Consumption_2016, JS=HUC8.month_commercial@data$CU_annual)
# York HUC is causing discrepancy (HUC 02080107)
comp.table$JS<- ifelse(is.infinite(comp.table$JS),NA, comp.table$JS)
comp.table$MM<- ifelse(is.infinite(comp.table$MM),NA, comp.table$MM)
comp.table$MM<- ifelse(is.na(comp.table$JS),NA, comp.table$MM)
comp.table$JS<- ifelse(is.na(comp.table$MM),NA, comp.table$JS)
comp.table<-comp.table[-30,]
cor(comp.table$MM[!is.na(comp.table$MM)], comp.table$JS[!is.na(comp.table$JS)]) #now correlation is 0.97

York.Data.MM<-HUC8_commercial@data[30,]
York.Data.JS<-HUC8.month_commercial@data[30,]

## Reload withdrawal processing to get individual records for each HUC
sector_withdrawal(VWUDS_2010_2017,"Commercial","_commercial") 
york.monthly<-subset(HUC8_Withdrawals.month_commercial, HUC8Name == "York")
york.monthly<-york.monthly[73:84 ,]

york.wd<-subset(VWUDS.test_commercial, HUC8Name== "York")
york.wd<-subset(york.wd, Year == 2016)
york.wd.MG<-sum(york.wd$Million.Gallons.Month)  # sum of monthly withdrawals in millions of gallons - 70.792
70.792/365  # 0.19 MGD
sum(york.wd$Withdrawals_MGD)  # 2.31 MGD

## There appears to be some issue arising in:
#     the conversion to MGD from MGM, 
#     the number of reporting monthsshould be 12 for all VWUDS facilities, but some have 6
#     the creation of annual totals.
# However, theres nothing to suggest that withdrawals are 1/40th of discharges, so we'll move forward with JS values. 

save.image("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis/SpatialAnalysis/VA_HUC8_Consumptive_Use_Season_ws.RData")

#load("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis/SpatialAnalysis/VA_HUC8_Consumptive_Use_Season_ws.RData")

###########################################################################################################################################
#-----------------------------------------Summary of cross-sectional variability across HUC-8s------------------------------------------------------#
#----------------------------------------- 2016 data only -------------------------------------------------------------------------

# Initialize table to store medians and 25/75 percentile values
xSecSummary<-data.frame(All.Nonenergy=rep(NA,4),Energy=rep(NA,4),Industrial=rep(NA,4),Commercial=rep(NA,4),Aquaculture=rep(NA,4),Municipal=rep(NA,4))
row.names(xSecSummary) <- c("Annual","Warm Season", "Cool Season","P-value")
sect.list.seasonal<-list(HUC8.month_nonenergy,HUC8.month_energy,HUC8.month_industrial,HUC8.month_commercial,HUC8.month_aq,HUC8.month_municipal)

# Initialize dataframes to create boxplot
fill<-rep(NA, dim(HUC8.month_nonenergy@data)[1])
bp.df <- data.frame(Nonenergy.Annual = fill, Nonenergy.Warm=fill, Nonenergy.Cool=fill,
                    Energy.Annual = fill, Energy.Warm=fill, Energy.Cool=fill,
                    Industrial.Annual = fill, Industrial.Warm=fill, Industrial.Cool=fill,
                    Commercial.Annual = fill, Commercial.Warm=fill, Commercial.Cool=fill,
                    Aquaculture.Annual = fill, Aquaculture.Warm=fill, Aquaculture.Cool=fill,
                    Municipal.Annual = fill, Municipal.Warm=fill, Municipal.Cool=fill)

for (s in 1:length(sect.list.seasonal)){
  # annual
  sp.table<-sect.list.seasonal[[s]]
  sp.table<-sp.table@data
  sp.table<-sp.table[, c(11,12,51:81)]
  values<-quantile(sp.table$CU_annual, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  xSecSummary[1,s]<-paste(round(values[2],2), " (", round(values[1],2), " - ", round(values[3],2), ")", sep="")
  col.id<-(s-1)*3+1
  bp.df[,col.id] <-sp.table$CU_annual
  
  # warm season
  values<-quantile(sp.table$CU_warm, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  xSecSummary[2,s]<-paste(round(values[2],2), " (", round(values[1],2), " - ", round(values[3],2), ")", sep="")
  col.id<-(s-1)*3+2
  bp.df[,col.id] <-sp.table$CU_warm
  
  # cool season
  values<-quantile(sp.table$CU_cool, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  xSecSummary[3,s]<-paste(round(values[2],2), " (", round(values[1],2), " - ", round(values[3],2), ")", sep="")
  col.id<-(s-1)*3+3
  bp.df[,col.id] <-sp.table$CU_cool
  
  # Wilcox Rank Sum Test (nonparameteric shift in distribution)
  test.table<-cbind(sp.table$CU_warm, sp.table$CU_cool)
  test.table<-test.table[!(is.infinite(test.table[,1]) | is.infinite(test.table[,2]) ) ,]
  test.table<-test.table[!(is.na(test.table[,1]) | is.na(test.table[,2]) ) ,]
  wt<-wilcox.test(test.table[,1], test.table[,2], alternative = "two.sided", paired=TRUE)
  xSecSummary[4,s]<- wt$p.value

}

write.csv(xSecSummary, "HUC8_XsecSummary.csv", row.names=TRUE)

## Revised xSec Summary to include confidence values around median
xSecRevised<-data.frame(All.Nonenergy=rep(NA,4),Energy=rep(NA,4),Industrial=rep(NA,4),Commercial=rep(NA,4),Aquaculture=rep(NA,4),Municipal=rep(NA,4))
row.names(xSecRevised) <- c("Annual","Warm Season", "Cool Season","P-value")
for (s in 1:length(sect.list.seasonal)){
  # annual
  sp.table<-sect.list.seasonal[[s]]
  sp.table<-sp.table@data
  sp.table<-sp.table[, c(11,12,51:81)]
  values<-MedianCI(sp.table$CU_annual,conf.level=0.90, na.rm=TRUE)
  xSecRevised[1,s]<-paste(round(values[1],2), " (", round(values[2],2), " - ", round(values[3],2), ")", sep="")
  # warm season
  values<-MedianCI(sp.table$CU_warm, conf.level=0.90, na.rm=TRUE)
  xSecRevised[2,s]<-paste(round(values[1],2), " (", round(values[2],2), " - ", round(values[3],2), ")", sep="")
  # cool season
  values<-MedianCI(sp.table$CU_cool, conf.level=0.90, na.rm=TRUE)
  xSecRevised[3,s]<-paste(round(values[1],2), " (", round(values[2],2), " - ", round(values[3],2), ")", sep="")

  # Wilcox Rank Sum Test (nonparameteric shift in distribution)
  test.table<-cbind(sp.table$CU_warm, sp.table$CU_cool)
  test.table<-test.table[!(is.infinite(test.table[,1]) | is.infinite(test.table[,2]) ) ,]
  test.table<-test.table[!(is.na(test.table[,1]) | is.na(test.table[,2]) ) ,]
  wt<-wilcox.test(test.table[,1], test.table[,2], alternative = "two.sided", paired=TRUE)
  xSecRevised[4,s]<- wt$p.value
}
write.csv(xSecRevised, "HUC8_XsecRevised.csv", row.names=TRUE)

## Summary table with quantile values
quant.summary <- data.frame(All.NonEnergy_25=rep(NA,3), All.NonEnergy_50=rep(NA,3), All.NonEnergy_75=rep(NA,3),
                            Energy_25=rep(NA,3), Energy_50=rep(NA,3), Energy_75=rep(NA,3),
                            Industrial_25=rep(NA,3),Industial_50=rep(NA,3),Industrial_75=rep(NA,3),
                            Commercial_25=rep(NA,3),Commercial_50=rep(NA,3), Commercial_75=rep(NA,3),
                            Aquaculture_25=rep(NA,3),Aquaculture_50=rep(NA,3), Aquaculture_75=rep(NA,3),
                            Municipal_25=rep(NA,3),Municipal_50=rep(NA,3), Municipal_75=rep(NA,3))
row.names(quant.summary)<- c("Annual", "Warm Season", "Cool Season")
for (s in 1:length(sect.list.seasonal)){
  sp.table<-sect.list.seasonal[[s]]
  sp.table<-sp.table@data
  sp.table<-sp.table[, c(11,12,51:81)]
  cols<-c((s*3-2):(s*3))
  # annual
  an.values<-quantile(sp.table$CU_annual, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  # warm season
  warm.values<-quantile(sp.table$CU_warm, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  # cool season
  cool.values<-quantile(sp.table$CU_cool, probs=c(0.25,0.5,0.75), na.rm=TRUE)
  quant.summary[1,cols] <- an.values 
  quant.summary[2,cols] <- warm.values 
  quant.summary[3,cols] <- cool.values 
}
write.csv(quant.summary, "HUC8_QuantSummary.csv", row.names=TRUE)



## Create Boxplot
min.val<- -1
for (c in 1:dim(bp.df)[2]){
  IQR<-quantile(bp.df[,c], probs=0.75, na.rm=TRUE) -
    quantile(bp.df[,c], probs=0.25, na.rm=TRUE)
  min.val <- min(min.val, quantile(bp.df$Nonenergy.Annual, probs=0.25, na.rm=TRUE) - 1.5*IQR)
}
y.bounds <-c(min.val, 1)

boxplot(bp.df$Nonenergy.Annual, ylim=y.bounds)
## Why is annual value for median consumption in industry lower than both warm and cool values??
summary(sp.table.ind$CU_cool < sp.table.ind$CU_warm) # 19 true, 11 false (warm season consumption is not always greater than cool)

summary(sp.table.ind$CU_annual < sp.table.ind$CU_warm)

## Manually look at values
quantile(HUC8_nonenergy@data$Consumption_ave2, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(HUC8_energy@data$Consumption_ave2, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(HUC8_industrial@data$Consumption_ave2, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(HUC8_commercial@data$Consumption_ave2, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(HUC8_aq@data$Consumption_ave2, probs=c(0.25,0.5,0.75), na.rm=TRUE)
quantile(HUC8_municipal@data$Consumption_ave2, probs=c(0.25,0.5,0.75), na.rm=TRUE)

save.image("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis/SpatialAnalysis/VA_HUC8_Consumptive_Use_Season_ws.RData")
#load("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis/SpatialAnalysis/VA_HUC8_Consumptive_Use_Season_ws.RData")

