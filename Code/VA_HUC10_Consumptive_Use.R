###########################################################################################################################################
#########################Estimating Consumptive Use in Virginia HUC 10 Watersheds##########################################################

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
####HUC10 and Virginia Shapefile Manipulation####

#Load databases and extract required layers
HUC10<-readOGR("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/HUC.gdb",layer='WBDHU10')
VA<-readOGR('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/EvapInputs.gdb',layer="VA")

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
deliveries<-deliveries[!duplicated(deliveries),]
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
dFrom@data$HUC10<-HUC10_Facilities$HUC10
dFrom@data$HUC10Name<-HUC10_Facilities$HUC10Name

####TO Delivery Transfers####
#Looks at all TO delivery transfers with real geometry and creates a spatial dataframe 'dTo'
#Spatially overlays HUC boundaries on dTo such that each TO transfer is labeled by origin HUC
dTo<-deliveries[!(is.na(deliveries$geomTlat)&is.na(deliveries$geomTlon)),]
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

for (i in 1:length(dTo@data$hydroid)){
  ToHUC<-as.character(dTo@data$HUC10)[i]
  FromHUC<-as.character(dFrom@data$HUC10)[i]
  if(is.na(ToHUC)){
    ToHUC<-'Null HUC10'
  }
  if(is.na(FromHUC)){
    FromHUC<-'Null HUC10' 
  }
  interbasin<-0
  if(ToHUC!=FromHUC){ #if the HUC code does not match, mark as interbasin delivery
    interbasin<-1
  }
  dTo@data$interbasin[i]<-interbasin
  dFrom@data$interbasin[i]<-interbasin
}

####Sum Net Water In and Out for each HUC10 Watershed####
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
###########################################################################################################################################
####Release Transfers (FROM)####
#Ignoring repeat hydroids (same transfer reported from both recieving and transfer facility),
#repeat the above steps using the remaining FROM transfers available at:
#http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/echo_facilities_and_outfalls/edit/views_data_export_5
GET('http://deq1.bse.vt.edu/d.bet/echo_transfer_from',write_disk(temp <- tempfile(fileext = ".csv")))
releases<-read.csv(temp)
releases$geom<-as.character(releases$geom)
releases<-releases[!(releases$hydroid%in%deliveries$hydroid),] #removing redundant data that exists in TO Transfer data frame
releases<-releases[!duplicated(releases),]
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

####Determine if Release FROM Transfers are leaving watershed boundaries####
rTo@data$interbasin<-NA
rFrom@data$interbasin<-NA

for (i in 1:length(rTo@data$hydroid)){
  ToHUC<-as.character(rTo@data$HUC10)[i]
  FromHUC<-as.character(rFrom@data$HUC10)[i]
  if(is.na(ToHUC[i])){ #if the HUC code is NA
    ToHUC<-'Null HUC10'
  }
  if(is.na(FromHUC[i])){
    FromHUC<-'Null HUC10' 
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
###########################################################################################################################################
####Calculate Net Transfers for Each HUC10 Watershed####
#Loop through each HUC 8 and check for summed releases and deliveries
#Water out is defined as the "from's" and Water in are the "to's"
#This is net transfer- so negative number means more water is leaving watershed (in terms of transfers)

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

HUC10_Transfers<-data.frame(HUC10_Name=HUC10@data$Name,
                           HUC10=HUC10@data$HUC10,
                           Transfers_2010_MGD=HUC10@data$transferred_2010,
                           Transfers_2011_MGD=HUC10@data$transferred_2011,
                           Transfers_2012_MGD=HUC10@data$transferred_2012,
                           Transfers_2013_MGD=HUC10@data$transferred_2013,
                           Transfers_2014_MGD=HUC10@data$transferred_2014,
                           Transfers_2015_MGD=HUC10@data$transferred_2015,
                           Transfers_2016_MGD=HUC10@data$transferred_2016,
                           Transfers_2017_MGD=HUC10@data$transferred_2017)

HUC10_Transfers<-HUC10_Transfers[order(HUC10_Transfers$HUC10_Name,decreasing=F),]

rm(delf,delt,dFrom,dTo,relf,relt,rFrom,rTo,temp,interbasin,ToHUC)
###########################################################################################################################################
####################################################Calculating Discharges#################################################################

#####Load in Discharge Data####
#Load in .txt file with DMR data from 2010-2017. 
ECHO_2010_2017<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC.txt", sep="\t", header=T)

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

####Overlay with HUC 8 Watershed Shapefile####
HUC10_Facilities<-over(ECHO_2010_2017,HUC10_Overlay)
ECHO_2010_2017@data$HUC10<-HUC10_Facilities$HUC10
ECHO_2010_2017@data$HUC10Name<-HUC10_Facilities$HUC10Name

####Sum Discharges in HUC 8 Watersheds####
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
                   HUC10=first(HUC10),
                   HUC10Name=first(HUC10Name))%>%arrange(desc(Discharges_MGD))

HUC10_Discharges<-Outfall_Discharges%>%
  dplyr::group_by(HUC10Name,Year)%>%
  dplyr::summarise(HUC10=first(HUC10),Discharge_MGD=sum(Discharges_MGD,na.rm=T))%>%arrange(desc(Discharge_MGD))

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
HUC10_VWUDS<-over(VWUDS_2010_2017,HUC10_Overlay)
VWUDS_2010_2017@data$HUC10<-HUC10_VWUDS$HUC10
VWUDS_2010_2017@data$HUC10Name<-HUC10_VWUDS$HUC10Name

VWUDS_2010_2017.test<-VWUDS_2010_2017@data
#Summarize by HUC to find the total withdrawal occurring in each HUC

Source_Withdrawals<-VWUDS_2010_2017@data%>%
  dplyr::group_by(DEQ.ID.of.Source,Year)%>%
  dplyr::summarise(Facility.ID=first(Facility.ID),
                   Facility_Name=first(Facility),
                   Mon_Reported=first(Mon_Reported),
                   Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T)/first(Mon_Reported),
                   Sector=first(Reclass_Use_Type),
                   HUC10=first(HUC10),
                   HUC10Name=first(HUC10Name))%>%arrange(desc(Withdrawals_MGD))

HUC10_Withdrawals<-Source_Withdrawals%>%
  dplyr::group_by(HUC10Name,Year)%>%
  dplyr::summarise(HUC10=first(HUC10),Withdrawals_MGD=sum(Withdrawals_MGD, na.rm=T))%>%arrange(desc(Withdrawals_MGD))

###########################################################################################################################################
####################################################Put Withdrawals and Discharrges into HUC10 Spatial Dataframe#############################

#---Year 2010----#
HUC10@data$Discharges_2010<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2010"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2010"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2010"])],NA)
HUC10@data$Withdrawals_2010<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2010"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2010"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2010"])],NA)

#---Year 2011----#
HUC10@data$Discharges_2011<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2011"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2011"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2011"])],NA)
HUC10@data$Withdrawals_2011<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2011"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2011"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2011"])],NA)

#---Year 2012----#
HUC10@data$Discharges_2012<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2012"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2012"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2012"])],NA)
HUC10@data$Withdrawals_2012<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2012"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2012"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2012"])],NA)

#---Year 2013----#
HUC10@data$Discharges_2013<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2013"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2013"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2013"])],NA)
HUC10@data$Withdrawals_2013<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2013"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2013"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2013"])],NA)

#---Year 2014----#
HUC10@data$Discharges_2014<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2014"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2014"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2014"])],NA)
HUC10@data$Withdrawals_2014<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2014"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2014"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2014"])],NA)

#---Year 2015----#
HUC10@data$Discharges_2015<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2015"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2015"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2015"])],NA)
HUC10@data$Withdrawals_2015<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2015"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2015"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2015"])],NA)

#---Year 2016----#
HUC10@data$Discharges_2016<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2016"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2016"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2016"])],NA)
HUC10@data$Withdrawals_2016<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2016"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2016"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2016"])],NA)

#---Year 2017----#
HUC10@data$Discharges_2017<-ifelse(HUC10@data$HUC10%in%HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2017"],HUC10_Discharges$Discharge_MGD[HUC10_Discharges$Year=="2017"][match(HUC10@data$HUC10,HUC10_Discharges$HUC10[HUC10_Discharges$Year=="2017"])],NA)
HUC10@data$Withdrawals_2017<-ifelse(HUC10@data$HUC10%in%HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2017"],HUC10_Withdrawals$Withdrawals_MGD[HUC10_Withdrawals$Year=="2017"][match(HUC10@data$HUC10,HUC10_Withdrawals$HUC10[HUC10_Withdrawals$Year=="2017"])],NA)

###########################################################################################################################################
#######################################################Net Water Balance and Consumtpive Use###############################################

#---Year 2010----#

#--With Transfers---#
#transfers have sign--negative mean water is leaving, positive means water is coming in 

HUC10@data$NetWB_2010_t<-(ifelse(is.na(HUC10@data$Discharges_2010),0,HUC10@data$Discharges_2010))+(ifelse(is.na(HUC10@data$transferred_2010),0,HUC10@data$transferred_2010))-(ifelse(is.na(HUC10@data$Withdrawals_2010),0,HUC10@data$Withdrawals_2010))

HUC10@data$NetWB_2010_t<-ifelse(is.na(HUC10@data$Discharges_2010)&is.na(HUC10@data$Withdrawals_2010)&is.na(HUC10@data$transferred_2010),NA,HUC10@data$NetWB_2010_t)

HUC10@data$Consumption_2010_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2010),0,HUC10@data$Withdrawals_2010))+(ifelse(is.na(HUC10@data$waterout_2010),0,-HUC10@data$waterout_2010)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2010),0,HUC10@data$Discharges_2010))+(ifelse(is.na(HUC10@data$waterin_2010),0,HUC10@data$waterin_2010)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2010),0,HUC10@data$Withdrawals_2010))+(ifelse(is.na(HUC10@data$waterout_2010),0,-HUC10@data$waterout_2010)))

HUC10@data$Consumption_2010_t<-ifelse(is.nan(HUC10@data$Consumption_2010_t)|is.infinite(HUC10@data$Consumption_2010_t),NA,HUC10@data$Consumption_2010_t)
#--Without Transfers---#

HUC10@data$NetWB_2010<-(ifelse(is.na(HUC10@data$Discharges_2010),0,HUC10@data$Discharges_2010))-(ifelse(is.na(HUC10@data$Withdrawals_2010),0,HUC10@data$Withdrawals_2010))

HUC10@data$NetWB_2010<-ifelse(is.na(HUC10@data$Discharges_2010)&is.na(HUC10@data$Withdrawals_2010),NA,HUC10@data$NetWB_2010)

HUC10@data$Consumption_2010<-((ifelse(is.na(HUC10@data$Withdrawals_2010),0,HUC10@data$Withdrawals_2010))-
                               (ifelse(is.na(HUC10@data$Discharges_2010),0,HUC10@data$Discharges_2010)))/(ifelse(is.na(HUC10@data$Withdrawals_2010),0,HUC10@data$Withdrawals_2010))

HUC10@data$Consumption_2010<-ifelse(is.nan(HUC10@data$Consumption_2010)|is.infinite(HUC10@data$Consumption_2010),NA,HUC10@data$Consumption_2010)
#---Year 2011----#

#----With transfers---#
HUC10@data$NetWB_2011_t<-(ifelse(is.na(HUC10@data$Discharges_2011),0,HUC10@data$Discharges_2011))+(ifelse(is.na(HUC10@data$transferred_2011),0,HUC10@data$transferred_2011))-(ifelse(is.na(HUC10@data$Withdrawals_2011),0,HUC10@data$Withdrawals_2011))

HUC10@data$NetWB_2011_t<-ifelse(is.na(HUC10@data$Discharges_2011)&is.na(HUC10@data$Withdrawals_2011)&is.na(HUC10@data$transferred_2011),NA,HUC10@data$NetWB_2011_t)

HUC10@data$Consumption_2011_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2011),0,HUC10@data$Withdrawals_2011))+(ifelse(is.na(HUC10@data$waterout_2011),0,-HUC10@data$waterout_2011)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2011),0,HUC10@data$Discharges_2011))+(ifelse(is.na(HUC10@data$waterin_2011),0,HUC10@data$waterin_2011)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2011),0,HUC10@data$Withdrawals_2011))+(ifelse(is.na(HUC10@data$waterout_2011),0,-HUC10@data$waterout_2011)))

HUC10@data$Consumption_2011_t<-ifelse(is.nan(HUC10@data$Consumption_2011_t)|is.infinite(HUC10@data$Consumption_2011_t),NA,HUC10@data$Consumption_2011_t)
#---Without Transfers----#
HUC10@data$NetWB_2011<-(ifelse(is.na(HUC10@data$Discharges_2011),0,HUC10@data$Discharges_2011))-(ifelse(is.na(HUC10@data$Withdrawals_2011),0,HUC10@data$Withdrawals_2011))

HUC10@data$NetWB_2011<-ifelse(is.na(HUC10@data$Discharges_2011)&is.na(HUC10@data$Withdrawals_2011),NA,HUC10@data$NetWB_2011)

HUC10@data$Consumption_2011<-((ifelse(is.na(HUC10@data$Withdrawals_2011),0,HUC10@data$Withdrawals_2011))-
                               (ifelse(is.na(HUC10@data$Discharges_2011),0,HUC10@data$Discharges_2011)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2011),0,HUC10@data$Withdrawals_2011))

HUC10@data$Consumption_2011<-ifelse(is.nan(HUC10@data$Consumption_2011)|is.infinite(HUC10@data$Consumption_2011),NA,HUC10@data$Consumption_2011)
#---Year 2012----#

#---With Transfers---#
HUC10@data$NetWB_2012_t<-(ifelse(is.na(HUC10@data$Discharges_2012),0,HUC10@data$Discharges_2012))+(ifelse(is.na(HUC10@data$transferred_2012),0,HUC10@data$transferred_2012))-(ifelse(is.na(HUC10@data$Withdrawals_2012),0,HUC10@data$Withdrawals_2012))

HUC10@data$NetWB_2012_t<-ifelse(is.na(HUC10@data$Discharges_2012)&is.na(HUC10@data$Withdrawals_2012)&is.na(HUC10@data$transferred_2012),NA,HUC10@data$NetWB_2012_t)

HUC10@data$Consumption_2012_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2012),0,HUC10@data$Withdrawals_2012))+(ifelse(is.na(HUC10@data$waterout_2012),0,-HUC10@data$waterout_2012)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2012),0,HUC10@data$Discharges_2012))+(ifelse(is.na(HUC10@data$waterin_2012),0,HUC10@data$waterin_2012)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2012),0,HUC10@data$Withdrawals_2012))+(ifelse(is.na(HUC10@data$waterout_2012),0,-HUC10@data$waterout_2012)))


HUC10@data$Consumption_2012_t<-ifelse(is.nan(HUC10@data$Consumption_2012_t)|is.infinite(HUC10@data$Consumption_2012_t),NA,HUC10@data$Consumption_2012_t)

#---Without Transfers---#
HUC10@data$NetWB_2012<-(ifelse(is.na(HUC10@data$Discharges_2012),0,HUC10@data$Discharges_2012))-(ifelse(is.na(HUC10@data$Withdrawals_2012),0,HUC10@data$Withdrawals_2012))

HUC10@data$NetWB_2012<-ifelse(is.na(HUC10@data$Discharges_2012)&is.na(HUC10@data$Withdrawals_2012),NA,HUC10@data$NetWB_2012)

HUC10@data$Consumption_2012<-((ifelse(is.na(HUC10@data$Withdrawals_2012),0,HUC10@data$Withdrawals_2012))-
                               (ifelse(is.na(HUC10@data$Discharges_2012),0,HUC10@data$Discharges_2012)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2012),0,HUC10@data$Withdrawals_2012))

HUC10@data$Consumption_2012<-ifelse(is.nan(HUC10@data$Consumption_2012)|is.infinite(HUC10@data$Consumption_2012),NA,HUC10@data$Consumption_2012)
#---Year 2013----#

#---With Transfers---#
HUC10@data$NetWB_2013_t<-(ifelse(is.na(HUC10@data$Discharges_2013),0,HUC10@data$Discharges_2013))+(ifelse(is.na(HUC10@data$transferred_2013),0,HUC10@data$transferred_2013))-(ifelse(is.na(HUC10@data$Withdrawals_2013),0,HUC10@data$Withdrawals_2013))

HUC10@data$NetWB_2013_t<-ifelse(is.na(HUC10@data$Discharges_2013)&is.na(HUC10@data$Withdrawals_2013)&is.na(HUC10@data$transferred_2013),NA,HUC10@data$NetWB_2013_t)

HUC10@data$Consumption_2013_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2013),0,HUC10@data$Withdrawals_2013))+(ifelse(is.na(HUC10@data$waterout_2013),0,-HUC10@data$waterout_2013)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2013),0,HUC10@data$Discharges_2013))+(ifelse(is.na(HUC10@data$waterin_2013),0,HUC10@data$waterin_2013)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2013),0,HUC10@data$Withdrawals_2013))+(ifelse(is.na(HUC10@data$waterout_2013),0,-HUC10@data$waterout_2013)))

HUC10@data$Consumption_2013_t<-ifelse(is.nan(HUC10@data$Consumption_2013_t)|is.infinite(HUC10@data$Consumption_2013_t),NA,HUC10@data$Consumption_2013_t)

#---Without Transfers---#
HUC10@data$NetWB_2013<-(ifelse(is.na(HUC10@data$Discharges_2013),0,HUC10@data$Discharges_2013))-(ifelse(is.na(HUC10@data$Withdrawals_2013),0,HUC10@data$Withdrawals_2013))

HUC10@data$NetWB_2013<-ifelse(is.na(HUC10@data$Discharges_2013)&is.na(HUC10@data$Withdrawals_2013),NA,HUC10@data$NetWB_2013)

HUC10@data$Consumption_2013<-((ifelse(is.na(HUC10@data$Withdrawals_2013),0,HUC10@data$Withdrawals_2013))-
                               (ifelse(is.na(HUC10@data$Discharges_2013),0,HUC10@data$Discharges_2013)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2013),0,HUC10@data$Withdrawals_2013))

HUC10@data$Consumption_2013<-ifelse(is.nan(HUC10@data$Consumption_2013)|is.infinite(HUC10@data$Consumption_2013),NA,HUC10@data$Consumption_2013)

#---Year 2014----#

#---With Transfers---#
HUC10@data$NetWB_2014_t<-(ifelse(is.na(HUC10@data$Discharges_2014),0,HUC10@data$Discharges_2014))+(ifelse(is.na(HUC10@data$transferred_2014),0,HUC10@data$transferred_2014))-(ifelse(is.na(HUC10@data$Withdrawals_2014),0,HUC10@data$Withdrawals_2014))

HUC10@data$NetWB_2014_t<-ifelse(is.na(HUC10@data$Discharges_2014)&is.na(HUC10@data$Withdrawals_2014)&is.na(HUC10@data$transferred_2014),NA,HUC10@data$NetWB_2014_t)

HUC10@data$Consumption_2014_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2014),0,HUC10@data$Withdrawals_2014))+(ifelse(is.na(HUC10@data$waterout_2014),0,-HUC10@data$waterout_2014)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2014),0,HUC10@data$Discharges_2014))+(ifelse(is.na(HUC10@data$waterin_2014),0,HUC10@data$waterin_2014)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2014),0,HUC10@data$Withdrawals_2014))+(ifelse(is.na(HUC10@data$waterout_2014),0,-HUC10@data$waterout_2014)))

HUC10@data$Consumption_2014_t<-ifelse(is.nan(HUC10@data$Consumption_2014_t)|is.infinite(HUC10@data$Consumption_2014_t),NA,HUC10@data$Consumption_2014_t)

#---Without Transfers---#
HUC10@data$NetWB_2014<-(ifelse(is.na(HUC10@data$Discharges_2014),0,HUC10@data$Discharges_2014))-(ifelse(is.na(HUC10@data$Withdrawals_2014),0,HUC10@data$Withdrawals_2014))

HUC10@data$NetWB_2014<-ifelse(is.na(HUC10@data$Discharges_2014)&is.na(HUC10@data$Withdrawals_2014),NA,HUC10@data$NetWB_2014)

HUC10@data$Consumption_2014<-((ifelse(is.na(HUC10@data$Withdrawals_2014),0,HUC10@data$Withdrawals_2014))-
                               (ifelse(is.na(HUC10@data$Discharges_2014),0,HUC10@data$Discharges_2014)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2014),0,HUC10@data$Withdrawals_2014))

HUC10@data$Consumption_2014<-ifelse(is.nan(HUC10@data$Consumption_2014)|is.infinite(HUC10@data$Consumption_2014),NA,HUC10@data$Consumption_2014)
#---Year 2015----#

#---With Transfers---#
HUC10@data$NetWB_2015_t<-(ifelse(is.na(HUC10@data$Discharges_2015),0,HUC10@data$Discharges_2015))+(ifelse(is.na(HUC10@data$transferred_2015),0,HUC10@data$transferred_2015))-(ifelse(is.na(HUC10@data$Withdrawals_2015),0,HUC10@data$Withdrawals_2015))

HUC10@data$NetWB_2015_t<-ifelse(is.na(HUC10@data$Discharges_2015)&is.na(HUC10@data$Withdrawals_2015)&is.na(HUC10@data$transferred_2015),NA,HUC10@data$NetWB_2015_t)

HUC10@data$Consumption_2015_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2015),0,HUC10@data$Withdrawals_2015))+(ifelse(is.na(HUC10@data$waterout_2015),0,-HUC10@data$waterout_2015)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2015),0,HUC10@data$Discharges_2015))+(ifelse(is.na(HUC10@data$waterin_2015),0,HUC10@data$waterin_2015)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2015),0,HUC10@data$Withdrawals_2015))+(ifelse(is.na(HUC10@data$waterout_2015),0,-HUC10@data$waterout_2015)))

HUC10@data$Consumption_2015_t<-ifelse(is.nan(HUC10@data$Consumption_2015_t)|is.infinite(HUC10@data$Consumption_2015_t),NA,HUC10@data$Consumption_2015_t)

#---Without Transfers---#
HUC10@data$NetWB_2015<-(ifelse(is.na(HUC10@data$Discharges_2015),0,HUC10@data$Discharges_2015))-(ifelse(is.na(HUC10@data$Withdrawals_2015),0,HUC10@data$Withdrawals_2015))

HUC10@data$NetWB_2015<-ifelse(is.na(HUC10@data$Discharges_2015)&is.na(HUC10@data$Withdrawals_2015),NA,HUC10@data$NetWB_2015)

HUC10@data$Consumption_2015<-((ifelse(is.na(HUC10@data$Withdrawals_2015),0,HUC10@data$Withdrawals_2015))-
                               (ifelse(is.na(HUC10@data$Discharges_2015),0,HUC10@data$Discharges_2015)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2015),0,HUC10@data$Withdrawals_2015))

HUC10@data$Consumption_2015<-ifelse(is.nan(HUC10@data$Consumption_2015)|is.infinite(HUC10@data$Consumption_2015),NA,HUC10@data$Consumption_2015)

#---Year 2016----#

#---With Transfers---#
HUC10@data$NetWB_2016_t<-(ifelse(is.na(HUC10@data$Discharges_2016),0,HUC10@data$Discharges_2016))+(ifelse(is.na(HUC10@data$transferred_2016),0,HUC10@data$transferred_2016))-(ifelse(is.na(HUC10@data$Withdrawals_2016),0,HUC10@data$Withdrawals_2016))

HUC10@data$NetWB_2016_t<-ifelse(is.na(HUC10@data$Discharges_2016)&is.na(HUC10@data$Withdrawals_2016)&is.na(HUC10@data$transferred_2016),NA,HUC10@data$NetWB_2016_t)

HUC10@data$Consumption_2016_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2016),0,HUC10@data$Withdrawals_2016))+(ifelse(is.na(HUC10@data$waterout_2016),0,-HUC10@data$waterout_2016)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2016),0,HUC10@data$Discharges_2016))+(ifelse(is.na(HUC10@data$waterin_2016),0,HUC10@data$waterin_2016)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2016),0,HUC10@data$Withdrawals_2016))+(ifelse(is.na(HUC10@data$waterout_2016),0,-HUC10@data$waterout_2016)))

HUC10@data$Consumption_2016_t<-ifelse(is.nan(HUC10@data$Consumption_2016_t)|is.infinite(HUC10@data$Consumption_2016_t),NA,HUC10@data$Consumption_2016_t)

#---Without Transfers---#
HUC10@data$NetWB_2016<-(ifelse(is.na(HUC10@data$Discharges_2016),0,HUC10@data$Discharges_2016))-(ifelse(is.na(HUC10@data$Withdrawals_2016),0,HUC10@data$Withdrawals_2016))

HUC10@data$NetWB_2016<-ifelse(is.na(HUC10@data$Discharges_2016)&is.na(HUC10@data$Withdrawals_2016),NA,HUC10@data$NetWB_2016)

HUC10@data$Consumption_2016<-((ifelse(is.na(HUC10@data$Withdrawals_2016),0,HUC10@data$Withdrawals_2016))-
                               (ifelse(is.na(HUC10@data$Discharges_2016),0,HUC10@data$Discharges_2016)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2016),0,HUC10@data$Withdrawals_2016))

HUC10@data$Consumption_2016<-ifelse(is.nan(HUC10@data$Consumption_2016)|is.infinite(HUC10@data$Consumption_2016),NA,HUC10@data$Consumption_2016)

#---Year 2017----#

#---With Transfers---#
HUC10@data$NetWB_2017_t<-(ifelse(is.na(HUC10@data$Discharges_2017),0,HUC10@data$Discharges_2017))+(ifelse(is.na(HUC10@data$transferred_2017),0,HUC10@data$transferred_2017))-(ifelse(is.na(HUC10@data$Withdrawals_2017),0,HUC10@data$Withdrawals_2017))

HUC10@data$NetWB_2017_t<-ifelse(is.na(HUC10@data$Discharges_2017)&is.na(HUC10@data$Withdrawals_2017)&is.na(HUC10@data$transferred_2017),NA,HUC10@data$NetWB_2017_t)

HUC10@data$Consumption_2017_t<-(((ifelse(is.na(HUC10@data$Withdrawals_2017),0,HUC10@data$Withdrawals_2017))+(ifelse(is.na(HUC10@data$waterout_2017),0,-HUC10@data$waterout_2017)))-
                                 (((ifelse(is.na(HUC10@data$Discharges_2017),0,HUC10@data$Discharges_2017))+(ifelse(is.na(HUC10@data$waterin_2017),0,HUC10@data$waterin_2017)))))/
  ((ifelse(is.na(HUC10@data$Withdrawals_2017),0,HUC10@data$Withdrawals_2017))+(ifelse(is.na(HUC10@data$waterout_2017),0,-HUC10@data$waterout_2017)))

HUC10@data$Consumption_2017_t<-ifelse(is.nan(HUC10@data$Consumption_2017_t)|is.infinite(HUC10@data$Consumption_2017_t),NA,HUC10@data$Consumption_2017_t)

#---Without Transfers---#
HUC10@data$NetWB_2017<-(ifelse(is.na(HUC10@data$Discharges_2017),0,HUC10@data$Discharges_2017))-(ifelse(is.na(HUC10@data$Withdrawals_2017),0,HUC10@data$Withdrawals_2017))

HUC10@data$NetWB_2017<-ifelse(is.na(HUC10@data$Discharges_2017)&is.na(HUC10@data$Withdrawals_2017),NA,HUC10@data$NetWB_2017)

HUC10@data$Consumption_2017<-((ifelse(is.na(HUC10@data$Withdrawals_2017),0,HUC10@data$Withdrawals_2017))-(ifelse(is.na(HUC10@data$Discharges_2017),0,HUC10@data$Discharges_2017)))/
  (ifelse(is.na(HUC10@data$Withdrawals_2017),0,HUC10@data$Withdrawals_2017))

HUC10@data$Consumption_2017<-ifelse(is.nan(HUC10@data$Consumption_2017)|is.infinite(HUC10@data$Consumption_2017),NA,HUC10@data$Consumption_2017)
#################################################################################################
#-------------Long Term Averages 2010-2017--------------#

#----Discharges-----#

#Be careful for NA values#
for (i in 1:length(HUC10@data$TNMID)){
  HUC10@data$Discharge_sum[i]<-(ifelse(is.na(HUC10@data$Discharges_2010[i]),0,HUC10@data$Discharges_2010[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2011[i]),0,HUC10@data$Discharges_2011[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2012[i]),0,HUC10@data$Discharges_2012[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2013[i]),0,HUC10@data$Discharges_2013[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2014[i]),0,HUC10@data$Discharges_2014[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2015[i]),0,HUC10@data$Discharges_2015[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2016[i]),0,HUC10@data$Discharges_2016[i])+
                                 ifelse(is.na(HUC10@data$Discharges_2017[i]),0,HUC10@data$Discharges_2017[i]))
}
HUC10@data$Discharge_ave<-HUC10@data$Discharge_sum/8
HUC10@data$Discharge_ave<-ifelse(HUC10@data$Discharge_ave==0,NA,HUC10@data$Discharge_ave)

#----Withdrawals----#
for (i in 1:length(HUC10@data$TNMID)){
  HUC10@data$Withdrawal_sum[i]<-(ifelse(is.na(HUC10@data$Withdrawals_2010[i]),0,HUC10@data$Withdrawals_2010[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2011[i]),0,HUC10@data$Withdrawals_2011[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2012[i]),0,HUC10@data$Withdrawals_2012[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2013[i]),0,HUC10@data$Withdrawals_2013[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2014[i]),0,HUC10@data$Withdrawals_2014[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2015[i]),0,HUC10@data$Withdrawals_2015[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2016[i]),0,HUC10@data$Withdrawals_2016[i])+
                                  ifelse(is.na(HUC10@data$Withdrawals_2017[i]),0,HUC10@data$Withdrawals_2017[i]))
}
HUC10@data$Withdrawal_ave<-HUC10@data$Withdrawal_sum/8
HUC10@data$Withdrawal_ave<-ifelse(HUC10@data$Withdrawal_ave==0,NA,HUC10@data$Withdrawal_ave)


#----Net Water Balance-----#
for (i in 1:length(HUC10@data$TNMID)){
  HUC10@data$NetWB_sum[i]<-(ifelse(is.na(HUC10@data$NetWB_2010[i]),0,HUC10@data$NetWB_2010[i])+
                             ifelse(is.na(HUC10@data$NetWB_2011[i]),0,HUC10@data$NetWB_2011[i])+
                             ifelse(is.na(HUC10@data$NetWB_2012[i]),0,HUC10@data$NetWB_2012[i])+
                             ifelse(is.na(HUC10@data$NetWB_2013[i]),0,HUC10@data$NetWB_2013[i])+
                             ifelse(is.na(HUC10@data$NetWB_2014[i]),0,HUC10@data$NetWB_2014[i])+
                             ifelse(is.na(HUC10@data$NetWB_2015[i]),0,HUC10@data$NetWB_2015[i])+
                             ifelse(is.na(HUC10@data$NetWB_2016[i]),0,HUC10@data$NetWB_2016[i])+
                             ifelse(is.na(HUC10@data$NetWB_2017[i]),0,HUC10@data$NetWB_2017[i]))
}
HUC10@data$NetWB_ave<-HUC10@data$NetWB_sum/8
HUC10@data$NetWB_ave<-ifelse(HUC10@data$NetWB_ave==0,NA,-HUC10@data$NetWB_ave)

#----Consumption----#

for (i in 1:length(HUC10@data$TNMID)){
  HUC10@data$Consumption_sum[i]<-(ifelse(is.na(HUC10@data$Consumption_2010[i]),0,HUC10@data$Consumption_2010[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2011[i]),0,HUC10@data$Consumption_2011[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2012[i]),0,HUC10@data$Consumption_2012[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2013[i]),0,HUC10@data$Consumption_2013[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2014[i]),0,HUC10@data$Consumption_2014[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2015[i]),0,HUC10@data$Consumption_2015[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2016[i]),0,HUC10@data$Consumption_2016[i])+
                                   ifelse(is.na(HUC10@data$Consumption_2017[i]),0,HUC10@data$Consumption_2017[i]))
}

HUC10@data$Consumption_ave<-HUC10@data$Consumption_sum/8
HUC10@data$Consumption_ave<-ifelse(HUC10@data$Consumption_ave==0,NA,-HUC10@data$Consumption_ave)

#-----------------With Transfers-------------------------#
for (i in 1:length(HUC10@data$TNMID)){
  HUC10@data$NetWB_t_sum[i]<-(ifelse(is.na(HUC10@data$NetWB_2010_t[i]),0,HUC10@data$NetWB_2010_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2011_t[i]),0,HUC10@data$NetWB_2011_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2012_t[i]),0,HUC10@data$NetWB_2012_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2013_t[i]),0,HUC10@data$NetWB_2013_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2014_t[i]),0,HUC10@data$NetWB_2014_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2015_t[i]),0,HUC10@data$NetWB_2015_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2016_t[i]),0,HUC10@data$NetWB_2016_t[i])+
                               ifelse(is.na(HUC10@data$NetWB_2017_t[i]),0,HUC10@data$NetWB_2017_t[i]))
}
HUC10@data$NetWB_t_ave<-HUC10@data$NetWB_t_sum/8
HUC10@data$NetWB_t_ave<-ifelse(HUC10@data$NetWB_t_ave==0,NA,HUC10@data$NetWB_t_ave)

for (i in 1:length(HUC10@data$TNMID)){
  HUC10@data$Consumption_t_sum[i]<-(ifelse(is.na(HUC10@data$Consumption_2010_t[i]),0,HUC10@data$Consumption_2010_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2011_t[i]),0,HUC10@data$Consumption_2011_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2012_t[i]),0,HUC10@data$Consumption_2012_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2013_t[i]),0,HUC10@data$Consumption_2013_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2014_t[i]),0,HUC10@data$Consumption_2014_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2015_t[i]),0,HUC10@data$Consumption_2015_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2016_t[i]),0,HUC10@data$Consumption_2016_t[i])+
                                     ifelse(is.na(HUC10@data$Consumption_2017_t[i]),0,HUC10@data$Consumption_2017_t[i]))
}

HUC10@data$Consumption_t_ave<-HUC10@data$Consumption_t_sum/8
HUC10@data$Consumption_t_ave<-ifelse(HUC10@data$Consumption_t_ave==0,NA,HUC10@data$Consumption_t_ave)

HUC10_glimpse<-HUC10@data
HUC10_glimpse[15:98]<-sapply(HUC10_glimpse[15:98],as.numeric)
HUC10_glimpse[15:98]<-round(HUC10_glimpse[15:98], digits=2)

rm(HUC10_Facilities)
#####################################################################################
#-----------------------------Visualization-----------------------------------------#
HUC10.df<-broom::tidy(HUC10)
HUC10$polyID<-sapply(slot(HUC10,"polygons"), function(x) slot(x, "ID"))
HUC10.df<-merge(HUC10.df, HUC10, by.x="id", by.y="polyID")

#Plot consumptive use by first assigning data to the cropped HUC boundary and then by running the plot and legend commands
#Crop Watersheds to Virginia State Boundaries
HUC10_Clipped<-gIntersection(HUC10,VA,id=as.character(HUC10@data$HUC10),byid=TRUE,drop_lower_td=TRUE)
HUC10_Clipped<-SpatialPolygonsDataFrame(HUC10_Clipped,HUC10@data[as.character(HUC10@data$HUC10)%in%names(HUC10_Clipped),],match.ID = "HUC10")

HUC10.df<-broom::tidy(HUC10_Clipped)
HUC10_Clipped$polyID<-sapply(slot(HUC10_Clipped,"polygons"), function(x) slot(x, "ID"))
HUC10.df<-merge(HUC10.df, HUC10_Clipped, by.x="id", by.y="polyID")


#-----HUC 8 Watershed Labels-----#
HUC10_Centroids<-as.data.frame(coordinates(HUC10))
names(HUC10_Centroids)<-c("Longitude","Latitude")
HUC10_Centroids$HUC10<-HUC10_glimpse$HUC10
HUC10_glimpse<-subset(HUC10_glimpse,select=-c(1,2,3,4,5,6,7,8,9,10,13,14))
HUC10_Centroids<-merge(HUC10_Centroids,HUC10_glimpse,by="HUC10")

ggplot()+
  geom_polygon(data=HUC10,aes(x=long, y= lat, group=group), colour='black', fill=NA)+
  geom_label_repel(data=HUC10_Centroids,aes(x=Longitude,y=Latitude,label=Name),size=4, segment.color = "#99000d", show.legend = F, arrow = arrow(length = unit(0.02, "npc")))+
  scale_colour_manual(values=c("#252525"))+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


###################################################################################
#----------------Discharge-----------------#

#---Long Term Summed Discharge Average---#

#--Discrete--#

Dis_Discrete<- c("#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")

quantile(HUC10.df$Discharge_ave,na.rm=T)

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$Discharge_ave,breaks=c(0,
                                                 (quantile(HUC10.df$Discharge_ave,c(1/4),na.rm=T)),
                                                 (quantile(HUC10.df$Discharge_ave,c(2/4),na.rm=T)),
                                                 (quantile(HUC10.df$Discharge_ave,c(3/4),na.rm=T)),
                                                 (quantile(HUC10.df$Discharge_ave,c(4/4),na.rm=T))),include.lowest=T), colour=""))+
  scale_fill_manual(name="Summed Discharge (MGD)", values=(Dis_Discrete), 
                    labels=c(paste(0,"MGD -",round(quantile(HUC10.df$Discharge_ave,c(1/4),na.rm=T),digits=3),"MGD"),
                             paste(round(quantile(HUC10.df$Discharge_ave,c(1/4),na.rm=T),digits=3),"MGD -",round(quantile(HUC10.df$Discharge_ave,c(2/4),na.rm=T),digits=3),"MGD"),
                             paste(round(quantile(HUC10.df$Discharge_ave,c(2/4),na.rm=T),digits=3),"MGD -",round(quantile(HUC10.df$Discharge_ave,c(3/4),na.rm=T),digits=3),"MGD"),
                             paste(round(quantile(HUC10.df$Discharge_ave,c(3/4),na.rm=T),digits=3),"MGD -",round(quantile(HUC10.df$Discharge_ave,c(4/4),na.rm=T),digits=3),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
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


#-----Refined Scale----#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$Discharge_ave,breaks=c(0,0.01,0.1,1,5,10,max(HUC10.df$Discharge_ave,na.rm=T)),include.lowest=T,include.highest=T), colour=""))+
  scale_fill_manual(name="Average Discharge (MGD)", values=(Dis_Discrete), 
                    labels=c(paste(0,"-",0.01),
                             paste(0.01,"-",0.1),
                             paste(0.1,"-",1),
                             paste(1,"-",5),
                             paste(5,"-",10),
                             paste(10,"<")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude, shape="Outfall"),
             size=1.25,colour="#252525")+
  scale_shape_manual(name="", values=17)+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
  labs(title = "Average Daily Discharge (MGD) 2010-2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)

#--By Sector--#
Dis_Discrete_sector<- c("#bdd7e7","#6baed6","#3182bd","#08519c")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$Discharge_ave,breaks=c(0,
                                                 (quantile(HUC10.df$Discharge_ave,c(1/4),na.rm=T)),
                                                 (quantile(HUC10.df$Discharge_ave,c(2/4),na.rm=T)),
                                                 (quantile(HUC10.df$Discharge_ave,c(3/4),na.rm=T)),
                                                 (quantile(HUC10.df$Discharge_ave,c(1),na.rm=T))),include.lowest=T), colour=""))+
  scale_fill_manual(name="Summed Discharge (MGD)", values=(Dis_Discrete_sector),
                    labels=c(paste(0,"MGD -",round(quantile(HUC10.df$Discharge_ave,c(1/4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$Discharge_ave,c(1/4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$Discharge_ave,c(2/4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$Discharge_ave,c(2/4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$Discharge_ave,c(3/4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$Discharge_ave,c(3/4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$Discharge_ave,c(4/4),na.rm=T),digits=1),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=ECHO_2010_2017.test, aes(x=Outfall_Longitude, y=Outfall_Latitude, shape="Outfall"),
             size=1.25,colour="#252525")+
  scale_shape_manual(name="", values=17)+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
  labs(title = "Average Annual Total Discharge (MGD) 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#---Distribution of Withdrawing Sources in HUC10-----#
VWUDS_2010_2017.test<-VWUDS_2010_2017@data
Mean_Withdrawal<-VWUDS_2010_2017.test%>%group_by(DEQ.ID.of.Source)%>%summarise(Mean_Withdrawal=mean(Withdrawals_MGD, na.rm=T))
VWUDS_2010_2017.test<-merge(VWUDS_2010_2017.test,Mean_Withdrawal, by="DEQ.ID.of.Source",all.x=T)

#---Long Term Summed Withdrawal Average---#

#---Discrete---#
With_Discrete<- c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#99000d")

quantile(HUC10.df$Withdrawal_ave,na.rm=T)

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$Withdrawal_ave,breaks=c(0,
                                                  (quantile(HUC10.df$Withdrawal_ave,c(1/4),na.rm=T)),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(2/4),na.rm=T)),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(3/4),na.rm=T)),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(4/4),na.rm=T))),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(5/6),na.rm=T)),include.lowest=T), colour=""))
  scale_fill_manual(name="Summed Withdrawal (MGD)", values=(With_Discrete),
                    labels=c(paste(0,"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(1/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$Withdrawal_ave,c(1/4),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(2/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$Withdrawal_ave,c(2/4),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(3/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$Withdrawal_ave,c(3/4),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(4/4),na.rm=T),digits=2),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude, shape="Withdrawing Source"),
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

  
  ##########
  NWB_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")
  
  quantile(HUC10.df$NetWB_ave,na.rm=T)
  
  ggplot()+
    geom_polygon(
      data=HUC10.df,
      aes(x=long, y= lat, group=group,
          fill=cut(HUC10.df$NetWB_ave,breaks=c(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),0,5,10,50,quantile(HUC10.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
    scale_fill_manual(name="Volume Consumed (MGD)", values=NWB_Discrete, 
                      labels=c(paste(round(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),digits=0),"-",0),
                               paste(0,"-",5),
                               paste(5,"-",10),
                               paste(10,"-",50),
                               paste(50,"<")),
                      na.value="transparent", drop=FALSE)+
    geom_polygon(
      data=HUC10.df,
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
    labs(title = "Average Volume Consumed (MGD) 2010-2017")+
    scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
              arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
    theme(line=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank())+coord_equal()

#---Refined Scale----#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$Withdrawal_ave,breaks=c(0,0.01,0.1,1,5,10,max(HUC10.df$Withdrawal_ave,na.rm=T)),include.lowest=T,include.highest=T), colour=""))+
  scale_fill_manual(name="Average Withdrawal (MGD)", values=(With_Discrete), 
                    labels=c(paste(0,"-",0.01),
                             paste(0.01,"-",0.1),
                             paste(0.1,"-",1),
                             paste(1,"-",5),
                             paste(5,"-",10),
                             paste(10,"<")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude, shape="Withdrawing Source"),
             size=1.54,colour="#252525")+
  scale_shape_manual(name="", values=20)+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         shape=guide_legend(override.aes=list(linetype=1,colour="black"),order=2),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=3))+
  labs(title = "Average Daily Withdrawal (MGD) 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


#--By Sectors--#
With_Discrete_sector<- c("#fcbba1","#fb6a4a","#de2d26","#a50f15")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$Withdrawal_ave,breaks=c(0,
                                                  (quantile(HUC10.df$Withdrawal_ave,c(1/4),na.rm=T)),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(2/4),na.rm=T)),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(3/4),na.rm=T)),
                                                  (quantile(HUC10.df$Withdrawal_ave,c(4/4),na.rm=T))),include.lowest=T), colour=""))+
  scale_fill_manual(name="Summed Withdrawal (MGD)", values=(With_Discrete_sector),
                    labels=c(paste(0,"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(1/4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$Withdrawal_ave,c(1/4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(2/4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$Withdrawal_ave,c(2/4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(3/4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$Withdrawal_ave,c(3/4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$Withdrawal_ave,c(4/4),na.rm=T),digits=1),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  geom_point(data=VWUDS_2010_2017.test, aes(x=Corrected_Longitude, y=Corrected_Latitude, shape="Withdrawing Source"),
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

#----Long Term Average----#
#---Net Water Balance---#

#---Discrete---#
NWB_Discrete<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#2b8cbe")

#--Cut and separate by positive and negative values--#

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$NetWB_ave,breaks=c(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(0.20),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(0.40),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(0.70),na.rm=T),
                                             0,
                                             quantile(HUC10.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
  scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete,
                    #labels=c(paste(round(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(0.2),na.rm=T),digits=1),"MGD"),
                    #paste(round(quantile(HUC10.df$NetWB_ave,c(0.2),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(0.4),na.rm=T),digits=1),"MGD"),
                    #paste(round(quantile(HUC10.df$NetWB_ave,c(0.4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(0.6),na.rm=T),digits=1),"MGD"),
                    # paste(round(quantile(HUC10.df$NetWB_ave,c(0.6),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(0.8),na.rm=T),digits=1),"MGD"),
                    #paste(round(quantile(HUC10.df$NetWB_ave,c(0.8),na.rm=T),digits=1),"MGD -",0,"MGD"),
                    #paste(0,"MGD -",round(quantile(HUC10.df$NetWB_ave,c(1),na.rm=T),digits=1),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
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

#--Refined Scale--#
NWB_Discrete<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#2b8cbe")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$NetWB_ave,breaks=c(quantile(HUC10.df$NetWB_ave,c(0),na.rm=T),-250,-150,-10,0,quantile(HUC10.df$NetWB_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
  scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete,
                    labels=c(paste(-1250,"-",-250),
                             paste(-250,"-",-150),
                             paste(-150,"-",-10),
                             paste(-10,"-",0),
                             paste(0,"<")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
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




#--By Sector--#
NWB_Discrete_sector<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#6baed6")
NWB_Discrete_sector<-c("#a50f15","#de2d26","#fb6a4a","#6baed6")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$NetWB_ave,breaks=c(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(1/4),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(2/4),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(3/4),na.rm=T),
                                             0,
                                             quantile(HUC10.df$NetWB_ave,c(4/4),na.rm=T)),
                  include.lowest=T),colour=""))+
  scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete_sector,
                    labels=c(paste(round(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(1/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_ave,c(1/4),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(2/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_ave,c(2/4),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(3/4),na.rm=T),digits=3),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_ave,c(3/4),na.rm=T),digits=3),"MGD -",0,"MGD"),
                             paste(0,"MGD -",round(quantile(HUC10.df$NetWB_ave,c(4/4),na.rm=T),digits=2),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
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

#--Energy--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$NetWB_ave,breaks=c(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(1/4),na.rm=T),
                                             quantile(HUC10.df$NetWB_ave,c(2/4),na.rm=T),
                                             0,
                                             quantile(HUC10.df$NetWB_ave,c(4/4),na.rm=T)),
                  include.lowest=T),colour=""))+
  scale_fill_manual(name="Net Water Balance (MGD)", values=NWB_Discrete_sector,
                    labels=c(paste(round(quantile(HUC10.df$NetWB_ave,c(0.0),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(1/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_ave,c(1/4),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$NetWB_ave,c(2/4),na.rm=T),digits=2),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_ave,c(2/4),na.rm=T),digits=2),"MGD -",0,"MGD"),
                             paste(0,"MGD -",round(quantile(HUC10.df$NetWB_ave,c(4/4),na.rm=T),digits=2),"MGD")),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
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

#---Consumption---#

#--Discrete--#

CU_Discrete<-c("#2b8cbe","#fcbba1","#fb6a4a","#de2d26","#a50f15")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill=cut(HUC10.df$Consumption_ave,breaks=c(quantile(HUC10.df$Consumption_ave,c(0.0),na.rm=T),0,0.25,0.5,0.75,1),include.lowest=T),colour=""))+
  scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                    labels=c(paste(round(quantile(HUC10.df$Consumption_ave,c(0.0),na.rm=T),digits=2),"-",0),
                             paste(0,"-",0.25),
                             paste(0.25,"-",0.5),
                             paste(0.5,"-",0.75),
                             paste(0.75,"-",1)),
                    na.value="transparent", drop=FALSE)+
  geom_polygon(
    data=HUC10.df,
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


#--By Sectors--#
CU_Discrete_sector<-c("#6baed6","#fcbba1","#fb6a4a","#de2d26","#a50f15")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill=cut(HUC10.df$Consumption_ave,breaks=c(quantile(HUC10.df$Consumption_ave,c(0.0),na.rm=T),
                                                  0,
                                                  quantile(HUC10.df$Consumption_ave,c(1/4),na.rm=T),
                                                  quantile(HUC10.df$Consumption_ave,c(2/4),na.rm=T),
                                                  quantile(HUC10.df$Consumption_ave,c(3/4),na.rm=T),
                                                  quantile(HUC10.df$Consumption_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
  scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete_sector, 
                    labels=c(paste(round(quantile(HUC10.df$Consumption_ave,c(0.0),na.rm=T),digits=2),"-",0),
                             paste(0,"-",round(quantile(HUC10.df$Consumption_ave,c(1/4),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_ave,c(1/4),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_ave,c(2/4),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_ave,c(2/4),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_ave,c(3/4),na.rm=T),digits=3)),
                             paste(round(quantile(HUC10.df$Consumption_ave,c(3/4),na.rm=T),digits=3),"-",round(quantile(HUC10.df$Consumption_ave,c(4/4),na.rm=T),digits=3))),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),colour="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)))+
  labs(title = "Average Consumption Coefficient 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Net Water Balance-------#

#--2010--#

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2010, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2011, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2012, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2013--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2013, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2014, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2015, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2016--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2016, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2017--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2017, colour=""))+
  scale_fill_gradient2("Net Water Balance (MGD)", 
                       mid="#fcbba1",low="#67000d", high="#9ecae1", na.value="transparent", 
                       limits=c(-2000,200),
                       oob=scales::squish, guide="colourbar")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent")))+
  labs(title = "Net Water Balance (MGD) by HUC10 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Consumption Plots-------#

#--2010--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2010),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2011),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2012),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2013--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2013),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2014),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2015),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


#--2016--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2016),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()


#--2017--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2017),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#252525", guide="colourbar")+
  labs(title = "Consumptive Use (%) by HUC10 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#######################################################################################################################
#-----------------------------Transfers----------------------------#

#----Long Term Average----#

#---Net Water Balance---#

#---Discrete---#
NWB_Discrete<-c("#a50f15","#de2d26","#fb6a4a","#fcbba1","#2b8cbe")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= cut(HUC10.df$NetWB_t_ave,breaks=c(quantile(HUC10.df$NetWB_t_ave,c(0.0),na.rm=T),
                                               quantile(HUC10.df$NetWB_t_ave,c(0.20),na.rm=T),
                                               quantile(HUC10.df$NetWB_t_ave,c(0.40),na.rm=T),
                                               quantile(HUC10.df$NetWB_t_ave,c(0.70),na.rm=T),
                                               0,
                                               quantile(HUC10.df$NetWB_t_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
  scale_fill_manual(name="Net Water Balance (MGD)", 
                    labels=c(paste(round(quantile(HUC10.df$NetWB_t_ave,c(0.0),na.rm=T),digits=2),"MGD -",round(quantile(HUC10.df$NetWB_t_ave,c(0.2),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_t_ave,c(0.2),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$NetWB_t_ave,c(0.4),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_t_ave,c(0.4),na.rm=T),digits=1),"MGD -",round(quantile(HUC10.df$NetWB_t_ave,c(0.7),na.rm=T),digits=1),"MGD"),
                             paste(round(quantile(HUC10.df$NetWB_t_ave,c(0.7),na.rm=T),digits=1),"MGD -",0,"MGD"),
                             paste(0,"MGD -",round(quantile(HUC10.df$NetWB_t_ave,c(1),na.rm=T),digits=1),"MGD")),
                    values=NWB_Discrete,na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent"),order=2))+
  labs(title = "Average Net Water Balance with Transfers (MGD) 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#---Consumption---#

#--Discrete--#

CU_Discrete<-c("#2171b5","#9ecae1","#fcbba1","#fc9272","#ef3b2c","#cb181d","#67000d")

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill=cut(HUC10.df$Consumption_t_ave,breaks=c(quantile(HUC10.df$Consumption_t_ave,c(0.0),na.rm=T),
                                                    quantile(HUC10.df$Consumption_t_ave,c(0.05),na.rm=T),
                                                    0,
                                                    quantile(HUC10.df$Consumption_t_ave,c(0.20),na.rm=T),
                                                    quantile(HUC10.df$Consumption_t_ave,c(0.40),na.rm=T),
                                                    quantile(HUC10.df$Consumption_t_ave,c(0.60),na.rm=T),
                                                    quantile(HUC10.df$Consumption_t_ave,c(0.80),na.rm=T),
                                                    quantile(HUC10.df$Consumption_t_ave,c(1),na.rm=T)),include.lowest=T),colour=""))+
  scale_fill_manual(name="Consumption Coefficient", values=CU_Discrete, 
                    labels=c(paste(round(quantile(HUC10.df$Consumption_t_ave,c(0.0),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_t_ave,c(0.05),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_t_ave,c(0.05),na.rm=T),digits=2),"-",0),
                             paste(0,"-",round(quantile(HUC10.df$Consumption_t_ave,c(0.20),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_t_ave,c(0.20),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_t_ave,c(0.40),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_t_ave,c(0.40),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_t_ave,c(0.60),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_t_ave,c(0.60),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_t_ave,c(0.80),na.rm=T),digits=2)),
                             paste(round(quantile(HUC10.df$Consumption_t_ave,c(0.80),na.rm=T),digits=2),"-",round(quantile(HUC10.df$Consumption_t_ave,c(1),na.rm=T),digits=2))),
                    na.value="transparent")+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group),color="#252525",fill="transparent")+
  scale_colour_manual(values=NA)+
  guides(fill=guide_legend(order=1),
         colour=guide_legend("No Data", override.aes = list(colour="black",fill="transparent",order=2)))+
  labs(title = "Average Consumption Coefficient with Transfers 2010-2017")+
  scale_bar(lon=-85,lat=36, distance_lon = 50, distance_lat = 20, distance_legend = 40, dist_unit = "km",
            arrow_length=75, arrow_distance = 60, arrow_north_size = 6)+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Net Water Balance-------#

#--2010--#

ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2010_t),color="#252525")+
  scale_fill_gradient("Net Water Balance (MGD)", 
                      high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2011--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2011_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2012--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2012_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2013--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2013_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2014--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2014_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2015--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2015_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2016--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2016_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#--2017--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= NetWB_2017_t),color="#252525")+
  scale_fill_continuous("Net Water Balance (MGD)", 
                        high="#fff5f0", low="#67000d", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Net Water Balance (MGD) with Transfers by HUC10 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()

#----Year by Year Consumption Plots-------#

#--2010--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2010_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2010")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2011--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2011_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2011")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2012--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2012_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2012")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2013--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2013_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2013")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2014--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2014_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2014")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2015--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2015_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2015")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2016--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2016_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2016")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



#--2017--#
ggplot()+
  geom_polygon(
    data=HUC10.df,
    aes(x=long, y= lat, group=group,
        fill= Consumption_2017_t),color="#252525")+
  scale_fill_continuous("Consumption Coefficient",
                        high="#a50f15", low="#fee5d9", space = "Lab", na.value="#bdbdbd", guide="colourbar")+
  labs(title = "Consumptive Use (%) with Transfers by HUC10 Watershed in 2017")+
  theme(line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())+coord_equal()



