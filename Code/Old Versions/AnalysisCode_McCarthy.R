#############################################################################################################################
###########################################Analysis Code#####################################################################

#Primary Author: Connor Brogan, M.S. Biological Systems Engineering, Virginia Tech
#Secondary Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

#############################################Purpose#########################################################################

#This script is intended to compare aggregated flows from CWA regulated facilities in the ECHO Database (ECHOInterface.R) to their respective 
#VPDES Individual Permitted flows. The Virginia Pollution Discharge Elimination System (VPDES) is administered 
#by Virginia's Department of Environmental Quality (VDEQ). Specifically, we are looking at Individual Permits 
#for municipal and industrial discharging facilities. An excel spreadsheet listing active VPDES Individual 
#Permits is found on the VDEQ website (https://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs).

#############################################################################################################################
############################################Library Initilization############################################################

library(foreign) #allows for easy manipulation of *.dbf file types
library(rgdal) #extract files from file geodatabases-like in ArcMap
library(dplyr)#data manipulation package that speeds up grouping, summarizing, ordering. 
library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(readxl) #reads excel files
library(jsonlite)
library(httr)
library(proj4)
library(data.table)

#############################################################################################################################
##################################################Inputs#####################################################################

#Required inputs: 
    #State: abbreviated 
    #Aggregated State Discharge Data extracted from ECHO REST Services-from ECHOInterface.R script
    #Withdrawing Facility Information available from DEQ Website

state<-"VA"
path<-"H:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated"

##################ECHO Discharge Data#####################

FlowFrame<-read.csv(paste0(path,"/FlowFrame_2012_present.csv"),stringsAsFactors = F)

FlowFrame_mon_mgd<-subset(FlowFrame, FlowFrame$Stat=="MK")

#Query from CWA ECHO REST Services to obtain all discharging facilities within state of interest
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
ECHO_Fac<-read.csv(uri_summary,stringsAsFactors = F)

#Download statistical codes from ECHO
CodeKey<-read.csv("https://echo.epa.gov/system/files/REF_ICIS-NPDES_STATISTICAL_BASE.csv",stringsAsFactors = F,na.strings = 'BLANK')

#################VPDES Permit Data#######################
#This section of code retrieves the coordinates and flow limits for each point source discharging facility with an individual VPDES permit. 
#General permits are written for a general class of discharges, while individual permits are for municipal and industrial facilities with effluent limitations and monitoring requirements.
#Coordinates for each unique outfall is found in the VDEQ's VEGIS Database ("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/"). 
#Permitted flow limits are found on the VDEQ website under Permitting & Compliance (https://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs)

#######Retrieve Coordinates of VPDES Outfalls#######
temp<-tempfile(fileext = ".zip")
#Locations and attribute data about active outfalls in the State
download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip", destfile = temp)
unzip(temp, exdir = "path")
#Explore what is in VPDES_Geodatabase.gdb
ogrListLayers(paste0(path,"/VPDES_Geodatabase.gdb")) #Two layers: VPDES Outfalls and OpenFileGDB
VPDES_Outfalls<-as.data.frame(readOGR(paste0(path,"/VPDES_Geodatabase.gdb"),layer="VPDES_OUTFALLS"))
names(VPDES)[names(VPDES)=="OUTFALL_ID"]<-'VPDESID'
VPDES_Outfalls<-VPDES[VPDES$VAP_TYPE=='VPDES_IP',] #Narrow down to Individual Permits: which specify effluent limitations for municipal and industrial facilities. 
names(ECHO_Fac)[names(ECHO_Fac)=="SourceID"]<-"VAP_PMT_NO"#Need to rename to give a central columnn name for future joins

#######Retrieve VPDES Permit Limits from DEQ Website#######
#Website that contains VPDES Individual Permits: http://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs

#Individual Permits updated as of March 2018
GET('http://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contact%20Flow%20for%20WEB%20March%202018.xls?ver=2018-03-13-170732-267', write_disk(temp <- tempfile(fileext = ".xls")))
VPDES_Limits <- read_excel(temp, skip=5)
VPDES_Limits<-VPDES_Limits[!is.na(VPDES_Limits$Facility),]
VPDES_Limits$`Design Flow (MGD)`<-as.numeric(VPDES_Limits$`Design Flow (MGD)`)
length(unique(VPDES_Limits$`Permit Number`))#865 Unique IP's as of March 2018

#However we see we have a lot of duplicate entries for the same permit because of multiple contacts
duplicated(VPDES_Limits$`Permit Number`)
length(VPDES_Limits$Facility[duplicated(VPDES_Limits$`Permit Number`)]) #377 duplicates
VPDES_Limits<-VPDES_Limits[!duplicated(VPDES_Limits$`Permit Number`),] #getting rid of duplicates and looking at unique permits

sum(VPDES_Limits$`Design Flow (MGD)`,na.rm=T)
max(VPDES_Limits$`Design Flow (MGD)`,na.rm=T)
length(VPDES_Limits$`Design Flow (MGD)`[is.na(VPDES_Limits$`Design Flow (MGD)`)])
sum(VPDES_Limits$`Total Flow`,na.rm=T)
max(VPDES_Limits$`Total Flow`,na.rm=T)
length(VPDES_Limits$`Total Flow`[is.na(VPDES_Limits$`Total Flow`)])
length(VPDES_Limits$Facility[VPDES_Limits$`Design Flow (MGD)`==0])

VPDES_Limits_Active<-VPDES_Limits %>% filter(`Date: Permit Expiration`>=as.POSIXct(Sys.Date()))
sum(VPDES_Limits_Active$`Design Flow (MGD)`,na.rm=T)
max(VPDES_Limits_Active$`Design Flow (MGD)`,na.rm=T)
length(VPDES_Limits_Active$`Design Flow (MGD)`[is.na(VPDES_Limits_Active$`Design Flow (MGD)`)])
sum(VPDES_Limits_Active$`Total Flow`,na.rm=T)
max(VPDES_Limits_Active$`Total Flow`,na.rm=T)
length(VPDES_Limits_Active$`Total Flow`[is.na(VPDES_Limits_Active$`Total Flow`)])
length(VPDES_Limits_Active$Facility[VPDES_Limits_Active$`Design Flow (MGD)`==0])


rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID,state,temp)#Remove clutter

#############################################################################################################################
################################################################################################################################
#Set initial projections for VPDES coordinates#
VPDES_Coordinates <- data.frame(x=VPDES_Outfalls$coords.x1, y=VPDES_Outfalls$coords.x2)
proj4string <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

#Transform VPDES data and store within VPDES_Outfalls
pj <- proj4::project(VPDES_Coordinates, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
VPDES_Outfalls$coords.x1<-latlon$lon
VPDES_Outfalls$coords.x2<-latlon$lat


for (i in 1:length(ECHO_Fac$CWPName)){
  json_file<-paste0("https://ofmpub.epa.gov/echo/dfr_rest_services.get_dfr?output=JSON&p_id=",ECHO_Fac$VAP_PMT_NO[i])
  json_data<-fromJSON(txt=json_file)
  if(length(json_data$Results$SpatialMetadata$Latitude83)>0){
    ECHO_Fac$Faclat[i]<-json_data$Results$SpatialMetadata$Latitude83
    ECHO_Fac$Faclong[i]<-json_data$Results$SpatialMetadata$Longitude83
  } else {
    ECHO_Fac$Faclat[i]<-NA
    ECHO_Fac$Faclong[i]<-NA
  }
  print(paste0("Processing SourceID: ",ECHO_Fac$VAP_PMT_NO[i]," (",i," of ",length(ECHO_Fac$CWPName),")"))
}

#Assign design flows to VPDES outfalls using the VPDES information spreadsheet
#is this a reasonable assumption though?
#I feel like the values in the VPDES spreadsheet represent the design and total flows for the entire facility. This would multiply the flows by however many outfalls there are. 
for (i in 1:length(VPDES_Outfalls$VAP_PMT_NO)){
  VPDES_Outfalls$DesFlow[i]<-NA
  if (length(VPDES_Limits$`Design Flow Null`[VPDES_Limits$`Permit Number`==VPDES_Outfalls$VAP_PMT_NO[i]])>0){
    VPDES_Outfalls$DesFlow[i]<-VPDES_Limits$`Design Flow Null`[VPDES_Limits$`Permit Number`==VPDES_Outfalls$VAP_PMT_NO[i]]  
  }
  VPDES_Outfalls$TotalFlow[i]<-NA
  if (length(VPDES_Limits$`Total Flow Null`[VPDES_Limits$`Permit Number`==VPDES_Outfalls$VAP_PMT_NO[i]])>0){
    VPDES_Outfalls$TotalFlow[i]<-VPDES_Limits$`Total Flow Null`[VPDES_Limits$`Permit Number`==VPDES_Outfalls$VAP_PMT_NO[i]]  
  }
}

#Analysis of ECHO data, providing all values for all statistics reported for a given facility
#First, flip flow frame to show statistical codes in columns rather than rows
FlowFrameFlipped<-FlowFrame[,c('VPDESID','FlowMed','Limit','Code')]
colnames(FlowFrameFlipped)[2]<-'MedFlow'
FlowFrameFlipped<-reshape(FlowFrameFlipped,idvar='VPDESID',timevar = 'Code',direction='wide')
#Add in VPDES IDs for each outfall
for (i in 1:length(FlowFrameFlipped$VPDESID)){
  FlowFrameFlipped$ECHOID[i]<-as.character(FlowFrame$ECHOID[FlowFrame$VPDESID==FlowFrameFlipped$VPDESID[i]][1])
}
#Basic functions for analysis
#'plus' is a modified sum function that will reveal if all values in a vector are NA
plus<-function(x){
  if(all(is.na(x))){
    c(NA)
  }else{
    sum(x,na.rm = TRUE)}
}
#'NAcount' counts the number of NAs in a vector
NAcount<-function(x){
  sum(is.na(x))
}
#Use VPDESID to provide a center for an inner join such that 
#data frame 'All' contains every outfall from ECHO and VPDES
All<-merge(VPDES_Outfalls,FlowFrameFlipped,by="VPDESID",all=T)
#Add an identifier so that each outfall's facility ID is held in a single column
for (i in 1:length(All$VPDESID)){
  if(is.na(All$VAP_PMT_NO[i])){
    All$FacilityID[i]<-as.character(All$ECHOID[i])
  }else{
    All$FacilityID[i]<-as.character(All$VAP_PMT_NO[i])
  }
}
#Data reclassification to avoid factors and character classes being assigned to numbers
All$VAP_PMT_NO<-as.character(All$VAP_PMT_NO)
All$DesFlow<-as.numeric(All$DesFlow)
All$TotalFlow<-as.numeric(All$TotalFlow)
#Create a data frame containing data in 'All' aggregated to the facility level
#Thus, data in AllFacs shows a facility ID and then VPDES flow and ECHO stats summed to the
#facility level. It also counts the number of outfalls at each facility reporting NA
AllFacs<-as.data.frame(All %>% group_by(FacilityID) %>% summarize_at(vars(19:60),funs(plus,NAcount)))
#Organize data such that all similar statistics are reported together
headers<-as.character(unique(FlowFrame$Code))
order<-numeric(0)
for (i in 1:length(headers)){
  orderi<-grep(headers[i],colnames(AllFacs))#reorganize by searching for a stat in column headers and grouping together those that have the same
  order<-c(order,orderi)
}
allcols<-seq(1,length(colnames(AllFacs)))
order<-c(allcols[!(allcols %in% order)],order)#Ensure that all columns are present
AllFacs<-AllFacs[,order]#Reorganize by the stat order developed in the immediate preceeding for loop

#Add in facility names lat/long, and number of outfalls from each dataset looking at ECHO first to find data than VPDES (indicated by source data)
for (i in 1:length(AllFacs$FacilityID)){
  AllFacs$FacilityName[i]<-NA
  if(length(ECHO_Fac$CWPName[ECHO_Fac$VAP_PMT_NO==AllFacs$FacilityID[i]])>0){#If the current examined facility is in the list of ECHO facilities, note that
    AllFacs$SourceData[i]<-'ECHO'
    AllFacs$FacilityName[i]<-ECHO_Fac$CWPName[ECHO_Fac$VAP_PMT_NO==AllFacs$FacilityID[i]]
  }
  if(is.na(AllFacs$FacilityName[i])){#If its not, than the facility is VPDES exclusive. Make note of that
    AllFacs$FacilityName[i]<-as.character(All$FAC_NAME[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
    AllFacs$SourceData[i]<-'VPDES'
  }
  #Find the number of data reporting outfalls in VPDES and ECHO
  AllFacs$ECHOOutfalls[i]<-sum(!(is.na(All$ECHOID[All$FacilityID==AllFacs$FacilityID[i]])))
  AllFacs$TotalOutfalls[i]<-length(All$FacilityID[All$FacilityID==AllFacs$FacilityID[i]])#Find the total number of outfalls
  AllFacs$VPDESOutfalls[i]<-length(VPDES$VAP_PMT_NO[VPDES$VAP_PMT_NO==AllFacs$FacilityID[i]])
}
for (i in 1:length(AllFacs$FacilityID)){
  if(AllFacs$SourceData[i]=='ECHO'){#Based on value of source data noted earlier, find lat/long. VPDES lat/long may need to be reprojected, so check ECHO first
    AllFacs$lat[i]<-ECHO_Fac$Faclat[ECHO_Fac$VAP_PMT_NO==AllFacs$FacilityID[i]]
    AllFacs$lon[i]<-ECHO_Fac$Faclong[ECHO_Fac$VAP_PMT_NO==AllFacs$FacilityID[i]]
  }else{
    AllFacs$lat[i]<-as.numeric(All$coords.x1[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
    AllFacs$lon[i]<-as.numeric(All$coords.x1[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
  }
  AllFacs$VPDES_DesFlow[i]<-All$DesFlow[All$FacilityID==AllFacs$FacilityID[i]][1]
  AllFacs$VPDES_TotalFlow[i]<-All$TotalFlow[All$FacilityID==AllFacs$FacilityID[i]][1]
}
diff<-numeric(0)
ind<-numeric(0)
for (i in 1:length(ECHO_Fac$CWPActualAverageFlowNmbr)){
  if(!is.na(ECHO_Fac$CWPActualAverageFlowNmbr[i])){
    if(length(AllFacs$VPDES_DesFlow[AllFacs$FacilityID==ECHO_Fac$VAP_PMT_NO[i]])>0){
      store<-ECHO_Fac$CWPActualAverageFlowNmbr[i]==AllFacs$VPDES_DesFlow[AllFacs$FacilityID==ECHO_Fac$VAP_PMT_NO[i]] 
      diff<-c(diff,store)
      ind<-c(ind,i)
    }
  }
}
#Reorder data such that statistics are reported after basic facility information
order<-c(1,seq(length(colnames(AllFacs))-8,length(colnames(AllFacs))),seq(2,length(colnames(AllFacs))-9))#May need manual adjustment if data changes. Basic reorganize
AllFacs<-AllFacs[,order]#Reorganize so that facility info presents before statistics
AllFacs_test<-AllFacs[order(AllFacs$MedFlow.MK_plus-AllFacs$VPDES_DesFlow,decreasing=T),]#Order by largest ECHO/VPDES discrepencies first-removes everything for some reason
rm(order,orderi,i,headers,allcols)

#Provide a summary on number of reporting facilities, value of statistics, etc.
#only interested in ECHO statistics
order<-grep('plus',colnames(AllFacs))#Only want those that summed data (i.e. not facility info or NA counts)
PlusFacs<-AllFacs[,order]
FacSummary<-data.frame(Stat=colnames(PlusFacs),StatCode=character(length(colnames(PlusFacs))),Description=character(length(colnames(PlusFacs))),Present=numeric(length(colnames(PlusFacs))))
FacSummary$Stat<-as.character(FacSummary$Stat);FacSummary$StatCode<-as.character(FacSummary$StatCode);FacSummary$Description<-as.character(FacSummary$Description)
#A simple lopp to extract code name and the number/value of facilities collectivley reporting it
for (i in 1:length(colnames(PlusFacs))){
  column<-as.vector(PlusFacs[,i])
  FacSummary$StatCode[i]<-gsub(".*[.]([^_]+)[_].*","\\1",FacSummary$Stat[i])#Extract stat using glob notation
  FacSummary$Description[i]<-NA
  if(FacSummary$StatCode[i] %in% CodeKey$STATISTICAL_BASE_CODE){#Add a description of a code where possible
  FacSummary$Description[i]<-CodeKey$STATISTICAL_BASE_LONG_DESC[CodeKey$STATISTICAL_BASE_CODE==FacSummary$StatCode[i]]
  }
  FacSummary$Facility_Count[i]<-sum(!(is.na(column)))#Provide the count of how many facilities are reporting this given statistic
  FacSummary$SumValue[i]<-plus(column)#Sum the column to show overall value of stat
}
#Create separate tables for flows and limits
order<-grep('Limit',as.character(FacSummary$Stat))
FacSummaryLimits<-FacSummary[order,]
FacSummaryFlow<-FacSummary[-order,]
rm(column)
#Quick means of finding number of total reported statistcs per facility
order<-c(1,2,grep('plus',colnames(AllFacs)))
order2<-grep('Limit',colnames(AllFacs[,order]))
order<-order[!(order %in% order2)]#Extract facility and outfall ID along with all summed stat columns
test<-AllFacs[,order]
test<-test[,-3]
for (i in 1:length(test$FacilityID)){
  test$StatTotal[i]<-rowSums(!is.na(test[i,3:length(colnames(test))]))#find how many stats a given facility is reporting
}
for (i in 1:length(AllFacs$FacilityName)){
  AllFacs$FlowStatTotal[i]<-test$StatTotal[test$FacilityID==AllFacs$FacilityID[i]]#Add the total number of columns onto the AllFacs output
}
rm(test,order,order2)


write.csv(AllFacs,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/2017 ECHO/VPDESvsECHO_.csv")


###########################################################################################################
##QA/QC Measures


#ECHO Flow > VPDES Flow

#So now here is the big question: do we look at monthly average to represent the ECHO flow for each facility? 
#What about the facilities that don't have a monthly average and have something else???

###Total Flow###
ECHO_Greater_Than_VPDES<-data.frame(FacilityID=AllFacs$FacilityID[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_TotalFlow)=="TRUE")],FacilityName=AllFacs$FacilityName[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_TotalFlow)=="TRUE")],ECHO_Flow_MGD=AllFacs$MedFlow.MK_plus[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_TotalFlow)=="TRUE")],VPDES_Total_Flow_MGD=AllFacs$VPDES_TotalFlow[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_TotalFlow)=="TRUE")])
ECHO_Greater_Than_VPDES<-ECHO_Greater_Than_VPDES[order(ECHO_Greater_Than_VPDES$VPDES_Total_Flow_MGD-ECHO_Greater_Than_VPDES$ECHO_Flow_MGD,decreasing=F),]

write.csv(ECHO_Greater_Than_VPDES,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/2017 ECHO/ECHO_Greater_Than_VPDES.csv")

###Design Flow###
ECHO_Greater_Than_VPDESDes<-data.frame(FacilityID=AllFacs$FacilityID[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_DesFlow)=="TRUE")],FacilityName=AllFacs$FacilityName[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_DesFlow)=="TRUE")],ECHO_Flow_MGD=AllFacs$MedFlow.MK_plus[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_DesFlow)=="TRUE")],VPDES_Des_Flow_MGD=AllFacs$VPDES_TotalFlow[which((AllFacs$MedFlow.MK_plus>AllFacs$VPDES_DesFlow)=="TRUE")])
ECHO_Greater_Than_VPDESDes<-ECHO_Greater_Than_VPDESDes[order(ECHO_Greater_Than_VPDESDes$VPDES_Des_Flow_MGD-ECHO_Greater_Than_VPDESDes$ECHO_Flow_MGD,decreasing=F),]

write.csv(ECHO_Greater_Than_VPDESDes,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/2017 ECHO/ECHO_Greater_Than_VPDESDes.csv")
