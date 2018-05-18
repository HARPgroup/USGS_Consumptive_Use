#Require flow frame flipped to be generated from ECHOInterface.R
#######################
#Library Initialization
library(foreign)
library(rgdal)
library(dplyr)
library(XML)
library(RCurl)
library(readxl)
library(jsonlite)
library(httr)

#Required inputs: State and Flow frame from ECHO
#input/output path will also be required as the script needs a place to store downloads from VPDES and read in flow frame\
state<-"VA"
Inputpath<-"G:\\My Drive\\USGS_ConsumptiveUse\\Spring Semester, 2018\\Connor\\USGS Testing"
Outputpath<-"C:/Users/connorb5/Desktop/GitHub/USGS_Consumptive_Use/Documentation/Imports/"
FlowFrame<-read.csv(paste0(Inputpath,"/2016 ECHO/FlowFrameMedSumNoDis2016.csv"),stringsAsFactors = F)

#Get ECHO Facility List and store in dataframe 'a'
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
a<-read.csv(uri_summary,stringsAsFactors = F)
a$CWPName<-toupper(a$CWPName)#Ensure all facility names are in all caps using "toupper" command
#Download list of active VPDES outfalls and separate individual permits
temp<-tempfile()
download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip",temp)
unzip(temp,exdir=Inputpath)
VPDES<-as.data.frame(readOGR(paste0(Inputpath,"/VPDES_Geodatabase.gdb"),layer="VPDES_OUTFALLS"))
names(VPDES)[names(VPDES)=="OUTFALL_ID"]<-'VPDESID'
VPDES_IP<-VPDES[VPDES$VAP_TYPE=='VPDES_IP',]
#Download list of VPDES design flows
GET('http://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20Active%20IP%20Nov%202017.xls?ver=2017-11-14-152041-490', write_disk(temp <- tempfile(fileext = ".xls")))
VPDESFlows <- read_excel(temp,skip=9)
VPDESFlows<-VPDESFlows[!is.na(VPDESFlows$Facility),]
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID,state,temp)#Remove clutter
################################################################################################################################
#Set initial projections for VPDES coordinates
d <- data.frame(x=VPDES_IP$coords.x1, y=VPDES_IP$coords.x2)
proj4string <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

#Transform VPDES data and store within VPDES_IP
pj <- proj4::project(d, proj4string, inverse=TRUE)
latlon <- data.frame(lat=pj$y, lon=pj$x)
VPDES_IP$coords.x1<-latlon$lon
VPDES_IP$coords.x2<-latlon$lat

#Assign design flows to VPDES outfalls using the VPDES information spreadsheet
for (i in 1:length(VPDES_IP$VAP_PMT_NO)){
  VPDES_IP$DesFlow[i]<-NA
  if (length(VPDESFlows$`Design Flow Null`[VPDESFlows$`Permit Number`==VPDES_IP$VAP_PMT_NO[i]])>0){
    VPDES_IP$DesFlow[i]<-VPDESFlows$`Design Flow Null`[VPDESFlows$`Permit Number`==VPDES_IP$VAP_PMT_NO[i]]  
  }
  VPDES_IP$TotalFlow[i]<-NA
  if (length(VPDESFlows$`Total Flow Null`[VPDESFlows$`Permit Number`==VPDES_IP$VAP_PMT_NO[i]])>0){
    VPDES_IP$TotalFlow[i]<-VPDESFlows$`Total Flow Null`[VPDESFlows$`Permit Number`==VPDES_IP$VAP_PMT_NO[i]]  
  }
}

#Analysis of ECHO data, providing all values for all statistics reported for a given facility
#First, flip flow frame to show statistical codes in columns rather than rows
FlowFrameFlipped<-FlowFrame[,c('VPDESID','FlowMed','Limit','Code')]
colnames(FlowFrameFlipped)[2]<-'Flow'
FlowFrameFlipped<-reshape(FlowFrameFlipped,idvar='VPDESID',timevar = 'Code',direction='wide')
#Add in VPDES IDs for each outfall
for (i in 1:length(FlowFrameFlipped$VPDESID)){
  FlowFrameFlipped$ECHOID[i]<-as.character(FlowFrame$ECHOID[FlowFrame$VPDESID==FlowFrameFlipped$VPDESID[i]][1])
}
#Use VPDESID to provide a center for an inner join such that 
#data frame 'All' contains every outfall from ECHO and VPDES
All<-merge(VPDES_IP,FlowFrameFlipped,by="VPDESID",all=T)
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

###########Imports#####################################################################################################
#Create empty vectors in the facility list to store attributes of interested from ECHO's Detailed Facility Report database
a$Faclat[i]<-NA
a$Faclong[i]<-NA
a$Status[i]<-NA
a$PermitExpDate[i]<-NA
a$LastInspection[i]<-NA
a$CSOFlg[i]<-NA
a$CWPCso[i]<-NA
a$RecievingWB[i]<-NA
a$RecievingReachCode[i]<-NA
a$WBDesignatedUse[i]<-NA
#Run through each facility in a for loop, accessing its detailed facility report and storing data of interest
#Many if statements are present that check to ensure data is present before having R access it. This prevents
#empty data from throwing errors in the code
for (i in 1:length(a$CWPName)){
  print(paste0("Processing SourceID: ",a$VAP_PMT_NO[i]," (",i," of ",length(a$CWPName),")"))
  json_file<-paste0("https://ofmpub.epa.gov/echo/dfr_rest_services.get_dfr?output=JSON&p_id=",a$SourceID[i])
  json_data<-fromJSON(txt=json_file)
  
  if(length(json_data$Results$SpatialMetadata$Latitude83)>0){
    a$Faclat[i]<-json_data$Results$SpatialMetadata$Latitude83
    a$Faclong[i]<-json_data$Results$SpatialMetadata$Longitude83
  } else {
    a$Faclat[i]<-NA
    a$Faclong[i]<-NA
  }
  
  if(length(json_data$Results$Permits$Statute[json_data$Results$Permits$Statute=="CWA"])>0){
    indexCWA<-which(json_data$Results$Permits$Statute=="CWA")
    a$Status[i]<-json_data$Results$Permits$FacilityStatus[indexCWA]
    a$PermitExpDate[i]<-json_data$Results$Permits$ExpDate[indexCWA]
  } else {
    a$Status[i]<-NA
    a$PermitExpDate[i]<-NA
  }
  
  if(length(json_data$Results$EnforcementComplianceSummaries$Summaries$Statute[json_data$Results$EnforcementComplianceSummaries$Summaries$Statute=="CWA"])>0){
    indexCWA<-which(json_data$Results$EnforcementComplianceSummaries$Summaries$Statute=="CWA")
    a$LastInspection[i]<-json_data$Results$EnforcementComplianceSummaries$Summaries$LastInspection[indexCWA]
  } else {
    a$LastInspection[i]<-NA
  }
  
  if(length(json_data$Results$WaterQuality$Sources$SourceID)>0){
    a$CSOFlg[i]<-json_data$Results$WaterQuality$Sources$CSS
    a$CWPCso[i]<-json_data$Results$WaterQuality$Sources$CWPCsoOutfalls
    a$RecievingWB[i]<-json_data$Results$WaterQuality$Sources$RadGnisName
    a$RecievingReachCode[i]<-json_data$Results$WaterQuality$Sources$RadReachcode
    a$WBDesignatedUse[i]<-paste(json_data$Results$WaterQuality$Sources$AttainsCauseGroups,collapse = '_')
  } else {
    a$CSOFlg[i]<-NA
    a$CWPCso[i]<-NA
    a$RecievingWB[i]<-NA
    a$RecievingReachCode[i]<-NA
    a$WBDesignatedUse[i]<-NA
  }
}
a$address<-paste0(a$CWPStreet,'; ',a$CWPCity)

#Facility Generation
#This section generates the facility import in VA Hydro. There is a long series of "if" checks
#that format facility names and search them for any buzz words that might help group them into 
#a VA Hydro ftype. For instance, "Surry Power Station" contains the word "Power" and is likely a 
#power plant of some kind
facilities<-data.frame(bundle='facility',name=a$CWPName)
facilities$ftype<-'unknown'
facilities$hydrocode<-paste0("echo_",a$SourceID)
for (i in 1:length(facilities$hydrocode)){
  if (length(grep('WTP',a$CWPName[i]))>0|
      length(grep('WASTE WATER',a$CWPName[i]))>0|
      length(grep('WATER TREATMENT PLANT',a$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT PLANT',a$CWPName[i]))>0|
      length(grep('WATER RECLAMATION',a$CWPName[i]))>0|
      length(grep('WTF',a$CWPName[i]))>0|
      length(grep('STP',a$CWPName[i]))>0|
      length(grep('SEW. TREAT. PLANT',a$CWPName[i]))>0|
      length(grep('WWTREAT PLANT',a$CWPName[i]))>0|
      length(grep('WATER TREATMEN',a$CWPName[i]))>0|
      length(grep('WATER TREATMENT PL',a$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT',a$CWPName[i]))>0|
      length(grep('POLLUTION CONTROL',a$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT PLAN',a$CWPName[i]))>0|
      length(grep('POLLUTION CONTR',a$CWPName[i]))>0|
      length(grep('WASTEWATE',a$CWPName[i]))>0|
      length(grep('WT PLANT',a$CWPName[i]))>0){
    facilities$ftype[i]<-'wwtp'
  } else if (length(grep('COMBINED SEW SYSTEM',a$CWPName[i]))|
             length(grep('COMBINED SEWER SYSTEM',a$CWPName[i]))>0){
    facilities$ftype[i]<-'public water supply'
  } else if (length(grep("POWER",a$CWPName[i]))>0|
             length(grep("ENERGY CENTER",a$CWPName[i]))>0|
             length(grep("ELECTRIC",a$CWPName[i]))>0){
    facilities$ftype[i]<-'fossilpower'
    if(length(grep("NUCLEAR",a$CWPName[i]>0))){
      facilities$ftype[i]<-"nuclearpower"
    } else if(length(grep("HYDRO",a$CWPName[i]>0))){
      facilities$ftype[i]<-"hydropower"
    }
  } else if(length(grep('NUCLEAR',a$CWPName[i]))>0){
    facilities$ftype[i]<-'nuclearpower' 
  }else if (length(grep("MINE",a$CWPName[i]))>0|
            length(grep("QUARRY",a$CWPName[i]))>0|
            length(grep("MINING",a$CWPName[i]))>0){
    facilities$ftype[i]<-'mining'
  }else if (length(grep("MS4",a$CWPName[i]))>0|
           length(grep("SCHOOL",a$CWPName[i]))>0|
           length(grep("TOWN",a$CWPName[i]))>0|
           length(grep("COMMUNITY",a$CWPName[i]))>0|
           length(grep('ARMY',a$CWPName[i]))>0|
           length(grep('NAVY',a$CWPName[i]))>0|
           length(grep("COUNTY",a$CWPName[i]))>0|
           length(grep("HOME",a$CWPName[i]))>0|
           length(grep("CHURCH",a$CWPName[i]))>0|
           length(grep("HOMES",a$CWPName[i]))>0|
           length(grep("MUSEUM",a$CWPName[i]))>0|
           length(grep("ESTATES",a$CWPName[i]))>0|
           length(grep("CAR WASH",a$CWPName[i]))>0|
           length(grep("LANDING",a$CWPName[i]))>0|
           length(grep("CORRECTION CENTER",a$CWPName[i]))>0|
           length(grep("DETENTION CENTER",a$CWPName[i]))>0|
           length(grep("CORRECTIONAL CENTER",a$CWPName[i]))>0|
           length(grep("CORRECTIONAL UNIT",a$CWPName[i]))>0|
           length(grep("VILLAGE",a$CWPName[i]))>0|
           length(grep("UNIVERSITY",a$CWPName[i]))>0|
           length(grep("HOSPITAL",a$CWPName[i]))>0|
           length(grep("RESTAURANT",a$CWPName[i]))>0|
           length(grep('AUTHORITY',a$CWPName[i]))>0|
           length(grep('TUNNEL',a$CWPName[i]))>0|
           length(grep("CITY OF",a$CWPName[i]))>0){
    facilities$ftype[i]<-'municipal'
  } else if(length(grep("FARM",a$CWPName[i]))>0|
            length(grep("FISH CULTURAL",a$CWPName[i]))>0){
    facilities$ftype[i]<-"agriculture"
  }else if(length(grep("CORPORATION",a$CWPName[i]))>0|
            length(grep("INC.",a$CWPName[i]))>0|
            length(grep("INCORPORATED",a$CWPName[i]))>0|
            length(grep('CORP.',a$CWPName[i]))>0|
            length(grep('LLC',a$CWPName[i]))>0|
            length(grep('AIRPORT',a$CWPName[i]))>0|
            length(grep("COMPANY",a$CWPName[i]))>0){
    facilities$ftype[i]<-'commercial'
  } else if(length(grep('PAPER',a$CWPName[i]))>0|
            length(grep('COMPANY',a$CWPName[i]))>0|
            length(grep('CONCRETE',a$CWPName[i]))>0|
            length(grep('WOOD',a$CWPName[i]))>0|
            length(grep('LUMBER',a$CWPName[i]))>0|
            length(grep('MOTORS',a$CWPName[i]))>0|
            length(grep('PRODUCTS',a$CWPName[i]))>0|
            length(grep('TIMBER',a$CWPName[i]))>0|
            length(grep('CHEMICAL',a$CWPName[i]))>0|
            length(grep('INDUSTRIES',a$CWPName[i]))>0|
            length(grep('INDUSTRIAL PARK',a$CWPName[i]))>0){
    facilities$ftype[i]<-'industrial'
  }  else if(length(grep('PLANT',a$CWPName[i]))>0|
             length(grep('MANUFACTURING',a$CWPName[i]))>0){ 
    facilities$ftype[i]<-'manufacturing'
  }
  facilities$fstatus[i]<-'inactive'
  if (a$SourceID[i]%in%FlowFrameFlipped$ECHOID){
    facilities$fstatus[i]<-'active'
  }
  if(!is.na(a$Faclat[i]) & !is.na(a$Faclong[i])){
    facilities$wkt_geom[i]<-paste0('POINT (',a$Faclong[a$SourceID==a$SourceID[i]],' ',a$Faclat[a$SourceID==a$SourceID[i]],')')
  } else {
    lat<-All$coords.x2[All$FacilityID==All$FacilityID[i]]
    long<-All$coords.x1[All$FacilityID==All$FacilityID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        facilities$wkt_geom[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        facilities$wkt_geom[i]<-'NULL'
      }
    }
  }
}
write.table(facilities,paste0(Outputpath,"facilities.txt"),sep="\t",row.names = F)

#Release Point Generation
#Generation of the release point imports. In essence, this portion just formats various data from both the facility
#and the outfall list (All) to create the release point attributes and geometry.
All$VPDESID<-as.character(All$VPDESID)
All$FacilityID<-as.character(All$FacilityID)
releasepoint<-data.frame(bundle=rep('transfer',length(All$VPDESID)))
for (i in 1:length(releasepoint$bundle)){
  releasepoint$name[i]<-paste0('TO ',All$VPDESID[i])
  releasepoint$ftype[i]<-'release'
  releasepoint$hydrocode[i]<-paste0('vahydro_',All$VPDESID[i])
  if(All$VPDESID[i]%in%FlowFrame$VPDESID){
    releasepoint$fstatus[i]<-'active'  
  } else {
    releasepoint$fstatus[i]<-'inactive'
  }
  if(!is.na(a$Faclat[a$SourceID==All$FacilityID[i]]) & !is.na(a$Faclong[a$SourceID==All$FacilityID[i]])){
    releasepoint$wkt_geom[i]<-paste0('POINT (',a$Faclong[a$SourceID==All$FacilityID[i]],' ',a$Faclat[a$SourceID==All$FacilityID[i]],')')
  } else {
    lat<-All$coords.x2[All$FacilityID==All$FacilityID[i]]
    long<-All$coords.x1[All$FacilityID==All$FacilityID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        releasepoint$wkt_geom[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        releasepoint$wkt_geom[i]<-'NULL'
      }
    }
  }
  releasepoint$dh_link_facility_mps[i]<-paste0('echo_',All$FacilityID[i])
}
write.table(releasepoint,paste0(Outputpath,"releasepoint.txt"),sep="\t",row.names = F)

#Conveyance Generation
#Generates the conveyance import using the outfall list in 'All'
conveyance<-data.frame(bundle=rep('conveyance',length(All$VPDESID)))
for (i in 1:length(conveyance$bundle)){
  conveyance$name[i]<-paste0(All$FacilityID[i],' TO ',All$VPDESID[i])
  conveyance$ftype[i]<-'water_transfer'
  conveyance$hydrocode[i]<-paste0('vahydro_',All$FacilityID[i],'_',All$VPDESID[i])
  if(All$VPDESID[i]%in%FlowFrame$VPDESID){
    conveyance$fstatus[i]<-'active'  
  } else {
    conveyance$fstatus[i]<-'inactive'
  }
  conveyance$from_node[i]<-paste0('vahydro_',All$VPDESID[i])
  conveyance$to_node[i]<-paste0('echo_',All$VPDESID[i])
}
write.table(conveyance,paste0(Outputpath,"conveyance.txt"),sep="\t",row.names = F)

#Outfall Generation
#Reformats 'All' using available VPDES or ECHO geometry data and ECHO attributes
outfalls<-data.frame(bundle=rep('transfer',length(All$VPDESID)))
for (i in 1:length(outfalls$bundle)){
  outfalls$name[i]<-paste0('FROM ',All$FacilityID[i])
  outfalls$ftype[i]<-'outfall'
  outfalls$hydrocode[i]<-paste0('echo_',All$VPDESID[i])
  if(All$VPDESID[i]%in%FlowFrame$VPDESID){
    outfalls$fstatus[i]<-'active'  
  } else {
    outfalls$fstatus[i]<-'inactive'
  }
  if(!is.na(All$coords.x2[All$VPDESID==All$VPDESID[i]]) & !is.na(All$coords.x1[All$VPDESID==All$VPDESID[i]])){
    outfalls$wkt_geom[i]<-paste0('POINT (',All$coords.x1[All$VPDESID==All$VPDESID[i]],' ',All$coords.x2[All$VPDESID==All$VPDESID[i]],')')  
    } else if (!is.na(a$Faclat[a$SourceID==All$FacilityID[i]]) & !is.na(a$Faclong[a$SourceID==All$FacilityID[i]])) {
      outfalls$wkt_geom[i]<-paste0('POINT (',a$Faclong[a$SourceID==All$FacilityID[i]],' ',a$Faclat[a$SourceID==All$FacilityID[i]])
    } else {
      outfalls$wkt_geom[i]<-'NULL'
  }
  outfalls$dh_link_facility_mps[i]<-paste0('echo_',All$FacilityID[i])
}
write.table(outfalls,paste0(Outputpath,"outfalls.txt"),sep="\t",row.names = F)
