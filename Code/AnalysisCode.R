#This code is intended to take inputs from external VPDES databases and combine them with outputs from ECHO, creating merged datasets
#It also formats and organizes data and searches for lat/long values to assign to VA Hydro MPs/Facs and those from ECHO
#Primary outputs are the reformatted VA Hydro facility "Hydro" that have corrected geometries and "AllFacs" which offers discharge summaries
#for every state facility. "All" given a merged version of VPDES and ECHO data on an outfall level, reordered for convenience.
#"FacSummary" offers information on the variety of statistics reported by state DMR records and adds a total stat count for each facility
#onto "AllFacs"
#######################
#Library Initialization
library(foreign)
library(rgdal)
library(dplyr)
library(XML)
library(RCurl)
library(readxl)
library(proj4)
library(httr)#Do we need this?

#Required inputs: State, Flow frame from ECHO run, flow frame from 2017 (shows change in outfalls),
#and VA Hydro facility list (http://deq1.bse.vt.edu/d.bet/vahydro_facilities)
#VA Hydro facility list and VPDES info spreadsheet are manual downloads due to slow internet connections, making it difficult to access without R timing out
#input/output path will also be required as the script needs a place to store downloads from VPDES
state<-"VA"
path<-"C:/Users/connorb5/Desktop/USGS Testing"
#Get ECHO Facility List and store in dataframe 'a'
  uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state,"&p_tribedist=0")
  ECHO_xml<-getURL(uri_query)
  ECHO_query<-xmlParse(ECHO_xml)
  QID<-xmlToList(ECHO_query)
  QID<-QID$QueryID
  uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
a<-read.csv(uri_summary,stringsAsFactors = F)
#Download list of active VPDES outfalls and separate individual permits
temp<-tempfile()
download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip",temp)
unzip(temp,exdir=path)
VPDES<-as.data.frame(readOGR(paste0(path,"/VPDES_Geodatabase.gdb"),layer="VPDES_OUTFALLS"))
names(VPDES)[names(VPDES)=="OUTFALL_ID"]<-'VPDESID'
VPDES_IP<-VPDES[VPDES$VAP_TYPE=='VPDES_IP',]
names(a)[names(a)=="SourceID"]<-"VAP_PMT_NO"#Need to rename to give a central columnn name for future joins
#Download statistical codes from ECHO
CodeKey<-read.csv("https://echo.epa.gov/system/files/REF_ICIS-NPDES_STATISTICAL_BASE.csv",stringsAsFactors = F,na.strings = 'BLANK')
#Manual inputs are as follows below. By default, assumes path above:
FlowFrame<-read.csv(paste0(path,"/2016 ECHO/FlowFrameNoDis2016.csv"),stringsAsFactors = F)
FlowFrameNew<-read.csv(paste0(path,"/2017 ECHO/FlowFrame.csv"),stringsAsFactors = F)
Hydro<-read.csv('http://deq1.bse.vt.edu/d.bet/vahydro_facilities',stringsAsFactors = F)
Hydro<-read.csv(paste0(path,"/vahydro_facilities.csv"),stringsAsFactors = F)
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
#Add a column to the list of ECHO facilities with the number of outfalls in ECHO in 2017
for (i in 1:length(a$VAP_PMT_NO)){
  a$ECHO2017Outfalls[i]<-length(unique(FlowFrameNew$VPDESID[FlowFrameNew$ECHOID==a$VAP_PMT_NO[i]]))
}

#Analysis of ECHO data, providing all values for all statistics reported for a given facility
#First, flip flow frame to show statistical codes in columns rather than rows
FlowFrameFlipped<-FlowFrame[,c('VPDESID','Flow','Limit','Code')]
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
#Data reclassification to avoid factors and character classes being assigned to numbers
All$VAP_PMT_NO<-as.character(All$VAP_PMT_NO)
All$DesFlow<-as.numeric(All$DesFlow)
All$TotalFlow<-as.numeric(All$TotalFlow)
#Create a data frame containing data in 'All' aggregated to the facility level
#Thus, data in AllFacs shows a facility ID and then VPDES flow and ECHO stats summed to the
#facility level. It also counts the number of outfalls at each facility reporting NA
AllFacs<-as.data.frame(All %>% group_by(FacilityID) %>% summarize_at(vars(19:64),funs(plus,NAcount)))
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
  if(length(a$CWPName[a$VAP_PMT_NO==AllFacs$FacilityID[i]])>0){#If the current examined facility is in the list of ECHO facilities, note that
    AllFacs$SourceData[i]<-'ECHO'
    AllFacs$FacilityName[i]<-a$CWPName[a$VAP_PMT_NO==AllFacs$FacilityID[i]]
  }
  if(is.na(AllFacs$FacilityName[i])){#If its not, than the facility is a VPDES exclusive. Make note of that
    AllFacs$FacilityName[i]<-as.character(All$FAC_NAME[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
    AllFacs$SourceData[i]<-'VPDES'
  }
  #Find the number of data reporting outfalls in VPDES and ECHO
  AllFacs$ECHOOutfalls[i]<-sum(!(is.na(All$ECHOID[All$FacilityID==AllFacs$FacilityID[i]])))
  AllFacs$TotalOutfalls[i]<-length(All$FacilityID[All$FacilityID==AllFacs$FacilityID[i]])#Find the total number of outfalls
  AllFacs$VPDESOutfalls[i]<-length(VPDES$VAP_PMT_NO[VPDES$VAP_PMT_NO==AllFacs$FacilityID[i]])
  AllFacs$ECHO2017Outfalls[i]<-length(unique(FlowFrameNew$VPDESID[FlowFrameNew$ECHOID==AllFacs$FacilityID[i]]))
}
for (i in 1:length(AllFacs$FacilityID)){
  if(AllFacs$SourceData[i]=='ECHO'){#Based on value of source data noted earlier, find lat/long. VPDES lat/long may need to be reprojected, so check ECHO first
    AllFacs$lat[i]<-a$FacLat[a$VAP_PMT_NO==AllFacs$FacilityID[i]]
    AllFacs$lon[i]<-a$FacLong[a$VAP_PMT_NO==AllFacs$FacilityID[i]]
  }else{
    AllFacs$lat[i]<-as.numeric(All$coords.x1[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
    AllFacs$lon[i]<-as.numeric(All$coords.x1[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
  }
  AllFacs$DesFlow[i]<-All$DesFlow[All$FacilityID==AllFacs$FacilityID[i]][1]
  AllFacs$TotalFlow[i]<-All$TotalFlow[All$FacilityID==AllFacs$FacilityID[i]][1]
}
diff<-numeric(0)
ind<-numeric(0)
for (i in 1:length(a$CWPActualAverageFlowNmbr)){
  if(!is.na(a$CWPActualAverageFlowNmbr[i])){
    if(length(AllFacs$DesFlow[AllFacs$FacilityID==a$VAP_PMT_NO[i]])>0){
      store<-a$CWPActualAverageFlowNmbr[i]==AllFacs$DesFlow[AllFacs$FacilityID==a$VAP_PMT_NO[i]] 
      diff<-c(diff,store)
      ind<-c(ind,i)
    }
  }
}
#Reorder data such that statistics are reported after basic facility information
order<-c(1,seq(length(colnames(AllFacs))-9,length(colnames(AllFacs))),seq(2,length(colnames(AllFacs))-10))#May need manual adjustment if data changes. Basic reorganize
AllFacs<-AllFacs[,order]#Reorganize so that facility info presents before statistics
AllFacs<-AllFacs[order(AllFacs$Flow.MK_plus-AllFacs$DesFlow_plus,decreasing=T),]#Order by largest ECHO/VPDES discrepencies first
rm(order,orderi,i,headers,allcols)

#Provide a summary on number of reporting facilities, value of statistics, etc.
#only interested in ECHO statistics
order<-grep('plus',colnames(AllFacs))#Only want those that summed data (i.e. not facility info or NA counts)
PlusFacs<-AllFacs[,order]
FacSummary<-data.frame(Stat=colnames(PlusFacs),StatCode=character(length(colnames(PlusFacs))),Description=character(length(colnames(PlusFacs))),Present=numeric(length(colnames(PlusFacs))))
FacSummary$Stat<-as.character(FacSummary$Stat);FacSummary$StatCode<-as.character(FacSummary$StatCode);FacSummary$Description<-as.character(FacSummary$Description)
#A simle lopp to extract code name and the number/value of facilities collectivley reporting it
for (i in 1:length(colnames(PlusFacs))){
  column<-as.vector(PlusFacs[,i])
  FacSummary$StatCode[i]<-gsub(".*[.]([^_]+)[_].*","\\1",FacSummary$Stat[i])#Extract stat using glob notation
  FacSummary$Description[i]<-NA
  if(FacSummary$StatCode[i] %in% CodeKey$STATISTICAL_BASE_CODE){#Add a description of a code where possible
  FacSummary$Description[i]<-CodeKey$STATISTICAL_BASE_LONG_DESC[CodeKey$STATISTICAL_BASE_CODE==FacSummary$StatCode[i]]
  }
  FacSummary$Present[i]<-sum(!(is.na(column)))#Provide the count of how many facilities are reporting this given statistic
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

#extract lat and long from WKT so that it can be used from VA Hydro numbers
#Hydro<-Hydro[!duplicated(Hydro),]
Hydro$FacLat<-as.numeric(gsub(".*[ ]([^)]+)[)].*","\\1",Hydro$fac_geom))
Hydro$FacLong<-as.numeric(gsub(".*[(]([^ ]+)[ ].*","\\1",Hydro$fac_geom))
Hydro$tsvalue<-as.numeric(Hydro$tsvalue)
Hydro$MPLat<-as.numeric(gsub(".*[ ]([^)]+)[)].*","\\1",Hydro$mp_geom))
Hydro$MPLong<-as.numeric(gsub(".*[(]([^ ]+)[ ].*","\\1",Hydro$mp_geom))
#A simple search function to check for NAs and replace them for use in boolean code
NAReplace<-function(x){
  if(is.na(x)){
    return(-99)
  }else{
    return(x)
  }
}
#Correct lat/long values for possible typos i.e. missing negative signs, switched lat/long, or both
#Is set to check if data falls in rough state boundaries set by (35.-70) and (45,-90)
Hydro$Lat<-numeric(length(Hydro$MPLat))
Hydro$Long<-numeric(length(Hydro$MPLong))
for (i in 1:length(Hydro$fac_name)){
  Hydro$Lat[i]<-Hydro$MPLat[i]
  Hydro$Long[i]<-Hydro$MPLong[i]
  Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
  Hydro$Long[i]<-NAReplace(Hydro$Long[i])
    if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
      Hydro$Lat[i]<-Hydro$MPLong[i]
      Hydro$Long[i]<-Hydro$MPLat[i]
      Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
      Hydro$Long[i]<-NAReplace(Hydro$Long[i]) 
      if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
        Hydro$Lat[i]<-Hydro$MPLat[i]
        Hydro$Long[i]<-(-1)*Hydro$MPLong[i]
        Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
        Hydro$Long[i]<-NAReplace(Hydro$Long[i]) 
        if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
          Hydro$Lat[i]<-Hydro$MPLong[i]
          Hydro$Long[i]<-(-1)*Hydro$MPLat[i]
          Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
          Hydro$Long[i]<-NAReplace(Hydro$Long[i]) 
          if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
            Hydro$Lat[i]<-Hydro$FacLat[i]
            Hydro$Long[i]<-Hydro$FacLong[i]
            Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
            Hydro$Long[i]<-NAReplace(Hydro$Long[i])
            if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
              Hydro$Lat[i]<-Hydro$FacLong[i]
              Hydro$Long[i]<-Hydro$FacLat[i]
              Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
              Hydro$Long[i]<-NAReplace(Hydro$Long[i])
              if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
                Hydro$Lat[i]<-Hydro$FacLat[i]
                Hydro$Long[i]<-(-1)*Hydro$FacLong[i]
                Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
                Hydro$Long[i]<-NAReplace(Hydro$Long[i])
                if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
                  Hydro$Lat[i]<-Hydro$FacLong[i]
                  Hydro$Long[i]<-(-1)*Hydro$FacLat[i]
                  Hydro$Lat[i]<-NAReplace(Hydro$Lat[i])
                  Hydro$Long[i]<-NAReplace(Hydro$Long[i])
                 if((Hydro$Lat[i]<35|Hydro$Lat[i]>45)|(Hydro$Long[i]<(-90)|Hydro$Long[i]>(-70))){
                    Hydro$Lat[i]<-Hydro$MPLat[i]
                    Hydro$Long[i]<-Hydro$MPLong[i]
                 }
                }
              }
            }
          }
        }
      }
    }
}
#Same process as above, but on a facilitiy level rather than an MP one. If no valid facility ID can be found,
#MP data may be used in its place. If no passable geometry can be fournd, return original data
HydroFacs<-as.data.frame(Hydro %>% group_by(fac_hydroid) %>% summarize(Name=first(fac_name),Sum=plus(tsvalue)))
HydroFacs$Lat<-numeric(length(HydroFacs$fac_hydroid))
HydroFacs$Long<-numeric(length(HydroFacs$fac_hydroid))

for (i in 1:length(HydroFacs$fac_hydroid)){
  HydroFacs$Lat[i]<-Hydro$FacLat[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
  HydroFacs$Long[i]<-Hydro$FacLong[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
  HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
  HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
  if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
    HydroFacs$Lat[i]<-Hydro$FacLong[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
    HydroFacs$Long[i]<-Hydro$FacLat[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
    HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
    HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
    if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
      HydroFacs$Lat[i]<-Hydro$FacLat[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
      HydroFacs$Long[i]<-(-1)*Hydro$FacLong[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
      HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
      HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
      if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
        HydroFacs$Lat[i]<-Hydro$FacLong[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
        HydroFacs$Long[i]<-(-1)*Hydro$FacLat[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
        HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
        HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
      }
    }
  }
  if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
    j<-1
    MP<-Hydro[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i],]
    while(j<(length(MP$fac_name)+1)){
      HydroFacs$Lat[i]<-MP$MPLat[j]
      HydroFacs$Long[i]<-MP$MPLong[j]
      HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
      HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
      if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
        HydroFacs$Lat[i]<-MP$MPLong[j]
        HydroFacs$Long[i]<-MP$MPLat[j]
        HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
        HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
        if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
          HydroFacs$Lat[i]<-MP$MPLat[j]
          HydroFacs$Long[i]<-(-1)*MP$MPLong[j]
          HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
          HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
          if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
            HydroFacs$Lat[i]<-MP$MPLong[j]
            HydroFacs$Long[i]<-(-1)*MP$MPLat[j]
            HydroFacs$Lat[i]<-NAReplace(HydroFacs$Lat[i])
            HydroFacs$Long[i]<-NAReplace(HydroFacs$Long[i])
          }
        }
      j<-j+1
      }else{
        j<-length(MP$fac_name)+1
      }
    }
    if((HydroFacs$Lat[i]<35|HydroFacs$Lat[i]>45)|(HydroFacs$Long[i]<(-90)|HydroFacs$Long[i]>(-70))){
    HydroFacs$Lat[i]<-Hydro$FacLat[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
    HydroFacs$Long[i]<-Hydro$FacLong[Hydro$fac_hydroid==HydroFacs$fac_hydroid[i]][1]
    }
  }
}
rm(i,j,MP,PlusFacs)



