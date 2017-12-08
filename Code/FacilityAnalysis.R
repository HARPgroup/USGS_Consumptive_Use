#This code is intended to take inputs from external VPDES databases and combine them with outputs from ECHO, creating merged datasets
#It also formats and organizes data and searches for lat/long values to assign to VA Hydro MPs/Facs and those from ECHO
#######################
#Library Initialization
library(foreign)
library(rgdal)
library(dplyr)
library(XML)
library(RCurl)
library(readxl)
library(httr)

#Required inputs: State and Flow frame from ECHO run
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
names(a)[1]<-"VAP_PMT_NO"#Need to rename to give a central columnn name for future joins
#Download statistical codes from ECHO
CodeKey<-read.csv("https://echo.epa.gov/system/files/REF_ICIS-NPDES_STATISTICAL_BASE.csv",stringsAsFactors = F,na.strings = 'BLANK')
#Manual inputs are as follows below. By default, assumes path above:
FlowFrame<-read.csv(paste0(path,"/FlowFrame.csv"),stringsAsFactors = F)
GET('http://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20Active%20IP%20Nov%202017.xls?ver=2017-11-14-152041-490', write_disk(temp <- tempfile(fileext = ".xls")))
VPDESFlows <- read_excel(temp,skip=9)
VPDESFlows<-VPDESFlows[!is.na(VPDESFlows$Facility),]
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID,state,temp)#Remove clutter
################################################################################################################################
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
AllFacs<-as.data.frame(All %>% group_by(FacilityID) %>% summarize_at(vars(c(17,18,19:64)),funs(plus,NAcount)))
#Organize data such that all similar statistics are reported together
headers<-as.character(unique(FlowFrame$Code))
order<-numeric(0)
for (i in 1:length(headers)){
  orderi<-grep(headers[i],colnames(AllFacs))
  order<-c(order,orderi)
}
allcols<-seq(1,length(colnames(AllFacs)))
order<-c(allcols[!(allcols %in% order)],order)
AllFacs<-AllFacs[,order]

#Add in facility names lat/long, and number of outfalls from each dataset looking at ECHO first to find data than VPDES (indicated by source data)
for (i in 1:length(AllFacs$FacilityID)){
  AllFacs$FacilityName[i]<-NA
  if(length(a$CWPName[a$VAP_PMT_NO==AllFacs$FacilityID[i]])>0){
    AllFacs$SourceData[i]<-'ECHO'
    AllFacs$FacilityName[i]<-a$CWPName[a$VAP_PMT_NO==AllFacs$FacilityID[i]]
  }
  if(is.na(AllFacs$FacilityName[i])){
    AllFacs$FacilityName[i]<-as.character(All$FAC_NAME[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
    AllFacs$SourceData[i]<-'VPDES'
  }
  AllFacs$VPDESOutfalls[i]<-sum(!(is.na(All$VAP_PMT_NO[All$FacilityID==AllFacs$FacilityID[i]])))
  AllFacs$ECHOOutfalls[i]<-sum(!(is.na(All$ECHOID[All$FacilityID==AllFacs$FacilityID[i]])))
  AllFacs$TotalOutfalls[i]<-length(All$FacilityID[All$FacilityID==AllFacs$FacilityID[i]])
  AllFacs$VPDES2017Outfalls[i]<-length(VPDES$VAP_PMT_NO[VPDES$VAP_PMT_NO==AllFacs$FacilityID[i]])
}
for (i in 1:length(AllFacs$FacilityID)){
  if(AllFacs$SourceData[i]=='ECHO'){
    AllFacs$lat[i]<-a$FacLat[a$VAP_PMT_NO==AllFacs$FacilityID[i]]
    AllFacs$lon[i]<-a$FacLong[a$VAP_PMT_NO==AllFacs$FacilityID[i]]
  }else{
    AllFacs$lat[i]<-as.numeric(All$coords.x1[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
    AllFacs$lon[i]<-as.numeric(All$coords.x1[All$VAP_PMT_NO==AllFacs$FacilityID[i]])[1]
  }
  }
#Reorder data such that statistics are reported after basic facility information
order<-c(1,seq(length(colnames(AllFacs))-8,length(colnames(AllFacs))),seq(2,length(colnames(AllFacs))-9))
AllFacs<-AllFacs[,order]
AllFacs<-AllFacs[order(AllFacs$Flow.MK_plus-AllFacs$DesFlow_plus,decreasing=T),]
rm(order,orderi,i,headers,allcols)

#Provide a summary on number of reporting facilities, value of statistics, etc.
#only interested in ECHO statistics
order<-grep('plus',colnames(AllFacs))
PlusFacs<-AllFacs[,order]
FacSummary<-data.frame(Stat=colnames(PlusFacs),StatCode=character(length(colnames(PlusFacs))),Description=character(length(colnames(PlusFacs))),Present=numeric(length(colnames(PlusFacs))))
FacSummary$Stat<-as.character(FacSummary$Stat);FacSummary$StatCode<-as.character(FacSummary$StatCode);FacSummary$Description<-as.character(FacSummary$Description)
#A simle lopp to extract code name and the number/value of facilities collectivley reporting it
for (i in 1:length(colnames(PlusFacs))){
  column<-as.vector(PlusFacs[,i])
  FacSummary$StatCode[i]<-gsub(".*[.]([^_]+)[_].*","\\1",FacSummary$Stat[i])
  FacSummary$Description[i]<-NA
  if(FacSummary$StatCode[i] %in% CodeKey$STATISTICAL_BASE_CODE){
  FacSummary$Description[i]<-CodeKey$STATISTICAL_BASE_LONG_DESC[CodeKey$STATISTICAL_BASE_CODE==FacSummary$StatCode[i]]
  }
  FacSummary$Present[i]<-sum(!(is.na(column)))
  FacSummary$SumValue[i]<-plus(column)
}
#Create separate tables for flows and limits
order<-grep('Limit',as.character(FacSummary$Stat))
FacSummaryLimits<-FacSummary[order,]
FacSummaryFlow<-FacSummary[-order,]
rm(column)
#Quick means of finding number of total reported statistcs per facility
order<-c(1,2,grep('plus',colnames(AllFacs)))
order2<-grep('Limit',colnames(AllFacs[,order]))
order<-order[!(order %in% order2)]
test<-AllFacs[,order]
test<-test[,-3]
for (i in 1:length(test$FacilityID)){
  test$StatTotal[i]<-rowSums(!is.na(test[i,3:length(colnames(test))]))
}
for (i in 1:length(AllFacs$FacilityName)){
  AllFacs$FlowStatTotal[i]<-test$StatTotal[test$FacilityID==AllFacs$FacilityID[i]]
}
rm(test,order,order2)