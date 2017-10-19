#The goal of this code is to extract data for effluent charts from ECHO in an automated manner to allow
#for a refreshable list of stations within Virginia as well as desired discharges. It is important to note
#that this code currently looks only at stations in Virginia but should be easily modifiable to allow for the 
#extraction of VA HUCs
##############################################################################################################
#First, start by removing old variables to prevent clutter
rm(list=ls())

#Some basic R libraries to get data from https servers and use parse XML data
library(XML)
library(RCurl)
library(dplyr)
library(foreign)

#Current inputs for the R script. Right now, these inputs are manual entry only but they can be easily
#converted into a function later on. Inputs are 'state', which is the state of interest, 'startDate' and
#'endDate' which are simply the data of interest. Most sites have data from 2012-present, some 2009-present
#The state should be entered as the USPS abbreviation and the dates should be entered 'mm/dd/yyyy'
#Current values for dates are default from ECHO
state<-"VA"
startDate<-"01/01/2000"
endDate<-"12/31/2016"
#endDate<-Sys.Date()
#endDate<-format(as.Date(endDate), "%m/%d/%Y")

#This section of the code will generate a query in the water facility database on ECHO
#The query will contain all facilities with a CWA permit in the state of Virginia. This can be
#Modified to include all VA HUCS as desired (easy option on the ECHO database)
#This query will be created in a two-step process. First, a short XML document will be created
#containing a queryID. This ID can be used in other parts of the database to access the summary
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state,"&p_tribedist=0")
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
#This portion of the code uses the query ID to find the facility station summary, contianing latitude/
#longitude and a unique ID for each station, which can be used to later generate effluent data
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
a<-read.csv(uri_summary,stringsAsFactors = F)

#This portion of the code uses the source ID from above to draw in effluent data for a single source ID
sourceID<-"VA0060844"
uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
b<-read.csv(uri_effluent,stringsAsFactors = F)

#This portion creates a basic dataframe from the effluent data to show some potential outputs of interest
b<-b[b$parameter_code==50050,]
#effluent<-data.frame(b$monitoring_period_end_date,b$standard_unit_desc,b$statistical_base_short_desc,b$dmr_value_nmbr)
#names(effluent)<-c("End Date","Units","Stat","DMR")
#effluent  
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID)
#The next three lines create four blank numeric vectors to store dates, number of outfalls, and data length from all downlodable effluent charts
#The objective of this portion of the code is to demonstrate which facilities have no data and which have minimal data
data_length<-numeric(length(a$SourceID))
date_min<-numeric(length(a$SourceID))
date_max<-numeric(length(a$SourceID))
outfalls<-numeric(length(a$SourceID))
#The following for loop goes iterates through each source ID and obtains its DMR data from echo. From there, it extracts
#only discharge data using the parameter code 50050. It then checks the discharge data for all unique months to develop
#the number of months studied at that particular facility. Finally, it stores maximum and minimum data if possible (else puts 0) as well
#as the number of unique outfalls identified for that particular iteration of source ID
for (i in 1:length(a$SourceID)) {
  sourceID<-a$SourceID[i]
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
  b<-read.csv(uri_effluent,stringsAsFactors = F)
  b<-b[b$parameter_code==50050,]
  b$monitoring_period_end_date<-as.Date(as.POSIXct(b$monitoring_period_end_date,"EST",format='%m/%d/%Y'))
  data_length[i]<-length(unique(b$monitoring_period_end_date))
  if (data_length[i]>0){
    date_min[i]<-min(b$monitoring_period_end_date)
    date_max[i]<-max(b$monitoring_period_end_date)
    outfalls[i]<-length(unique(b$perm_feature_nmbr))
  }
}
#Converts the dates extracted from the DMR data into the appropriate format (R converted these to numbers in the loop)
date_min<-as.Date(date_min,origin="1970-01-01")
date_max<-as.Date(date_max,origin="1970-01-01")
#Outputs a data frame with the earliest date on record, the latest, the source ID, and the number of months on record
#Also created a data frame DMR_summary which extracts only facilities with DMR data
all_summary<-data.frame("Mindate"=date_min,"Maxdate"=date_max,"SourceID"=as.character(a$SourceID),"UniqueDate"=data_length,"Outfalls"=outfalls)
DMR_summary<-all_summary[all_summary$UniqueDate!=0,]
rm(data_length,outfalls,date_max,date_min,i)

#The next three lines of code create three empty character vectors. These vectors will be used in a for loop to extract Source IDs and outfall IDs from 
#ECHO. These use the DMR_summary data frame created above, to prevent the script from searching facilities with no DMR data available. At the end of the 
#loop, the source IDs and outfall IDs are combined to match VPDES ID formatting
VPDESID<-"";VPDESID<-VPDESID[-1]
ECHOID<-"";ECHOID<-ECHOID[-1]
feat_num<-"";feat_num<-feat_num[-1]

#This loop iterates through each facility with reporting discharge DMR data. In every loop, it reads in DMR effluent data, extracts the number of 
#discharge outfalls, and then formats the outfall ID to a three digit number, adding leading zeroes as necessary. Finally, it adds source IDs, ECHO 
#outfall IDs, and the VPDES IDs to the blank vectors created above.
for (i in 1:length(DMR_summary$SourceID)){
  sourceID<-as.character(DMR_summary$SourceID[i])
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
  b<-read.csv(uri_effluent,stringsAsFactors = F)
  b<-b[b$parameter_code==50050,]
  features<-unique(b$perm_feature_nmbr)
  for (j in 1:length(features)){
    if(!is.na(as.numeric(features[j]))){
      addedzeroes<-paste(rep(0,3-nchar(features[j])),collapse = '')
      features[j]<-paste0(addedzeroes,as.character(features[j]))
    } else{
      features[j]<-as.character(features[j])
    }
  }
  VAID<-paste0(sourceID,features)
  sourceID<-rep(sourceID,length(VAID))
  feat_num<-c(feat_num,features)
  VPDESID<-c(VPDESID,VAID)
  ECHOID<-c(ECHOID,sourceID)
}

#Writes out a csv that contains ECHO ID, VPDES ID, and the outfall identifier (which is combined with ECHO ID to form VPDES ID) to a csv, ECHOConv.csv
ECHOConvWater<-data.frame(ECHOID,VPDESID,feat_num)
write.csv(ECHOConv,"ECHOConvWaterOnly.csv")
rm(i,ECHOID,VPDESID,feat_num,VAID,addedzeroes,features,uri_effluent)

#This next portion of the code is somewhat redundant. Using the feature numbers and corresponding ECHO ID's generated in the section preceeding this
#one, this loop uses the ECHOConv data frame to locate every outfall with DMR data in ECHO. It then finds an average mean flow and any corresponding
#discharge limits and associated units for each type of statistic (monthly average, daily max, etc.) reported by ECHO. These are stored alongside the
#ECHO/VPDES ID's into a new data frame, FlowFrame.This can be sorted by statistical code and written out to csv/dbf for GIS uses
Flow<-0;Flow<-Flow[-1]
Unit<-"";Unit<-Unit[-1]
Limit<-0;Limit<-Limit[-1]
VPDESID<-"";VPDESID<-VPDESID[-1]
ECHOID<-"";ECHOID<-ECHOID[-1]
feat_num<-"";feat_num<-feat_num[-1]
Code<-'';Code<-Code[-1]
Coded<-'';Coded<-Coded[-1]
for (i in 1:length(ECHOConvWater$feat_num)){
  sourceID<-as.character(ECHOConvWater$ECHOID[i])
  outfall<-as.character(ECHOConvWater$feat_num[i])
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&outfall=",outfall,"&start_date=",startDate,"&end_date=",endDate)
  b<-read.csv(uri_effluent,stringsAsFactors = F)
  b<-b[b$parameter_code==50050,]
  codes<-unique(b$statistical_base_code)
  Flowi<-numeric(length(codes))
  Uniti<-numeric(length(codes))
  Limiti<-numeric(length(codes))
  Codedi<-unique(b$statistical_base_code)
  for (j in 1:length(codes)){
    Flowi[j]<-mean(b$dmr_value_nmbr[b$statistical_base_code==codes[j]],na.rm=T)
    Uniti[j]<-unique(b$standard_unit_desc[b$statistical_base_code==codes[j]])
    LimitswNA<-unique(b$limit_value_nmbr[b$statistical_base_code==codes[j]])
    if(length(LimitswNA)>1){#Occasionally limits report as NA which can alter this code
      if(length(LimitswNA[!is.na(LimitswNA)])>1){
        warning("More than one real limit found, only using median of latest permit")
        Limiti[j]<-median(b$limit_value_nmbr[b$statistical_base_code==codes[j]&b$limit_end_date==max(b$limit_end_date[b$statistical_base_code==codes[j]],na.rm=T)],na.rm=T)
      }else{
      Limiti[j]<-LimitswNA[!is.na(LimitswNA)] 
      }
    }else{
      Limiti[j]<-LimitswNA
    }
    Codedi[j]<-unique(b$statistical_base_short_desc[b$statistical_base_code==codes[j]])
  }
  Flow<-c(Flow,Flowi)
  Unit<-c(Unit,Uniti)
  Limit<-c(Limit,Limiti)
  Code<-c(Code,codes)
  Coded<-c(Coded,Codedi)
  feat_num<-c(feat_num,rep(outfall,length(codes)))
  VPDESID<-c(VPDESID,rep(as.character(ECHOConv$VPDESID[i]),length(codes)))
  ECHOID<-c(ECHOID,rep(sourceID,length(codes)))
}

#These next few lines subset and export the data developed in the above loops
FlowFrame<-data.frame(ECHOID,VPDESID,feat_num,Flow,Unit,Limit,Code,Coded)
test<-FlowFrame[FlowFrame$Code=='MK',]
write.csv(test,"test.csv")
test2<-as.data.frame(FlowFrame %>% group_by(Coded) %>% summarize(Count=n()))
write.dbf(test,"test")
rm(codes,Codedi,Limiti,Uniti,Flowi,sourceID,i,j,outfall,Coded,Code,Unit,Flow,feat_num,ECHOID,VPDESID,uri_effluent,Limit,LimitswNA,b)

library(foreign)
library(rgdal)
temp<-tempfile()
download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip",temp)
unzip(temp,exdir="C:/Users/connorb5/Desktop/USGS Testing")
VPDES<-as.data.frame(readOGR("C:/Users/connorb5/Desktop/USGS Testing/VPDES_Geodatabase.gdb",layer="VPDES_OUTFALLS"))
