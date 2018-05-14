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

#Current inputs for the R script. Right now, these inputs are manual entry only but they can be easily
#converted into a function later on. Inputs are 'state', which is the state of interest, 'startDate' and
#'endDate' which are simply the data of interest. Most sites have data from 2012-present, some 2009-present
#The state should be entered as the USPS abbreviation and the dates should be entered 'mm/dd/yyyy'
#Current values for dates are default from ECHO
state<-"VA"
startDate<-"01/01/2017"
endDate<-"12/31/2017"
path<-"C:/Users/connorb5/Desktop/USGS Testing"
#path<-"C:\\Users\\nrf46657\\Desktop\\connor_code\\"
#endDate<-Sys.Date()
#endDate<-format(as.Date(endDate), "%m/%d/%Y")

#This section of the code will generate a query in the water facility database on ECHO
#The query will contain all facilities with a CWA permit in the state of Virginia. This can be
#Modified to include all VA HUCS as desired (easy option on the ECHO database)
#This query will be created in a two-step process. First, a short XML document will be created
#containing a queryID. This ID can be used in other parts of the database to access the summary
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
#This portion of the code uses the query ID to find the facility station summary, contianing latitude/
#longitude and a unique ID for each station, which can be used to later generate effluent data
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
a<-read.csv(uri_summary,stringsAsFactors = F)
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID)

#This portion of the code uses the source ID from above to draw in effluent data for a single source ID
#sourceID<-"VA0060844"
#uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
#b<-read.csv(uri_effluent,stringsAsFactors = F)
#This portion creates a basic dataframe from the effluent data to show some potential outputs of interest
#b<-b[b$parameter_code==50050,]


#The next three lines of code create three empty character vectors. These vectors will be used in a for loop to extract Source IDs and outfall IDs from 
#ECHO. These use the DMR_summary data frame created above, to prevent the script from searching facilities with no DMR data available. At the end of the 
#loop, the source IDs and outfall IDs are combined to match VPDES ID formatting
Flow<-0;Flow<-Flow[-1]
Unit<-"";Unit<-Unit[-1]
Limit<-0;Limit<-Limit[-1]
VPDESID<-"";VPDESID<-VPDESID[-1]
ECHOID<-"";ECHOID<-ECHOID[-1]
feat_num<-"";feat_num<-feat_num[-1]
Code<-'';Code<-Code[-1]
Coded<-'';Coded<-Coded[-1]

#The following for loop goes iterates through each source ID and obtains its DMR data from echo. From there, it extracts
#only discharge data using the parameter code 50050. It then checks the discharge data for all unique months to develop
#the number of months studied at that particular facility. Finally, it stores maximum and minimum data if possible (else puts 0) as well
#as the number of unique outfalls identified for that particular iteration of source ID
for (i in 1:length(a$SourceID)) {
  sourceID<-a$SourceID[i]
  print(paste("Processing SourceID: ",sourceID," (",i," of ",length(a$SourceID),")", sep=""))
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
  b<-read.csv(uri_effluent,stringsAsFactors = F)#Obtain DMR data for outfall i
  b<-b[b$parameter_code==50050,]#Extract only the parameter data from the DMR
  b$dmr_value_nmbr[b$nodi_code %in% c('C','7')]<-0#Where values are noted as no dischartge, set discharge to zero rather than NA. This includes them in futrue calculations
  b$monitoring_period_end_date<-as.Date(as.POSIXct(b$monitoring_period_end_date,"EST",format='%m/%d/%Y'))#Convert dates to Date class through POSIXct forcing
  data_length<-length(unique(b$monitoring_period_end_date))#Obtain length of monitoring record. Note: May not be number of months if annual, quarterly, etc. reporting
  if (data_length>0){
    featuresID<-unique(b$perm_feature_nmbr)#Store all outfalls
    features<-unique(b$perm_feature_nmbr)#Create a variable to store reformatted outfall IDs that will have leading zeroes where necessary
    for (j in 1:length(features)){
      if(!is.na(as.numeric(features[j]))){
        addedzeroes<-paste(rep(0,3-nchar(features[j])),collapse = '')#If the feature j is numeric, add leading zeroes such that the ID 
        features[j]<-paste0(addedzeroes,as.character(features[j]))#is three places long (for outfall ID like those in VPDES)
      } else{
        features[j]<-as.character(features[j])#If the feature j is not numeric, then no formatting is necessary
      }
    }
    for (k in 1:length(features)){
      outfall<-as.character(features[k])#This loop will deal with outfall k
      bspec<-b[b$perm_feature_nmbr==featuresID[k],]#Extract data from the DMR such that it only deals with feature/outfall k
      codes<-unique(bspec$statistical_base_code)#Determine how many statistics are reported for this outfall
      Flowi<-numeric(length(codes))#Create areas to store extracted flows, units, and limits
      Uniti<-numeric(length(codes))
      Limiti<-numeric(length(codes))
      Codedi<-unique(bspec$statistical_base_code)
      for (j in 1:length(codes)){
        Flowi[j]<-median(bspec$dmr_value_nmbr[bspec$statistical_base_code==codes[j]],na.rm=T)#Store the median of all discharge records for this outfall. The median helps eliminate the need to spot quarterly vs. annual. monthly data
        Uniti[j]<-unique(bspec$standard_unit_desc[bspec$statistical_base_code==codes[j]])#Find the units being associated with this particular outfall
        LimitswNA<-unique(bspec$limit_value_nmbr[bspec$statistical_base_code==codes[j]])#Store limits and eliminate if NA or take median if multiple
        if(length(LimitswNA)>1){#Occasionally limits report as NA which can alter this code
          if(length(LimitswNA[!is.na(LimitswNA)])>1){
            warning("More than one real limit found, only using median")
            Limiti[j]<-median(bspec$limit_value_nmbr[bspec$statistical_base_code==codes[j]&bspec$limit_end_date==max(bspec$limit_end_date[bspec$statistical_base_code==codes[j]],na.rm=T)],na.rm=T)
          }else{
            Limiti[j]<-LimitswNA[!is.na(LimitswNA)] 
          }
        }else{
          Limiti[j]<-LimitswNA
        }
        Codedi[j]<-unique(bspec$statistical_base_short_desc[bspec$statistical_base_code==codes[j]])#Store the statistic used in developing the meadian above
      }
      Flow<-c(Flow,Flowi)#Store flow, units, limits, and stat codes as needed
      Unit<-c(Unit,Uniti)
      Limit<-c(Limit,Limiti)
      Code<-c(Code,codes)
      Coded<-c(Coded,Codedi)
      feat_num<-c(feat_num,rep(outfall,length(codes)))
      VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length(codes))))
      ECHOID<-c(ECHOID,rep(sourceID,length(codes)))
    }
  }else{
    Flow<-c(Flow,NA)
    Unit<-c(Unit,NA)
    Limit<-c(Limit,NA)
    Code<-c(Code,NA)
    Coded<-c(Coded,NA)
    feat_num<-c(feat_num,NA)
    VPDESID<-c(VPDESID,NA)
    ECHOID<-c(ECHOID,sourceID)
  }
}

#These next few lines subset and export the data developed in the above loops
FlowFrame<-data.frame(ECHOID,VPDESID,feat_num,Flow,Unit,Limit,Code,Coded)
#rm(codes,Codedi,Limiti,Uniti,Flowi,sourceID,i,j,outfall,Coded,Code,Unit,Flow,feat_num,ECHOID,VPDESID,uri_effluent,Limit,LimitswNA)
FlowFrame<-FlowFrame[!is.na(FlowFrame$VPDESID),]
write.csv(FlowFrame,paste0(path,'/FlowFrame.csv'))
