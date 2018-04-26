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
library(lubridate)

#Current inputs for the R script. Right now, these inputs are manual entry only but they can be easily
#converted into a function later on. Inputs are 'state', which is the state of interest, 'startDate' and
#'endDate' which are simply the data of interest, and 'path' which is where data will be written out to.
#Most sites have data from 2012-present, some 2009-present. The state should be entered as the USPS 
#abbreviation and the dates should be entered 'mm/dd/yyyy'
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
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state,"&p_tribedist=0")
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


#The next few lines of code create empty character and numeric vectors. These vectors will be used in
#a 'for' loop to extract Source IDs, flow values, permit limits, statistical codes, and outfall IDs from ECHO.
#These use the DMR_summary data frame created above, to prevent the script from searching facilities with no
#DMR data available. At the end of the loop, the source IDs and outfall IDs are combined to match VPDES ID formatting
FlowSum<-numeric()
FlowMed<-numeric()
Unit<-character()
Limit<-numeric()
VPDESID<-character()
ECHOID<-character()
feat_num<-character()
Code<-character()
Coded<-character()
#A data frame of the months of the year and their corresponding number of days, to be used to generate sums of discharge
#in time frame
mr<-data.frame(month=1:12,days=c(31,29,31,30,31,30,31,31,30,31,30,31))

#The following 'for' loop iterates through each source ID and obtains its DMR data from echo. From there, it extracts
#only discharge data using the parameter code 50050. It then checks the discharge data for all unique months to develop
#the number of months studied at that particular facility. It sotres the unique identifiers of the facility and outfall
#and then stores DMR statistics, including a sum and median of DMR values. Permit limits and statistical codes (i.e. is 
#reported discharge a monthly average, a daily maximum, a 7-day maximum, etc.)
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
      FlowSumi<-numeric(length(codes))#Create areas to store extracted flows, units, and limits
      FlowMedi<-numeric(length(codes))
      Uniti<-numeric(length(codes))
      Limiti<-numeric(length(codes))
      Codedi<-unique(bspec$statistical_base_code)
      for (j in 1:length(codes)){
        bcode<-bspec[bspec$statistical_base_code==codes[j],]
        nodays<-numeric()
        nmbr<-numeric()
        for (l in 1:length(bcode$nmbr_of_submission)){
          mo<-month(bcode$monitoring_period_end_date[l])
          nmbr[l]<-bcode$nmbr_of_submission[l]
          nodays[l]<-sum(mr$days[mr$month%in%seq(mo-nmbr[l]+1,mo)])
        }
        FlowSumi[j]<-sum(bcode$dmr_value_nmbr*nodays,na.rm=T)/sum(nodays)#Store the aggregated sum of all discharge for this outfall. This sheds light on seasonal trends but may be affected by outliers/typos
        FlowMedi[i]<-median(bcode$dmr_value_nmbr,na.rm=T)#Store the median of all discharge records for this outfall. The median helps eliminate the need to spot quarterly vs. annual. monthly data
        Uniti[j]<-unique(bcode$standard_unit_desc)#Find the units being associated with this particular outfall
        LimitswNA<-unique(bcode$limit_value_nmbr)#Store limits and eliminate if NA or take median if multiple
        if(length(LimitswNA)>1){#Occasionally limits report as NA which can alter this code
          if(length(LimitswNA[!is.na(LimitswNA)])>1){
            warning("More than one real limit found, only using median")
            Limiti[j]<-median(bcode$limit_value_nmbr[bcode$limit_end_date==max(bcode$limit_end_date,na.rm=T)],na.rm=T)
          }else{
            Limiti[j]<-LimitswNA[!is.na(LimitswNA)] 
          }
        }else{
          Limiti[j]<-LimitswNA
        }
        Codedi[j]<-unique(bcode$statistical_base_short_desc)#Store the statistic used in developing the meadian above
      }
      FlowSum<-c(FlowSum,FlowSumi)#Store flow, units, limits, and stat codes as needed
      FlowMed<-c(FlowMed,FlowMedi)
      Unit<-c(Unit,Uniti)
      Limit<-c(Limit,Limiti)
      Code<-c(Code,codes)
      Coded<-c(Coded,Codedi)
      feat_num<-c(feat_num,rep(outfall,length(codes)))
      VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length(codes))))
      ECHOID<-c(ECHOID,rep(sourceID,length(codes)))
    }
  }else{
    FlowSum<-c(FlowSum,NA)
    FlowMed<-c(FlowMed,NA)
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
FlowFrame<-data.frame(ECHOID,VPDESID,feat_num,FlowSum,FlowMed,Unit,Limit,Code,Coded)
#rm(codes,Codedi,Limiti,Uniti,Flowi,sourceID,i,j,outfall,Coded,Code,Unit,FlowSum,FlowMed,feat_num,ECHOID,VPDESID,uri_effluent,Limit,LimitswNA)
FlowFrame<-FlowFrame[!is.na(FlowFrame$VPDESID),]
write.csv(FlowFrame,paste0(path,'/FlowFrame.csv'))

