#############################################################################################################################
##############################################ECHO Interface.R###############################################################

#Primary Author: Connor Brogan, M.S. Biological Systems Engineering, Virginia Tech
#Adapted by: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

#################################################Purpose#####################################################################

#Utilize the EPA's Enforcement and Compliance Online History (ECHO) Representation State Transfer (REST) Services
#to extract data on facilities regulated under the Clean Water Act (CWA) and managed under the National Pollutant 
#Discharge Elimination System (NPDES) Program. 

#Ultimately this statewide discharge data will be combined with withdrawal data to refine State Water Budget estimates.
#Accurate representations of water supply will fuel more informed policies and management practices.

#################################################Resources##################################################################

#The following links include inforamtion about the REST services used to query data associated with discharging facilities. 

#ECHO CWA REST Services: Facility Search - Water 
#https://echo.epa.gov/tools/web-services/facility-search-water

#ECHO EFF REST Services: Effluent Charts-Discharge Monitoring Reports
#https://echo.epa.gov/tools/web-services/effluent-charts

#ECHO DFR REST Services: Detailed Facility Report
#https://echo.epa.gov/tools/web-services/detailed-facility-report

#############################################################################################################################
#############################################Initilisation###################################################################

rm(list=ls()) #Start with clear environment

library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(lubridate) #parses and manipulates dates

#Required Inputs for ECHO REST Query 
state<-"VA" #Input for State of Interest. Must be Inputted as Abreviation 
startDate<-"01/01/2012" #mm/dd/yyyy: data on ECHO is limited to 2012 for most sites or 2009 for a few
endDate<-"12/31/2017" #mm/dd/yyyy If date range is not specified, query will include data from last three years
path<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code"
#endDate<-Sys.Date()
#endDate<-format(as.Date(endDate), "%m/%d/%Y")

#############################################################################################################################
###########################################CWA Facility Download##################################################################

#This section of the code generate a query in the CWA Water Facility Search REST Services in ECHO.
#It pulls every discharging facility regulated by the CWA with a NPDES permit. 
#Queries are created using a generated URL with the base address of: https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?

#This particular query is created in two-steps. An XML document of all CWA facilities in VA is first downloaded (uri_query and ECHO_xml).
#Then the XML is parsed (ECHO_query) to generate a query ID (QID) that can be used to access summary data for each facility (uri_summary).

uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query) #this function downloads the URL from above, which creates an XML of facilities with CWA permit
ECHO_query<-xmlParse(ECHO_xml) #parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to see query ID or QID

QID<-xmlToList(ECHO_query) #Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID #Extracts Query ID 
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID) #Uses QID to extract facility summary data
ECHO_Facilities<-read.csv(uri_summary,stringsAsFactors = F)
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID) #Clear unnecessary variables

#Each facility reports different statistics for their flows, ranging from averages to maximums, minimums, etc. 
#Download statistical codes from ECHO to get a better idea of what each statistical code means:
CodeKey<-read.csv("https://echo.epa.gov/system/files/REF_ICIS-NPDES_STATISTICAL_BASE.csv",stringsAsFactors = F,na.strings = 'BLANK')

#############################################################################################################################
######################################Manipulation of Summary Facility Data##################################################

#This section of the code loops through each CWA Facility in the State and obtains its Discharge Monitoring Report (DMR).
#The REST service used for extracting DMR data uses the following base URL: https://ofmpub.epa.gov/echo/eff_rest_services.get_effluent_chart?

#Information including number of outfalls, outfall IDs, flow values, and statistical codes for those flows are collected.
#Flow is aggregated by sum and median values over the monitoring periods in the time range specified in the initilization section of this code. 

#Create Variables for Desired Information
FlowSum<-numeric() #Summed Flow 
FlowMed<-numeric() #Median Flow
Units<-character() #Units reported for Flow
Limit<-numeric() #Limit/permitted amount of discharge for a single facility-not regulary updated in VA
VPDESID<-character() #Unique ID used in Virginia for a facility's outfall: concatonated facility ID with 3 digit outfall ID
ECHOID<-character() #Facility ID indicated in ECHO System
outfall_num<-character() #Outfall ID
Stat<-character() #Statistical Code Abrievation associated with flow value. Includes averages, maximums, minimums, etc. 
Stat_Description<-character() #Description of Code
mon_in_year<-data.frame(month=1:12,days=c(31,29,31,30,31,30,31,31,30,31,30,31)) #Months of the year and their days to be used to generate summed discharge over monitoring periods

#This loops through each discharging facility in the State of Virginia to extract associated outfall information

for (i in 1:length(ECHO_Facilities$SourceID)) {
  sourceID<-ECHO_Facilities$SourceID[i]
  print(paste("Processing SourceID: ",sourceID," (",i," of ",length(ECHO_Facilities$SourceID),")", sep=""))
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe
  DMR_Data<-read.csv(uri_effluent,stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains DMR for a single facility
  DMR_Data<-DMR_Data[DMR_Data$parameter_code==50050,]#only looks at Flow, in conduit or thru treatment plant - there are 347 parameter codes defined in ECHO
  DMR_Data$dmr_value_nmbr[DMR_Data$nodi_code %in% c('C','7')]<-0#nodi_code is the unique code indicating the reason why an expected DMR value was not submitted, C and 7 means no influent. So set to 0.
  DMR_Data$monitoring_period_end_date<-as.Date(as.POSIXct(DMR_Data$monitoring_period_end_date,"EST",format='%m/%d/%Y'))#Convert dates to Date class through POSIXct forcing
  data_length<-length(unique(DMR_Data$monitoring_period_end_date))#sees if there is any reported data worth extracting and examining
  if (data_length>0){#if the value is NOT NA, enter loop
    outfall_nmbr<-unique(DMR_Data$perm_feature_nmbr)#Stores Outfalls which are called permanent features in the DMR
    outfall_ID<-unique(DMR_Data$perm_feature_nmbr)#perm_feature_nmbr is a three-character code in ICIS-NPDES that identifies the point of discharge
    for (j in 1:length(outfall_ID)){#If the code is less than three characters in the .CSV, append zeros to the beginning of the number (e.g., 1 is equivalent to 001)
      if(!is.na(as.numeric(outfall_ID[j]))){
        addedzeroes<-paste(rep(0,3-nchar(outfall_ID[j])),collapse = '')#this section assigns leading zeros in front of the outfall numbers if they are under 3 characters
        outfall_ID[j]<-paste0(addedzeroes,as.character(outfall_ID[j]))
      } else{
        outfall_ID[j]<-as.character(outfall_ID[j])#if the outfall number is already three digits, no reformatting needed
      }
    }
    for (k in 1:length(outfall_ID)){ #Each unique outfall (discharging point) associated with the singel facility at hand is looped through
      outfall<-as.character(outfall_ID[k])
      outfall_DMR<-DMR_Data[DMR_Data$perm_feature_nmbr==outfall_nmbr[k],]#extracts DMR data (specfically parameter code 50050) for unique outfall
      stats<-unique(outfall_DMR$statistical_base_code)#Determine how many statistics are reported for this outfall
      FlowSumi<-numeric(length(stats))#Create variables to store extracted flows (summed and median for specified date range), units, and limits
      FlowMedi<-numeric(length(stats))
      Unitsi<-numeric(length(stats))
      Limiti<-numeric(length(stats))
      Stat_Descriptioni<-unique(outfall_DMR$statistical_base_code)
      for (j in 1:length(stats)){ #For Each Statistical Code Reported for the Outfall, store the DMR 
        outfall_stat<-outfall_DMR[outfall_DMR$statistical_base_code==stats[j],] #Keeping the reported statistics seperate promotes correct analysis of reported effluent values
        nodays<-numeric()
        nmbr<-numeric()
        for (l in 1:length(outfall_stat$nmbr_of_submission)){ #The number of submissions in a DMR correlates with the number of months within the monitoring period
          mo<-month(outfall_stat$monitoring_period_end_date[l]) 
          nmbr[l]<-outfall_stat$nmbr_of_submission[l]
          nodays[l]<-sum(mon_in_year$days[mon_in_year$month%in%seq(mo-nmbr[l]+1,mo)]) #Knowing the number of months and days within those months, we can get days in each monitoring period
        }
        FlowSumi[j]<-sum(outfall_stat$dmr_value_nmbr*nodays,na.rm=T)/sum(nodays)#Store the aggregated sum of all discharge for this outfall. This sheds light on seasonal trends but may be affected by outliers/typos
        FlowMedi[j]<-median(outfall_stat$dmr_value_nmbr,na.rm=T)#Store the median of all discharge records for this outfall. The median helps eliminate the need to spot quarterly vs. annual. monthly data
        Unitsi[j]<-unique(outfall_stat$standard_unit_desc)#Store the units being associated with this particular outfall
        LimitswNA<-unique(outfall_stat$limit_value_nmbr)#Store limits and eliminate if NA or take median if multiple
        if(length(LimitswNA)>1){#Occasionally limits report as NA which can alter this code
          if(length(LimitswNA[!is.na(LimitswNA)])>1){ #Remember limits are the permitted amounts of discharge the facility is allowed to release into surface water
            warning("More than one real limit found, only using median")
            Limiti[j]<-median(outfall_stat$limit_value_nmbr[outfall_stat$limit_end_date==max(outfall_stat$limit_end_date,na.rm=T)],na.rm=T)
          }else{
            Limiti[j]<-LimitswNA[!is.na(LimitswNA)] 
          }
        }else{
          Limiti[j]<-LimitswNA
        }
        Stat_Descriptioni[j]<-unique(outfall_stat$statistical_base_short_desc)#Store the statistic used in developing flows above
      }
      FlowSum<-c(FlowSum,FlowSumi)#Store flow, units, limits, and stat codes after every iteration into larger data frames.
      FlowMed<-c(FlowMed,FlowMedi)
      Units<-c(Units,Unitsi)
      Limit<-c(Limit,Limiti)
      Stat<-c(Stat,stats)
      Stat_Description<-c(Stat_Description,Stat_Descriptioni)
      outfall_num<-c(outfall_num,rep(outfall,length(stats)))
      VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length(stats))))
      ECHOID<-c(ECHOID,rep(sourceID,length(stats)))
    }
  }else{ #If the CWA regulated facility did not inlcude relevant DMR data (no recorded monitoring periods within specfied date range), flows, units, limits, and statistical codes are noted as NA values. 
    FlowSum<-c(FlowSum,NA)
    FlowMed<-c(FlowMed,NA)
    Units<-c(Units,NA)
    Limit<-c(Limit,NA)
    Stat<-c(Stat,NA)
    Stat_Description<-c(Stat_Description,NA)
    outfall_num<-c(outfall_num,NA)
    VPDESID<-c(VPDESID,NA)
    ECHOID<-c(ECHOID,sourceID)
  }
}

#############################################################################################################################
#################################################Export Data#################################################################

#These next few lines subset and export the data developed in the above loops
FlowFrame<-data.frame(ECHOID,VPDESID,outfall_num,FlowSum,FlowMed,Units,Limit,Stat,Stat_Description)
FlowFrame<-FlowFrame[!is.na(FlowFrame$VPDESID),] #Keep only completed cases
write.csv(FlowFrame,paste0(path,'/FlowFrame_2012_present.csv'))

#############################################################################################################################
###This block of code calls this script so that we can reference it in a seperate R markdown file.

source(paste0(path,'/ECHOInterface_McCarthy.R'))
library(knitr)
knit2html('G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Mark Down Documents/ECHOInterface_MD.Rmd')