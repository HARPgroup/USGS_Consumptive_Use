library(foreign)
library(rgdal)
library(dplyr)
library(XML)
library(RCurl)
library(readxl)
library(jsonlite)
library(lubridate)
library(httr)


state<-"VA"
startDate<-"01/01/2011"
endDate<-"6/20/2018"

hydrocode<-character()
varkey<-character()
tsvalue<-numeric()
tstime<-character()
tsendtime<-character()
tscode<-numeric()
VPDESID<-character()

uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
ECHO_Facilities<-read.csv(uri_summary,stringsAsFactors = F)
ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)

for (i in 1:length(ECHO_Facilities$SourceID)){
    sourceID<-ECHO_Facilities$SourceID[i]
    print(paste("Processing Facility ID: ", sourceID, "(",i," of ",length(ECHO_Facilities$SourceID),")", sep=""))
    DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe
    DMR_data<-read.csv(DMR_data,stringsAsFactors = F)
    DMR_data<-DMR_data[DMR_data$parameter_code==50050,]
    DMR_data$dmr_value_nmbr[DMR_data$nodi_code %in% c('C','7')]<-0
    data_length<-length(unique(DMR_data$monitoring_period_end_date))
        if(data_length>0){ 
          outfall_nmbr<-as.character(unique(DMR_data$perm_feature_nmbr)) 
          outfall_ID<-unique(DMR_data$perm_feature_nmbr) 
          for(j in 1:length(outfall_ID)){ 
            if(!is.na(as.numeric(outfall_ID[j]))){
              leadingzeros<-paste(rep(0,3-nchar(outfall_ID[j])),collapse= '')
              outfall_ID[j]<-paste0(leadingzeros,as.character(outfall_ID[j]))
            }else{
              outfall_ID[j]<-as.character(outfall_ID[j])
            }
          }
          for(k in 1:length(outfall_ID)){ 
            outfall<-as.character(outfall_ID[k])
            outfall_DMR<-DMR_data[DMR_data$perm_feature_nmbr==outfall_nmbr[k],]
            unique_stat_codes<-unique(outfall_DMR$statistical_base_code)
            tsvalue_i<-numeric(length(outfall_DMR$perm_feature_nmbr))
            tsendtime_i<-character()
            tscode_i<-numeric(length(outfall_DMR$perm_feature_nmbr))
            tstime_i<-character()
            varkey_i<-character(length(outfall_DMR$perm_feature_nmbr))
            for(l in 1:length(outfall_DMR$perm_feature_nmbr)){ 
              if(!is.na(outfall_DMR$statistical_base_code[l]=="MK")){ 
                tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="MK"])[l] 
                tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="MK"][l] 
                tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="MK"])[l]
                tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month"))
                varkey_i[l]<-"dmr_mon_mgd"
                
              }else if(!is.na(outfall_DMR$statistical_base_code[l]=="DB")){ #if it is missing a monthly average, look at daily average in MGD
                tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="DB"])[l] #takes discharge from monthly average column 
                tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="DB"][l] #character class
                tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="DB"])[l]
                tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) #uses Lubridate package, date must be object of class POSIXlt, POSIXct, or Date
                varkey_i[l]<-"dmr_day_mgd"
                
              }else if(!is.na(outfall_DMR$statistical_base_code[l]=="WA")){ #if it is also missing a daily average, look at weekly average in MGD
                tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="WA"])[l] #takes discharge from monthly average column 
                tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="WA"][l] #character class
                tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="WA"])[l]
                tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) #uses Lubridate package, date must be object of class POSIXlt, POSIXct, or Date
                varkey_i[l]<-"dmr_wk_mgd"
                
              }else if(!is.na(outfall_DMR$statistical_base_code[l]=="AB")){ #if it is also missing this, look at annual average in MGD
                tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="AB"])[l] #takes discharge from monthly average column 
                tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="AB"][l] #character class
                tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="AB"])[l]
                tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) #uses Lubridate package, date must be object of class POSIXlt, POSIXct, or Date
                varkey_i[l]<-"dmr_yr_mgd"
              }
              
            }
            tsvalue<-c(tsvalue,tsvalue_i)
            tsendtime<-c(tsendtime,tsendtime_i)
            tscode<-c(tscode,tscode_i)
            tstime<-c(tstime,tstime_i)
            varkey<-c(varkey,varkey_i)
            VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length((tsvalue_i)))))
            hydrocode<-paste0('echo_',VPDESID)
          }
        }else{ #parentheses connected to big if statement at top
          hydrocode<-c(hydrocode,NA)
          VPDESID<-c(VPDESID,NA)
          varkey<-c(varkey,NA)
          tsvalue<-c(tsvalue,NA)
          tstime<-c(tstime,NA)
          tsendtime<-c(tsendtime,NA)
          tscode<-c(tscode,NA)
        }
        }
        timeseries<-data.frame(hydrocode=hydrocode,varkey=varkey,tsvalue=tsvalue,tstime=tstime,tsendtime=tsendtime,tscode=tscode)
        timeseries<-timeseries[complete.cases(timeseries),]#returns outfalls that have data
        
      write.table(timeseries,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/timeseries.txt", sep="\t",row.names = F)
      