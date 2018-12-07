##################################################################################################################################
###########################################VAHydro to ECHO Import#################################################################

#This code serves to extract data about NPDES permitted facilities in Virginia from the EPA's ECHO REST Services.
#The metadata is then reformatted to fit the mapping structure of the VDEQ's surface and ground water modeling platform, VAHydro.

#The second half of the script serves to import formatted data into VAHydro using Drupal REST Services. 

##################################################################################################################################
##################################################Library Initialization##########################################################

library(foreign)
library(rgdal)
library(dplyr)
library(XML)
library(RCurl)
library(readxl)
library(jsonlite)
library(lubridate)
library(httr)
library(stringr)
library(RCurl)
library(xml2)

##################################################################################################################################
###############################################Inputs#############################################################################

state<-"VA"
Inputpath<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated"
Outputpath<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/ECHO_VAHydro Imports"

#Use Aggregated Flows generated from ECHOInterface Script and list of outfalls for creating release and conveyance points.
VPDES_Outfalls<-read.table(paste0(Inputpath,"/VPDES_Outfalls.txt"),sep="\t",header = T)



#Query from CWA ECHO REST Services to obtain all discharging facilities within state of interest
#Properties and attributes of the facility can be extracted by identifying the column it is located in (qcolumn).
Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,14,23,24,25,26,27,60,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&p_st=",state)
URL_Download<-getURL(Req_URL) #Download URL from above
URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID
GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,14,23,24,25,26,27,60,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&qid=",QID)
ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)#Ensure all facility names are in all caps using "toupper" command


#Individual Permits updated as of March 2018---contains design flow for facilities
#Warnings about unknown or uninitiliased columns: previous IP contact sheets named the columns differently. It doesn't hinder any processes though. 
GET('http://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contact%20Flow%20for%20WEB%20March%202018.xls?ver=2018-03-13-170732-267', 
    write_disk(temp <- tempfile(fileext = ".xls")))
VPDES_IP <- read_excel(temp, skip=5)
VPDES_IP<-VPDES_IP[!is.na(VPDES_IP$Facility),]
VPDES_IP$`Design Flow (MGD)`<-as.numeric(VPDES_IP$`Design Flow (MGD)`)
VPDES_IP<-VPDES_IP[!duplicated(VPDES_IP$`Permit Number`),] #getting rid of duplicates and looking at unique permits
VPDES_DesignFlow<-subset(VPDES_IP,select=c(3,15))
colnames(VPDES_DesignFlow)<-c("Facility.ID","DesignFlow_mgd")
ECHO_Facilities<-merge(ECHO_Facilities,VPDES_DesignFlow,by="Facility.ID",all.x=T) #put design flow in facility spreadsheet
design_flow<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/ECHO_VAHydro Imports/design_flow.txt", sep="\t", header=T)

rm(Req_URL,URL_Download,URL_Parse,QID,GET_Facilities,VPDES_IP)



##################################################################################################################################
################################################Imports###########################################################################

#1 Import Outfall Timeseries Data

#Outfall dH Timeseries Mapping

#hydrocode, varkey, tsvalue, tstime, tsendtime, tscode
#hydrocode is for each unique outfall not facility--maybe include potential flag for virtual outfalls???

#tsvalue is the dmr_value_nmbr
#tsendtime is the monitoring_period_end_date
#tstime is the period end date minus the number of submissions-always the first of the month 
#Need to configure number of submissions based on limit set
#Number of submissions is the number of months of discharges represented
#1-monthly
#2-bi-monthly
#3-quarterly
#4-triannual
#6-semi-annual
#12-annual 

#DMR data can be found from the following base URL query: 
#https://ofmpub.epa.gov/echo/eff_rest_services.get_effluent_chart?

state<-"VA"
startDate<-"01/01/2010" #mm/dd/yyyy: data on ECHO is limited to 2012 for most sites or 2009 for a few
endDate<-Sys.Date()
endDate<-format(as.Date(endDate), "%m/%d/%Y")
options(scipen=999) #Disable scientific notation

#Create Place Holders for Desired Variables
hydrocode<-character()
varkey<-character()
tsvalue<-numeric()
tstime<-character()
tsendtime<-character()
tscode<-numeric()
VPDESID<-character()
nodi<-character()

#Query from CWA ECHO REST Services to obtain all discharging facilities within state of interest
Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,14,23,24,25,26,27,60,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&p_st=",state)
URL_Download<-getURL(Req_URL) #Download URL from above
URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID
GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,14,23,24,25,26,27,60,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&qid=",QID)
ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)#Ensure all facility names are in all caps using "toupper" command

rm(Req_URL,URL_Download,URL_Parse,QID,GET_Facilities)

#This loop goes through each CWA regulated facility one by one to extract reported discharges 
#from each unique outfall. In the end, there will be ECHO_Facilities table with timeseries data for each
#outfall located in VA. 
for (i in 1:length(ECHO_Facilities$SourceID)){
  sourceID<-ECHO_Facilities$SourceID[i]
  print(paste("Processing Facility ID: ", sourceID, "(",i," of ",length(ECHO_Facilities$SourceID),")", sep=""))
  DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe
  DMR_data<-read.csv(DMR_data,stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
  DMR_data<-DMR_data[DMR_data$parameter_code==50050,]#only looks at Flow, in conduit ot thru treatment plant - there are 347 parameter codes defined in ECHO
  DMR_data$dmr_value_nmbr[DMR_data$nodi_code %in% c('C','7')]<-0#nodi_code is the unique code indicating the reason why an expected DMR value was not submitted. C=No Discharge, B=Below Detection Limit, 9=Conditional Monitoring, 7=parameter/value not reported
  data_length<-length(unique(DMR_data$monitoring_period_end_date))#sees if there is any reported data worth extracting and examining
  if(data_length>0){ #if the value is NOT NA, enter loop
    outfall_nmbr<-as.character(unique(DMR_data$perm_feature_nmbr)) #Stores Outfalls which are called permanent features in the DMR
    outfall_ID<-unique(DMR_data$perm_feature_nmbr) #perm_feature_nmbr is a three-character code in ICIS-NPDES that identifies the point of discharge
    for(j in 1:length(outfall_ID)){ #If the code is less than three characters in the .CSV, append zeros to the beginning of the number (e.g., 1 is equivalent to 001)
      if(!is.na(as.numeric(outfall_ID[j]))){
        leadingzeros<-paste(rep(0,3-nchar(outfall_ID[j])),collapse= '')
        outfall_ID[j]<-paste0(leadingzeros,as.character(outfall_ID[j]))
      }else{
        outfall_ID[j]<-as.character(outfall_ID[j])#if the outfall number is already three digits, no reformatting needed
      }
    }
    for(k in 1:length(outfall_ID)){ #Now we go through the DMR attached to each unique individual outfall and extract the information we would like
      outfall<-as.character(outfall_ID[k])
      outfall_DMR<-DMR_data[DMR_data$perm_feature_nmbr==outfall_nmbr[k],]#specifies that we want to go through each unique outfall
      unique_stat_codes<-unique(outfall_DMR$statistical_base_code)#collects the unique statistical codes reported for this specific outfall
      tsvalue_i<-numeric(length(outfall_DMR$perm_feature_nmbr)) #Create variables that will store DMR data for each outfall
      tsendtime_i<-character()
      tscode_i<-numeric(length(outfall_DMR$perm_feature_nmbr))
      tstime_i<-character()
      varkey_i<-character(length(outfall_DMR$perm_feature_nmbr))
      nodi_i<-character()
      for(l in 1:length(outfall_DMR$perm_feature_nmbr)){ #extracts discharge quantity from each outfall by examining the statistical code associated with it. In this case, we want an average.
        if(!is.na(outfall_DMR$statistical_base_code[l]=="MK")){ #ideally, we want a monthly average, which is indicated by the code "MK"
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="MK"])[l] 
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="MK"][l] #character class
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="MK"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month"))#uses Lubridate package, date must be object of class POSIXlt, POSIXct, or Date
          varkey_i[l]<-"dmr_mon_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="MK"][l] 
          
        }else if(!is.na(outfall_DMR$statistical_base_code[l]=="DB")){ #if it is missing a monthly average, look at daily average in MGD
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="DB"])[l]  
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="DB"][l] #character class
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="DB"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) 
          varkey_i[l]<-"dmr_day_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="DB"][l] 
          
        }else if(!is.na(outfall_DMR$statistical_base_code[l]=="WA")){ #if it is also missing a daily average, look at weekly average in MGD
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="WA"])[l] 
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="WA"][l] 
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="WA"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) 
          varkey_i[l]<-"dmr_wk_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="WA"][l] 
          
        }else if(!is.na(outfall_DMR$statistical_base_code[l]=="AB")){ #if it is also missing this, look at annual average in MGD
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="AB"])[l]  
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="AB"][l] 
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="AB"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) 
          varkey_i[l]<-"dmr_yr_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="AB"][l] 
        }
        
      }
      #Now we store the values we get from each outfall in each facility[i] in a larger matrix
      #We do this so that results are not over written after each iteration
      tsvalue<-c(tsvalue,tsvalue_i) 
      tsendtime<-c(tsendtime,tsendtime_i)
      tscode<-c(tscode,tscode_i)
      tstime<-c(tstime,tstime_i)
      varkey<-c(varkey,varkey_i)
      nodi<-c(nodi,nodi_i)
      VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length((tsvalue_i)))))
      hydrocode<-paste0('echo_',VPDESID)
    }
  }else{ #if the DMR contains no data, set variables to NA
    hydrocode<-c(hydrocode,NA)
    VPDESID<-c(VPDESID,NA)
    varkey<-c(varkey,NA)
    tsvalue<-c(tsvalue,NA)
    tstime<-c(tstime,NA)
    tsendtime<-c(tsendtime,NA)
    tscode<-c(tscode,NA)
    nodi<-c(nodi,NA)
  }
}
timeseries<-data.frame(hydrocode=hydrocode,varkey=varkey,tsvalue=tsvalue,tstime=tstime,tsendtime=tsendtime,tscode=tscode,nodi=nodi)
timeseries<-timeseries[!(is.na(timeseries$tsendtime)),]#returns outfalls that have data
timeseries$tsendtime<-format(mdy(timeseries$tsendtime))
nodi<-timeseries

timeseries<-subset(timeseries,select=-c(7))
#write.table(timeseries,paste0(Outputpath,"/timeseries.txt"),sep="\t",row.names = F)
#write.table(timeseries,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/timeseries.txt",sep="\t",row.names = F)
save.image(file="G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/timeseries_2010_present.RData")

timeseries$facilityID<-gsub("echo_","", as.character(timeseries$hydrocode))
timeseries$facilityID<-substr(timeseries$facilityID,1,nchar(timeseries$facilityID)-3)
#timeseries<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/ECHO_VAHydro Imports/timeseries.txt",sep='\t',header=T)
#timeseriesNA<-subset(timeseries,subset=is.na(timeseries$tsvalue)) #599 NA Values
##################################################################################################################################
#############################################1: Import Permits####################################################################
#Purpose: Extract all NPDES Permits in VA to import into VAHydro. This includes active and inactive. 

#dH Adminreg Feature Mapping
#bundle, admincode, description, name, ftype, fstatus, startdate, enddate, permit_id, dh_link_admin_reg_issuer

#Under the CWA, all facilities discharging pollutant from a point source must have a NPDES Permit
#Status is considered Effective, Admin Continued, Expired, Not Needed, Pending, Retured, or Terminated

#important to note that sourceID in matrix a is equal to admincode. It becomes hydrocode when word 'echo_' is added before
#permit ID not to be confused with VPDESID which is unique for each outfall

adminreg<-data.frame(bundle='permit', admincode=ECHO_Facilities$SourceID, description='National Pollutant Discharge Elimination System (NPDES) Permit', name=ECHO_Facilities$CWPName)

adminreg$ftype<-'npdes'

for (i in 1:length(adminreg$admincode)){
  if (length(grep('Effective',ECHO_Facilities$CWPPermitStatusDesc[i]))>0|
      length(grep('Compliance Tracking Off',ECHO_Facilities$CWPPermitStatusDesc[i]))>0|
      length(grep('Admin Continued',ECHO_Facilities$CWPPermitStatusDesc[i]))>0|
      length(grep('Effective; Compliance Tracking Partially Off',ECHO_Facilities$CWPPermitStatusDesc[i]))>0){
    adminreg$fstatus[i]<-'active'
  }
  else if (length(grep('Terminated', ECHO_Facilities$CWPPermitStatusDesc[i]))>0|
           length(grep('Terminated; Compliance Tracking Off', ECHO_Facilities$CWPPermitStatusDesc[i]))>0){
    adminreg$fstatus[i]<-'revoked'
  }
  else if (length(grep('Not Needed', ECHO_Facilities$CWPPermitStatusDesc[i]))>0|
           length(grep('NA', ECHO_Facilities$CWPPermitStatusDesc[i]))>0){
    adminreg$fstatus[i]<-'unknown'
  }
  else if (length(grep('Expired', ECHO_Facilities$CWPPermitStatusDesc[i]))>0){
    adminreg$fstatus[i]<-'expired'
  }
}

adminreg$startdate<-ECHO_Facilities$CWPEffectiveDate

#end date is permit expiration date rather than limit_end_date
adminreg$enddate<-ECHO_Facilities$CWPExpirationDate
adminreg$permit_id<-ECHO_Facilities$SourceID
adminreg$dh_link_admin_reg_issuer<-'epa'
adminreg$startdate<-as.Date(adminreg$startdate,format="%m/%d/%Y")
adminreg$enddate<-as.Date(adminreg$enddate,format="%m/%d/%Y")
#write.table(adminreg,paste0(Outputpath,"/adminreg.txt"),sep="\t",row.names = F)

##################################################################################################################################
###################################################2 Import Facilities############################################################
#Purpose: Extract all discharging facilities found in ECHO 

#Facility dH Feature Mapping

#bundle, name, ftype, hydrocode, fstatus, wkt_geom, address1, city, dh_link_admin_location

#This section generates the facility import in VA Hydro. There is a long series of "if" checks
#that format facility names and search them for any buzz words that might help group them into 
#a VA Hydro ftype. For instance, "Surry Power Station" contains the word "Power" and is likely a 
#power plant of some kind
facilities<-data.frame(bundle='facility',name=ECHO_Facilities$CWPName)
facilities$ftype<-'unknown'
facilities$hydrocode<-paste0("echo_",ECHO_Facilities$SourceID)
for (i in 1:length(facilities$hydrocode)){
  if (length(grep('WTP',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WASTE WATER',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WATER TREATMENT PLANT',ECHO_Facilities$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT PLANT',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WATER RECLAMATION',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WTF',ECHO_Facilities$CWPName[i]))>0|
      length(grep('STP',ECHO_Facilities$CWPName[i]))>0|
      length(grep('SEW. TREAT. PLANT',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WWTREAT PLANT',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WATER TREATMEN',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WATER TREATMENT PL',ECHO_Facilities$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT',ECHO_Facilities$CWPName[i]))>0|
      length(grep('POLLUTION CONTROL',ECHO_Facilities$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT PLAN',ECHO_Facilities$CWPName[i]))>0|
      length(grep('POLLUTION CONTR',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WASTEWATE',ECHO_Facilities$CWPName[i]))>0|
      length(grep('WT PLANT',ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'wwtp'
  } else if (length(grep('COMBINED SEW SYSTEM',ECHO_Facilities$CWPName[i]))|
             length(grep('COMBINED SEWER SYSTEM',ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'public water supply'
  } else if (length(grep("POWER",ECHO_Facilities$CWPName[i]))>0|
             length(grep("ENERGY CENTER",ECHO_Facilities$CWPName[i]))>0|
             length(grep("ELECTRIC",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'fossilpower'
    if(length(grep("NUCLEAR",ECHO_Facilities$CWPName[i]>0))){
      facilities$ftype[i]<-"nuclearpower"
    } else if(length(grep("HYDRO",ECHO_Facilities$CWPName[i]>0))){
      facilities$ftype[i]<-"hydropower"
    }
  } else if(length(grep('NUCLEAR',ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'nuclearpower' 
  }else if (length(grep("MINE",ECHO_Facilities$CWPName[i]))>0|
            length(grep("QUARRY",ECHO_Facilities$CWPName[i]))>0|
            length(grep("MINING",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'mining'
  }else if (length(grep("MS4",ECHO_Facilities$CWPName[i]))>0|
           length(grep("SCHOOL",ECHO_Facilities$CWPName[i]))>0|
           length(grep("TOWN",ECHO_Facilities$CWPName[i]))>0|
           length(grep("COMMUNITY",ECHO_Facilities$CWPName[i]))>0|
           length(grep('ARMY',ECHO_Facilities$CWPName[i]))>0|
           length(grep('NAVY',ECHO_Facilities$CWPName[i]))>0|
           length(grep("COUNTY",ECHO_Facilities$CWPName[i]))>0|
           length(grep("HOME",ECHO_Facilities$CWPName[i]))>0|
           length(grep("CHURCH",ECHO_Facilities$CWPName[i]))>0|
           length(grep("HOMES",ECHO_Facilities$CWPName[i]))>0|
           length(grep("MUSEUM",ECHO_Facilities$CWPName[i]))>0|
           length(grep("ESTATES",ECHO_Facilities$CWPName[i]))>0|
           length(grep("CAR WASH",ECHO_Facilities$CWPName[i]))>0|
           length(grep("LANDING",ECHO_Facilities$CWPName[i]))>0|
           length(grep("CORRECTION CENTER",ECHO_Facilities$CWPName[i]))>0|
           length(grep("DETENTION CENTER",ECHO_Facilities$CWPName[i]))>0|
           length(grep("CORRECTIONAL CENTER",ECHO_Facilities$CWPName[i]))>0|
           length(grep("CORRECTIONAL UNIT",ECHO_Facilities$CWPName[i]))>0|
           length(grep("VILLAGE",ECHO_Facilities$CWPName[i]))>0|
           length(grep("UNIVERSITY",ECHO_Facilities$CWPName[i]))>0|
           length(grep("HOSPITAL",ECHO_Facilities$CWPName[i]))>0|
           length(grep("RESTAURANT",ECHO_Facilities$CWPName[i]))>0|
           length(grep('AUTHORITY',ECHO_Facilities$CWPName[i]))>0|
           length(grep('TUNNEL',ECHO_Facilities$CWPName[i]))>0|
           length(grep("CITY OF",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'municipal'
  } else if(length(grep("FARM",ECHO_Facilities$CWPName[i]))>0|
            length(grep("FISH CULTURAL",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-"agriculture"
  }else if(length(grep("CORPORATION",ECHO_Facilities$CWPName[i]))>0|
            length(grep("INC.",ECHO_Facilities$CWPName[i]))>0|
            length(grep("INCORPORATED",ECHO_Facilities$CWPName[i]))>0|
            length(grep('CORP.',ECHO_Facilities$CWPName[i]))>0|
            length(grep('LLC',ECHO_Facilities$CWPName[i]))>0|
            length(grep('AIRPORT',ECHO_Facilities$CWPName[i]))>0|
            length(grep("COMPANY",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'commercial'
  } else if(length(grep('PAPER',ECHO_Facilities$CWPName[i]))>0|
            length(grep('COMPANY',ECHO_Facilities$CWPName[i]))>0|
            length(grep('CONCRETE',ECHO_Facilities$CWPName[i]))>0|
            length(grep('WOOD',ECHO_Facilities$CWPName[i]))>0|
            length(grep('LUMBER',ECHO_Facilities$CWPName[i]))>0|
            length(grep('MOTORS',ECHO_Facilities$CWPName[i]))>0|
            length(grep('PRODUCTS',ECHO_Facilities$CWPName[i]))>0|
            length(grep('TIMBER',ECHO_Facilities$CWPName[i]))>0|
            length(grep('CHEMICAL',ECHO_Facilities$CWPName[i]))>0|
            length(grep('INDUSTRIES',ECHO_Facilities$CWPName[i]))>0|
            length(grep('INDUSTRIAL PARK',ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'industrial'
  }  else if(length(grep('PLANT',ECHO_Facilities$CWPName[i]))>0|
             length(grep('MANUFACTURING',ECHO_Facilities$CWPName[i]))>0){ 
    facilities$ftype[i]<-'manufacturing'
  }
  facilities$fstatus[i]<-'inactive'
  if (ECHO_Facilities$SourceID[i]%in%timeseries$facilityID){ #if it is reporting flow for ECHO, it is most likely active. Also check status of permit.
    facilities$fstatus[i]<-'active'
  }
  if(!is.na(ECHO_Facilities$FacLat[i]) & !is.na(ECHO_Facilities$FacLong[i])){
    facilities$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$SourceID==ECHO_Facilities$SourceID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$SourceID==ECHO_Facilities$SourceID[i]],')')
  } else {
    lat<-VPDES_Outfalls$Latitude[VPDES_Outfalls$FacilityID==VPDES_Outfalls$FacilityID[i]]
    long<-VPDES_Outfalls$Longitude[VPDES_Outfalls$FacilityID==VPDES_Outfalls$FacilityID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        facilities$dh_geofield[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        facilities$dh_geofield[i]<-'NULL'
      }
    }
  }
}

facilities$address1<-ECHO_Facilities$CWPStreet
facilities$city<-ECHO_Facilities$CWPCity
facilities$dh_link_admin_location<-ECHO_Facilities$SourceID

#write.table(facilities,paste0(Outputpath,"/facilities.txt"),sep="\t",row.names = F)

##################################################################################################################################
###########################################3 Release Point Generation#############################################################

#Generation of the release point imports. In essence, this portion just formats various data from both the facility
#and the outfall list (VPDES_Outfalls) to create the release point attributes and geometry.

VPDES_Outfalls$VPDESID<-as.character(VPDES_Outfalls$VPDESID)
VPDES_Outfalls$FacilityID<-as.character(VPDES_Outfalls$FacilityID)
releasepoint<-data.frame(bundle=rep('transfer',length(VPDES_Outfalls$VPDESID)))
for (i in 1:length(releasepoint$bundle)){
  releasepoint$name[i]<-paste0('TO ',VPDES_Outfalls$VPDESID[i])
  releasepoint$ftype[i]<-'release'
  releasepoint$hydrocode[i]<-paste0('vahydro_',VPDES_Outfalls$VPDESID[i])
  if(VPDES_Outfalls$VPDESID[i]%in%gsub("echo_","", as.character(timeseries$hydrocode))){
    releasepoint$fstatus[i]<-'active'  
  } else {
    releasepoint$fstatus[i]<-'inactive'
  }
  if(!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]])){
    releasepoint$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]],')')
  } else {
    lat<-VPDES_Outfalls$Latitude[VPDES_Outfalls$FacilityID==VPDES_Outfalls$FacilityID[i]]
    long<-VPDES_Outfalls$Longitude[VPDES_Outfalls$FacilityID==VPDES_Outfalls$FacilityID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        releasepoint$dh_geofield[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        releasepoint$dh_geofield[i]<-'NULL'
      }
    }
  }
  releasepoint$dh_link_facility_mps[i]<-paste0('echo_',VPDES_Outfalls$FacilityID[i])
}
#write.table(releasepoint,paste0(Outputpath,"/releasepoint.txt"),sep="\t",row.names = F)

########################################################################################################################
#Conveyance Generation

#Generates the conveyance import using the outfall list in 'VPDES_Outfalls'
conveyance<-data.frame(bundle=rep('conveyance',length(VPDES_Outfalls$VPDESID)))
for (i in 1:length(conveyance$bundle)){
  conveyance$name[i]<-paste0(VPDES_Outfalls$FacilityID[i],' TO ',VPDES_Outfalls$VPDESID[i])
  conveyance$ftype[i]<-'water_transfer'
  conveyance$hydrocode[i]<-paste0('vahydro_',VPDES_Outfalls$FacilityID[i],'_',VPDES_Outfalls$VPDESID[i])
  if(VPDES_Outfalls$VPDESID[i]%in%gsub("echo_","", as.character(timeseries$hydrocode))){
    conveyance$fstatus[i]<-'active'  
  } else {
    conveyance$fstatus[i]<-'inactive'
  }
  conveyance$field_dh_from_entity[i]<-paste0('vahydro_',VPDES_Outfalls$VPDESID[i])
  conveyance$field_dh_to_entity[i]<-paste0('echo_',VPDES_Outfalls$VPDESID[i])
}
#write.table(conveyance,paste0(Outputpath,"/conveyance.txt"),sep="\t",row.names = F)

#Outfall_Outfalls Generation
#Reformats 'VPDES_Outfalls' using available VPDES or ECHO geometry data and ECHO attributes
outfalls<-data.frame(bundle=rep('transfer',length(VPDES_Outfalls$VPDESID)))
for (i in 1:length(outfalls$bundle)){
  outfalls$name[i]<-paste0('FROM ',VPDES_Outfalls$FacilityID[i])
  outfalls$ftype[i]<-'outfall'
  outfalls$hydrocode[i]<-paste0('echo_',VPDES_Outfalls$VPDESID[i])
  if(VPDES_Outfalls$VPDESID[i]%in%gsub("echo_","", as.character(timeseries$hydrocode))){
    outfalls$fstatus[i]<-'active'  
  } else {
    outfalls$fstatus[i]<-'inactive'
  }
  if(!is.na(VPDES_Outfalls$Latitude[VPDES_Outfalls$VPDESID==VPDES_Outfalls$VPDESID[i]]) & !is.na(VPDES_Outfalls$Longitude[VPDES_Outfalls$VPDESID==VPDES_Outfalls$VPDESID[i]])){
    outfalls$dh_geofield[i]<-paste0('POINT (',VPDES_Outfalls$Longitude[VPDES_Outfalls$VPDESID==VPDES_Outfalls$VPDESID[i]],' ',VPDES_Outfalls$Latitude[VPDES_Outfalls$VPDESID==VPDES_Outfalls$VPDESID[i]],')')  
    } else if (!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]])) {
      outfalls$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$SourceID==VPDES_Outfalls$FacilityID[i]])
    } else {
      outfalls$dh_geofield[i]<-'NULL'
  }
  outfalls$dh_link_facility_mps[i]<-paste0('echo_',VPDES_Outfalls$FacilityID[i])
}
#write.table(outfalls,paste0(Outputpath,"/outfalls.txt"),sep="\t",row.names = F)


#################################################################################
#3 Import Facility Metadata

#Facility dH Property Mapping

#hydrocode, varkey, propname, propvalue, proptext, propcode, startdate, enddate

last_inspect<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='last_inspect', propname='last_inspect', propvalue='',proptext='',propcode='',startdate=ECHO_Facilities$CWPDateLastInspection,enddate='')
#write.table(last_inspect,paste0(Outputpath,"/last_inspect.txt"),sep="\t",row.names = F)

css<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='css', propname='css', propvalue='',proptext='',propcode=ECHO_Facilities$CWPCsoFlag, startdate='',enddate='')
#write.table(css,paste0(Outputpath,"/css.txt"),sep="\t",row.names = F)

cwp_cso_outfalls<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='cwp_cso_outfalls', propname='cwp_cso_outfalls', propvalue=ECHO_Facilities$CWPCsoOutfalls,proptext='',propcode='', startdate='',enddate='')
cwp_cso_outfalls$propvalue[is.na(cwp_cso_outfalls$propvalue)]<-""
#write.table(cwp_cso_outfalls,paste0(Outputpath,"/cwp_cso_outfalls.txt"),sep="\t",row.names = F)

wb_gnis_name<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='wb_gnis_name', propname='wb_gnis_name', propvalue='', proptext='',propcode=ECHO_Facilities$RadGnisName, startdate='',enddate='')
#write.table(wb_gnis_name,paste0(Outputpath,"/wb_gnis_name.txt"),sep="\t",row.names = F)

reachcode_rad<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='reachcode_rad', propname='reachcode_rad', propvalue='', proptext='',propcode=ECHO_Facilities$RadReachcode, startdate='',enddate='')
reachcode_rad$propcode[is.na(reachcode_rad$propcode)]<-""
#write.table(reachcode_rad,paste0(Outputpath,"/reachcode_rad.txt"),sep="\t",row.names = F)

impair_cause<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='impair_cause', propname='impair_cause', propvalue='', proptext=ECHO_Facilities$AttainsCauseGroups,propcode='', startdate='',enddate='')
#write.table(impair_cause,paste0(Outputpath,"/impair_cause.txt"),sep="\t",row.names = F)




design_flow<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$SourceID), varkey='design_flow', propname='design_flow', propvalue=)

###################################################################################

##################################################################################################################################
###########################################Pushing DMR timeseries Data to VAHydro#################################################

  
  site <- "http://deq1.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
  hydro_tools <- 'G:\\My Drive\\hydro-tools' #location of hydro-tools repo
  
  #----------------------------------------------
  
  #Generate REST token for authentication              
  rest_uname = FALSE
  rest_pw = FALSE
  source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
  source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
  token <- rest_token(site, token, rest_uname, rest_pw)

  ############################################################################################
  # RETRIEVE EPA AGENCY ADMINREG FEATURE
  ############################################################################################
  
  agency_inputs <- list(
    bundle = 'authority',
    ftype = 'federal_enviro_agency',
    admincode = 'epa',
    stringsAsFactors = FALSE
  ) 
  agency.dataframe <- getAdminregFeature(agency_inputs, site, adminreg_feature)
  agency.adminid <- as.character(agency.dataframe$adminid)
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE PERMIT ADMINREG FEATURE
  ############################################################################################  
  #1943 Permits use sappply

  permit.adminid<-character()#to store all iterations in one big dataframe 
  permit.dataframe<-data.frame()

  for (i in 1:length(adminreg$admincode)){
  permit_inputs <- list(
    bundle = as.character(adminreg$bundle[i]),
    ftype = as.character(adminreg$ftype[i]),
    admincode = as.character(adminreg$admincode[i]),
    name = as.character(adminreg$name[i]),
    fstatus = as.character(adminreg$fstatus[i]),
    description = as.character(adminreg$description[i]),
    startdate = format(as.POSIXlt(adminreg$startdate[i]),"%s"), 
    enddate = format(as.POSIXlt(adminreg$enddate[i]),"%s"),
    permit_id = as.character(adminreg$admincode[i]),
    dh_link_admin_reg_issuer = agency.adminid, #actual id and not "epa"
    stringsAsFactors = FALSE
  ) 
  permit.dataframe_i <- postAdminregFeature(permit_inputs, site, adminreg_feature)
  print(permit.dataframe_i) #print status of updating: error 403 means administration block
  permit.dataframe_i <- getAdminregFeature(permit_inputs, site, adminreg_feature)
  permit.dataframe<-rbind(permit.dataframe,permit.dataframe_i)
  
  permit.adminid_i <- as.character(permit.dataframe_i$adminid) #store each adminid into larger data frame
  permit.adminid<-c(permit.adminid,permit.adminid_i)
  
  print(paste("Processing Permit ",i," of ", length(adminreg$admincode)))
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE FACILITY DH FEATURE
  ############################################################################################  
  #1943 Facilities
  facility.dataframe <- data.frame()
  facility.hydroid <- character()
  
  for (i in 1:length(facilities$hydrocode)){
  facility_inputs <- list(
    bundle = as.character(facilities$bundle[i]),
    ftype = as.character(facilities$ftype[i]),
    hydrocode = as.character(facilities$hydrocode[i]),
    name = as.character(facilities$name[i]),
    fstatus = as.character(facilities$fstatus[i]),
    address1 = as.character(facilities$address1[i]),
    city = as.character(facilities$city[i]),
    dh_link_admin_location =  permit.adminid[permit.dataframe$admincode%in%facilities$dh_link_admin_location[i]], #%in% operator looks for matches in left operand and returns permit.adminid if there is one
    dh_geofield = as.character(facilities$dh_geofield[i]),
    stringsAsFactors=FALSE
  ) 
  facility.dataframe_i <- postFeature(facility_inputs, site, feature)
  print(facility.dataframe_i)
  facility.dataframe_i <- getFeature(facility_inputs, token, site, feature) #need token now for access
  facility.dataframe<-rbind(facility.dataframe,facility.dataframe_i)
  
  facility.hydroid_i <- as.character(facility.dataframe_i$hydroid)
  facility.hydroid<-c(facility.hydroid,facility.hydroid_i) #store each hydroid into larger dataframe
  
  print(paste("Processing Facility ",i," of ", length(facilities$dh_link_admin_location)))
  }
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE FACILITY METADATA PROPERTIES
  # Make sure hydrocodes are in same order as in the facility data frame as well
  ############################################################################################   
  # Waterbody Name (GNIS): Name of waterbody from the Geographic Names Information System database where the facility is permitted to discharge directly
  
  property.pid_wbgnis<-character()
  property.dataframe<-data.frame()
  
  for (i in 1:length(wb_gnis_name$hydrocode)){
  prop_inputs <-list(
    featureid = facility.hydroid[facility.dataframe$hydrocode%in%wb_gnis_name$hydrocode[i]],
    varkey = as.character(wb_gnis_name$varkey[i]),
    entity_type = 'dh_feature',
    propname = as.character(wb_gnis_name$propname[i]),
    propvalue = NULL,
    proptext = NULL,
    propcode = as.character(wb_gnis_name$propcode[i]),
    startdate = NULL,
    enddate = NULL
  )
  property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
  print(property.dataframe_i)
  property.dataframe_i <- getProperty(prop_inputs, site, prop)
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
  
  property.pid_i <- as.character(property.dataframe$pid)
  property.pid_wbgnis<-c(property.pid_wbgnis,property.pid_i)
  
  print(paste("Processing Hydrocode ",i," of ", length(wb_gnis_name$hydrocode)))
  }
  
  ############################################################################################ 
  #Combined Sewer System (CWPCsoFlag): The discharge from a combined sewer system at a point prior to a treatment plant
  
  property.pid_css<-character()
  
  for (i in 1:length(css$hydrocode)){
    prop_inputs <-list(
      featureid = facility.hydroid[facility.dataframe$hydrocode%in%css$hydrocode[i]],
      varkey = as.character(css$varkey[i]),
      entity_type = 'dh_feature',
      propname = as.character(css$propname[i]),
      propvalue = NULL,
      proptext = NULL,
      propcode = as.character(css$propcode[i]),
      startdate = NULL,
      enddate = NULL
    )
    property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs, site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
    property.pid_i <- as.character(property.dataframe$pid)
    property.pid_css<-c(property.pid_css,property.pid_i)
    
    print(paste("Processing Hydrocode ",i," of ", length(css$hydrocode)))
  }
  
  ############################################################################################ 
  #Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)
  
  property.pid_cso<-character()
  
  for (i in 1:length(cwp_cso_outfalls$hydrocode)){
    prop_inputs <-list(
      featureid = facility.hydroid[facility.dataframe$hydrocode%in%cwp_cso_outfalls$hydrocode[i]],
      varkey = as.character(cwp_cso_outfalls$varkey[i]),
      entity_type = 'dh_feature',
      propname = as.character(cwp_cso_outfalls$propname[i]),
      propvalue = as.numeric(cwp_cso_outfalls$propvalue[i]),
      proptext = NULL,
      propcode = NULL,
      startdate = NULL,
      enddate = NULL
    )
    property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs, site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
    property.pid_i <- as.character(property.dataframe$pid)
    property.pid_cso<-c(property.pid_cso,property.pid_i) #unique pids for combined sewer outfall property
    
    print(paste("Processing Hydrocode ",i," of ", length(cwp_cso_outfalls$hydrocode)))
  }
  
  ############################################################################################ 
  #Impairment Class or Category of the Waterbody (impair_cause)
  
  property.pid_impair<-character()
  
  for (i in 1:length(impair_cause$hydrocode)){
    prop_inputs <-list(
      featureid = facility.hydroid[facility.dataframe$hydrocode%in%impair_cause$hydrocode[i]],
      varkey = as.character(impair_cause$varkey[i]),
      entity_type = 'dh_feature',
      propname = as.character(impair_cause$propname[i]),
      propvalue = NULL,
      proptext = as.character(impair_cause$proptext[i]),
      propcode = NULL,
      startdate = NULL,
      enddate = NULL
    )
    property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs, site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
    property.pid_i <- as.character(property.dataframe$pid)
    property.pid_impair<-c(property.pid_impair,property.pid_i)
    
    print(paste("Processing Hydrocode ",i," of ", length(impair_cause$hydrocode)))
  }
  
  ############################################################################################ 
  #Date of most recent inspection of the facility (last_inspect)
  
  property.pid_inspect<-character()
  
  for (i in 1:length(last_inspect$hydrocode)){
    prop_inputs <-list(
      featureid = facility.hydroid[facility.dataframe$hydrocode%in%last_inspect$hydrocode[i]],
      varkey = as.character(last_inspect$varkey[i]),
      entity_type = 'dh_feature',
      propname = as.character(last_inspect$propname[i]),
      propvalue = NULL,
      proptext = NULL,
      propcode = NULL,
      startdate = format(as.POSIXlt(last_inspect$startdate[i]),"%s"),
      enddate = NULL
    )
    property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs, site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
    property.pid_i <- as.character(property.dataframe$pid)
    property.pid_inspect<-c(property.pid_inspect,property.pid_i)
    
    print(paste("Processing Hydrocode ",i," of ", length(last_inspect$hydrocode)))
  }
  
  ############################################################################################ 
  #Unique ID for Waterbody (reachcode_rad)
  
  for (i in 1:length(reachcode_rad$hydrocode)){
    prop_inputs <-list(
      featureid = facility.hydroid[facility.dataframe$hydrocode%in%reachcode_rad$hydrocode[i]],
      varkey = as.character(reachcode_rad$varkey[i]),
      entity_type = 'dh_feature',
      propname = as.character(reachcode_rad$propname[i]),
      propvalue = NULL,
      proptext = NULL,
      propcode = as.character(reachcode_rad$propcode[i]),
      startdate = NULL,
      enddate = NULL
    )
    property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs, site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
    
    property.pid <- as.character(property.dataframe$pid)
    
    print(paste("Processing Hydrocode ",i," of ", length(reachcode_rad$hydrocode)))
  }
  
  ############################################################################################ 
  # Facility Design Flow in MGD (design_flow)
  
  property.pid_desflow<-character()
  property.dataframe<-data.frame()
  
  for (i in 1:length(design_flow$hydrocode)){
    prop_inputs <-list(
      featureid = facility.hydroid[facility.dataframe$hydrocode%in%design_flow$hydrocode[i]],
      varkey = as.character(design_flow$varkey[i]),
      entity_type = 'dh_feature',
      propname = as.character(design_flow$propname[i]),
      propvalue = as.numeric(design_flow$propvalue[i]),
      proptext = NULL,
      propcode = as.character(design_flow$propcode[i]),
      startdate = NULL,
      enddate = NULL
    )
    property.dataframe_i <- postProperty(prop_inputs, fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs, site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
    
    property.pid_i <- as.character(property.dataframe$pid)
    property.pid_desflow<-c(property.pid_desflow,property.pid_i)
    
    print(paste("Processing Hydrocode ",i," of ", length(design_flow$hydrocode)))
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE RELEASE DH FEATURE
  ############################################################################################  
  #2,895 release points
  release.hydroid<-character()
  release.dataframe<-data.frame()
  
  for (i in 1:length(releasepoint$hydrocode)){
  release_inputs <- list(
    bundle = as.character(releasepoint$bundle[i]),
    ftype = as.character(releasepoint$ftype[i]),
    hydrocode = as.character(releasepoint$hydrocode[i]),
    name = as.character(releasepoint$name[i]),
    fstatus = as.character(releasepoint$fstatus[i]),
    dh_link_facility_mps =  facility.hydroid[facility.dataframe$hydrocode%in%releasepoint$dh_link_facility_mps[i]], #dependent on facility import
    dh_geofield = as.character(releasepoint$dh_geofield[i]),
    stringsAsFactors=FALSE
  ) 
  release.dataframe_i <- postFeature(release_inputs, site, feature)
  print(release.dataframe_i)
  release.dataframe_i <- getFeature(release_inputs, token, site, feature)
  release.dataframe<-rbind(release.dataframe,release.dataframe_i)
  
  
  release.hydroid_i <- as.character(release.dataframe_i$hydroid)
  release.hydroid<-c(release.hydroid,release.hydroid_i)
  
  print(paste("Processing Hydrocode ",i," of ", length(releasepoint$hydrocode)))
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE OUTFALL DH FEATURE
  ############################################################################################   
  #2,895 outfalls
  outfall.hydroid<-character()
  outfall.dataframe<-data.frame()
  
  for (i in 1:length(outfalls$hydrocode)){
  outfall_inputs <- list(
    bundle = as.character(outfalls$bundle[i]),
    ftype = as.character(outfalls$ftype[i]),
    hydrocode = as.character(outfalls$hydrocode[i]),
    name = as.character(outfalls$name[i]),
    fstatus = as.character(outfalls$fstatus[i]),
    dh_link_facility_mps =  facility.hydroid[facility.dataframe$hydrocode%in%outfalls$dh_link_facility_mps[i]], #dependent on facility import
    dh_geofield = as.character(outfalls$dh_geofield[i]),
    stringsAsFactors=FALSE
  ) 
  outfall.dataframe_i <- postFeature(outfall_inputs, site, feature)
  print(outfall.dataframe_i)
  outfall.dataframe_i <- getFeature(outfall_inputs, token, site, feature)
  outfall.dataframe<-rbind(outfall.dataframe,outfall.dataframe_i)
  
  outfall.hydroid_i <- as.character(outfall.dataframe$hydroid)
  outfall.hydroid<-c(outfall.hydroid,outfall.hydroid_i)
  
  print(paste("Processing Hydrocode ",i," of ", length(outfalls$hydrocode)))
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE CONVEYANCE DH FEATURE
  ############################################################################################  
  #2,895 conveyance features
  
  # Format conveyance geom from release and outfall geoms 
  
  release.geofield <- substring(release.dataframe$dh_geofield, 8)
  release.geofield <-substr(release.geofield, 1, nchar(release.geofield)-1) 
  
  outfall.geofield <- substring(outfall.dataframe$dh_geofield, 8)
  outfall.geofield <-substr(outfall.geofield, 1, nchar(outfall.geofield)-1) 
  conveyance.geofield <- paste('LINESTRING (',release.geofield,', ',outfall.geofield,')',sep="")
  
  conveyance.hydroid<-character()
  conveyance.dataframe<-data.frame()
  
  for (i in 1:length(conveyance$hydrocode)){
  conveyance_inputs <- list(
    bundle = as.character(conveyance$bundle[i]),
    ftype = as.character(conveyance$ftype[i]),
    hydrocode = as.character(conveyance$hydrocode[i]),
    name = as.character(conveyance$name[i]),
    fstatus = as.character(conveyance$fstatus[i]),
    field_dh_from_entity =  release.hydroid[release.dataframe$hydrocode%in%conveyance$field_dh_from_entity[i]], 
    field_dh_to_entity =  outfall.hydroid_i[outfall.dataframe$hydrocode%in%conveyance$field_dh_to_entity[i]],
    dh_geofield = conveyance.geofield[i],
    stringsAsFactors=FALSE
  ) 
  conveyance.dataframe_i <- postFeature(conveyance_inputs, site, feature)
  print(conveyance.dataframe_i)
  conveyance.dataframe_i <- getFeature(conveyance_inputs, token, site, feature)
  conveyance.dataframe<- rbind(conveyance.dataframe, conveyance.dataframe_i)
  
  conveyance.hydroid_i <- as.character(conveyance.dataframe$hydroid)
  conveyance.hydroid<-c(conveyance.hydroid,conveyance.hydroid_i)
  
  print(paste("Processing Hydrocode ",i," of ", length(conveyance$hydrocode)))
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE TIMESERIES
  ############################################################################################  
  #44,386 timeseries entries: processing speed is about 1,000 entries every 5-6 minutes
  timeseries.tid<-character()
  timeseries.dataframe<-data.frame()
  
  for (i in 1:length(timeseries$hydrocode)){
  ts_inputs<-list(
    featureid = outfall.hydroid[outfall.dataframe$hydrocode%in%timeseries$hydrocode[i]],
    varkey = as.character(timeseries$varkey[i]),
    entity_type = 'dh_feature',
    tsvalue = timeseries$tsvalue[i],
    tscode = timeseries$tscode[i],
    tstime = format(as.POSIXlt(timeseries$tstime[i]),"%s"),
    tsendtime = format(as.POSIXlt(timeseries$tsendtime[i]),"%s")
  )
  timeseries.dataframe_i <- postTimeseries(ts_inputs,site,ts)
  print(timeseries.dataframe_i)
  timeseries.dataframe_i <- getTimeseries(ts_inputs, site, ts)
  timeseries.dataframe<-rbind(timeseries.dataframe,timeseries.dataframe_i)
  
  timeseries.tid_i <- as.character(timeseries.dataframe$tid)
  timeseries.tid<-c(timeseries.tid,timeseries.tid_i) #put into larger dataframe
  
  print(paste("Processing Hydrocode ",i," of ", length(timeseries$hydrocode)))
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE FLAGGING PROPERTIES OF TIMESERIES
  ############################################################################################   
  
  ############################################################################################ 
  # ECHO Adminstered Flag 
  echo_flag<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/echo_flag.txt",sep="\t",header=T)
  
  for (i in 1:length(echo_flag$hydrocode)){
  flag_inputs <-list(
    featureid = timeseries.tid[timeseries.dataframe$%in%],
    varkey = 'echo_flag',
    entity_type = 'dh_timeseries',
    propname = 'echo_flag',
    proptext= echo_flag$proptext[i]
    propcode = echo_flag$propcode[i]
  )
  flag.dataframe_i <- postProperty(flag_inputs, fxn_locations, site, prop)
  print(flag.dataframe_i)#print import status
  flag.dataframe_i <- getProperty(flag_inputs, site, prop)
  flag.dataframe<-rbind(flag.dataframe,flag.dataframe_i)
  
  flag.pid <- as.character(flag.dataframe$pid)
  flag.pid_echo<-c(flag.pid_echo,flag.pid)
  
  print(paste("Processing Hydrocode ",i," of ", length(echo_flag$hydrocode)))
  }
  
  ############################################################################################ 
  # Entries that exceed the faiclity's design flow (dmr_flag_desflow)
  
  dmr_flag_desflow<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/dmr_flag_desflow.txt",sep="\t",header=T)
  
  for (i in 1:length(dmr_flag_desflow$hydrocode)){
    flag_inputs <-list(
      featureid = timeseries.tid[timeseries.dataframe$%in%],
      varkey = 'dmr_flag_desflow',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_desflow',
      proptext= dmr_flag_desflow$proptext[i]
      propcode = dmr_flag_desflow$propcode[i]
    )
    flag.dataframe_i <- postProperty(flag_inputs, fxn_locations, site, prop)
    print(flag.dataframe_i)#print import status
    flag.dataframe_i <- getProperty(flag_inputs, site, prop)
    flag.dataframe<-rbind(flag.dataframe,flag.dataframe_i)
    
    flag.pid <- as.character(flag.dataframe$pid)
    flag.pid_MEgreaterDF<-c(flag.pid_echo,flag.pid)
    
    print(paste("Processing Hydrocode ",i," of ", length(dmr_flag_desflow$hydrocode)))
  }
  
  ############################################################################################ 
  # DMR entries that exceed 100 times the median outfall discharge
  dmr_flag_units_100<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/dmr_flag_units_100.txt",sep="\t",header=T)
  
  for (i in 1:length(dmr_flag_units_100$hydrocode)){
    flag_inputs <-list(
      featureid = timeseries.tid[timeseries.dataframe$%in%],
      varkey = 'dmr_flag_units_100',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_units_100',
      proptext= dmr_flag_units_100$proptext[i]
      propcode = dmr_flag_units_100$propcode[i]
    )
    flag.dataframe_i <- postProperty(flag_inputs, fxn_locations, site, prop)
    print(flag.dataframe_i)#print import status
    flag.dataframe_i <- getProperty(flag_inputs, site, prop)
    flag.dataframe<-rbind(flag.dataframe,flag.dataframe_i)
    
    flag.pid <- as.character(flag.dataframe$pid)
    flag.pid_units100<-c(flag.pid_echo,flag.pid)
    
    print(paste("Processing Hydrocode ",i," of ", length(dmr_flag_units_100$hydrocode)))
  }
  
  
  ############################################################################################ 
  # DMR entries that exceed 1,000,000 times the median outfall discharge
  dmr_flag_units_1000000<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/dmr_flag_units_1000000.txt",sep="\t",header=T)
  
  for (i in 1:length(dmr_flag_units_1000000$hydrocode)){
    flag_inputs <-list(
      featureid = timeseries.tid[timeseries.dataframe$%in%],
      varkey = 'dmr_flag_units_1000000',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_units_1000000',
      proptext= dmr_flag_units_1000000$proptext[i]
      propcode = dmr_flag_units_1000000$propcode[i]
    )
    flag.dataframe_i <- postProperty(flag_inputs, fxn_locations, site, prop)
    print(flag.dataframe_i)#print import status
    flag.dataframe_i <- getProperty(flag_inputs, site, prop)
    flag.dataframe<-rbind(flag.dataframe,flag.dataframe_i)
    
    flag.pid <- as.character(flag.dataframe$pid)
    flag.pid_units1000000<-c(flag.pid_echo,flag.pid)
    
    print(paste("Processing Hydrocode ",i," of ", length(dmr_flag_units_1000000$hydrocode)))
  }
  
  