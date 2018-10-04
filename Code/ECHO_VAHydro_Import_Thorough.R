##################################################################################################################################
###########################################VAHydro to ECHO Import#################################################################

#This code serves to extract data concerning NPDES permitted facilities in Virginia from the EPA's ECHO REST Services 
#and import it into the VDEQ's VAHydro system. 

#-------Elements that are imported (hierarchy required)---------#

#-1: Agency 
#-2: Permits 
#-3: Facilities 
#-4: Facility Metadata/Properties 
      #-Waterbody Name (GNIS)
      #-Combined Sewer System Flag (CWPCsoFlag)
      #-Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)
      #-Impairment Class or Category of the Waterbody (impair_cause)
      #-Date of most recent inspection of the facility (last_inspect)
      #-Unique ID for Waterbody (reachcode_rad)
      #-Facility Design Flow in MGD (design_flow)
#-5: Release Points
#-6: Outfalls
#-7: Conveyance Features
#-8: Outfall DMR Timeseries 
#-9: Timeseries Flags 
      #-ECHO Administered (echo_flag)
      #-Entries that exceed the faiclity's design flow (dmr_flag_desflow)
      #-Entries that exceed 100 times the median outfall discharge (dmr_flag_units_100)
      #-Entries that exceed 1,000,000 times the median outfall discharge (dmr_flag_units_1000000)

#-------Importing Structure------#
#This script updates all features and will take a sufficient amount of time to complete. 
#This should be run a couple times a year to ensure all features are updated. 

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


#-------Retrieve list of Discharging Facilities in ECHO---------------#
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
colnames(ECHO_Facilities)[2]<-c("Facility.ID")

#---------Retrieve List of Individual Permits located in VPDES Database---------#
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
ECHO_Facilities$CWPPermitTypeDesc<-ifelse(ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit","National Pollutant Discharge Elimination System (NPDES) Permit",ECHO_Facilities$CWPPermitTypeDesc)

#----------Seperate Design Flow as a Facility Property---------------#
design_flow<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='design_flow', propname='design_flow', 
                        propvalue=ECHO_Facilities$DesignFlow_mgd, propcode=ifelse(ECHO_Facilities$DesignFlow_mgd==0,"fac_flag_zerodesflow",NA))

#----------Retrieve coordinates of outfalls-----------------#
#Use Aggregated Flows generated from ECHOInterface Script and list of outfalls for creating release and conveyance points.
temp<-tempfile(fileext = ".zip")
#Locations and attribute data about active outfalls in the State
download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip", destfile = temp)
unzip(temp, exdir = Inputpath)
#Explore what is in VPDES_Geodatabase.gdb
ogrListLayers(paste0(Inputpath,"/VPDES_Geodatabase.gdb")) #Two layers: VPDES Outfalls and OpenFileGDB
VPDES_Outfalls<-as.data.frame(readOGR(paste0(Inputpath,"/VPDES_Geodatabase.gdb"),layer="VPDES_OUTFALLS"))
names(VPDES_Outfalls)[names(VPDES_Outfalls)=="OUTFALL_ID"]<-'OutfallID'
names(VPDES_Outfalls)[names(VPDES_Outfalls)=="VAP_PMT_NO"]<-'Facility.ID'
names(ECHO_Facilities)[names(ECHO_Facilities)=="SourceID"]<-"Facility.ID"#Need to rename to give a central columnn name for future joins

VPDES_Coordinates<-VPDES_Outfalls[,c(15,16)]
VPDES_Coordinates <- proj4::project(VPDES_Coordinates, proj="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", inverse=TRUE)

#Replace coordinates in VPDES_Outfalls data frame
VPDES_Outfalls$Longitude<-VPDES_Coordinates$x
VPDES_Outfalls$Latitude<-VPDES_Coordinates$y

rm(Req_URL,URL_Download,URL_Parse,QID,GET_Facilities,VPDES_IP,temp)

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

#This loop goes through each CWA regulated facility one by one to extract reported discharges 
#from each unique outfall. In the end, there will be ECHO_Facilities table with timeseries data for each
#outfall located in VA. 
for (i in 1:length(ECHO_Facilities$Facility.ID)){
  Facility.ID<-ECHO_Facilities$Facility.ID[i]
  print(paste("Processing Facility ID: ", Facility.ID, "(",i," of ",length(ECHO_Facilities$Facility.ID),")", sep=""))
  DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",Facility.ID,"&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe
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
      VPDESID<-c(VPDESID,paste0(Facility.ID,rep(outfall,length((tsvalue_i)))))
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
timeseries$facilityID<-gsub("echo_","", as.character(timeseries$hydrocode))
timeseries$facilityID<-substr(timeseries$facilityID,1,nchar(timeseries$facilityID)-3)

save.image(file="G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/timeseries_2010_present.RData")



#timeseries<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/ECHO_VAHydro Imports/timeseries.txt",sep='\t',header=T)
#timeseriesNA<-subset(timeseries,subset=is.na(timeseries$tsvalue)) #599 NA Values
##################################################################################################################################
#############################################1: Import Permits####################################################################
#Purpose: Extract all NPDES Permits in VA to import into VAHydro. This includes active and inactive. 

#dH Adminreg Feature Mapping
#bundle, admincode, description, name, ftype, fstatus, startdate, enddate, permit_id, dh_link_admin_reg_issuer

#Under the CWA, all facilities discharging pollutant from a point source must have a NPDES Permit
#Status is considered Effective, Admin Continued, Expired, Not Needed, Pending, Retured, or Terminated

#important to note that Facility.ID in matrix a is equal to admincode. It becomes hydrocode when word 'echo_' is added before
#permit ID not to be confused with VPDESID which is unique for each outfall

adminreg<-data.frame(bundle='permit', admincode=ECHO_Facilities$Facility.ID, description=ECHO_Facilities$CWPPermitTypeDesc,name=ECHO_Facilities$CWPName)
adminreg<-subset(adminreg, subset=adminreg$description=="National Pollutant Discharge Elimination System (NPDES) Permit"|
         adminreg$description=="General Permit Covered Facility")
#These permits are the ones that report flow 

adminreg$ftype<-ifelse(adminreg$description=="General Permit Covered Facility","npdes_gp","npdes_ip")

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

adminreg$startdate<-ECHO_Facilities$CWPEffectiveDate[match(adminreg$admincode,ECHO_Facilities$Facility.ID)]

#end date is permit expiration date rather than limit_end_date
adminreg$enddate<-ECHO_Facilities$CWPExpirationDate[match(adminreg$admincode,ECHO_Facilities$Facility.ID)]
adminreg$permit_id<-ECHO_Facilities$Facility.ID[match(adminreg$admincode,ECHO_Facilities$Facility.ID)]
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
facilities$hydrocode<-paste0("echo_",ECHO_Facilities$Facility.ID)
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
           length(grep('COURTHOUSE',ECHO_Facilities$CWPName[i]))>0|
           length(grep('GROCERY',ECHO_Facilities$CWPName[i]))>0|
           length(grep("CITY OF",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-'municipal'
  } else if(length(grep("FARM",ECHO_Facilities$CWPName[i]))>0|
            length(grep("FISH CULTURAL",ECHO_Facilities$CWPName[i]))>0){
    facilities$ftype[i]<-"agriculture"
  }else if(length(grep("CORPORATION",ECHO_Facilities$CWPName[i]))>0|
            length(grep("INC.",ECHO_Facilities$CWPName[i]))>0|
            length(grep("INC",ECHO_Facilities$CWPName[i]))>0|
            length(grep("INCORPORATED",ECHO_Facilities$CWPName[i]))>0|
            length(grep('CORP.',ECHO_Facilities$CWPName[i]))>0|
            length(grep('LLC',ECHO_Facilities$CWPName[i]))>0|
            length(grep('L.L.C.',ECHO_Facilities$CWPName[i]))>0|
            length(grep('AIRPORT',ECHO_Facilities$CWPName[i]))>0|
            length(grep('LTD',ECHO_Facilities$CWPName[i]))>0|
            length(grep('CO',ECHO_Facilities$CWPName[i]))>0|
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
  if (ECHO_Facilities$Facility.ID[i]%in%timeseries$facilityID){ #if it is reporting flow for ECHO, it is most likely active. Also check status of permit.
    facilities$fstatus[i]<-'active'
  }
  if(!is.na(ECHO_Facilities$FacLat[i]) & !is.na(ECHO_Facilities$FacLong[i])){
    facilities$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==ECHO_Facilities$Facility.ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==ECHO_Facilities$Facility.ID[i]],')')
  } else {
    lat<-VPDES_Outfalls$Latitude[VPDES_Outfalls$Facility.ID==VPDES_Outfalls$Facility.ID[i]]
    long<-VPDES_Outfalls$Longitude[VPDES_Outfalls$Facility.ID==VPDES_Outfalls$Facility.ID[i]]
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
facilities$dh_link_admin_location<-ECHO_Facilities$Facility.ID

#write.table(facilities,paste0(Outputpath,"/facilities.txt"),sep="\t",row.names = F)

##################################################################################################################################
###########################################3 Release Point Generation#############################################################

#Generation of the release point imports. In essence, this portion just formats various data from both the facility
#and the outfall list (VPDES_Outfalls) to create the release point attributes and geometry.

#---------Narrow Outfalls by whether their facility.ID is in the ECHO facilities dataframe----------#
VPDES_Outfalls$OutfallID<-as.character(VPDES_Outfalls$OutfallID)
VPDES_Outfalls$Facility.ID<-as.character(VPDES_Outfalls$Facility.ID)
VPDES_Outfalls<-VPDES_Outfalls[VPDES_Outfalls$Facility.ID%in%ECHO_Facilities$Facility.ID,]

releasepoint<-data.frame(bundle=rep('transfer',length(VPDES_Outfalls$OutfallID)),
                         name=paste0('TO ',VPDES_Outfalls$OutfallID),
                         ftype=rep('release',length(VPDES_Outfalls$OutfallID)),
                         hydrocode=paste0('vahydro_',VPDES_Outfalls$OutfallID),
                         fstatus=ifelse(VPDES_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                         dh_link_facility_mps=paste0('echo_',VPDES_Outfalls$Facility.ID))

for (i in 1:length(releasepoint$bundle)){
  print(paste("Processing Release Point ",i," of ", length(releasepoint$hydrocode)))
  if(!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]])){
    releasepoint$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]],')')
  } else {
    lat<-VPDES_Outfalls$Latitude[VPDES_Outfalls$Facility.ID==VPDES_Outfalls$Facility.ID[i]]
    long<-VPDES_Outfalls$Longitude[VPDES_Outfalls$Facility.ID==VPDES_Outfalls$Facility.ID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        releasepoint$dh_geofield[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        releasepoint$dh_geofield[i]<-'NULL'
      }
    }
  }
}
#write.table(releasepoint,paste0(Outputpath,"/releasepoint.txt"),sep="\t",row.names = F)

########################################################################################################################
#Conveyance Generation

#Generates the conveyance import using the outfall list in 'VPDES_Outfalls'
conveyance<-data.frame(bundle=rep('conveyance',length(VPDES_Outfalls$OutfallID)),
                       name=paste0(VPDES_Outfalls$Facility.ID,' TO ',VPDES_Outfalls$OutfallID),
                       ftype="water_transfer",
                       hydrocode=paste0('vahydro_',VPDES_Outfalls$Facility.ID,'_',VPDES_Outfalls$OutfallID),
                       fstatus=ifelse(VPDES_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                       field_dh_from_entity=paste0('vahydro_',VPDES_Outfalls$OutfallID),
                       field_dh_to_entity=paste0('echo_',VPDES_Outfalls$OutfallID))

#write.table(conveyance,paste0(Outputpath,"/conveyance.txt"),sep="\t",row.names = F)

#Outfall_Outfalls Generation
#Reformats 'VPDES_Outfalls' using available VPDES or ECHO geometry data and ECHO attributes
outfalls<-data.frame(bundle=rep('transfer',length(VPDES_Outfalls$OutfallID)),
                     name=paste0('FROM ',VPDES_Outfalls$Facility.ID),
                     ftype='outfall',
                     hydrocode=paste0('echo_',VPDES_Outfalls$OutfallID),
                     fstatus=ifelse(VPDES_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                     dh_link_facility_mps=paste0('echo_',VPDES_Outfalls$Facility.ID))

for (i in 1:length(outfalls$bundle)){
  print(paste("Processing Outfall ",i," of ", length(outfalls$hydrocode)))
  if(!is.na(VPDES_Outfalls$Latitude[VPDES_Outfalls$OutfallID==VPDES_Outfalls$OutfallID[i]]) & !is.na(VPDES_Outfalls$Longitude[VPDES_Outfalls$OutfallID==VPDES_Outfalls$OutfallID[i]])){
    outfalls$dh_geofield[i]<-paste0('POINT (',VPDES_Outfalls$Longitude[VPDES_Outfalls$OutfallID==VPDES_Outfalls$OutfallID[i]],' ',VPDES_Outfalls$Latitude[VPDES_Outfalls$OutfallID==VPDES_Outfalls$OutfallID[i]],')')  
    } else if (!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]])) {
      outfalls$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==VPDES_Outfalls$Facility.ID[i]])
    } else {
      outfalls$dh_geofield[i]<-'NULL'
  }
}
#write.table(outfalls,paste0(Outputpath,"/outfalls.txt"),sep="\t",row.names = F)


#################################################################################
#3 Import Facility Metadata

#Facility dH Property Mapping

#hydrocode, varkey, propname, propvalue, proptext, propcode, startdate, enddate

last_inspect<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='last_inspect', propname='last_inspect', propvalue='',proptext='',propcode='',startdate=ECHO_Facilities$CWPDateLastInspection,enddate='')
#write.table(last_inspect,paste0(Outputpath,"/last_inspect.txt"),sep="\t",row.names = F)

css<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='css', propname='css', propvalue='',proptext='',propcode=ECHO_Facilities$CWPCsoFlag, startdate='',enddate='')
#write.table(css,paste0(Outputpath,"/css.txt"),sep="\t",row.names = F)

cwp_cso_outfalls<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='cwp_cso_outfalls', propname='cwp_cso_outfalls', propvalue=ECHO_Facilities$CWPCsoOutfalls,proptext='',propcode='', startdate='',enddate='')
cwp_cso_outfalls$propvalue[is.na(cwp_cso_outfalls$propvalue)]<-""
#write.table(cwp_cso_outfalls,paste0(Outputpath,"/cwp_cso_outfalls.txt"),sep="\t",row.names = F)

wb_gnis_name<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='wb_gnis_name', propname='wb_gnis_name', propvalue='', proptext='',propcode=ECHO_Facilities$RadGnisName, startdate='',enddate='')
#write.table(wb_gnis_name,paste0(Outputpath,"/wb_gnis_name.txt"),sep="\t",row.names = F)

reachcode_rad<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='reachcode_rad', propname='reachcode_rad', propvalue='', proptext='',propcode=ECHO_Facilities$RadReachcode, startdate='',enddate='')
reachcode_rad$propcode[is.na(reachcode_rad$propcode)]<-""
#write.table(reachcode_rad,paste0(Outputpath,"/reachcode_rad.txt"),sep="\t",row.names = F)

impair_cause<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='impair_cause', propname='impair_cause', propvalue='', proptext=ECHO_Facilities$AttainsCauseGroups,propcode='', startdate='',enddate='')
#write.table(impair_cause,paste0(Outputpath,"/impair_cause.txt"),sep="\t",row.names = F)


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
  #1943 Permits 

  permit.adminid<-character()#to store all iterations in one big dataframe 
  permit.dataframe<-data.frame()

  permit_inputs <- data.frame(
    bundle = as.character(adminreg$bundle),
    ftype = as.character(adminreg$ftype),
    admincode = as.character(adminreg$admincode),
    name = as.character(adminreg$name),
    fstatus = as.character(adminreg$fstatus),
    description = as.character(adminreg$description),
    startdate = format(as.POSIXlt(adminreg$startdate),"%s"), 
    enddate = format(as.POSIXlt(adminreg$enddate),"%s"),
    permit_id = as.character(adminreg$admincode),
    dh_link_admin_reg_issuer = agency.adminid, #actual id and not "epa"
    stringsAsFactors = FALSE
  ) 
  
  for (i in 1:length(permit_inputs$admincode)){
  print(paste("Processing Permit ",i," of ", length(permit_inputs$admincode)))
  permit.dataframe_i <- postAdminregFeature(permit_inputs[i,], site, adminreg_feature) #updates feature
  print(permit.dataframe_i) #print status of updating: error 403 means administration block
  permit.dataframe_i <- getAdminregFeature(permit_inputs[i,], site, adminreg_feature)
  permit.dataframe<-rbind(permit.dataframe,permit.dataframe_i)
  }
  
  #unique adminid's for each permit. Used to link facilities to permits
  permit.adminid<-data.frame(admincode=permit.dataframe$admincode,adminid=permit.dataframe$adminid)
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE FACILITY DH FEATURE
  ############################################################################################  
  
  facilities<-facilities[facilities$dh_link_admin_location%in%permit.adminid$admincode,]
  
  facility.dataframe <- data.frame()
  facility.hydroid <- character()
  
  facility_inputs <- data.frame(
    bundle = as.character(facilities$bundle),
    ftype = as.character(facilities$ftype),
    hydrocode = as.character(facilities$hydrocode),
    name = as.character(facilities$name),
    fstatus = as.character(facilities$fstatus),
    address1 = as.character(facilities$address1),
    city = as.character(facilities$city),
    dh_link_admin_location =  permit.adminid$adminid[match(permit.adminid$admincode,facilities$dh_link_admin_location)], #%in% operator looks for matches in left operand and returns permit.adminid if there is one
    dh_geofield = as.character(facilities$dh_geofield),
    stringsAsFactors=FALSE
  ) 
  
  for (i in 1:length(facility_inputs$hydrocode)){
  print(paste("Processing Facility ",i," of ", length(facilities$dh_link_admin_location)))
  facility.dataframe_i <- postFeature(facility_inputs[i,], site, feature)
  print(facility.dataframe_i)
  facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature) #need token now for access
  facility.dataframe<-rbind(facility.dataframe,facility.dataframe_i)
  }
  facility.hydroid<-data.frame(hydrocode=facility.dataframe$hydrocode,hydroid=facility.dataframe$hydroid)
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE FACILITY METADATA PROPERTIES
  # Make sure hydrocodes are in same order as in the facility data frame as well
  ############################################################################################   
  # Waterbody Name (GNIS): Name of waterbody from the Geographic Names Information System database where the facility is permitted to discharge directly
  
  property.pid_wbgnis<-character()
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.hydroid$hydrocode,wb_gnis_name$hydrocode)],
    varkey = as.character(wb_gnis_name$varkey),
    entity_type = rep(paste0('dh_feature'),length(wb_gnis_name$hydrocode)),
    propname = as.character(wb_gnis_name$propname),
    propvalue = rep(NA,length(wb_gnis_name$hydrocode)),
    proptext = rep(NA,length(wb_gnis_name$hydrocode)),
    propcode = as.character(wb_gnis_name$propcode),
    startdate = rep(NA,length(wb_gnis_name$hydrocode)),
    enddate = rep(NA,length(wb_gnis_name$hydrocode))
  )
  
  for (i in 1:length(wb_gnis_name$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(wb_gnis_name$hydrocode)))
  property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
  print(property.dataframe_i)
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script

  }
  property.pid_wbgnis<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  ############################################################################################ 
  #Combined Sewer System (CWPCsoFlag): The discharge from a combined sewer system at a point prior to a treatment plant
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,css$hydrocode)],
    varkey = as.character(css$varkey),
    entity_type = 'dh_feature',
    propname = as.character(css$propname),
    propvalue = rep(NA,length(css$hydrocode)),
    proptext = rep(NA,length(css$hydrocode)),
    propcode = as.character(css$propcode),
    startdate = rep(NA,length(css$hydrocode)),
    enddate = rep(NA,length(css$hydrocode))
  )
  
  for (i in 1:length(css$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(css$hydrocode)))
    property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
  }
  
  property.pid_css<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  
  ############################################################################################ 
  #Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,cwp_cso_outfalls$hydrocode)],
    varkey = as.character(cwp_cso_outfalls$varkey),
    entity_type = rep(paste0('dh_feature'),length(cwp_cso_outfalls$hydrocode)),
    propname = as.character(cwp_cso_outfalls$propname),
    propvalue = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    proptext = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    propcode = as.character(cwp_cso_outfalls$propcode),
    startdate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    enddate = rep(NA,length(cwp_cso_outfalls$hydrocode))
  )
  
  for (i in 1:length(cwp_cso_outfalls$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(cwp_cso_outfalls$hydrocode)))
    property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
  }
  
  property.pid_cwp_cso_outfalls<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  
  
  ############################################################################################ 
  #Impairment Class or Category of the Waterbody (impair_cause)
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,impair_cause$hydrocode)],
    varkey = as.character(impair_cause$varkey),
    entity_type = rep(paste0('dh_feature'),length(impair_cause$hydrocode)),
    propname = as.character(impair_cause$propname),
    propvalue = rep(NA,length(impair_cause$hydrocode)),
    proptext = rep(NA,length(impair_cause$hydrocode)),
    propcode = as.character(impair_cause$propcode),
    startdate = rep(NA,length(impair_cause$hydrocode)),
    enddate = rep(NA,length(impair_cause$hydrocode))
  )
  
  for (i in 1:length(impair_cause$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(impair_cause$hydrocode)))
    property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
  }
  
  property.pid_impair<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  
  
  ############################################################################################ 
  #Date of most recent inspection of the facility (last_inspect)
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,last_inspect$hydrocode)],
    varkey = as.character(last_inspect$varkey),
    entity_type = rep(paste0('dh_feature'),length(last_inspect$hydrocode)),
    propname = as.character(last_inspect$propname),
    propvalue = rep(NA,length(last_inspect$hydrocode)),
    proptext = rep(NA,length(last_inspect$hydrocode)),
    propcode = as.character(last_inspect$propcode),
    startdate = rep(NA,length(last_inspect$hydrocode)),
    enddate = rep(NA,length(last_inspect$hydrocode))
  )
  
  for (i in 1:length(last_inspect$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(last_inspect$hydrocode)))
    property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
  }
  
  property.pid_last_inspect<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  
  ############################################################################################ 
  #Unique ID for Waterbody (reachcode_rad)
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,reachcode_rad$hydrocode)],
    varkey = as.character(reachcode_rad$varkey),
    entity_type = rep(paste0('dh_feature'),length(reachcode_rad$hydrocode)),
    propname = as.character(reachcode_rad$propname),
    propvalue = rep(NA,length(reachcode_rad$hydrocode)),
    proptext = rep(NA,length(reachcode_rad$hydrocode)),
    propcode = as.character(reachcode_rad$propcode),
    startdate = rep(NA,length(reachcode_rad$hydrocode)),
    enddate = rep(NA,length(reachcode_rad$hydrocode))
  )
  
  for (i in 1:length(reachcode_rad$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(reachcode_rad$hydrocode)))
    property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
  }
  
  property.pid_reachcode_rad<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  
  ############################################################################################ 
  # Facility Design Flow in MGD (design_flow)
  
  prop_inputs <-data.frame(
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,design_flow$hydrocode)],
    varkey = as.character(design_flow$varkey),
    entity_type = rep(paste0('dh_feature'),length(design_flow$hydrocode)),
    propname = as.character(design_flow$propname),
    propvalue = rep(NA,length(design_flow$hydrocode)),
    proptext = rep(NA,length(design_flow$hydrocode)),
    propcode = as.character(design_flow$propcode),
    startdate = rep(NA,length(design_flow$hydrocode)),
    enddate = rep(NA,length(design_flow$hydrocode))
  )
  
  for (i in 1:length(design_flow$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(design_flow$hydrocode)))
    property.dataframe_i <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_i)
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    property.dataframe<-rbind(property.dataframe,property.dataframe_i)
    
  }
  
  property.pid_design_flow<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE RELEASE DH FEATURE
  ############################################################################################  
  #2,895 release points
  release.hydroid<-character()
  release.dataframe<-data.frame()
  
  release_inputs <- data.frame(
    bundle = as.character(releasepoint$bundle),
    ftype = as.character(releasepoint$ftype),
    hydrocode = as.character(releasepoint$hydrocode),
    name = as.character(releasepoint$name),
    fstatus = as.character(releasepoint$fstatus),
    dh_link_facility_mps =  facility.hydroid$hydroid[match(releasepoint$dh_link_facility_mps,facility.hydroid$hydrocode)], #dependent on facility import
    dh_geofield = as.character(releasepoint$dh_geofield),
    stringsAsFactors=FALSE
  ) 
  
  #Looks for matching hydrocode in facility list. If it's not there, a release will not be made. So let's get rid of the NA values
  
  
  for (i in 1:length(releasepoint$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(releasepoint$hydrocode)))
  release.dataframe_i <- postFeature(release_inputs[i,], site, feature)
  print(release.dataframe_i)
  release.dataframe_i <- getFeature(release_inputs[i,], token, site, feature)
  release.dataframe<-rbind(release.dataframe,release.dataframe_i)
  }
  
  release.hydroid<-data.frame(hydrocode=release.dataframe$hydrocode, hydroid=release.dataframe$hydroid)
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE OUTFALL DH FEATURE
  ############################################################################################   
  #2,895 outfalls

  outfall.dataframe<-data.frame()
  
  outfall_inputs <- data.frame(
    bundle = as.character(outfalls$bundle),
    ftype = as.character(outfalls$ftype),
    hydrocode = as.character(outfalls$hydrocode),
    name = as.character(outfalls$name),
    fstatus = as.character(outfalls$fstatus),
    dh_link_facility_mps =  as.character(facility.hydroid$hydroid[match(outfalls$dh_link_facility_mps,facility.hydroid$hydrocode)]), #dependent on facility import
    dh_geofield = as.character(outfalls$dh_geofield),
    stringsAsFactors=FALSE
  ) 
  
  for (i in 1:length(outfalls$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(outfalls$hydrocode)))
  outfall.dataframe_i <- postFeature(outfall_inputs[i,], site, feature)
  print(outfall.dataframe_i)
  outfall.dataframe_i <- getFeature(outfall_inputs[i,], token, site, feature)
  outfall.dataframe<-rbind(outfall.dataframe,outfall.dataframe_i)
  
  }

  outfall.hydroid<-data.frame(hydrocode=outfall.dataframe$hydrocode, hydroid=outfall.dataframe$hydroid)
  
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
  
  conveyance.dataframe<-data.frame()
  
  conveyance_inputs <- data.frame(
    bundle = as.character(conveyance$bundle),
    ftype = as.character(conveyance$ftype),
    hydrocode = as.character(conveyance$hydrocode),
    name = as.character(conveyance$name),
    fstatus = as.character(conveyance$fstatus),
    field_dh_from_entity =  release.hydroid$hydroid[match(release.hydroid$hydrocode,conveyance$field_dh_from_entity)], 
    field_dh_to_entity =  outfall.hydroid$hydroid[match(outfall.hydroid$hydrocode,conveyance$field_dh_to_entity)],
    dh_geofield = conveyance.geofield,
    stringsAsFactors=FALSE
  ) 
  
  for (i in 1:length(conveyance_inputs$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(conveyance_inputs$hydrocode)))
  conveyance.dataframe_i <- postFeature(conveyance_inputs[i,], site, feature)
  print(conveyance.dataframe_i)
  conveyance.dataframe_i <- getFeature(conveyance_inputs[i,], token, site, feature)
  conveyance.dataframe<- rbind(conveyance.dataframe, conveyance.dataframe_i)
  }
  conveyance.hydroid<-data.frame(hydrocode=conveyance.dataframe$hydrocode,hydroid=conveyance.dataframe$hydroid)
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE TIMESERIES
  ############################################################################################  
  #44,386 timeseries entries: processing speed is about 1,000 entries every 5-6 minutes
  timeseries.tid<-character()
  timeseries.dataframe<-data.frame()
  
  ts_inputs<-data.frame(
    featureid = outfall.hydroid$hydroid[match(timeseries$hydrocode,outfall.hydroid$hydrocode)],
    varkey = as.character(timeseries$varkey),
    entity_type = rep(paste0('dh_feature'),length(timeseries$hydrocode)),
    tsvalue = timeseries$tsvalue,
    tscode = timeseries$tscode,
    tstime = format(as.POSIXlt(timeseries$tstime),"%s"),
    tsendtime = format(as.POSIXlt(timeseries$tsendtime),"%s"),
    dmr_flag_desflow=as.character(timeseries$dmr_flag_desflow),
    dmr_flag_units_100=as.character(timeseries$dmr_flag_units_100),
    dmr_flag_units_1000000=as.character(timeseries$dmr_flag_units_1000000),
    echo_flag=as.character(timeseries$violation),
    stringsAsFactors = F
  )
  
  ts_inputs$dmr_flag_desflow[is.na(ts_inputs$dmr_flag_desflow)]<-""
  ts_inputs$dmr_flag_units_100[is.na(ts_inputs$dmr_flag_units_100)]<-""
  ts_inputs$dmr_flag_units_1000000[is.na(ts_inputs$dmr_flag_units_1000000)]<-""
  ts_inputs$echo_flag[is.na(ts_inputs$echo_flag)]<-""
  
  ts_inputs<-ts_inputs[!is.na(ts_inputs$featureid),]
  
  for (i in 1:length(ts_inputs$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(timeseries$hydrocode)))
  timeseries.dataframe_i <- postTimeseries(ts_inputs[i,],site,ts)
  print(timeseries.dataframe_i)
  timeseries.dataframe_i <- getTimeseries(ts_inputs[i,], site, ts)
  timeseries.dataframe<-rbind(timeseries.dataframe,timeseries.dataframe_i)
  
  if(ts_inputs$dmr_flag_desflow[i]=="dmr_flag_desflow"){
    flag_inputs_dmr_flag_desflow_i <-data.frame(
      featureid_dmr_flag_desflow = as.character(timeseries.dataframe$tid),
      varkey = 'dmr_flag_desflow',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_desflow',
      proptext= 'Exceeds Facility Design Flow',
      propcode = "dmr_flag_desflow"
    )
    
    flag_inputs_dmr_flag_desflow<-rbind(flag_inputs_dmr_flag_desflow,flag_inputs_dmr_flag_desflow_i)
  }else{
    print("No dmr_flag_desflow flag")
  }
  
  
  
  if(ts_inputs$dmr_flag_units_100[i]=="dmr_flag_units_100"){
    flag_inputs_dmr_flag_units_100_i <-data.frame(
      featureid_dmr_flag_units_100 = as.character(timeseries.dataframe$tid),
      varkey = 'dmr_flag_units_100',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_units_100',
      proptext= "Exceeds 100*Median Outfall Discharge",
      propcode = "dmr_flag_units_100"
    )
    flag_inputs_dmr_flag_units_100<-rbind(flag_inputs_dmr_flag_units_100,flag_inputs_dmr_flag_units_100_i)
  }else{
    print("No dmr_flag_units_100 flag")
  }
  
  
  
  if(ts_inputs$dmr_flag_units_1000000[i]=="dmr_flag_units_1000000"){
    flag_inputs_1000000_i <-data.frame(
      featureid_dmr_flag_units_1000000 = as.character(timeseries.dataframe$tid),
      varkey = 'dmr_flag_units_1000000',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_units_1000000',
      proptext= "Exceeds 1000000*Median Outfall Discharge",
      propcode = "dmr_flag_units_1000000"
    )
    flag_inputs_1000000<-rbind(flag_inputs_1000000,flag_inputs_1000000_i)
  }else{
    print("No dmr_flag_units_1000000 flag")
  }
  
  
  
  if(!(ts_inputs$echo_flag[i]=="")){
    flag_inputs_echo_flag_i <-data.frame(
      featureid_echo_flag = as.character(timeseries.dataframe$tid),
      varkey = 'echo_flag',
      entity_type = 'dh_timeseries',
      propname = 'echo_flag',
      proptext= ts_inputs$echo_flag[i],
      propcode = ts_inputs$echo_flag[i]
    )
    flag_inputs_echo_flag<-rbind(flag_inputs_echo_flag,flag_inputs_echo_flag_i)
  }else{
    print("No echo flag")
  }
  
  }
  
  ############################################################################################
  # RETRIEVE/CREATE/UPDATE FLAGGING PROPERTIES OF TIMESERIES
  ############################################################################################   
  
  ############################################################################################ 
  # ECHO Adminstered Flag 
  
  
  #----Classify property text depending on property code------#
  for(i in 1:length(flag_inputs_echo_flag$featureid)){
    if(flag_inputs_echo_flag$propcode[i]=="E90"){
      flag_inputs_echo_flag$proptext[i]<-"Effluent Violation"
    }else if(flag_inputs_echo_flag$propcode[i]=="D90"){
      flag_inputs_echo_flag$proptext[i]<-"DMR Overdue, with a numeric limit"
    }else if(flag_inputs_echo_flag$propcode[i]=="D80"){
      flag_inputs_echo_flag$proptext[i]<-"DMR Overdue, monitoring only required"
    }
  }
  
  flag.dataframe_ii<-data.frame()
  flag.dataframe_echo<-data.frame()
  
  for (i in 1:length(flag_inputs_echo_flag$featureid)){
    print(paste("Processing Property ",i," of ", length(flag_inputs_echo_flag$featureid)))
    flag.dataframe_i <- getProperty(flag_inputs_echo_flag[i,], site, prop)
    
    if(flag.dataframe_i[1]==FALSE){
      flag.dataframe_ii <- postProperty(flag_inputs_echo_flag[i,], fxn_locations, site, prop)
      print(flag.dataframe_ii)
    }else{
      print("Flag Already Exists")
    }
    
    flag.dataframe_echo<-rbind(flag.dataframe_echo,flag.dataframe_i,flag.dataframe_ii)
  }
  
  ############################################################################################ 
  # Entries that exceed the faiclity's design flow (dmr_flag_desflow)
  
  flag.dataframe_ii<-data.frame()
  flag.dataframe_desflow<-data.frame()
  
  for (i in 1:length(flag_inputs_dmr_flag_desflow$featureid)){
    print(paste("Processing Property ",i," of ", length(flag_inputs_dmr_flag_desflow$featureid)))
    flag.dataframe_i <- getProperty(flag_inputs_dmr_flag_desflow[i,], site, prop)
    
    if(flag.dataframe_i[1]==FALSE){
      flag.dataframe_ii <- postProperty(flag_inputs_dmr_flag_desflow[i,], fxn_locations, site, prop)
      print(flag.dataframe_ii)
    }else{
      print("Flag Already Exists")
    }
    
    flag.dataframe_desflow<-rbind(flag.dataframe_desflow,flag.dataframe_i,flag.dataframe_ii)
  }
  
  ############################################################################################ 
  # DMR entries that exceed 100 times the median outfall discharge
  
  flag.dataframe_ii<-data.frame()
  flag.dataframe_units_100<-data.frame()
  
  for (i in 1:length(flag_inputs_dmr_flag_units_100$featureid)){
    print(paste("Processing Property ",i," of ", length(flag_inputs_dmr_flag_units_100$featureid)))
    flag.dataframe_i <- getProperty(flag_inputs_dmr_flag_units_100[i,], site, prop)
    
    if(flag.dataframe_i[1]==FALSE){
      flag.dataframe_ii <- postProperty(flag_inputs_dmr_flag_units_100[i,], fxn_locations, site, prop)
      print(flag.dataframe_ii)
    }else{
      print("Flag Already Exists")
    }
    
    flag.dataframe_units_100<-rbind(flag.dataframe_units_100,flag.dataframe_i,flag.dataframe_ii)
  }
  
  
  ############################################################################################ 
  # DMR entries that exceed 1,000,000 times the median outfall discharge
  
  flag.dataframe_ii<-data.frame()
  flag.dataframe_units_million<-data.frame()
  
  for (i in 1:length(flag_inputs_1000000$featureid)){
    print(paste("Processing Property ",i," of ", length(flag_inputs_1000000$featureid)))
    flag.dataframe_i <- getProperty(flag_inputs_1000000[i,], site, prop)
    
    if(flag.dataframe_i[1]==FALSE){
      flag.dataframe_ii <- postProperty(flag_inputs_1000000[i,], fxn_locations, site, prop)
      print(flag.dataframe_ii)
    }else{
      print("Flag Already Exists")
    }
    
    flag.dataframe_units_million<-rbind(flag.dataframe_units_million,flag.dataframe_i,flag.dataframe_ii)
  }
  
  
  
  