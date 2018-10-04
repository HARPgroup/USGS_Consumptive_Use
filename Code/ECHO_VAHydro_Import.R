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
      #-Entries that exceed the facility's design flow (dmr_flag_desflow)
      #-Entries that exceed 100 times the median outfall discharge (dmr_flag_units_100)
      #-Entries that exceed 1,000,000 times the median outfall discharge (dmr_flag_units_1000000)

#-------Importing Structure------#
#This script checks to see if the feature/property already exists in VAHydro.
#If so, it is not updated.
#This can be run frequently.

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
ECHO_Facilities$CWPPermitTypeDesc<-ifelse(ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit","National Pollutant Discharge Elimination System (NPDES) Permit",ECHO_Facilities$CWPPermitTypeDesc)


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
outfallID<-character()
nodi<-character()
violation<-character() #Code identifying if a Violation has occurred  (e.g., D80 = Required Monitoring DMR Value Non-Receipt, E90 = Effluent Violation, C20 = Schedule Event Achieved Late).
violation_severity<-numeric() #Severity of any alleged violation caused by the reported value: 5 = significant noncompliance; 3 = reportable noncompliance; 2 = effluent violation, i.e., discharge in excess of permitted limit; 1 = monitoring or reporting violation; 0 = no violation.

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
      violation_i<-character()
      violation_severity_i<-numeric()
      
      for(l in 1:length(outfall_DMR$perm_feature_nmbr)){ #extracts discharge quantity from each outfall by examining the statistical code associated with it. In this case, we want an average.
        if(!is.na(outfall_DMR$statistical_base_code[l]=="MK")){ #ideally, we want a monthly average, which is indicated by the code "MK"
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="MK"])[l] 
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="MK"][l] #character class
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="MK"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month"))#uses Lubridate package, date must be object of class POSIXlt, POSIXct, or Date
          varkey_i[l]<-"dmr_mon_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="MK"][l] 
          violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="MK"][l]
          violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="MK"][l]
          
        }else if(!is.na(outfall_DMR$statistical_base_code[l]=="DB")){ #if it is missing a monthly average, look at daily average in MGD
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="DB"])[l]  
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="DB"][l] #character class
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="DB"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) 
          varkey_i[l]<-"dmr_day_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="DB"][l] 
          violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="DB"][l]
          violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="DB"][l]
          
          
        }else if(!is.na(outfall_DMR$statistical_base_code[l]=="WA")){ #if it is also missing a daily average, look at weekly average in MGD
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="WA"])[l] 
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="WA"][l] 
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="WA"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) 
          varkey_i[l]<-"dmr_wk_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="WA"][l] 
          violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="WA"][l]
          violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="WA"][l]
          
        }else if(!is.na(outfall_DMR$statistical_base_code[l]=="AB")){ #if it is also missing this, look at annual average in MGD
          tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="AB"])[l]  
          tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="AB"][l] 
          tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="AB"])[l]
          tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month")) 
          varkey_i[l]<-"dmr_yr_mgd"
          nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="AB"][l] 
          violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="AB"][l]
          violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="AB"][l]
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
      violation<-c(violation,violation_i)
      violation_severity<-c(violation_severity,violation_severity_i)
      outfallID<-c(outfallID,paste0(Facility.ID,rep(outfall,length((tsvalue_i)))))
      hydrocode<-paste0('echo_',outfallID)
    }
  }else{ #if the DMR contains no data, set variables to NA
    hydrocode<-c(hydrocode,NA)
    outfallID<-c(outfallID,NA)
    varkey<-c(varkey,NA)
    tsvalue<-c(tsvalue,NA)
    tstime<-c(tstime,NA)
    tsendtime<-c(tsendtime,NA)
    tscode<-c(tscode,NA)
    nodi<-c(nodi,NA)
    violation<-c(violation,NA)
    violation_severity<-c(violation_severity,NA)
  }
}
timeseries<-data.frame(hydrocode=hydrocode,varkey=varkey,tsvalue=tsvalue,tstime=tstime,tsendtime=tsendtime,tscode=tscode,nodi=nodi,violation=violation,violation_severity=violation_severity)
timeseries<-timeseries[!(is.na(timeseries$tsendtime)),]#returns outfalls that have data
timeseries$tsendtime<-format(mdy(timeseries$tsendtime))
timeseries$facilityID<-gsub("echo_","", as.character(timeseries$hydrocode))
timeseries$facilityID<-substr(timeseries$facilityID,1,nchar(timeseries$facilityID)-3)


save.image(file="G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/timeseries_2010_present.RData")

#------------------Timeseries Flags-------------------#

#-----------------------------------------------------------------#
#-------ECHO Measured Effluents > VPDES Design flow---------------#

#Flag facilities that report measured effluent greater than the design flow 
df<-subset(design_flow,select=c(1,4))
colnames(df)<-c("facilityID","DesignFlow_mgd")
df$facilityID<-gsub("echo_","", as.character(df$facilityID))
timeseries<-merge(timeseries,df,by="facilityID",all.x=T)
timeseries$dmr_flag_desflow<-ifelse(timeseries$tsvalue>timeseries$DesignFlow_mgd & timeseries$DesignFlow_mgd>0,"dmr_flag_desflow",NA)

#-----------------------------------------------------------------#
#------Measured Effluents > 100*Median Measured Effluent----------#
#------Measured Effluents > 100,000*Median Measured Effluent------#
#---------------Potential Unit Conversion Error-------------------#

#Summmarize measured effluent values from ECHO by OutfallID--Not by Facility#
timeseries_summary<-timeseries%>%
  dplyr::group_by(hydrocode)%>% #important to note that we are summarizing discharge by outfall here 
  summarise(Median_ME=median(tsvalue, na.rm = T))

timeseries<-merge(timeseries,timeseries_summary,by="hydrocode",all.x=T)

timeseries$dmr_flag_units_100<-ifelse(timeseries$tsvalue>100*timeseries$Median_ME,"dmr_flag_units_100",NA)
timeseries$dmr_flag_units_1000000<-ifelse(timeseries$tsvalue>1000000*timeseries$Median_ME,"dmr_flag_units_1000000",NA)

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
#permit ID not to be confused with outfallID which is unique for each outfall


adminreg<-data.frame(bundle='permit', admincode=ECHO_Facilities$Facility.ID, description=ECHO_Facilities$CWPPermitTypeDesc, name=ECHO_Facilities$CWPName,stringsAsFactors = F)
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
facilities<-data.frame(bundle='facility',name=ECHO_Facilities$CWPName,stringsAsFactors = F)
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

#---------Assign Coordinates to Outfalls that have DMRs from 2010-Present----------#
Facility_Coord<-subset(ECHO_Facilities,select=c(1,6,7)) #Isolate Facility Level Coordinates
timeseries$OutfallID<-gsub("echo_","",timeseries$hydrocode)

ECHO_Outfalls<-timeseries%>%
                    group_by(OutfallID)%>%summarise(Facility.ID=first(facilityID))
ECHO_Outfalls<-merge(ECHO_Outfalls,Facility_Coord,by="Facility.ID",all.x=T)

VPDES_Outfalls<-subset(VPDES_Outfalls,select = c(1,17,18))
ECHO_Outfalls<-merge(ECHO_Outfalls,VPDES_Outfalls,by="OutfallID",all.x=T)

#----If the outfall is not in the VPDES database and doesn't have specified outfall coordinates, use facility level coordinates----#
ECHO_Outfalls$Longitude<-ifelse(is.na(ECHO_Outfalls$Longitude),ECHO_Outfalls$FacLong,ECHO_Outfalls$Longitude)
ECHO_Outfalls$Latitude<-ifelse(is.na(ECHO_Outfalls$Latitude),ECHO_Outfalls$FacLat,ECHO_Outfalls$Latitude)


releasepoint<-data.frame(bundle=rep('transfer',length(ECHO_Outfalls$OutfallID)),
                         name=paste0('TO ',ECHO_Outfalls$OutfallID),
                         ftype=rep('release',length(ECHO_Outfalls$OutfallID)),
                         hydrocode=paste0('vahydro_',ECHO_Outfalls$OutfallID),
                         fstatus=ifelse(ECHO_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                         dh_link_facility_mps=paste0('echo_',ECHO_Outfalls$Facility.ID),
                         stringsAsFactors = F)

for (i in 1:length(releasepoint$bundle)){
  print(paste("Processing Release Point ",i," of ", length(releasepoint$hydrocode)))
  if(!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]])){
    releasepoint$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]],')')
  } else {
    lat<-ECHO_Outfalls$Latitude[ECHO_Outfalls$Facility.ID==ECHO_Outfalls$Facility.ID[i]]
    long<-ECHO_Outfalls$Longitude[ECHO_Outfalls$Facility.ID==ECHO_Outfalls$Facility.ID[i]]
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

#Generates the conveyance import using the outfall list in 'ECHO_Outfalls'
#Generates the conveyance import using the outfall list in 'ECHO_Outfalls'
conveyance<-data.frame(bundle=rep('conveyance',length(ECHO_Outfalls$OutfallID)),
                       name=paste0(ECHO_Outfalls$Facility.ID,' TO ',ECHO_Outfalls$OutfallID),
                       ftype="water_transfer",
                       hydrocode=paste0('vahydro_',ECHO_Outfalls$Facility.ID,'_',ECHO_Outfalls$OutfallID),
                       fstatus=ifelse(ECHO_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                       field_dh_from_entity=paste0('vahydro_',ECHO_Outfalls$OutfallID),
                       field_dh_to_entity=paste0('echo_',ECHO_Outfalls$OutfallID),
                       stringsAsFactors = F)

#write.table(conveyance,paste0(Outputpath,"/conveyance.txt"),sep="\t",row.names = F)

#Outfall_Outfalls Generation
#Reformats 'ECHO_Outfalls' using available VPDES or ECHO geometry data and ECHO attributes
outfalls<-data.frame(bundle=rep('transfer',length(ECHO_Outfalls$OutfallID)),
                     name=paste0('FROM ',ECHO_Outfalls$Facility.ID),
                     ftype='outfall',
                     hydrocode=paste0('echo_',ECHO_Outfalls$OutfallID),
                     fstatus=ifelse(ECHO_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                     dh_link_facility_mps=paste0('echo_',ECHO_Outfalls$Facility.ID),
                     stringsAsFactors = F)

for (i in 1:length(outfalls$bundle)){
  print(paste("Processing Outfall ",i," of ", length(outfalls$hydrocode)))
  if(!is.na(ECHO_Outfalls$Latitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]]) & !is.na(ECHO_Outfalls$Longitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]])){
    outfalls$dh_geofield[i]<-paste0('POINT (',ECHO_Outfalls$Longitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]],' ',ECHO_Outfalls$Latitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]],')')  
  } else if (!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]])) {
    outfalls$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility.ID==ECHO_Outfalls$Facility.ID[i]])
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

#write.table(cwp_cso_outfalls,paste0(Outputpath,"/cwp_cso_outfalls.txt"),sep="\t",row.names = F)

wb_gnis_name<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='wb_gnis_name', propname='wb_gnis_name', propvalue='', proptext='',propcode=ECHO_Facilities$RadGnisName, startdate='',enddate='')
#write.table(wb_gnis_name,paste0(Outputpath,"/wb_gnis_name.txt"),sep="\t",row.names = F)

reachcode_rad<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='reachcode_rad', propname='reachcode_rad', propvalue='', proptext='',propcode=ECHO_Facilities$RadReachcode, startdate='',enddate='')

#write.table(reachcode_rad,paste0(Outputpath,"/reachcode_rad.txt"),sep="\t",row.names = F)

impair_cause<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility.ID), varkey='impair_cause', propname='impair_cause', propvalue='', proptext=ECHO_Facilities$AttainsCauseGroups,propcode='', startdate='',enddate='')
#write.table(impair_cause,paste0(Outputpath,"/impair_cause.txt"),sep="\t",row.names = F)

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
permit.dataframe_ii<-data.frame()

#----vectorise and pre-allocate data structures before entering loop-----#
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
  
  permit.dataframe_i <- getAdminregFeature(permit_inputs[i,], site, adminreg_feature)
  
  if(permit.dataframe_i[1]==FALSE){ #if the features exists in VAHydro, it will not create it
    permit.dataframe_ii <- postAdminregFeature(permit_inputs[i,], site, adminreg_feature)
    print(permit.dataframe_ii) #print status of updating: error 403 means administration block
    
  } else {
    print("This Adminreg Feature already exists")
  }
  permit.dataframe<-rbind(permit.dataframe,permit.dataframe_i,permit.dataframe_ii) #creating this to keep adminids
}

#unique adminid's for each permit. Used to link facilities to permits
permit.dataframe<-permit.dataframe[complete.cases(permit.dataframe),]
permit.adminid<-data.frame(admincode=permit.dataframe$admincode,adminid=permit.dataframe$adminid) 

rm(permit.dataframe_i,permit.dataframe_ii)
############################################################################################
# RETRIEVE/CREATE/UPDATE FACILITY DH FEATURE
############################################################################################  


facilities<-facilities[facilities$dh_link_admin_location%in%permit.adminid$admincode,]

facility.dataframe <- data.frame()
facility.hydroid <- character()
facility.dataframe_ii<-data.frame()


facility_inputs <- data.frame(
  bundle = as.character(facilities$bundle),
  ftype = as.character(facilities$ftype),
  hydrocode = as.character(facilities$hydrocode),
  name = as.character(facilities$name),
  fstatus = as.character(facilities$fstatus),
  address1 = as.character(facilities$address1),
  city = as.character(facilities$city),
  dh_link_admin_location = permit.adminid$adminid[match(permit.adminid$admincode,facilities$dh_link_admin_location)], #%in% operator looks for matches in left operand and returns permit.adminid if there is one
  dh_geofield = as.character(facilities$dh_geofield),
  stringsAsFactors=FALSE
) 

for (i in 1:length(facility_inputs$hydrocode)){
  print(paste("Processing Facility ",i," of ", length(facilities$dh_link_admin_location)))
  facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature) #need token now for access
  
  if(facility.dataframe_i[1]==FALSE){
  facility.dataframe_ii <- postFeature(facility_inputs[i,], site, feature)
  print(facility.dataframe_ii)

  }else{
    print("This Facility Feature already exists")
  }
 
  facility.dataframe<-rbind(facility.dataframe,facility.dataframe_i,facility.dataframe_ii)
  
}

facility.hydroid<-data.frame(hydrocode=facility.dataframe$hydrocode,hydroid=facility.dataframe$hydroid)

rm(facility.dataframe_i,facility.dataframe_ii)
############################################################################################
# RETRIEVE/CREATE/UPDATE FACILITY METADATA PROPERTIES
# Make sure hydrocodes are in same order as in the facility data frame as well
############################################################################################   
# Waterbody Name (GNIS): Name of waterbody from the Geographic Names Information System database where the facility is permitted to discharge directly

wb_gnis_name<-wb_gnis_name[gsub("echo_","",wb_gnis_name$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

prop_inputs <-data.frame(
  featureid = facility.hydroid$hydroid[match(facility.hydroid$hydrocode,wb_gnis_name$hydrocode)],
  varkey = as.character(wb_gnis_name$varkey),
  entity_type = rep(paste0('dh_feature'),length(wb_gnis_name$hydrocode)),
  propname = as.character(wb_gnis_name$propname),
  propvalue = rep(NA,length(wb_gnis_name$hydrocode)),
  proptext = rep(NA,length(wb_gnis_name$hydrocode)),
  propcode = as.character(wb_gnis_name$propcode),
  startdate = rep(NA,length(wb_gnis_name$hydrocode)),
  enddate = rep(NA,length(wb_gnis_name$hydrocode)),
  stringsAsFactors = F
)

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(wb_gnis_name$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
  property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
  print(property.dataframe_ii)

  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_wbgnis<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################ 
#Combined Sewer System (css): The discharge from a combined sewer system at a point prior to a treatment plant

css<-css[gsub("echo_","",css$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

prop_inputs <-data.frame(
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,css$hydrocode)],
  varkey = as.character(css$varkey),
  entity_type = rep(paste0('dh_feature'),length(css$hydrocode)),
  propname = as.character(css$propname),
  propvalue = rep(NA,length(css$hydrocode)),
  proptext = rep(NA,length(css$hydrocode)),
  propcode = as.character(css$propcode),
  startdate = rep(NA,length(css$hydrocode)),
  enddate = rep(NA,length(css$hydrocode)),
  stringsAsFactors = F
)

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(css$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_css<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################ 
#Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)

cwp_cso_outfalls<-cwp_cso_outfalls[gsub("echo_","",cwp_cso_outfalls$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

prop_inputs <-data.frame(
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,cwp_cso_outfalls$hydrocode)],
  varkey = as.character(cwp_cso_outfalls$varkey),
  entity_type = rep(paste0('dh_feature'),length(cwp_cso_outfalls$hydrocode)),
  propname = as.character(cwp_cso_outfalls$propname),
  propvalue = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  proptext = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  propcode = as.character(cwp_cso_outfalls$propcode),
  startdate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  enddate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  stringsAsFactors = F
)

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(cwp_cso_outfalls$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_cwp_cso_outfalls<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################ 
#Impairment Class or Category of the Waterbody (impair_cause)

impair_cause<-impair_cause[gsub("echo_","",impair_cause$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

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

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(impair_cause$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_impair<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################ 
#Date of most recent inspection of the facility (last_inspect)

last_inspect<-last_inspect[gsub("echo_","",last_inspect$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

prop_inputs <-data.frame(
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,last_inspect$hydrocode)],
  varkey = as.character(last_inspect$varkey),
  entity_type = rep(paste0('dh_feature'),length(last_inspect$hydrocode)),
  propname = as.character(last_inspect$propname),
  propvalue = rep(NA,length(last_inspect$hydrocode)),
  proptext = rep(NA,length(last_inspect$hydrocode)),
  propcode = as.character(last_inspect$propcode),
  startdate = rep(NA,length(last_inspect$hydrocode)),
  enddate = rep(NA,length(last_inspect$hydrocode)),
  stringsAsFactors = F
)

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(last_inspect$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_last_inspect<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################ 
#Unique ID for Waterbody (reachcode_rad)

reachcode_rad<-reachcode_rad[gsub("echo_","",reachcode_rad$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

prop_inputs <-data.frame(
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,reachcode_rad$hydrocode)],
  varkey = as.character(reachcode_rad$varkey),
  entity_type = rep(paste0('dh_feature'),length(reachcode_rad$hydrocode)),
  propname = as.character(reachcode_rad$propname),
  propvalue = rep(NA,length(reachcode_rad$hydrocode)),
  proptext = rep(NA,length(reachcode_rad$hydrocode)),
  propcode = as.character(reachcode_rad$propcode),
  startdate = rep(NA,length(reachcode_rad$hydrocode)),
  enddate = rep(NA,length(reachcode_rad$hydrocode)),
  stringsAsFactors = F
)

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(reachcode_rad$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==F){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_reachcoderad<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################ 
# Facility Design Flow in MGD (design_flow)

design_flow<-design_flow[gsub("echo_","",design_flow$hydrocode)%in%permit.adminid$admincode,]

property.dataframe<-data.frame()
property.dataframe_ii<-data.frame()

prop_inputs <-data.frame(
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,design_flow$hydrocode)],
  varkey = as.character(design_flow$varkey),
  entity_type = rep(paste0('dh_feature'),length(design_flow$hydrocode)),
  propname = as.character(design_flow$propname),
  propvalue = rep(NA,length(design_flow$hydrocode)),
  proptext = rep(NA,length(design_flow$hydrocode)),
  propcode = as.character(design_flow$propcode),
  startdate = rep(NA,length(design_flow$hydrocode)),
  enddate = rep(NA,length(design_flow$hydrocode)),
  stringsAsFactors = F
)

for (i in 1:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(design_flow$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i,property.dataframe_ii) #store all properties in same large dataframe to double check quality of script
}
property.pid_design_flow<-data.frame(featureid=property.dataframe$featureid,property.pid=property.dataframe$pid)

rm(property.dataframe_i,property.dataframe_ii)
############################################################################################
# RETRIEVE/CREATE/UPDATE RELEASE DH FEATURE
############################################################################################  

release.dataframe<-data.frame()
release.dataframe_ii<-data.frame()

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

for (i in 1:length(release_inputs$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(release_inputs$hydrocode)))
  release.dataframe_i <- getFeature(release_inputs[i,], token, site, feature)
  
  if(release.dataframe_i[1]==FALSE){
  release.dataframe_ii <- postFeature(release_inputs[i,],site, feature)
  print(release.dataframe_ii)
  
  }else{
    print("This Feature already exists")
  }
  release.dataframe<-rbind(release.dataframe,release.dataframe_i,release.dataframe_ii)
 
}
release.dataframe<-release.dataframe[complete.cases(release.dataframe),]
release.hydroid<-data.frame(hydrocode=release.dataframe$hydrocode, hydroid=release.dataframe$hydroid)


rm(release.dataframe_i,release.dataframe_ii)
############################################################################################
# RETRIEVE/CREATE/UPDATE OUTFALL DH FEATURE
############################################################################################   

outfall.dataframe<-data.frame()
outfall.dataframe_i<-data.frame()
outfall.dataframe_ii<-data.frame()

outfall_inputs <- data.frame(
  bundle = as.character(outfalls$bundle),
  ftype = as.character(outfalls$ftype),
  hydrocode = as.character(outfalls$hydrocode),
  name = as.character(outfalls$name),
  fstatus = as.character(outfalls$fstatus),
  dh_link_facility_mps =  facility.hydroid$hydroid[match(outfalls$dh_link_facility_mps,facility.hydroid$hydrocode)], #dependent on facility import
  dh_geofield = as.character(outfalls$dh_geofield),
  stringsAsFactors=FALSE
) 

for (i in 1:length(outfalls$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(outfalls$hydrocode)))
  outfall.dataframe_i <- getFeature(outfall_inputs[i,], token, site, feature)
  
  if(outfall.dataframe_i[1]==FALSE){
  outfall.dataframe_ii <- postFeature(outfall_inputs[i,], site, feature)
  print(outfall.dataframe_ii)
  
  }else{
    print("This Feature already exists")
  }
  outfall.dataframe<-rbind(outfall.dataframe,outfall.dataframe_i,outfall.dataframe_ii)
}

outfall.hydroid<-data.frame(hydrocode=outfall.dataframe$hydrocode,hydroid=outfall.dataframe$hydroid)


############################################################################################
# RETRIEVE/CREATE/UPDATE CONVEYANCE DH FEATURE
############################################################################################  


# Format conveyance geom from release and outfall geoms 

release.geofield <- substring(release.dataframe$dh_geofield, 8)
release.geofield <-substr(release.geofield, 1, nchar(release.geofield)-1) 

outfall.geofield <- substring(outfall.dataframe$dh_geofield, 8)
outfall.geofield <-substr(outfall.geofield, 1, nchar(outfall.geofield)-1) 
conveyance.geofield <- paste('LINESTRING (',release.geofield,', ',outfall.geofield,')',sep="")

conveyance.dataframe<-data.frame()
conveyance.dataframe_ii<-data.frame()


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
  conveyance.dataframe_i <- getFeature(conveyance_inputs[i,], token, site, feature)
  
  if(conveyance.dataframe_i[1]==FALSE){
    conveyance.dataframe_ii <- postFeature(conveyance_inputs[i,], site, feature)
    print(conveyance.dataframe_ii)
  }else{
    print("This Feature already exists")
  }
  conveyance.dataframe<- rbind(conveyance.dataframe, conveyance.dataframe_i,conveyance.dataframe_ii)

}
conveyance.dataframe<-conveyance.dataframe[complete.cases(conveyance.dataframe),]
conveyance.hydroid<-data.frame(hydrocode=conveyance.dataframe$hydrocode,hydroid=conveyance.dataframe$hydroid)
############################################################################################
# RETRIEVE/CREATE/UPDATE TIMESERIES
############################################################################################  

timeseries.tid<-character()
timeseries.dataframe<-data.frame()
timeseries.complete<-data.frame()
timeseries.dataframe_i<-data.frame()
timeseries.dataframe_ii<-data.frame()
featureid_dmr_flag_desflow<-character()
featureid_dmr_flag_units_100<-character()
featureid_dmr_flag_units_1000000<-character()
featureid_echo_flag<-character()
flag_inputs_dmr_flag_desflow<-data.frame()
flag_inputs_dmr_flag_units_100<-data.frame()
flag_inputs_1000000<-data.frame()
flag_inputs_echo_flag<-data.frame()

ts_inputs<-data.frame(
  featureid = as.character(outfall.hydroid$hydroid[match(timeseries$hydrocode,outfall.hydroid$hydrocode)]),
  varkey = as.character(timeseries$varkey),
  entity_type = rep(paste0('dh_feature'),length(timeseries$hydrocode)),
  tsvalue = as.numeric(timeseries$tsvalue),
  tscode = as.numeric(timeseries$tscode),
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

for (i in 44285:length(ts_inputs$featureid)){
  print(paste("Processing DMR Entry ",i," of ", length(ts_inputs$featureid)))
  timeseries.dataframe_i <- getTimeseries(ts_inputs[i,], site, ts)
  
  if(timeseries.dataframe_i[1]==FALSE){
  timeseries.dataframe_ii <- postTimeseries(ts_inputs[i,],site,ts)
  print(timeseries.dataframe_ii)
  timeseries.dataframe_ii<-getTimeseries(ts_inputs[i,], site, ts)
  }else{
    print("This timeseries Feature already exists")
  }

  timeseries.dataframe<-rbind(timeseries.dataframe_i,timeseries.dataframe_ii)
  timeseries.complete<-rbind(timeseries.complete,timeseries.dataframe_i,timeseries.dataframe_ii)

  if(ts_inputs$dmr_flag_desflow[i]=="dmr_flag_desflow"){
    flag_inputs_dmr_flag_desflow_i <-data.frame(
      featureid_dmr_flag_desflow = as.character(subset(timeseries.dataframe$tid,!is.na(timeseries.dataframe$tid))),
      varkey = 'dmr_flag_desflow',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_desflow',
      proptext= 'Exceeds Facility Design Flow',
      propcode = "dmr_flag_desflow",
      stringsAsFactors = F
    )
    
    flag_inputs_dmr_flag_desflow<-rbind(flag_inputs_dmr_flag_desflow,flag_inputs_dmr_flag_desflow_i)
  }else{
    print("No dmr_flag_desflow flag")
  }
  
  if(ts_inputs$dmr_flag_units_100[i]=="dmr_flag_units_100"){
    flag_inputs_dmr_flag_units_100_i <-data.frame(
      featureid_dmr_flag_units_100 = as.character(subset(timeseries.dataframe$tid,!is.na(timeseries.dataframe$tid))),
      varkey = 'dmr_flag_units_100',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_units_100',
      proptext= "Exceeds 100*Median Outfall Discharge",
      propcode = "dmr_flag_units_100",
      stringsAsFactors = F
    )
    flag_inputs_dmr_flag_units_100<-rbind(flag_inputs_dmr_flag_units_100,flag_inputs_dmr_flag_units_100_i)
  }else{
    print("No dmr_flag_units_100 flag")
  }
  
  
    
  if(ts_inputs$dmr_flag_units_1000000[i]=="dmr_flag_units_1000000"){
    flag_inputs_1000000_i <-data.frame(
      featureid_dmr_flag_units_1000000 = as.character(subset(timeseries.dataframe$tid,!is.na(timeseries.dataframe$tid))),
      varkey = 'dmr_flag_units_1000000',
      entity_type = 'dh_timeseries',
      propname = 'dmr_flag_units_1000000',
      proptext= "Exceeds 1000000*Median Outfall Discharge",
      propcode = "dmr_flag_units_1000000",
      stringsAsFactors=F
    )
    flag_inputs_1000000<-rbind(flag_inputs_1000000,flag_inputs_1000000_i)
  }else{
    print("No dmr_flag_units_1000000 flag")
  }
  
  if(!(ts_inputs$echo_flag[i]=="")){
    flag_inputs_echo_flag_i <-data.frame(
      featureid_echo_flag = as.character(subset(timeseries.dataframe$tid,!is.na(timeseries.dataframe$tid))),
      varkey = 'echo_flag',
      entity_type = 'dh_timeseries',
      propname = 'echo_flag',
      proptext= ts_inputs$echo_flag[i],
      propcode = ts_inputs$echo_flag[i],
      stringsAsFactors = F
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

