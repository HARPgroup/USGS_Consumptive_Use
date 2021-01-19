##################################################################################################################################
#-------------------------------------------------ECHO_Timeseries.R Script-------------------------------------------------------#

# Primary Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

##################################################################################################################################
#--------------------------------------------------------Purpose-----------------------------------------------------------------#

# Utilize the EPA's Enforcement and Compliance Online History (ECHO) Representation State Transfer (REST) Services
# to extract data on facilities regulated under the Clean Water Act (CWA) and managed under the National Pollutant 
# Discharge Elimination System (NPDES) Program. 

# Ultimately this statewide discharge data will be combined with withdrawal data to refine State Water Budget estimates.
# Accurate representations of water supply will fuel more informed policies and management practices.

##################################################################################################################################
#------------------------------------------------Links to ECHO Resources---------------------------------------------------------#

#The following links include inforamtion about the REST services used to query data associated with discharging facilities. 

#ECHO CWA REST Services: Facility Search - Water 
#https://echo.epa.gov/tools/web-services/facility-search-water

#ECHO EFF REST Services: Effluent Charts-Discharge Monitoring Reports
#https://echo.epa.gov/tools/web-services/effluent-charts

#ECHO DFR REST Services: Detailed Facility Report
#https://echo.epa.gov/tools/web-services/detailed-facility-report

##################################################################################################################################
#------------------------------------------------Load Library and Options--------------------------------------------------------#

rm(list=ls()) #Start with clear environment
localpath <-"G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis/DEQ Code updates"

library(foreign) #allows for easy manipulation of *.dbf file types
library(rgdal) #extract files from file geodatabases-like in ArcMap
library(dplyr) #data manipulation package that speeds up grouping, summarizing, ordering. 
library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(readxl) #reads excel files
library(jsonlite)
library(lubridate)
library(httr)
library(daff)#helps calculate the differences between two dataframes
library(data.table)
library(echor)
library(sqldf) #used for subsetting and filtering 


options(scipen=999) #Disable scientific notation
options(digits = 9)

##################################################################################################################################
#-------------------------------------------Clean Water Act Facility Download----------------------------------------------------#

# This section of the code generate a query in the CWA Water Facility Search REST Services in ECHO.
# It pulls every discharging facility regulated by the CWA with a NPDES permit. 
# Queries are created using a generated URL with the base address of: https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?

# This particular query is created in two-steps. An XML document of all CWA facilities in VA is first downloaded (uri_query and ECHO_xml).
# Then the XML is parsed (ECHO_query) to generate a query ID (QID) that can be used to access summary data for each facility (uri_summary).

echo_cols <- c(
  'CWPPermitTypeDesc', 'CWPName', 'CWPCounty', 'FacDerivedHuc', 'FacFIPSCode', 
  'SourceID', 'StateAuthPretreat', 'StateAuthGen', 'StateAuthBiosolids', 
  'NPDESDataGroups', 'CWPSICCodes', 'ElectrRptWaiverTypeCode', 'ElectrRptWaiverTypeDesc', 
  'CWPNAICSCodes', 'E90Pollutants3yr', 'FacLat', 'FacLong', 'CWPTotalDesignFlowNmbr', 
  'CWPActualAverageFlowNmbr', 'CWPFacilityTypeIndicator', 'CWPStreet', 'CWPCity', 
  'CWPState', 'IssuingAgency', 'SubmittedDate', 'CWPIssueDate', 'CWPTerminationDate', 
  'CWPMajorMinorStatusFlag', 'CWPSNCStatus', 'RegistryID', 'CWPComplianceTracking', 
  'CWPDateLastInspection', 'CWPDateLastInspSt','CWPPermitStatusDesc',
  'CWPEffectiveDate','CWPExpirationDate', 'CWPCsoFlag', 'CWPCsoOutfalls', 'RadGnisName',
  'RadReachcode', 'AttainsStateCauses' 
)

# Function to look up ECHO columns
ECHO_column_lookup <- function(echo_cols, mode='ObjectName', echo_meta = NULL) {
  if (is.null(echo_meta)) {
    echo_meta <- echoWaterGetMeta()
  }
  if (mode == 'ObjectName') {
    luargs <- paste(echo_cols,collapse = "','","", sep='')
    wclause = paste0(
      "WHERE ObjectName in ('",
      luargs,
      "')"
    )
    retcol = 'ColumnID'
  }
  if (mode == 'ColumnID') {
    luargs <- paste(echo_cols,collapse = ",","", sep='')
    wclause = paste0(
      "WHERE ColumnID in (",
      luargs,
      ")"
    )
    retcol = 'ObjectName'
  }
  retvals = sqldf(
    paste0(
      "select ", retcol,
      " FROM echo_meta ",
      wclause,
      " ORDER BY ",
      retcol
    )
  )
  return( retvals)
}

## Download facility info using bounding coordinates
echo_qcolids <- ECHO_column_lookup(echo_cols)
qcol_list <-  paste(echo_qcolids$ColumnID,collapse = ",","", sep='')
#using bounding box extent of VA to restrict which facilities are pulled
start_time <- Sys.time()
print(paste("Using echoWaterGetFacilityInfo() | (Start time: ",start_time,")",sep=""))
ECHO_Facilities <- echoWaterGetFacilityInfo(
  xmin = '-84', ymin = '35', 
  xmax = '-75',  ymax = '41', 
  output = 'df',
  qcolumns=qcol_list
)

## 
ECHO_Facilities <- subset(ECHO_Facilities, CWPState == "VA")


##################################################################################################################################
#----------------------------------------------Discharge Monitoring Report Download----------------------------------------------#


#rename SourceID column to Facility_ID 
colnames(ECHO_Facilities)[colnames(ECHO_Facilities)=="SourceID"] <- "Facility_ID"

startDate <- '01/01/2015'
endDate <- '12/31/2019'

DMR_DF <- c()
for (i in 1:length(ECHO_Facilities$Facility_ID)){
  ECHO_Facilities_i <- ECHO_Facilities[i,]
  DMR_data<-echoGetEffluent(ECHO_Facilities_i$Facility_ID, parameter_code = '50050',start_date=startDate,end_date=endDate)
  DMR_DF <- rbind (DMR_DF, DMR_data)
}

ECHO_timeseries <- DMR_DF
##################################################################################################################################
#---------------------------------Convert Annual, Semi-Annual, and Quarterly Entries into Monthly Entries------------------------#

nmbr_submissions_conversion<- function(nmbr_submissions,label){
  
  Submissions<-subset(ECHO_timeseries,ECHO_timeseries$Mon_in_MP==nmbr_submissions)
  
  summary<-Submissions%>%
    dplyr::summarise(Facilities=n_distinct(Submissions$Facility.ID),Outfalls=n_distinct(Submissions$OutfallID),
                     entries=n())
  
  FacilityName<-character()
  Facility.ID<-character()
  OutfallID<-character()
  Statistic<-character()
  Measured_Effluent<-numeric()
  Units<-character()
  Permitted_Limit<-numeric()
  MP_Begin_Date<-character()
  MP_End_Date<-character()
  Mon_in_MP<-numeric()
  Violation_Code<-character()
  Violation_Severity<-character()
  NODI<-character()
  Outfall_Type<-character()
  
  for (i in 1:length(Submissions$Facility.ID)){
    FacilityName_i<-character()
    Facility.ID_i<-character()
    OutfallID_i<-character()
    Statistic_i<-character()
    Measured_Effluent_i<-numeric()
    Units_i<-character()
    Permitted_Limit_i<-numeric()
    MP_Begin_Date_i<-character()
    MP_End_Date_i<-character()
    Mon_in_MP_i<-numeric()
    Violation_Code_i<-character()
    Violation_Severity_i<-character()
    NODI_i<-character()
    Outfall_Type_i<-character()
    
    FacilityName_i<-rep(as.character(Submissions$FacilityName[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Facility.ID_i<-rep(as.character(Submissions$Facility.ID[i]), as.numeric(Submissions$Mon_in_MP[i]))
    OutfallID_i<-rep(as.character(Submissions$OutfallID[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Statistic_i<-rep(as.character(Submissions$Statistic[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Measured_Effluent_i<-rep(as.character(Submissions$Measured_Effluent[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Units_i<-rep(as.character(Submissions$Units[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Permitted_Limit_i<-rep(as.character(Submissions$Permitted_Limit[i]), as.numeric(Submissions$Mon_in_MP[i]))
    MP_Begin_Date_i<-seq(as.Date(Submissions$MP_Begin_Date[i]),length=nmbr_submissions,by="1 month")
    MP_End_Date_i<-ceiling_date(MP_Begin_Date_i,"month")-days(1)
    Mon_in_MP<-rep(12, as.numeric(Submissions$Mon_in_MP[i]))
    Violation_Code_i<-rep(as.character(Submissions$Violation_Code[i]),as.numeric(Submissions$Mon_in_MP[i]))
    Violation_Severity_i<-rep(as.character(Submissions$Violation_Severity[i]),as.numeric(Submissions$Mon_in_MP[i]))
    NODI_i<-rep(as.character(Submissions$NODI[i]),as.numeric(Submissions$Mon_in_MP[i]))
    Outfall_Type_i<-rep(as.character(Submissions$Outfall_Type[i]),as.numeric(Submissions$Mon_in_MP[i]))
    
    FacilityName<-c(FacilityName,FacilityName_i)
    Facility.ID<-c(Facility.ID,Facility.ID_i)
    OutfallID<-c(OutfallID,OutfallID_i)
    Statistic<-c(Statistic,Statistic_i)
    Outfall_Type<-c(Outfall_Type,Outfall_Type_i)
    Measured_Effluent<-c(Measured_Effluent,Measured_Effluent_i)
    Units<-c(Units,Units_i)
    Permitted_Limit<-c(Permitted_Limit,Permitted_Limit_i)
    MP_Begin_Date<-c(MP_Begin_Date,as.character(MP_Begin_Date_i))
    MP_End_Date<-c(MP_End_Date,as.character(MP_End_Date_i))
    Mon_in_MP<-c(Mon_in_MP,Mon_in_MP_i)
    Violation_Code<-c(Violation_Code,Violation_Code_i)
    Violation_Severity<-c(Violation_Severity,Violation_Severity_i)
    NODI<-c(NODI,NODI_i)
    
  }
  
  nmbr_to_Monthly<-data.frame(FacilityName=FacilityName,
                                Facility.ID=Facility.ID,
                                OutfallID=OutfallID,
                                Statistic=Statistic,
                                Outfall_Type=Outfall_Type,
                                Measured_Effluent=Measured_Effluent,
                                Units=Units,
                                Permitted_Limit=Permitted_Limit,
                                MP_Begin_Date=MP_Begin_Date,
                                MP_End_Date=MP_End_Date,
                                Mon_in_MP=Mon_in_MP,
                                Violation_Code=Violation_Code,
                                Violation_Severity=Violation_Severity,
                                NODI=NODI)
  
  assign(label,nmbr_to_Monthly,envir = .GlobalEnv)
  
  return(list(summary))
  
  
}
nmbr_submissions_conversion(12,"Annual_to_Monthly")
nmbr_submissions_conversion(6,"SemiAnnual_to_Monthly")
nmbr_submissions_conversion(3,"Quarterly_to_Monthly")

#--------------------------------------------------------------------------------------------------------------------------------#
#------Remove Annual, Semi-Annual, and Quarterly Entries from ECHO_Timeseries and Replace with Converted Monthly Entries---------#

ECHO_timeseries<-subset(ECHO_timeseries,ECHO_timeseries$Mon_in_MP==1)
ECHO_timeseries%>%dplyr::summarise(Facilities=n_distinct(Facility.ID),Outfalls=n_distinct(OutfallID),entries=n())

ECHO_timeseries<-rbind(ECHO_timeseries,Annual_to_Monthly,SemiAnnual_to_Monthly,Quarterly_to_Monthly)
ECHO_timeseries<-as.data.table(ECHO_timeseries)

duplicated<-ECHO_timeseries[duplicated(ECHO_timeseries, by=c("OutfallID","MP_End_Date")),]
ECHO_timeseries<-ECHO_timeseries[!duplicated(ECHO_timeseries, by=c("OutfallID","MP_End_Date")),]

#################################################################################################################################
#----------------------------Save Timeseries as table for QA/QC in other scripts------------------------------------------------#

# Trim dataframe to specified range
ECHO_timeseries<-ECHO_timeseries[ECHO_timeseries$MP_Begin_Date %within% interval("2010-01-01","2016-12-31"),]

save.image("./ECHO_Timeseries.RData") #Save the global environment for future reference

#In personal file
write.table(ECHO_timeseries,"./ECHO_timeseries_3_28.txt",sep="\t",row.names = F)


