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

#library(foreign) --Not used in script from what I can see - JM
library(sp) # used for coordinates() #load sp before rgdal #SPtransform() used in R_functions.R for spatial containment
library(rgdal) #readOGR() used to read VPDES outfall layer in VPDES_Geodatabase.gdb #also used in r_functions 
library(dplyr) #conflicts with plyr, so be mindful of this 
library(XML) #xmlParse and xmlToList() used in R_functions.R for QID pull 
library(RCurl) #getURL() used in R_functions.R for QID pull 
library(readxl) #read_excel() used for design flow spreadsheet pull from VPDES
#library(jsonlite) --Not used in script from what I can see - JM
library(lubridate) #round_date() used for monthly average outfall discharge 
library(httr) #GET() used to pull design flow spreadsheet
#library(stringr) --Not used in script (might be remnant from trying to trim whitespace str_trim()) - JM
library(proj4) #project() used to change coordinate projections of VPDES outfalls
#library(xml2) --Not used in script from what I can see - JM
library(tibble) #add_column() used to add "Year" column to outfall ts   
library(data.table) #first() used in group_by to summarize (not sure how that works)
library(magrittr) #forward-pipe operator used to read dplyr functions left to right 
library(rgeos) #over() used in R_functions.R for spatial containment function 
library(sqldf) #used for subsetting and filtering 
library(anytime) #required for date formatting (may change later)
library(echor) #used to pull ECHO data
library(readr) #used to view problems() errors
library(rgdal)

basepath ='/var/www/R'
source(paste0(basepath,'/config.R'))
#localpath <-"/usr/local/home/git/"
HUC6_path <- paste0(github_location,"/HARParchive/GIS_LAYERS/HUC.gdb") #Location of HUC .gdb
HUC6_layer_name <- 'WBDHU6' #HUC6 layer withing the HUC .gdb
source(paste(localpath,"/USGS_Consumptive_Use/Code/ECHO to VAHydro/R_functions.R", sep = ""))
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/GIS_functions/GIS_functions.R")

base_url <- "http://deq1.bse.vt.edu/d.alpha"

# Generate REST token for authentication              
token <-trimws(rest_token(base_url, token, rest_uname, rest_pw))


#set timeframe
startDate <- '01/01/2020'
endDate <- '12/31/2020'

#####################################################################
# Parse command line arguments
argst <- commandArgs(trailingOnly=T)
if (length(argst) > 0) {
  spoint <- as.integer(argst[1])
} else {
  spoint = 1
}
if (length(argst) > 1) {
  import_mode <- argst[2]
} else {
  import_mode = 'vahydro'
}
if (length(argst) > 2) {
  id_prefix <- argst[3]
} else {
  id_prefix <- ''
}
if (length(argst) > 3) {
  base_url <- argst[4]
}
print(argst)

#id_prefix is used for filtering out specific permit IDs

print(paste0("Using Import mode ", import_mode))
print(paste0("Using Base URL ", base_url))
print(paste0("Allowed prefix ", id_prefix))


####################################Inputs##########################################
 #shows a list of all fields and descriptions
 
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
#stash ECHO_facilities because ECHO is not always online to pull at the time of running the code
#write.csv(ECHO_Facilities,file=paste0(github_location,"/USGS_Consumptive_Use/ECHO_Facilities.csv"))
 ECHO_Facilities <- read.csv(paste0(github_location,"/USGS_Consumptive_Use/ECHO_Facilities.csv"))

 end_time <- Sys.time()
 print(paste("Download Process Complete: ",end_time ,sep=""))
 print(paste("Time elapsed: ",end_time-start_time,sep=""))
 
 print(paste("Number of Facilities Before Spatial Containment", length(ECHO_Facilities[,1])))
 
 coordinates(ECHO_Facilities) <- c("FacLong", "FacLat") # add col of coordinates, convert dataframe to Large SpatialPointsDataFrame
 ECHO_Facilities <- sp_contain(HUC6_path,HUC6_layer_name,ECHO_Facilities)
 #------------------------------------------------------------
 #ECHO_Facilities_original <- ECHO_Facilities 
 ECHO_Facilities <- ECHO_Facilities[-which(is.na(ECHO_Facilities$Poly_Code)),]
 #think about adding a visual check like plotting on a map
 print(paste("Number of Facilities After Spatial Containment", length(ECHO_Facilities[,1])))
 

ECHO_Facilities <- data.frame(ECHO_Facilities)

#use sqldf for replacements
keep_permits <- "SELECT *
                FROM ECHO_Facilities
                WHERE ( CWPPermitTypeDesc = 'NPDES Individual Permit'
                OR CWPPermitTypeDesc = 'General Permit Covered Facility' ) "
# #use sqldf for replacements
# if (id_prefix != '') {
#   keep_permits <- paste0(keep_permits, 
#     "AND SourceID LIKE '", id_prefix, "%'")
# }

ECHO_Facilities <- sqldf(keep_permits)


print(paste("Number of Facilities After Permit Type Description Subset: ",length(ECHO_Facilities[,1])))

#rename SourceID column to Facility_ID 
colnames(ECHO_Facilities)[colnames(ECHO_Facilities)=="SourceID"] <- "Facility_ID"

backup <- ECHO_Facilities

#GET EPA ADMINREG FEATURE FROM VAHYDRO
agency_inputs <- list(bundle = 'authority',ftype = 'federal_enviro_agency',admincode = 'epa',stringsAsFactors = FALSE) 
agency_dataframe <- getAdminregFeature(agency_inputs, base_url, adminreg_feature)
agency_adminid <- as.character(agency_dataframe$adminid)


effdate_default <- '1970/01/01'
expdate_default <- '1970/01/01'
endDate<-format(as.Date(endDate, "%m/%d/%Y"), "%m/%d/%Y")

# Get outfall locs from VPDES )(if present)
VPDES_Outfalls <- cu_echo_get_VPDES_outfalls() #SKIP THESE IN R, RUN ON COMMAND LINE WHEN THERE ARE ID_PREFIX
# get design_flow from VPDES (if present)
VPDES_DesignFlow <- cu_echo_get_VPDES() 
# Attach design flow to Facilities
ECHO_Facilities <- df_coord_pull(ECHO_Facilities, VPDES_DesignFlow)
# get formatted list of design flows for outfalls
design_flow <- cu_echo_get_VPDES_design_flow(ECHO_Facilities)
write.table(ECHO_Facilities,"ECHO_Facilities.txt",append = FALSE, quote = TRUE, sep="\t") # SKIP THROUGH THIS LINE
#i <- 1048
#i <- 26951
# Create or retreive the Permit for each facility 
#ECHO_Facilities <- ECHO_Facilities[1:5,] # JM uses: 13465:13470 # 8034:8040 misc Dominion energy
permit_dataframe <- NULL
facility_dataframe <- NULL
for (i in spoint:(length(ECHO_Facilities[,1]))){
  ECHO_Facilities_i <- ECHO_Facilities[i,]
  print(paste("Checking for DMR DATA FOR FACILITY ",i," OF ",length(ECHO_Facilities[,1]),sep=""))
  DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",ECHO_Facilities_i$Facility_ID,"&parameter_code=50050&start_date=",startDate,"&end_date=",endDate) 
  #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe # 50050 only looks at Flow, in conduit ot thru treatment plant - there are 347 parameter codes defined in ECHO
  #DMR_data<-read.csv(DMR_data,sep = ",", stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
  DMR_data<-echoGetEffluent(ECHO_Facilities_i$Facility_ID, parameter_code = '50050',start_date=startDate,end_date=endDate)
  # We only create facility/permit features if we have actual outfall data to manage
  if ((as.integer(nrow(DMR_data)) > 0) ) {
    if (is.na(ECHO_Facilities_i$CWPEffectiveDate)) {
      ECHO_Facilities_i$CWPEffectiveDate <- effdate_default
    }
    if (is.na(ECHO_Facilities_i$CWPExpirationDate)) {
      ECHO_Facilities_i$CWPExpirationDate <- expdate_default
    }
    # 
    print(paste("PROCESSING PERMIT ",i," OF ",length(ECHO_Facilities[,1]),sep=""))
    permit <- permit_REST(ECHO_Facilities_i, agency_adminid)
    if (is.null(permit_dataframe)) {
      permit_dataframe <- permit
    } else {
      permit_dataframe <- sqldf(
        "select * from permit_dataframe
         UNION
           select * from permit
      ")
    }
    print(permit)
    
    print("PROCESSING FACILITY")
    # check if facility has a known match Facility in vahydro that is NOT of ECHO origin
    # - If YES, just load the facility, do not push any updates
    # - If NO, create/update 
    ECHO_Facilities_i$hydrocode <- as.character(paste0("echo_",ECHO_Facilities_i$Facility_ID))
    ECHO_Facilities_i <- vahydro_facility_match(ECHO_Facilities_i)
    if (!is.na(ECHO_Facilities_i$matched_hydroid)) {
      print(paste0("Found Matched Facility with hydroid = ", ECHO_Facilities_i$matched_hydroid))
      facility <- getFeature(list(hydroid = ECHO_Facilities_i$matched_hydroid), token, base_url)
      dh_link_admin_location = as.character(permit$adminid)
    } else {
      facility <- facility_REST(ECHO_Facilities_i, permit, token)
    }
    
    if (is.null(facility_dataframe)) {
      facility_dataframe <- facility
    } else {
      facility_dataframe <- sqldf(
        "select * from facility_dataframe
         UNION
           select * from facility
      ")
    }
    print(facility)
    
    print("PROCESSING OUTFALLS")
    print(paste("PROCESSING DMR DATA FOR FACILITY ",i," OF ",length(ECHO_Facilities[,1]),sep=""))
    outfalls <- outfall_features_REST(DMR_data, facility, token, base_url)
    # get timeseries - this function makes a redundant call to echo for ts data... should replace
    facts <- ts_ECHO_pull(ECHO_Facilities_i,1, startDate, endDate)
    # flag errors
    facts <- permit(facts)
    if (import_mode == 'vahydro') {
      # push to VAHydro
      tsdf <- ts_import(outfalls,facts,1, base_url)
    } else {
      # export as a file 
      tsdf <- dh_echo_format_ts(facts, outfalls)
      if (i == 1) {
        write.table(
          tsdf,"ts-export.txt",append = FALSE, 
          quote = FALSE, sep="\t", row.names = FALSE,
          col.names = TRUE
        )
      } else {
        write.table(
          tsdf,"ts-export.txt",append = TRUE, 
          quote = TRUE, sep="\t", row.names = FALSE,
          col.names = FALSE
        )
      }
      
    }
  } else {
    print(paste0("No DMR data exists for facility ", i, " will not process") )
  }
}

# Returns number of entries in database assigned to each state
n_states<- function(database){
  DC<-length(grep("DC",database$hydrocode))
  MD<-length(grep("MD",database$hydrocode))
  NC<-length(grep("NC",database$hydrocode))
  PA<-length(grep("PA",database$hydrocode))
  VA<-length(grep("VA",database$hydrocode))
  WV<-length(grep("WV",database$hydrocode))
  
  return(list(DC=DC,MD=MD,NC=NC,PA=PA,VA=VA,WV=WV))
  
}

#n_states(release.dataframe)
n_states(timeseries)
