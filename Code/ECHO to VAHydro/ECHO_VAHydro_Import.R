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

localpath <-"/usr/local/home/git/"
HUC6_path <- "hydro-tools/GIS_LAYERS/HUC.gdb" #Location of HUC .gdb
HUC6_layer_name <- 'WBDHU6' #HUC6 layer withing the HUC .gdb

basepath <- "http://deq2.bse.vt.edu/d.alpha"

# #Generate REST token for authentication              
rest_uname = FALSE
rest_pw = FALSE
source(paste(localpath,"hydro-tools/auth.private", sep = "")); #load rest username and password, contained in auth.private file
source(paste(localpath,"hydro-tools/VAHydro-2.0/rest_functions.R", sep = ""))
token <-trimws(rest_token(basepath, token, rest_uname, rest_pw))

#Load functions
source(paste(localpath,"USGS_Consumptive_Use/Code/ECHO to VAHydro/R_functions.R", sep = ""))


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
                WHERE CWPPermitTypeDesc = 'NPDES Individual Permit'
                OR CWPPermitTypeDesc = 'General Permit Covered Facility'"
ECHO_Facilities <- sqldf(keep_permits)

print(paste("Number of Facilities After Permit Type Description Subset: ",length(ECHO_Facilities[,1])))

#rename SourceID column to Facility_ID 
colnames(ECHO_Facilities)[colnames(ECHO_Facilities)=="SourceID"] <- "Facility_ID"
backup <- ECHO_Facilities

#GET EPA ADMINREG FEATURE FROM VAHYDRO
agency_inputs <- list(bundle = 'authority',ftype = 'federal_enviro_agency',admincode = 'epa',stringsAsFactors = FALSE) 
agency_dataframe <- getAdminregFeature(agency_inputs, basepath, adminreg_feature)
agency_adminid <- as.character(agency_dataframe$adminid)

startDate <- '01/01/2019'
endDate <- '12/31/2019'
endDate<-format(as.Date(endDate), "%m/%d/%Y")

#i <- 1048 
#i <- 26951
# Create or retreive the Permit for each facility 
#ECHO_Facilities <- ECHO_Facilities[1:5,] # JM uses: 13465:13470 # 8034:8040 misc Dominion energy
permit_dataframe <- NULL
facility_dataframe <- NULL
for (i in 1:(length(ECHO_Facilities[,1]))){
  ECHO_Facilities_i <- ECHO_Facilities[i,]
  
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
    facility <- getFeature(list(hydroid = ECHO_Facilities_i$matched_hydroid), token, basepath)
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
  
  
  # print("PROCESSING FACILITY PROPERTIES")
  # facility_properties <- ECHO_properties_REST(ECHO_Facilities_i,facility,token,basepath)
  
  #-Waterbody Name (GNIS)
  #-Combined Sewer System Flag (CWPCsoFlag)
  #-Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)
  #-Impairment Class or Category of the Waterbody (impair_cause)
  #-Date of most recent inspection of the facility (last_inspect)
  #-Unique ID for Waterbody (reachcode_rad)
  #-Facility Design Flow in MGD (design_flow)
  
  
  print("PROCESSING OUTFALLS")
  # #echor package has 2 functions for pulling effluent data echoGetEffluent() and downloadDMRs(). However, the url being used to download is not working causing these functions to fail. Manualing pulling from the rest_services url does work. 
  # effluent_data <- echoGetEffluent(p_id = 'VA0089133',  parameter_code = '50050')
  # 
  # df <- tibble::tibble("permit" = c('VA0089133'))
  # df <- downloadDMRs(df, permit)
  print(paste("PROCESSING DMR DATA FOR FACILITY ",i," OF ",length(ECHO_Facilities[,1]),sep=""))
  DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",ECHO_Facilities_i$Facility_ID,"&parameter_code=50050&start_date=",startDate,"&end_date=",endDate) 
#CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe # 50050 only looks at Flow, in conduit ot thru treatment plant - there are 347 parameter codes defined in ECHO
  DMR_data<-read.csv(DMR_data,sep = ",", stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
  if (as.integer(nrow(DMR_data)) > 0) {
    outfalls <- outfall_features_REST(DMR_data, facility, token, basepath)
  }
}

#---------Retrieve Design Flows and Outfall Coordinates in VPDES Database---------#

df_coord_pull<- function(){
  
  # Individual Permits updated as of October 2018---contains design flow for facilities
  # Warnings about unknown or uninitiliased columns: previous IP contact sheets named the columns differently. 
  # It doesn't hinder any processes though. 
  
  GET('https://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contact%20Flow%20for%20WEB%20Jan%202019.xlsx?ver=2019-01-23-151510-490', 
      write_disk(temp <- tempfile(fileext = ".xlsx")))
  VPDES_IP <- read_excel(temp)
  VPDES_IP<-VPDES_IP[!is.na(VPDES_IP$Facility),]
  VPDES_IP$`Design Flow (MGD)`<-as.numeric(VPDES_IP$`Design Flow (MGD)`)
  VPDES_IP<-VPDES_IP[!duplicated(VPDES_IP$`Permit Number`),] #getting rid of duplicates and looking at unique permits
  VPDES_DesignFlow<-VPDES_IP[c("Permit Number", "Design Flow (MGD)")]
  colnames(VPDES_DesignFlow)<-c("Facility_ID","DesignFlow_mgd")
  
   ECHO_Facilities <- sqldf(
    " select a.*, b.DesignFlow_mgd 
      from ECHO_Facilities as a 
      left outer join VPDES_DesignFlow as b 
      on (a.Facility_ID = b.Facility_ID)
    "
  )
  #----------Seperate Design Flow as a Facility Property---------------#
  design_flow<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='design_flow', propname='design_flow', 
                          propvalue=ECHO_Facilities$DesignFlow_mgd, propcode=ifelse(ECHO_Facilities$DesignFlow_mgd==0,"fac_flag_zerodesflow",NA), stringsAsFactors = F)
  
  #----------Retrieve coordinates of outfalls-----------------#
  #Use Aggregated Flows generated from ECHOInterface Script and list of outfalls for creating release and conveyance points.
  temp<-tempfile(fileext = ".zip")
  #Locations and attribute data about active outfalls in the State
  download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip", destfile = temp)
  unzip(temp)
  #Explore what is in VPDES_Geodatabase.gdb
  ogrListLayers("VPDES_Geodatabase.gdb") #Two layers: VPDES Outfalls and OpenFileGDB
  VPDES_Outfalls<-as.data.frame(readOGR("VPDES_Geodatabase.gdb",layer="VPDES_OUTFALLS"))
  names(VPDES_Outfalls)[names(VPDES_Outfalls)=="OUTFALL_ID"]<-'OutfallID'
  names(VPDES_Outfalls)[names(VPDES_Outfalls)=="VAP_PMT_NO"]<-'Facility_ID'
  names(ECHO_Facilities)[names(ECHO_Facilities)=="SourceID"]<-"Facility_ID"#Need to rename to give a central columnn name for future joins
  
  VPDES_Coordinates<-VPDES_Outfalls[,c(15,16)]
  VPDES_Coordinates <- proj4::project(VPDES_Coordinates, proj="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", inverse=TRUE)
  
  #Replace coordinates in VPDES_Outfalls data frame
  VPDES_Outfalls$Longitude<-VPDES_Coordinates$x
  VPDES_Outfalls$Latitude<-VPDES_Coordinates$y
  
  assign("design_flow",design_flow,envir = .GlobalEnv)
  assign("VPDES_Outfalls",VPDES_Outfalls,envir = .GlobalEnv)
  assign("ECHO_Facilities",ECHO_Facilities,envir = .GlobalEnv)
  
}
df_coord_pull()

##################################################################################################################################
################################################Imports###########################################################################

#1 Import Outfall Timeseries Data
timeseries <- ts_ECHO_pull(ECHO_Facilities,1, startDate, endDate)
#write.table(timeseries,file="timeseries.txt", sep='\t', row.names = F)

save.image(file="timeseries_2010_present.RData")


#------------------Timeseries Flags-------------------#

ts_flagging<- function(timeseries){
  
  #-----------------------------------------------------------------#
  #-------ECHO Measured Effluents > VPDES Design flow---------------#
  
  #Flag facilities that report measured effluent greater than the design flow 
  df<-subset(design_flow,select=c(1,4))
  colnames(df)<-c("Facility_ID","DesignFlow_mgd")
  df$Facility_ID<-gsub("echo_","", as.character(df$Facility_ID))
  timeseries <- sqldf(
    " select a.*, b.DesignFlow_mgd,
        CASE WHEN ( (b.DesignFlow_mgd < a.tsvalue) and (b.DesignFlow_mgd > 0) ) THEN 'dmr_flag_desflow'
        ELSE NULL
        END as dmr_flag_desflow
      from timeseries as a 
      left outer join df as b 
      on (
        a.Facility_ID = b.Facility_ID
      )
    "
  ) 
  #-----------------------------------------------------------------#
  #------Measured Effluents > 100*Median Measured Effluent----------#
  #------Measured Effluents > 100,000*Median Measured Effluent------#
  #---------------Potential Unit Conversion Error-------------------#
  
  #Summmarize measured effluent values from ECHO by OutfallID--Not by Facility#
  timeseries_summary<-timeseries%>%
    dplyr::group_by(hydrocode,Year=substr(tstime,1,4))%>% #important to note that we are summarizing discharge by outfall here 
    dplyr::summarise(Median_ME=median(tsvalue, na.rm = T))
  
  timeseries<-timeseries%>%add_column(Year=substr(timeseries$tstime,1,4), .before="tstime")
  timeseries<-merge(timeseries,timeseries_summary,by=c("hydrocode","Year"),all.x=T)
  
  timeseries$dmr_flag_units_100<-ifelse(timeseries$tsvalue>100*timeseries$Median_ME,"dmr_flag_units_100",NA)
  
  # Add flag for Reston Lake AC 
  timeseries$dmr_flag_units_100[timeseries$hydrocode=="echo_VA0091995"&timeseries$tsendtime=="2017-07-01"]<-"dmr_flag_units_100"
  timeseries$dmr_flag_units_100[timeseries$hydrocode=="echo_VA0091995"&timeseries$tsendtime=="2017-08-01"]<-"dmr_flag_units_100"
  timeseries$dmr_flag_units_100[timeseries$hydrocode=="echo_VA0091995"&timeseries$tsendtime=="2017-09-01"]<-"dmr_flag_units_100"
 
  timeseries$dmr_flag_units_1000000<-ifelse(timeseries$tsvalue>1000000*timeseries$Median_ME,"dmr_flag_units_1000000",NA)
  
  
  assign("timeseries",timeseries,envir = .GlobalEnv)
  
}
ts_flagging(timeseries)


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

##################################################################################################################################
###########################################Pushing DMR timeseries Data to VAHydro#################################################

# site <- "http://deq2.bse.vt.edu/d.alpha"    #Specify the site of interest, either d.bet OR d.dh
# hydro_tools <- 'C:\\Users\\maf95834\\Documents\\Github\\hydro-tools' #location of hydro-tools repo
# 
# #----------------------------------------------#
# 
# #Generate REST token for authentication              
# rest_uname = FALSE
# rest_pw = FALSE
# source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
# source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
# token <-trimws(rest_token(site, token, rest_uname, rest_pw))

############################################################################################
# RETRIEVE EPA AGENCY ADMINREG FEATURE
############################################################################################

# agency_inputs <- list(bundle = 'authority',ftype = 'federal_enviro_agency',admincode = 'epa',stringsAsFactors = FALSE) 
# agency.dataframe <- getAdminregFeature(agency_inputs, site, adminreg_feature)
# agency.adminid <- as.character(agency.dataframe$adminid)

############################################################################################
# RETRIEVE/CREATE/UPDATE PERMIT ADMINREG FEATURE
############################################################################################  
# this function permit_import() has been replaced by permit_ECHO, but I keep it here in case it does something Other than
# to simply re-retrieve the list of permits for facilities in order to stash in a text file
#permit_dataframe <- permit_import(ECHO_Facilities,agency_adminid,1)
#write.table(permit_dataframe,"permit_dataframe.txt",sep="\t",row.names = F)


#save.image(file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/afterpermitimport_2factest.RData")

############################################################################################
# RETRIEVE/CREATE/UPDATE TIMESERIES
############################################################################################  

timeseries.dataframe <- ts_import(outfalls,timeseries,1)
#write.table(timeseries.dataframe,file="timeseries.dataframe.txt",sep="\t",row.names=F)
