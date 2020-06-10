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
  'CWPEffectiveDate','CWPExpirationDate'
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

startDate <- '01/01/2010'
endDate<-Sys.Date()
endDate<-format(as.Date(endDate), "%m/%d/%Y")

#i <- 1048
#i <- 26951
ECHO_Facilities <- ECHO_Facilities[1:5,]
for (i in 1:(length(ECHO_Facilities[,1]))){
  ECHO_Facilities_i <- ECHO_Facilities[i,]
  
  print(paste("PROCESSING PERMIT ",i," OF ",length(ECHO_Facilities[,1]),sep=""))
  permit <- permit_REST(ECHO_Facilities_i, agency_adminid)
  print(permit)
  
  print("PROCESSING FACILITY")
  facility <- facility_REST(ECHO_Facilities_i, permit, token)
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
    outfall <- outfall_features_REST(DMR_data, facility, token, basepath)
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

ts_ECHO_pull<- function(ECHO_Facilities,iteration, startDate="01/01/2010",endDate=NULL){
  #mm/dd/yyyy: data on ECHO is limited to 2012 for most sites or 2009 for a few
  if (is.null(endDate)) {
    endDate<-Sys.Date()
    endDate<-format(as.Date(endDate), "%m/%d/%Y")
  }
  options(scipen=999) #Disable scientific notation
  options(digits = 9)
  
  #Create Place Holders for Desired Variables
  hydrocode<-character() # unique ID for facility 
  varkey<-character() # reporting statistic of flow--most likely monthly average
  tsvalue<-numeric() #measured effluent through outfall 
  tstime<-character() #beginning date of monitoring period 
  tsendtime<-character() #end date of monitoring period 
  tscode<-numeric() #the number of months including in the monitoring period
  outfallID<-character() #Unique ID used in Virginia for a facility's outfall: concatonated facility ID with 3 digit outfall ID
  nodi<-character() #if the DMR value is NA, the no data indicator code describes why that is the case
  violation<-character() #Code identifying if a Violation has occurred  (e.g., D80 = Required Monitoring DMR Value Non-Receipt, E90 = Effluent Violation, C20 = Schedule Event Achieved Late).
  violation_severity<-numeric() #Severity of any alleged violation caused by the reported value: 5 = significant noncompliance; 3 = reportable noncompliance; 2 = effluent violation, i.e., discharge in excess of permitted limit; 1 = monitoring or reporting violation; 0 = no violation.
  
  #This loop goes through each CWA regulated facility one by one to extract reported discharges 
  #from each unique outfall. In the end, there will be ECHO_Facilities table with timeseries data for each
  #outfall located in VA. 
  for (i in iteration:length(ECHO_Facilities$Facility_ID)){
    
    Facility_ID<-ECHO_Facilities$Facility_ID[i]
    print(paste("Processing Facility ID: ", Facility_ID, "(",i," of ",length(ECHO_Facilities$Facility_ID),")", sep=""))
    
    DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",Facility_ID,"&parameter_code=50050&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe # 50050 only looks at Flow, in conduit ot thru treatment plant - there are 347 parameter codes defined in ECHO
    DMR_data<-read.csv(DMR_data,sep = ",", stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
    
    DMR_data$dmr_value_nmbr[DMR_data$nodi_code %in% c('C','7')]<-0#nodi_code is the unique code indicating the reason why an expected DMR value was not submitted. C=No Discharge, B=Below Detection Limit, 9=Conditional Monitoring, 7=parameter/value not reported
    data_length<-length(unique(DMR_data$monitoring_period_end_date))#sees if there is any reported data worth extracting and examining
    if(data_length>0){ #if the value is NOT NA, enter loop
      outfall_nmbr<-as.character(unique(DMR_data$perm_feature_nmbr)) #Stores Outfalls which are called permanent features in the DMR
      outfall_ID<-unique(DMR_data$perm_feature_nmbr) #perm_feature_nmbr is a three-character code in ICIS-NPDES that identifies the point of discharge
      for(j in 1:length(outfall_ID)){ #If the code is less than three characters in the .CSV, append zeros to the beginning of the number (e.g., 1 is equivalent to 001)
        if(nchar(as.character(outfall_ID[j]), type="chars")<3){
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
            varkey_i[l]<-"dmr_period_mgd"
            nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="MK"][l] 
            violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="MK"][l]
            violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="MK"][l]
            
          }else if(!is.na(outfall_DMR$statistical_base_code[l]=="3C")){ #30 day average
            tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="3C"])[l] 
            tsendtime_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="3C"][l] #character class
            tscode_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="3C"])[l]
            tstime_i[l]<-as.character(round_date(mdy(tsendtime_i[l]) %m-% months(tscode_i[l]),unit="month"))#uses Lubridate package, date must be object of class POSIXlt, POSIXct, or Date
            varkey_i[l]<-"dmr_period_mgd"
            nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="3C"][l] 
            violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="3C"][l]
            violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="3C"][l]
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
        outfallID<-c(outfallID,paste0(Facility_ID,rep(outfall,length((tsvalue_i)))))
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
  
  timeseries$Facility_ID<-gsub("echo_","", as.character(timeseries$hydrocode))
  timeseries$Facility_ID<-substr(timeseries$Facility_ID,1,9)
  
  assign("timeseries",timeseries,envir = .GlobalEnv)
  
  
  
  write.table(timeseries,file="timeseries.txt", sep='\t', row.names = F)
}
ts_ECHO_pull(ECHO_Facilities,1)

save.image(file="timeseries_2010_present.RData")


#------------------Timeseries Flags-------------------#

ts_flagging<- function(timeseries){
  
  #-----------------------------------------------------------------#
  #-------ECHO Measured Effluents > VPDES Design flow---------------#
  
  #Flag facilities that report measured effluent greater than the design flow 
  df<-subset(design_flow,select=c(1,4))
  colnames(df)<-c("Facility_ID","DesignFlow_mgd")
  df$Facility_ID<-gsub("echo_","", as.character(df$Facility_ID))
  timeseries<-merge(timeseries,df,by="Facility_ID",all.x=T)
  timeseries$dmr_flag_desflow<-ifelse(timeseries$tsvalue>timeseries$DesignFlow_mgd & timeseries$DesignFlow_mgd>0,"dmr_flag_desflow",NA)
  
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


##################################################################################################################################
###########################################3 Release Point Generation#############################################################

# Generation of the release point imports. In essence, this portion just formats various data from both the facility
# and the outfall list (VPDES_Outfalls and timeseries) to create the release point attributes and geometry.

release_generation<- function(ECHO_Facilities,timeseries){
  
  #---------Assign Coordinates to Outfalls that have DMRs from 2010-Present----------#
  Facility_Coord<-subset(ECHO_Facilities,select=c(1,11,12)) #Isolate Facility Level Coordinates
  timeseries$OutfallID<-gsub("echo_","",timeseries$hydrocode)
  
  ECHO_Outfalls<-sqldf("select OutfallID, Facility_ID from timeseries group by OutfallID, Facility_ID")
  ECHO_Outfalls<-sqldf(
    "select a.*, 
     CASE 
       WHEN b.Latitude is NULL THEN c.FacLat
       ELSE b.Latitude
     END as Latitude,  
     CASE 
       WHEN b.Longitude is NULL THEN c.FacLong
       ELSE b.Longitude
     END as Longitude 
     from ECHO_Outfalls as a 
     left outer join VPDES_Outfalls as b 
     on (
       a.OutfallID = b.OutfallID
     )
     left outer join ECHO_Facilities as c 
     on (
       Facility_ID = Facility_ID
     )
    "
  )
  
  releasepoint<-data.frame(bundle=rep('transfer',length(ECHO_Outfalls$OutfallID)),
                           name=paste0('TO ',ECHO_Outfalls$OutfallID),
                           ftype=rep('release',length(ECHO_Outfalls$OutfallID)),
                           hydrocode=paste0('vahydro_',ECHO_Outfalls$OutfallID),
                           fstatus=ifelse(ECHO_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                           dh_link_facility_mps=paste0('echo_',ECHO_Outfalls$Facility_ID),
                           stringsAsFactors = F)
  
  for (i in 1:length(releasepoint$bundle)){
    print(paste("Processing Release Point ",i," of ", length(releasepoint$hydrocode)))
    if(!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]])){
      releasepoint$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]],')')
    } else {
      lat<-ECHO_Outfalls$Latitude[ECHO_Outfalls$Facility_ID==ECHO_Outfalls$Facility_ID[i]]
      long<-ECHO_Outfalls$Longitude[ECHO_Outfalls$Facility_ID==ECHO_Outfalls$Facility_ID[i]]
      for (i in 1:length(lat)){
        if(!is.na(lat[i]) & !is.na(long[i])){
          releasepoint$dh_geofield[i]<-paste0('POINT (',long[i],' ',lat[i],')')
          break
        } else {
          releasepoint$dh_geofield[i]<-'NULL'
        }
      }
    }
  } #bracket for line 628 for loop
  
  assign("releasepoint",releasepoint,envir = .GlobalEnv)
  assign("ECHO_Outfalls",ECHO_Outfalls,envir = .GlobalEnv)
  write.table(releasepoint,"releasepoint.txt",sep="\t",row.names = F)

} 
release_generation(ECHO_Facilities,timeseries)

##################################################################################################################################
###########################################4 Conveyance and Outfall Generation####################################################

# Generates the conveyance import using the outfall list in 'ECHO_Outfalls' which is dependent on reporting outfalls in timeseries
# Important to save ECHO_Outfalls from release_generation function

conveyance_generation<- function(ECHO_Outfalls){

conveyance<-data.frame(bundle=rep('conveyance',length(ECHO_Outfalls$OutfallID)),
                       name=paste0(ECHO_Outfalls$Facility_ID,' TO ',ECHO_Outfalls$OutfallID),
                       ftype="water_transfer",
                       hydrocode=paste0('vahydro_',ECHO_Outfalls$Facility_ID,'_',ECHO_Outfalls$OutfallID),
                       fstatus=ifelse(ECHO_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                       field_dh_from_entity=paste0('vahydro_',ECHO_Outfalls$OutfallID),
                       field_dh_to_entity=paste0('echo_',ECHO_Outfalls$OutfallID),
                       stringsAsFactors = F)



#Outfalls Generation
#Reformats 'ECHO_Outfalls' using available VPDES or ECHO geometry data and ECHO attributes
outfalls<-data.frame(bundle=rep('transfer',length(ECHO_Outfalls$OutfallID)),
                     name=paste0('FROM ',ECHO_Outfalls$Facility_ID),
                     ftype='outfall',
                     hydrocode=paste0('echo_',ECHO_Outfalls$OutfallID),
                     fstatus=ifelse(ECHO_Outfalls$OutfallID%in%gsub("echo_","", as.character(timeseries$hydrocode)),'active','inactive'),
                     dh_link_facility_mps=paste0('echo_',ECHO_Outfalls$Facility_ID),
                     stringsAsFactors = F)

for (i in 1:length(outfalls$bundle)){
  print(paste("Processing Outfall ",i," of ", length(outfalls$hydrocode)))
  if(!is.na(ECHO_Outfalls$Latitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]]) & !is.na(ECHO_Outfalls$Longitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]])){
    outfalls$dh_geofield[i]<-paste0('POINT (',ECHO_Outfalls$Longitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]],' ',ECHO_Outfalls$Latitude[ECHO_Outfalls$OutfallID==ECHO_Outfalls$OutfallID[i]],')')  
  } else if (!is.na(ECHO_Facilities$FacLat[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]]) & !is.na(ECHO_Facilities$FacLong[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]])) {
    outfalls$dh_geofield[i]<-paste0('POINT (',ECHO_Facilities$FacLong[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]],' ',ECHO_Facilities$FacLat[ECHO_Facilities$Facility_ID==ECHO_Outfalls$Facility_ID[i]])
  } else {
    outfalls$dh_geofield[i]<-'NULL'
  }
}


assign("conveyance",conveyance,envir = .GlobalEnv)
assign("outfalls",outfalls,envir = .GlobalEnv)
write.table(conveyance,paste0(Outputpath,"/conveyance.txt"),sep="\t",row.names = F)
write.table(outfalls,paste0(Outputpath,"/outfalls.txt"),sep="\t",row.names = F)

}
conveyance_generation(ECHO_Outfalls)


outfall_properties<- function(){
  outfall_types<-read.table(file="C:/Users/maf95834/Documents/Github/USGS_Consumptive_Use/Code/Data Cleaning/Outfall_Types.txt",header=T,sep="|")
  
  
  cooling<-subset(outfall_types,outfall_types$Outfall_Type=="COOL")
  cooling<-data.frame(hydrocode=paste0("echo_",cooling$OutfallID),varkey="vpdes_outfall_type",
                          propname="vpdes_outfall_type",propvalue="",
                          proptext="Power station cooling water outfall",
                          propcode="cooling",stringsAsFactors = F)
  
  stormwater<-subset(outfall_types,outfall_types$Outfall_Type=="STORM")
  stormwater<-data.frame(hydrocode=paste0("echo_",stormwater$OutfallID),varkey="vpdes_outfall_type",
                          propname="vpdes_outfall_type",propvalue="",
                          proptext="Outfall that tracks stormwater discharge through municipal separate storm sewer systems (MS4)",
                          propcode="stormwater",stringsAsFactors = F)
  
  internal<-subset(outfall_types,outfall_types$Outfall_Type=="INO")
  internal<-data.frame(hydrocode=paste0("echo_",internal$OutfallID),varkey="vpdes_outfall_type",
                       propname="vpdes_outfall_type",propvalue="",
                       proptext="Outfall that monitors waste streams within a facility before being discharged.",
                       propcode="internal",stringsAsFactors = F)
  
  internal_sum<-subset(outfall_types,outfall_types$Outfall_Type=="INO_SUM")
  internal_sum<-data.frame(hydrocode=paste0("echo_",internal_sum$OutfallID),varkey="vpdes_outfall_type",
                        propname="vpdes_outfall_type",propvalue="",
                        proptext="Outfall that monitors the cumulative waste streams within a facility before being discharged.",
                        propcode="internal_sum",stringsAsFactors = F)
  
  outfall_props<<-rbind(cooling,stormwater,internal,internal_sum)
  assign("outfall_props",outfall_props,envir = .GlobalEnv)
  }
outfall_properties()
##################################################################################################################################
###########################################5 Facility Properties Generation####################################################

#Facility dH Property Mapping

#hydrocode, varkey, propname, propvalue, proptext, propcode, startdate, enddate

#Facility Metadata/Properties 
    #-Waterbody Name (GNIS)
    #-Combined Sewer System Flag (CWPCsoFlag)
    #-Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)
    #-Impairment Class or Category of the Waterbody (impair_cause)
    #-Date of most recent inspection of the facility (last_inspect)
    #-Unique ID for Waterbody (reachcode_rad)
    #-Facility Design Flow in MGD (design_flow)

facility_properties<- function(ECHO_Facilities){

last_inspect<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='last_inspect', propname='last_inspect',
                         propvalue='',proptext='',propcode='',startdate=ECHO_Facilities$CWPDateLastInspection,enddate='',stringsAsFactors = F)
#write.table(last_inspect,paste0(Outputpath,"/last_inspect.txt"),sep="\t",row.names = F)

css<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='css', propname='css', 
                propvalue='',proptext='',propcode=ECHO_Facilities$CWPCsoFlag, startdate='',enddate='',stringsAsFactors = F)
#write.table(css,paste0(Outputpath,"/css.txt"),sep="\t",row.names = F)

cwp_cso_outfalls<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='cwp_cso_outfalls', propname='cwp_cso_outfalls', 
                             propvalue=ECHO_Facilities$CWPCsoOutfalls,proptext='',propcode='', startdate='',enddate='',stringsAsFactors = F)
#write.table(cwp_cso_outfalls,paste0(Outputpath,"/cwp_cso_outfalls.txt"),sep="\t",row.names = F)

wb_gnis_name<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='wb_gnis_name', propname='wb_gnis_name', 
                         propvalue='', proptext='',propcode=ECHO_Facilities$RadGnisName, startdate='',enddate='',stringsAsFactors = F)
#write.table(wb_gnis_name,paste0(Outputpath,"/wb_gnis_name.txt"),sep="\t",row.names = F)

reachcode_rad<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='reachcode_rad', propname='reachcode_rad', 
                          propvalue='', proptext='',propcode=ECHO_Facilities$RadReachcode, startdate='',enddate='',stringsAsFactors = F)
#write.table(reachcode_rad,paste0(Outputpath,"/reachcode_rad.txt"),sep="\t",row.names = F)

impair_cause<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='impair_cause', propname='impair_cause', 
                         propvalue='', proptext=ECHO_Facilities$AttainsStateCauses,propcode='', startdate='',enddate='',stringsAsFactors = F)
#write.table(impair_cause,paste0(Outputpath,"/impair_cause.txt"),sep="\t",row.names = F)

design_flow<<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$Facility_ID), varkey='design_flow', propname='design_flow', 
                        propvalue=ECHO_Facilities$DesignFlow_mgd, propcode=ifelse(ECHO_Facilities$DesignFlow_mgd==0,"fac_flag_zerodesflow",NA), stringsAsFactors = F)


write.table(design_flow,paste0(Outputpath,"/design_flow.txt"),sep="\t",row.names = F)

}
facility_properties(ECHO_Facilities)

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

permit_import<- function(adminreg,iteration){ 

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

for (i in iteration:length(permit_inputs$admincode)){
  print(paste("Processing Permit ",i, "(ID:",permit_inputs$admincode[i],")"," of ", length(permit_inputs$admincode))) #track iterations
  
  permit.dataframe_i <- getAdminregFeature(permit_inputs[i,], site, adminreg_feature)
  if(permit.dataframe_i[1]==FALSE){ #if the features exists in VAHydro, it will not create it
    permit.dataframe_ii <- postAdminregFeature(permit_inputs[i,], site, adminreg_feature) #status 201 if feature created succesfully
    print(permit.dataframe_ii) #print status of updating: error 403 means administration block
    permit.dataframe_i <- getAdminregFeature(permit_inputs[i,], site, adminreg_feature) #grab info that has just been imported into VAHydro
  } else {
    print("This Adminreg Feature already exists")
  }
  permit.dataframe<-rbind(permit.dataframe,permit.dataframe_i) #creating this to keep adminids
}

#unique adminid's for each permit. Used to link facilities to permits
permit.adminid<-data.frame(admincode=permit.dataframe$admincode,adminid=permit.dataframe$adminid) 

assign("permit.dataframe",permit.dataframe,envir = .GlobalEnv)
assign("permit.adminid",permit.adminid,envir = .GlobalEnv)

write.table(permit.dataframe,paste0(Outputpath,"/permit.dataframe.txt"),sep="\t",row.names = F)

}

permit_import(adminreg,1)


#save.image(file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/afterpermitimport_2factest.RData")

############################################################################################
# RETRIEVE/CREATE/UPDATE FACILITY DH FEATURE
############################################################################################  

#--------Facilities with a match in VAHydro--------------#
# Switch out hydrocodes for the facilities we know are already in VAHydro with a withdrawing permit.
# This is a manual step that requires matching of facilities. 

matched_switch<- function(facilities){
  
  facility.dataframe <- data.frame()
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
  
  # We keep the same NPDES permit dh_link 
  Matched<-read.csv("https://raw.githubusercontent.com/HARPgroup/USGS_Consumptive_Use/master/Code/Facility%20Matching/Runninglist_Matches.csv",sep=",", header = T, stringsAsFactors = F)
  Matched$VWUDS.Hydrocode<-gsub("\\s+$","",Matched$VWUDS.Hydrocode)
  
  #--Retrieve attributes of corresponding matched facility in VAHydro with getFeature
  facility_inputs<-subset(facility_inputs,facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,select=c(1,3))
  facility_inputs$hydrocode<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$hydrocode)
  facility_inputs$hydrocode<-gsub("\\s+$","",facility_inputs$hydrocode) #trim any trailing whitespaces--very important
  
  for (i in 1:length(facility_inputs$hydrocode)){
    print(paste("Processing Facility ",i, "(",facility_inputs$hydrocode[i],")","  of ", length(facility_inputs$hydrocode)))
    facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature) #need token now for access
    facility.dataframe<-rbind(facility.dataframe,facility.dataframe_i)
  }
  
  facility.dataframe<-subset(facility.dataframe,select=c(3,4,6,7,9,10,11,20,23))
  colnames(facility.dataframe)[2]<-c("VWUDS.Hydrocode")
  Matched<-merge(Matched,facility.dataframe,by=c("VWUDS.Hydrocode"))
  Matched<-mutate_if(Matched,is.factor,as.character)
  Matched$VWUDS.Name<-gsub(".*: ","",Matched$VWUDS.Name)
  
  facility_inputs <- data.frame(
    bundle = as.character(facilities$bundle),
    ftype = as.character(facilities$ftype),
    hydrocode = as.character(facilities$hydrocode),
    name = as.character(facilities$name),
    fstatus = as.character(facilities$fstatus),
    address1 = as.character(facilities$address1),
    city = as.character(facilities$city),
    dh_link_admin_location = as.character(permit.adminid$adminid[match(permit.adminid$admincode,facilities$dh_link_admin_location)]), #%in% operator looks for matches in left operand and returns permit.adminid if there is one
    dh_geofield = as.character(facilities$dh_geofield),
    stringsAsFactors=FALSE
  ) 
  
  #--If there is a match, there is no need to create a new facility feature. Therefore replace ECHO/VPDES hydrocode with the VWUDS hydrocode and other attributes--#
  facility_inputs$ftype<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$ftype[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$ftype)
  facility_inputs$name<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Name[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$name)
  facility_inputs$fstatus<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$fstatus[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$fstatus)
  facility_inputs$address1<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$address1[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$address1)
  facility_inputs$city<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$city[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$city)
  facility_inputs$dh_geofield<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$dh_geofield[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$dh_geofield)
  
  #--Replace VPDES hydrocode with matching VWUDS hydrocode to attach properties to existing facilities in VAHydro--#
  wb_gnis_name$hydrocode<-ifelse(wb_gnis_name$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(wb_gnis_name$hydrocode,Matched$VPDES.Hydrocode)],wb_gnis_name$hydrocode)
  css$hydrocode<-ifelse(css$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(css$hydrocode,Matched$VPDES.Hydrocode)],css$hydrocode)
  cwp_cso_outfalls$hydrocode<-ifelse(cwp_cso_outfalls$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(cwp_cso_outfalls$hydrocode,Matched$VPDES.Hydrocode)],cwp_cso_outfalls$hydrocode)
  design_flow$hydrocode<-ifelse(design_flow$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(design_flow$hydrocode,Matched$VPDES.Hydrocode)],design_flow$hydrocode)
  impair_cause$hydrocode<-ifelse(impair_cause$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(impair_cause$hydrocode,Matched$VPDES.Hydrocode)],impair_cause$hydrocode)
  last_inspect$hydrocode<-ifelse(last_inspect$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(last_inspect$hydrocode,Matched$VPDES.Hydrocode)],last_inspect$hydrocode)
  reachcode_rad$hydrocode<-ifelse(reachcode_rad$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(reachcode_rad$hydrocode,Matched$VPDES.Hydrocode)],reachcode_rad$hydrocode)
  
  releasepoint$dh_link_facility_mps<-ifelse(releasepoint$dh_link_facility_mps%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(releasepoint$dh_link_facility_mps,Matched$VPDES.Hydrocode)],releasepoint$dh_link_facility_mps)
  outfalls$dh_link_facility_mps<-ifelse(outfalls$dh_link_facility_mps%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(outfalls$dh_link_facility_mps,Matched$VPDES.Hydrocode)],outfalls$dh_link_facility_mps)
  
  # Important to do this switch last 
  facility_inputs$hydrocode<-ifelse(facility_inputs$hydrocode%in%Matched$VPDES.Hydrocode,Matched$VWUDS.Hydrocode[match(facility_inputs$hydrocode,Matched$VPDES.Hydrocode)],facility_inputs$hydrocode)
  
  # assign updated facilities to global environment
  assign("releasepoint",releasepoint,envir=.GlobalEnv)
  assign("outfalls",outfalls,envir=.GlobalEnv)
  
  assign("wb_gnis_name",wb_gnis_name,envir=.GlobalEnv)
  assign("css",css,envir=.GlobalEnv)
  assign("cwp_cso_outfalls",cwp_cso_outfalls,envir=.GlobalEnv)
  assign("design_flow",design_flow,envir=.GlobalEnv)
  assign("impair_cause",impair_cause,envir=.GlobalEnv)
  assign("last_inspect",last_inspect,envir=.GlobalEnv)
  assign("reachcode_rad",reachcode_rad,envir=.GlobalEnv)
  
  assign("facility_inputs",facility_inputs,envir=.GlobalEnv)
  
}
#matched_switch(facilities)
#matched_switch(facility.test)
matched_switch(facilities)

#--------------------------------------------------------#

facility_import<- function(facility_inputs,iteration){
  old <- Sys.time() # get start time
  facility.dataframe <- data.frame()

for (i in iteration:length(facility_inputs$hydrocode)){
  print(paste("Processing Facility ",i, "(",facility_inputs$hydrocode[i],")","  of ", length(facility_inputs$dh_link_admin_location)))
  facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature) #need token now for access
  
  if(facility.dataframe_i[1]==FALSE){
  facility.dataframe_ii <- postFeature(facility_inputs[i,], site, feature)
  print(facility.dataframe_ii)
  facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature)

  }else{
    print("This Facility Feature already exists")
  }
 
  facility.dataframe<-rbind(facility.dataframe,facility.dataframe_i)
  
}

facility.hydroid<-data.frame(hydrocode=facility.dataframe$hydrocode,hydroid=facility.dataframe$hydroid)

assign("facility.dataframe",facility.dataframe,envir = .GlobalEnv)
assign("facility.hydroid",facility.hydroid,envir = .GlobalEnv)

write.table(facility.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/facility.dataframe.txt",sep="\t",row.names=F)
new <- Sys.time() - old # calculate difference
print <- Sys.time()
print(new) # print in nice format
}
facility_import(facility_inputs,1)
#start at iteration == 12576
############################################################################################
# RETRIEVE/CREATE/UPDATE FACILITY METADATA PROPERTIES
# Make sure hydrocodes are in same order as in the facility data frame as well
############################################################################################   
# Waterbody Name (GNIS): Name of waterbody from the Geographic Names Information System database where the facility is permitted to discharge directly

waterbody_import<- function(wb_gnis_name,iteration){
property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(wb_gnis_name$hydrocode)),
  featureid = facility.hydroid$hydroid[match(facility.hydroid$hydrocode,wb_gnis_name$hydrocode)],
  varkey = as.character(wb_gnis_name$varkey),
  entity_type = rep(paste0('dh_feature'),length(wb_gnis_name$hydrocode)),
  propname = as.character(wb_gnis_name$propname),
  #propvalue = rep(NA,length(wb_gnis_name$hydrocode)),
  #proptext = rep(NA,length(wb_gnis_name$hydrocode)),
  propcode = as.character(wb_gnis_name$propcode),
  #startdate = rep(NA,length(wb_gnis_name$hydrocode)),
  #enddate = rep(NA,length(wb_gnis_name$hydrocode)),
  stringsAsFactors = F
)

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(wb_gnis_name$hydrocode)))
  
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop) #"http://deq1.bse.vt.edu/d.alpha" 
  
  if(property.dataframe_i[1]==FALSE){
  property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
  print(property.dataframe_ii)
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
}

assign("wbgnis.dataframe",property.dataframe,envir=.GlobalEnv)

}
waterbody_import(wb_gnis_name,1)

############################################################################################ 
#Combined Sewer System (css): The discharge from a combined sewer system at a point prior to a treatment plant

css_import<- function(css,iteration){
property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(css$hydrocode)),
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,css$hydrocode)],
  varkey = as.character(css$varkey),
  entity_type = rep(paste0('dh_feature'),length(css$hydrocode)),
  propname = as.character(css$propname),
  #propvalue = rep(NA,length(css$hydrocode)),
  #proptext = rep(NA,length(css$hydrocode)),
  propcode = as.character(css$propcode),
  #startdate = rep(NA,length(css$hydrocode)),
  #enddate = rep(NA,length(css$hydrocode)),
  stringsAsFactors = F
)

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(css$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
}

assign("css.dataframe",property.dataframe,envir=.GlobalEnv)

}
css_import(css,1)

############################################################################################ 
#Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)

cso_outfall_import<- function(cwp_cso_outfalls,iteration){
property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(cwp_cso_outfalls$hydrocode)),
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,cwp_cso_outfalls$hydrocode)],
  varkey = as.character(cwp_cso_outfalls$varkey),
  entity_type = rep(paste0('dh_feature'),length(cwp_cso_outfalls$hydrocode)),
  propname = as.character(cwp_cso_outfalls$propname),
  propvalue = as.character(cwp_cso_outfalls$propvalue),
  #proptext = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  #propcode = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  #startdate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  #enddate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
  stringsAsFactors = F
)

prop_inputs<-subset(prop_inputs,!is.na(prop_inputs$propvalue))

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(cwp_cso_outfalls$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
}

assign("csooutfall.dataframe",property.dataframe,envir=.GlobalEnv)

}

#replace NA values in the properties dataframes with 0
cwp_cso_outfalls$propvalue <- 0

cso_outfall_import(cwp_cso_outfalls,1)

############################################################################################ 
#Impairment Class or Category of the Waterbody (impair_cause)

impair_import<- function(impair_cause,iteration){

property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(impair_cause$hydrocode)),
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,impair_cause$hydrocode)],
  varkey = as.character(impair_cause$varkey),
  entity_type = rep(paste0('dh_feature'),length(impair_cause$hydrocode)),
  propname = as.character(impair_cause$propname),
  #propvalue = rep(NA,length(impair_cause$hydrocode)),
  proptext = as.character(impair_cause$proptext),
  #propcode = rep(NA,length(impair_cause$hydrocode)),
  #startdate = rep(NA,length(impair_cause$hydrocode)),
  #enddate = rep(NA,length(impair_cause$hydrocode)),
  stringsAsFactors = F
)

prop_inputs<-subset(prop_inputs,!prop_inputs$proptext=="")

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(impair_cause$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
}

assign("impair.dataframe",property.dataframe,envir=.GlobalEnv)

}
impair_import(impair_cause,1)

############################################################################################ 
#Date of most recent inspection of the facility (last_inspect)

#last_inspect_import function does not like the last_inspect$startdate
last_inspect$startdate <- c('2018/12/20', '2018/08/29')


last_inspect_import<- function(last_inspect,iteration){
property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(last_inspect$hydrocode)),
  featureid = as.character(facility.hydroid$hydroid[match(facility.dataframe$hydrocode,last_inspect$hydrocode)]),
  varkey = as.character(last_inspect$varkey),
  entity_type = rep(paste0('dh_feature'),length(last_inspect$hydrocode)),
  propname = as.character(last_inspect$propname),
  #propvalue = rep(NA,length(last_inspect$hydrocode)),
  #proptext = rep(NA,length(last_inspect$hydrocode)),
  #propcode = as.character(last_inspect$propcode),
  startdate = as.character(last_inspect$startdate),
  #startdate = as.PosixCT(as.character(last_inspect$startdate),
  #enddate = rep(NA,length(last_inspect$hydrocode)),
  stringsAsFactors = F
)

prop_inputs<-subset(prop_inputs,!prop_inputs$startdate=="")

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(last_inspect$hydrocode)))
  
  property.dataframe_i <- getProperty(prop_inputs[1,], site, prop)
  
  if(property.dataframe_i==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
}

assign("inspect.dataframe",property.dataframe,envir = .GlobalEnv)
}
last_inspect_import(last_inspect,1)

############################################################################################ 
#Unique ID for Waterbody (reachcode_rad)

reachcode_import<- function(reachcode_rad,iteration){
property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(reachcode_rad$hydrocode)),
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,reachcode_rad$hydrocode)],
  varkey = as.character(reachcode_rad$varkey),
  entity_type = rep(paste0('dh_feature'),length(reachcode_rad$hydrocode)),
  propname = as.character(reachcode_rad$propname),
  #propvalue = rep(NA,length(reachcode_rad$hydrocode)),
  #proptext = rep(NA,length(reachcode_rad$hydrocode)),
  propcode = as.character(reachcode_rad$propcode),
  #startdate = rep(NA,length(reachcode_rad$hydrocode)),
  #enddate = rep(NA,length(reachcode_rad$hydrocode)),
  stringsAsFactors = F
)

prop_inputs<-subset(prop_inputs,!is.na(prop_inputs$propcode))

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(reachcode_rad$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==F){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
}

assign("reachcode.dataframe",property.dataframe,envir = .GlobalEnv)
}
reachcode_import(reachcode_rad,10265)

############################################################################################ 
# Facility Design Flow in MGD (design_flow)

df_import<- function(design_flow,iteration){
property.dataframe<-data.frame()

prop_inputs <-data.frame(
  bundle = rep(paste0('dh_properties'),length(design_flow$hydrocode)),
  featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,design_flow$hydrocode)],
  varkey = as.character(design_flow$varkey),
  entity_type = rep(paste0('dh_feature'),length(design_flow$hydrocode)),
  propname = as.character(design_flow$propname),
  propvalue = as.character(design_flow$propvalue[match(design_flow$hydrocode,facility.dataframe$hydrocode)]),
  #proptext = rep(NA,length(design_flow$hydrocode)),
  #propcode = rep(NA,length(design_flow$hydrocode)),
  #startdate = rep(NA,length(design_flow$hydrocode)),
  #enddate = rep(NA,length(design_flow$hydrocode)),
  stringsAsFactors = F
)

for (i in iteration:length(prop_inputs$featureid)){
  print(paste("Processing Hydrocode ",i," of ", length(design_flow$hydrocode)))
  property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  
  if(property.dataframe_i[1]==FALSE){
    property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
    print(property.dataframe_ii)
    if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
      break
    }
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
  }else{
    print("This Property already exists")
  }
  property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
}

assign("df.dataframe",property.dataframe,envir = .GlobalEnv)
}
df_import(design_flow,12331)

############################################################################################
# RETRIEVE/CREATE/UPDATE RELEASE DH FEATURE
############################################################################################  

release_import<- function(releasepoint,iteration){
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

for (i in iteration:length(release_inputs$hydrocode)){
  print(paste("Processing Hydrocode ",i," of ", length(release_inputs$hydrocode)))
  release.dataframe_i <- getFeature(release_inputs[i,], token, site, feature)
  
  if(release.dataframe_i[1]==FALSE){
  release.dataframe_ii <- postFeature(release_inputs[i,],site, feature)
  print(release.dataframe_ii)
  
  release.dataframe_i <- getFeature(release_inputs[i,], token, site, feature)
  
  }else{
    print("This Feature already exists")
  }
  release.dataframe<-rbind(release.dataframe,release.dataframe_i)
 
}

release.hydroid<-data.frame(hydrocode=release.dataframe$hydrocode, hydroid=release.dataframe$hydroid)

assign("release.dataframe",release.dataframe,env=.GlobalEnv)
assign("release.hydroid",release.hydroid,env=.GlobalEnv)

write.table(release.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/release.dataframe.txt",sep="\t",row.names=F)

}
release_import(releasepoint,1)

############################################################################################
# RETRIEVE/CREATE/UPDATE OUTFALL DH FEATURE
############################################################################################   

outfall_import<- function(outfalls,iteration){
  
  outfall.dataframe<-data.frame()
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
  
  for (i in iteration:length(outfalls$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(outfalls$hydrocode)))
    outfall.dataframe_i <- getFeature(outfall_inputs[i,], token, site, feature)
    
    if(outfall.dataframe_i[1]==FALSE){
      outfall.dataframe_ii <- postFeature(outfall_inputs[i,], site, feature)
      print(outfall.dataframe_ii)
      outfall.dataframe_i <- getFeature(outfall_inputs[i,], token, site, feature)
      
    }else{
      print("This Feature already exists")
    }
    outfall.dataframe<-rbind(outfall.dataframe,outfall.dataframe_i)
  }
  
  outfall.hydroid<-data.frame(hydrocode=outfall.dataframe$hydrocode,hydroid=outfall.dataframe$hydroid)
  
  assign("outfall.dataframe",outfall.hydroid,env=.GlobalEnv)
  assign("outfall.hydroid",outfall.hydroid,env=.GlobalEnv)
  
  write.table(outfall.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/outfall.dataframe.txt",sep="\t",row.names=F)
}
outfall_import(outfalls,1)

outfall_type_import<- function(outfall_props,iteration){
  
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(outfall_props$hydrocode)),
    featureid = outfall.dataframe$hydroid[match(outfall_props$hydrocode,outfall.dataframe$hydrocode)],
    varkey = as.character(outfall_props$varkey),
    entity_type = rep(paste0('dh_feature'),length(outfall_props$hydrocode)),
    propname = as.character(outfall_props$propname),
    proptext = as.character(outfall_props$proptext),
    propcode = as.character(outfall_props$propcode),
    stringsAsFactors = F
  )
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(prop_inputs$featureid)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop) #"http://deq1.bse.vt.edu/d.alpha" 
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
  }
  
  assign("outfall.type.dataframe",property.dataframe,envir=.GlobalEnv)
}
outfall_type_import(outfall_props,1)

############################################################################################
# RETRIEVE/CREATE/UPDATE CONVEYANCE DH FEATURE
############################################################################################  

conveyance_import<- function(conveyance,iteration){

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
    conveyance.dataframe_i <- getFeature(conveyance_inputs[i,], token, site, feature)
    
  }else{
    print("This Feature already exists")
  }
  conveyance.dataframe<- rbind(conveyance.dataframe, conveyance.dataframe_i)

}

conveyance.hydroid<-data.frame(hydrocode=conveyance.dataframe$hydrocode,hydroid=conveyance.dataframe$hydroid)

assign("conveyance.dataframe",conveyance.dataframe,env=.GlobalEnv)
assign("conveyance.hydroid",conveyance.hydroid,env=.GlobalEnv)

write.table(conveyance.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/conveyance.dataframe.txt",sep="\t",row.names=F)

}
conveyance_import(conveyance,1)

############################################################################################
# RETRIEVE/CREATE/UPDATE TIMESERIES
############################################################################################  

ts_import<- function(timeseries,iteration){
timeseries.tid<-character()
timeseries.dataframe<-data.frame()
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
ts_inputs$tsvalue[ts_inputs$tsvalue==""]<-NA
ts_inputs<-subset(ts_inputs,!is.na(ts_inputs$tsvalue))

for (i in iteration:length(ts_inputs$featureid)){
  print(paste("Processing DMR Entry ",i," of ", length(ts_inputs$featureid)))
  timeseries.dataframe_i <- getTimeseries(ts_inputs[i,1:7], site, ts)
  
  if(timeseries.dataframe_i[1]==FALSE){
  timeseries.dataframe_ii <- postTimeseries(ts_inputs[i,1:7],site,ts)
  print(timeseries.dataframe_ii)
  
    timeseries.dataframe_i<-getTimeseries(ts_inputs[i,1:7], site, ts)
    
  }else{
    print("This timeseries Feature already exists")
  }

  timeseries.dataframe<-rbind(timeseries.dataframe,timeseries.dataframe_i)

  if(ts_inputs$dmr_flag_desflow[i]=="dmr_flag_desflow"){
    flag_inputs_dmr_flag_desflow_i <-data.frame(
      featureid_dmr_flag_desflow = as.character(timeseries.dataframe_i$tid),
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
      featureid_dmr_flag_units_100 = as.character(timeseries.dataframe_i$tid),
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
      featureid_dmr_flag_units_1000000 = as.character(timeseries.dataframe_i$tid),
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
      featureid_echo_flag = as.character(timeseries.dataframe_i$tid),
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

assign("timeseries.dataframe",timeseries.dataframe,envir=.GlobalEnv)
assign("flag_inputs_echo_flag",flag_inputs_echo_flag,envir=.GlobalEnv)
assign("flag_inputs_1000000",flag_inputs_1000000,envir=.GlobalEnv)
assign("flag_inputs_dmr_flag_units_100",flag_inputs_dmr_flag_units_100,envir=.GlobalEnv)
assign("No dmr_flag_desflow flag",flag_inputs_dmr_flag_units_100,envir=.GlobalEnv)

write.table(timeseries.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/timeseries.dataframe.txt",sep="\t",row.names=F)

}
ts_import(timeseries,1)

############################################################################################
# CREATE/UPDATE FLAGGING PROPERTIES OF TIMESERIES
############################################################################################   

tsflag_import<- function(flag_dataframe,iteration){
  
  flag.dataframe_ii<-data.frame()
  
  for (i in iteration:length(flag_dataframe$featureid)){
    print(paste("Processing Property ",i," of ", length(flag_dataframe$featureid)))
    flag.dataframe_i <- getProperty(flag_dataframe[i,], site, prop)
    if(flag.dataframe_i[1]==FALSE){
      flag.dataframe_ii <- postProperty(flag_dataframe[i,], fxn_locations, site, prop)
      print(flag.dataframe_ii)
      if(flag.dataframe_ii=="Status 200, Error: Property Not Created Successfully"){
        break
      }
    }else{
      print("Flag Already Exists")
    }
    
  }
  
}

for(i in 1:length(flag_inputs_echo_flag$featureid)){
  if(flag_inputs_echo_flag$propcode[i]=="E90"){
    flag_inputs_echo_flag$proptext[i]<-"Effluent Violation"
  }else if(flag_inputs_echo_flag$propcode[i]=="D90"){
    flag_inputs_echo_flag$proptext[i]<-"DMR Overdue, with a numeric limit"
  }else if(flag_inputs_echo_flag$propcode[i]=="D80"){
    flag_inputs_echo_flag$proptext[i]<-"DMR Overdue, monitoring only required"
  }
}
tsflag_import(flag_inputs_echo_flag,1)
tsflag_import(flag_inputs_dmr_flag_desflow,1)
tsflag_import(flag_inputs_dmr_flag_units_100,1)
tsflag_import(flag_inputs_1000000,1)

