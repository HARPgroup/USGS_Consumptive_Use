#SEE BELOW FOR ECHO R FUNCTIONS



#Use link below to see all available data columns from echo webservice
#paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=",paste(1:305,collapse=","),"&passthrough=Y&qid=","QID")

ECHO_state_pull<- function(state,QID){
  
  start_time <- Sys.time()
  localpath <- tempdir()
  print(paste("Downloading ECHO data to ",localpath,"(Start time: ",start_time,")",sep=""))
  filename <- paste("echo_fac_",state,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,61,62,64,66,68,85,92,96,98,205,206,207,208,210,211,224&passthrough=Y&qid=",QID), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  print(head(data.all))
  
  end_time <- Sys.time()
  print(paste("Download Process Complete: ",end_time ,sep=""))
  print(paste("Time elapsed: ",end_time-start_time,sep=""))
  
  return(data.all)
}


QID <- function(state){
  start_time <- Sys.time()
  print(paste("Retrieving QID for ",state," (Start time: ",start_time,")",sep=""))
  
  Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1&passthrough=Y&p_st=",state)
  print(paste("Using URL: ",Req_URL,sep=""))
  URL_Download<-getURL(Req_URL) #Download URL from above
  URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
  QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
  QID<-QID$QueryID
  print(paste("QID for ",state," = ",QID,sep=""))
  
  end_time <- Sys.time()
  print(paste("Download Process Complete: ",end_time ,sep=""))
  print(paste("Time elapsed: ",end_time-start_time,sep=""))
  
  return(QID)
}



# Spatial containment function 
# deprecated function below, current function found here: https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/GIS_functions/GIS_functions.R
# 
# sp_contain <- function(poly_path,poly_layer_name,point_df,epsg_code = "4326"){
# 
#   # ####################################
#   # Testing 10/11/22
#   # poly_path = HUC6_path
#   # poly_layer_name = HUC6_layer_name
#   # point_df = ECHO_Facilities
#   ####################################
#   
#   start_time <- Sys.time()
#   print(paste("Start time: ",start_time,sep=""))
#   
#   # read in polygons
#   # poly_layer_load <- readOGR(paste(localpath,poly_path,sep=""),layer=poly_layer_name)
#   poly_layer_load <- readOGR(poly_path,layer=poly_layer_name)
#   poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))
# 
#   # tell R that point_df coordinates are in the same lat/lon reference system
#   # as the poly_layer data 
#   proj4string(point_df) <- proj4string(poly_layer)
# 
#   # combine is.na() with over() to do the containment test; note that we
#   # need to "demote" point_df to a SpatialPolygons object first
#   inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
# 
#   # what fraction of points are inside a polygon?
#   print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
#   
#   # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
#   # object, to determine which polygon (if any) contains each point, and
#   # store the polygon name and code as attributes of the point data
#   point_df$Poly_Name <- over(point_df, poly_layer)$Name
#   point_df$Poly_Code <- over(point_df, poly_layer)$Code
#   
#   end_time <- Sys.time()
#   print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
#   
#   return(point_df)
# }



permit_REST <- function(ECHO_Facilities_i, agency_adminid, permit){

  if (ECHO_Facilities_i$CWPPermitTypeDesc =="General Permit Covered Facility"){
    permit_ftype <- "npdes_gp"
  } else if (ECHO_Facilities_i$CWPPermitTypeDesc == "NPDES Individual Permit") {
    permit_ftype <- "npdes_ip"
  }
  
  #CWPPermitStatusDesc = "Pending" and "Denied" set as "unknown" for now but may want to create a new fstatus
  permit_fstatus <- as.character(ECHO_Facilities_i$CWPPermitStatusDesc)
  if (length(grep('Effective',permit_fstatus))>0|
      length(grep('Admin Continued',permit_fstatus))>0){
    permit_fstatus <-'active'
  } else if (length(grep('Terminated', permit_fstatus))>0){
    permit_fstatus <-'revoked'
  } else if (length(grep('Not Needed', permit_fstatus))>0|
             length(grep('NA', permit_fstatus))>0|
             length(grep('Denied', permit_fstatus))>0|
             length(grep('Pending', permit_fstatus))>0){
    permit_fstatus <-'unknown'
  } else if (length(grep('Expired', permit_fstatus))>0){
    permit_fstatus <-'expired'
  } 
  print(paste("permit fstatus: ",permit_fstatus))
  
  # prints list of all status type in column CWPPermitStatusDesc
  # is_it_there <- "select distinct CWPPermitStatusDesc
  # from ECHO_Facilities_original"
  # sqldf(is_it_there)
  
  
  permit_inputs <- data.frame(
    bundle = 'permit',
    ftype = permit_ftype,
    admincode = as.character(ECHO_Facilities_i$Facility_ID),
    name = as.character(ECHO_Facilities_i$CWPName),
    fstatus = permit_fstatus,
    description = as.character(ECHO_Facilities_i$CWPPermitTypeDesc),
    startdate = as.numeric(as.POSIXlt(anydate(as.character(ECHO_Facilities_i$CWPEffectiveDate)))), 
    enddate = as.numeric(as.POSIXlt(anydate(as.character(ECHO_Facilities_i$CWPExpirationDate)))),
    permit_id = as.character(ECHO_Facilities_i$Facility_ID),
    dh_link_admin_reg_issuer = agency_adminid,
    stringsAsFactors = FALSE
  ) 
  
  permit <- postAdminregFeature(permit_inputs, base_url)
  permit <- getAdminregFeature(permit_inputs, base_url)
  
  return(permit)
}


#fipscode <- 24033
fips_REST <- function(fipscode,token){
  print(paste("Retrieving hydroid for fips code:", fipscode), sep=" ")
  
  fips_inputs <- data.frame(
    bundle = 'usafips',
    hydrocode = fipscode,
    stringsAsFactors = FALSE
  ) 
  
  fips_hydroid <- getFeature(fips_inputs, token, base_url)
  
  if(fips_hydroid != FALSE){
    fips_hydroid <- as.character(fips_hydroid$hydroid)
  } else {
    fips_hydroid <- ''
  }
  
  return(fips_hydroid)
}

facility_REST <- function(ECHO_Facilities_i, permit, token, facility){
  facility_name <- as.character(ECHO_Facilities_i$CWPName)
  print(paste("Processing FTYPE for Facility:", facility_name), sep=" ")
  facility_ftype <-'unknown'
  #determining ftype based on facility_name
  #SIC CODE could be used to determine ftype, if null then use name matching
  # prints list of all SIC codes by major group in column CWPPSICCodes
  # ECHO_Facilities_original$siccodes <- ECHO_Facilities_original$CWPSICCodes
  # ECHO_Facilities_original$siccodes <- substr(ECHO_Facilities_original$siccodes, 1,2) 
  # is_it_there <- distinct(ECHO_Facilities_original, siccodes)
  
  if (length(grep('\\bWASTE WATER\\b',facility_name))>0|
        length(grep('\\bWWTP\\b',facility_name))>0|
        length(grep('\\bWWTF\\b',facility_name))>0|
        length(grep('\\bWASTEWATER\\b',facility_name))>0|
        length(grep('\\bWASTEWATER Facility\\b',facility_name))>0|
        length(grep('\\bWT PLANT\\b',facility_name))>0|
        length(grep('\\bPOLLUTION CONTROL\\b',facility_name))>0|
        length(grep('\\bPOLLUTION CONTR\\b',facility_name))>0){
      facility_ftype<-'wwtp'
    } else if (length(grep('COMBINED SEW SYSTEM',facility_name))|
               length(grep('\\bMUNICIPAL\\b',facility_name))>0|
               length(grep('\\bSERVICE AREA\\b',facility_name))>0|
               length(grep('\\bSERV AREA\\b',facility_name))>0|
               length(grep('\\bREGIONAL WATER SYSTEM\\b',facility_name))>0|
               length(grep('\\bWWTREAT PLANT\\b',facility_name))>0|
               length(grep('\\bTRICKLING FILTER\\b',facility_name))>0|
               length(grep('\\bFILTRATION PLANT\\b',facility_name))>0|
               length(grep('\\bCENTRAL SYSTEM\\b',facility_name))>0|
               length(grep("\\bMS4\\b",facility_name))>0| #Municipal Separate Storm Sewer System
               length(grep("\\bTRAILER\\b",facility_name))>0|
               length(grep("\\bMOBILE HOME\\b",facility_name))>0|
               length(grep("\\bTRACT\\b",facility_name))>0|
               length(grep("\\bCOMMUNITY\\b",facility_name))>0|
               length(grep("\\bHOMES\\b",facility_name))>0|
               length(grep("\\bAPARTMENTS\\b",facility_name))>0|
               length(grep("\\bSUBDIVISION\\b",facility_name))>0|
               length(grep('\\bPARK WATER SYSTEM\\b',facility_name))>0|
               length(grep('\\bSTP\\b',facility_name))>0|
               length(grep('COMBINED SEWER SYSTEM',facility_name))>0){
      facility_ftype<-'municipal'
    } else if (length(grep('\\bPOWER\\b',facility_name))>0|
               length(grep('\\bPOWER STATION\\b',facility_name))>0|
               length(grep('\\bELECTRIC\\b',facility_name))>0){
      facility_ftype<-'fossilpower'
      if(length(grep("\\bNUCLEAR\\b",facility_name>0))){
        facility_ftype<-"nuclearpower"
      } else if(length(grep("\\bHYDRO\\b",facility_name>0))){
        facility_ftype<-"hydropower"
      }
    }  else if(length(grep('\\bNUCLEAR\\b',facility_name))>0){
      facility_ftype<-'nuclearpower' 
    }  else if(length(grep('\\bHYDROELECTRIC\\b',facility_name))>0){
      facility_ftype<-'hydropower' 
    }else if (length(grep("\\bMINE\\b",facility_name))>0|
              length(grep('\\bQUARRY\\b',facility_name))>0|
              length(grep('\\bSAND AND GRAVEL\\b',facility_name))>0|
              length(grep("\\bMINING\\b",facility_name))>0){
      facility_ftype<-'mining'
    } else if(length(grep('\\bIRRIGATION\\b',facility_name))>0|
          length(grep('\\bNURSERY\\b',facility_name))>0|
          length(grep('\\bNURSERIES\\b',facility_name))>0|
          length(grep("\\bGREENHOUSE\\b",facility_name))>0){
      facility_ftype<-"irrigation"
  } else if (length(grep('\\bFARM\\b',facility_name))>0|
              length(grep('\\bORNAMENTALS\\b',facility_name))>0|
              length(grep('\\bPRODUCE\\b',facility_name))>0|
              length(grep('\\bLAWN\\b',facility_name))>0|
              length(grep('\\bCENTER PIVOT\\b',facility_name))>0|
              length(grep('\\bHOG\\b',facility_name))>0|
              length(grep('\\bDAIRY\\b',facility_name))>0|
              length(grep('\\bORCHARD\\b',facility_name))>0|
              length(grep('\\bVINEYARD\\b',facility_name))>0|
              length(grep("\\bFISHERIES\\b",facility_name))>0|
              length(grep("\\bFISH\\b",facility_name))>0|
              length(grep("\\bHATCHERY\\b",facility_name))>0){
      facility_ftype<-"agriculture"
    }else if(length(grep('\\bAIRPORT\\b',facility_name))>0|
             length(grep("\\bGOLF COURSE\\b",facility_name))>0|
             length(grep("\\bCOUNTRY CLUB\\b",facility_name))>0|
             length(grep("\\bCOUNTRY CLB\\b",facility_name))>0|
             length(grep("\\bCLUB\\b",facility_name))>0|
             length(grep("\\bGOLF\\b",facility_name))>0|
             length(grep("\\bCOURSE\\b",facility_name))>0|
             length(grep("\\bCHURCH\\b",facility_name))>0|
             length(grep("\\bCOMPLEX\\b",facility_name))>0|
             length(grep("\\bSCHOOL\\b",facility_name))>0|
             length(grep("\\bSCHOOLS\\b",facility_name))>0|
             length(grep("\\bRECREATION\\b",facility_name))>0|
             length(grep("\\bLEARNING CENTER\\b",facility_name))>0|
             length(grep("\\bELEMENTARY\\b",facility_name))>0|
             length(grep("\\bINSTITUTE\\b",facility_name))>0|
             length(grep("\\bCOURTHOUSE\\b",facility_name))>0|
             length(grep("\\bNAVAL\\b",facility_name))>0|
             length(grep("\\bSPACE FLIGHT CENTER\\b",facility_name))>0|
             length(grep("\\bEDUCATIONAL\\b",facility_name))>0|
             length(grep("\\bCEMETERY\\b",facility_name))>0|
             length(grep("\\bREST AREA\\b",facility_name))>0|
             length(grep("\\bRENTALS\\b",facility_name))>0|
             length(grep("\\bINN\\b",facility_name))>0|
             length(grep("\\bMUSEUM\\b",facility_name))>0|
             length(grep("\\bBUILDING\\b",facility_name))>0|
             length(grep("\\bUNIVERSITY\\b",facility_name))>0|
             length(grep("\\bHOSPITAL\\b",facility_name))>0|
             length(grep("\\bRESTAURANT\\b",facility_name))>0|
             length(grep("\\bCORRECTION CENTER\\b",facility_name))>0|
             length(grep("\\bTRAINING CENTER\\b",facility_name))>0|
             length(grep("\\bDETENTION CENTER\\b",facility_name))>0|
             length(grep("\\bCORRECTIONAL\\b",facility_name))>0|
             length(grep("\\bREHABILITATION\\b",facility_name))>0|
             length(grep("\\bCAMPGROUND\\b",facility_name))>0|
             length(grep("\\bCORRECTION UNIT\\b",facility_name))>0|
             length(grep("\\bTRAVEL CENTER\\b",facility_name))>0|
             length(grep("\\bSTATE PARK\\b",facility_name))>0|
             length(grep("\\bDEPARTMENT OF LABOR\\b",facility_name))>0|
             length(grep("\\bRESORT\\b",facility_name))>0|
             length(grep("\\bYMCA\\b",facility_name))>0|
             length(grep("\\bCOOPERATIVE\\b",facility_name))>0|
             length(grep("\\bBUSCH GARDENS\\b",facility_name))>0|
             length(grep("\\bRETREAT\\b",facility_name))>0|
             length(grep('\\bTIMBER\\b',facility_name))>0|
             length(grep('\\bLUMBER\\b',facility_name))>0|
             length(grep("\\bLANDFILL\\b",facility_name))>0|
             length(grep("\\bCAR WASH\\b",facility_name))>0){
      facility_ftype<-'commercial'
    } else if(length(grep('\\bPAPER\\b',facility_name))>0|
              length(grep('\\bCONCRETE\\b',facility_name))>0|
              length(grep('\\bAMMUNITION\\b',facility_name))>0|
              length(grep('\\bTERMINALS\\b',facility_name))>0|
              length(grep('\\bCONCENTRATOR\\b',facility_name))>0|
              length(grep('\\bCONSTRUCTION\\b',facility_name))>0|
              length(grep('\\bPLT\\b',facility_name))>0|
              length(grep('\\bMOTORS\\b',facility_name))>0|
              length(grep('\\bPRODUCTS\\b',facility_name))>0|
              length(grep('\\bCHEMICAL\\b',facility_name))>0|
              length(grep('\\bINDUSTRIES\\b',facility_name))>0|
              length(grep('\\bINDUSTRIAL\\b',facility_name))>0|
              length(grep('\\bINDUSTRIAL PARK\\b',facility_name))>0|
              length(grep('\\bWAREHOUSE\\b',facility_name))>0|
              length(grep('\\bBREWERY\\b',facility_name))>0|
              length(grep('\\bPURINA\\b',facility_name))>0){
      facility_ftype<-'industrial'
    }  else if(length(grep('\\bPLANT\\b',facility_name))>0|
               length(grep('\\bMANUFACTURING\\b',facility_name))>0){ 
      facility_ftype<-'manufacturing'
  } #END OF FTYPE ASSIGNMENT
  print(paste("FTYPE = ", facility_ftype, sep=""))
  
  
  fips <- fips_REST(as.character(ECHO_Facilities_i$FacFIPSCode),token)
  
  facility_inputs <- data.frame(
    bundle = 'facility',
    name = facility_name,
    ftype = facility_ftype,
    hydrocode = as.character(ECHO_Facilities_i$hydrocode),
    fstatus = as.character(permit$fstatus),
    dh_geofield = paste0('POINT (', ECHO_Facilities_i$FacLong, ' ', ECHO_Facilities_i$FacLat,')'),
    address1 = ECHO_Facilities_i$CWPStreet,
    city = as.character(ECHO_Facilities_i$CWPCity),
    dh_link_admin_location = as.character(permit$adminid),
    stringsAsFactors = FALSE
  ) 
  
  if (length(fips) > 1) {
    facility_inputs$dh_link_admin_fa_usafips = fips
  }
  
  facility <- postFeature(facility_inputs, base_url)
  facility <- getFeature(facility_inputs, token, base_url)
  return(facility)
  
} #END OF FACILITY_REST FUNCTION

ECHO_properties_REST <- function(outfall_inputs,facility, token, base_url, prop){
  #facility_hydroid <- as.character(facility$hydroid)
  
  
  #last_inspect (Last date permit was inspected on-site)
  # last_inspect_input <- data.frame(
  #   bundle = 'dh_properties',
  #   entity_type = 'dh_feature',
  #   featureid = facility_hydroid,
  #   varkey = 'last_inspect',
  #   propname = 'last_inspect',
  #   startdate = as.numeric(as.POSIXlt(anydate(as.character(ECHO_Facilities_i$CWPDateLastInspection)))),
  #   stringsAsFactors = FALSE
  # )
  # last_inspect_prop <- postProperty(last_inspect_input,base_url=base_url)
  # last_inspect_prop <- getProperty(last_inspect_input,base_url=base_url)
  #css (Logical combined sewer system value)
  css_input <- data.frame(
    bundle = 'dh_properties',
    entity_type = 'dh_feature',
    #hydrocode = as.character(paste0("echo_",ECHO_Facilities_i$Facility_ID))
    featureid = as.character(facility$hydroid),
    varkey = 'css',
    propname = 'css',
    propcode = as.character(ECHO_Facilities_i$CWPCsoFlag),
    stringsAsFactors = FALSE
  )
  css_prop <- postProperty(css_input,base_url=base_url)
  css_prop <- getProperty(css_input,base_url=base_url)
  #cwp_cso_outfalls (number of upstream outfalls)
  cso_outfalls_input <- data.frame(
    bundle = 'dh_properties',
    entity_type = 'dh_feature',
    featureid = facility_hydroid,
    varkey = 'cwp_cso_outfalls',
    propname = 'cwp_cso_outfalls',
    propvalue = as.character(ECHO_Facilities_i$CWPCsoOutfalls),
    stringsAsFactors = FALSE
  )
  cso_outfalls_prop <- postProperty(cso_outfalls_input,base_url=base_url)
  cso_outfalls_prop <- getProperty(cso_outfalls_input,base_url=base_url)
  #wb_gnis_name (Receiving waterbody USGS name)
  wb_gnis_name_input <- data.frame(
    bundle = 'dh_properties',
    entity_type = 'dh_feature',
    featureid = facility_hydroid,
    varkey = 'wb_gnis_name',
    propname = 'wb_gnis_name',
    propcode = as.character(ECHO_Facilities_i$RadGnisName),
    stringsAsFactors = FALSE
  )
  wb_gnis_name_prop <- postProperty(wb_gnis_name_input,base_url=base_url)
  wb_gnis_name_prop <- getProperty(wb_gnis_name_input,base_url=base_url)
  #reachcode_rad (USGS reach code)
  reachcode_rad_input <- data.frame(
    bundle = 'dh_properties',
    entity_type = 'dh_feature',
    featureid = facility_hydroid,
    varkey = 'reachcode_rad',
    propname = 'reachcode_rad',
    propcode = as.character(ECHO_Facilities_i$RadReachcode),
    stringsAsFactors = FALSE
  )
  reachcode_rad_prop <- postProperty(reachcode_rad_input,base_url=base_url)
  reachcode_rad_prop <- getProperty(reachcode_rad_input,base_url=base_url)
  #impair_cause (Stressors that are causing impairment)
  impair_cause_input <- data.frame(
    bundle = 'dh_properties',
    entity_type = 'dh_feature',
    featureid = facility_hydroid,
    varkey = 'impair_cause',
    propname = 'impair_cause',
    proptext = as.character(ECHO_Facilities_i$AttainsStateCauses),
    stringsAsFactors = FALSE
  )
  impair_cause_prop <- postProperty(impair_cause_input,base_url=base_url)
  impair_cause_prop <- getProperty(impair_cause_input,base_url=base_url)
  # #design_flow (Design Flow)
  #   design_flow_input <- data.frame(
  #   bundle = 'dh_properties',
  #   entity_type = 'dh_feature',
  #   hydrocode = as.character(paste0("echo_",ECHO_Facilities_i$Facility_ID)),
  #   varkey = 'design_flow',
  #   propname = 'design_flow',
  #   propvalue = as.numeric(ECHO_Facilities_i$DesignFlow_mgd),
  #   stringsAsFactors = FALSE
  #   )
  #   design_flow_prop <- postProperty(design_flow_input,base_url=base_url)
} #END OF ECHO_PROPERTIES_REST FUNCTION

dh_echo_format_ts <- function(timeseries, outfalls) {
  
  ts_inputs <- sqldf(
    " select a.hydroid as featureid, 
        b.varkey, 
        'dh_feature' as entity_type,
        b.tsvalue,
        b.tscode,
        b.tstime,
        b.tsendtime,
        b.dmr_flag_desflow,
        b.dmr_flag_units_100,
        b.dmr_flag_units_1000000,
        b.violation as echo_flag
      from outfalls as a
      left outer join timeseries as b 
      on (a.hydrocode = b.hydrocode)
    "
  )
  # Format Dates in unix timestamp
  ts_inputs$tstime <- format(as.POSIXlt(ts_inputs$tstime),"%s")
  ts_inputs$tsendtime <- format(as.POSIXlt(ts_inputs$tsendtime),"%s")
  # format flags
  ts_inputs$dmr_flag_desflow[is.na(ts_inputs$dmr_flag_desflow)]<-""
  ts_inputs$dmr_flag_units_100[is.na(ts_inputs$dmr_flag_units_100)]<-""
  ts_inputs$dmr_flag_units_1000000[is.na(ts_inputs$dmr_flag_units_1000000)]<-""
  ts_inputs$echo_flag[is.na(ts_inputs$echo_flag)]<-""
  ts_inputs$tsvalue[ts_inputs$tsvalue==""]<-NA
  ts_inputs<-subset(ts_inputs,!is.na(ts_inputs$tsvalue))
  
  return (ts_inputs)
}

ts_import<- function(outfalls,timeseries,iteration, base_url){
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
  
  ts_inputs <- dh_echo_format_ts(timeseries, outfalls)
  
  if (length(ts_inputs$featureid) == 0) {
    return(NULL)
  }
  for (i in iteration:length(ts_inputs$featureid)){
    print(paste("Processing DMR Entry ",i," of ", length(ts_inputs$featureid)))
    timeseries.dataframe_i <- getTimeseries(ts_inputs[i,1:7], base_url, ts)
    
    if(timeseries.dataframe_i[1]==FALSE){
      timeseries.dataframe_ii <- postTimeseries(ts_inputs[i,1:7],base_url,ts)
      print(timeseries.dataframe_ii)
      
      timeseries.dataframe_i<-getTimeseries(ts_inputs[i,1:7], base_url, ts)
      
    }else{
      print("This timeseries Feature already exists")
    }
    
    timeseries.dataframe<-rbind(timeseries.dataframe,timeseries.dataframe_i)
    
    if(ts_inputs$dmr_flag_desflow[i]=="dmr_flag_desflow"){
      flag_inputs_dmr_flag_desflow_i <-data.frame(
        featureid = as.character(timeseries.dataframe_i$tid),
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
        featureid = as.character(timeseries.dataframe_i$tid),
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
        featureid = as.character(timeseries.dataframe_i$tid),
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
        featureid = as.character(timeseries.dataframe_i$tid),
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
  
  assign("flag_inputs_echo_flag",flag_inputs_echo_flag,envir=.GlobalEnv)
  assign("flag_inputs_1000000",flag_inputs_1000000,envir=.GlobalEnv)
  assign("flag_inputs_dmr_flag_units_100",flag_inputs_dmr_flag_units_100,envir=.GlobalEnv)
  assign("No dmr_flag_desflow flag",flag_inputs_dmr_flag_units_100,envir=.GlobalEnv)
  
  return(timeseries.dataframe)
}


#---------Retrieve Design Flows and Outfall Coordinates in VPDES Database---------#
cu_echo_get_VPDES <- function() {
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
  return(VPDES_DesignFlow)
  
}

cu_echo_get_VPDES_outfalls <- function() {
  
  #Use Aggregated Flows generated from ECHOInterface Script and list of outfalls for creating release and conveyance points.
  temp<-tempfile(fileext = ".zip")
  #Locations and attribute data about active outfalls in the State
  download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip", destfile = temp)
  unzip(temp)
  #Explore what is in VPDES_Geodatabase.gdb
  ogrListLayers("VPDES_Geodatabase.gdb") #Two layers: VPDES Outfalls and OpenFileGDB
  VPDES_Outfalls<-as.data.frame(readOGR("VPDES_Geodatabase.gdb",layer="VPDES_OUTFALLS"))
  #----------Retrieve coordinates of outfalls-----------------#
  names(VPDES_Outfalls)[names(VPDES_Outfalls)=="OUTFALL_ID"]<-'OutfallID'
  names(VPDES_Outfalls)[names(VPDES_Outfalls)=="VAP_PMT_NO"]<-'Facility_ID'
  
  VPDES_Coordinates<-VPDES_Outfalls[,c(15,16)]
  VPDES_Coordinates <- proj4::project(VPDES_Coordinates, proj="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", inverse=TRUE)
  
  #Replace coordinates in VPDES_Outfalls data frame
  VPDES_Outfalls$Longitude<-VPDES_Coordinates$x
  VPDES_Outfalls$Latitude<-VPDES_Coordinates$y
  
  return(VPDES_Outfalls)
}


cu_echo_get_VPDES_design_flow <- function(ECHO_Facilities) {
  #----------Seperate Design Flow as a Facility Property---------------#
  design_flow<-data.frame(
    hydrocode=paste0(
      "echo_",ECHO_Facilities$Facility_ID
    ), 
    varkey='design_flow', 
    propname='design_flow', 
    propvalue=ECHO_Facilities$DesignFlow_mgd,
    propcode=ifelse(ECHO_Facilities$DesignFlow_mgd==0,"fac_flag_zerodesflow",NA), 
    stringsAsFactors = F
  )
  
  return(design_flow)
}

df_coord_pull<- function(ECHO_Facilities, VPDES_DesignFlow){
  
  ECHO_Facilities <- sqldf(
    " select a.*, b.DesignFlow_mgd 
      from ECHO_Facilities as a 
      left outer join VPDES_DesignFlow as b 
      on (a.Facility_ID = b.Facility_ID)
    "
  )
  return(ECHO_Facilities)
  
}

outfall_features_REST <- function(DMR_data, facility, token, base_url, outfall){
  
  outfall_inputs <- vahydro_echo_outfalls(DMR_data, facility)
  
  outfall <- data.frame(
    hydroid = character(),
    bundle = character(),
    ftype = character(),
    hydrocode = character(),
    name = character(),
    fstatus = character(),
    dh_link_facility_mps =character(),
    dh_geofield = character(),
    stringsAsFactors = FALSE
  ) 
  
  #z <- 1
  for (z in 1:(length(outfall_inputs[,1]))){
    outfall_inputs_z <- outfall_inputs[z,]
    print(paste0('PROCESSING OUTFALL ', z, ' of ', length(outfall_inputs[,1])))
    
    #outfall_inputs_z <- outfall_inputs[1,]
  outfall_z <- postFeature(outfall_inputs_z, base_url)
  outfall_z <- getFeature(outfall_inputs_z, token, base_url)
  outfall <- rbind(outfall,outfall_z)
  }
 
  return(outfall)
} #END OF OUTFALL_FEATURES_REST FUNCTION

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


vahydro_echo_outfalls <- function(DMR_data, facility){
  #Outfalls Generation
  outfalls <- sqldf(
    "
      SELECT  
      ('echo_' || npdes_id || rightstr('000' || perm_feature_nmbr, 3)) as hydrocode,
      ('FROM ' || npdes_id) as name,
      'transfer' as bundle,
      'outfall' as ftype,
      'active' as fstatus
      FROM DMR_data
    GROUP BY perm_feature_nmbr
  ")
  outfalls$dh_link_facility_mps <- as.character(facility$hydroid)
  outfalls$dh_geofield <- as.character(facility$geom)
  
  return(outfalls)
}


# Is this deprecated? Used a different format for hydrocode I think
outfall_formatted<- function(ECHO_Outfalls){
  #Outfalls Generation
  #Reformats 'ECHO_Outfalls' using available VPDES or ECHO geometry data and ECHO attributes
  outfalls<-sqldf(
    "select 'transfer' as bundle, 
       'FROM ' || Facility_ID as name, 
       'outfall' as ftype,
       'echo_' || OutfallID as hydrocode,
       'active' as fstatus,
       'echo_' || Facility_ID as dh_link_facility_mps,
       CASE 
         WHEN (Latitude is not null) and (Longitude is not null) THEN  
           'POINT (' || Longitude || ' ' || Latitude || ')' 
         ELSE NULL
       END as dh_geofield
     from ECHO_Outfalls
    "
  )
  return(outfalls)
}


release_generation<- function(ECHO_Facilities,ECHO_Outfalls,timeseries){
  
  #---------Assign Coordinates to Outfalls that have DMRs from 2010-Present----------#
  
  
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
  return(releasepoint)
} 


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
  
  return(conveyance)
}


outfall_properties<- function(outfall_types){
  
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
  return(outfall_props)
}


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
  return(design_flow)
}


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

ts_ECHO_pull<- function(ECHO_Facilities,DMR_data, iteration, startDate="01/01/2010",endDate=NULL){
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
    
#GM#    DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",Facility_ID,"&parameter_code=50050&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe # 50050 only looks at Flow, in conduit ot thru treatment plant - there are 347 parameter codes defined in ECHO
#GM#   DMR_data<-read.csv(DMR_data,sep = ",", stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
#GM input DMR_data using echoGetEffluent instead of this method above    
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
            #GM FLAG we entering this IF bc ==MK is false, and the ! makes the IF statement return TRUE, which may or may not be fine. 
            #GM cont, but then the values definitions here are false due to some faulty logic or the outfall_DMR tibble structure
            print("Entering if MK")
            tsvalue_i[l]<-3 #GM remove this when done and uncomment tsvalue_i]]<-as.number....
            #tsvalue_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="MK"])[l] 
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
        # #Now we store the values we get from each outfall in each facility[i] in a larger matrix
        # #We do this so that results are not over written after each iteration
        # tsvalue<-c(tsvalue,tsvalue_i)
        # tsendtime<-c(tsendtime,tsendtime_i)
        # tscode<-c(tscode,tscode_i)
        # tstime<-c(tstime,tstime_i)
        # varkey<-c(varkey,varkey_i)
        # nodi<-c(nodi,nodi_i)
        # violation<-c(violation,violation_i)
        # violation_severity<-c(violation_severity,violation_severity_i)
        # outfallID<-c(outfallID,paste0(Facility_ID,rep(outfall,length((tsvalue_i)))))
        # hydrocode<-paste0('echo_',outfallID)
        #GM comment out block above and replace with block below, should repeat for the 'else'
        #Now we store the values we get from each outfall in each facility[i] directly because ts_ECHO_pull is only fed one DMR_data line, so there is only one iteration anyway
        tsvalue<-tsvalue_i
        print(paste0("Entering tsvalue = ",tsvalue)) #GM remove print line when done fixing function
        tsendtime<-tsendtime_i
        tscode<-tscode_i
        tstime<-tstime_i
        varkey<-varkey_i
        nodi<-nodi_i
        violation<-violation_i
        violation_severity<-violation_severity_i
        outfallID<-paste0(Facility_ID,rep(outfall,length((tsvalue_i))))
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
  
  #GM once the function is fixed, uncomment out this line below
  #timeseries<-timeseries[!(is.na(timeseries$tsendtime)),]#returns outfalls that have data
  timeseries$tsendtime<-format(mdy(timeseries$tsendtime))
  
  timeseries$Facility_ID<-gsub("echo_","", as.character(timeseries$hydrocode))
  timeseries$Facility_ID<-substr(timeseries$Facility_ID,1,9)
  timeseries$OutfallID<-gsub("echo_","",timeseries$hydrocode)
  return(timeseries)
}
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
  
  
  return(timeseries)
  
}

permit_import<- function(ECHO_Facilities, agency_adminid,iteration){ 
  #adminreg<-data.frame(bundle='permit', admincode=ECHO_Facilities$Facility_ID, description='National Pollutant Discharge Elimination System (NPDES) Permit', name=ECHO_Facilities$CWPName)
  adminreg <- sqldf(
    "select 'permit' as bundle,
      CWPName as name,
      Facility_ID as admincode,
      '' as description,
      CWPEffectiveDate as startdate,
      CWPExpirationDate as enddate,
      CWPPermitStatusDesc as fstatus,
      'National Pollutant Discharge Elimination System (NPDES) Permit' as description,
      CASE 
        WHEN (CWPPermitTypeDesc = 'General Permit Covered Facility') THEN 'npdes_gp'
        ELSE 'npdes_ip'
      END as ftype 
      FROM ECHO_Facilities 
    "
  )
  permit_dataframe<-data.frame()
  permit.dataframe_ii<-data.frame()
  
  #----vectorise and pre-allocate data structures before entering loop-----#
  permit_inputs <- data.frame(
    bundle = as.character(adminreg$bundle),
    ftype = as.character(adminreg$ftype),
    admincode = as.character(adminreg$admincode),
    name = as.character(adminreg$name),
    fstatus = as.character(adminreg$fstatus),
    description = as.character(adminreg$description),
    startdate = format(as.POSIXlt(adminreg$startdate,origin='1970-01-01', tz = "GMT"),"%s"), 
    enddate = format(as.POSIXlt(adminreg$enddate,origin='1970-01-01', tz = "GMT"),"%s"),
    permit_id = as.character(adminreg$admincode),
    dh_link_admin_reg_issuer = agency_adminid, #actual id and not "epa"
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
    permit_dataframe<-rbind(permit_dataframe,permit.dataframe_i) #creating this to keep adminids
  }
  
  #unique adminid's for each permit. Used to link facilities to permits
  permit.adminid<-data.frame(admincode=permit_dataframe$admincode,adminid=permit_dataframe$adminid) 
  
  assign("permit.adminid",permit.adminid,envir = .GlobalEnv)
  return(permit_dataframe)
  
}


# Deprecated function
matched_switch<- function(facility_dataframe, permit_dataframe){
  # deprecated
  # We keep the same NPDES permit dh_link 
  Matched<-read.csv("https://raw.githubusercontent.com/HARPgroup/USGS_Consumptive_Use/master/Code/Facility%20Matching/Runninglist_Matches.csv",sep=",", header = T, stringsAsFactors = F)
  Matched$VWUDS.Hydrocode<-gsub("\\s+$","",Matched$VWUDS.Hydrocode)
  
  facility.dataframe <- data.frame()
  facility.dataframe_ii<-data.frame()
  faciilty_inputs <- sqldf(
    " select CASE WHEN a.hydrocode = b.vpdes_hydrocode
      from facility_dataframe as a 
      left outer join Matched as b 
      on (
      
      )
    "
  )
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
  
  return(facility)
}


vahydro_facility_match<- function(facility) {
  
  # We keep the same NPDES permit dh_link 
  Matched<-read.csv("https://raw.githubusercontent.com/HARPgroup/USGS_Consumptive_Use/master/Code/Facility%20Matching/Runninglist_Matches.csv",sep=",", header = T, stringsAsFactors = F)
  Matched$VWUDS.Hydrocode<-gsub("\\s+$","",Matched$VWUDS.Hydrocode)
  # make the names compatible with sqldf
  Matched$vwuds_hydrocode <- Matched$VWUDS.Hydrocode
  Matched$vpdes_hydrocode <- Matched$VPDES.Hydrocode
  Matched$vwuds_hydroid <- Matched$VWUDS.HydroID
  facility <- sqldf(
    " select a.*, 
      b.vwuds_hydroid as matched_hydroid 
      from facility as a 
      left outer join Matched as b 
      on (
        a.hydrocode = b.vpdes_hydrocode
      )
    "
  )
  
  # if this has a match, get this facility by hydroid
  
  return(facility)
}

# pieces left over, 
matched_switch_remnant<- function(facility_dataframe, permit_dataframe){
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
  
}
