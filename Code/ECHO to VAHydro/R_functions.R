#SEE BELOW FOR ECHO R FUNCTIONS



#Use link below to see all available data columns from echo webservice
#paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=",paste(1:500,collapse=","),"&passthrough=Y&qid=","QID")
       
ECHO_state_pull<- function(state,QID){
  
  start_time <- Sys.time()
  localpath <- tempdir()
  print(paste("Downloading ECHO data to ",localpath,"(Start time: ",start_time,")",sep=""))
  filename <- paste("echo_fac_",state,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,64,65,66,67,68,84,91,95,97,204,205,206,207,209,210,223&passthrough=Y&qid=",QID), destfile = destfile, method = "libcurl")  
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



#Spatial containment function 
# Supply 1) file path to .gdb containing layer of polygon features 
#        2) polygon layer of interest within the .gdb above (must have "Name" and "Code" attributes)
#        3) Large SpatialPointsDataFrame of point features with column of coordinates
#        4) epsg code of interest, default to 4326
# Function returns a Large SpatialPointsDataFrame
sp_contain <- function(poly_path,poly_layer_name,point_df,epsg_code = "4326"){

  start_time <- Sys.time()
  print(paste("Start time: ",start_time,sep=""))
  
  # read in polygons
  poly_layer_load <- readOGR(paste(localpath,poly_path,sep=""),layer=poly_layer_name)
  poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))

  # tell R that point_df coordinates are in the same lat/lon reference system
  # as the poly_layer data 
  proj4string(point_df) <- proj4string(poly_layer)

  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" point_df to a SpatialPolygons object first
  inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))

  # what fraction of points are inside a polygon?
  print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$Poly_Name <- over(point_df, poly_layer)$Name
  point_df$Poly_Code <- over(point_df, poly_layer)$Code
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
  
  return(point_df)
}



permit_REST <- function(ECHO_Facilities_i, agency_adminid, permit){

if (ECHO_Facilities_i$CWPPermitTypeDesc =="General Permit Covered Facility"){
  permit_ftype <- "npdes_gp"
} else if (ECHO_Facilities_i$CWPPermitTypeDesc == "NPDES Individual Permit") {
  permit_ftype <- "npdes_ip"
}

#CWPPermitStatusDesc = Pending is set as "unknown" for now but may want to create a new fstatus
permit_fstatus <- as.character(ECHO_Facilities_i$CWPPermitStatusDesc)
if (length(grep('Effective',permit_fstatus))>0|
    length(grep('Admin Continued',permit_fstatus))>0){
  permit_fstatus <-'active'
} else if (length(grep('Terminated', permit_fstatus))>0){
  permit_fstatus <-'revoked'
} else if (length(grep('Not Needed', permit_fstatus))>0|
           length(grep('NA', permit_fstatus))>0|
           length(grep('Pending', permit_fstatus))>0){
  permit_fstatus <-'unknown'
} else if (length(grep('Expired', permit_fstatus))>0){
  permit_fstatus <-'expired'
} 
print(paste("permit fstatus: ",permit_fstatus))
#prints list of all status type in column CWPPermitStatusDesc
#is_it_there <- "select distinct CWPPermitStatusDesc
#from ECHO_Facilities_original"
#sqldf(is_it_there)

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

permit <- postAdminregFeature(permit_inputs, basepath)
permit <- getAdminregFeature(permit_inputs, basepath)

return(permit)
}


facility_REST <- function(ECHO_Facilities_i, permit, token, facility){
  facility_name <- as.character(ECHO_Facilities_i$CWPName)
  print(paste("Processing FTYPE for Facility:", facility_name), sep=" ")
  facility_ftype <-'unknown'
  #determining ftype based on facility_name
  #SIC CODE could be used to determine ftype, if null then use name matching
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
  
  
  facility_inputs <- data.frame(
    bundle = 'facility',
    name = facility_name,
    ftype = facility_ftype,
    hydrocode = as.character(paste0("echo_",ECHO_Facilities_i$Facility_ID)),
    fstatus = as.character(permit$fstatus),
    dh_geofield = paste0('POINT (', ECHO_Facilities_i$FacLong, ' ', ECHO_Facilities_i$FacLat,')'),
    address1 = ECHO_Facilities_i$CWPStreet,
    city = as.character(ECHO_Facilities_i$CWPCity),
    dh_link_admin_location = as.character(permit$adminid),
    stringsAsFactors = FALSE
  ) 
  
  facility <- postFeature(facility_inputs, basepath)
  facility <- getFeature(facility_inputs, token, basepath)
  return(facility)
  
  } #END OF FACILITY_REST FUNCTION
