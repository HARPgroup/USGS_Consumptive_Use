
  site <- "http://deq1.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
  #hydro_tools <- 'G:\\My Drive\\HARP' #location of hydro-tools repo
  hydro_tools <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\' #location of hydro-tools repo
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
  # RETRIEVE/CREATE PERMIT ADMINREG FEATURE
  ############################################################################################  
  permit_inputs <- list(
    bundle = 'permit',
    ftype = 'npdes',
    admincode = 'VA_JK_TEST100',
    name = 'JK_TEST_100',
    fstatus = 'active',
    description = 'National Pollutant Discharge Elimination System (NPDES) Permit',
    startdate = format(as.POSIXlt('2014-04-01'),"%s"), 
    enddate = format(as.POSIXlt('2016-04-15'),"%s"),
    permit_id = 'VA_JK_TEST100',
    dh_link_admin_reg_issuer = agency.adminid,
    stringsAsFactors = FALSE
  ) 
  permit.dataframe <- postAdminregFeature(permit_inputs, site, adminreg_feature)
  permit.dataframe <- getAdminregFeature(permit_inputs, site, adminreg_feature)
  permit.adminid <- as.character(permit.dataframe$adminid)
  
  ############################################################################################
  # RETRIEVE/CREATE FACILITY DH FEATURE
  ############################################################################################  
  facility_inputs <- list(
    bundle = 'facility',
    ftype = 'wwtp',
    hydrocode = 'echo_TEST_100',
    name = "JK_TEST",
    fstatus = 'active',
    address1 = '123 JK Road',
    city = 'JKburg',
    dh_link_admin_location =  permit.adminid, 
    dh_geofield = 'POINT (-77 38)',
    stringsAsFactors=FALSE
  ) 
  facility.dataframe <- postFeature(facility_inputs, site, feature)
  facility.dataframe <- getFeature(facility_inputs, site, feature)
  facility.hydroid <- as.character(facility.dataframe$hydroid)
  
  ############################################################################################
  # RETRIEVE/CREATE FACILITY METADATA PROPERTIES
  ############################################################################################   
  # Waterbody Name (GNIS)
  prop_inputs <-list(
    featureid = facility.hydroid,
    varkey = 'wb_gnis_name',
    entity_type = 'dh_feature',
    propname = 'wb_gnis_name',
    propvalue = NULL,
    proptext = NULL,
    propcode = 'James River',
    startdate = NULL,
    enddate = NULL
  )
  property.dataframe <- postProperty(prop_inputs, fxn_locations, site, prop)
  property.dataframe <- getProperty(prop_inputs, site, prop)
  property.pid <- as.character(property.dataframe$pid)
  
  # This is where all the other facility properites will be attached
  
  ############################################################################################
  # RETRIEVE/CREATE RELEASE DH FEATURE
  ############################################################################################  
  release_inputs <- list(
    bundle = 'transfer',
    ftype = 'release',
    hydrocode = 'vahydro_JK_RELEASE_100',
    name = "JK RELEASE",
    fstatus = 'active',
    dh_link_facility_mps =  facility.hydroid, 
    dh_geofield = 'POINT (-77 38)',
    stringsAsFactors=FALSE
  ) 
  release.dataframe <- postFeature(release_inputs, site, feature)
  release.dataframe <- getFeature(release_inputs, site, feature)
  release.hydroid <- as.character(release.dataframe$hydroid)

  ############################################################################################
  # RETRIEVE/CREATE OUTFALL DH FEATURE
  ############################################################################################   
  outfall_inputs <- list(
    bundle = 'transfer',
    ftype = 'outfall',
    hydrocode = 'echo_JK_OUTFALL_100',
    name = "JK OUTFALL",
    fstatus = 'active',
    dh_link_facility_mps =  facility.hydroid, 
    dh_geofield = 'POINT (-77.5 38.5)',
    stringsAsFactors=FALSE
  ) 
  outfall.dataframe <- postFeature(outfall_inputs, site, feature)
  outfall.dataframe <- getFeature(outfall_inputs, site, feature)
  outfall.hydroid <- as.character(outfall.dataframe$hydroid)
  
  ############################################################################################
  # RETRIEVE/CREATE CONVEYANCE DH FEATURE
  ############################################################################################  
  # Format conveyance geom from release and outfall geoms 
  release.geofield <- substring(release.dataframe$dh_geofield, 8)
  release.geofield <-substr(release.geofield, 1, nchar(release.geofield)-1) 
  outfall.geofield <- substring(outfall.dataframe$dh_geofield, 8)
  outfall.geofield <-substr(outfall.geofield, 1, nchar(outfall.geofield)-1) 
  conveyance.geofield <- paste('LINESTRING (',release.geofield,', ',outfall.geofield,')',sep="")
  
  conveyance_inputs <- list(
    bundle = 'conveyance',
    ftype = 'water_transfer',
    hydrocode = 'vahydro_JK_OUTFALL_CONVEYANCE_100',
    name = "JK CONVEYANCE",
    fstatus = 'active',
    field_dh_from_entity =  release.hydroid, 
    field_dh_to_entity =  outfall.hydroid,
    dh_geofield = conveyance.geofield,
    stringsAsFactors=FALSE
  ) 
  conveyance.dataframe <- postFeature(conveyance_inputs, site, feature)
  conveyance.dataframe <- getFeature(conveyance_inputs, site, feature)
  conveyance.hydroid <- as.character(conveyance.dataframe$hydroid)

  ############################################################################################
  # RETRIEVE/CREATE TIMESERIES
  ############################################################################################  
  timeseries <- data.frame(hydrocode = 'echo_VA0091529001', 
                           varkey = 'dmr_mon_mgd', 
                           tsvalue = 999, 
                           tstime = '2016-04-01', 
                           tsendtime = '2016-04-30', 
                           tscode = 1)
  
  ts_inputs<-list(
    featureid = outfall.hydroid,
    varkey = as.character(timeseries$varkey),
    entity_type = 'dh_feature',
    tsvalue = timeseries$tsvalue,
    tscode = timeseries$tscode,
    tstime = format(as.POSIXlt(timeseries$tstime),"%s"),
    tsendtime = format(as.POSIXlt(timeseries$tsendtime),"%s")
  )
  timeseries.dataframe <- postTimeseries(ts_inputs,site,ts)
  timeseries.dataframe <- getTimeseries(ts_inputs, site, ts)
  timeseries.tid <- as.character(timeseries.dataframe$tid)
  
  ############################################################################################
  # RETRIEVE/CREATE FLAGGING PROPERTIES OF TIMESERIES
  ############################################################################################   
  flag_inputs <-list(
    featureid = timeseries.tid,
    varkey = 'echo_flag',
    entity_type = 'dh_timeseries',
    propname = 'echo_flag',
    propcode = 'E90'
  )
  flag.dataframe <- postProperty(flag_inputs, fxn_locations, site, prop)
  flag.dataframe <- getProperty(flag_inputs, site, prop)
  flag.pid <- as.character(flag.dataframe$pid)

  # This is where the other flagging properites will be attached
