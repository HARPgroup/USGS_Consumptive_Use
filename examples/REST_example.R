rm(list = ls())  #clear variables
#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
hydro_tools <- 'G:\\My Drive\\HARP' #location of hydro-tools repo

#----------------------------------------------

#Generate REST token              


rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"config.local.private.example", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
token <- rest_token(site, token, rest_uname, rest_pw)

#--------------------------------------------------------------------------------------------

# Retrieve Outfall Feature From VAHydro 
#--------------------------------------------------------------------------------------------  

inputs <- list (
  bundle = 'transfer',
  ftype = 'outfall',
  hydrocode = 'echo_VA0000248006'
)

dataframe <- getFeature(inputs, token, site)
hydroid <- as.character(dataframe$hydroid)

#--------------------------------------------------------------------------------------------

# Retrieve Timeseries from VAHydro Using Feature Hydroid From Above 

#--------------------------------------------------------------------------------------------  
ts_inputs <- list (
  featureid = hydroid,
  varkey = 'dmr_mon_mgd',
  entity_type = 'dh_feature' #By omitting tstime parameter you can retrieve all timeseries
)


timeseries.df <- getTimeseries(ts_inputs, site, ts)
#------------------------------------------------------------------------------------------ 
# Alternatively you can retrieve a single timeseries by supplying a date 

ts_inputs <- list (
  featureid = hydroid,
  varkey = 'dmr_mon_mgd',
  entity_type = 'dh_feature',
  tstime = format(as.POSIXlt('2011-08-01'),"%s") #retrieve timeseries for a specified date (must convert human-readable to unix timestamp)
)

timeseries.df <- getTimeseries(ts_inputs, site, ts)



#tid <- as.character(timeseries.df$tid) <- tid will be used for setting flagging properties on timseries 

#--------------------------------------------------------------------------------------------   

#--------------------------------------------------------------------------------------------

# Creating Timeseries in VAHydro Using Feature Hydroid From Above

# NOTE the postTimeseries function works in the following way: 

#   *if no timeseries exists matching the following featureid, varkey, tstime and tsendtime; one will be created

#   *if existing property is found, that property will be updated   

#   *you can use the above retrieval code to confirm a timeseries was created or updated  

#--------------------------------------------------------------------------------------------    

ts_post_inputs <- list (
  featureid = hydroid,
  varkey = 'dmr_mon_mgd',
  entity_type = 'dh_feature',
  tsvalue = 999, 
  tstime = format(as.POSIXlt('2018-07-01'),"%s"), 
  tsendtime = format(as.POSIXlt('2018-07-31'),"%s"), 
  tscode = 1
)

postTimeseries(ts_post_inputs,site,ts) 
