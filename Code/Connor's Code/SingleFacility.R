#The goal of this code is to extract data for effluent charts from ECHO in an automated manner to allow
#for a refreshable list of stations within Virginia as well as desired discharges. It is important to note
#that this code currently looks only at stations in Virginia but should be easily modifiable to allow for the 
#extraction of VA HUCs
##############################################################################################################
#First, start by removing old variables to prevent clutter
rm(list=ls())

#Some basic R libraries to get data from https servers and use parse XML data
library(XML)
library(RCurl)

#Current inputs for the R script. Right now, these inputs are manual entry only but they can be easily
#converted into a function later on. Inputs are 'state', which is the state of interest, 'startDate' and
#'endDate' which are simply the data of interest. Most sites have data from 2012-present, some 2009-present
#The state should be entered as the USPS abbreviation and the dates should be entered 'mm/dd/yyyy'
#Current values for dates are default from ECHO
state<-"VA"
startDate<-"01/01/2016"
endDate<-"12/31/2016"
#endDate<-Sys.Date()
#endDate<-format(as.Date(endDate), "%m/%d/%Y")

#This section of the code will generate a query in the water facility database on ECHO
#The query will contain all facilities with a CWA permit in the state of Virginia. This can be
#Modified to include all VA HUCS as desired (easy option on the ECHO database)
#This query will be created in a two-step process. First, a short XML document will be created
#containing a queryID. This ID can be used in other parts of the database to access the summary
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
#This portion of the code uses the query ID to find the facility station summary, contianing latitude/
#longitude and a unique ID for each station, which can be used to later generate effluent data
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
a<-read.csv(uri_summary,stringsAsFactors = F)
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID)

#This portion of the code uses the source ID from above to draw in effluent data for a single source ID
sourceID<-"VA0004090"
uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
facility<-read.csv(uri_effluent,stringsAsFactors = F)
#This portion creates a basic dataframe from the effluent data to show some potential outputs of interest
facility<-facility[facility$parameter_code==50050,]
#Function to automate DMR download
DMR<-function(sourceID,startDate,endDate,parameter_code=50050){
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate)
  facility<-read.csv(uri_effluent,stringsAsFactors = F)
  facility<-facility[facility$parameter_code==parameter_code,]
  return(facility)
}