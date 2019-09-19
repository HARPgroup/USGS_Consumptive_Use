#SEE BELOW FOR ECHO R FUNCTIONS


ECHO_state_pull<- function(state,QID){
  
  start_time <- Sys.time()
  localpath <- tempdir()
  print(paste("Downloading ECHO data to ",localpath,"(Start time: ",start_time,")",sep=""))
  filename <- paste("echo_fac_",state,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,223&passthrough=Y&qid=",QID), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  print(head(data.all))
  
  end_time <- Sys.time()
  print(paste("Download Process Complete: ",end_time ,sep=""))
  print(paste("Time elapsed: ",end_time-start_time,sep=""))
  
  return(data.all)
}


QID <- function(state){
  start_time <- Sys.time()
  print(paste("Retrieving QID for ",state,"(Start time: ",start_time,")",sep=""))
  
  Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&p_st=",state)
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



poly_path <- "hydro-tools/GIS_LAYERS/HUC.gdb"
poly_layer <- 'WBDHU6'

#Spatial containment function 
# Supply 1) file path to .gdb containing layer of polygon features 
#        2) polygon layer of interest within the .gdb above
#        3) dataframe of point features with columns for latitude and longitude 
sp_contain <- function(poly_path,poly_layer,point_path){
##############################################
coordinates(ECHO_Facility) <- c("FacL+ong", "FacLat")

# read in HUC6 polygons
HUC6<-readOGR(paste(localpath,poly_path,sep=""),layer=poly_layer)
HUC6<-spTransform(HUC6, CRS("+init=epsg:4326"))

# tell R that facility coordinates are in the same lat/lon reference system
# as the facility data -- BUT ONLY BECAUSE WE KNOW THIS IS THE CASE!
proj4string(ECHO_Facility) <- proj4string(HUC6)

# combine is.na() with over() to do the containment test; note that we
# need to "demote" facility to a SpatialPolygons object first
inside.HUC6 <- !is.na(over(ECHO_Facility, as(HUC6, "SpatialPolygons")))

# what fraction of facilities are inside a HUC6?
mean(inside.HUC6)

# use 'over' again, this time with HUC6 as a SpatialPolygonsDataFrame
# object, to determine which HUC6 (if any) contains each facility, and
# store the HUC6 name and code as attributes of the facility data
ECHO_Facility$Name <- over(ECHO_Facility, HUC6)$Name
ECHO_Facility$HUC6 <- over(ECHO_Facility, HUC6)$HUC6

}












