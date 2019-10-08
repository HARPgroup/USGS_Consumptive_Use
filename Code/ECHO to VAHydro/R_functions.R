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
  print(paste("Fraction of points within polygon layer: ", mean(inside.poly_layer),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$Poly_Name <- over(point_df, poly_layer)$Name
  point_df$Poly_Code <- over(point_df, poly_layer)$Code
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",end_time-start_time,sep=""))
  
  return(point_df)
}


