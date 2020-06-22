# FUNCTION FOR RETRIEVING A LARGE VIEW EXPORT FROM VAHYDRO
from_vahydro <- function (datasite,export_view,localpath = tempdir(),output_filename, cached=FALSE) {
  
  # DOWNLOAD FACILITY FILE
  vahydro_item <- paste(datasite,export_view,sep="/")
  #file downloaded into local directory, as long as file exists it will not be re-downloaded
  fexist = file.exists(paste(localpath, output_filename, sep = '/'))
  if ( (fexist & cached) == FALSE) {
    print(paste("DOWNLOADING FILE", sep = ''))
    destfile <- paste(localpath,output_filename,sep="\\")
    start_time <- Sys.time()
    print(paste("...DOWNLOAD START: ",format(Sys.time(), "%X, %a %b %d %Y")),sep="")
      download.file(vahydro_item, destfile = destfile, method = "libcurl")
    end_time <- Sys.time()
    print(paste("...DOWNLOAD END:   ",format(end_time, "%X, %a %b %d %Y")),sep="")
    #print(paste("...Time ELAPSED:   ",end_time - start_time,sep=""))
    print(end_time - start_time)  
  } else {
    print(paste("FILE PREVIOUSLY DOWNLOADED", sep = ''))
  }
  
  #READ CSV FROM LOCAL DIRECTORY
  output.dataframe <- read.csv(file=paste(localpath,output_filename,sep="\\"), header=TRUE, sep=",")
  
  return(output.dataframe)
}