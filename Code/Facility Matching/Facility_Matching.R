########################################################################################################
####Matching VWUDS and VPDES Facilities####

##This code serves to match withdrawal facilites (State Maintained) to a discharge facility (Federally Maintained)
##Let it be known that some withdrawal facilties will not having a matching discharge facility due to its water use type 
#(agriculture, livestock, etc.) Therefore we are matching withdrawal facilities to a list of dishcarge facilities. 

##There are two methods to matching: 
##1:First is to create a search radius from each discharge facility and then indicate the withdrawal facilites that fall within that radius. 
##2: Then we take the facilities that fall within the search radius and match them by facility name (using fuzzy logic)

########################################################################################################
####Load Necessary Packages####

setwd("G:/My Drive/GRA/RCode for VWUDS and VPDES Analysis")

#Clean up environment
rm(list=objects())

library(sp)
library(rgeos)
library(geosphere)
library(xtable)
library(dplyr)

options(scipen=999) #disable scientific notation 


path<-"G:/My Drive/GRA/RCode for VWUDS and VPDES Analysis"

########################################################################################################
####Set up workspace with directory and dataframes####

Existing_Matches<-read.csv("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Matched Facilities/Runninglist_Matches.csv",header=T)

#-------------Discharging Facilities in VPDES Database----------------#
 VA_Facility_Pull<- function(state){
  Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&p_st=",state)
  URL_Download<-getURL(Req_URL) #Download URL from above
  URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
  QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
  QID<-QID$QueryID
  GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&qid=",QID)
  ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
  ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)
  
  ECHO_Facilities<-subset(ECHO_Facilities, subset=ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit")
  assign("ECHO_Facilities",ECHO_Facilities,envir = .GlobalEnv)
  
}
# VA_Facility_Pull("VA")
# VPDES<-ECHO_Facilities
VPDES<-VPDES%>%dplyr::group_by(SourceID)%>%dplyr::summarise(Facility_Name=first(CWPName), 
                                                            Longitude=first(FacLong),
                                                            Latitude=first(FacLat))
colnames(VPDES)[1]<-c("Facility.ID")
VPDES<-subset(VPDES,VPDES$Facility.ID %in% missing$Hydrocode)

VPDES<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/ECHO_2010_2017_QAQC.txt", sep="\t", header=T)

load(paste0(path,"/cum_dis.RData"))
colnames(Discharge)[1]<-c("Facility.ID")
VPDES<-VPDES%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(Facility_Name=first(FacilityName), Longitude=mean(Facility_Longitude, na.rm=T),
                                                               Latitude=mean(Facility_Latitude, na.rm=T), Use_type=first(Reclass_Use_Type),Design_Flow=first(DesignFlow_mgd), 
                                                               City=first(City), County=first(County))



VPDES<-merge(VPDES,Discharge,by=c("Facility.ID","Facility_Name"),all.x=T)

#--------------Withdrawing Facilities in VWUDS Database
load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017.RData")
VWUDS<-VWUDS_2010_2017
VWUDS<-mutate_if(VWUDS,is.factor,as.character)
VWUDS$Owner_Facility<-do.call(paste, c(VWUDS[c("Owner","Facility")],sep=":"))

load(paste0(path,"/cum_with.RData"))
colnames(Withdrawal)[1]<-c("Facility.ID")


VWUDS<-VWUDS%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(Facility_Name=first(Owner_Facility),Longitude=mean(Corrected_Longitude, na.rm=T),
                                                               Latitude=mean(Corrected_Latitude, na.rm=T), Use_type=first(Reclass_Use_Type), 
                                                               Source=first(Source.Type), County=first(Locality))

VWUDS<-merge(VWUDS,Withdrawal,by=c("Facility.ID"),all.x=T)

colnames(VWUDS)[2]<-"Facility_Name"
VWUDS$Facility_Name<-toupper(VWUDS$Facility_Name)
VPDES$Facility_Name<-toupper(VPDES$Facility_Name)


########################################################################################################
####Narrowing Facilities Based on Distance####

####Setting up loop to go through list of VPDES facilities (One by One) to calculate distance to each VWUDS facility. 
#Then we mark those that fall within a set search radius####

########################################################################################################
#Define a search Radius. Distance calculated below is in kilometers
search_radius<-10  

#Pulling Latitude and Longitude from imported csv files and putting into separate matrix 
#***Important that Longitude is in first column for distance function below***
VPDES_Loc<-matrix(c(VPDES$Longitude,VPDES$Latitude), ncol=2)
colnames(VPDES_Loc)<-c("Longitude", "Latitude")

VWUDS_Loc<-matrix(c(VWUDS$Longitude,VWUDS$Latitude), ncol=2)
colnames(VWUDS_Loc)<-c("Longitude", "Latitude")

VWUDS.facilities<-matrix(ncol=nrow(VWUDS), nrow=nrow(VPDES))
rownames(VWUDS.facilities)<-toupper(VPDES$Facility_Name)
colnames(VWUDS.facilities)<-toupper(VWUDS$Facility_Name)
VWUDS.facilities[is.na(VWUDS.facilities)]<-colnames(VWUDS.facilities)[which(is.na(VWUDS.facilities), arr.ind = TRUE)[,'col']]

#View(VWUDS.facilities)
########################################################################################################
#Set up empty table that will store distances. 
#Number of rows is equal to number of discharging facilities 
#Number of Columns is number of withdrawing facilities
#Creates a comparison matrix 

distance<-matrix(nrow=nrow(VPDES_Loc), ncol=nrow(VWUDS_Loc))
rownames(distance)<-VPDES$Facility_Name
colnames(distance)<-VWUDS$Facility_Name

#A loop that calculates the distance from each point in VPDES list to every point in VWUDS list 
#(single point against multiple points)

for(i in 1:nrow(VPDES_Loc)){
  #Calculate Distance between single discharge facility and each withdrawal facility. 
  
  #seperate each VPDES Facility into single_VPDES_facility to allow to go through each single facility
  single_VPDES_facility<-c(VPDES_Loc[i,1], VPDES_Loc[i,2])
  
  #distm function calculates distance between each single point in VPDES to all points from VWUDS
  #places output in rows to show that each point from VPDES list is compared to each points in VWUDS list.
  #distance calculated here is in kilometers and uses distHaversine function
  #distHaverine calculates the shortest distance between two spatial points assuming a spherical shaped earth. 
  # Assumes radius of earth is 6378137 m
  # It used the same units as r, so we are dealing with meters. However, I will divide by 1000 to work in km
  distance[i,]<-(distm(single_VPDES_facility,VWUDS_Loc, fun=distHaversine)/1000)
}

#Let's see what are results look like
      
# View(distance) #shows the distance in km from each VPDES facility (on left) to the VWUDS facilities (on top)

# write.csv(distance,paste0(path,'/distance.csv'))

#print(distance<=search_radius) #TRUE means the withdrawing facility falls within the search radius for the specified discharging facility 

#Create an empty table that will store matches from loop
search_radiusmatch<-matrix(nrow=nrow(VPDES_Loc), ncol=nrow(VWUDS_Loc))

#Now we want to see whether the distance falls within the search radius or not. 
#Remember that Discharging facilities are on the left and withdrawal facilities are across the top

search_radiusmatch<-distance<=search_radius

# write.csv(search_radiusmatch,paste0(path,'/search_radiusmatch.csv'))
########################################################################################################
####Matching Facilites Based on Name####
##Now we are going to narrow the list of facilites that fell within the search radius by facility name##
##This will be done through fuzzy matching since facilities are inputted by different poeple and 
#may be spelled differently/have extra characters/have capital letters/contain spaces/etc.##

########################################################################################################
##set up empty matrix to store results of search_radiusmatch in##
##it's bascially the same matrix, but we are manipulating it in subsequent steps

VWUDS.facilities.within.radius<-matrix(ncol=ncol(search_radiusmatch), nrow=nrow(search_radiusmatch))

for (r in 1:nrow(search_radiusmatch)){
  print(paste("Processing: ", r," of ",nrow(search_radiusmatch), sep=""))
  VWUDS.facilities.within.radius[r,]<-(search_radiusmatch[r,])# extracts the location-matching results for a single VWUDS location. Row by row analysis.
  ##the [r,] next to loc.results is an additional requirement to store locations row by row as well.
  colnames(VWUDS.facilities.within.radius)<-colnames(search_radiusmatch)
  
  #Reassign the row names to the VWUDS Facility names again to get a better visual of the narrowed down possible matches. 
  rownames(VWUDS.facilities.within.radius)<-rownames(search_radiusmatch)
  
  #We don't want to consider the facilities that didn't fall within the search radius, 
  #so lets just get rid of them in general by setting them to "-"
  VWUDS.facilities.within.radius[VWUDS.facilities.within.radius=="FALSE"]<-"-"
}
#Now let's rename all of the "TRUE" values to the corresponding VPDES facility to see our narrowed list. Have outside of for loop. 

VWUDS.facilities.within.radius[VWUDS.facilities.within.radius=="TRUE"]<-colnames(VWUDS.facilities.within.radius)[which(VWUDS.facilities.within.radius=="TRUE", arr.ind = TRUE)[,'col']]


# View(VWUDS.facilities.within.radius) #Take a peek: but remember to set this outside of loop or it will open as many tabs from 1:nrow(search_radiusmatch)

# write.csv(VWUDS.facilities.within.radius,paste0(path,'/VWUDS.facilities.within.radius.csv'))

VWUDS.facilities.within.radius<-unname(VWUDS.facilities.within.radius) #need to remove row names and column names to properly combine dataframes at end of code

########################################################################################################
####Now that we have are narrowed matrix of potential VPDES facilities for each VWUDS facility, we can partially match by name####

##But now that we have a larger matrix, we need to alter this code in such a way to go through each row comparing the 
# the row name (VWUDS facility) to the narrowed down VPDES facilities##
##In the end we will have a list of all VPDES facilites and potential VWUDS facilities that match them##

##This method of Fuzzy String Matching is adapted from bigdata-doctor.com##

for (r in 1:nrow(VWUDS.facilities.within.radius)){
  VWUDS.facilities.within.radius[r,]<-as.character(VWUDS.facilities.within.radius[r,])#specifying all components in each row are characters 
}
VPDES_Facilities<-rownames(search_radiusmatch)
VPDES_Facilities<-as.character(VPDES_Facilities)

#Create a function that takes the facility names and deletes spaces and unnecessary characters that may throw off comparison
trim<-function(x) gsub("^\\s+|\\s+$", "", x)

#Create custom adist function that will compute approximate string distance between VWUDS and VPDES facility names. 
#Sliding means that the function will "slide" over the largest string distance to take the minimum distance (better matches)
adist.custom<-function(strVWUDS,strVPDES, sliding=T)
{
  strVWUDS<-strsplit(trim(strVWUDS), split = '') 
  strVPDES<-strsplit(trim(strVPDES), split = '')
  strVPDES<-trim(unlist(strVPDES))
  strVWUDS<-trim(unlist(strVWUDS))
  
  #dictate whether character length between VPDES & VWUDS names are short or long.
  #sort accordingly
  if (length(strVPDES)>=length(strVWUDS))
  {
    short.str<-strVWUDS
    long.str<-strVPDES
  }else{
    short.str<-strVPDES
    long.str<-strVWUDS
  }
  
  #sliding
  return.dist<-0
  if(sliding==TRUE)
  {
    min<-99999
    sVWUDS<-trim(paste(short.str,collapse=''))
    for(k in 1:(length(long.str)-length(short.str)))
    {
      sVPDES<-trim(paste(long.str[k:(length(short.str)+(k-1))], collapse=''))
      
      #I'm using the adist function which computes the approximate string distance between characters
      #This basically looks at the weighted number of insertions, deletions, and substitutions 
      #needed for the VPDES facility name to exactly match the VWUDS facility name.
      #We indicate that it's fine for the match to be partial and not exact and to ignore the case of the names
      ads<-adist(sVWUDS,sVPDES, partial=TRUE, ignore.case = TRUE) #ignore.case ensures that all characters are compared, capitilized or not 
      #want minimum distance-which is "best" match
      min<-ifelse(ads<min,ads,min)
    }
    return.dist<-min
  }else{
    #strings start matching
    sVWUDS<-trim(paste(short.str, collapse=''))
    sVPDES<-trim(paste(long.str[1:length(short.str)], collapse=''))
    return.dist<-adist(sVWUDS, sVPDESm, partial=TRUE, ignore.case=TRUE)
  }
  return(return.dist) #end of creating character string comparison function
}
########################################################################################################
#Now we apply this custom adist function to our VPDES and VWUDS facility names
#This will show us on average, how many insertions, deletions, and substitutions it takes for the facilities names to be identical 
string_distance<-matrix(NA, nrow=length(VPDES_Facilities),ncol=ncol(VWUDS.facilities.within.radius)) #create matrix with same dimensions as search_radiusmatch to store character distances in
for(i in 1:ncol(VWUDS.facilities.within.radius)){
  for(j in 1:length(VPDES_Facilities)){
    if (VWUDS.facilities.within.radius[j,i]=="-"){ #create if statement to ignore NA values and just compare names
      string_distance[j,i]<-9999 #if the matrix has a missing or NA value, substitute with 9999 to exclude it from list of possible matches
    }else{
      string_distance[j,i]<-adist.custom(tolower(VPDES_Facilities[j]),tolower(VWUDS.facilities.within.radius[j,i])) #[i,] to indicate we want every VPDES name for each row to correspond with the single VWUDS facility in the row [j]
    }
    #tolower transalates all characters to lower case
  }
}


#find minimum string distance from caluclated string distances from above
#However we have cases where string distances are the same or very close.
#So let's also look at top three matches for each facility
#Returns Minimum String Distance Value
min_string_distance<-apply(string_distance,1,min) #the 1 refers to the margin. 1 indicates rows, 2 indicates columns, C(1,2) indicates rows and columns

min_haverstine_distance<-apply(distance,1,min)

##########################################################################################################
#This section of the code associates the three minimum string distance values with their corresponding facilities

matchVWUDSVPDES<-NULL
#Column names for the two dataframes must match in order to be combined: common error I encountered-rerun entire code if giving you error
names(matchVWUDSVPDES)
names(VWUDS.facilities.within.radius)
#If different, use unname function to remove headers from columns and row

for(i in 1:nrow(string_distance)){
  #Find corresponding location of minimum string distance in each line of string distance matrix
  Best_name_match<-match(min_string_distance[i],string_distance[i,])
  Best_dist_match<-match(min_haverstine_distance[i],distance[i,])
  #therefore the number you see is the position of the "matching" value
  VPDES_Facilities.i<-i
  matchVWUDSVPDES<-rbind(data.frame(VPDES_Name=VPDES_Facilities[VPDES_Facilities.i],
                                    VWUDS_Best_name_Match=VWUDS.facilities.within.radius[i,Best_name_match], 
                                    CharacterandDistance_Best_Match=min_string_distance[i], 
                                    VWUDS_Best_dist_match=VWUDS.facilities[i,Best_dist_match],
                                    Distance_km=min_haverstine_distance[i],
                                    stringsAsFactors = FALSE, row.names = ""), matchVWUDSVPDES)
  
}
#Let's look at the results
View(matchVWUDSVPDES)

#----Select matches that have a character distance less or equal to 10 (remember this includes facilities that fit within a 10km distance)---#
matchVWUDSVPDES$VWUDS_Best_name_Match<-ifelse(matchVWUDSVPDES$CharacterandDistance_Best_Match==9999,matchVWUDSVPDES$VWUDS_Best_dist_match,matchVWUDSVPDES$VWUDS_Best_name_Match)
matchVWUDSVPDES<-subset(matchVWUDSVPDES,subset=matchVWUDSVPDES$CharacterandDistance_Best_Match<9999, select=-c(4))
colnames(matchVWUDSVPDES)[2]<-c("VWUDS_Name")

# VWUDS_info<-subset(VWUDS,VWUDS$Facility_Name%in%matchVWUDSVPDES$VWUDS_Name,select=-c(3,4,10,11))

VWUDS_info<-subset(VWUDS,select=-c(3,4,8:17,19,21,22))
colnames(VWUDS_info)<-c("VWUDS_HydroID","VWUDS_Name","VWUDS_Use","VWUDS_Source","VWUDS_County","VWUDS_Ave_2010_2017","VWUDS_perc_total")

# VPDES_info<-subset(VPDES,VPDES$Facility_Name%in%matchVWUDSVPDES$VPDES_Name,select=-c(3,4,6,7,11,12))
VPDES_info<-subset(VPDES,select=-c(3,4,6,7,9:17,19,21,22))
colnames(VPDES_info)<-c("VPDES_Hydrocode","VPDES_Name","VPDES_Use","VPDES_County","VPDES_Ave_2010_2017","VPDES_perc_total")

matchVWUDSVPDES<-merge(matchVWUDSVPDES,VPDES_info,by="VPDES_Name",all.x=T)
matchVWUDSVPDES<-merge(matchVWUDSVPDES,VWUDS_info,by="VWUDS_Name",all.x=T)


matchVWUDSVPDES<-matchVWUDSVPDES[c("VPDES_Hydrocode","VPDES_Name","VWUDS_HydroID","VWUDS_Name","Distance_km","VWUDS_Source","VPDES_County","VWUDS_County",
                  "VPDES_Use","VWUDS_Use","VPDES_perc_total","VWUDS_perc_total",
                  "VPDES_Ave_2010_2017","VWUDS_Ave_2010_2017")]

matchVWUDSVPDES<-matchVWUDSVPDES%>%arrange(desc(VWUDS_perc_total))

#---------Consumption-------#

matchVWUDSVPDES$Ave_Consumption<-(matchVWUDSVPDES$VWUDS_Ave_2010_2017-matchVWUDSVPDES$VPDES_Ave_2010_2017)/matchVWUDSVPDES$VWUDS_Ave_2010_2017


write.csv(matchVWUDSVPDES,paste0(path,'/matchVWUDSVPDES.csv'))
########################################################################################################


