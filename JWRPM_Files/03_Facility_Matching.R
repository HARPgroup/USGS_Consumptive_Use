########################################################################################################
####Matching VWUDS and VPDES Facilities####

## This code serves to match withdrawal facilites (State Maintained) to a discharge facility (Federally Maintained)
## Some withdrawal facilties will not having a matching discharge facility due to its water use type 
## (agriculture, livestock, etc.) Therefore we are starting with discharge facilities and finding a match from 
## the list of withdrawal facilities 

## There are two methods to matching: 
## 1:First is to create a search radius from each discharge facility and then indicate the withdrawal facilites that fall within that radius. 
## 2: Then we take the facilities that fall within the search radius and match them by facility name (using fuzzy logic)

########################################################################################################
####Load Necessary Packages####

setwd("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis_PredictiveModels/Facility_Matching")

#Clean up environment
rm(list=objects())

library(sp)
library(rgeos)
library(geosphere)
library(xtable)
library(dplyr)

options(scipen=999) #disable scientific notation 


path<-"G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis_PredictiveModels/Facility_Matching"

########################################################################################################
#### Load in discharge and withdrawal time series data

#Existing_Matches<-read.csv("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Matched Facilities/Runninglist_Matches.csv",header=T)

VPDES<-read.table("ECHO_2010_2017_QAQC.txt", sep="\t", header=T)
VPDES.DMR<-VPDES

# Extract unique VPDES facilities
VPDES<-VPDES%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(Facility_Name=first(FacilityName), Longitude=mean(Facility_Longitude, na.rm=T),
                                                               Latitude=mean(Facility_Latitude, na.rm=T), Use_type=first(Reclass_Use_Type),Design_Flow=first(DesignFlow_mgd), 
                                                               City=first(City), County=first(County))

#load(paste0(path,"/cum_dis.RData"))  # can't run this, can't find RData file in github files
#colnames(Discharge)[1]<-c("Facility.ID")
# VPDES<-merge(VPDES,Discharge,by=c("Facility.ID","Facility_Name"),all.x=T)
#   Merges VPDES facilities with some measure of average discharge, but unclear how this average discharge is calculated
#   Need to search through Morgan's other scripts
#   Ignore it for now

#--------------Withdrawing Facilities in VWUDS Database
load("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/Analysis_PredictiveModels/Facility_Matching/VWUDS_2010_2017.RData")
VWUDS<-VWUDS_2010_2017
VWUDS<-mutate_if(VWUDS,is.factor,as.character)
VWUDS$Owner_Facility<-do.call(paste, c(VWUDS[c("Owner","Facility")],sep=":"))

VWUDS<-VWUDS%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(Facility_Name=first(Owner_Facility),Longitude=mean(Corrected_Longitude, na.rm=T),
                                                               Latitude=mean(Corrected_Latitude, na.rm=T), Use_type=first(Reclass_Use_Type), 
                                                               Source=first(Source.Type), County=first(Locality))
VWUDS$Use_type<-as.factor(VWUDS$Use_type)
colnames(VWUDS)[2]<-"Facility_Name"
VWUDS$Facility_Name<-toupper(VWUDS$Facility_Name)
VPDES$Facility_Name<-toupper(VPDES$Facility_Name)

#load(paste0(path,"/cum_with.RData")) # can't run this, can't find RData file in github files
#colnames(Withdrawal)[1]<-c("Facility.ID")
#VWUDS<-merge(VWUDS,Withdrawal,by=c("Facility.ID"),all.x=T)

## Summarize facility types in each dataset
summary(VPDES$Use_type)
summary(VWUDS$Use_type)

########################################################################################################
####Narrowing Facilities Based on Distance####
########################################################################################################
## Setting up loop to go through list of VPDES facilities (One by One) to calculate distance to each VWUDS facility. 
## Then we mark those that fall within a set search radius####

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

#Create an empty table that will store matches from loop
search_radiusmatch<-matrix(nrow=nrow(VPDES_Loc), ncol=nrow(VWUDS_Loc))

#Now we want to see whether the distance falls within the search radius or not. 
#Remember that Discharging facilities are on the left and withdrawal facilities are across the top

search_radiusmatch<-distance<=search_radius

## Count how many VWUDS facilities are within radius for each VPDES facility
MatchCount_dist<-data.frame(Facility=rownames(search_radiusmatch), 
                            VPDES.UseType=VPDES$Use_type,  
                            Nmatch=rep(-999,dim(search_radiusmatch)[1]))
for (f in 1:dim(search_radiusmatch)[1]){
  MatchCount_dist[f,3]<-sum(search_radiusmatch[f,])
}
hist(MatchCount_dist[,3]) # majority of facilities (544) have less than 10 distance matches
sum(MatchCount_dist[,3]<10)

# non-municipal facilities only
MatchCount.nonMun<-subset(MatchCount_dist,!VPDES.UseType == "Municipal")  # 358 facilities 
MatchCount.nonMun<-subset(MatchCount.nonMun, Nmatch>0)  # remove facilities with no withdrawing facility within 10 km
sum(MatchCount.nonMun[,3]<10) # majority of facilities (267, 76%) have less than 10 distance matches
hist(MatchCount.nonMun[,3])
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

dim(VWUDS.facilities.within.radius)

########################################################################################################
#### Name Matching
###########################################################################################################

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

## Create list of potential matches for non-municipal facilities
## List length = 350 (number of non-municipal VPDES facilities with at least one withdrawing facility within 10 km)
## Each list is data frame with information on potential VWUDS matches (name, use type, distance, and string distance)

match.list<-list()
for (f in 1:dim(MatchCount.nonMun)[1]){
  VPDES.FacID<-MatchCount.nonMun[f,1]
  fac.rowID<-match(VPDES.FacID,MatchCount_dist[,1]) # identify row in larger tables (including municipal) that correspond to that facility
  n.match<-MatchCount.nonMun[f,3]
  fill<-rep(-999,n.match)
  
  # Create empty data frame and populate with VWUDS info 
  match.data<-data.frame(Facility=fill, Use.type=fill, distance=fill, st.distance=fill)
  match.data$Facility <-  colnames(search_radiusmatch)[ search_radiusmatch[fac.rowID,] == TRUE]
  match.data$distance <-  distance[fac.rowID, search_radiusmatch[fac.rowID,] == TRUE]
  match.data$st.distance <-  string_distance[fac.rowID, search_radiusmatch[fac.rowID,] == TRUE]
  
  # Add in VWUDS use types
  for (f2 in 1:n.match){
    match.data$Use.type[f2]<-paste(VWUDS$Use_type[VWUDS$Facility_Name==match.data$Facility[f2] ])
  }
  match.list[[f]]<-match.data
}

## Loop through - identify easy matches
## Easy match occurs if only one withdrawing facility is within 10 km, or if best distance and st distance matches are same facility
fill<-rep(NA, dim(MatchCount.nonMun)[1])
match.summary<-data.frame(VPDES.FacName=MatchCount.nonMun$Facility, VPDES.UseType=MatchCount.nonMun$VPDES.UseType, Nmatch=MatchCount.nonMun$Nmatch, Easy.match=fill, 
                          Match1.FacName=fill, Match1.UseType=fill, Match1.distance=fill, Match1.stdist=fill, 
                          Match2.FacName=fill, Match2.UseType=fill, Match2.distance=fill, Match2.stdist=fill)
for (f in 1:dim(MatchCount.nonMun)[1]){
  match.table<-match.list[[f]]
  # Remove municipal and agricultural VWUDS facilities
  match.table.trim<-subset(match.table,!Use.type=="Agriculture/Irrigation")
  match.table.trim<-subset(match.table.trim,!Use.type=="Municipal")
  match.table<-match.table.trim
  if(dim(match.table)[1] == 0){
    match.summary$Easy.match[f]<-"FALSE"
  }else{
    if (dim(match.table)[1] == 1){
      match.summary$Easy.match[f]<-"TRUE"
      match.summary$Match1.FacName[f]<-match.table$Facility[1]
      match.summary$Match1.UseType[f]<-match.table$Use.type[1]
      match.summary$Match1.distance[f]<-match.table$distance[1]
      match.summary$Match1.stdist[f]<-match.table$st.distance[1]
    }else{
      best.dist<-match.table$Facility[which.min(match.table$distance)]
      best.stdist<-match.table$Facility[which.min(match.table$st.distance)]
      # Match 1 is best match based on distance
      match.summary$Match1.FacName[f]<-match.table$Facility[which.min(match.table$distance)]
      match.summary$Match1.UseType[f]<-match.table$Use.type[which.min(match.table$distance)]
      match.summary$Match1.distance[f]<-match.table$distance[which.min(match.table$distance)]
      match.summary$Match1.stdist[f]<-match.table$st.distance[which.min(match.table$distance)]
      if (best.dist == best.stdist){
        match.summary$Easy.match[f]<-"TRUE" # if match is easy, no second match
      }else{
        match.summary$Easy.match[f]<-"FALSE"  # if match is not easy, second is based on string distance
        match.summary$Match2.FacName[f]<-match.table$Facility[which.min(match.table$st.distance)]
        match.summary$Match2.UseType[f]<-match.table$Use.type[which.min(match.table$st.distance)]
        match.summary$Match2.distance[f]<-match.table$distance[which.min(match.table$st.distance)]
        match.summary$Match2.stdist[f]<-match.table$st.distance[which.min(match.table$st.distance)]
      }
    }
  }
}

sum(match.summary$Easy.match=="TRUE") #192 matches classified as easy

# save to table for manual review
write.csv(match.summary,file="MatchSummary_TrimMatches.csv",row.names=FALSE)
# manually check results and confirm in excel file "MatchSummary_Checked.xlsx"

## Load in list of unmatched facilities from manual review
match.check<-read.csv("MatchSummary_TrimMatches_Checked.csv")
unmatched.fac<-subset(match.check,Matched.=="N")

## Loop through unmatched facilities and make table with withdrawing facilities within 10k
vpdes.list<-as.character(MatchCount.nonMun$Facility)
unmatch.summary<-data.frame(Facility=character(), Use.type=character(), distance=numeric(), st.distance=numeric(), 
                            VPDES.fac=character(), VPDES.use=character() ) 
for (f in 1:dim(unmatched.fac)[1]){
  vpdes.fac<-unmatched.fac$VPDES.FacName[f]
  vpdes.use<-unmatched.fac$VPDES.UseType[f]
  vpdes.row<-which(vpdes.list==vpdes.fac)
  match.table<-match.list[[vpdes.row]]
  match.table$VPDES.fac<-rep(vpdes.fac,dim(match.table)[1])
  match.table$VPDES.use<-rep(vpdes.use,dim(match.table)[1])
  unmatch.summary<-rbind(unmatch.summary,match.table)
}

# save to table for manual review
write.csv(unmatch.summary,file="UnMatchDetails.csv",row.names=FALSE)
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

matches.trim<-subset(matchVWUDSVPDES,CharacterandDistance_Best_Match<10)

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


