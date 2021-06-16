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

setwd("G:/My Drive/VT/Research/Virginia/USGS_WUDR_ConsumptiveUse/MorgansCode/GithubFiles/GithubFiles/Facility Matching")

#Clean up environment
rm(list=objects())

library(sp)
library(rgeos)
library(geosphere)
library(xtable)
library(dplyr)

#options(scipen=999) #disable scientific notation 


#path<-"G:/My Drive/GRA/RCode for VWUDS and VPDES Analysis"

VPDES<-read.table("ECHO_2010_2017_QAQC.txt", sep="\t", header=T)

#load(paste0(path,"/cum_dis.RData"))
#colnames(Discharge)[1]<-c("Facility.ID")
VPDES<-VPDES%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(Facility_Name=first(FacilityName), Longitude=mean(Facility_Longitude, na.rm=T),
                                                               Latitude=mean(Facility_Latitude, na.rm=T), Use_type=first(Reclass_Use_Type),Design_Flow=first(DesignFlow_mgd), 
                                                               City=first(City), County=first(County))


# Remove municipal facilities
summary(VPDES$Use_type)
VPDES.trim <- VPDES[VPDES$Use_type != "Municipal" ,]
VPDES.trim <- VPDES.trim[!is.na(VPDES.trim$Use_type) ,]
VPDES <- VPDES.trim

#VPDES<-merge(VPDES,Discharge,by=c("Facility.ID","Facility_Name"),all.x=T)

#--------------Withdrawing Facilities in VWUDS Database
load("VWUDS_2010_2017.RData")
VWUDS<-VWUDS_2010_2017
VWUDS<-mutate_if(VWUDS,is.factor,as.character)
VWUDS$Owner_Facility<-do.call(paste, c(VWUDS[c("Owner","Facility")],sep=":"))

VWUDS<-VWUDS%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(Facility_Name=first(Owner_Facility),Longitude=mean(Corrected_Longitude, na.rm=T),
                                                               Latitude=mean(Corrected_Latitude, na.rm=T), Use_type=first(Reclass_Use_Type), 
                                                               Source=first(Source.Type), County=first(Locality))

colnames(VWUDS)[2]<-"Facility_Name"
VWUDS$Facility_Name<-toupper(VWUDS$Facility_Name)
VPDES$Facility_Name<-toupper(VPDES$Facility_Name)

## Remove municipal and agricultural/irrigation facilities
## Now it just includes commercial, industrial, energy, aquaculture
VWUDS.trim <- VWUDS[VWUDS$Use_type != "Municipal" ,]
VWUDS.trim <- VWUDS.trim[VWUDS.trim$Use_type != "Agriculture/Irrigation" ,]
VWUDS <- VWUDS.trim

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
      
View(distance) #shows the distance in km from each VPDES facility (on left) to the VWUDS facilities (on top)

# write.csv(distance,paste0(path,'/distance.csv'))

#print(distance<=search_radius) #TRUE means the withdrawing facility falls within the search radius for the specified discharging facility 

## Calculate number of facilities within search radius for each VPDES facility
distance.matches<-rep(NA,dim(distance)[1])
for (v in 1:dim(distance)[1]){
  distance.matches[v] <- sum(distance[v,] < search_radius)
}
hist(distance.matches)
summary(distance.matches)
summary(distance.matches == 0) # 56 facilities with no distance matches

## By sector - number of facilities
sum(VPDES$Use_type == "Commercial")
sum(VPDES$Use_type == "Industrial")
sum(VPDES$Use_type == "Energy")
sum(VPDES$Use_type == "Aquaculture")

## By sector - at least one distance match
sectors <- c("Aquaculture", "Commercial", "Energy", "Industrial")
for (s in 1:length(sectors)){
  sector.distance <- distance.matches [VPDES$Use_type == sectors[s] ]
  print (sectors[s])
  print(summary (sector.distance == 0))
  print(summary(sector.distance))
}

sum(VPDES$Use_type == "Commercial")
sum(VPDES$Use_type == "Industrial")
sum(VPDES$Use_type == "Energy")
sum(VPDES$Use_type == "Aquaculture")

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
  #print(paste("Processing: ", r," of ",nrow(search_radiusmatch), sep=""))
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
    if (distance[j,i] > search_radius){ #create if statement to ignore NA values and just compare names
      string_distance[j,i]<-9999 #if the matrix has a missing or NA value, substitute with 9999 to exclude it from list of possible matches
    }else{
      string_distance[j,i]<-adist.custom(tolower(VPDES_Facilities[j]),tolower(VWUDS.facilities[j,i])) #[i,] to indicate we want every VPDES name for each row to correspond with the single VWUDS facility in the row [j]
    }
    #tolower transalates all characters to lower case
  }
}

string.min <- apply(string_distance,1,min, na.rm=TRUE)
sum(string.min == 9999) #56 with 9999 value
summary(string.min[string.min != 9999])
hist(string.min[string.min != 9999])

## String distance by sector
for (s in 1:length(sectors)){
  sector.string <- string.min [VPDES$Use_type == sectors[s] ]
  sector.string <- sector.string[ sector.string != 9999]
  print (sectors[s])
  print(summary(sector.string))
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

#----Select matches that have a character distance less or equal to 10 (remember this includes facilities that fit within a 10km distance)---#
matchVWUDSVPDES$VWUDS_Best_name_Match<-ifelse(matchVWUDSVPDES$CharacterandDistance_Best_Match==9999,matchVWUDSVPDES$VWUDS_Best_dist_match,matchVWUDSVPDES$VWUDS_Best_name_Match)
matchVWUDSVPDES<-subset(matchVWUDSVPDES,subset=matchVWUDSVPDES$CharacterandDistance_Best_Match<9999, select=-c(4))
colnames(matchVWUDSVPDES)[2]<-c("VWUDS_Name")

matchVWUDSVPDES.long <- matchVWUDSVPDES

## Compare to final (manually-reviewed) list of matches (existing_matches)
Existing_Matches<-read.csv("Runninglist_Matches.csv",header=T)
## Remove municipal matches
Final_matches<-Existing_Matches[Existing_Matches$Sector != "Municipal" ,]
dim(Final_matches)  # 123 matches total

# Final matches by sector
sum(Final_matches$Sector == "Aquaculture")
sum(Final_matches$Sector == "Commercial")
sum(Final_matches$Sector == "Energy")
sum(Final_matches$Sector == "Industrial")

## Results from minimum string distance within 10km
dim(matchVWUDSVPDES) # selects match each facility - all 302 with facility in 10km
summary(matchVWUDSVPDES$VPDES_Name %in% Final_matches$VPDES.Name)
  # 106 included, 196 removed
  # this is the results of Morgan's manual review
summary(Final_matches$VPDES.Name %in% matchVWUDSVPDES$VPDES_Name)
  # False 16, true 107

## Add additional selection steps - matches where distance and string distance are the same
match.trim <- matchVWUDSVPDES.long[matchVWUDSVPDES.long$VWUDS_Best_name_Match == matchVWUDSVPDES.long$VWUDS_Best_dist_match ,]
dim(match.trim) # 192 facilities have matching distance and character string
dim(matchVWUDSVPDES.long)

# Average distance and string distance
summary(match.trim$CharacterandDistance_Best_Match)
summary(match.trim$Distance_km)

hist(match.trim$Distance_km) # majority are within one km
hist(match.trim$CharacterandDistance_Best_Match) # evenly distributed between 0 and 25

## False positives and negatives
summary(Final_matches$VPDES.Name %in% match.trim$VPDES_Name)

############################################################
## Add one more step - make sectors consistent
## Run matching procedure based on consistent sectors only
############################################################

VPDES.aq<- VPDES.trim[VPDES.trim$Use_type == "Aquaculture" ,]
VPDES.com<- VPDES.trim[VPDES.trim$Use_type == "Commercial" ,]
VPDES.en<- VPDES.trim[VPDES.trim$Use_type == "Energy" ,]
VPDES.ind<- VPDES.trim[VPDES.trim$Use_type == "Industrial" ,]

VWUDS.aq <- VWUDS.trim[VWUDS.trim$Use_type == "Aquaculture" ,]
VWUDS.com <- VWUDS.trim[VWUDS.trim$Use_type == "Commercial",]
VWUDS.en <- VWUDS.trim[VWUDS.trim$Use_type == "Energy",]
VWUDS.ind <- VWUDS.trim[VWUDS.trim$Use_type == "Industrial",]

sector.match <- function(VPDES.data,VWUDS.data){
  ## Create distance matrix
  distance.matrix <- matrix(nrow=nrow(VPDES.data), ncol=nrow(VWUDS.data))
  rownames(distance.matrix)<-VPDES.data$Facility_Name
  colnames(distance.matrix)<-VWUDS.data$Facility_Name
  
  ## Extract lat/lon
  VPDES_Loc<-matrix(c(VPDES.data$Longitude,VPDES.data$Latitude), ncol=2)
  colnames(VPDES_Loc)<-c("Longitude", "Latitude")
  VWUDS_Loc<-matrix(c(VWUDS.data$Longitude,VWUDS.data$Latitude), ncol=2)
  colnames(VWUDS_Loc)<-c("Longitude", "Latitude")
  
  distance.matches <- c()
  for(i in 1:nrow(VPDES.data)){
    single_VPDES_facility<-c(VPDES_Loc[i,1], VPDES_Loc[i,2])
    distance.matrix[i,]<-(distm(single_VPDES_facility,VWUDS_Loc, fun=distHaversine)/1000)
    distance.matches[i] <- sum(distance.matrix[i,] < search_radius)
  }
  
  ## String distance
  sd.matrix <- matrix(nrow=nrow(VPDES.data), ncol=nrow(VWUDS.data))
  rownames(sd.matrix)<-VPDES.data$Facility_Name
  colnames(sd.matrix)<-VWUDS.data$Facility_Name
  
  for(i in 1:nrow(VWUDS.data)){
    for(j in 1:nrow(VPDES.data)){
      if (distance.matrix[j,i] > search_radius){ #create if statement to ignore NA values and just compare names
        sd.matrix[j,i]<-9999 #if the matrix has a missing or NA value, substitute with 9999 to exclude it from list of possible matches
      }else{
        sd.matrix[j,i]<-adist.custom(tolower(VPDES.data$Facility_Name[j]),
                                           tolower(VWUDS.data$Facility_Name[i])) #[i,] to indicate we want every VPDES name for each row to correspond with the single VWUDS facility in the row [j]
      }
    }
  }
  
  min_sd<-apply(sd.matrix,1,min) #the 1 refers to the margin. 1 indicates rows, 2 indicates columns, C(1,2) indicates rows and columns
  min_distance<-apply(distance.matrix,1,min)
  
  ## Create match matrix
  sector.matches<-NULL
  for(i in 1:nrow(sd.matrix)){
    #Find corresponding location of minimum string distance in each line of string distance matrix
    Best_name_match<-match(min_sd[i],sd.matrix[i,])
    Best_dist_match<-match(min_distance[i],distance.matrix[i,])
    #therefore the number you see is the position of the "matching" value
    VPDES_Facilities.i<-i
    sector.matches<-rbind(data.frame(VPDES_Name=VPDES.data$Facility_Name[i],
                                      VWUDS_Best_name_Match=VWUDS.data$Facility_Name[Best_name_match], 
                                      CharacterandDistance_Best_Match=min_sd[i], 
                                      VWUDS_Best_dist_match=VWUDS.data$Facility_Name[Best_dist_match],
                                      Distance_km=min_distance[i],
                                      stringsAsFactors = FALSE, row.names = ""), sector.matches)
  }
  return (list(sector.matches=sector.matches,
               distance.matches=distance.matches, 
               min_sd=min_sd))
  # distance.matches shows the number of wd facilities within 10 km for each VDPES facility
  # min_sd shows the minimum string distance of close (<10km) facilities for each VPDES facility
  # sector.matches is the table of match results
}

matches.aq<-sector.match(VPDES.data=VPDES.aq, VWUDS.data=VWUDS.aq)
matches.com<-sector.match(VPDES.data=VPDES.com, VWUDS.data=VWUDS.com)
matches.en<-sector.match(VPDES.data=VPDES.en, VWUDS.data=VWUDS.en)
matches.ind<-sector.match(VPDES.data=VPDES.ind, VWUDS.data=VWUDS.ind)

## Distance info for table 
sum(matches.aq$distance.matches > 0)
summary(matches.aq$distance.matches)

sum(matches.com$distance.matches > 0)
summary(matches.com$distance.matches)

sum(matches.en$distance.matches > 0)
summary(matches.en$distance.matches)

sum(matches.ind$distance.matches > 0)
summary(matches.ind$distance.matches)

## String Distance info for table 
summary(matches.aq$min_sd[matches.aq$min_sd < 9999])
summary(matches.com$min_sd[matches.com$min_sd < 9999])
summary(matches.en$min_sd[matches.en$min_sd < 9999])
summary(matches.ind$min_sd[matches.ind$min_sd < 9999])

## Number of matches by sector
sum(matches.aq$sector.matches$VWUDS_Best_name_Match == 
      matches.aq$sector.matches$VWUDS_Best_dist_match)
sum(matches.com$sector.matches$VWUDS_Best_name_Match == 
      matches.com$sector.matches$VWUDS_Best_dist_match)
sum(matches.en$sector.matches$VWUDS_Best_name_Match == 
      matches.en$sector.matches$VWUDS_Best_dist_match)
sum(matches.ind$sector.matches$VWUDS_Best_name_Match == 
      matches.ind$sector.matches$VWUDS_Best_dist_match)

## Merge results into single table
matches.all <- rbind (matches.aq$sector.matches, matches.com$sector.matches, 
                      matches.en$sector.matches, matches.ind$sector.matches)
matches.all.trim <- matches.all[matches.all$VWUDS_Best_name_Match == 
                                  matches.all$VWUDS_Best_dist_match ,]

## Number of matches by sector
dim(matches.all.trim)

## False positive and negative rates
########################################

n.facility <- dim(VPDES)

## True matches
n.match <- dim(Final_matches)[1]
n.nonmatch <- dim(VPDES)[1] - n.match

## Test results (pos indicates match found)
n.test.pos <- dim(matches.all.trim)[1]
n.test.neg <- dim(VPDES)[1] - n.test.pos

## Positive test results
n.pos.pos <- sum(Final_matches$VPDES.Name %in% matches.all.trim$VPDES_Name)
n.neg.pos <- n.test.pos - n.pos.pos  # neg true, pos test

## Negative test results
n.pos.neg <- n.match - n.pos.pos# pos true, neg test
n.neg.neg <- n.test.neg - n.pos.neg

false.pos <- n.neg.pos/n.test.pos
false.neg <- n.pos.neg/n.test.neg

summary(Final_matches$VPDES.Name %in% matches.all.trim$VPDES_Name)
length(levels(Final_matches$VPDES.Name))
# 96 true, 27 false
summary(matches.all.trim$VPDES_Name %in% Final_matches$VPDES.Name)
# 95 true, 90 false

## Which one doesn't match up
both.match1 <- Final_matches$VPDES.Name[Final_matches$VPDES.Name %in% matches.all.trim$VPDES_Name]
both.match2 <- matches.all.trim$VPDES_Name[matches.all.trim$VPDES_Name %in% Final_matches$VPDES.Name]
both.match1 [!(both.match1 %in% both.match2)]

summary(Final_matches$VPDES.Name %in% match.trim$VPDES_Name)
sum(!(Final_matches$VPDES.Name %in% match.trim$VPDES_Name))/length(Final_matches$VPDES.Name)
  # 90 final matches included in match results
  # False negatives (algorithm - but final +): 33/198 = 53.6%



## Code below is incorporating additional info (withdrawal, discharge etc.)
# VWUDS_info<-subset(VWUDS,VWUDS$Facility_Name%in%matchVWUDSVPDES$VWUDS_Name,select=-c(3,4,10,11))

#VWUDS_info<-subset(VWUDS,select=-c(3,4,8:17,19,21,22))
#colnames(VWUDS_info)<-c("VWUDS_HydroID","VWUDS_Name","VWUDS_Use","VWUDS_Source","VWUDS_County","VWUDS_Ave_2010_2017","VWUDS_perc_total")

# VPDES_info<-subset(VPDES,VPDES$Facility_Name%in%matchVWUDSVPDES$VPDES_Name,select=-c(3,4,6,7,11,12))
#VPDES_info<-subset(VPDES,select=-c(3,4,6,7,9:17,19,21,22))
#colnames(VPDES_info)<-c("VPDES_Hydrocode","VPDES_Name","VPDES_Use","VPDES_County","VPDES_Ave_2010_2017","VPDES_perc_total")

#matchVWUDSVPDES<-merge(matchVWUDSVPDES,VPDES_info,by="VPDES_Name",all.x=T)
#matchVWUDSVPDES<-merge(matchVWUDSVPDES,VWUDS_info,by="VWUDS_Name",all.x=T)


#matchVWUDSVPDES<-matchVWUDSVPDES[c("VPDES_Hydrocode","VPDES_Name","VWUDS_HydroID","VWUDS_Name","Distance_km","VWUDS_Source","VPDES_County","VWUDS_County",
#                  "VPDES_Use","VWUDS_Use","VPDES_perc_total","VWUDS_perc_total",
#                  "VPDES_Ave_2010_2017","VWUDS_Ave_2010_2017")]

#matchVWUDSVPDES<-matchVWUDSVPDES%>%arrange(desc(VWUDS_perc_total))

#---------Consumption-------#

#matchVWUDSVPDES$Ave_Consumption<-(matchVWUDSVPDES$VWUDS_Ave_2010_2017-matchVWUDSVPDES$VPDES_Ave_2010_2017)/matchVWUDSVPDES$VWUDS_Ave_2010_2017


#write.csv(matchVWUDSVPDES,paste0(path,'/matchVWUDSVPDES.csv'))
########################################################################################################


