#Require flow frame flipped to be generated from AnalysisCode.R
#######################
#Library Initialization
library(XML)
library(jsonlite)
library(RCurl)
state<-"VA"
#Get ECHO Facility List and store in dataframe 'a'
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state,"&p_tribedist=0")
ECHO_xml<-getURL(uri_query)
ECHO_query<-xmlParse(ECHO_xml)
QID<-xmlToList(ECHO_query)
QID<-QID$QueryID
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID)
a<-read.csv(uri_summary,stringsAsFactors = F)
a$CWPName<-toupper(a$CWPName)

for (i in 1:length(a$CWPName)){
  json_file<-paste0("https://ofmpub.epa.gov/echo/dfr_rest_services.get_dfr?output=JSON&p_id=",a$SourceID[i])
  json_data<-fromJSON(txt=json_file)
  if(length(json_data$Results$SpatialMetadata$Latitude83)>0){
    a$Faclat[i]<-json_data$Results$SpatialMetadata$Latitude83
    a$Faclong[i]<-json_data$Results$SpatialMetadata$Longitude83
  } else {
    a$Faclat[i]<-NA
    a$Faclong[i]<-NA
  }
  print(paste0("Processing SourceID: ",a$VAP_PMT_NO[i]," (",i," of ",length(a$CWPName),")"))
}


#Facility Generation
facilities<-data.frame(bundle='facility',name=a$CWPName)
facilities$ftype<-'unknown'
facilities$hydrocode<-paste0("echo_",a$SourceID)
for (i in 1:length(facilities$hydrocode)){
  if (length(grep('WTP',a$CWPName[i]))>0|
      length(grep('WASTE WATER',a$CWPName[i]))>0|
      length(grep('WATER TREATMENT PLANT',a$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT PLANT',a$CWPName[i]))>0|
      length(grep('WATER RECLAMATION',a$CWPName[i]))>0|
      length(grep('WTF',a$CWPName[i]))>0|
      length(grep('STP',a$CWPName[i]))>0|
      length(grep('SEW. TREAT. PLANT',a$CWPName[i]))>0|
      length(grep('WWTREAT PLANT',a$CWPName[i]))>0|
      length(grep('WATER TREATMEN',a$CWPName[i]))>0|
      length(grep('WATER TREATMENT PL',a$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT',a$CWPName[i]))>0|
      length(grep('POLLUTION CONTROL',a$CWPName[i]))>0|
      length(grep('SEWAGE TREATMENT PLAN',a$CWPName[i]))>0|
      length(grep('POLLUTION CONTR',a$CWPName[i]))>0|
      length(grep('WASTEWATE',a$CWPName[i]))>0|
      length(grep('WT PLANT',a$CWPName[i]))>0){
    facilities$ftype[i]<-'wwtp'
  } else if (length(grep('COMBINED SEW SYSTEM',a$CWPName[i]))|
             length(grep('COMBINED SEWER SYSTEM',a$CWPName[i]))>0){
    facilities$ftype[i]<-'public water supply'
  } else if (length(grep("POWER",a$CWPName[i]))>0|
             length(grep("ENERGY CENTER",a$CWPName[i]))>0|
             length(grep("ELECTRIC",a$CWPName[i]))>0){
    facilities$ftype[i]<-'fossilpower'
    if(length(grep("NUCLEAR",a$CWPName[i]>0))){
      facilities$ftype[i]<-"nuclearpower"
    } else if(length(grep("HYDRO",a$CWPName[i]>0))){
      facilities$ftype[i]<-"hydropower"
    }
  } else if(length(grep('NUCLEAR',a$CWPName[i]))>0){
    facilities$ftype[i]<-'nuclearpower' 
  }else if (length(grep("MINE",a$CWPName[i]))>0|
            length(grep("QUARRY",a$CWPName[i]))>0|
            length(grep("MINING",a$CWPName[i]))>0){
    facilities$ftype[i]<-'mining'
  }else if (length(grep("MS4",a$CWPName[i]))>0|
           length(grep("SCHOOL",a$CWPName[i]))>0|
           length(grep("TOWN",a$CWPName[i]))>0|
           length(grep("COMMUNITY",a$CWPName[i]))>0|
           length(grep('ARMY',a$CWPName[i]))>0|
           length(grep('NAVY',a$CWPName[i]))>0|
           length(grep("COUNTY",a$CWPName[i]))>0|
           length(grep("HOME",a$CWPName[i]))>0|
           length(grep("CHURCH",a$CWPName[i]))>0|
           length(grep("HOMES",a$CWPName[i]))>0|
           length(grep("MUSEUM",a$CWPName[i]))>0|
           length(grep("ESTATES",a$CWPName[i]))>0|
           length(grep("CAR WASH",a$CWPName[i]))>0|
           length(grep("LANDING",a$CWPName[i]))>0|
           length(grep("CORRECTION CENTER",a$CWPName[i]))>0|
           length(grep("DETENTION CENTER",a$CWPName[i]))>0|
           length(grep("CORRECTIONAL CENTER",a$CWPName[i]))>0|
           length(grep("CORRECTIONAL UNIT",a$CWPName[i]))>0|
           length(grep("VILLAGE",a$CWPName[i]))>0|
           length(grep("UNIVERSITY",a$CWPName[i]))>0|
           length(grep("HOSPITAL",a$CWPName[i]))>0|
           length(grep("RESTAURANT",a$CWPName[i]))>0|
           length(grep('AUTHORITY',a$CWPName[i]))>0|
           length(grep('TUNNEL',a$CWPName[i]))>0|
           length(grep("CITY OF",a$CWPName[i]))>0){
    facilities$ftype[i]<-'municipal'
  } else if(length(grep("FARM",a$CWPName[i]))>0|
            length(grep("FISH CULTURAL",a$CWPName[i]))>0){
    facilities$ftype[i]<-"agriculture"
  }else if(length(grep("CORPORATION",a$CWPName[i]))>0|
            length(grep("INC.",a$CWPName[i]))>0|
            length(grep("INCORPORATED",a$CWPName[i]))>0|
            length(grep('CORP.',a$CWPName[i]))>0|
            length(grep('LLC',a$CWPName[i]))>0|
            length(grep('AIRPORT',a$CWPName[i]))>0|
            length(grep("COMPANY",a$CWPName[i]))>0){
    facilities$ftype[i]<-'commercial'
  } else if(length(grep('PAPER',a$CWPName[i]))>0|
            length(grep('COMPANY',a$CWPName[i]))>0|
            length(grep('CONCRETE',a$CWPName[i]))>0|
            length(grep('WOOD',a$CWPName[i]))>0|
            length(grep('LUMBER',a$CWPName[i]))>0|
            length(grep('MOTORS',a$CWPName[i]))>0|
            length(grep('PRODUCTS',a$CWPName[i]))>0|
            length(grep('TIMBER',a$CWPName[i]))>0|
            length(grep('CHEMICAL',a$CWPName[i]))>0|
            length(grep('INDUSTRIES',a$CWPName[i]))>0|
            length(grep('INDUSTRIAL PARK',a$CWPName[i]))>0){
    facilities$ftype[i]<-'industrial'
  }  else if(length(grep('PLANT',a$CWPName[i]))>0|
             length(grep('MANUFACTURING',a$CWPName[i]))>0){ 
    facilities$ftype[i]<-'manufacturing'
  }
  facilities$fstatus[i]<-'inactive'
  if (a$SourceID[i]%in%FlowFrameFlipped$ECHOID){
    facilities$fstatus[i]<-'active'
  }
  if(!is.na(a$Faclat[i]) & !is.na(a$Faclong[i])){
    facilities$wkt_geom[i]<-paste0('POINT (',a$Faclong[a$SourceID==a$SourceID[i]],' ',a$Faclat[a$SourceID==a$SourceID[i]],')')
  } else {
    lat<-All$coords.x2[All$FacilityID==All$FacilityID[i]]
    long<-All$coords.x1[All$FacilityID==All$FacilityID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        facilities$wkt_geom[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        facilities$wkt_geom[i]<-'NULL'
      }
    }
  }
}
write.table(facilities,"C:/Users/connorb5/Desktop/GitHub/USGS_Consumptive_Use/Documentation/Imports/facilities.txt",sep="\t",row.names = F)

#Release Point Generation
All$VPDESID<-as.character(All$VPDESID)
All$FacilityID<-as.character(All$FacilityID)
releasepoint<-data.frame(bundle=rep('transfer',length(All$VPDESID)))
for (i in 1:length(releasepoint$bundle)){
  releasepoint$name[i]<-paste0('TO ',All$VPDESID[i])
  releasepoint$ftype[i]<-'release'
  releasepoint$hydocode[i]<-paste0('vahydro_',All$VPDESID[i])
  if(All$VPDESID[i]%in%FlowFrame$VPDESID){
    releasepoint$fstatus[i]<-'active'  
  } else {
    releasepoint$fstatus[i]<-'inactive'
  }
  if(!is.na(a$Faclat[a$SourceID==All$FacilityID[i]]) & !is.na(a$Faclong[a$SourceID==All$FacilityID[i]])){
    releasepoint$wkt_geom[i]<-paste0('POINT (',a$Faclong[a$SourceID==All$FacilityID[i]],' ',a$Faclat[a$SourceID==All$FacilityID[i]],')')
  } else {
    lat<-All$coords.x2[All$FacilityID==All$FacilityID[i]]
    long<-All$coords.x1[All$FacilityID==All$FacilityID[i]]
    for (i in 1:length(lat)){
      if(!is.na(lat[i]) & !is.na(long[i])){
        releasepoint$wkt_geom[i]<-paste0('POINT (',long[i],' ',lat[i],')')
        break
      } else {
        releasepoint$wkt_geom[i]<-'NULL'
      }
    }
  }
  releasepoint$dh_link_facility_mps[i]<-paste0('echo_',All$FacilityID[i])
}
write.table(releasepoint,"C:/Users/connorb5/Desktop/GitHub/USGS_Consumptive_Use/Documentation/Imports/releasepoint.txt",sep="\t",row.names = F)

#Conveyance Generation
conveyance<-data.frame(bundle=rep('conveyance',length(All$VPDESID)))
for (i in 1:length(conveyance$bundle)){
  conveyance$name[i]<-paste0(All$FacilityID[i],' TO ',All$VPDESID[i])
  conveyance$ftype[i]<-'water_transfer'
  conveyance$hydocode[i]<-paste0('vahydro_',All$FacilityID[i],'_',All$VPDESID[i])
  if(All$VPDESID[i]%in%FlowFrame$VPDESID){
    conveyance$fstatus[i]<-'active'  
  } else {
    conveyance$fstatus[i]<-'inactive'
  }
  conveyance$from_node[i]<-paste0('vahydro_',All$VPDESID[i])
  conveyance$to_node[i]<-paste0('echo_',All$VPDESID[i])
}
write.table(conveyance,"C:/Users/connorb5/Desktop/GitHub/USGS_Consumptive_Use/Documentation/Imports/conveyance.txt",sep="\t",row.names = F)

#Outfall Generation
outfalls<-data.frame(bundle=rep('transfer',length(All$VPDESID)))
for (i in 1:length(outfalls$bundle)){
  outfalls$name[i]<-paste0('FROM ',All$FacilityID[i])
  outfalls$ftype[i]<-'outfall'
  outfalls$hydrocode<-paste0('echo_',All$VPDESID[i])
  if(All$VPDESID[i]%in%FlowFrame$VPDESID){
    outfalls$fstatus[i]<-'active'  
  } else {
    outfalls$fstatus[i]<-'inactive'
  }
  if(!is.na(All$coords.x2[All$VPDESID==All$VPDESID[i]]) & !is.na(All$coords.x1[All$VPDESID==All$VPDESID[i]])){
    outfalls$wkt_geom[i]<-paste0('POINT (',All$coords.x1[All$VPDESID==All$VPDESID[i]],' ',All$coords.x2[All$VPDESID==All$VPDESID[i]],')')  
    } else if (!is.na(a$Faclat[a$SourceID==All$FacilityID[i]]) & !is.na(a$Faclong[a$SourceID==All$FacilityID[i]])) {
      outfalls$wkt_geom[i]<-paste0('POINT (',a$Faclong[a$SourceID==All$FacilityID[i]],' ',a$Faclat[a$SourceID==All$FacilityID[i]])
    } else {
      outfalls$wkt_geom[i]<-'NULL'
  }
  outfalls$dh_link_facility_mps[i]<-paste0('echo_',All$FacilityID[i])
}
write.table(outfalls,"C:/Users/connorb5/Desktop/GitHub/USGS_Consumptive_Use/Documentation/Imports/outfalls.txt",sep="\t",row.names = F)
