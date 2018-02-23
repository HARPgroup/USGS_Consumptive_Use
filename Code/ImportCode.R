#Require flow frame flipped to be generated from AnalysisCode.R
#######################
#Library Initialization
library(XML)
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
}
facilities$wkt_geom<-paste0('POINT (',a$FacLat,' ',a$FacLong,')')
write.csv(facilities,"C:/Users/Connor/Desktop/USGS Testing/Import/facilities.csv",row.names = F)

#Release Point Generation
releasepoint<-data.frame(hydocode=as.character(FlowFrameFlipped$VPDESID),bundle='transfer',ftype='release')
releasepoint$hydocode<-as.character(releasepoint$hydocode)
for (i in 1:length(releasepoint$hydocode)){
  releasepoint$hydocode[i]<-paste0(FlowFrameFlipped$ECHOID[i],':',FlowFrameFlipped$VPDESID[i])
  releasepoint$name[i]<-paste0('Release to outfall ',FlowFrameFlipped$VPDESID[i])
  releasepoint$wkt_geom[i]<-paste0('POINT (',a$FacLat[a$VAP_PMT_NO==FlowFrameFlipped$ECHOID[i]],' ',a$FacLong[a$VAP_PMT_NO==FlowFrameFlipped$ECHOID[i]],')')
}
write.csv(releasepoint,"C:/Users/Connor/Desktop/USGS Testing/Import/releasepoint.csv",row.names = F)

#Conveyance Generation
conveyance<-data.frame(hydocode=as.character(FlowFrameFlipped$VPDESID),bundle='coneyance',ftype='water_transfer')
conveyance$hydocode<-as.character(conveyance$hydocode)
for (i in 1:length(conveyance$hydocode)){
  conveyance$hydocode[i]<-paste0(FlowFrameFlipped$ECHOID[i],':',FlowFrameFlipped$VPDESID[i])
  conveyance$name[i]<-paste0('From ',FlowFrameFlipped$ECHOID[i],' to ',FlowFrameFlipped$VPDESID[i])
  conveyance$from_node[i]<-paste0(FlowFrameFlipped$ECHOID[i],':',FlowFrameFlipped$VPDESID[i])
  conveyance$to_node[i]<-FlowFrameFlipped$VPDESID[i]
}
write.csv(conveyance,"C:/Users/Connor/Desktop/USGS Testing/Import/conveyance.csv",row.names = F)

#Outfall Generation
outfalls<-data.frame(bundle='transfer',ftype='outfall',hydrocode=FlowFrameFlipped$VPDESID)
for (i in 1:length(outfalls$hydrocode)){
  outfalls$wkt_geom[i]<-paste0('POINT (',a$FacLat[a$VAP_PMT_NO==FlowFrameFlipped$ECHOID[i]],' ',a$FacLong[a$VAP_PMT_NO==FlowFrameFlipped$ECHOID[i]],')')
  outfalls$geom_source[i]<-'estimated'
  outfalls$name[i]<-paste0('Outfall ',FlowFrameFlipped$VPDESID[i],' of facility ',FlowFrameFlipped$ECHOID[i])
}
write.csv(outfalls,"C:/Users/Connor/Desktop/USGS Testing/Import/outfalls.csv",row.names = F)
