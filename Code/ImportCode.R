#name, hydrocode,wkt_geom,ftype,bundle
#Need from_node and to_node for conveyance. Omit wkt_geom for conveyance
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

#Facility Generation
facilities<-data.frame(hydrocode=a$VAP_PMT_NO,name=a$CWPName)
facilities$bundle<-'facility'
facilities$ftype<-'wwtp'
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
