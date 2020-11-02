rm(list=ls())
library(rgdal)

wd<-"C:/Users/fclapuyt/Nextcloud/Students/Therese/data"
setwd(wd)

dir.tracks<-"tracks"
dir.outputs<-"tracks-utm"

list.shp<-list.files(file.path(wd,dir.tracks),pattern="\\.shp$",full.names = TRUE)

# shp<-list.shp[2]
for(shp in list.shp){
  
  track<-readOGR(dsn=shp,stringsAsFactors=FALSE)
  track.name<-ogrListLayers(shp)
  track.utm<-spTransform(track, CRS("+init=epsg:32635"))  
  writeOGR(obj=track.utm,dsn=file.path(wd,dir.outputs),
           layer=track.name,driver="ESRI Shapefile",encoding="UTF-8",
           overwrite_layer=TRUE)  

}
 
  