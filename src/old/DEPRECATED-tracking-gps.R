# Reset working directory
rm(list=ls())

# # Check package availability
# packages.needed<-c("rgdal","plotKML","chron","spatstat","maptools","raster")
# if (length(setdiff(packages.needed, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages.needed, rownames(installed.packages())))  
# }

# Load packages
library(rgdal)
# library(plotKML)
library(spatstat)
library(maptools)
library(chron)
library(raster)
library(ggplot2)
library(rlist)
library(plotrix)
library(zoo)
library(dplyr)

# README ------------------------------------------------------------------

# Environment settings ----------------------------------------------------

# Working directory
# wd<-"/home/elic/fclapuyt/tracking-gps/"
# wd<-"C:/Users/fclapuyt/Nextcloud/Students/Therese/tracking-gps"
wd<-"/home/franz/Nextcloud/Students/Therese/tracking-gps"
# wd<-"~/Nextcloud/Students/Therese/tracking-gps"

# Subfolders
dir.area<-"data/malia"
dir.inputs<-file.path(dir.area,"tracks-utm")
dir.outputs<-file.path(dir.area,"outputs")
dir.src<-"src"
dir.functions<-"functions"
dir.gpx<-"gpx"
dir.shp<-"shp"
dir.aoi<-"aoi"
dir.graphs<-"graphs"
dir.lines<-"lines"
dir.points<-"points"
dir.thresholded<-"thresholded"
dir.tables<-"tables"

# Variables ---------------------------------------------------------------

# Define variables

# Name of the shapefile containing the limits of the area
limits.area<-"limits-area"
# EPSG number of local projection coordinate system (UTM) 
id.proj<-"+init=epsg:32635"
# Speed threshold (km/h)
speed.threshold<-12
# Smooth elevation and speed data
smooth.ele<-0.05
smooth.speed<-0.02
# Filter speed greather than a threshold ? 
speed.threshold.apply<-TRUE
speed.threshold.upper<-10
time.threshold.apply<-TRUE
time.threshold.begin<-100
direction.smooth<-20
# Create graphs
create.graphs<-TRUE

# Script GPS Tracking -----------------------------------------------------

# Define working directory
setwd(wd)

# Create missing directories
if(!dir.exists(file.path(wd,dir.outputs))){
  dir.create(file.path(wd, dir.outputs))  
}
if(!dir.exists(file.path(wd,dir.outputs,dir.shp))){
  dir.create(file.path(wd,dir.outputs,dir.shp))  
}
if(!dir.exists(file.path(wd,dir.outputs,dir.graphs))){
  dir.create(file.path(wd,dir.outputs,dir.graphs))  
}
if(!dir.exists(file.path(wd,dir.outputs,dir.shp,dir.aoi))){
  dir.create(file.path(wd,dir.outputs,dir.shp,dir.aoi))  
}
if(!dir.exists(file.path(wd,dir.outputs,dir.shp,dir.lines))){
  dir.create(file.path(wd,dir.outputs,dir.shp,dir.lines))  
}
if(!dir.exists(file.path(wd,dir.outputs,dir.shp,dir.points))){
  dir.create(file.path(wd,dir.outputs,dir.shp,dir.points))  
}
if(!dir.exists(file.path(wd,dir.outputs,dir.shp,dir.thresholded))){
  dir.create(file.path(wd,dir.outputs,dir.shp,dir.thresholded))  
}


# Load custom functions
# source(file.path(wd,dir.src,"distLatLon.R"))
source(file.path(wd,dir.src,dir.functions,"shiftVector.R"))

#list of gpx files in the working directory defined as input
gpx.files<-list.files(file.path(wd,dir.inputs),pattern=".shp")

# Create empty objects to collect results
gpx.spdf.list<-list()
summary.list<-list()
gpx.df.list<-list()

# Read AOI shapefile
aoi.spdf<-readOGR(dsn=file.path(wd,dir.area,"museum","museum.shp"),stringsAsFactors=FALSE)
aoi.spdf<-spTransform(aoi.spdf,CRS(id.proj))

# gpx.file<-gpx.files[4]
for (gpx.file in gpx.files){
  
  # gpx name
  gpx.name<-unlist(strsplit(gpx.file,split="\\."))[1]
  # gpx.name<-gsub(pattern="Trac?",replacement="Track",gpx.name)
  gpx.name<-gsub(pattern=" ",replacement="-",gpx.name)

  # Read GPX file and project in UTM coordinates
  gpx.spdf<-readOGR(file.path(wd,dir.inputs,gpx.file))
  # gpx.spdf<-spTransform(gpx.spdf,CRS(id.proj))
  

  #  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean attribute table and join coordinates of points
  gpx.spdf@data<-cbind(gpx.spdf@coords,gpx.spdf@data[,c("track_se_1","ele","time")])
  colnames(gpx.spdf@data)[1:2]<-c("lon","lat")
  gpx.spdf@data$track_se_1<-as.numeric(gpx.spdf@data$track_se_1)  
  gpx.spdf@data$time<-as.character(gpx.spdf@data$time)  
  
  #  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add filename to points
  gpx.spdf@data$gpx.name<-gpx.name
  
  # Shift vectors for lat and lon so that each row also contains the next position.
  gpx.spdf@data$lat.p2<-shiftVector(gpx.spdf@data$lat,-1)
  gpx.spdf@data$lon.p2<-shiftVector(gpx.spdf@data$lon,-1)
  # Compute distances (in metres) between each point and the next one, and cumulated distance
  gpx.spdf@data$dist<-pointDistance(p1=gpx.spdf@data[,c("lon","lat")],p2=gpx.spdf@data[,c("lon.p2","lat.p2")],lonlat=FALSE)
  gpx.spdf@data$dist.cum<-cumsum(gpx.spdf@data$dist)
  
  # Convert time in to POSIXct format
  gpx.spdf@data$time<-as.POSIXct(strptime(gpx.spdf@data$time,format="%Y/%m/%d %H:%M:%S"),format="%Y/%m/%d %H:%M:%S")

  # Shift time so that each row contains the timestamp of the next point
  gpx.spdf@data$time.p2<-shiftVector(gpx.spdf@data$time,-1)
  # Compute the number of seconds between each point and the next one.
  gpx.spdf@data$time.diff<-as.numeric(difftime(gpx.spdf@data$time.p2,gpx.spdf@data$time))
  
  # Compute speed and smoothed speed
  gpx.spdf@data$speed.ms<-gpx.spdf@data$dist/gpx.spdf@data$time.diff
  gpx.spdf@data$speed.kmh<-gpx.spdf@data$speed.ms*3.6
  gpx.spdf@data$speed.lowess<-lowess(gpx.spdf@data$speed.kmh,f=smooth.speed)$y
  
  # Threshold speed for high values that are not realistic
  if(speed.threshold.apply == TRUE){
    
    gpx.spdf<-gpx.spdf[which(gpx.spdf@data$speed.kmh < speed.threshold.upper),]
    
  }
  
  # Compute smoothed elevation and total climb
  gpx.spdf@data$ele.lowess<-lowess(gpx.spdf@data$ele,f=smooth.ele)$y
  # Compute elevation difference between each point
  gpx.spdf@data$ele.p2<-shiftVector(gpx.spdf@data$ele,-1)
  gpx.spdf@data$climb<-gpx.spdf@data$ele.p2-gpx.spdf@data$ele
  
  # gpx.df<-gpx.spdf@data
  
  # Compute vector components of displacements between points
  gpx.spdf@data$vec.x<-gpx.spdf@data$lon.p2-gpx.spdf@data$lon
  gpx.spdf@data$vec.y<-gpx.spdf@data$lat.p2-gpx.spdf@data$lat
  gpx.spdf@data$azimut.rad<-atan2(gpx.spdf@data$vec.y,gpx.spdf@data$vec.x)
  gpx.spdf@data$azimut.deg<-atan2(gpx.spdf@data$vec.y,gpx.spdf@data$vec.x)*(180/pi)
  
  gpx.spdf@data$azimut.smooth<-rollmean(x=gpx.spdf@data$azimut.deg,k=direction.smooth,fill=NA)
  gpx.spdf@data$id.sort<-seq(1,nrow(gpx.spdf@data),1)
  
  # Remove points with NA values (missing speed)
  gpx.spdf<-gpx.spdf[c(1:(nrow(gpx.spdf)-2)),]
  
  # Apply time threshold to remove wrong start of GPS
  if(time.threshold.apply == TRUE){
    
    # Find points which have a time difference greater than threshold
    time.id<-which(gpx.spdf@data$time.diff > time.threshold.begin)
    
    # If points are found, process them
    if(length(time.id) > 0){
      
      if(time.id[1] < 100){
        
        gpx.spdf<-gpx.spdf[c((time.id[1]+1):nrow(gpx.spdf)),]
        
      }
      
    }
    
      
  }
  
  # Compute summary statistics and store in dataframe
  time.start<-gpx.spdf@data[1,"time"]
  duration.total<-as.numeric(difftime(gpx.spdf@data[nrow(gpx.spdf@data),"time"],time.start,units="mins"))
  dist.total<-sum(gpx.spdf@data$dist)
  speed.kmh.mean<-mean(gpx.spdf@data$speed.kmh)
  speed.lowess.mean<-mean(gpx.spdf@data$speed.lowess,na.rm=TRUE)
  ele.mean<-mean(gpx.spdf@data$ele)
  ele.lowess.mean<-mean(gpx.spdf@data$ele.lowess)
  climb.total<-sum(gpx.spdf@data$climb[gpx.spdf@data$climb>=0])
  summary.list<-list.append(summary.list,list(name=gpx.name,duration=duration.total,distance=dist.total,
                                              speed.kmh.mean=speed.kmh.mean,speed.lowess.mean=speed.lowess.mean,
                                              ele.mean=ele.mean,ele.lowess.mean=ele.lowess.mean,climb.total=climb.total))

  # Create plots of elevation and speed
  if(create.graphs == TRUE){
    
    # Plot elevation against time and save in output folder
    plot.elev.lowess<-ggplot(data=gpx.spdf@data,aes(x=time,y=ele))+
      geom_point(size=1,color="darkgrey")+
      geom_point(aes(y=ele.lowess),size=1,color="red")+
      labs(x="Time",y="Elevation (m)",
           title=paste0("Start time: ",time.start," / Duration: ",round(duration.total,2)," min"))
    # print(plot.elev.lowess)
    ggsave(plot.elev.lowess,filename=file.path(wd,dir.outputs,dir.graphs,paste0("elevation-vs-time-",gpx.name,".png")),device="png",dpi=300,units="cm",width=20,height=15)
    
    # Plot speed against time and save in output folder
    plot.speed<-ggplot(data=gpx.spdf@data,aes(x=time,y=speed.kmh))+
      geom_point(size=1,color="darkgrey")+
      geom_point(aes(y=speed.lowess),size=1,color="red")+
      labs(x="Time",y="Speed (km/h)",
           title=paste0("Start time: ",time.start," / Duration: ",round(duration.total,2)," min"))
    # print(plot.speed)
    ggsave(plot.speed,filename=file.path(wd,dir.outputs,dir.graphs,paste0("speed-vs-time-",gpx.name,".png")),device="png",dpi=300,units="cm",width=20,height=15)
    
  }
  
  # Collect gpx.spdf in a list for further processing
  gpx.spdf.list<-list.append(gpx.spdf.list,gpx.spdf)
  gpx.df.list<-list.append(gpx.df.list,as.data.frame(gpx.spdf))
  
  # Map gpx track
  # plot(gpx.spdf,pch=".",col="red")
  # Plot vector field for visitor's displacements
  gpx.vec<-gpx.spdf[seq(from=(direction.smooth/2),to=nrow(gpx.spdf)-(direction.smooth/2),by=8),]
  # vectorField(xpos=gpx.vec@data$lon,ypos=gpx.vec@data$lat,u=gpx.vec@data$azimut.rad,v=1,vecspec="rad")
  
  # Save current gpx to POINT shapefile
  writeOGR(obj=gpx.spdf,dsn=file.path(wd,dir.outputs,dir.shp,dir.points,paste0(gpx.name,"-points.shp")),layer=paste0(gpx.name,"-points"),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create line shapefile from track
  
  # How many points between line segments (smoothing factor)
  lines.interval<-1
  
  # Loop within gpx to create vertices of lines
  lines.data<-data.frame()
  for(point in seq(1,nrow(gpx.spdf@data),lines.interval)){
    
    line.temp<-rbind(gpx.spdf@data[point,],gpx.spdf@data[point+lines.interval,])
    line.temp$id<-point
    
    lines.data<-rbind(lines.data,line.temp)
    
  }
  # Remove NA lines
  lines.data<-lines.data[which(lines.data$id != point),]
  
  # Convert lines.data into SpatialLinesDataframes
  d<-lines.data
  coordinates(d)<-~lon+lat
  x <- lapply(split(d, d$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
  
  lines <- SpatialLines(x)
  data <- data.frame(id = unique(d$id))
  rownames(data) <- data$id
  gpx.spdf.line <- SpatialLinesDataFrame(lines, data)
  proj4string(gpx.spdf.line)<-CRS(id.proj)
  
  # gpx.spdf.line<-SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(coordinates(gpx.spdf))), "id"))),
  #                                      data=data.frame(x=1,y=1,row.names = "id"))
  
  # Save current gpx to LINE shapefile
  writeOGR(obj=gpx.spdf.line,dsn=file.path(wd,dir.outputs,dir.shp,dir.lines,paste0(gpx.name,"-lines.shp")),layer=paste0(gpx.name,"-lines"),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  # Speed threshold ---------------------------------------------------------
  # Extract points which are characterised by a speed greater than the speed threshold
  speed.threshold<-1
  gpx.spdf.speed<-gpx.spdf[which(gpx.spdf@data$speed.kmh > speed.threshold),]
  
  # Save current gpx to POINT shapefile
  writeOGR(obj=gpx.spdf.speed,dsn=file.path(wd,dir.outputs,dir.shp,dir.thresholded,paste0(gpx.name,"-points-thresholded.shp")),layer=gpx.name,driver="ESRI Shapefile",overwrite_layer=TRUE)
  
}

# Convert summary list to dataframe
summary<-data.frame(gpx.name=sapply(summary.list,"[[","name"),
                    duration=sapply(summary.list,"[[","duration"),
                    distance=sapply(summary.list,"[[","distance"),
                    speed.kmh.mean=sapply(summary.list,"[[","speed.kmh.mean"),
                    speed.lowess.mean=sapply(summary.list,"[[","speed.lowess.mean"),
                    ele.mean=sapply(summary.list,"[[","ele.mean"),
                    ele.lowess.mean=sapply(summary.list,"[[","ele.lowess.mean"),
                    climb.total=sapply(summary.list,"[[","climb.total")
                    )
# Write summary in csv
write.csv2(x=summary,file=file.path(wd,dir.outputs,"summary-tracks.csv"))

# Merge all gpx files in a data.frame and convert to spdf
gpx.df.full<-do.call("rbind",gpx.df.list)
gpx.spdf.full<-SpatialPointsDataFrame(gpx.df.full[,c("lon","lat")],gpx.df.full)
proj4string(gpx.spdf.full)<-CRS(id.proj)
# Save all gpx to shapefile
writeOGR(obj=gpx.spdf.full,dsn=file.path(wd,dir.outputs,dir.shp,"gpx-full.shp"),layer="gpx-full",driver="ESRI Shapefile",overwrite_layer=TRUE)

# Speed threshold ---------------------------------------------------------
# Extract points which are characterised by a speed greater than the speed threshold
speed.threshold<-1
gpx.spdf.speed<-gpx.spdf.full[which(gpx.spdf.full@data$speed.kmh > speed.threshold),]
plot(gpx.spdf.full,pch=".")
plot(gpx.spdf.speed,pch=".",col="red",add=TRUE)

# Save speed thresholded gpx file to shapefile
writeOGR(obj=gpx.spdf.speed,dsn=file.path(wd,dir.outputs,dir.shp,"gpx-full-thresholded.shp"),layer="gpx-full-thresholded",driver="ESRI Shapefile",overwrite_layer=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute data within aois for full data

# Azimut classes to categorize directions
azimut.classes<-seq(-180,180,45)

# List to store results 
mean.dir.aois<-list()

# Two columns to store results in aoi shp
# aoi.spdf@data$tan<-0
aoi.spdf@data$tan2<-0
# aoi.spdf@data$tan.th<-0
aoi.spdf@data$tan2.th<-0

# Iterate within aois
for(aoi.id in aoi.spdf@data$Id){
  
  # Current AOI
  aoi.id<-as.numeric(aoi.id)
  aoi.cur<-aoi.spdf[aoi.id,]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Select points within current aoi
  pts.cur<-gpx.spdf.full[aoi.cur,]
  pts.cur.th<-gpx.spdf.speed[aoi.cur,]
  
  # Compute total x and y components of movement
  sum.x<-sum(pts.cur@data$vec.x)
  sum.y<-sum(pts.cur@data$vec.y)
  sum.x.th<-sum(pts.cur.th@data$vec.x)
  sum.y.th<-sum(pts.cur.th@data$vec.y)

  # Compute mean total movement within aoi
  mean.dir.tan<-atan(sum.y/sum.x)*(180/pi)
  mean.dir.tan2<-atan2(sum.y,sum.x)*(180/pi)
  mean.dir.tan.th<-atan(sum.y.th/sum.x.th)*(180/pi)
  mean.dir.tan2.th<-atan2(sum.y.th,sum.x.th)*(180/pi)
  
  # Store in aoi spdf
  # aoi.spdf@data[aoi.id,"mean.tan"]<-mean.dir.tan
  aoi.spdf@data[aoi.id,"tan2"]<-mean.dir.tan2
  # aoi.spdf@data[aoi.id,"tan.th"]<-mean.dir.tan.th
  aoi.spdf@data[aoi.id,"tan2.th"]<-mean.dir.tan2.th
  
  # Store in list
  mean.dir.aois[[aoi.id]]<-list(mean.dir.tan=mean.dir.tan,
                                mean.dir.tan2=mean.dir.tan2,
                                mean.dir.tan.th=mean.dir.tan.th,
                                mean.dir.tan2.th=mean.dir.tan2.th
                                )
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create vectors per AOI
  
  # Create Line dataframe for AOI
  aoi.lines = SpatialLinesDataFrame(
    sl=SpatialLines(
      LinesList=list(
        Lines(
          list(
            Line(
              coords=matrix(c(0,1,0,1),2,2)
            )
          ),ID=1)
      )
    ),
    data=data.frame(a=1,b=2,c=3))[-1,]
  # Project empty dataframe in utm
  proj4string(aoi.lines)<-CRS(id.proj)
  
  # Get unique values from tracks within AOI
  tracks.cur<-unique(pts.cur@data$gpx.name)
  id.line<-1
  # track.name<-tracks.cur[2]
  for(track.name in tracks.cur){
  
    # Selects points from current track 
    track.cur<-pts.cur[which(pts.cur@data$gpx.name == track.name),]
    track.cur<-track.cur[c(1,length(track.cur)),]
    
    # Compute direction from first to last point of the track within AOI
    vec.x<-track.cur@data$lon[2]-track.cur@data$lon[1]
    vec.y<-track.cur@data$lat[2]-track.cur@data$lat[1]
    azimut.deg<-atan2(vec.y,vec.x)*(180/pi)
    
    # Loop within gpx to create vertices of lines
    lines.data<-data.frame()
    for(point in seq(1,nrow(track.cur@data),1)){
      
      # line.id<-line.id+1
      
      line.temp<-rbind(track.cur@data[point,],track.cur@data[point+1,])
      line.temp$id<-point
      
      lines.data<-rbind(lines.data,line.temp)

    }
    
    # Remove NA lines
    lines.data<-lines.data[which(lines.data$id != point),]
    
    # Convert lines.data into SpatialLinesDataframes
    d<-lines.data
    coordinates(d)<-~lon+lat
    x <- lapply(split(d, d$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
    
    lines <- SpatialLines(x)
    data <- data.frame(id = unique(d$id),id2=id.line,track=track.name,azimut=azimut.deg)
    rownames(data) <- data$id
    track.cur.line <- SpatialLinesDataFrame(lines, data)
    proj4string(track.cur.line)<-CRS(id.proj)
    
    # Collect line to aoi.lines
    aoi.lines<-rbind(aoi.lines,track.cur.line)
    id.line<-id.line+1
    
  }
  
  # Extract aoi lines data frame 
  # aoi.lines.df<-aoi.lines@data
  # aoi.lines.df$id<-NULL
  # colnames(aoi.lines.df)[1]<-"id"    
  
  # Create linear scale bins
  aoi.lines@data$azimut.class<-as.vector(cbind(bin=cut(aoi.lines@data$azimut,azimut.classes)))

  # Compute azimut frequency per binned direction
  aoi.lines.freq<-aoi.lines@data %>%
    count(azimut.class)
  colnames(aoi.lines.freq)[2]<-"count"
  
  # Save tables of frequencies and aoi.lines
  write.csv2(aoi.lines@data,file=file.path(wd,dir.outputs,dir.shp,dir.aoi,dir.tables,paste0("aoi-",aoi.id,"-vectors.csv")))
  write.csv2(aoi.lines.freq,file=file.path(wd,dir.outputs,dir.shp,dir.aoi,dir.tables,paste0("aoi-",aoi.id,"-azimut-freq.csv")))
  
  # Save current aoi vectors to list
  writeOGR(obj=aoi.lines,dsn=file.path(wd,dir.outputs,dir.shp,dir.aoi,dir.shp,paste0("aoi-",aoi.id,"-vectors.shp")),
           layer=paste0("aoi-",aoi.id,"-vectors"),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  # Save current aoi vectors to list
  # aoi.lines.df$ws<-1
  # aoi.lines.df$azimut.north<-0
  # 
  # aoi.lines.df[which(aoi.lines.df$azimut > 0)]
  # pollutionRose(aoi.lines.df,ws="ws",wd="azimut")
  # 
  # view(pollutionRose)

}

# Save aoi spdf to shapefile
writeOGR(obj=aoi.spdf,dsn=file.path(wd,dir.outputs,dir.shp,"aoi-mean-directions.shp"),layer="aoi-mean-directions",
         driver="ESRI Shapefile",overwrite_layer=TRUE)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Point pattern analysis --------------------------------------------------
# Useful documentation: https://mgimond.github.io/Spatial/

# Read shapefile containing limits of the study area
area.spdf<-readOGR(dsn=file.path(wd,dir.inputs,list.files(path=file.path(wd,dir.inputs,dir.shp),pattern=".shp")))

# # Divide study area into quadrats
# area.rstr<-raster(area.spdf)
# res(area.rstr)<-100
# area.quads <- as(rasterize(area.spdf,area.rstr), 'SpatialPolygons')
# 
# # Display quads and gpx tracks
# plot(quads)
# points(gpx.spdf.full,col="red",cex=.5)

# Convert limits to owin type and get coordinates of points
area.owin<-as.owin(area.spdf)
gpx.pts<-coordinates(gpx.spdf.full)

# Point pattern analysis
gpx.ppp<-ppp(gpx.pts[,1],gpx.pts[,2],window=area.owin)
plot(gpx.ppp)
gpx.density<-density(gpx.ppp)
plot(gpx.density)

# Count points within quadrats
pts.count<-quadratcount(gpx.ppp,nx=50,ny=50)
# Plot count
plot(gpx.ppp, pch=".", cols="grey70")
plot(pts.count, add=TRUE)

# Compute density of points within quadrats
pts.intensity<-intensity(pts.count)
plot(intensity(pts.count,image=TRUE))
plot(gpx.ppp,pch=".",add=TRUE,col="green",main="Density of points")

pts.tess<-as.tess(pts.count)
pts.spdf<-as(pts.tess,"SpatialPolygons")

pts.count$tile

plot(pts.spdf)

# owin2Polygons <- function(x, id="1") {
#   stopifnot(is.owin(x))
#   x <- as.polygonal(x)
#   closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
#   pieces <- lapply(x$bdry,
#                    function(p) {
#                      Polygon(coords=closering(cbind(p$x,p$y)),
#                              hole=is.hole.xypolygon(p))  })
#   z <- Polygons(pieces, id)
#   return(z)
# }
# 
# tess2SP <- function(x) {
#   stopifnot(is.tess(x))
#   y <- tiles(x)
#   nam <- names(y)
#   z <- list()
#   for(i in seq(y))
#     z[[i]] <- owin2Polygons(y[[i]], nam[i])
#   return(SpatialPolygons(z))
# }
# 
# owin2SP <- function(x) {
#   stopifnot(is.owin(x))
#   y <- owin2Polygons(x)
#   z <- SpatialPolygons(list(y))
#   return(z)
# }
# 
# test<-owin2SP(test)
# is(pts.count)

