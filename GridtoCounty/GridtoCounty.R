#converting unit base data to county unit data, for all the layers, and below is the example of solar
setwd("~/Users/kaminarikiminorushi/Dropbox/Soko_Lei/GridtoCounty")

library(raster)
library(maptools)
library(maps)


#read America scale solar data
solar1<-readShapePoly("solar1.shp");
y<-raster(ncols=587,nrows=256, xmn=-125.1, xmx=-66.5, ymn=24.2, ymx=49.7);
solargrid1<-rasterize(solar1, y, 'DNIANN') 
solargrid1[is.na(solargrid1)==TRUE]<-NaN
write.table(as.matrix(solargrid1),"solargrid1.txt",row.names=FALSE, col.names=FALSE)
solarpoint<-rasterToPoints(solargrid1, fun=NULL, spatial=FALSE);
x<-solarpoint[,1]
y<-solarpoint[,2]
xy<-cbind(x,y)
solar_value<-solarpoint[,3]

#read county scale boundary data
#USA_adm1 <- readRDS("USA_adm2.rds")
CountyBoundary<-readShapePoly("US_county_2000-simple-latlon.shp",verbose=TRUE,proj4string=CRS("+proj=tmerc +lon_0=-84 +lat_0=0 +x_0=500000 +k=0.9999 +datum=WGS84") );



#set a loop to read the raster data into counties
county <-matrix(numeric(0), 3109,0);
for(i in 1:3109){
  countyboundary <- subset(CountyBoundary,PID==i )
  temp_JJJ <- raster(res=10) #actually I don't the influence of setting resolution
  extent(temp_JJJ) <- extent(countyboundary)
  out <- rasterize(countyboundary, temp_JJJ,'PID')
  countyresult<- rasterize(xy,out, solar_value)
  county[i]<-cellStats(countyresult,mean)
}


#found county centroid
library(rgeos)
Centroid<-matrix(numeric(0), 3,2);
for(i in 1:3){
  countycentroid <- subset(CountyBoundary,PID==i )

  centroid<-gCentroid(countycentroid)
  centroid<-as.data.frame(centroid)
  centroid<-data.frame(centroid$x,centroid$y)
}
end

#https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
install.packages("GISTools")
library(GISTools)
centroids <- getSpPPolygonsLabptSlots(CountyBoundary)


write.csv(CountyBoundary$PID,CountyBoundary$ICPSRNAM,CountyBoundary$STATENAM,centroids, file = "County.csv")

# Urban Value Map 

urban1<-readShapePoly("urban1.shp");
y<-raster(ncols=587,nrows=256, xmn=-125.1, xmx=-66.5, ymn=24.2, ymx=49.7);
urbangrid1<-rasterize(urban1, y, 'URBANAP020') 
urbangrid1[is.na(urbangrid1)==TRUE]<-0

is_urban=urban>0;
is_urban=double(is_urban);
value1=sqrt(2)*V*[1:R R:-1:1]/R;
value2=(ones(2*R,1)*value1).*(ones(2*R,1)*value1)';
raw=conv2(value2,is_urban);
urban_value=raw(R:(100+R-1),R:(100+R-1));







