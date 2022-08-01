library(sf)

library(maps)
library(mapdata)
library(maptools)
library(rerddap)
library(rerddapXtracto)
library(lubridate)

test=st_read('LMEs66.shp')

lme_list=c(60,4,10,21,25,19,12)

i=3
#for (i in 1:length(lme_list)) {
  id=lme_list[i]
  bounds=st_coordinates(test[id,])[,(1:2)]
  I=which(bounds[,1]<0)
  bounds[I,1]=bounds[I,1]+360
#}
  
  xlim=c(min(bounds[,1])-5,max(bounds[,1])+5)
  ylim=c(min(bounds[,2])-5,max(bounds[,2])+5)
  
  land <- maps::map("world2", fill = TRUE, col='grey',xlim = xlim, ylim = ylim, 
                    plot=FALSE, xaxs='i',yaxs='i',asp=1, cex.axis = 3, 
                    proj4string = CRS("+proj=longlat +datum=WGS84"))
  ids <- sapply(strsplit(land$names, ":"), function(x) x[1])
  bPols <- map2SpatialPolygons(land, IDs = ids, proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  
  plot(bPols, col = "grey", axes = FALSE, xlim = xlim, ylim = ylim, xaxs='i',yaxs='i'
       ,asp=1, cex.axis = 3)
  lines(bounds[,1],bounds[,2],col=2,lwd=2)
  x = seq(round(xlim[1]), round(xlim[2]), 5)
  axis(1, x, sapply(paste(-x + 360, "ºW", sep = ""), function(x) x[1]))
  
  y = seq(round(ylim[1]), round(ylim[2]), 5)
  axis(2, las = 1, y, sapply(paste(y, "ºN", sep = ""), function(x) x[1]))
  
  box()
  
  
  
  #data extraction - SST
  
  ERDDAP_Node="https://oceanwatch.pifsc.noaa.gov/erddap/"
  dataInfo <- rerddap::info('CRW_sst_v3_1_monthly', url=ERDDAP_Node)
  
  parameter=dataInfo$variable$variable_name[1]

  xcoord <- bounds[,1]
  ycoord <- bounds[,2]
  tcoord <- c("1985-01-31T12:00:00Z", "2021-12-31")
  
  sst <- rxtractogon (dataInfo, parameter=parameter, xcoord=xcoord, ycoord=ycoord, tcoord=tcoord)
  
 
  sst_mean_all=array(NA,dim(sst$sea_surface_temperature)[3])
  for (i in 1:dim(sst$sea_surface_temperature)[3])
    sst_mean_all[i]=mean(sst$sea_surface_temperature[,,i],na.rm=TRUE)
  
  sst_mean=array(NA,length(1985:2021))
  i=1
  for (y in 1985:2021) {
    I=which(year(sst$time)==y)
    sst_mean[i]=mean(sst_mean_all[I],na.rm=TRUE)
    i=i+1
  }
  
  plot(1985:2021,sst_mean,type='o',pch=20)
  
  #################################################################################################
  
  #data extraction - CHL
  
  ERDDAP_Node="https://oceanwatch.pifsc.noaa.gov/erddap/"
  dataInfo <- rerddap::info('esa-cci-chla-monthly-v5-0', url=ERDDAP_Node)
  
  parameter=dataInfo$variable$variable_name[1]
  
  xcoord <- bounds[,1]
  ycoord <- bounds[,2]
  tcoord <- c("1998-01-01", "2021-12-01")
  
  chl <- rxtractogon (dataInfo, parameter=parameter, xcoord=xcoord, ycoord=ycoord, tcoord=tcoord)
  
  
  chl_mean_all=array(NA,dim(chl$chlor_a)[3])
  for (i in 1:dim(chl$chlor_a)[3])
    chl_mean_all[i]=median(chl$chlor_a[,,i],na.rm=TRUE)
  
  chl_mean2=array(NA,length(1998:2021))
  i=1
  for (y in 1998:2021) {
    I=which(year(chl$time)==y)
    chl_mean2[i]=median(chl_mean_all[I],na.rm=TRUE)
    i=i+1
  }
  
  chl_mean=array(NA,length(1998:2021))
  i=1
  for (y in 1998:2021) {
    I=which(year(chl$time)==y)
    chl_mean[i]=median(chl$chlor_a[,,I],na.rm=TRUE)
    i=i+1
  }
  
  
  
  plot(1998:2021,chl_mean,type='o')
  