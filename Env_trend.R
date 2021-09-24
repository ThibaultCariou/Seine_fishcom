library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(RColorBrewer)

coast <- rgdal::readOGR("data/Coast_estuary_detailled.gpkg")
coast <- raster::crop(coast, raster::extent(-0.3,0.3,49.25,49.7))
coast.fort <- fortify(coast)

NSyears <- c(1995:2002,2008:2010,2017:2019)

#Salinity 1992-2018#####
Sal<- nc_open("data/env/MetO-NWS-PHY-mm-SAL_1583156080399.nc")
Sal<- stack("data/env/MetO-NWS-PHY-mm-SAL_1583156080399.nc")
Sal <-raster::crop(Sal, raster::extent(-0.3,0.3,49.25,49.7))

# Conversion raster - tableau
fortify.Raster <- function(Sal, maxPixel = 1000000) {
  
  if (ncell(Sal) > maxPixel) {
    x <- sampleRegular(Sal, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Sal, seq_len(ncell(Sal)))
  out <- Sal %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabSal<- fortify(Sal)
pixelok<- which(!is.na(apply(TabSal,1,mean)))
TabSal<- pivot_longer(TabSal, cols=1:323, names_to = "Date", values_to = "Salinite", values_drop_na = TRUE)

TabSal$Date<-sub("values.X","",TabSal$Date)
TabSal$Date<-substr(TabSal$Date,1,10)
TabSal$Date<-strptime(TabSal$Date, format= "%Y.%m.%d")
  
TabSal$Year <- as.numeric(substr(as.character(TabSal$Date),1,4))
TabSal$Month<- as.numeric(substr(as.character(TabSal$Date), 6,7))
TabSal$Day  <- as.numeric(substr(as.character(TabSal$Date), 9,10))

#TabSal <- TabSal %>% dplyr::filter(Year %in% NSyears)
TabSal2<- TabSal %>% dplyr::group_by(x,y,Year) %>% dplyr::summarize(moySal= mean(Salinite))
ggplot(TabSal2)+
  geom_tile(aes(x=x, y=y, fill= moySal))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Salinity")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Salinity")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "Greens"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

TabSal4<- TabSal %>% dplyr::group_by(Year) %>% dplyr::summarize(moybaie= mean(Salinite))

Salseries<- ggplot(TabSal4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean salinity 1992-2018")+
  xlab("Year")+
  ylab("1E-3")+
  theme_minimal()  
Salseries

# Serie tempo mean salinity (month)
TabSal6<- TabSal %>% dplyr::group_by(Month) %>% dplyr::summarize(moybaie= mean(Salinite))

Salseries2<- ggplot(TabSal6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean salinity")+
  xlab("Month")+
  ylab("1E-3")+
  theme_minimal()  
Salseries2
#####

#Temperature 1982-2020#####

sst<- stack("data/env/cmems-IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1618837117606.nc", varname="analysed_sst")
sst<- sst-275.15
sst<-raster::crop(sst, raster::extent(-0.3,0.3,49.25,49.7))

# Conversion raster - tableau
fortify.Raster <- function(sst, maxPixel = 1000000) {
  
  if (ncell(sst) > maxPixel) {
    x <- sampleRegular(sst, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(sst, seq_len(ncell(sst)))
  out <- sst %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tabsst<- fortify.Raster(sst)
pixelok<- which(!is.na(apply(Tabsst,1,mean)))
Tabsst<- pivot_longer(Tabsst, cols=1:13232, names_to = "Date", values_to = "SST", values_drop_na = TRUE)

Tabsst$Date<-sub("values.X","",Tabsst$Date)
Tabsst$Date<-strptime(Tabsst$Date, format= "%Y.%m.%d")

Tabsst$Year <- as.numeric(substr(as.character(Tabsst$Date),1,4))
Tabsst$Month<- as.numeric(substr(as.character(Tabsst$Date), 6,7))
Tabsst$Day  <- as.numeric(substr(as.character(Tabsst$Date), 9,10))



Tabsst<- na.omit(Tabsst)
#Tabsst <- Tabsst %>% dplyr::filter(Year %in% NSyears)

# Mean SST per year
Tabsst2<- Tabsst %>% dplyr::group_by(x,y,Year) %>% dplyr::summarize(moySST= mean(SST))
ggplot(Tabsst2)+
  geom_tile(aes(x=x, y=y, fill=moySST))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("SST")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="°C")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "YlOrRd"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

#ggplot(Tabsst2, aes(x= Year, y=moySST, group=Year))+geom_boxplot()


# Mean SST 1981-2018
#Tabsst3<- Tabsst2 %>% group_by(x,y) %>% summarize(moyper= mean(moySST))
#ggplot(Tabsst3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("SST moyenne 1981-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="SST (°C)")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean SST (year)
Tabsst4<- Tabsst %>% dplyr::group_by(Year) %>% dplyr::summarize(moybaie= mean(SST))

SSTseries<- ggplot(Tabsst4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean Sea Surface Temperature 1982-2018")+
  xlab("Year")+
  ylab("°C")+
  theme_minimal()
SSTseries

# Serie tempo mean SST (month)
Tabsst6<- Tabsst %>% dplyr::group_by(Month) %>% dplyr::summarize(moybaie= mean(SST))

SSTseries2<- ggplot(Tabsst6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean Sea Surface Temperature")+
  xlab("Month")+
  ylab("°C")+
  theme_minimal()
SSTseries2
#####

#Turbidity 1998-2020#####

Turb<- stack("data/env/dataset-oc-atl-opt-multi_cci-l3-adg443_1km_daily-rep_1618837894149.nc")
Turb <-raster::crop(Turb, raster::extent(-0.3,0.3,49.25,49.7))

# Conversion raster
fortify.Raster <- function(Turb, maxPixel = 1000000) {
  
  if (ncell(Turb) > maxPixel) {
    x <- sampleRegular(Turb, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Turb, seq_len(ncell(Turb)))
  out <- Turb %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabTurb<-fortify(Turb)
pixelok<- which(!is.na(apply(TabTurb,1,mean)))
TabTurb<- pivot_longer(TabTurb, cols=1:(dim(TabTurb)[2]-2), names_to = "Date", values_to = "Turbidity", values_drop_na = TRUE)

{
  TabTurb$Date<-sub("values.X","",TabTurb$Date)
  TabTurb$Year <- as.numeric(substr(as.character(TabTurb$Date),1,4))
  TabTurb$Month<- as.numeric(substr(as.character(TabTurb$Date), 6,7))
  TabTurb$Day<- as.numeric(substr(as.character(TabTurb$Date), 9,10))
}

TabTurb <- TabTurb %>% dplyr::filter(Year>1997) #data strart in september

# Mean turb per year
TabTurb2<- TabTurb %>% dplyr::group_by(x,y,Year) %>% dplyr::summarize(moyTurb= mean(Turbidity))
ggplot(TabTurb2)+
  geom_tile(aes(x=x, y=y, fill=moyTurb))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Turbidity")+
  facet_wrap(. ~Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Turbidity")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "PuBu"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

#ggplot(TabTurb2, aes(x=Year, y=moyTurb, group=Year))+
#  geom_boxplot()


# Mean turb 1997-2017
TabTurb3<- TabTurb2 %>% dplyr::group_by(x,y) %>% dplyr::summarize(moyper= mean(moyTurb))
#ggplot(TabTurb3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Turbidité moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean turb (year)
TabTurb4<- TabTurb %>% dplyr::group_by(Year) %>% dplyr::summarize(moybaie= mean(Turbidity))

Turbseries<- ggplot(TabTurb4)+
  geom_line(aes(x=Year, y=moybaie))+
  ggtitle("Mean turbidity 1997-2019")+
  xlab("Year")+
  ylab("m-1")+
  theme_minimal() 
Turbseries

# Serie tempo mean turb (month)
TabTurb6<- TabTurb %>% dplyr::group_by(Month) %>% dplyr::summarize(moybaie= mean(Turbidity))

Turbseries2<- ggplot(TabTurb6)+
  geom_line(aes(x=Month, y=moybaie))+
  ggtitle("Mean turbidity")+
  xlab("Month")+
  ylab("m-1")+
  theme_minimal() 
Turbseries2
#####

#Chlorophyll 1998-2020#####
chl<- stack("data/env/dataset-oc-atl-chl-multi_cci-l4-chl_1km_monthly-rep-v02_1618836890864.nc")
chl<-raster::crop(chl, raster::extent(-0.3,0.3,49.25,49.7))


# Conversion raster - tableau
fortify.Raster <- function(chl, maxPixel = 1000000) {
  
  if (ncell(chl) > maxPixel) {
    x <- sampleRegular(chl, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(chl, seq_len(ncell(chl)))
  out <- chl %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tabchl<- fortify(chl)
pixelok<- which(!is.na(apply(Tabchl,1,mean)))
Tabchl<- tidyr::pivot_longer(Tabchl, cols=1:(dim(Tabchl)[2]-2), names_to = "Date", values_to = "Chloro", values_drop_na = TRUE)

Tabchl$Date<- sub("values.X","",Tabchl$Date)
Tabchl$Year<- as.numeric(substr(as.character(Tabchl$Date),1,4))
Tabchl$Month<- as.numeric(substr(as.character(Tabchl$Date), 6,7))
Tabchl$Day<- as.numeric(substr(as.character(Tabchl$Date), 9,10))

Tabchl <- Tabchl %>% dplyr::filter(Year>1997) #data start in september

# Mean chl per year
Tabchl2<- Tabchl %>% dplyr::group_by(x,y,Year) %>% dplyr::summarize(moyChl= mean(Chloro))
ggplot(Tabchl2)+
  geom_tile(aes(x=x, y=y, fill=moyChl))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Chlorophylle a")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Chlorophylle a")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "PuRd"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

#ggplot(Tabchl2, aes(x= year, y=moyChl, group=year))+
#  geom_boxplot()


# Mean chl 1997-2017
#Tabchl3<- Tabchl2 %>% group_by(x,y) %>% summarize(moyper= mean(moyChl))
#ggplot(Tabchl3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Chlorophylle moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="?g/L")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))


# Serie tempo mean chl (year)
Tabchl4<- Tabchl %>% dplyr::group_by(Year) %>% dplyr::summarize(moybaie= mean(Chloro))

Chlseries<- ggplot(Tabchl4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean chlorophyll 1997-2019")+
  xlab("Year")+
  ylab("mg/m3")+
  theme_minimal() 



# Serie tempo mean chl (month)
Tabchl6<- Tabchl %>% dplyr::group_by(Month) %>% dplyr::summarize(moybaie= mean(Chloro))

Chlseries2<- ggplot(Tabchl6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean chlorophyll")+
  xlab("Month")+
  ylab("mg/m3")+
  theme_minimal() 

Chlseries3<- Chlseries2 +
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15, colour = "blue"))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15, colour = "red"))

#####

#Oxygen 1998-2018#####

O2<- nc_open("data/env/MetO-NWS-BIO-dm-DOXY_1583828769643.nc")
O2<- stack("data/env/MetO-NWS-BIO-dm-DOXY_1583828769643.nc")
O2 <-raster::crop(O2, raster::extent(-0.3,0.3,49.25,49.7))

# Conversion raster - tableau
fortify.Raster <- function(O2, maxPixel = 1000000) {
  
  if (ncell(O2) > maxPixel) {
    x <- sampleRegular(O2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(O2, seq_len(ncell(O2)))
  out <- O2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabO2<- fortify(O2)
pixelok<- which(!is.na(apply(TabO2,1,mean)))
TabO2<- pivot_longer(TabO2, cols=1:7669, names_to = "Date", values_to = "O2", values_drop_na = TRUE)

TabO2$Date<-sub("values.X","",TabO2$Date)
TabO2$Date<-substr(TabO2$Date,1,10)
TabO2$Date<-strptime(TabO2$Date, format= "%Y.%m.%d")

TabO2$Year <- as.numeric(substr(as.character(TabO2$Date),1,4))
TabO2$Month<- as.numeric(substr(as.character(TabO2$Date), 6,7))
TabO2$Day  <- as.numeric(substr(as.character(TabO2$Date), 9,10))


#length(TabO2$O2[TabO2$O2<0])
#NEG<- TabO2 %>% filter(O2<0) %>% select(x, y, O2, Year)
#ggplot(NEG)+ geom_point(aes(x=x, y=y))+ facet_wrap(.~Year)
TabO2<- TabO2 %>% filter(O2>0)
#TabO2 <- TabO2 %>% dplyr::filter(Year %in% NSyears)
# Mean O2 per year
TabO22<- TabO2 %>% dplyr::group_by(x,y,Year) %>% dplyr::summarize(moyO2= mean(O2))
ggplot(TabO22)+
  geom_tile(aes(x=x, y=y, fill=moyO2))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Oxygène dissous")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="O2")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "Purples"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))


#ggplot(TabO22, aes(x=Year, y=moyO2m, group=Year))+geom_boxplot()


# Mean O2 1998-2018
#TabO23<- TabO22 %>% group_by(x,y) %>% summarize(moyper= mean(moyO2))
#ggplot(TabO23)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("O2 moyen 1998-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="mmol/m3")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean 02 (year)
TabO24<- TabO2 %>% dplyr::group_by(Year) %>% dplyr::summarize(moybaie= mean(O2))

O2series<- ggplot(TabO24)+
  geom_line(aes(x=Year, y=moybaie))+
  ggtitle("Mean dissolved oxygen 1998-2018")+
  xlab("Year")+
  ylab("mmol/m3")+
  theme_minimal()  
O2series

# Serie tempo mean 02 (month)
TabO26<- TabO2 %>% dplyr::group_by(Month) %>% dplyr::summarize(moybaie= mean(O2))

O2series2<- ggplot(TabO26)+
  geom_line(aes(x=Month, y=moybaie))+
  ggtitle("Mean dissolved oxygen")+
  xlab("Month")+
  ylab("mmol/m3")+
  theme_minimal()  
O2series2

O2series3<- O2series2 +
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15, colour = "blue"))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15, colour = "red"))

#####

#Seine flow#####
Flow <- read.csv2("data/env/Seine_Vernon_95-19.csv", dec=".")
names(Flow)[3:14] <- 1:12
TabFlow <- Flow %>%tidyr::pivot_longer(3:14, names_to="month", values_to="Values")
#####

#NAO index#####
NAO <- read.table("data/env/nao_monthly_table.txt",head=F)
names(NAO) <- c("Years","01","02","03","04","05","06","07","08","09","10","11","12")
NAO <- NAO %>% tidyr::pivot_longer(cols=2:13, names_to="Month",values_to="Index")
NAO$date <- as.Date(paste0("01-",NAO$Month,"-",NAO$Years), format="%d-%m-%Y")
NAO
#####


#Anomalies#####

# Preparation of the dataset
names(TabSal2)[4] <- "Values"
TabSal2$Parameter <- "Salinity"
names(Tabsst2)[4] <- "Values"
Tabsst2$Parameter <- "SST"
names(TabO22)[4] <- "Values"
TabO22$Parameter <- "Oxygen"
names(TabTurb2)[4] <- "Values"
TabTurb2$Parameter <- "Turbidity"
names(Tabchl2)[4] <- "Values"
Tabchl2$Parameter <- "Chlorophyll"


Env <- rbind(TabSal2,Tabsst2,TabO22,TabTurb2,Tabchl2)

#Cells anomalies

Env.anom <- Env %>% dplyr::group_by(Year, Parameter) %>% dplyr::mutate(Values=Values-mean(Values))

ggplot(Env.anom[Env.anom$Parameter=="Oxygen",])+
  geom_tile(aes(x=x, y=y, fill=Values))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  ggtitle("Turbidity")+
  facet_wrap(. ~Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Turbidity")+
  theme_minimal()+
  scale_fill_viridis_c()+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

#Annual anomalies

Env.anomY<- Env %>% dplyr::group_by(Year, Parameter) %>% dplyr::summarise(Values= mean(Values)) %>%
  dplyr::group_by(Parameter) %>% dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
TabFlow2 <- TabFlow %>% dplyr::group_by(Year) %>% dplyr::summarise(Values= mean(Values,na.rm=T)) %>%
  dplyr::mutate(Values= Values-  mean(Values))
TabFlow2$Parameter<- "Flow"
TabFlow2 <- TabFlow2[,c(1,3,2)]
Env.anomY <- rbind(Env.anomY, TabFlow2)
Env.anomY$absolute <- ifelse(Env.anomY$Values<0,1,2)
Env.anomY <- Env.anomY[Env.anomY$Parameter%in%c("Chlorophyll","Flow","SST","Turbidity"),]

ggplot(Env.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Environmental anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_wrap(Parameter~., scales="free_y")+
  theme_minimal()+
  guides(fill=F)
#####

#Populations trends CGFS#####

require(icesDatras)


###Get Channel Ground Fish Survey (CGFS) data from DATRAS #####
#Is time consuming as files are heavy (especially HL data)

view <- getDatrasDataOverview(surveys = "FR-CGFS", long = FALSE) #Check data availability
HH_CGFS <- getDATRAS(record="HH", survey="FR-CGFS", year=colnames(view$`FR-CGFS`),
                     quarters=4) #Haul data
HL_CGFS <- getDATRAS(record="HL", survey="FR-CGFS", year=colnames(view$`FR-CGFS`),
                     quarters=4) #Length-based data
CA_CGFS <- getDATRAS(record="CA", survey="FR-CGFS", year=colnames(view$`FR-CGFS`),
                     quarters=4) #Age-based data


#Keep original data intact
HH <- HH_CGFS
HL <- HL_CGFS
CA <- CA_CGFS

##Data cleaning

HH[which(HH==-9, arr.ind = T)]<- NA #Convert -9 to NA as it is a convention 
HL[which(HL==-9, arr.ind = T)]<- NA
CA[which(CA==-9, arr.ind = T)]<- NA

#Remove variables unused later
CA <- CA[,-c(1,2,3,4,5,6,7,8,9,13,15,16,17,21,25)]
HH<- HH[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 18, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,55, 56, 57, 58, 59, 60, 61)] # RecordType, Survey, Quarter, Country, Ship, Gear, SweepLngt, GearExp (NA), DoorType, Mois, Jour, TimeShot, Stratum (NA), DayNight, Depth, HaulVal, HydroStNo, StdSpecRecCode, BycSpecRecCode, DataType, NetOpening, Rigging (Na), Tickler (NA), WarpLgnt, WarpDia, WarpDen (NA), DoorSurface, DoorWgt, DoorSpread, Buoyancy, KiteDim (NA), WgtGroundRope, TowDir, GroundSpeed, SpeedWater, SurCurDir (NA), SurCurSpeed (NA),BotCurDir(NA), BotCurSpeed (NA), WindDir, WindSpeed, SwellDir(NA), SwellHeight(NA), SurTemp, BotTemp, SurSal, BotSal, ThermoCline (NA), ThClineDepth (NA), DateofCalculation. 
HL<- HL[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 15, 16, 18, 19, 20, 21, 23, 28)] # RecordType, Survey, Quarter (all 4), Country, Ship (GWD ou THA2), Gear, SweepLngt, GearExp (NA), DoorType (P ou R), SpecCodeType (all W), SpecVal, Sex, CatIdentifier, NoMeas, SubFactor, SubWgt (NA), LngtCode, Date of calculation.
HL<- unique(HL)

#Complete and choose species variable 
HL$Valid_Aphia[is.na(HL$Valid_Aphia)]<-	HL$SpecCode[is.na(HL$Valid_Aphia)] #Replace AphiaID with SpecCode more complete
HL<- HL[!is.na(HL$Valid_Aphia),] #Keep rows with a species code
HL<- HL[, -4] #Remove the redundant variable

#WingSpread
Wing<- read.csv(file= "data/env/WingCGFS2015_2019 original.csv", sep=";") #Complete the WingSpread variable. File obtained by IFREMER
Wing<- Wing[, -c(1, 2, 4, 5, 6)]
names(Wing)[1]<- "StNo"
names(Wing)[2]<- "WingSpread"


# Joint tables
J1<- HH %>% left_join(Wing, by= c("StNo"))
J1$WingSpread.x[is.na(J1$WingSpread.x)]<-	J1$WingSpread.y[is.na(J1$WingSpread.x)]
J1<- J1[, -11]
names(J1)[10]<- "WingSpread"
J1$WingSpread[is.na(J1$WingSpread)]<- 10 #Replace NA with the standard value

J2<- HL %>% left_join(J1, by= c("StNo", "HaulNo", "Year"))

J3 <- J2 %>% left_join(CA, by=c("StNo", "HaulNo", "Year","LngtClass","Valid_Aphia"))


J3<- J3 %>%
  mutate(Superficie= Distance*WingSpread) %>%
  mutate(meanLat= (ShootLat+HaulLat)/2) %>%
  mutate(meanLong= (ShootLong+HaulLong)/2)

J3<- J3 %>%
  group_by(Year,meanLong,meanLat,Valid_Aphia) %>%
  mutate(Density=mean(TotalNo*(HaulDur/60)/Superficie)) %>% #TotalNo is number of individuals caught per hour. Scale to get number of individuals caught during the fishing operation 
  ungroup()

Species_name <- wormsbyid(unique(J3$Valid_Aphia)) #Adding species names from WORMS
J3 <- J3 %>% left_join(Species_name[,c(3,9)], by=c("Valid_Aphia"="valid_AphiaID"))

CGFS<- J3 %>% dplyr::select(StNo, Year, Superficie,Distance, meanLat, meanLong, HaulDur, scientificname,LngtClass,HLNoAtLngt, TotalNo, Sex, Age, Maturity, NoAtALK, IndWgt,Density)


# Final dataset
#write.csv(CGFS, file= "data/env/CGFS.csv")
#####
CGFS <- read.csv("data/env/CGFS.csv")

#Packages needed
library(rnaturalearth)
library(rnaturalearthdata)

#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(ggplot2)

world <- ne_countries(scale=10, returnclass = "sf")

#Species reduction and pca####
ClassifCGFS <- worms::wormsbynames(as.character(unique(CGFS$scientificname)),match = T)
SpCGFS_keep <- ClassifCGFS$name[ClassifCGFS$class %in% c("Actinopterygii","Elasmobranchii")]
CGFS <- CGFS[CGFS$scientificname %in% SpCGFS_keep,]


#Table formating: removing size info
CGFS.red <- CGFS %>%  filter(-1.5<=meanLong & meanLong <= 0.5 & 49<=meanLat & meanLat<=49.8)%>%
  group_by(StNo,Year,meanLat,meanLong,scientificname) %>% 
  dplyr::select(-X,-HaulDur,-LngtClass,-HLNoAtLngt,-Sex,-Age,-Maturity,-NoAtALK,-IndWgt, -TotalNo)%>% unique()

Comm.CGFS <- CGFS.red %>% tidyr::pivot_wider(1:6, names_from="scientificname", values_from="Density", values_fill=0)

metadata.CGFS <- Comm.CGFS[,c(1:6)]
Comm.CGFS <- Comm.CGFS %>% ungroup() %>% dplyr::select(-StNo,-Year,-meanLong,-meanLat,-Distance,-Superficie)


Comm.hel <- decostand(Comm.CGFS, "hellinger")
res.pca <- prcomp(Comm.hel)

ind12 <- fviz_pca_ind(res.pca,geom="point",title="PCA fish community - 1/2")
ind13 <- fviz_pca_ind(res.pca,geom="point",title="PCA fish community - 1/3", axes = c(1,3))
ind23 <- fviz_pca_ind(res.pca,geom="point",title="PCA fish community - 1/3", axes = c(2,3))

var12 <- fviz_pca_var(res.pca,title="PCA fish community - 1/2", select.var = list(cos2=10), labelsize=4, repel=T)
var13 <- fviz_pca_var(res.pca,title="PCA fish community - 1/3", select.var = list(cos2=10), labelsize=4, repel=T, axes = c(1,3))
var23 <- fviz_pca_var(res.pca,title="PCA fish community - 2/3", select.var = list(cos2=10), labelsize=4, repel=T, axes = c(2,3))

cowplot::plot_grid(ind12,var12,ind13,var13,ind23,var23,ncol=2)

res.pca <- PCA(Comm.hel, scale.unit = F)
res.pca$ind$coord[,1] <- (-1)*res.pca$ind$coord[,1] 
#res.pca$ind$coord[,2] <- (-1)*res.pca$ind$coord[,2]
res.pca$ind$coord[,3] <- (-1)*res.pca$ind$coord[,3]
res.hcpc <-  HCPC(res.pca, nb.clust = 6)


res.hcpc$desc.var$quanti$`1`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`2`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`3`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`4`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`5`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`6`[1:6,c(1,2,3,6)]

Clust <- cbind(metadata.CGFS, Cluster=res.hcpc$data.clust$clust)
ggplot(Clust)+
  geom_sf(data = world, fill = "grey") + 
  coord_sf(xlim = c(-1.5, 0.5), ylim = c(49.2, 49.8), expand = FALSE)+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  geom_point(aes(x=meanLong,y=meanLat,col=Cluster))+
  scale_color_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(res.hcpc$data.clust$clust))))+
  facet_wrap(.~Year)+
  guides(col=guide_legend("Cluster"))+
  theme(axis.text.x = element_text(angle = 30, hjust=1), legend.position = "bottom")

tileplot <- Clust %>% group_by(Year, Cluster) %>% dplyr::mutate(Number=length(StNo)) %>% ungroup() 

ggplot(tileplot)+
  geom_point(aes(x=Year,y=Cluster,col=Cluster, size=Number),alpha=0.2)+
  scale_color_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(tileplot$Cluster))))




CGFS.red <- CGFS %>%  filter(-1.5<=meanLong & meanLong <= 0.5 & 49<=meanLat & meanLat<=49.8)%>%
  group_by(StNo,Year,meanLat,meanLong,scientificname) %>% 
  dplyr::select(-X,-HaulDur,-LngtClass,-HLNoAtLngt,-Sex,-Age,-Maturity,-NoAtALK,-IndWgt, -TotalNo)%>% unique()


CGFS.red.anomY<- CGFS.red %>% dplyr::group_by(Year, scientificname) %>% dplyr::summarise(Density= mean(Density)) %>%
  dplyr::group_by(scientificname) %>% dplyr::mutate(Density= Density-  mean(Density))
Env.anomY$absolute <- ifelse(Env.anomY$Density<0,1,2)

ggplot(Env.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Environmental anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_wrap(Parameter~., scales="free_y")+
  theme_minimal()
#####

#CGFS functionnal groups#####
datatrait <- read.csv("data/Trait.csv", head=T, sep=";", dec=",")

#RSpecies list
sp <- SpCGFS_keep

table(sp %in% datatrait$taxon) #2 manquants  Dasyatis tortonesei & Hippocampus
sp[which(c(sp %in% datatrait$taxon)==F)]
sp_t <- sp[which(c(sp %in% datatrait$taxon)==T)]

datatrait_red <- datatrait[datatrait$taxon %in% sp_t,]
table(datatrait_red$taxon, datatrait_red$LME)[which(rowSums(table(datatrait_red$taxon, datatrait_red$LME))>0),]

datatrait_red <- unique(rbind(datatrait_red[datatrait_red$taxon=="Lepadogaster" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Alopias vulpinus" & datatrait_red$LME==7,],
                              datatrait_red[datatrait_red$taxon=="Diplecogaster bimaculata bimaculata" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Gobius paganellus" & datatrait_red$LME==24,],
                              #datatrait_red[datatrait_red$taxon=="Hippocampus hippocampus" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Microchirus" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Pagellus bogaraveo" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Pagellus erythrinus" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Symphodus bailloni" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Symphodus roissali" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Trigla lyra" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$taxon=="Zeugopterus regius" & datatrait_red$LME==24,],
                              datatrait_red[datatrait_red$LME==22,]))

#write.csv(datatrait_red, "data/Trait_red.csv")
#Lepadogaster 24 but all 22
#datatrait_red <- read.csv2("data/Trait_red.csv")
sp_t <- data.frame(taxon=sp_t)

trait.df <- dplyr::left_join(sp_t,datatrait_red, by="taxon")
trait.df <- trait.df[,!c(names(trait.df) %in% names(trait.df)[grepl("reference",names(trait.df))])]
trait.df <- trait.df %>% dplyr::select(taxon,habitat,feeding.mode,body.shape,fin.shape,AR,offspring.size,spawning.type,age.maturity,length.max,age.max,growth.coefficient,tl,fecundity)
trait.df <- na.omit(unique(trait.df))


# Categorize numerical values
catvar<- function(a,value){
  value<- c(0,as.vector(value))
  print(value)
  lval<- length(value)
  interval<- paste0("[",round(value[-lval],3),",",round(value[-1],3),"[")
  a1<- interval[findInterval(a,value,all.inside=T)]
  return(a1)
}

trait.df$tl<-                 catvar(trait.df$tl, c(2,3,3.5,4,5))
trait.df$AR<-                 catvar(trait.df$AR, c(1,2,3,4,6))
trait.df$offspring.size<-     catvar(trait.df$offspring.size, c(10,24,50,100,200,500,1500))
trait.df$length.max<-         catvar(trait.df$length.max, c(10,30,50,80,100,150,250,500))
trait.df$age.max<-            catvar(trait.df$age.max, c(2,5,10,15,20,30,50,80))
trait.df$fecundity<-          catvar(trait.df$fecundity, c(100,10000,100000,1000000000))
trait.df$age.maturity<-       catvar(trait.df$age.maturity, c(1,2,3,4,5,15))
trait.df$growth.coefficient<- catvar(trait.df$growth.coefficient, c(0.5,1,1.5,2,2.5))



toto <- as.matrix(trait.df[,-1])
rownames(toto) <- trait.df[,1]
res <- MCA(toto)
res.hcpc <- HCPC(res, consol=F)

res.hcpc$desc.var$category
res.hcpc$desc.ind$dist

# trait.df.clust <- data.frame(taxon=trait.df$taxon, res.hcpc$data.clust$clust)
# trait.df.clust <- left_join(data.frame(taxon=sp_t), trait.df.clust, by="taxon")
#####


#Pelagic and Benthogeneralist species in the bay of Seine

CGFS.red.Y <- CGFS.red %>% dplyr::group_by(Year, scientificname) %>% dplyr::summarise(Density=mean(Density))

#Uses Comm_analysis and trait.df.clust
Pelagic <- na.omit(trait.df.clust[trait.df.clust$res.hcpc.data.clust.clust==2,])
Generalist <-na.omit(trait.df.clust[trait.df.clust$res.hcpc.data.clust.clust==4,])

CGFS.red.Y <- CGFS.red.Y[CGFS.red.Y$scientificname %in% c(as.character(Pelagic$taxon),as.character(Generalist$taxon)),]
CGFS.red.Y$Group <- 0
CGFS.red.Y$Group[CGFS.red.Y$scientificname %in% as.character(Pelagic$taxon)] <- "Pelagic"
CGFS.red.Y$Group[CGFS.red.Y$scientificname %in% as.character(Generalist$taxon)] <- "Benthopelagic"

ggplot(CGFS.red.Y)+
  geom_line(aes(x=Year,y=Density,col=Group, group=scientificname))+facet_wrap(.~Group, scales="free_y")


CGFS.red.Y2 <- CGFS.red.Y %>% dplyr::group_by(Year,Group) %>% dplyr::summarise(Density=mean(Density))

ggplot(CGFS.red.Y)+
  geom_bar(aes(x=Year,y=Density,fill=Group),position = "dodge", stat = "identity")+facet_wrap(.~Group, scales="free_y")





library(dplyr)
toto <- data.frame(sp=c("A","B","C"))
gp.1 <- data.frame(sp=c("A","C"), gp=c(1,2))
gp.2 <- data.frame(sp=c("B"), gp=c(1))

gp <- rbind(gp.1, gp.2)
toto <- toto %>% dplyr::left_join(gp, by=c("sp"="sp")) #Ajout des groupes qui crée la colonne gp dans toto

#####NAO###

NAO <- read.table("data/env/nao_station_annual.txt",head=F, sep="\t")
NAO$V1 <- as.character(NAO$V1)
NAOlist <- strsplit(NAO$V1,"  ")
NAO <- data.frame(Years=as.numeric(unlist(lapply(NAOlist,"[[",1))),
                  Index=as.numeric(unlist(lapply(NAOlist,"[[",2))))
ggplot(NAO[NAO$Years>1990,], aes(x=Years,y=Index))+geom_bar(stat = "identity")

NAO <- read.table("data/env/nao_monthly_table.txt",head=F)
names(NAO) <- c("Years","01","02","03","04","05","06","07","08","09","10","11","12")
NAO <- NAO %>% tidyr::pivot_longer(cols=2:13, names_to="Month",values_to="Index")
NAO$date <- as.Date(paste0("01-",NAO$Month,"-",NAO$Years), format="%d-%m-%Y")
ggplot(NAO[NAO$Month=="09",], aes(x=date,y=Index))+geom_bar(stat = "identity")
#####