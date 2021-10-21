#Fish Community analyses

#Libraries
require(tidyr)
require(dplyr)
require(ggpubr)
require(worms)
library("factoextra")
require(FactoMineR)
require(vegan)


#Data formatting for NOURSEINE with juveniles#####
Nourseine <- read.csv("data/NOURSEINE_dataset_1995-2019.csv", head=T)

##Create single points coordinates for each station
NSj.df <- Nourseine %>% mutate(meanLat=(Starting_Latitude_decimal+Ending_Latitude_decimal)/2,
                              meanLon=(Starting_Longitude_decimal+Ending_Longitude_decimal)/2) %>%
  dplyr::select(-Starting_Latitude_decimal,-Starting_Longitude_decimal,-Ending_Latitude_decimal,-Ending_Longitude_decimal)


#According to size distribution in Age group, we redefine the group variable according to size and remove any information when only a couple years haven been embedded
NSj.df$Age_Group <- as.character(NSj.df$Age_Group)
NSj.df$Age_Group[NSj.df$Age_Group %in% c("G2","G2+","G3","G4","G5","G7","G14")] <- "G2p"
#ggplot(NS.df[NS.df$Scientific_Name =="Platichthys flesus",])+
#  geom_boxplot(aes(x=Age_Group,y=Size))


NSj.df$Age_Group [NSj.df$Scientific_Name =="Clupea harengus" & NSj.df$Size <=11] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Clupea harengus" & NSj.df$Size >11 & NSj.df$Size <=21] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Clupea harengus" & NSj.df$Size >21] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Dicentrarchus labrax" & NSj.df$Size <=11] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Dicentrarchus labrax" & NSj.df$Size >11 & NSj.df$Size <=21] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Dicentrarchus labrax" & NSj.df$Size >21] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Merlangius merlangus" & NSj.df$Size <=20] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Merlangius merlangus" & NSj.df$Size >20 & NSj.df$Size <=27] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Merlangius merlangus" & NSj.df$Size >27] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Limanda limanda" & NSj.df$Size <=10] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Limanda limanda" & NSj.df$Size >10 & NSj.df$Size <=22] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Limanda limanda" & NSj.df$Size>22] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Platichthys flesus" & NSj.df$Size <=12] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Platichthys flesus" & NSj.df$Size >12 & NSj.df$Size <=20] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Platichthys flesus" & NSj.df$Size >20] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Pleuronectes platessa" & NSj.df$Size<=17] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Pleuronectes platessa" & NSj.df$Size>17 & NSj.df$Size <=27] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Pleuronectes platessa" & NSj.df$Size >27] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Solea solea" & NSj.df$Size <=14] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Solea solea" & NSj.df$Size >14 & NSj.df$Size <=22] <- "G1"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Solea solea" & NSj.df$Size >22] <- "G2p"

NSj.df$Age_Group [NSj.df$Scientific_Name =="Sprattus sprattus" & NSj.df$Size <=14] <- "G0"
NSj.df$Age_Group [NSj.df$Scientific_Name =="Sprattus sprattus" & NSj.df$Size >14 ] <- "G1"



NSj.df$Age_Group [NSj.df$Scientific_Name =="Ammodytes spp"] <- NA #2 G1 before
NSj.df$Age_Group [NSj.df$Scientific_Name =="Chelidonichthys lucerna"] <- NA #94% of NA
NSj.df$Age_Group [NSj.df$Scientific_Name =="Engraulis encrasicolus"] <- NA #1 G0 before
NSj.df$Age_Group [NSj.df$Scientific_Name =="Pollachius pollachius"] <- NA #3 G0 before
NSj.df$Age_Group [NSj.df$Scientific_Name =="Scophtalmus maximus"] <- NA #1 G0 and 1 G1 before
NSj.df$Age_Group [NSj.df$Scientific_Name =="Scophthalmus rhombus"] <- NA #1 G0 before
NSj.df$Age_Group [NSj.df$Scientific_Name =="Trachurus trachurus"] <- NA #90% NA 
NSj.df$Age_Group [NSj.df$Scientific_Name =="Trisopterus luscus"] <- NA #All G0
NSj.df$Age_Group [NSj.df$Scientific_Name =="Pecten maximus"]  <- NA #2 G2p and 1 G1
NSj.df$Age_Group [NSj.df$Scientific_Name =="Mullus surmuletus"] <- NA #3 G0 before
NSj.df$Age_Group [NSj.df$Scientific_Name =="Spondyliosoma cantharus"] <- NA #1 G0 before

NSj.df <- NSj.df[!c(NSj.df$Scientific_Name %in% c("Clupea harengus", "Dicentrarchus labrax", "Merlangius merlangus",
                                                "Limanda limanda","Platichthys flesus","Pleuronectes platessa","Solea solea",
                                                "Sprattus sprattus") & 
                    is.na(NSj.df$Size)),]

##Recalculating densities with agre_group and adding rows with 0 densities
NSj.df$Scientific_Name <- as.character(NSj.df$Scientific_Name)
NSj.df[NSj.df$Scientific_Name %in% unique(NSj.df$Scientific_Name[which(NSj.df$Age_Group %in% c("G0","G1","G2p"))]),]$Scientific_Name <- paste(NSj.df[NSj.df$Scientific_Name %in% unique(NSj.df$Scientific_Name[which(NSj.df$Age_Group %in% c("G0","G1","G2p"))]),]$Scientific_Name,
                                                                                                                                              NSj.df[NSj.df$Scientific_Name %in% unique(NSj.df$Scientific_Name[which(NSj.df$Age_Group %in% c("G0","G1","G2p"))]),]$Age_Group, sep="_")
NSj.df <- NSj.df %>% mutate(Number_Measured=ifelse(is.na(Number_Measured),Number_in_Haul,Number_Measured)) %>% group_by(Station_Code,Scientific_Name) %>% mutate(Density=sum(Number_Measured)*Division/Trawled_Surface_m2) %>% ungroup


ttsel <- NSj.df %>% dplyr::select(Station_Code, Scientific_Name, Year, meanLon, meanLat, Species_Density, Age_Group) %>% distinct() 
codif <- ttsel %>% dplyr::select(Station_Code, Year, meanLon, meanLat) %>% distinct() 
allstrat <- ttsel %>% expand(Station_Code, Scientific_Name) 
NSj.df0 <- left_join(allstrat, codif) %>% left_join(ttsel) %>% dplyr::select(Station_Code, meanLon, meanLat, Scientific_Name, Species_Density) %>% mutate(Species_Density = ifelse(is.na(Species_Density), 0, Species_Density))
NSj.df0 <- left_join(codif, NSj.df0) %>% dplyr::select(Station_Code, Year, meanLon, meanLat, Scientific_Name, Species_Density) %>% unique() 
#####

#Filtering species: keeping fish, map sector####
Classif <- worms::wormsbynames(unique(NSj.df0$Scientific_Name),match = T)
Sp_keep <- Classif$name[Classif$class %in% c("Actinopteri","Elasmobranchii","Bacillariophyceae")] #"Bacillariophyceae" for flounder
NSj.df0 <- NSj.df0[NSj.df0$Scientific_Name %in% Sp_keep,]

# Matrix
Comm <- NSj.df0 %>% pivot_wider(names_from = "Scientific_Name",values_from="Species_Density", values_fn=mean)
metadata <- Comm[,c(1:4)]
metadata <- unique(dplyr::left_join(metadata,NSj.df[,c("Station_Code","Sector","Starting_Depth","Ending_Depth")], by="Station_Code"))
metadata$Depth <- rowSums(metadata[,c(6,7)], na.rm=T)
metadata$Depth[!is.na(rowSums(metadata[,c(6,7)]))] <- metadata$Depth[!is.na(rowSums(metadata[,c(6,7)]))]/2
metadata$Depth <- cut(metadata$Depth, breaks = c(0,5,10,15,20,25,30,35), include.lowest = T)
ggplot(metadata)+
  geom_text(aes(x=meanLon,y=meanLat,label=Sector))
Comm <- Comm %>% dplyr::select(-Station_Code,-Year,-meanLon,-meanLat)

coast <- rgdal::readOGR("data/Coast_estuary_detailled.gpkg")
coast <- raster::crop(coast, raster::extent(-0.3,0.3,49.25,49.7))
load("data/stratepoly.RData")
coordinates <-metadata[,c("meanLon","meanLat")]
coordinates <- cbind(as.data.frame(coordinates), Sector = metadata$Sector)
centro <- aggregate(cbind(meanLon, meanLat) ~ Sector, data = coordinates, FUN = mean)
coast.fort <- fortify(coast)

ggplot()+
  #geom_point(data=metadata,aes(x=meanLon,y=meanLat))+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = stratepoly.df, aes(x = long, y = lat, group = group),fill=NA, col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  coord_fixed()+
  geom_text(data=centro,aes(x=meanLon,y=meanLat,label=Sector), col="navy",fontface = "bold")

#####

#Biodiversity analysis#####
library(labdsv)
library(vegan)

#abuocc(Comm)
H <- diversity(Comm)   #Shannon index: diversity
J <- H/log(specnumber(Comm)) #Pielou eveness
S <- specnumber(Comm)
Density <- rowSums(Comm)
Juveniles <- rowSums(Comm[,which(grepl("G0", colnames(Comm)))])
  
index <- data.frame(Richness=as.numeric(S),Shannon=as.numeric(H),Pielou=as.numeric(J),Density=as.numeric(Density),Juveniles=as.numeric(Juveniles), Sector = as.character(metadata$Sector), Year = metadata$Year)
index_sector <- aggregate(cbind(S, H, J,Density,Juveniles) ~ Sector, data = index, FUN = mean)
index_year <- aggregate(cbind(S, H, J) ~ Year, data = index, FUN = mean)

index_sector_long <- pivot_longer(index_sector,col=c(2:6), names_to = "Index", values_to = "Values")
index_year_long <- pivot_longer(index_year,col=c(2:4), names_to = "Index", values_to = "Values")
as.data.frame(index)

Srate_color <- left_join(stratepoly.df,index_sector,by=c("Ifremer_id"="Sector"))

H.plot <- ggplot()+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = Srate_color, aes(x = long, y = lat, group = group, fill=H), col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  coord_fixed()+
  geom_text(data=centro,aes(x=meanLon,y=meanLat,label=Sector), col="navy",fontface = "bold")+
  scale_fill_viridis_c()+theme(legend.position = "bottom")+
  theme(legend.key.size = unit(1,"cm"))

J.plot <- ggplot()+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = Srate_color, aes(x = long, y = lat, group = group, fill=J), col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  coord_fixed()+
  geom_text(data=centro,aes(x=meanLon,y=meanLat,label=Sector), col="navy",fontface = "bold")+
  scale_fill_viridis_c()+theme(legend.position = "bottom")+
  theme(legend.key.size = unit(1,"cm"))

# cowplot::plot_grid(H.plot,J.plot)

Dens.plot <- ggplot()+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = Srate_color, aes(x = long, y = lat, group = group, fill=Density), col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  coord_fixed()+
  geom_text(data=centro,aes(x=meanLon,y=meanLat,label=Sector), col="navy",fontface = "bold")+
  scale_fill_viridis_c()+theme(legend.position = "bottom")+
  theme(legend.key.size = unit(1,"cm"))
Juv.plot <- ggplot()+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = Srate_color, aes(x = long, y = lat, group = group, fill=Juveniles), col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  coord_fixed()+
  geom_text(data=centro,aes(x=meanLon,y=meanLat,label=Sector), col="navy",fontface = "bold")+
  scale_fill_viridis_c()+theme(legend.position = "bottom")+
  theme(legend.key.size = unit(1,"cm"))

cowplot::plot_grid(H.plot,NULL,J.plot,Dens.plot,NULL,Juv.plot, nrow=2, rel_widths = c(1,-0.3,1))

index_year_long$group <- ifelse(as.numeric(index_year_long$Year)<9,1,2)
index_year_long$group <- ifelse(as.numeric(index_year_long$Year)>11,3,index_year_long$group)

ggplot(index_year_long)+
  geom_point(aes(x=Year,y=Values,col=Index))+
  geom_line(aes(x=Year,y=Values,col=Index, group=interaction(Index,group)))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  geom_vline(xintercept = 8.5, linetype=2, col="grey40") + 
  geom_vline(xintercept = 11.5, linetype=2, col="grey40")+ 
  scale_color_manual(values=c("#00AFBB","#E7B800", "#FC4E07"))

index <- cbind(Richness=S, Shannon=H,Pielou=J, Sector = as.character(metadata$Sector), Year = metadata$Year)
index_long <- aggregate(cbind(S, H, J) ~ Sector+Year, data = index, FUN = mean)
index_long <- pivot_longer(index_long, cols=c(3:5), values_to="Values",names_to="Index")

index_long$group <- ifelse(as.numeric(index_long$Year)<9,1,2)
index_long$group <- ifelse(as.numeric(index_long$Year)>11,3,index_long$group)

ggplot(index_long)+
  geom_point(aes(x=Year,y=Values,col=Sector))+
  geom_line(aes(x=Year,y=Values,col=Sector, group=interaction(Sector,group)))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  geom_vline(xintercept = 8.5, linetype=2, col="grey40") + 
  geom_vline(xintercept = 11.5, linetype=2, col="grey40")+ 
  scale_color_manual(values=get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(index_long$Sector))))+
  facet_grid(Index~., scales = "free_y")

RelAbund <- sort(colSums(Comm)/sum(Comm)*100, decreasing = T)[1:15]
RelAbund <- data.frame(Species=names(RelAbund),Proportion=RelAbund)
RelAbund$Species <- factor(RelAbund$Species, ordered = T, levels=RelAbund$Species)
ggplot(RelAbund)+
  geom_bar(aes(x=Species,y=Proportion),stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#Index anom
indexY <- index %>% dplyr::select(-Richness, -Sector) %>% tidyr::pivot_longer(cols=c(1:4), values_to="Values",names_to="Index")
Ind.anomY<- indexY %>% dplyr::group_by(Year, Index) %>% dplyr::summarise(Values= mean(Values, na.rm=T)) %>%
  dplyr::group_by(Index) %>% dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
Ind.anomY$absolute <- ifelse(Ind.anomY$Values<0,1,2)
Ind.anomY$Index <- factor(Ind.anomY$Index, ordered=T, levels = c("Shannon","Pielou","Density","Juveniles"))
ggplot(Ind.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Biodiversity indices anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_wrap(Index~., scales=c("free"))+
  theme_minimal()+
  guides(fill=F)


#####

#PCA####
Comm.hel <- decostand(Comm, "hellinger")
#Comm.hel <- Comm.hel %>% dplyr::select(-`Callionymus lyra`,`Buglossidium luteum`,-Gobiidae)
res.pca <- prcomp(Comm.hel)

#Commn99 <- Comm[metadata$Year!=1999,]
#Comm.hel <- decostand(Commn99, "hellinger")
#res.pca <- prcomp(Comm.hel)  #48% au lieu de 50% a l'axe 3

eig.val <- get_eigenvalue(res.pca)
head(eig.val)
# fviz_screeplot(res.pca)
# fviz_cos2(res.pca, choice="var", top=10, axes = 1)
# fviz_cos2(res.pca, choice="var", top=10, axes = 2)
# fviz_cos2(res.pca, choice="var", top=10, axes = 3)
# fviz_cos2(res.pca, choice="var", top=10, axes = 4)
# fviz_contrib(res.pca, choice="var", top=10, axes = 1)
# fviz_contrib(res.pca, choice="var", top=10, axes = 2)
# fviz_contrib(res.pca, choice="var", top=10, axes = 3)
# fviz_contrib(res.pca, choice="var", top=10, axes = 4)
ind12 <- fviz_pca_ind(res.pca,geom="point",title="PCA fish community - 1/2")
ind13 <- fviz_pca_ind(res.pca,geom="point",title="PCA fish community - 1/3", axes = c(1,3))
ind23 <- fviz_pca_ind(res.pca,geom="point",title="PCA fish community - 1/3", axes = c(2,3))

var12 <- fviz_pca_var(res.pca,title="PCA fish community - 1/2", select.var = list(cos2=10), labelsize=4, repel=T)
var13 <- fviz_pca_var(res.pca,title="PCA fish community - 1/3", select.var = list(cos2=10), labelsize=4, repel=T, axes = c(1,3))
var23 <- fviz_pca_var(res.pca,title="PCA fish community - 2/3", select.var = list(cos2=10), labelsize=4, repel=T, axes = c(2,3))

cowplot::plot_grid(ind12,var12,ind13,var13,ind23,var23,ncol=2)


cowplot::plot_grid(var12,var13,ncol=2)


# groups <- factor(metadata$Sector)
# pca12_group <-fviz_pca_ind(res.pca, geom="point", 
#                         col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
#                         addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
#                         repel = TRUE, title="PCA fish community - Group Sector")
# pca12_group
# pca13_group <-fviz_pca_ind(res.pca, geom="point", axes = c(1, 3),
#                            col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
#                            addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
#                            repel = TRUE, title="PCA fish community - Group Sector")
# pca13_group
# pca23_group <-fviz_pca_ind(res.pca, geom="point", axes = c(2, 3),
#                            col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
#                            addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
#                            repel = TRUE, title="PCA fish community - Group Sector")
# pca23_group

#pca12_sector <- pca12_group ; pca13_sector <- pca13_group ;pca23_sector <-  pca23_group
#pca12_year <- pca12_group ; pca13_year <- pca13_group ;pca23_year <-  pca23_group
#pca12_depth <- pca12_group ; pca13_depth <- pca13_group ;pca23_depth <-  pca23_group

# cowplot::plot_grid(pca12_sector,pca12_year, pca13_sector,pca13_year,ncol=2)


#fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", 
#                select.var=list(contrib=10), geom.ind="point", 
#                title="PCA fish community - Axes 1/2 ") +
#  theme(legend.position = "none")

#fviz_pca_biplot(res.pca,axes=c(1,3), repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", 
#                select.var=list(contrib=10), geom.ind="point", 
#                title="PCA fish community - Axes 1/3 ") +
#  theme(legend.position = "none")

#fviz_pca_biplot(res.pca,axes=c(5,7), repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", 
 #               select.var=list(contrib=10), geom.ind="point", 
  #              title="PCA fish community - Axes 1/3 ") +
  #theme(legend.position = "none")

#Ordispider sector
var <- get_pca_var(res.pca)
ind <- get_pca_ind(res.pca)

scores <-ind$coord
scores <- cbind(as.data.frame(scores), Sector = metadata$Sector)
cent <- aggregate(cbind(Dim.1, Dim.2) ~ Sector, data = scores, FUN = mean)
segs <- merge(scores, setNames(cent, c("Sector","oDim1","oDim2")),
              by = "Sector", sort = FALSE)

groups <- factor(metadata$Sector)
pal <- get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups)))
pca_sector <-ggplot(scores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_segment(data = segs,
               mapping = aes(xend = oDim1, yend = oDim2),
               col="grey80") + # spiders
  geom_point(data = cent, size = 6, aes(color=Sector)) +  
  geom_text(data=cent, aes(label=Sector),color="black")+
  #geom_point() +                                             
  coord_fixed() +                                              
  scale_color_manual(values=pal)+ xlab("Dim1 (27.8%)")+ylab("Dim2 (13.4%)")+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))

#Ordispider Year
var <- get_pca_var(res.pca)
ind <- get_pca_ind(res.pca)

scores <-ind$coord
scores <- cbind(as.data.frame(scores), Year = as.factor(metadata$Year))
cent <- aggregate(cbind(Dim.1, Dim.2) ~ Year, data = scores, FUN = mean)
segs <- merge(scores, setNames(cent, c("Year","oDim1","oDim2")),
              by = "Year", sort = FALSE)

groups <- factor(metadata$Year)
pal <- get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups)))
pca_year <- ggplot(scores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_segment(data = segs,
               mapping = aes(xend = oDim1, yend = oDim2),
               col="grey80") + # spiders
  geom_point(data = cent, size = 6, aes(color=Year)) +  
  geom_text(data=cent, aes(label=Year),color="black")+
  #geom_point() +                                             
  coord_fixed() +                                              
  scale_color_manual(values=pal)+ xlab("Dim1 (27.8%)")+ylab("Dim2 (13.4%)")+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow=3,byrow=TRUE))

cowplot::plot_grid(pca_sector,pca_year)

#Trawl contributing to pca axes
# PCA.coord <- data.frame(ind=1:dim(Comm)[1],
#                         lon=metadata$meanLon,
#                         lat=metadata$meanLat,
#                         ind$coord[,c(1:3)])
# 
# PCA.coord <- pivot_longer(PCA.coord,cols = c(4:6), values_to="Coordinates", names_to="Dim")
# ind$contrib[,"Dim.1"]>100/dim(Comm)[1]
# 
# ggplot(PCA.coord[ind$contrib[,"Dim.1"]>100/dim(Comm)[1] & PCA.coord$Dim=="Dim.1",])+
#   geom_point(aes(x=lon,y=lat,col=Coordinates),size=3)+
#   scale_color_viridis_c()
# 
# ggplot(PCA.coord[ind$contrib[,"Dim.2"]>100/dim(Comm)[1] & PCA.coord$Dim=="Dim.2",])+
#   geom_point(aes(x=lon,y=lat,col=Coordinates),size=3)+
#   scale_color_viridis_c()
# 
# ggplot(PCA.coord[ind$contrib[,"Dim.3"]>100/dim(Comm)[1] & PCA.coord$Dim=="Dim.3",])+
#   geom_point(aes(x=lon,y=lat,col=Coordinates),size=3)+
#   scale_color_viridis_c()

#####

#PCA without some sectors#####
Comm.red <- Comm[!c(metadata$Sector.x %in% c("F","M","E","D","L")), ]
metadata.red <- metadata[!c(metadata$Sector.x %in% c("F","M","E","D","L")), ]
Comm.red.hel <- decostand(Comm.red, "hellinger")
res.pca <- prcomp(Comm.red.hel)

groups <- factor(metadata.red$Depth)
pca12_group <-fviz_pca_ind(res.pca, geom="point", 
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="PCA fish community - Group Year")
pca12_group
pca13_group <-fviz_pca_ind(res.pca, geom="point", axes = c(1, 3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="PCA fish community - Group Year")
pca13_group
pca23_group <-fviz_pca_ind(res.pca, geom="point", axes = c(2, 3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="PCA fish community - Group Year")
pca23_group


fviz_pca_biplot(res.pca, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", 
                select.var=list(contrib=10), geom.ind="point", 
                title="PCA fish community - Axes 1/2 ") +
  theme(legend.position = "none")

fviz_pca_biplot(res.pca,axes=c(1,3), repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", 
                select.var=list(contrib=10), geom.ind="point", 
                title="PCA fish community - Axes 1/3 ") +
  theme(legend.position = "none")

fviz_pca_biplot(res.pca,axes=c(5,7), repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", 
                select.var=list(contrib=10), geom.ind="point", 
                title="PCA fish community - Axes 1/3 ") +
  theme(legend.position = "none")
#####

#CA  fish occurence#####
Comm.occ <- ifelse(Comm>0,1,0)
Comm.occ <- Comm.occ[,-c(which(colSums(Comm.occ)<7))]
res.ca <- CA(Comm.occ, ncp=50)
fviz_screeplot (res.ca, addlabels = TRUE)

#fviz_ca_row(res.ca, repel = TRUE)
#fviz_ca_col (res.ca)

row <- get_ca_row(res.ca)
row
head(row$cos2, 4)

col <- get_ca_col(res.ca)
col
head(col$cos2[,c(1:3)], 4)
fviz_cos2 (res.ca, choice = "col", axes = 2)
fviz_contrib (res.ca, choice = "col", axes = 2)

fviz_contrib (res.ca, choice = "col", axes = 1:2)

fviz_ca_biplot (res.ca, geom.row=c("point"),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             select.col=list(contrib=17))

groups <- factor(metadata$Year)
fviz_ca_row(res.ca, geom="point", 
            col.row = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
            addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
            repel = TRUE, title="CA fish community - Group Year")
fviz_ca_row(res.ca, geom="point", axes = c(1, 3),
            col.row = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
            addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
            repel = TRUE, title="CA fish community - Group Year")
fviz_ca_row(res.ca, geom="point", axes = c(2, 3),
            col.row = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
            addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
            repel = TRUE, title="CA fish community - Group Year")


#####

#CA all taxa occurrence#####
AllNS <- left_join(allstrat, codif) %>% left_join(ttsel) %>% dplyr::select(Station_Code, meanLon, meanLat, Scientific_Name, Species_Density) %>% mutate(Species_Density = ifelse(is.na(Species_Density), 0, Species_Density))
AllNS <- left_join(codif, AllNS) %>% dplyr::select(Station_Code, Year, meanLon, meanLat, Scientific_Name, Species_Density) %>% unique() 


# Matrix
Comm.all <- AllNS %>% pivot_wider(names_from = "Scientific_Name",values_from="Species_Density", values_fn=mean)
metadata.all <- Comm.all[,c(1:4)]
metadata.all <- unique(dplyr::left_join(metadata.all,NSj.df[,c("Station_Code","Sector","Starting_Depth","Ending_Depth")], by="Station_Code"))
metadata.all$Depth <- rowSums(metadata.all[,c(6,7)], na.rm=T)
metadata.all$Depth[!is.na(rowSums(metadata.all[,c(6,7)]))] <- metadata.all$Depth[!is.na(rowSums(metadata.all[,c(6,7)]))]/2
metadata.all$Depth <- cut(metadata.all$Depth, breaks = c(0,5,10,15,20,25,30,35), include.lowest = T)

Comm.all <- Comm.all %>% dplyr::select(-Station_Code,-Year,-meanLon,-meanLat)

Comm.all.occ <- ifelse(Comm.all>0,1,0)
Comm.all.occ <- Comm.all.occ[,-c(which(colSums(Comm.all.occ)<7))]
res.ca <- CA(Comm.all.occ, ncp=50)
fviz_screeplot (res.ca, addlabels = TRUE)

groups <- factor(metadata.all$Year)
fviz_ca_row(res.ca, geom="point", 
            col.row = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
            addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
            repel = TRUE, title="CA NS community - Group Year")

#####

#Clustering#####
library(NbClust)

res.pca <- PCA(Comm.hel, scale.unit = F)
res.pca$ind$coord[,1] <- (-1)*res.pca$ind$coord[,1] 
#res.pca$ind$coord[,2] <- (-1)*res.pca$ind$coord[,2]
#res.pca$ind$coord[,3] <- (-1)*res.pca$ind$coord[,3]
#NbClust(res.pca$ind$coord, method =  "ward.D2")
res.hcpc <-  HCPC(res.pca, nb.clust = 4)
res.hcpc$desc.axes
res.hcpc$desc.var$quanti$`1`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`2`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`3`[1:6,c(1,2,3,6)]
res.hcpc$desc.var$quanti$`4`[1:6,c(1,2,3,6)]


Clust <- cbind(metadata, Cluster=res.hcpc$data.clust$clust)
groups <- as.character(Clust$Cluster)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))


fviz_dend(res.hcpc, 
          cex = 0.7,      
          palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                                ylab="Distance",main="")

pca12_group <-fviz_pca_ind(res.pca, geom="point", 
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="")
pca13_group <-fviz_pca_ind(res.pca, geom="point", axes = c(1, 3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="")
pca23_group <-fviz_pca_ind(res.pca, geom="point", axes = c(2, 3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="PCA fish community - Group Cluster")
#gridExtra::grid.arrange(pca12_group, pca13_group, pca23_group, ncol = 2, layout_matrix=rbind(c(1,1,2,2), c(NA, 3, 3, NA)))

pca12_group <-fviz_pca_ind(res.pca, geom="point", 
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="")+guides(col=F, pch=F, fill=F)
pca13_group <-fviz_pca_ind(res.pca, geom="point", axes = c(1, 3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="")
gridExtra::grid.arrange(pca12_group, pca13_group, nrow = 1,layout_matrix=rbind(c(1,1,1,2,2,2,2)))

pca12_group <-fviz_pca_ind(res.pca, geom="point", 
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="")+theme(legend.position="bottom")+guides(col=guide_legend(nrow=1,byrow=TRUE))
pca13_group <-fviz_pca_ind(res.pca, geom="point", axes = c(1, 3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="")+guides(col=F, pch=F, fill=F)

legend <- get_legend(pca12_group)
pca12_group <- pca12_group+theme(legend.position="none")

cowplot::plot_grid(pca12_group,var12,pca13_group,var13,ncol=2)

gridExtra::grid.arrange(var12,var13,pca12_group,pca13_group,legend
                        ,nrow=3, ncol=2,
                        layout_matrix=rbind(c(1,2),c(3,4),c(5,5)),
                        heights=c(2.5,2.5,0.2))


coast <- rgdal::readOGR("data/Coast_estuary_detailled.gpkg")
coast <- raster::crop(coast, raster::extent(-0.2,0.3,49.25,49.65))
coast.fort <- fortify(coast)
Clust1 <- Clust
groups <- as.character(Clust1$Cluster)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
Clust1$Cluster <- groups

ggplot(Clust1)+
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  geom_point(aes(x=meanLon,y=meanLat,col=Cluster))+
  scale_color_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(res.hcpc$data.clust$clust))))+
  facet_wrap(.~Year)+
  guides(col=guide_legend("Cluster"))+
  theme(axis.text.x = element_text(angle = 30, hjust=1), legend.position = "bottom")


tileplot <- cbind(metadata, Cluster=res.hcpc$data.clust$clust)
groups <- as.character(tileplot$Cluster)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
tileplot$Cluster <- groups
tileplot <- tileplot %>% group_by(Year, Sector, Cluster) %>% dplyr::mutate(Number=length(Station_Code)) %>% ungroup() 

ggplot(tileplot)+
  geom_point(aes(x=Year,y=Sector,col=Cluster, size=Number),alpha=0.7)+
  scale_color_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(tileplot$Cluster))))

tileplot <- cbind(metadata, Cluster=res.hcpc$data.clust$clust)
tileplot <- tileplot %>% group_by(Year, Cluster) %>% dplyr::mutate(Number=length(Station_Code)) %>% ungroup() 
groups <- as.character(tileplot$Cluster)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
tileplot$Cluster <- groups
ggplot(tileplot)+
  geom_point(aes(x=Year,y=Cluster,col=Cluster, size=Number),alpha=0.2)+
  scale_color_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(tileplot$Cluster))))
ggplot(tileplot, aes(x=Year))+
  geom_bar(aes(fill=Cluster),position="stack", stat="count")

tileplot2 <- tileplot %>% dplyr::group_by(Year) %>%
  dplyr::mutate(perc = Number / length(Station_Code)) %>% dplyr::select(Year, Cluster, Number, perc) %>% unique()

ggplot(tileplot2, aes(x=Year)) +
  geom_bar(aes(y=perc, fill=Cluster), stat="identity", position="stack")



tileplot$Survey <- 0
tileplot$Survey[tileplot$Year %in% c(1995:2002)] <- 1
tileplot$Survey[tileplot$Year %in% c(2008:2010)] <- 2
tileplot$Survey[tileplot$Year %in% c(2017:2019)] <- 3

ggplot(tileplot)+
  geom_line(aes(x=Year,y=Number,col=Cluster, group=interaction(Survey,Cluster)),alpha=1)+
  scale_color_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(tileplot$Cluster))))+
  theme_minimal()

#Barplot and total density
Abund.rel.com <- cbind(metadata, Cluster=res.hcpc$data.clust$clust,Comm)
Abund.rel.com <- Abund.rel.com %>% tidyr::pivot_longer(cols = 12:76,names_to="Taxa",values_to="Density") %>%
  dplyr::group_by(Year,Cluster) %>% dplyr::mutate(Group_density=sum(Density)) %>% ungroup() %>%
  dplyr::group_by(Year) %>% dplyr::mutate(Total_density=sum(Density)) %>% ungroup()
Abund.rel.com <- Abund.rel.com %>% dplyr::select(Year,Cluster,Group_density,Total_density) %>% distinct()
Abund.rel.com <- Abund.rel.com %>% dplyr::group_by(Year) %>% dplyr::mutate(Group_density=Group_density/Total_density)


groups <- as.character(Abund.rel.com$Cluster)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
Abund.rel.com$Cluster <- groups

Abund.rel.com$Survey <- 0
Abund.rel.com$Survey[Abund.rel.com$Year %in% c(1995:2002)] <- "1995-2002"
Abund.rel.com$Survey[Abund.rel.com$Year %in% c(2008:2010)] <- "2008-2010"
Abund.rel.com$Survey[Abund.rel.com$Year %in% c(2017:2019)] <- "2017-2019"
#Abund.rel.com$Group_density <- Abund.rel.com$Group_density*100
library(patchwork)
coeff <- max(Abund.rel.com$Total_density)
ggplot(Abund.rel.com, aes(x=Year))+
  geom_bar(aes(y=Group_density,fill=Cluster),position="stack", stat="identity")+
  geom_line(aes(y=Total_density/coeff, group=Survey))+ 
  scale_y_continuous(
    "Relative density", 
    sec.axis = sec_axis(~ . *coeff, name = "Total density (ind/m²)")
  )+
  scale_fill_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Abund.rel.com$Cluster))))+
  facet_grid(.~Survey,scales="free",space="free")+
  theme(strip.text.x = element_blank())

ggplot(Abund.rel.com, aes(x=Year))+
  geom_bar(aes(y=Group_density,fill=Cluster),position="stack", stat="identity")+
  geom_line(aes(y=Total_density/coeff, group=Survey))+ 
  scale_y_continuous(
    "Densité relative (%)", 
    sec.axis = sec_axis(~ . * coeff, name = "Densité totale (ind/m²)")
  )+
  scale_fill_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Abund.rel.com$Cluster))))+
  facet_grid(.~Survey,scales="free",space="free")+
  theme(strip.text.x = element_blank())+xlab("Années")


#Scatterpie map
tileplot <- cbind(metadata, Cluster=res.hcpc$data.clust$clust)
tileplot <- tileplot %>% group_by(Year, Sector, Cluster) %>% dplyr::summarise(Number=length(Station_Code)) %>% ungroup() 
coordinates <-metadata[,c("meanLon","meanLat")]
coordinates <- cbind(as.data.frame(coordinates), Sector = metadata$Sector)
centro <- aggregate(cbind(meanLon, meanLat) ~ Sector, data = coordinates, FUN = mean)
tileplot <- tileplot %>% left_join(centro, by=c("Sector"="Sector"))
tileplot <- tileplot %>% group_by(Year, meanLon, meanLat, Sector) 


library(scatterpie)
names(tileplot)[4] <- "value"
ggplot() + 
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = stratepoly.df, aes(x = long, y = lat, group = group),fill=NA, col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  geom_scatterpie(data=tileplot, aes(x=meanLon, y=meanLat, r=0.01), cols="Cluster",long_format = T)+
  facet_wrap(.~Year)+
  scale_fill_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(tileplot$Cluster))))


#Boxplot test richness
S <- vegan::specnumber(Comm)
Abund.com <- cbind(metadata, Cluster=res.hcpc$data.clust$clust,S)
Abund.com <- Abund.com %>% dplyr::select(Year,Cluster,S) %>% distinct()

groups <- as.character(Abund.com$Cluster)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
Abund.com$Cluster <- groups

Abund.com$Survey <- 0
Abund.com$Survey[Abund.com$Year %in% c(1995:2002)] <- "1995-2002"
Abund.com$Survey[Abund.com$Year %in% c(2008:2010)] <- "2008-2010"
Abund.com$Survey[Abund.com$Year %in% c(2017:2019)] <- "2017-2019"

#Boxplot index distribution
my_comparisons <- list(c("Gobiidae\nassemblage","Pelagic-like\nassemblage"),c("Gobiidae\nassemblage","Dragonet\nassemblage"),c("Pelagic-like\nassemblage","Benthic-like\nassemblage"))
Abund.com$Cluster <- sub(" ","\n",Abund.com$Cluster)
Abund.com$Cluster <- factor(Abund.com$Cluster, ordered = T, levels=c("Dragonet\nassemblage","Gobiidae\nassemblage","Pelagic-like\nassemblage","Benthic-like\nassemblage"))
ggboxplot(Abund.com,x="Cluster",y="S",
          scales="free_y")+
  stat_compare_means(aes(group=Cluster),label.y=0)+
  stat_compare_means(comparisons = my_comparisons)+
  xlab("Assemblages") + ylab("Richesse spécifique")

#####

#Changes species abundance#####
Annual_dens <- aggregate(Comm, by=list(metadata$Year), FUN="mean")
Annual_cor <- apply(as.matrix(Annual_dens[,-1]),2,cor, method="spearman",y=unique(metadata$Year))
sort(Annual_cor, decreasing=T)
sort(abs(Annual_cor), decreasing=T)[1:10]

Annual_cor <- apply(as.matrix(Annual_dens[,-1]),2,cor.test, method="spearman",y=unique(metadata$Year))
Result_corr <- data.frame(Species=colnames(Comm),
                          Rho=sapply(Annual_cor,"[[",4),
                          p.value=sapply(Annual_cor,"[[",3))
Result_corr.signi <- Result_corr[Result_corr$p.value<0.05,]

sort(Result_corr$Rho, decreasing=T)
sort(abs(Annual_cor), decreasing=T)[1:10]
######

#Trait#####

datatrait <- read.csv("data/Trait.csv", head=T, sep=";", dec=",")

#Remove G0/1/2p
sp <- strsplit(Sp_keep, "_")
sp <- lapply(sp, `[[`, 1)
sp <- unlist(sp)

#Remove spp for genus
sp[grepl("spp",sp)] <- substr(sp[grepl("spp",sp)],1,nchar(sp[grepl("spp",sp)])-4)

table(sp %in% datatrait$taxon) #4 manquants  "Blennius,Hippocampus,Liza, Scophtalmus maximus"
sp[which(c(sp %in% datatrait$taxon)==F)]
sp[8] <- "Blenniidae"
sp[54] <- "Scophthalmus maximus"
sp_t <- sp[which(c(sp %in% datatrait$taxon)==T)]

datatrait_red <- datatrait[datatrait$taxon %in% sp_t,]

datatrait_red <- unique(rbind(datatrait_red[datatrait_red$taxon=="Lepadogaster" & datatrait_red$LME==24,],
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

trait.df$tl<-                 catvar(trait.df$tl, c(2.5,3,3.5,4,5))
trait.df$AR<-                 catvar(trait.df$AR, c(1,2,3,4))
trait.df$offspring.size<-     catvar(trait.df$offspring.size, c(10,24,50,100,200,500))
trait.df$length.max<-         catvar(trait.df$length.max, c(10,30,50,70,90,110,130))
trait.df$age.max<-            catvar(trait.df$age.max, c(2,5,10,15,20,30,50))
trait.df$fecundity<-          catvar(trait.df$fecundity, c(100,10000,100000,1000000000))
trait.df$age.maturity<-       catvar(trait.df$age.maturity, c(1,2,3,4,5))
trait.df$growth.coefficient<- catvar(trait.df$growth.coefficient, c(0.5,1,1.5,2,2.5))



toto <- as.matrix(trait.df[,-1])
rownames(toto) <- trait.df[,1]
res <- MCA(toto)
mcavar12 <-fviz_mca_var(res, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())+ ggtitle("Variables ACM")
mcaind12 <-fviz_mca_ind(res, 
                        repel = TRUE, # Avoid text overlapping (slow)
                        ggtheme = theme_minimal(), labelsize=0.35) +ggtitle("Individus ACM")
cowplot::plot_grid(mcavar12,mcaind12,ncol=2)

res.hcpc <- HCPC(res, nb.clust = -1, consol=F)

library(viridis)
fviz_dend(res.hcpc, 
          cex = 0.7,      
          palette = "jco",
          rect=T, rect_fill = F,
          ylab="Distance",main="")


res.hcpc$desc.var$category
res.hcpc$desc.ind$dist

trait.df.clust <- data.frame(taxon=trait.df$taxon, res.hcpc$data.clust$clust)
trait.df.clust <- left_join(data.frame(taxon=sp_t), trait.df.clust, by="taxon")

#Hippocampus,Liza
Abund <- cbind(as.data.frame(Comm))
Abund <- aggregate(Abund,by=list(metadata$Year), FUN = sum)
Abund <- Abund[,-1]/rowSums(Abund[,-1])
Abund <- cbind(Abund, Year=unique(metadata$Year))

Abund <- tidyr::pivot_longer(Abund, 1:68, names_to="taxon", values_to="Rel.Abund")
Abund <- Abund[!Abund$taxon%in%c("Liza spp","Hippocampus spp"),]
Abund$taxon <- rep(sp_t$taxon, times=length(unique(metadata$Year)))
Abund <- left_join(Abund, trait.df.clust, by="taxon")
names(Abund)[4] <- "Cluster"


groups <- as.character(Abund$Cluster)
groups[groups==0] <- "Missing data species"
groups[groups==1] <- "Guarder species"
groups[groups==2] <- "Pelagic planktivorous"
groups[groups==3] <- "Demersal bethivorous"
groups[groups==4] <- "Benthopelagic generalist"
groups[groups==5] <- "Sharks"
Abund$Cluster <- factor(groups)

ggplot(Abund, aes(fill=Cluster, y=Rel.Abund, x=Year)) + 
  geom_bar(position="stack", stat="identity")+ylab("Density")+
  scale_fill_viridis_d()+theme_minimal()


Clust.trait <- as.numeric(trait.df.clust$res.hcpc.data.clust.clust)
Clust.trait[is.na(Clust.trait)] <- 0


Clust.comp <- Comm[,!names(Comm)%in%c("Liza spp","Hippocampus spp")]
Clust.comp <- data.frame(Station=Clust$Station_Code,Year=Clust$Year,Clust.comp=Clust$Cluster,Clust.comp)
Clust.comp <- Clust.comp %>% pivot_longer(cols=4:69,names_to="Clust.trait", values_to="Abund")
Clust.comp$Clust.trait <- rep(Clust.trait, times=dim(Clust.comp)[1]/length(Clust.trait))

ggplot(Clust.comp, aes(fill=as.factor(Clust.trait), y=Abund, x=Clust.comp)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Clust.comp$Clust.trait))))

Clust.comp.rel <- Clust.comp
names(Clust.comp.rel)[3] <- "Assemblage"
Clust.comp.rel <- Clust.comp.rel %>% dplyr::group_by(Year,Clust.trait) %>%
  dplyr::mutate(Abund_group=sum(Abund)) %>% ungroup() %>% dplyr::group_by(Year) %>%
  dplyr::mutate(Abund_rel=Abund_group/sum(Abund)) %>% ungroup() %>% dplyr::select(-Station,-Abund) %>% distinct()

groups <- as.character(Clust.comp.rel$Assemblage)
groups[groups==1] <- "Benthic-like assemblage"
groups[groups==2] <- "Pelagic-like assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
Clust.comp.rel$Assemblage <- groups
groups <- as.character(Clust.comp.rel$Clust.trait)
groups[groups==0] <- "Missing data species"
groups[groups==1] <- "Guarder species"
groups[groups==2] <- "Pelagic planktivorous"
groups[groups==3] <- "Demersal bethivorous"
groups[groups==4] <- "Benthopelagic generalist"
groups[groups==5] <- "Sharks"
Clust.comp.rel$Clust.trait <- factor(groups)
Clust.comp.rel$Survey <- 0
Clust.comp.rel$Survey[Clust.comp.rel$Year %in% c(1995:2002)] <- "1995-2002"
Clust.comp.rel$Survey[Clust.comp.rel$Year %in% c(2008:2010)] <- "2008-2010"
Clust.comp.rel$Survey[Clust.comp.rel$Year %in% c(2017:2019)] <- "2017-2019"


ggplot(distinct(Clust.comp.rel[,-2]), aes(fill=Clust.trait, y=Abund_rel, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_manual(values=get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Clust.comp$Clust.trait))))+
  scale_fill_viridis_d()+
  labs(fill="Functional groups")+xlab("Years")+ylab("Relative abundance")+
  facet_grid(.~Survey,scales="free",space="free")+
  theme(strip.text.x = element_blank())

ggplot(distinct(Clust.comp.rel[,-2]), aes(fill=Clust.trait, y=Abund_rel, x=Year)) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_manual(values=get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Clust.comp$Clust.trait))))+
  scale_fill_viridis_d()+
  labs(fill="Groupes fonctionnels")+xlab("Années")+ylab("Abondance relative")+
  facet_grid(.~Survey,scales="free",space="free")+
  theme(strip.text.x = element_blank())



Clust.trait.var <- Clust.comp %>% dplyr::group_by(Year,Clust.trait) %>% dplyr::summarise(Tot=sum(Abund))
Clust.trait.var$Survey <- 0
Clust.trait.var$Survey[Clust.trait.var$Year %in% c(1995:2002)] <- 1
Clust.trait.var$Survey[Clust.trait.var$Year %in% c(2008:2010)] <- 2
Clust.trait.var$Survey[Clust.trait.var$Year %in% c(2017:2019)] <- 3

groups <- as.character(Clust.trait.var$Clust.trait)
groups[groups==0] <- "Missing data species"
groups[groups==1] <- "Guarder species"
groups[groups==2] <- "Pelagic planktivorous"
groups[groups==3] <- "Demersal bethivorous"
groups[groups==4] <- "Benthopelagic generalist"
groups[groups==5] <- "Sharks"
Clust.trait.var$Clust.trait <- factor(groups)

ggplot(Clust.trait.var) + 
  geom_line(aes(x=Year,y=Tot, group=interaction(Survey,Clust.trait),col=Clust.trait),lwd=1.1)+
  #scale_fill_manual(values=get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Clust.comp$Clust.trait))))
  scale_color_viridis_d()+theme_minimal()+ylab("Total density")+labs(col="Functional group")





Clust.comp.rel <- Clust.comp
Clust.comp.rel <- Clust.comp.rel %>% dplyr::group_by(Clust.comp, Clust.trait) %>% dplyr::summarise(Abund=sum(Abund))
Clust.comp.rel <- Clust.comp.rel %>% dplyr::group_by(Clust.comp) %>% dplyr::mutate(Abund= Abund/sum(Abund))

groups <- as.character(Clust.comp.rel$Clust.comp)
groups[groups==1] <- "Benthic assemblage"
groups[groups==2] <- "Pelagic assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"
groups <- factor(groups, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic assemblage","Benthic assemblage"))
Clust.comp.rel$Clust.comp <- groups

groups <- as.character(Clust.comp.rel$Clust.trait)
groups[groups==0] <- "Missing data species"
groups[groups==1] <- "Guarder species"
groups[groups==2] <- "Pelagic planktivorous"
groups[groups==3] <- "Demersal bethivorous"
groups[groups==4] <- "Benthopelagic generalist"
groups[groups==5] <- "Sharks"
#groups[groups==6] <- "Sharks"
groups <- factor(groups)
Clust.comp.rel$Clust.trait <- groups

ggplot(Clust.comp.rel, aes(fill=Clust.trait, y=Abund, x=Clust.comp)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(Clust.comp$Clust.trait))))+
  labs(fill="Functional groups")+xlab("Assemblage")+ylab("Relative abundance")+
  scale_fill_viridis_d()


Clust.comp.rel%>% group_by(Clust.comp,Clust.trait) %>% dplyr::mutate(Abund=sum(Abund))%>%
  ungroup()%>% dplyr::group_by(Clust.comp) %>% dplyr::mutate(Abund=Abund/sum(Abund)*100)

tileplot <- left_join(metadata, Clust.comp[Clust.comp$Abund>0,], by=c("Station_Code"="Station","Year"="Year"))
tileplot <- tileplot %>% unique() %>% group_by(Year, Sector, Clust.trait) %>% dplyr::summarise(Number=length(Station_Code)) %>% ungroup() 
coordinates <-metadata[,c("meanLon","meanLat")]
coordinates <- cbind(as.data.frame(coordinates), Sector = metadata$Sector)
centro <- aggregate(cbind(meanLon, meanLat) ~ Sector, data = coordinates, FUN = mean)
tileplot <- tileplot %>% left_join(centro, by=c("Sector"="Sector"))
tileplot <- tileplot %>% group_by(Year, meanLon, meanLat, Sector) 

library(scatterpie)
names(tileplot)[4] <- "value"
tileplot$Clust.trait <- as.factor(tileplot$Clust.trait)
ggplot() + 
  geom_polygon(data = coast.fort, aes(x = long, y = lat, group = group),fill="grey", col="black")+
  geom_polygon(data = stratepoly.df, aes(x = long, y = lat, group = group),fill=NA, col="black")+
  theme_minimal() + xlab("Longitude") + ylab("Latitude") + 
  geom_scatterpie(data=tileplot, aes(x=meanLon, y=meanLat, r=0.01), cols="Clust.trait",long_format = T)+
  facet_wrap(.~Year)+
  scale_fill_manual(values = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(tileplot$Clust.trait))))


Abund <- cbind(metadata, as.data.frame(Comm))
Abund <- tidyr::pivot_longer(Abund, 9:76, names_to="Scientific_Name", values_to="Density")
Abund <- Abund %>% dplyr::group_by(Year, Scientific_Name) %>% dplyr::summarise(Density=mean(Density))
Abund <- Abund[!Abund$Scientific_Name%in%c("Liza spp","Hippocampus spp"),]
Abund$taxon <- rep(sp_t$taxon, times=length(unique(metadata$Year)))
Abund <- left_join(Abund, trait.df.clust, by="taxon")
names(Abund)[5] <- "Cluster"


groups <- as.character(Abund$Cluster)
groups[groups==0] <- "Missing data species"
groups[groups==1] <- "Guarder species"
groups[groups==2] <- "Pelagic planktivorous"
groups[groups==3] <- "Demersal bethivorous"
groups[groups==4] <- "Benthopelagic generalist"
groups[groups==5] <- "Sharks"
Abund$Cluster <- factor(groups)



Abund.Y2 <- Abund %>% dplyr::group_by(Year,Cluster) %>% dplyr::summarise(Density=mean(Density))

ggplot(Abund.Y2)+
  geom_bar(aes(x=Year,y=Density,fill=Cluster),position = "dodge", stat = "identity")+facet_wrap(.~Cluster, scales="free_y")






#Community weighted values#####
Abund <- cbind(as.data.frame(Comm))
Abund <- aggregate(Abund,by=list(metadata$Year), FUN = sum)
names(Abund)[1] <- "Years"

Abund <- tidyr::pivot_longer(Abund, 2:69, names_to="taxon", values_to="Abund")
Abund <- Abund[!Abund$taxon%in%c("Liza spp","Hippocampus spp"),]
Abund$taxon <- rep(sp_t$taxon, times=length(unique(metadata$Year)))
Abund <- left_join(Abund, trait.df.clust, by="taxon")
names(Abund)[4] <- "Cluster"


groups <- as.character(Abund$Cluster)
groups[groups==0] <- "Missing data species"
groups[groups==1] <- "Guarder species"
groups[groups==2] <- "Pelagic planktivorous"
groups[groups==3] <- "Demersal bethivorous"
groups[groups==4] <- "Benthopelagic generalist"
groups[groups==5] <- "Sharks"
Abund$Cluster <- factor(groups)

#Density per trait per year
Abund.trait <- Abund %>% dplyr::left_join(trait.df, by="taxon")
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,habitat) %>% dplyr::mutate(Habitat_CWM=sum(Abund)) %>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,feeding.mode) %>% dplyr::mutate(Feeding_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,body.shape) %>% dplyr::mutate(Body_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,fin.shape) %>% dplyr::mutate(Fin_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,AR) %>% dplyr::mutate(AR_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,offspring.size) %>% dplyr::mutate(Offspring_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,spawning.type) %>% dplyr::mutate(Spawning_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,age.maturity) %>% dplyr::mutate(Maturity_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,length.max) %>% dplyr::mutate(Length_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,age.max) %>% dplyr::mutate(Age_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,growth.coefficient) %>% dplyr::mutate(Growth_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,tl) %>% dplyr::mutate(Trophic_CWM=sum(Abund))%>% ungroup()
Abund.trait <- Abund.trait %>%dplyr::group_by(Years,fecundity) %>% dplyr::mutate(Fecundity_CWM=sum(Abund))%>% ungroup()


#Relative density: CWM
Abund.trait <- Abund.trait %>%dplyr::group_by(Years) %>% dplyr::mutate(Habitat_CWM=Habitat_CWM/sum(Abund)) %>%
  dplyr::mutate(Feeding_CWM=Feeding_CWM/sum(Abund))%>% 
  dplyr::mutate(Body_CWM=Body_CWM/sum(Abund)) %>%
  dplyr::mutate(Fin_CWM=Fin_CWM/sum(Abund))%>%
  dplyr::mutate(AR_CWM=AR_CWM/sum(Abund))%>%
  dplyr::mutate(Offspring_CWM=Offspring_CWM/sum(Abund))%>% 
  dplyr::mutate(Spawning_CWM=Spawning_CWM/sum(Abund))%>%
  dplyr::mutate(Maturity_CWM=Maturity_CWM/sum(Abund))%>%
  dplyr::mutate(Length_CWM=Length_CWM/sum(Abund))%>% 
  dplyr::mutate(Age_CWM=Age_CWM/sum(Abund))%>%
  dplyr::mutate(Growth_CWM=Growth_CWM/sum(Abund))%>% 
  dplyr::mutate(Trophic_CWM=Trophic_CWM/sum(Abund))%>% 
  dplyr::mutate(Fecundity_CWM=Fecundity_CWM/sum(Abund))%>% ungroup()

Abund.trait.red <- Abund.trait %>% dplyr::select(-taxon,-Abund) %>% distinct()
#Abund.trait.red <- Abund.trait.red %>% tidyr::pivot_longer(cols=3:15, names_to="Traits",values_to="Modality")
Abund.trait.red <- cbind(Survey=0,Abund.trait.red)
Abund.trait.red$Survey[Abund.trait.red$Years %in% c(1995:2002)] <- "1995-2002"
Abund.trait.red$Survey[Abund.trait.red$Years %in% c(2008:2010)] <- "2008-2010"
Abund.trait.red$Survey[Abund.trait.red$Years %in% c(2017:2019)] <- "2017-2019"
Abund.trait.red <- Abund.trait.red[,-3]
Hab.CWM <- distinct(Abund.trait.red[,c(1,2,3,16)]) 
Feed.CWM <- distinct(Abund.trait.red[,c(1,2,4,17)])
Body.CWM <- distinct(Abund.trait.red[,c(1,2,5,18)])
Fin.CWM <- distinct(Abund.trait.red[,c(1,2,6,19)])
AR.CWM <- distinct(Abund.trait.red[,c(1,2,7,20)])
Off.CWM <- distinct(Abund.trait.red[,c(1,2,8,21)])
Spawn.CWM <- distinct(Abund.trait.red[,c(1,2,9,22)])
Mat.CWM <- distinct(Abund.trait.red[,c(1,2,10,23)])
Leng.CWM <- distinct(Abund.trait.red[,c(1,2,11,24)])
Age.CWM <- distinct(Abund.trait.red[,c(1,2,12,25)])
Gro.CWM <- distinct(Abund.trait.red[,c(1,2,13,26)])
Tro.CWM <- distinct(Abund.trait.red[,c(1,2,14,27)])
Fec.CWM <- distinct(Abund.trait.red[,c(1,2,15,28)])


Hab.plot <- ggplot(Hab.CWM)+
  geom_bar(aes(x=Years,y=Habitat_CWM,fill=habitat), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")
Tro.plot <- ggplot(Tro.CWM)+
  geom_bar(aes(x=Years,y=Trophic_CWM,fill=tl), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")
Leng.plot <- ggplot(Leng.CWM)+
  geom_bar(aes(x=Years,y=Length_CWM,fill=length.max), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")
Mat.plot <- ggplot(Mat.CWM)+
  geom_bar(aes(x=Years,y=Maturity_CWM,fill=age.maturity), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")
Body.plot <- ggplot(Body.CWM)+
  geom_bar(aes(x=Years,y=Body_CWM,fill=body.shape), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")
Age.plot <- ggplot(Age.CWM)+
  geom_bar(aes(x=Years,y=Age_CWM,fill=age.max), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")
Feed.plot <- ggplot(Feed.CWM)+
  geom_bar(aes(x=Years,y=Feeding_CWM,fill=feeding.mode), position="stack",stat="identity")+
  facet_grid(.~Survey,space="free",scales = "free")


#Correlation trait/years
names(Hab.CWM) <- c("Survey","Years","Trait","CWM")
names(Feed.CWM) <-c("Survey","Years","Trait","CWM")
names(Body.CWM)<- c("Survey","Years","Trait","CWM")
names(Fin.CWM) <- c("Survey","Years","Trait","CWM")
names(AR.CWM) <- c("Survey","Years","Trait","CWM")
AR.CWM$Trait <- paste0("AR_",AR.CWM$Trait)
names(Off.CWM) <- c("Survey","Years","Trait","CWM")
Off.CWM$Trait <- paste0("Off_",Off.CWM$Trait)
names(Spawn.CWM) <- c("Survey","Years","Trait","CWM")
Spawn.CWM$Trait <- paste0("Spa_",Spawn.CWM$Trait)
names(Mat.CWM) <- c("Survey","Years","Trait","CWM")
Mat.CWM$Trait <- paste0("Mat_",Mat.CWM$Trait)
names(Leng.CWM) <- c("Survey","Years","Trait","CWM")
Leng.CWM$Trait <- paste0("Len_",Leng.CWM$Trait)
names(Age.CWM) <- c("Survey","Years","Trait","CWM")
Age.CWM$Trait <- paste0("Age_",Age.CWM$Trait)
names(Gro.CWM) <- c("Survey","Years","Trait","CWM")
Gro.CWM$Trait <- paste0("Gro_",Gro.CWM$Trait)
names(Tro.CWM) <- c("Survey","Years","Trait","CWM")
Tro.CWM$Trait <- paste0("Tro_",Tro.CWM$Trait)
names(Fec.CWM) <- c("Survey","Years","Trait","CWM")
Fec.CWM$Trait <- paste0("Fec_",Fec.CWM$Trait)
TraitY <- rbind(Hab.CWM,Feed.CWM,Body.CWM,Fin.CWM,AR.CWM,
                Off.CWM,Spawn.CWM,Mat.CWM,Leng.CWM,Age.CWM,
                Gro.CWM,Tro.CWM,Fec.CWM)
TraitY <- TraitY %>% na.omit() %>% tidyr::pivot_wider(id_cols = c(1:2),names_from="Trait",values_from="CWM")
Annual_cor_trait <- apply(as.matrix(TraitY[,-c(1,2)]),2,cor, method="spearman",y=TraitY$Years)
sort(Annual_cor_trait, decreasing=T)
sort(abs(Annual_cor_trait), decreasing=T)[1:10]

Annual_cor_trait <- apply(as.matrix(TraitY[,-c(1:2)]),2,cor.test, method="spearman",y=TraitY$Years)
Result_cor_trait <- data.frame(Trait=names(TraitY)[3:66],
                          Rho=sapply(Annual_cor_trait,"[[",4),
                          p.value=sapply(Annual_cor_trait,"[[",3))
Result_cor_trait.signi <- Result_cor_trait[Result_cor_trait$p.value<0.05,]

sort(Result_cor_trait$Rho, decreasing=T)
sort(abs(Annual_cor_trait), decreasing=T)[1:10]
#####

#Changes in estuaries sectors: tests#####

Comm.red <- Comm[c(metadata$Sector %in% c("F","M")), ]
metadata.red <- metadata[c(metadata$Sector %in% c("F","M")), ]
data_estuary <- cbind(metadata.red,Comm.red)

mean_Comm <- aggregate(data_estuary[,9:84],  by=data_estuary[,c(2,5)], FUN = sum)

rownames(mean_Comm) <- paste(mean_Comm$Year,mean_Comm$Sector, sep="_")
mean_Comm <- mean_Comm[,-c(1:2)]
Comm.red.hel <- decostand(mean_Comm, "hellinger")

plot(hclust(dist(Comm.red.hel),method="ward.D2"))
res.pca <- PCA(Comm.red.hel, scale.unit = F)
fviz_pca_biplot(res.pca,geom="text",title="PCA fish community - 1/2")
res.hcpc <-  HCPC(res.pca, nb.clust = -1)

groups <- substr(rownames(Comm.red.hel),6,6)
pca12_group <-fviz_pca_ind(res.pca, geom="point", 
                         col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                         addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                         repel = TRUE, title="PCA fish community - Group Sector")
pca13_group <-fviz_pca_ind(res.pca, geom="point", axes=c(1,3),
                           col.ind = groups,  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), length(unique(groups))),
                           addEllipses = T, ellipse.type = "confidence", legend.title = "Groups",
                           repel = TRUE, title="PCA fish community - Group Sector")

res.simp <- simper(Comm.red.hel,substr(rownames(Comm.red.hel),1,4))
summary(res.simp)
diss.list <- lapply(res.simp, FUN=function(x){x$overall})
D <- unlist(diss.list)

S <- diag(13)
S[upper.tri(S, diag=TRUE)] <- D
S <- t(S)


colnames(S) <-sort(unique(metadata.red$Year))[1:13]
rownames(S) <-sort(unique(metadata.red$Year))[2:14]
library(ggcorrplot)

library(viridis)

#Correlation matrix
#Dab


#Combining GIC/LIC index in matrix plot
ggcorrplot(S,
           method="square",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           show.diag = F,
           lab=T,
           type="lower")+
  #scale_fill_gradientn(colours=rev(col2(200)),limit=c(0, 1))+
  scale_fill_viridis()+
  xlab("distance")+ggtitle("D") + labs(fill="Index value")+
  guides(fill=F)


#Specnumber in each assemblage

mean(specnumber(Comm[res.hcpc$data.clust$clust==1,]))
mean(specnumber(Comm[res.hcpc$data.clust$clust==2,]))
mean(specnumber(Comm[res.hcpc$data.clust$clust==3,]))
mean(specnumber(Comm[res.hcpc$data.clust$clust==4,]))

groups[groups==1] <- "Benthic assemblage"
groups[groups==2] <- "Pelagic assemblage"
groups[groups==3] <- "Gobiidae assemblage"
groups[groups==4] <- "Dragonet assemblage"

richness <- data.frame(S=c(specnumber(Comm[res.hcpc$data.clust$clust==1,]),
                           specnumber(Comm[res.hcpc$data.clust$clust==2,]),
                           specnumber(Comm[res.hcpc$data.clust$clust==3,]),
                           specnumber(Comm[res.hcpc$data.clust$clust==4,])),
                       Comm=c(rep(c(1,2,3,4), times=c(length(specnumber(Comm[res.hcpc$data.clust$clust==1,])),
                                                   length(specnumber(Comm[res.hcpc$data.clust$clust==2,])),
                                                   length(specnumber(Comm[res.hcpc$data.clust$clust==3,])),
                                                   length(specnumber(Comm[res.hcpc$data.clust$clust==4,]))))))

kruskal.test(S~Comm,data=richness)
wilcox.test(specnumber(Comm[res.hcpc$data.clust$clust==3,]),
            specnumber(Comm[res.hcpc$data.clust$clust==2,]))


#Community weighted values


#####


#Environmental changes#####
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
Tabsst<- pivot_longer(Tabsst, cols=1:(dim(Tabsst)[2]-2), names_to = "Date", values_to = "SST", values_drop_na = TRUE)

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
Tabchl<- pivot_longer(Tabchl, cols=1:(dim(Tabchl)[2]-2), names_to = "Date", values_to = "Chloro", values_drop_na = TRUE)

Tabchl$Date<- sub("values.X","",Tabchl$Date)
Tabchl$Year<- as.numeric(substr(as.character(Tabchl$Date),1,4))
Tabchl$Month<- as.numeric(substr(as.character(Tabchl$Date), 6,7))
Tabchl$Day<- as.numeric(substr(as.character(Tabchl$Date), 9,10))


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
NAO <- NAO[NAO$Years>1990,]


#####

# Preparation of the dataset & correlation#####
names(Tabsst2)[4] <- "Values"
Tabsst2$Parameter <- "SST"
names(TabTurb2)[4] <- "Values"
TabTurb2$Parameter <- "Turbidity"
names(Tabchl2)[4] <- "Values"
Tabchl2$Parameter <- "Chlorophyll a"


Env <- rbind(Tabsst2,TabTurb2,Tabchl2)


Env.Y<- Env %>% dplyr::group_by(Year, Parameter) %>% dplyr::summarise(Values= mean(Values)) %>% ungroup()
TabFlow2 <- TabFlow %>% dplyr::group_by(Year) %>% dplyr::summarise(Values= mean(Values,na.rm=T))
TabFlow2$Parameter<- "Flow"
TabFlow2 <- TabFlow2[,c(1,3,2)]
TabNAO <- NAO %>% dplyr::group_by(Years) %>% dplyr::summarise(Values= mean(Index,na.rm=T))
TabNAO$Parameter<- "NAO"
TabNAO <- TabNAO[,c(1,3,2)]
names(TabNAO)[1] <- "Year"

Env.Y <- rbind(Env.Y, TabFlow2, TabNAO)


#Anom
Env.anomY<- Env.Y %>% dplyr::group_by(Year, Parameter) %>% dplyr::summarise(Values= mean(Values)) %>%
  dplyr::group_by(Parameter) %>% dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
Env.anomY$absolute <- ifelse(Env.anomY$Values<0,1,2)

ggplot(Env.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_wrap(Parameter~., scales=c("free"))+
  theme_minimal()+
  guides(fill=F)



#BioENV
Env.Y <- Env.Y[Env.Y$Year %in% NSyears,]

Comm.an <- aggregate(Comm, by=list(metadata$Year), FUN="sum")
rownames(Comm.an) <-Comm.an$Group.1 ; Comm.an <- Comm.an[,-1]

Env.Ywide <- Env.Y %>% tidyr::pivot_wider(id_cols=c(1),names_from ="Parameter",values_from="Values")
Env.Ywide.lag1 <- Env.Ywide
names(Env.Ywide.lag1) <- paste0(names(Env.Ywide.lag1),"_1")
Env.Ywide.lag1 <- Env.Ywide.lag1[-dim(Env.Ywide.lag1)[1],]
Env.Ywide.lag1 <- rbind(NA,Env.Ywide.lag1)
Env.Ywide <- cbind(Env.Ywide,Env.Ywide.lag1[,c(2,6)])

Env.Ywide <- na.omit(Env.Ywide)
Comm.an <- Comm.an[-c(1:3),]
Comm.an.hell <- decostand(Comm.an, "hellinger")

corenv <-  bioenv(Comm.an.hell,Env.Ywide[,-1], index="euclidean", metric="euclidean")
summary(corenv)

cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,1])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,2])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,3])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,4])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,5])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,6])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,7])), method="spearman")
cor.test(dist(Comm.an.hell),dist(scale(Env.Ywide[,8])), method="spearman")


#####


#Functionnal diversity#
library(FD)


#Data formatting for NOURSEINE without juveniles#####
Nourseine <- read.csv("data/NOURSEINE_dataset_1995-2019.csv", head=T)

##Create single points coordinates for each station
NS.df <- Nourseine %>% mutate(meanLat=(Starting_Latitude_decimal+Ending_Latitude_decimal)/2,
                              meanLon=(Starting_Longitude_decimal+Ending_Longitude_decimal)/2) %>%
  dplyr::select(-Starting_Latitude_decimal,-Starting_Longitude_decimal,-Ending_Latitude_decimal,-Ending_Longitude_decimal)


##Adding rows with 0 densities
ttsel <- NS.df %>% dplyr::select(Station_Code, Scientific_Name, Year, meanLon, meanLat, Species_Density) %>% distinct() 
codif <- ttsel %>% dplyr::select(Station_Code, Year, meanLon, meanLat) %>% distinct() 
allstrat <- ttsel %>% expand(Station_Code, Scientific_Name) 
NS.df0 <- dplyr::left_join(allstrat, codif) %>% dplyr::left_join(ttsel) %>% dplyr::select(Station_Code, meanLon, meanLat, Scientific_Name, Species_Density) %>% mutate(Species_Density = ifelse(is.na(Species_Density), 0, Species_Density))
NS.df0 <- dplyr::left_join(codif, NS.df0) %>% dplyr::select(Station_Code, Year, meanLon, meanLat, Scientific_Name, Species_Density) %>% unique() 

NS.df0$Scientific_Name <- as.character(NS.df0$Scientific_Name)

Classif <- worms::wormsbynames(unique(NS.df0$Scientific_Name),match = T)
Sp_keep <- Classif$name[Classif$class %in% c("Actinopteri","Elasmobranchii","Bacillariophyceae")] #"Bacillariophyceae" for flounder
NS.df0 <- NS.df0[NS.df0$Scientific_Name %in% Sp_keep,]

NS.df0 <- aggregate(Species_Density ~ Station_Code+meanLon+meanLat+Year+Scientific_Name, data=NS.df0, sum)

# Matrix
Comm <- NS.df0 %>% pivot_wider(names_from = "Scientific_Name",values_from="Species_Density", values_fn=mean)
metadata <- Comm[,c(1:4)]
metadata <- unique(dplyr::left_join(metadata,NS.df[,c("Station_Code","Sector","Starting_Depth","Ending_Depth")], by="Station_Code"))
metadata$Depth <- rowSums(metadata[,c(6,7)], na.rm=T)
metadata$Depth[!is.na(rowSums(metadata[,c(6,7)]))] <- metadata$Depth[!is.na(rowSums(metadata[,c(6,7)]))]/2
metadata$Depth <- cut(metadata$Depth, breaks = c(0,5,10,15,20,25,30,35), include.lowest = T)
Comm <- Comm %>% dplyr::select(-Station_Code,-Year,-meanLon,-meanLat)
#####
#Functional indices #####
datatrait <- read.csv("data/Trait.csv", head=T, sep=";", dec=",")

#Remove G0/1/2p
sp <- strsplit(Sp_keep, "_")
sp <- lapply(sp, `[[`, 1)
sp <- unlist(sp)

#Remove spp for genus
sp[grepl("spp",sp)] <- substr(sp[grepl("spp",sp)],1,nchar(sp[grepl("spp",sp)])-4)

table(sp %in% datatrait$taxon) #4 manquants  "Blennius,Hippocampus,Liza, Scophtalmus maximus"
sp[which(c(sp %in% datatrait$taxon)==F)]
sp[8] <- "Blenniidae"
sp[60] <- "Scophthalmus maximus"
sp_t <- sp[which(c(sp %in% datatrait$taxon)==T)]

datatrait_red <- datatrait[datatrait$taxon %in% sp_t,]

datatrait_red <- unique(rbind(datatrait_red[datatrait_red$taxon=="Lepadogaster" & datatrait_red$LME==24,],
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

trait.df$tl<-                 catvar(trait.df$tl, c(2.5,3,3.5,4,5))
trait.df$AR<-                 catvar(trait.df$AR, c(1,2,3,4))
trait.df$offspring.size<-     catvar(trait.df$offspring.size, c(10,24,50,100,200,500))
trait.df$length.max<-         catvar(trait.df$length.max, c(10,30,50,70,90,110,130))
trait.df$age.max<-            catvar(trait.df$age.max, c(2,5,10,15,20,30,50))
trait.df$fecundity<-          catvar(trait.df$fecundity, c(100,10000,100000,1000000000))
trait.df$age.maturity<-       catvar(trait.df$age.maturity, c(1,2,3,4,5))
trait.df$growth.coefficient<- catvar(trait.df$growth.coefficient, c(0.5,1,1.5,2,2.5))


#####


#Diversity index#####

trait.dffactor<-trait.df[order(trait.df$taxon),]
row.names(trait.dffactor) <- trait.dffactor$taxon
trait.dffactor <- trait.dffactor[,-1]

abund.trait  <- cbind(Comm[,c(names(Comm)%in%trait.df$taxon)],Comm[,c("Ammodytes spp","Arnoglossus spp","Blennius spp","Scophtalmus maximus","Syngnathus spp")])
names(abund.trait)[40:44] <- c("Ammodytes","Arnoglossus","Blenniidae","Scophthalmus maximus","Syngnathus")
abund.trait <-abund.trait[,order(colnames(abund.trait))]
abund.trait <- as.matrix(abund.trait)
row.names(abund.trait) <- metadata$Station_Code

#CWM dominant
abund.trait.an.assemblage <- aggregate(x=abund.trait,by=list(Clust$Year,Clust$Cluster),FUN="sum")
row.names(abund.trait.an) <- unique(metadata$Year)
CMW.dom <- dbFD(as.matrix(trait.dffactor), abund.trait.an.assemblage[,-c(1:2)], CWM.type="dom")
View(CMW.dom$CWM)
View(cbind(abund.trait.an.assemblage[,c(1:2)],CMW.dom$CWM))


#https://github.com/ThibaultCariou/Seine_fishcom
#https://github.com/ThibaultCariou/Seine_fishcom.git

#FD index
FDiv <- dbFD(as.matrix(trait.dffactor), abund.trait, CWM.type="dom")
View(FDiv$CWM)
Div <- data.frame(metadata,FRic=FDiv$FRic,FEve=FDiv$FEve,FDis=FDiv$FDis,Rao=FDiv$RaoQ,Simpson=diversity(abund.trait, index = "simpson"))
Div$Redun <- 1-(Div$Rao/Div$Simpson)
toto <- Clust[,c(1,2,9)]
# toto$Year <- factor(toto$Year); toto$Station_Code <- factor(toto$Station_Code) ; toto$Cluster <- factor(toto$Cluster)
# Div$Year <- factor(Div$Year); Div$Station_Code <- factor(Div$Station_Code)
Div <- Div %>% dplyr::left_join(toto, by=c("Year"="Year","Station_Code"="Station_Code"))
names(Div)[15] <- "Assemblage"

Div$Assemblage <- as.character(Div$Assemblage)
Div$Assemblage[Div$Assemblage==1] <- "Pelagic-like assemblage"
Div$Assemblage[Div$Assemblage==2] <- "Benthic-like assemblage"
Div$Assemblage[Div$Assemblage==3] <- "Gobiidae assemblage"
Div$Assemblage[Div$Assemblage==4] <- "Dragonet assemblage"
Div$Assemblage <- factor(Div$Assemblage, ordered = T, levels = c("Dragonet assemblage","Gobiidae assemblage","Pelagic-like assemblage","Benthic-like assemblage"))
Div.long <- Div %>% tidyr::pivot_longer(9:14, names_to="Index",values_to="Values")


toto <- aggregate(Values~Index+Sector+Year, data=Div.long, mean)

res.wilcox <- data.frame(toto, signi=c("c","b","b","a","a","a","b","b","b","c","b","b") )
#Histo first version
ggplot(toto)+
  geom_bar(aes(x=Year,y=Values,fill=Index,),stat="identity")+
  #geom_text(aes(x=Assemblage, y=Values, label=signi),data=res.wilcox, position = position_nudge(y=0.05))+
  facet_grid(Index~Sector, scales="free_y")+
  guides(fill=F)+
  scale_fill_viridis_d()+
  theme_minimal()

wilcox.test(Div.long$Values[Div.long$Index=="FEve"& Div.long$Assemblage=="Gobiidae assemblage"],Div.long$Values[Div.long$Index=="FEve"& Div.long$Assemblage=="Benthic-like assemblage"])

#Div.long <- Div %>% tidyr::pivot_longer(9:11, names_to="Index",values_to="Values")
Div.long$Survey <- 0
Div.long$Survey[Div.long$Year %in% c(1995:2002)] <- 1
Div.long$Survey[Div.long$Year %in% c(2008:2010)] <- 2
Div.long$Survey[Div.long$Year %in% c(2017:2019)] <- 3
toto <- aggregate(Values~Index+Assemblage+Year, data=Div.long, mean)

res.wilcox <- data.frame(toto, signi=c("c","b","b","a","a","a","b","b","b","c","b","b") )


ggplot(toto)+
  geom_bar(aes(x=Year,y=Values,fill=Index,),stat="identity")+
  #geom_text(aes(x=Assemblage, y=Values, label=signi),data=res.wilcox, position = position_nudge(y=0.05))+
  facet_grid(Index~Assemblage, scales="free_y")+
  guides(fill=F)+
  scale_fill_viridis_d()+
  theme_minimal()

#Boxplot index distribution
my_comparisons <- list(c("Gobiidae\nassemblage","Pelagic-like\nassemblage"),c("Gobiidae\nassemblage","Dragonet\nassemblage"),c("Pelagic-like\nassemblage","Benthic-like\nassemblage"))
Div.long$Assemblage <- sub(" ","\n",Div.long$Assemblage)
Div.long$Assemblage <- factor(Div.long$Assemblage, ordered = T, levels=c("Dragonet\nassemblage","Gobiidae\nassemblage","Pelagic-like\nassemblage","Benthic-like\nassemblage"))
Div.long <- Div.long[!is.na(Div.long$Values),]
ggboxplot(Div.long[!c(Div.long$Index %in% c("Rao","Simpson")),],x="Assemblage",y="Values",
          facet.by="Index", scales="free_y")+
  stat_compare_means(aes(group=Assemblage),label.y.npc = c("left","bottom"))+
  stat_compare_means(comparisons = my_comparisons)
  #stat_compare_means()
#####

#Anomalies FD indexes
#Annual anomalies

Div.anomY<- Div.long %>% dplyr::group_by(Year, Index) %>% dplyr::summarise(Values= mean(Values, na.rm=T)) %>%
  dplyr::group_by(Index) %>% dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
Div.anomY$absolute <- ifelse(Div.anomY$Values<0,1,2)

ggplot(Div.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Functional indices anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_wrap(Index~., scales="free_y")+
  theme_minimal()+
  guides(fill=F)

Div.anomC<- Div.long %>% dplyr::group_by(Year, Index,Assemblage) %>% dplyr::summarise(Values= mean(Values, na.rm=T)) %>%
  dplyr::group_by(Index, Assemblage) %>% dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
Div.anomC$absolute <- ifelse(Div.anomC$Values<0,1,2)

ggplot(Div.anomC)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Functional indices anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_grid(Index~Assemblage, scales="free_y")+
  theme_minimal()+
  guides(fill=F)

#Anomalies biodiv
index.long <- as.data.frame(index) %>% tidyr::pivot_longer(1:3, names_to="Index",values_to="Values")
index.long$Values <- as.numeric(as.character(index.long$Values)) 
index.long$Year <- as.numeric(as.character(index.long$Year)) 

index.anomY<- index.long %>% dplyr::group_by(Year, Index) %>% dplyr::summarise(Values= mean(Values, na.rm=T)) %>%
  dplyr::group_by(Index) %>% dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
index.anomY$absolute <- ifelse(index.anomY$Values<0,1,2)

index.anomY <- index.anomY[index.anomY$Index!="Richness",]

anom.ind.plot <- ggplot(index.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Biodiversity indices anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  facet_wrap(.~Index, scales="free_y")+
  theme_minimal()+
  guides(fill=F)

#Anomlies densities (!!! Comm with juveniles)
Dens <- cbind(metadata,Values=rowSums(Comm))

Dens.anomY<- Dens %>% dplyr::group_by(Year) %>% dplyr::summarise(Values= mean(Values, na.rm=T)) %>%
   dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
Dens.anomY$absolute <- ifelse(Dens.anomY$Values<0,1,2)

Dens.anom.plot <- ggplot(Dens.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Mean densities anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  theme_minimal()+
  guides(fill=F)


Dens <- cbind(metadata,Values=rowSums(Comm[,which(grepl("G0", colnames(Comm)))]))

Dens.anomY<- Dens %>% dplyr::group_by(Year) %>% dplyr::summarise(Values= mean(Values, na.rm=T)) %>%
  dplyr::mutate(Values= Values-  mean(Values)) %>% ungroup()
Dens.anomY$absolute <- ifelse(Dens.anomY$Values<0,1,2)

Dens.anom.juv.plot <- ggplot(Dens.anomY)+
  geom_bar(aes(x= Year, y= Values, fill=absolute),stat="identity")+
  ggtitle("Mean juveniles densities anomalies")+
  scale_fill_viridis_c()+
  xlab("Year")+
  ylab("Anomalies")+
  labs(fill="Anomalies")+
  theme_minimal()+
  guides(fill=F)

gridExtra::grid.arrange(anom.ind.plot,Dens.anom.plot,Dens.anom.juv.plot,nrow=2,ncol=2,layout_matrix=rbind(c(1,1),c(2,3)))

library(gganimate)
install.packages("gganimate")



animated<- ggplot(data=correction,aes(x = Passage...Mois, y= Total, fill =Group_Name, frame=Passage...Année))+
  geom_area()+ scale_x_continuous(breaks=c(1:12)) +
  scale_fill_viridis(discrete = TRUE)+
  #transition_states(Passage...Année, transition_length = 2, state_length = 1)+
  #ggtitle("Fréquence d'apparition des différents groupes
  # en fonction des Mois")+
  xlab("Mois")+
  ylab(paste("Densities (log10)",Ss)) +
  labs(fill="Functional Group")+
  facet_wrap(~Station) 

gganimate(animated,interval = 1, filename=paste('Figures/geomarea_annualvariation.gif'))
Aa

