

library(sp)
library(raster)
library(rgdal)
library(sf)
# Cargar bandas de sentinel
B1 = raster("Sentinel/T19LCF_20220901T145729_B01.jp2")
B2 = raster("Sentinel/T19LCF_20220901T145729_B02.jp2")
B3 = raster("Sentinel/T19LCF_20220901T145729_B03.jp2")
B4 = raster("Sentinel/T19LCF_20220901T145729_B04.jp2")
B5 = raster("Sentinel/T19LCF_20220901T145729_B05.jp2")
B6 = raster("Sentinel/T19LCF_20220901T145729_B06.jp2")
B7 = raster("Sentinel/T19LCF_20220901T145729_B07.jp2")
B8 = raster("Sentinel/T19LCF_20220901T145729_B08.jp2")
B11 = raster("Sentinel/T19LCF_20220901T145729_B11.jp2")

# Zona de Interes
Zona = st_read("SHP/Zona_Amortiguamiento.geojson")
Zona_py <- st_transform(Zona ,
                        crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
#Indice de NDVI
ndvi <- (B8-B4)/(B8+B4) # NDVI
ndvi_py= projectRaster(ndvi, crs="+proj=longlat +datum=WGS84 +no_defs")

# Cortamos con el Area al raster
NDVI_clip= crop(ndvi_py, Zona_py)

NDVI_clip[NDVI_clip>1] <- 1; NDVI_clip[NDVI_clip< (-1)] <- -1 #Reescalado para evitar outliers
# Visualización NDVI guardada en el objeto ndvi_plot
plot(NDVI_clip)

# exportar la capa raster
writeRaster(NDVI_clip, "NDVI/NDVI_ZonaAmortiguamient.tif", drivername="Gtiff")


#  Clasificacion no supervisada por kmeans
NDVI_cli <- raster("NDVI/NDVI_ZonaAmortiguamient.tif")
NDVI_clip     <- NDVI_clip  <- mask(NDVI_cli , Zona_py)

plot(NDVI_clip)
# Combertir en reflextancia de superficie 
v <- getValues(NDVI_clip)
i <- which(!is.na(v))
v <- na.omit(v)
## clasificación kmeans 

E <- kmeans(v, 4, iter.max = 100, nstart = 10)
kmeans_raster <- raster(NDVI_clip)
kmeans_raster[i] <- E$cluster
plot(kmeans_raster)


# Exportar la clasificacion
writeRaster(kmeans_raster, "clasificcion/NDVI_ZonaAmortiguamiento_kmeans.tif")
#------------------------------------------------------------------------
# Graficar la clasificacion 
library(raster)
library(ggplot2)
RandomForest <- raster("clasificcion/NDVI_ZonaAmortiguamiento_kmeans.tif")
plot(RandomForest)
RandomForest=clara_raster
#cambio = cbind(c(1,2,3,4), c(NA,2,NA,NA))
#Deforestacion = reclassify(RandomForest, rcl = cambio)
#plot(Deforestacion)
#Deforestacion_pol = rasterToPolygons(Deforestacion)

RandomForest.pa        <-  rasterToPoints(RandomForest)
RandomForest.pa_a      <-  data.frame(RandomForest.pa)
colnames(RandomForest.pa_a) <- c("x","y", "Cober")

cols <-c("#386641", # Bosque 1
         "#8ac926", # Bosque Secundario 2
         "#c1121f", # Suelo desnudo 3
         "#0077b6" )# Cuerpos de Agua 4


library(rgdal)
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(grid)
library(RStoolbox)

RandomForest.pa_a %>% subset(Cober <= 3 & Cober> 2) ->Deforestacion

sum = RandomForest.pa_a %>%
  group_by(Cober) %>%
  summarise(count =n())%>%
  ungroup()

sum = mutate(sum, meters =count * 100, has= meters / 10000)
sum = dplyr::select(sum, Cober, has)

sum = mutate(sum, Categoria =c("Bosque",      
                               "Bosque Secundario", 
                               "Suelo desnudo",  
                               "Cuerpos de Agua "))       
summ <- sum %>%
  mutate(Categoria= fct_reorder(Categoria, has, .desc = TRUE))

summ

Esta_2008= ggplot(data = summ, aes(x=Categoria, y=has, fill=Categoria))+
  scale_fill_manual(values = cols )+
  geom_col()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif",face="bold", angle=90),
        axis.text.x  = element_text(color="black", size=10, family="serif",face="bold", angle = 25, hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  
  geom_text(aes(label=round(has,2), y = 100), position = position_dodge(0.90), size=4, angle=90)+
  labs(x= "",
       y= "Kilometros (km2)")
Esta_2008

summ$has =round(summ$has,2)
# lets count proportions
summ$fraction = summ$has/sum(summ$has)
summ

# Compute the cumulative proportions (top of each rectangle)
summ$ymax = cumsum(summ$fraction)
summ
# Compute the bottom of each rectangle
summ$ymin = c(0, head(summ$ymax, n=-1))
summ
#compute label position
summ$labelPosition= (summ$ymax+summ$ymin)/2
summ
#get data label
summ$label= paste0(summ$Categoria ,"\n Valor=", summ$has)
summ
library(ggrepel)

summ$Porcentaje =  summ$has*100/168317.87
summ$Porcentaje =round(summ$Porcentaje,2)
# the plot
Pastel=ggplot(summ,aes(ymax=ymax,ymin=ymin,xmax=4, xmin=2, fill=Categoria ))+
  geom_rect(alpha=0.8)+
  coord_polar(theta="y")+
  xlim(c(0,4))+
  theme_void()+
  geom_text(aes(y=labelPosition,label=paste0(round(Porcentaje,2), "%")),x=3, 
            color="black", family="serif",angle=90,
            size=2.5)+
  theme(legend.position = "none")+
  scale_fill_manual(values = cols)+
  geom_label_repel(data = summ,
                   aes(y = labelPosition, label = label),x=7,
                   size = 2, nudge_x = 4, show.legend = FALSE) 
Pastel

dtafram = data.frame(Porcentaje =summ$Porcentaje,
                     Hectarias = summ$has,
                     Cobertura = summ$Categoria)

dtaframe <-dtafram %>%
  mutate(Cobertura = fct_reorder(Cobertura, Hectarias , .desc = TRUE))

Factor = 1500

Bar_gg= ggplot()+
  geom_bar(data =dtaframe, aes(x= Cobertura, y=  Hectarias, fill=Cobertura), stat="identity",alpha = 0.7)+
  geom_line(data=dtaframe, aes(x=Cobertura, y=Porcentaje*Factor, group=1, linetype = "dashed"),
            show.legend = F)+
  geom_point(data =dtaframe, aes(x= Cobertura, y=Porcentaje*Factor), color="red")+
  geom_text(data =dtaframe,  aes(x= Cobertura, y=Porcentaje*Factor, 
                                 label= paste0(round(Porcentaje,2), "%")), size=2)+
  scale_fill_manual(values = cols)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=8,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=8,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=8, family="serif", angle=90),
        axis.text.x  = element_blank(),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 7))+
  scale_y_continuous(sec.axis = sec_axis(trans= ~.*1/1500, name="Porcentaje (%)"))+
  labs(x= "Clasificacion",
       y= "Hectáreas (%)",
       fill="Tipo de Cobertura")
Bar_gg
library(elevatr)
elev = get_elev_raster(Zona_py, z=7)
plot(elev)
Poligo_alt    <- crop(elev, Zona_py)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Zona_py)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)


library(ggspatial)
Clasifi = ggplot()+
  geom_raster(data = RandomForest.pa_a , aes(x,y,fill =Cober)) + 
  scale_fill_gradientn(colours = cols, name="Clasificación de \nla cubierta terrestre",
                       labels = c("[Bosque] ",
                                  "[Suelo desnudo]", 
                                  "[Bosque Secundario]",
                                  "[Cuerpos de Agua]"),
                       breaks = c(1,2,3,4))+
  geom_sf(data = Zona_py, fill=NA, size=0.05, color=NA)+
  coord_sf(xlim = c(-70.13, -69.86), ylim = c(-13.02 ,-12.89)) +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(legend.position = c(0.90, 0.85),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))

Clasifi

GG_Defore=ggplot()+
  geom_raster(data = Deforestacion, aes(x,y), fill="red") +
  coord_sf(xlim = c(-70.13, -69.86), ylim = c(-13.02 ,-12.89)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        axis.title = element_text(face="bold", color="black",family="serif",size=7),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  labs( y = 'Perdida de Bosque', x="")
GG_Defore
library(cowplot)

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD           <- subset(Per, NAME_1  == "Madre de Dios")
Tambopata <- subset(Per, NAME_2 == "Tambopata")

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")


MDD_GG=ggplot()+
  geom_sf(data = MDD, fill=NA, color="black")+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black")+
  geom_sf(data = Tambopata, fill="gray", color="black")+
  geom_sf(data = Zona_py, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -13.4, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 4, family="serif", color = 
             "black",  fontface="italic")

NDVI.pa        <-  rasterToPoints(NDVI_clip)
NDVI.pa_a      <-  data.frame(NDVI.pa)
colnames(NDVI.pa_a) <- c("x","y", "NDVI")

viz_ndvi <- list(palette = cpt("grass_ndvi"))
find_cpt("grass")

col = cpt(pal = "grass_ndvi")
#------------------------------------------------------------------------
GG_NDVI=ggplot()+
  geom_raster(data = NDVI.pa_a , aes(x,y,fill = NDVI)) + 
  scale_fill_gradientn(colors = cpt(pal = "grass_evi"), name="NDVI") +
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  geom_sf(data = Zona_py, fill=NA, size=0.05, color=NA)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2.5,"line"), #ancho de cuadrados de referencia 
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))



ggAgru =ggdraw() +
  coord_equal(xlim = c(0, 9), ylim = c(0, 21), expand = FALSE) +
  draw_plot(GG_Defore , width = 7.5, height = 7.5,x = 1, y = -1)+
  draw_plot(Pastel, width = 8, height = 8,x = 1, y = 4.5)+
  draw_plot(Bar_gg , width = 8, height = 8,x = 1, y = 12.5)+
  theme_void()
ggAgru

Mapa=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Clasifi  , width = 21, height = 21,x = -0.5, y = -4)+
  draw_plot(SurA, width = 7,       height = 7, x = -0.5, y = 12)+
  draw_plot(MDD_GG, width = 7.5,   height = 7, x = 4.9, y = 12)+
  draw_plot(GG_NDVI, width = 7.5,   height = 7, x = 13, y = 12)+
  
  draw_plot(ggAgru  , width = 20, height = 20,x = 14, y = 1)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  annotate(geom = "text", x = 4, y = 20, hjust = 0, vjust = 1,
           label = "Perdida de bosque en Madre de Dios Sentinel-2, Zona de Amortiguamiento ",
           size = 5,family="serif", color = "black", face = "italic")



ggsave(plot=Mapa ,"Mapa/Mapa de clasificacion zona de amaoricuamiento.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)

