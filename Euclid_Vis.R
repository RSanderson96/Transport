library(plyr)
library(dplyr)
library(sf)
library(tmap)
library(readr)
library(rgdal)

Path = getwd()

#Import all results as one dataframe
mydir = "Euclid_Comp" #Folder with data in
myfiles = list.files(path=mydir, pattern="*.txt", full.names=TRUE) #retrieve list of files
#myfiles #This will show all files in the directory
dat_csv = ldply(myfiles, read_csv) #import all csv files
Index = data.frame(dat_csv)%>%select(-1)


#Import MSOA file and merge
Index_Map<-st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
  st_transform(4326)%>%
  st_make_valid()%>%
  merge(., Index, by.x = "msoa11cd", by.y = "Orig_Code")
rm(Index,dat_csv)
#Remove Isles of Scilly - exception!
Index_Map<-filter(Index_Map, msoa11nm !="Isles of Scilly 001")

Cent_Info = st_read(dsn = (paste0(Path, "/UK_Centroids")),layer = "UK_CENTROIDS" )
Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)

Cent_Info = Cent_Info %>%st_transform(27700)%>% as_Spatial()
Regions = Regions%>%st_transform(27700) %>% as_Spatial()

MSOA = st_read(dsn = (paste0(Path, "/MSOA")),layer = "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries" )
a.data <- over(Cent_Info, Regions[,'rgn17nm'])

Cent_Info = st_read(dsn = (paste0(Path, "/UK_Centroids")),layer = "UK_CENTROIDS" )%>%
  mutate(ID = c(1:8480))%>%
  select("InterZone", "msoa11cd", "MSOA", "ID") %>%
  mutate(Code = coalesce(InterZone,msoa11cd))
Cent_Info$Region = a.data[,1]
Cent_Info = Cent_Info%>% select(c("Code", "Region"))%>%rename(msoa11cd = Code)%>%st_drop_geometry()
Index_Map = left_join(Index_Map, Cent_Info, by = "msoa11cd" )

#Import regions file for visualisation
Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)
Index_Map$Test = 1000*Index_Map$MinPMet
tmap_mode("plot")

# breaks argument used instead of style


tm_shape(Index_Map) +
  tm_fill("MetPMin",
          style = "fisher",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Metres per minute",
          colorNA = "black") +
  tm_layout(title = "Average ",        # add a title
            title.size = 1,
            title.color = "Black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=1,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)

tm_shape(Index_Map) +
  tm_fill("Test",
          style = "fisher",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Minutes per metre",
          colorNA = "black") +
  tm_layout(title = "Average Travel Time to Another Centroid (Minutes per Straight Line Metre)",        # add a title
            title.size = 1,
            title.color = "Black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=1,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)


tm_shape(Index_Map) +
  tm_fill("Total_WalkTime",
          style = "fisher",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Travel Time (Minutes)",
          colorNA = "black") +
  tm_layout(title = "Average Travel Time on to Any MSOA Centroid (Walking Section)",        # add a title
            title.size = 1,
            title.color = "Black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=1,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)

tm_shape(Index_Map) +
  tm_fill("Total_PublicTransitTime",
          style = "fisher",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Travel Time (Minutes)",
          colorNA = "black") +
  tm_layout(title = "Average Travel Time on to Any MSOA Centroid (Public Transport Section)",        # add a title
            title.size = 1,
            title.color = "Black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=1,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)



tm_shape(Index_Map) +
  tm_fill("Total_Travel_Time",
          style = "fisher",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Travel Time (Minutes)",
          colorNA = "black") +
  tm_layout(title = "Average Total Travel Time on to Any MSOA Centroid",        # add a title
            title.size = 1,
            title.color = "Black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=1,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)




Rural_Urban = read.csv("RUC_Class.csv")
Index_Map = left_join(Index_Map,Rural_Urban)

library(ggplot2)
library(RColorBrewer)
# Basic scatter plot
ggplot(Index_Map, aes(x=Total_Travel_Time, y=MinPMet, color = RUC11 ))+ geom_point()



