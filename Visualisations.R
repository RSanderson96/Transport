library(dplyr)
library(sf)
library(tmap)
library(readr)

Path = getwd()

#Import all results as one dataframe
mydir = "Cleaned_Arc" #Folder with data in
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE) #retrieve list of files
#myfiles #This will show all files in the directory
dat_csv = ldply(myfiles, read_csv) #import all csv files
Index = data.frame(dat_csv)%>%select(-1)


#Import MSOA file and merge
Index_Map<-st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
  st_transform(4326)%>%
  st_make_valid()%>%
  merge(., Index, by.x = "msoa11cd", by.y = "Name")

#Import regions file for visualisation
Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)

tmap_mode("plot")

# breaks argument used instead of style


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
  tm_text("rgn17nm", size = 0.6)+
  tm_shape(Missing)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)



normalize <- function(x) {
  return((x- min(x)) /(max(x)-min(x)))
}

Missing <- Regions[Regions$rgn17nm %in% c("South West", "West Midlands"), ]

Index_Map$Emp_Index_Norm=normalize(Index_Map$Emp_Index)
breaks = c(0, 0.2,0.4,0.6, 0.8, 1) 
plot(Index_Map$Emp_Index_Norm)
tm_shape(Index_Map) +
  tm_fill("Emp_Index_Norm",
          style = "quantile",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Index",
          colorNA = "black") +
  tm_layout(title = "Accessibility Score",        # add a title
            title.size = 1,
            title.color = "black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=0.8,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)+
  tm_shape(Missing)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6)


tm_shape(Index_Map) +
  tm_fill("Top_Sec",
          #style = "jenks",    # used instead of user defined breaks
          #breaks = breaks,
          palette = 'Set3', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Sector",
          colorNA = "black") +
  tm_layout(title = "Most Accessible Employmant Sector",        # add a title
            title.size = 1,
            title.color = "black",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            #fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=0.8,
            text.size = 0.8,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom"))+
  tm_shape(Regions)+
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_text("rgn17nm", size = 0.6) 

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


library(ggplot2)
library(RColorBrewer)
# Basic scatter plot
ggplot(Index_Map, aes(x=Total_PublicTransitTime, y=Total_WalkTime, color = Region ))+ geom_point()



