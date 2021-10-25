#Import files of OD data collected from ARCGIS Pro- build a network, then run the OD Matrix tool.

####################
#Establish path for retrieving files
Path = getwd()
#Packages:dplyr, sf, tmap
library(plyr)
library(dplyr)
library(readr)
library(sf)
#Read in files - all runs of the network analysis need to be in one folder

myfiles = list.files(path=(file.path(Path,"/Arc_Results_Full")), pattern="*.txt", full.names=TRUE)
myfiles
#OD = read.csv(myfiles[])
OD = data.frame(ldply(myfiles[c(13:15)], read.csv))
filename = "WM_Results.csv"

##############
#Step 1 - Split the Origin-destination data

#This seperates the origins and destinations that are linked 

OD<- tidyr::separate(data = OD,
                     col = Name,
                     into = c("Orig_Code", "Dest_Code"),
                     sep = " - ",
                     extra = "merge",
                     fill = "left",
                     remove = FALSE)%>%
  select(-c(1,2))

#You should now have a dataframe that has origins-destinations, and time to travel between them

###############################################

###POTENTIAL ADDITIONAL STEP: CHANGING THE DESTS INFO TO ZONES - NEEDED FOR "Arc_Networks" files
#Dest_info: All of the destinations in order
Dest_Info = read.csv("DESTS.txt")%>% 
  rename(DestinationID = 1) %>%
  select("DestinationID", "Name")%>%
  mutate(ID = c(1:8480))

Cent_Info = st_read(dsn = (paste0(Path, "/UK_Centroids")),layer = "UK_CENTROIDS" )%>%
  mutate(ID = c(1:8480))%>%st_drop_geometry()%>%
  select("InterZone", "msoa11cd", "MSOA", "ID") %>%
  mutate(Code = coalesce(InterZone,msoa11cd))
Zones = merge(Dest_Info, Cent_Info, by = "ID")%>%select("DestinationID", "MSOA", "Code")


OD = OD%>%rename(Orig_MSOA = Orig_Code,Dest_MSOA = Dest_Code)
OD = left_join(OD, Zones[,c(1,3)], by = "DestinationID")%>% rename(Dest_Code = Code)
Zones = Zones[,c(2:3)]%>%rename(Orig_MSOA = MSOA)
OD = left_join(OD, Zones, by = "Orig_MSOA")%>% rename(Orig_Code = Code)
rm(Cent_Info, Dest_Info, Zones)
##########################

OD<- OD %>%select("Orig_Code", "Dest_Code", "Total_PublicTransitTime", "Total_WalkTime")


#############

#Step 2 - Import Employment Information

#Import Employment File (Nomis) Business Register and Employment Survey
Emp = read.csv("Employment_Data.csv")
Emp_All = read.csv("Emp_All.csv")
Grouped<- OD %>%  merge(.,Emp, by.x = "Dest_Code", by.y = "Code")%>%
  select(-"Name") %>%
  merge(.,Emp_All, by.x = "Dest_Code", by.y = "Code")

Grouped$Total_Travel_Time = Grouped$Total_PublicTransitTime+Grouped$Total_WalkTime
#Step 3: Index for Analysis

########## All Opportunities

#f(c) = e^-B x t

B = 0.054
Index<-Grouped$Total*
  exp((-B)*Grouped$Total_Travel_Time)

Indices = Grouped %>% select("Dest_Code", "Orig_Code", "Total_PublicTransitTime", "Total_WalkTime", "Total_Travel_Time") %>%
  mutate(Emp_Index = as.numeric(Index))

Indices2<- Indices %>%
  group_by(Orig_Code) %>%
  summarise(Total_PublicTransitTime = mean(Total_PublicTransitTime), #What is the average travel time to each MSOA?
            Total_WalkTime = mean(Total_WalkTime),
            Total_Travel_Time = mean(Total_Travel_Time),
            Emp_Index = sum(Emp_Index)
            )

############Sector
Sec = Grouped[,c(1:2)]
Jobs = names(Emp[,c(3:21)])

for (k in 1:length(Jobs)){
  Index<-Grouped[,(which(colnames(Grouped)==Jobs[k]))]*
    exp(-B*Grouped$Total_Travel_Time)
  
  Sec<- cbind(Sec,Index)
  
  names(Sec)[k+2] <- Jobs[k]
  
  print(paste(Jobs[k], "done"))
}

Sec_agg<- aggregate(x = Sec[,c(3:21)],                # Specify data column
                    by = list(Sec$Orig_Code),              # Specify group indicator
                    FUN = sum)%>%
  rename(Name = 1)


Top <- as.data.frame(t(Sec_agg[,-1]))
colnames(Top) <- Sec_agg[,1]
Top$myfactor <- factor(row.names(Top))

Top_Sec = vector()

for (i in 1:nrow(Sec_agg)){
  Test = Top[,c(i,(which(colnames(Top)=="myfactor")))]
  T = top_n(Test, 1, Test[1])
  Top_Sec[i] = as.character(T$myfactor[1])
}
Sec_agg$Top_Sec = Top_Sec

Indices2 = Indices2 %>% rename(Name = Orig_Code) %>%
  left_join(.,Sec_agg[,c(1,21)], by = "Name")

write.csv(Indices2, filename)
####################################
#Step5: Mapping the Index

Path = getwd()

#Import all results as one dataframe
mydir = "Cleaned_Arc" #Folder with data in
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE) #retrieve list of files
myfiles
dat_csv = ldply(myfiles, read_csv) #import all csv files
Index = data.frame(dat_csv)%>%select(-1)



library(sf)
#Import MSOA file and merge
Index_Map<-st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
  st_transform(4326)%>%
  st_make_valid()%>%
  merge(., Index, by.x = "msoa11cd", by.y = "Name")

#Import regions file for visualisation
Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)
# Subset the sf object - remove this if not filtering
#Regions <- Regions[Regions$rgn17nm %in% c("North East", "Yorkshire and The Humber"), ]

library(tmap)
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



