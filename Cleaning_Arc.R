Path = getwd()
#Packages:dplyr, sf, tmap

#List all files to include - edit based on needs
ArcOD = "Lines_NE.csv"
ORIGINS = "Origins_MSOA_NE.csv"


#Read in files
OD = read.csv(ArcOD)
ORIGINS = read.csv(ORIGINS)

#Step 1 - Split the Origin-destination data
library(dplyr)
#This seperates the origins and destinations that are linked in R
OD<- tidyr::separate(data = OD,
                     col = Name,
                     into = c("ORIGS", "DESTS"),
                     sep = " - ",
                     remove = FALSE)%>%
  select(-"Name")
OD<-OD%>% select(c("ORIGS", "DESTS", "Total_PublicTransitTime", "Total_WalkTime"))


###THIS SHOULD BE ABLE TO BE DELETED - ADDING IN THE MSOA NAMES IF NOT IN ORIGINALLY
Test = read.csv("Points_NE.csv")%>%
  rename("ID" = 1,
         "Location" = 2)
Test<- left_join(ORIGINS,Test)
Test<-select(Test, c("Name", "Location"))

OD<- merge(OD,Test, by.x = "ORIGS", by.y = "Location")%>%
  select(-"ORIGS") %>%
  rename("ORIGS" = "Name")
OD<- merge(OD,Test, by.x = "DESTS", by.y = "Location")%>%
  select(-"DESTS") %>%
  rename("DESTS" = "Name")

write.csv(OD, "Arc_Cleaned.csv")
#############

library(sf)
#Import MSOA file (national)
MSOA<- st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
  st_transform(4326)%>%st_make_valid()
#Import Population file & tidy
#Population data from UK.gov
MSOA_Population = read.csv("MSOA_Population.csv")%>% select(c(1,2,3,53)) 
#merge
MSOA <- merge(MSOA, MSOA_Population, by.x="msoa11nm", by.y="MSOA_Name") #Merge
#Import Employment File (Nomis) Business Register and Employment Survey
MSOA_Emp = read.csv("Employment_Count_2019.csv") 
#merge
MSOA <- merge(MSOA, MSOA_Emp, by.x="msoa11nm", by.y="MSOA_Name") #Merge

rm(MSOA_Population, MSOA_Emp)



Grouped<-merge(OD, st_drop_geometry(MSOA), by.x = "ORIGS", by.y = "msoa11nm")%>% 
  select(-c("objectid", "msoa11nmw", "st_areasha", "st_lengths", "Emp_Count", "MSOA_Code", "msoa11cd")) %>%
  merge(., st_drop_geometry(MSOA), by.x = "DESTS", by.y = "msoa11nm")%>% 
  select(-c("objectid", "msoa11nmw","st_areasha", "st_lengths", "All_Ages.y", "Age_16_to_64.y"))%>%
  select(c("DESTS", "ORIGS", "Total_PublicTransitTime", "Total_WalkTime", "All_Ages.x", "Age_16_to_64.x", "Emp_Count"))%>%
  rename(Dest_Emp = Emp_Count)%>% 
  filter(.,ORIGS !=DESTS)

Grouped$Index<- Grouped$Dest_Emp *(1/(Grouped$Total_PublicTransitTime + Grouped$Total_WalkTime)) 

#This gives an average distance to each MSOA's nearest 50 MSOAs
Grouped<- Grouped %>%
  group_by(ORIGS) %>%
  summarise(Total_PublicTransitTime = mean(Total_PublicTransitTime),
            Total_WalkTime = mean(Total_WalkTime),
            All_Ages= mean(All_Ages.x),
            Age_16_to_64= mean(Age_16_to_64.x),
            Dest_Emp= mean(Dest_Emp),
            Index = sum(Index))

normalize <- function(x) {
  return((x- min(x)) /(max(x)-min(x)))
}

Grouped$Index=normalize(Grouped$Index)




Index<-st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
  st_transform(4326)%>%
  st_make_valid()%>%
  merge(., Grouped, by.x = "msoa11nm", by.y = "ORIGS")


Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)
# Subset the sf object - remove this if not filtering
#Regions <- Regions[Regions$rgn17nm %in% c("North East", "Yorkshire and The Humber"), ]



library(tmap)
tmap_mode("view")

# breaks argument used instead of style
breaks = c(0, 0.2,0.4,0.6, 0.8, 1) 


tm_shape(Index) +
  tm_fill("Index",
          #style = "jenks",    # used instead of user defined breaks
          breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Accessibility Index",
          colorNA = "black") +
  tm_layout(title = "Accessibility Across Tyne and Wear and Yorkshire",        # add a title
            title.size = 1.5,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            fontfamily = 'Georgia',
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
  tm_borders(col = "grey40", lwd = 0.1)
#edit