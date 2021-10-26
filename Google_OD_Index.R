#Need to start with OD data - this will generally be in the format with individual columns for origins and destinations, with additional columns for distance and possible other factors

#Packages - dplyr, readr,sf
Path = getwd()

#Import Data

#Google Data
library(dplyr)
library(readr)
myfiles = list.files(path=(file.path(Path,"/Google_Results")), pattern="*.csv", full.names=TRUE)
OD = data.frame()

for (i in 1:(length(myfiles))){
  X = read_csv(myfiles[i],trim_ws = T)%>% select(-1)
  OD<-rbind(OD, X)
}
rm(myfiles,X,i)
#####Import MSOA employment data
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
  select(-c("objectid", "msoa11nmw", "st_areasha", "st_lengths", "MSOA_Code", "msoa11cd")) %>%
  rename(Orig_Emp = Emp_Count,
         Orig_Pop = All_Ages) %>%
  merge(., st_drop_geometry(MSOA), by.x = "DESTS", by.y = "msoa11nm")%>% 
  rename(Dest_Pop = All_Ages,
         Dest_Emp = Emp_Count)%>%
  select(c("DESTS", "ORIGS", "Google_Dist", "duration","Orig_Pop","Dest_Pop", "Orig_Emp", "Dest_Emp", "Mode"))  #%>% 

Grouped$Emp_Reach<- Grouped$Dest_Emp*(Grouped$Google_Dist+Grouped$duration) 
Grouped$Pop_Reach<- Grouped$Orig_Pop*(Grouped$Google_Dist+Grouped$duration)

Grouped<-within(Grouped, Emp_Reach[Emp_Reach==0] <- (Orig_Emp[Emp_Reach==0]))
Grouped<-within(Grouped, Pop_Reach[Pop_Reach==0] <- Dest_Pop[Pop_Reach==0])


Grouped<- Grouped %>%
  group_by(ORIGS) %>%
  summarise(Google_Dist = mean(Google_Dist),
            duration = mean(duration),
            #All_Ages= mean(All_Ages.x),
            #Age_16_to_64= mean(Age_16_to_64.x),
            #Dest_Emp= mean(Dest_Emp),
            Emp_Reach = sum(Emp_Reach),
            Pop_Reach = sum(Pop_Reach))

Grouped$Index<-Grouped$Emp_Reach/Grouped$Pop_Reach


#Grouped<-merge(OD, st_drop_geometry(MSOA), by.x = "ORIGS", by.y = "msoa11nm")%>% 
#  select(-c("objectid", "msoa11nmw", "st_areasha", "st_lengths", "Emp_Count", "MSOA_Code", "msoa11cd")) %>%
#  merge(., st_drop_geometry(MSOA), by.x = "DESTS", by.y = "msoa11nm")%>% 
#  select(-c("objectid", "msoa11nmw","st_areasha", "st_lengths", "All_Ages.y", "Age_16_to_64.y"))%>%
#  select(c("DESTS", "ORIGS", "Google_Dist", "duration", "Mode", "All_Ages.x", "Age_16_to_64.x", "Emp_Count", "Line_Dist"))%>%
#  rename(Dest_Emp = Emp_Count)%>%
#  filter(.,Google_Dist != "NA") 

#Accessibility = Opportunities x cost
#Cost = Time+distance+........
#Waiting time? Time to access the network? Weight of walking? Economic cost?
#DOES ANYTHING NEED TO BE NORMALISED



Grouped$Index<- Grouped$Dest_Emp *(1/(Grouped$Google_Dist + Grouped$duration))

#This gives an average distance to each MSOA's nearest 50 MSOAs
Grouped<- Grouped %>%
  group_by(ORIGS) %>%
  summarise(Google_Dist = mean(Google_Dist),
            Line_Dist = mean(Line_Dist),
            #Dist_Diff = mean(Dist_Diff),
            Duration = mean(duration),
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
Regions <- Regions[Regions$rgn17nm %in% c("North East", "Yorkshire and The Humber"), ]



library(tmap)
tmap_mode("view")

# breaks argument used instead of style
breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7, 0.8, 0.9, 1) 


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



########################################################################################################

#% of lowest IMD decile per LAD
IMD_LSOA_2019 = read.csv("IMD_2019_Arc2.csv")
Convert = read.csv("PCD_OA_LSOA_MSOA_LAD_FEB20_UK_LU.csv")
Convert = Convert [,c(8,9,10)]
names(Convert)[1] <- "LSOA_Code"
names(Convert)[2] <- "MSOA_Code"
#names(Convert)[3] <- "LAD2019_Code"
Convert = dplyr::distinct(Convert) #Removes duplicate rows, producing a list of LSOAs and their matching MSOAs/LADs
IMD_2019 <- merge(IMD_LSOA_2019,Convert,by="LSOA_Code")
rm(Convert)

#G_Results<- G_Results[,c(1,2,3,4,5,6,10)]
#G_Results<-  rename(G_Results,msoa11nm = Origin)
#G_Results<- left_join(G_Results, CENTROIDS[,c(2:3)])
name<-unique(Index$msoa11cd)
C = length (name)
IMD_Percent = vector()
for (i in 1:C){ 
  y = name[i]
  X <- IMD_2019 %>% dplyr::filter (IMD_2019[,5] == name[i]) #select LADs that match list 
  XL = nrow (X) #How many LSOAs are in the LAD
  D = sum(X[4] == "1") #How many LSOAs are in the bottom decile
  P = (D/XL)*100 #Percentage in bottom decile
  IMD_Percent[i] = P #add to vector
}
MSOA_IMD_2019 = data.frame(name, IMD_Percent)
MSOA_IMD_2019[2]<- round(MSOA_IMD_2019[2], digits = 2)
names(MSOA_IMD_2019)[1] <- "msoa11cd"
names(MSOA_IMD_2019)[2] <- "MSOA_IMD_2019"


IMD<-left_join(Index,MSOA_IMD_2019 )

library(ggplot2)
library(RColorBrewer)
# Basic scatter plot
ggplot(IMD, aes(x=Google_Dist, y=All_Ages, color = MSOA_IMD_2019 )) + geom_point()






