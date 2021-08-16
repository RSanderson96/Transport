#Need to start with OD data - this will generally be in the format with individual columns for origins and destinations, with additional columns for distance and possible other factors

#Packages - dplyr, readr,sf
Path = getwd()

#Import Data
library(dplyr)
#Arc Data
OD = read.csv("Arc_Cleaned.csv") %>%
  select(-1)

#Google Data
library(readr)
myfiles = list.files(path=(file.path(Path,"/Google_Results")), pattern="*.csv", full.names=TRUE)
OD = data.frame()

for (i in 1:(length(myfiles))){
  X = read_csv(myfiles[i],trim_ws = T)%>% select(-1)
  OD<-rbind(OD, X)
}
rm(myfiles)
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
  select(-c("objectid", "msoa11nmw", "st_areasha", "st_lengths", "Emp_Count", "MSOA_Code", "msoa11cd")) %>%
  merge(., st_drop_geometry(MSOA), by.x = "DESTS", by.y = "msoa11nm")%>% 
  select(-c("objectid", "msoa11nmw","st_areasha", "st_lengths", "All_Ages.y", "Age_16_to_64.y"))%>%
  select(c("DESTS", "ORIGS", "Google_Dist", "duration", "Mode", "All_Ages.x", "Age_16_to_64.x", "Emp_Count", "Line_Dist"))%>%
  rename(Dest_Emp = Emp_Count)%>%
  filter(.,Google_Dist != "NA") 
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







