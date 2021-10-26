#Import files of OD data collected from ARCGIS Pro- build a network, then run the OD Matrix tool.

#Establish path for retrieving files
Path = getwd()
#Packages:dplyr, sf, tmap. readr
library(plyr)
library(dplyr)
library(readr)
library(sf)

#Read in files - all runs of the network analysis need to be in one folder
dists = read.csv("Dist_Matrix.csv")

OD_Storage = "Arc_To_Correct" #This is the folder that has the original OD data in 
myfiles = list.files(path=(file.path(paste0(Path,"/",OD_Storage))), pattern="*.txt", full.names=TRUE)
File_Orig = paste0("C:/Users/b9054751/OneDrive - Newcastle University/PhD/Data/Transport/Git/Transport/",OD_Storage) 

ldply(myfiles, Process) #import all csv files

Process = function(Data){
  print(paste("Importing:", Data))
  OD = read_csv(Data)%>%
    tidyr::separate(data = .,
                    col = Name,
                    into = c("Orig_Code", "Dest_Code"),
                    sep = " - ",
                    extra = "merge",
                    fill = "left",
                    remove = FALSE)%>%
    select(-c(1,2))%>% 
    filter(., Orig_Code !=Dest_Code)
  
  print(paste("Separated:", Data))
  
  if (length(which(colnames(OD)=="Total_Walk_Distance")) == 0){
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
  } else {
    print("Not Needed")
  }

  OD<- OD %>%select("Orig_Code", "Dest_Code", "Total_PublicTransitTime", "Total_WalkTime")
  
  OD$Total_Travel_Time = OD$Total_PublicTransitTime+OD$Total_WalkTime
  
  OD = left_join(OD, dists, by = c("Orig_Code", "Dest_Code"))
  
  OD$MetPMin = OD$dist/OD$Total_Travel_Time
  OD$MinPMet = OD$Total_Travel_Time/OD$dist
  
  #Group by each origin
  Euc_Indices<- OD %>%
    group_by(Orig_Code) %>%
    summarise(Total_PublicTransitTime = mean(Total_PublicTransitTime), #What is the average travel time to each MSOA?
              Total_WalkTime = mean(Total_WalkTime),
              Total_Travel_Time = mean(Total_Travel_Time),
              dist = mean(dist),
              MetPMin = mean(MetPMin),
              MinPMet = mean(MinPMet)
    )
 
  filename = gsub(File_Orig, "", Data)
  write.csv(Euc_Indices, paste0(Path,"/Euclid_Comp/",filename))
  print(paste("Completed:", Data, "Saved as:", filename))
}



myfiles
OD = ldply(myfiles, read_csv) #import all csv files

#OD = data.frame(ldply(myfiles, read.csv))

filename = "All_Euclid.csv"


#Step 1 - Import and clean the data

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

#This is only needed because of a flaw in data collection - will not be needed in most situations
if (length(which(colnames(OD)=="Total_Walk_Distance")) == 0){
  source("Arc_Correction.R")
} else {
  print("Not Needed")
}


OD<- OD %>%select("Orig_Code", "Dest_Code", "Total_PublicTransitTime", "Total_WalkTime")

OD$Total_Travel_Time = OD$Total_PublicTransitTime+OD$Total_WalkTime

OD = left_join(OD, dists, by = c("Orig_Code", "Dest_Code"))

OD$MPH = OD$dist/OD$Total_Travel_Time

OD<-OD%>% filter(., Orig_Code !=Dest_Code)

#Group by each origin
Euc_Indices<- OD %>%
  group_by(Orig_Code) %>%
  summarise(Total_PublicTransitTime = mean(Total_PublicTransitTime), #What is the average travel time to each MSOA?
            Total_WalkTime = mean(Total_WalkTime),
            Total_Travel_Time = mean(Total_Travel_Time),
            dist = mean(dist),
            MPH = mean(MPH)
            )


write.csv(Euc_Indices, paste0(Path,"/Euclid_Comp/",filename))

          