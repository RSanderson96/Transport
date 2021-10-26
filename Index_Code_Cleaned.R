#Import files of OD data collected from ARCGIS Pro- build a network, then run the OD Matrix tool.

#Establish path for retrieving files
Path = getwd()
#Packages:dplyr, sf, tmap. readr
library(plyr)
library(dplyr)
library(readr)
library(sf)

#Read in files - all runs of the network analysis need to be in one folder

OD_Storage = "Arc_Results_Full" #This is the folder that has the original OD data in 


myfiles = list.files(path=(file.path(paste0(Path,"/",OD_Storage))), pattern="*.txt", full.names=TRUE)
myfiles

OD1 = data.frame(ldply(myfiles[1], read.csv))

filename = "WM_Results.csv"


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
if (is.integer(((which(colnames(OD)!="Total_Walk_Distance")))) == TRUE){
  source("Arc_Correction.R")
}

#Seclect the columns is needed
OD<- OD %>%select("Orig_Code", "Dest_Code", "Total_PublicTransitTime", "Total_WalkTime")


#Step 2 - Import Employment Information

#Import Employment File (Nomis) Business Register and Employment Survey
Emp = read.csv("Employment_Data.csv")
Emp_All = read.csv("Emp_All.csv")
Grouped<- OD %>%  merge(.,Emp, by.x = "Dest_Code", by.y = "Code")%>%
  select(-"Name") %>%
  merge(.,Emp_All, by.x = "Dest_Code", by.y = "Code")

Grouped$Total_Travel_Time = Grouped$Total_PublicTransitTime+Grouped$Total_WalkTime


#Step 3: Index for Analysis

#f(c) = e^-B x t -Accessibility index

B = 0.054 #B found online

#Calculate the index column
Index<-Grouped$Total*
  exp((-B)*Grouped$Total_Travel_Time)

#Make a DF with the data to investigate 
Indices = Grouped %>% select("Dest_Code", "Orig_Code", "Total_PublicTransitTime", "Total_WalkTime", "Total_Travel_Time") %>%
  mutate(Emp_Index = as.numeric(Index))

#Group by each origin
Indices<- Indices %>%
  group_by(Orig_Code) %>%
  summarise(Total_PublicTransitTime = mean(Total_PublicTransitTime), #What is the average travel time to each MSOA?
            Total_WalkTime = mean(Total_WalkTime),
            Total_Travel_Time = mean(Total_Travel_Time),
            Emp_Index = sum(Emp_Index)
            )


#Step 4 - which industry is the most accessible?
Sec = Grouped[,c(1:2)] # Select Info about origs/tests
Jobs = names(Emp[,c(3:21)])

#This loop calculates the index based on each job sector
for (k in 1:length(Jobs)){
  Index<-Grouped[,(which(colnames(Grouped)==Jobs[k]))]*
    exp(-B*Grouped$Total_Travel_Time)
  
  Sec<- cbind(Sec,Index)
  
  names(Sec)[k+2] <- Jobs[k]
  
  print(paste(Jobs[k], "done"))
}

#Aggregate this by origin
Sec_agg<- aggregate(x = Sec[,c(3:21)],                # Specify data column
                    by = list(Sec$Orig_Code),              # Specify group indicator
                    FUN = sum)%>%
  rename(Name = 1)

#Which is the top sector?
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

#Add to overall indices info
Indices = Indices %>% rename(Name = Orig_Code) %>%
  left_join(.,Sec_agg[,c(1,21)], by = "Name")

write.csv(Indices, paste0(Path,"/Cleaned_Arc/",filename))

          