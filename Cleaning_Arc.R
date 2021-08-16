PATH = getwd()
#Packages:dplyr
ArcOD = "Lines_NE.csv"
ORIGINS = "Origins_MSOA_NE.csv"


OD = read.csv(ArcOD)
ORIGINS = read.csv(ORIGINS)

#Step 1 - Split the Origin-destination data
library(dplyr)
#This seperates the origins and destinations that are linked in R
OD<- tidyr::separate(data = OD,
                     col = Name,
                     into = c("Origin", "Destination"),
                     sep = " - ",
                     remove = FALSE)%>%
  select(-"Name")
OD<-OD%>% select(c("Origin", "Destination", "Total_PublicTransitTime", "Total_WalkTime"))


###THIS SHOULD BE ABLE TO BE DELETED - ADDING IN THE MSOA NAMES IF NOT IN ORIGINALLY
Test = read.csv("Points_NE.csv")%>%
  rename("ID" = 1,
         "Location" = 2)
Test<- left_join(ORIGINS,Test)
Test<-select(Test, c("Name", "Location"))

OD<- merge(OD,Test, by.x = "Origin", by.y = "Location")%>%
  select(-"Origin") %>%
  rename("Origin" = "Name")
OD<- merge(OD,Test, by.x = "Destination", by.y = "Location")%>%
  select(-"Destination") %>%
  rename("Destination" = "Name")

#############

