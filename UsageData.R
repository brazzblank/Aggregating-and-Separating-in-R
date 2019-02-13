install.packages("tidyverse")
library(tidyverse)

#Millions of rows needed to be downloaded into separate csv files and then combined

##### SZ Timer Data #####
Data1 <- read.csv("42 Aug 20th 1.csv") #Put whatever your csv file is named
Data2 <- read.csv("42 Aug 20th 2.csv")
Data3 <- read.csv("42 Aug 20th 3.csv") #Put whatever your csv file is named
Data4 <- read.csv("42 Aug 20th 4.csv") #Put whatever your csv file is named
DataAll <- rbind(Data1, Data2, Data3, Data4)

###### SZ Cloud timer Data ######
Cloud1 <- read.csv("42.1 Aug 20th cloud timer.csv") #Put whatever your csv file is named
Cloud2 <- read.csv("42.2 Aug 20th cloud timer.csv")
Cloud3 <- read.csv("42.3 Aug 6th cloud timer.csv")
CloudAll <- rbind(Cloud1, Cloud2, Cloud3)

################################################ PROD DOMAIN ################################################

###### ISOLATE PRODUCTION DOMAINS ######
Prod <- subset(DataAll, grepl("P", DataAll$DOMAIN)) 
#head(Prod)


###### NUMBER OF LOADS ######
Prod_Loads <- Prod %>%
  group_by(.dots = c("CLIENT","DOMAIN")) %>%
  summarize(count = n())
#print(Prod_Loads)


###### NUMBER OF UNIQUE USERS ######
Prod_UU <- Prod %>%
  group_by(.dots = c("CLIENT","DOMAIN")) %>%
  summarize(count = n_distinct(USERPRINCIPALS))
#print.data.frame(Prod_UU)


###### MEAN LOAD TIME ######
Prod_Load_Time <- aggregate(ELAPSEDTIME~CLIENT + DOMAIN, data = Prod, mean)
#print(Prod_Load_Time)


###### PUTTING ALL OF THE RESULTS IN A TABLE ######
All_Prod <- full_join(Prod_Loads, Prod_UU, by = c("CLIENT", "DOMAIN")) %>%
  left_join(., Prod_Load_Time, by= c("CLIENT", "DOMAIN"))

names(All_Prod) <- c('Client', 'Domain', 'Loads', 'Unique Users', 'Mean (s)')
print.data.frame(All_Prod)


################################################ NONPROD DOMAINS ################################################

###### ISOLATING NONPRODUCTION DOMAINS ######
NonProd <- subset(DataAll, !grepl("P", DataAll$DOMAIN)) 


###### NUMBER OF LOADS ######
NonProd_Loads <- NonProd %>%
  group_by(.dots = c("CLIENT","DOMAIN")) %>%
  summarize(count = n())
#print(Prod_Loads)


###### NUMBER OF UNIQUE USERS ######
NonProd_UU <- NonProd %>%
  group_by(.dots = c("CLIENT","DOMAIN")) %>%
  summarize(count = n_distinct(USERPRINCIPALS))
#print.data.frame(Prod_UU)


###### MEAN LOAD TIME ######
NonProd_Load_Time <- aggregate(ELAPSEDTIME~CLIENT + DOMAIN, data = NonProd, mean)
#print(Prod_Load_Time)


###### PUTTING ALL OF THE RESULTS IN A TABLE ######
All_NonProd <- full_join(NonProd_Loads, NonProd_UU, by= c("CLIENT", "DOMAIN")) %>%
  full_join(., NonProd_Load_Time, by= c("CLIENT", "DOMAIN"))
#print(All_NonProd)

names(All_NonProd) <- c('Client','Domain', 'Loads', 'Unique Users', 'Mean (s)')
print.data.frame(All_NonProd)


################################################ CLOUD TIMER ################################################ 

###### Isolate Prod Domains ######
CloudProd <- subset(CloudAll, grepl("P", CloudAll$DOMAIN)) 
#head(CloudProd)
#str(CloudProd)

### Convert ELAPSEDTIME in to a numeric ### 
CloudProd$ELAPSEDTIME <- as.numeric(as.character(CloudProd$ELAPSEDTIME))


###### NUMBER OF LOADS ######
Cloud_Prod_Loads <- CloudProd %>%
  group_by(.dots = c("CLIENT","DOMAIN")) %>%
  summarize(count = n())
#print(Cloud_Prod_Loads)


###### NUMBER OF UNIQUE USERS ######
Cloud_Prod_UU <- CloudProd %>%
  group_by(.dots = c("CLIENT","DOMAIN")) %>%
  summarize(count = n_distinct(USERPRINCIPALS))
#print.data.frame(Prod_UU)


###### MEAN LOAD TIME ######
Cloud_Prod_Load_Time <- aggregate(ELAPSEDTIME~CLIENT + DOMAIN, data = CloudProd, mean)
#print(Cloud_Prod_Load_Time)



###### PUTTING ALL OF THE RESULTS IN A TABLE ######
All_Cloud <- full_join(Cloud_Prod_Loads, Cloud_Prod_UU, by = c("CLIENT", "DOMAIN")) %>%
  left_join(., Cloud_Prod_Load_Time, by= c("CLIENT", "DOMAIN"))

names(All_Cloud) <- c('Client', 'Domain', 'Loads', 'Unique Users', 'Mean (s)')
print.data.frame(All_Cloud)


################################################ Meta Data Count ################################################

###### Pulling in the columns that are needed ######
MetaData <- subset(CloudAll, select =c("CLIENT", "DOMAIN", "METADATA3"))

###### Getting rid of the blank columns ######
no_blanks <- MetaData[!(is.na(MetaData$METADATA3) | MetaData$METADATA3==""), ]

###### Separating into individual rows if there are more than one notification in a cell ######       
SepData <- separate_rows(no_blanks, METADATA3, sep = ",")   

###### Separating the notification type from the number ######
SplitData <- SepData %>%
  separate(METADATA3, c("Notification", "Count"), sep = "=")

###### Changing the count type to numeric and notification type to factor ######
SplitData$Count <- as.numeric(as.character(SplitData$Count))  
SplitData$Notification <- as.factor(SplitData$Notification)
#sapply(SZsplit, levels)         
#str(SZsplit)

###### Aggregating and grouping the notifications ######
All_Meta <- aggregate(Count~CLIENT + DOMAIN + Notification, data = SplitData, sum)
All_Meta <- All_Meta[order(All_Meta$CLIENT), ]

###### Totaling all of the types of notifications ######      
All_Total_Notif <- aggregate(Count~Notification, data = SplitData, sum)

