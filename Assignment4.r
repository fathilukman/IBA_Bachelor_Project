#Libraries
library(reshape2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(readxl)

#Set Working Directory 
getwd()

#Open the files 
Picking_Activity_20210406_110209 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_110209.csv")
Picking_Activity_20210406_113054 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_113054.csv")
Picking_Activity_20210406_113211 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_113211.csv")
Picking_Activity_20210406_113246 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_113246.csv")
Picking_Activity_20210406_114444 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_114444.csv")
Picking_Activity_20210406_114513 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_114513.csv")
Picking_Activity_20210406_114734 <- read.csv("PICKING_ACTIVITY_by_date_range_NL-ROS-05_MUJ356_20210406_114734.csv")

#Count the dimensions of the dataframes
dim(Picking_Activity_20210406_110209)
dim(Picking_Activity_20210406_113054)
dim(Picking_Activity_20210406_113211)
dim(Picking_Activity_20210406_113246)
dim(Picking_Activity_20210406_114444)
dim(Picking_Activity_20210406_114513)
dim(Picking_Activity_20210406_114734)

#All dataframes have the same number of columns, hence can be horizontally concatenated
Full_data <- rbind(Picking_Activity_20210406_110209, Picking_Activity_20210406_113054, Picking_Activity_20210406_113211, Picking_Activity_20210406_113246, Picking_Activity_20210406_114444, Picking_Activity_20210406_114513, Picking_Activity_20210406_114734)

#Identify any duplicated rows on the Dataframe 
sum(duplicated(Full_data))
#Result is 0, thus no duplicated rows

#Tag.ID seems empty 
print(Full_data %>% group_by(Tag.ID) %>% summarise(Percentage=n()/nrow(.)), n = Inf)
#Each of these values are represented in less than 1% of the case, hence it seems judicious to delete the column
Full_data <- select(Full_data, -Tag.ID)

#Columns Condition.Code, QC.Status, Batch.ID, Customs.Commodity.Code and Hazardous.Class seems to be populated solely by NA 
summary(Full_data)
#It is indeed the case, thus it can be removed
Full_data <- select(Full_data, -Condition.Code, -QC.Status, -Batch.ID, -Customs.Commodity.Code, -Hazardous.Class)

#Format the date column on the Full_data to default R date format
Full_data$Date <- as.Date(Full_data$Date, format = "%d-%b-%Y")

#Since our definition of workload balance takes into account time and weight and size, columns with empty values on those will be removed
which(colnames(Full_data) == "Each.Height")
which(colnames(Full_data) == "Nett.Volume")

#See how much of the data are missing values
missing_percentages <- sapply(Full_data[, 18:23], function(x) sum(is.na(x))/nrow(Full_data)*100)
print(missing_percentages)
#Height, Width, and Depth have similar value around 30% 
#This percentage indicates that the NA is not missing completely at random, hence it would be sound to remove them 
Full_data <- Full_data[complete.cases(Full_data[,18:20]),]

#Nett.Volume contains almost only NA most probably due to a systemic reason(not random), hence will be removed
#Additionally, since the team will focus on the Size and Weight for the workload balance of worker Nett.Volume will be removed
#Each Weight will deleted, as the team would rather focus on the weight of item that excludes the packaging. 
Full_data <- select(Full_data, -Nett.Volume, -Each.Weight)

#To know where the locations at on the warehouse, the location document will be loaded and converted
Location <- read_excel("CEVA_oud_gastel_bay_to_lp_location.xlsx")
distinct(Location)

#Join the Location and Full_data in such way that we have according location and LP
Full_data <- Full_data %>%
  left_join(Location, by = c("Current.Pickface" = "Location"))
#Change name of the added column
colnames(Full_data)[colnames(Full_data) == "LP"] <- "Current.LP"

#Doing the same for From.Location
Full_data <- Full_data %>%
  left_join(Location, by = c("From.Location" = "Location"))
#Change name of the added column
colnames(Full_data)[colnames(Full_data) == "LP"] <- "From.LP"
#We will eliminate rows that contains empty values in those 2 columns simultaneously 
na_rows <- which(is.na(Full_data$From.LP) & is.na(Full_data$Current.LP))
Full_data <- Full_data[-na_rows, ]

#Looks like some columns have the same values repeating 
sapply(lapply(Full_data, unique), length)
#It is the case for Site.ID, Client.ID, Code, Expiry.Date, Above.Ground.Level, Location.Zone
#This uniquely repetitive pattern cannot bring much added-value, hence will be removed
Full_data <- select(Full_data, -Site.ID, -Client.ID, -Code, -Expiry.Date, -Above.Ground.Level, -Location.Zone)

