#check the Url
filename<-"repdata_data_StormData.csv.bz2"
if(!file.exists(filename)){
        fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileUrl,filename,method = "curl")
}

#load the raw data
storm_data <- read.csv(bzfile(filename))
weather_event <- as.data.frame(storm_data)
str(weather_event)
dim(weather_event)

##subset the data
library(dplyr)
weather_impact <- select(weather_event,c("EVTYPE","FATALITIES","INJURIES",
                                  "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP"))
Health_impact <- select(weather_impact,c("EVTYPE","FATALITIES","INJURIES"))
Economic_impact <- select(weather_impact,c("EVTYPE","PROPDMG","PROPDMGEXP",
                                           "CROPDMG","CROPDMGEXP"))

##check the missing value in the subset
sum(is.na(weather_impact$FATALITIES))
sum(is.na(weather_impact$INJURIES))
sum(is.na(weather_impact$PROPDMG))
sum(is.na(weather_impact$PROPDMGEXP))
sum(is.na(weather_impact$CROPDMG))
sum(is.na(weather_impact$CROPDMGEXP))

##Transform the subset data

#Weather events re-group
sort(table(weather_impact$EVTYPE),decreasing=TRUE)[1:20]
#Re-arrange and group all the weather event with the highly mentioned key words 
#like "HAIL", "WIND", "FLOOD", "RAIN" etc. All the new-grouped event type will be
#collected into a new variable called BRIEF_EVENT. Through this variable, I organise
#the data in a clear way with a few similar key words in one variable. Keyword "OTHERS"
#covers all the rare weather events not in the top 20.
weather_impact$BRIEF_EVENT<-"OTHERS"
weather_impact$BRIEF_EVENT[grep("HAIL", weather_impact$EVTYPE, ignore.case = TRUE)] <- "HAIL"
weather_impact$BRIEF_EVENT[grep("FLOOD", weather_impact$EVTYPE, ignore.case = TRUE)] <- "FLOOD"
weather_impact$BRIEF_EVENT[grep("WIND", weather_impact$EVTYPE, ignore.case = TRUE)] <- "WIND"
weather_impact$BRIEF_EVENT[grep("STORM", weather_impact$EVTYPE, ignore.case = TRUE)] <- "STORM"
weather_impact$BRIEF_EVENT[grep("TORNADO", weather_impact$EVTYPE, ignore.case = TRUE)] <- "TORNADO"
weather_impact$BRIEF_EVENT[grep("LIGHTNING", weather_impact$EVTYPE, ignore.case = TRUE)] <- "LIGHTNING"
weather_impact$BRIEF_EVENT[grep("SNOW", weather_impact$EVTYPE, ignore.case = TRUE)] <- "SNOW"
weather_impact$BRIEF_EVENT[grep("RAIN", weather_impact$EVTYPE, ignore.case = TRUE)] <- "RAIN"
weather_impact$BRIEF_EVENT[grep("HEAT", weather_impact$EVTYPE, ignore.case = TRUE)] <- "HEAT"
weather_impact$BRIEF_EVENT[grep("WINTER", weather_impact$EVTYPE, ignore.case = TRUE)] <- "WINTER"

sort(weather_impact$BRIEF_EVENT,decreasing = TRUE)


#Economic damage re-group
#check the contents distributions in PROPDMGEXP and CROPDMGEXP variable
sort(table(weather_impact$PROPDMGEXP),decreasing = TRUE)
sort(table(weather_impact$CROPDMGEXP),decreasing = TRUE)

#Quoting the Storm Data Documentation, the characters in PROPDMGEXP and CROPDMGEXP 
#variable means that "Alphabetical characters used to signify magnitude include 
#“K” for thousands, “M” for millions, and “B” for billions". In the following sub-section,
#I convert the mentioned damage data into same unit level.

weather_impact$PROPDMGEXP <- as.character(weather_impact$PROPDMGEXP)
weather_impact$PROPDMGEXP[is.na(weather_impact$PROPDMGEXP)] <- 0 
weather_impact$PROPDMGEXP[!grepl("K|M|B", weather_impact$PROPDMGEXP, ignore.case = TRUE)] <- 0
weather_impact$PROPDMGEXP[grep("K", weather_impact$PROPDMGEXP, ignore.case = TRUE)] <- "3"
weather_impact$PROPDMGEXP[grep("M", weather_impact$PROPDMGEXP, ignore.case = TRUE)] <- "6"
weather_impact$PROPDMGEXP[grep("B", weather_impact$PROPDMGEXP, ignore.case = TRUE)] <- "9"
weather_impact$PROPDMGEXP <- as.numeric(as.character(weather_impact$PROPDMGEXP))
weather_impact$PROPERTY <- weather_impact$PROPDMG * 10^weather_impact$PROPDMGEXP

weather_impact$CROPDMGEXP <- as.character(weather_impact$CROPDMGEXP)
weather_impact$CROPDMGEXP[is.na(weather_impact$CROPDMGEXP)] <- 0 
weather_impact$CROPDMGEXP[!grepl("K|M|B", weather_impact$CROPDMGEXP, ignore.case = TRUE)] <- 0
weather_impact$CROPDMGEXP[grep("K", weather_impact$CROPDMGEXP, ignore.case = TRUE)] <- "3"
weather_impact$CROPDMGEXP[grep("M", weather_impact$CROPDMGEXP, ignore.case = TRUE)] <- "6"
weather_impact$CROPDMGEXP[grep("B", weather_impact$CROPDMGEXP, ignore.case = TRUE)] <- "9"
weather_impact$CROPDMGEXP <- as.numeric(as.character(weather_impact$CROPDMGEXP))
weather_impact$CROP <- weather_impact$CROPDMG * 10^weather_impact$CROPDMGEXP

sort(table(weather_impact$PROPERTY),decreasing = TRUE)[1:20]
sort(table(weather_impact$CROP),decreasing = TRUE)[1:20]

##Data Analysis Process
#Clean and calculate the data of Public Health variables
install.packages("plyr")
library(plyr)
#Calculate the total fatalities and injuries numbers
total_health_loss<-ddply(weather_impact, .(BRIEF_EVENT), summarize, 
                         Total = sum(FATALITIES + INJURIES,  na.rm = TRUE))
total_health_loss$Type <- "Fatalities and Injuries"

#Fatalities
Fatalities <- ddply(weather_impact, .(BRIEF_EVENT), summarize, 
                    Total = sum(FATALITIES, na.rm = TRUE))
Fatalities$Type <- "Fatalities"

#Injuries
Injuries <- ddply(weather_impact, .(BRIEF_EVENT), summarize, 
                  Total = sum(INJURIES, na.rm = TRUE))
Injuries$Type <- "Injuries"

#Health damage
Health_damage <- rbind(Fatalities,Injuries)

Health_Event<-join(Fatalities, Injuries, by="BRIEF_EVENT")
Health_Event

#Clean and calculate the data of economic variables
# total economic damage
total_economic_damage <- ddply(weather_impact, .(BRIEF_EVENT), 
                               summarize, Total = sum(PROPERTY + CROP,  na.rm = TRUE))
total_economic_damage$Type <- "Property and Crop Damage"

# Property Damage 
PROPERTY <- ddply(weather_impact, .(BRIEF_EVENT), 
                  summarize, Total = sum(PROPERTY, na.rm = TRUE))
PROPERTY$Type <- "Property"

# Crop Damage
CROP <- ddply(weather_impact, .(BRIEF_EVENT), 
              summarize, Total = sum(CROP, na.rm = TRUE))
CROP$Type <- "crop"

# Economic Damage
Economic_Damage <- rbind(PROPERTY, CROP)
Economic_Event <- join(PROPERTY, CROP,by="BRIEF_EVENT")
Economic_Event


##The plots
#Plot the health damage 
library(ggplot2)
Health_damage$BRIEF_EVENT <- as.factor(Health_damage$BRIEF_EVENT)

HP <- ggplot(Health_damage,aes(x=BRIEF_EVENT,y=Total,fill=Type))+ geom_bar(stat = "identity") +
        coord_flip() +
        labs(x="Event",y="Numbers of Fatalities and Injuries",title="Severe Weather Impacts on Public Health") +
        theme(plot.title = element_text(hjust = 1))
HP+theme_bw()


#Plot the economic damage
Economic_Damage$BRIEF_EVENT <- as.factor(Economic_Damage$BRIEF_EVENT)
EP <- ggplot(Economic_Damage,aes(x=BRIEF_EVENT,y=Total,fill=Type))+ geom_bar(stat = "identity") +
        coord_flip() +
        labs(x="Event",y="Amouts of Dollar Loss",title="Severe Weather Impacts on Economy") +
        theme(plot.title = element_text(hjust = 1))
EP+theme_bw()
        