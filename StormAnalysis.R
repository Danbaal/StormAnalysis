
#Data Processing

#Function to download the data ----------------
read_data <- function(fileName, source_url) {  
  if (!file.exists(fileName)) {
    download.file(source_url, destfile = fileName, method = "curl")
  }
  dataset <- read.csv(fileName, stringsAsFactors=FALSE)
  dataset
}
#-------------------------------------------

setwd("~/DataScience/StormAnalysis")

#Downloading and Loading the Data
stormData <- read_data("repdata-data-StormData.csv.bz2", 
                       "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

# ------ Cleanin the Data -----

#number of unique event types
length(unique(stormData$EVTYPE))

#Cleaning the Data
events <- tolower(stormData$EVTYPE) #all characters to lowercase
events <- gsub("[[:blank:][:punct:]+]", " ", events) #cleaning punct. characters

unique(events[grepl(".*tornado.*",events)]) #look for uniques tornados
unique(events[grepl(".*flood.*",events)]) #look for uniques floods
unique(events[ (grepl(".*tstm.*",events) | grepl(".*thunderstorm.*",events))  & grepl(".*wind.*",events)]) #Thunderstorm winds
unique(events[grepl(".*hurricane.*",events)]) #find hurricanes
unique(events[grepl(".*drought.*",events)]) #find droughts
unique(events[grepl(".*heat.*",events)]) #find heats... do after drought
unique(events[grepl(".*hail.*",events)])# find hail
unique(events[grepl(".*ice.*",events) | grepl(".*snow.*",events)]) # ice and snow


events <- gsub(".*flood.*","flood", events) #grouping all kinds of floods

#number of unique event types after cleaning
length(unique(events))

#update the dataframe
stormData$EVTYPE <- events

#No Further data processing will be perform

#---- Sumarize Data ---------

#eventsData <- ddply(stormData, .(EVTYPE), summarise,
#                    fatalities = sum(FATALITIES),
#                    injuries = sum(INJURIES))

#----------------------------


#---- Time Line Data --------

stormTimeLine <- stormData[c('EVTYPE','FATALITIES','INJURIES')]
stormTimeLine$DATE <- as.Date(stormData$BGN_DATE, '%m/%d/%Y')

#Order by date
stormTimeLine <- stormTimeLine[order(stormTimeLine$DATE),]
#Get dates from 2001
stormTimeLine <- subset(stormTimeLine, as.Date("1/1/2001" , "%m/%d/%Y") < DATE)
#Delete irrelevant events
stormTimeLine <- subset(stormTimeLine, FATALITIES != 0 | INJURIES != 0 )
#CumSums
stormTimeLine$FATCUMSUM = cumsum(stormTimeLine$FATALITIES)
stormTimeLine$INJCUMSUM = cumsum(stormTimeLine$INJURIES)
head(stormTimeLine)

library(ggplot2)
qplot(data = stormTimeLine, DATE, FATCUMSUM, geom="step")
#------------------------------------------

#Loading the Data
#path <- "C:/Users/Dani/DataScience/Reproducible Research/StormAnalysis/repdata-data-StormData.csv"
#path <- "./DataScience/repdata-data-StormData.csv"
#stormData <- read.csv(file = path)

#find the event types that are most harmful to population health
library(plyr)
library(ggplot2)

incidents <- ddply(stormData, .(EVTYPE), summarise,
                   fatalities = sum(FATALITIES),
                   injuries = sum(INJURIES))
head(incidents)

#qplot(data = incidents, 
#      x=EVTYPE, y=cumsum(fatalities)) + geom_step


fatalData <- head(incidents[order(incidents$fatalities, decreasing = T), ], 10)
injurData <- head(incidents[order(incidents$injuries, decreasing = T), ], 10)

#Top 10 events that caused largest number of deaths are

fatalData[, c("EVTYPE", "fatalities")]

#Top 10 events that caused most number of injuries are

injurData[, c("EVTYPE", "injuries")]

#Processing the date for evaluating Economic effects

######################### AUX_FUNCTION #################################
exp_transform <- function(e) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (e %in% c('h', 'H'))
        return(2)
    else if (e %in% c('k', 'K'))
        return(3)
    else if (e %in% c('m', 'M'))
        return(6)
    else if (e %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(e))) # if a digit
        return(as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid exponent value.")
    }
}
########################################################################

prop_dmg_exp <- sapply(stormData$PROPDMGEXP, FUN=exp_transform)
stormData$prop_dmg <- stormData$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(stormData$CROPDMGEXP, FUN=exp_transform)
stormData$crop_dmg <- stormData$CROPDMG * (10 ** crop_dmg_exp)

# Compute the economic loss by event type
econ_loss <- ddply(stormData, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)

#Top 10 events that caused most property damage (in dollars) are as follows

prop_dmg_events[, c("EVTYPE", "prop_dmg")]

#Similarly, the events that caused biggest crop damage are

crop_dmg_events[, c("EVTYPE", "crop_dmg")]

#Results

library(ggplot2)
library(gridExtra)

p1 <- ggplot(data = fatalData,
             aes(x=EVTYPE, fatalities, y=fatalities, fill=fatalities))+
    geom_bar(stat="identity") +
    coord_flip() +
    ylab("Total number of fatalities") +
    xlab("Event type") +
    theme(legend.position="none")

             
