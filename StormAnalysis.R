
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

#setwd("~/DataScience/Reproducible Research/StormAnalysis")
#setwd("C:/Users/Dani/DataScience/Reproducible Research/StormAnalysis")

#Downloading and Loading the Data
stormData <- read_data("repdata-data-StormData.csv.bz2", 
                       "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

# ------ Cleanin the Data -----

#number of unique event types
length(unique(stormData$EVTYPE)) #985

#Cleaning the Data
events <- tolower(stormData$EVTYPE) #all characters to lowercase
events <- gsub("[[:blank:][:punct:]+]", " ", events) #cleaning punct. characters

#----------------------------------Exploring grouping-------------------
unique(events[grepl(".*tornado.*",events)]) #look for uniques tornados
unique(events[grepl(".*flood.*",events)]) #look for uniques floods
#-----------------------------------------------------------------------

events <- gsub(".*tornado.*", "tornado", events)
events <- gsub(".*flood.*", "flood", events)
events[ (grepl(".*tstm.*",events) | grepl(".*thunderstorm.*",events))  & grepl(".*wind.*",events)] <- "thunderstorm wind"
events <- gsub(".*hurricane.*", "hurricane", events)
events <- gsub(".*drought.*", "drought", events)
events <- gsub(".*heat.*", "heat", events)
events <- gsub(".*hail.*", "hail", events)
events[grepl(".*ice.*",events) | grepl(".*snow.*",events)] <- "snow and ice"

#number of unique event types after cleaning
length(unique(events))

#update the dataframe
stormData$EVTYPE <- events

#No Further data processing will be perform

#---- Sumarize Data ---------
library(plyr)
#Events types that are most harmful to population health
casualities <- ddply(stormData, .(EVTYPE), summarise,
                    FATAL = sum(FATALITIES),
                    INJUR = sum(INJURIES))
health_top10 <- head(casualities[order(casualities$FATAL + casualities$INJUR, decreasing=T),] , 10)
top10 <- health_top10$EVTYPE

#Preparing the ggplot
fatalities_p <- health_top10[,1:2]
names(fatalities_p) <- c("EVENT","CASUALITIES")
fatalities_p$TYPE <- "Fatalities"

injuries_p <- health_top10[,c(1,3)]
names(injuries_p) <- c("EVENT","CASUALITIES")
injuries_p$TYPE <- "Injuries"

health_plot <- rbind(fatalities_p , injuries_p)
health_plot$EVENT <- factor(health_plot$EVENT , levels = top10)
health_plot <- health_plot[order(health_plot$EVENT, decreasing=T),]

library(ggplot2)
ggplot(health_plot, aes(x=EVENT, y=CASUALITIES, fill=TYPE)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = c("red", "orange")) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
    

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


library(plyr)
library(ggplot2)



#qplot(data = incidents, 
#      x=EVTYPE, y=cumsum(fatalities)) + geom_step


gggg

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

             
