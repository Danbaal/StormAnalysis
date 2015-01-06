
#Data Processing

#Loading the Data
path <- "C:/Users/Dani/DataScience/Reproducible Research/StormAnalysis/repdata-data-StormData.csv"
stormData <- read.csv(file = path)

#number of unique event types
length(unique(stormData$EVTYPE))

#Cleaning the Data
events <- tolower(stormData$EVTYPE) #all characters to lowercase
events <- gsub("[[:blank:][:punct:]+]", " ", events) #cleaning punct. characters

#number of unique event types after cleaning
length(unique(events))

#update the dataframe
stormData$EVTYPE <- events

#No Further data processing will be perform

#find the event types that are most harmful to population health
library(plyr)

incidents <- ddply(stormData, .(EVTYPE), summarise,
                   fatalities = sum(FATALITIES),
                   injuries = sum(INJURIES))
head(incidents) 

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

             
