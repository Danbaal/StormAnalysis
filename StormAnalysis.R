
#Data Processing

#Function to download the data -------------------------------------
read_data <- function(fileName, source_url) {  
  if (!file.exists(fileName)) {
    download.file(source_url, destfile = fileName, method = "curl")
  }
  dataset <- read.csv(fileName, stringsAsFactors=FALSE)
  dataset
}
#-------------------------------------------------------------------

setwd("~/DataScience/StormAnalysis")
#setwd("C:/Users/Dani/DataScience/Reproducible Research/StormAnalysis")

#Downloading and Loading the Data
stormData <- read_data("repdata-data-StormData.csv.bz2", 
                       "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

dim(stormData)

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

#Taking just records for this century
stormData$DATE <- as.Date(stormData$BGN_DATE, '%m/%d/%Y')
#Get dates from 2001
stormData <- subset(stormData, as.Date("1/1/2001" , "%m/%d/%Y") < DATE)

#Selecting the variables we need for the analysis
stormData <- stormData[,c('EVTYPE','PROPDMG','PROPDMGEXP','CROPDMG',
                          'CROPDMGEXP','DATE', 'FATALITIES', 'INJURIES')]

#No Further data processing will be perform

#---- Sumarize Data ---------
library(plyr)

#Events types that are most harmful to population health
casualities <- ddply(stormData, .(EVTYPE), summarise,
                    FATAL = sum(FATALITIES),
                    INJUR = sum(INJURIES))
health_top10 <- head(casualities[order(casualities$FATAL + casualities$INJUR, decreasing=T),] , 10)
health_top10_names <- health_top10$EVTYPE

#Preparing the ggplot
fatalities_p <- health_top10[,1:2]
names(fatalities_p) <- c("EVENT","CASUALITIES")
fatalities_p$TYPE <- "Fatalities"

injuries_p <- health_top10[,c(1,3)]
names(injuries_p) <- c("EVENT","CASUALITIES")
injuries_p$TYPE <- "Injuries"

health_plot <- rbind(fatalities_p , injuries_p)
health_plot$EVENT <- factor(health_plot$EVENT , levels = health_top10_names)
health_plot <- health_plot[order(health_plot$EVENT, decreasing=T),]

#Economic loss

#Convert the PROPDMGEXP and CROPDMGEXP field 
#into a usuable factor and calculate the true damage to crops and property.

stormData$PROPDMGEXP<-toupper(stormData$PROPDMGEXP)
stormData$CROPDMGEXP<-toupper(stormData$CROPDMGEXP)

stormData[stormData$PROPDMGEXP %in% c("",NA, "-","?", "+"),]$PROPDMGEXP <- 0
stormData[stormData$PROPDMGEXP %in% c("H", "B"),]$PROPDMGEXP <- 2
stormData[stormData$PROPDMGEXP %in% "K",]$PROPDMGEXP <- 3
stormData[stormData$PROPDMGEXP %in% "M",]$PROPDMGEXP <- 6

stormData[stormData$CROPDMGEXP %in% c("",NA,"-","?", "+"),]$CROPDMGEXP <- 0
stormData[stormData$CROPDMGEXP %in% c("H", "B"),]$CROPDMGEXP <- 2
stormData[stormData$CROPDMGEXP %in% "K",]$CROPDMGEXP <- 3
stormData[stormData$CROPDMGEXP %in% "M",]$CROPDMGEXP <- 6

stormData$PROPDMG <- stormData$PROPDMG * 10 ** as.numeric(stormData$PROPDMGEXP)
stormData$CROPDMG <- stormData$CROPDMG * 10 ** as.numeric(stormData$CROPDMGEXP)

econ_loss <- ddply(stormData, .(EVTYPE), summarise,
                   PROP = sum(PROPDMG),
                   CROP = sum(CROPDMG))

econ_top10 <- head(econ_loss[order(econ_loss$PROP + econ_loss$CROP, decreasing=T),] , 10)
econ_top10_names <- econ_top10$EVTYPE


#Preparing the ggplot
property_p <- econ_top10[,1:2]
names(property_p) <- c("EVENT","DAMAGE")
property_p$TYPE <- "Property"

crop_p <- econ_top10[,c(1,3)]
names(crop_p) <- c("EVENT","DAMAGE")
crop_p$TYPE <- "Crop"

econ_plot <- rbind(property_p , crop_p)
econ_plot$EVENT <- factor(econ_plot$EVENT , levels = econ_top10_names)
econ_plot <- econ_plot[order(econ_plot$EVENT, decreasing=T),]

#---- Time series Data --------

stormTimeLine <- stormData[stormData$EVTYPE %in% econ_top10_names[1:3], c('EVTYPE','PROPDMG','CROPDMG','BGN_DATE' )]
#stormTimeLine$DATE <- as.Date(stormTimeLine$BGN_DATE, '%m/%d/%Y')
stormTimeLine$DMG <- stormTimeLine$PROPDMG + stormTimeLine$CROPDMG
stormTimeLine <- stormTimeLine[,c('EVTYPE','DMG','DATE')]


#Order by date
stormTimeLine <- stormTimeLine[order(stormTimeLine$DATE),]

#Delete irrelevant events
stormTimeLine <- subset(stormTimeLine, DMG != 0 )

type1_data <- stormTimeLine[stormTimeLine$EVTYPE == econ_top10_names[1],]
type2_data <- stormTimeLine[stormTimeLine$EVTYPE == econ_top10_names[2],]
type3_data <- stormTimeLine[stormTimeLine$EVTYPE == econ_top10_names[3],]

type1_data$CUMDMG <- cumsum(type1_data$DMG)
type2_data$CUMDMG <- cumsum(type2_data$DMG)
type3_data$CUMDMG <- cumsum(type3_data$DMG)

stormTimeLine <- rbind(type1_data[c('EVTYPE','CUMDMG','DATE')],
                       type2_data[c('EVTYPE','CUMDMG','DATE')],
                       type3_data[c('EVTYPE','CUMDMG','DATE')])




library(ggplot2)

#------------------------------------------

#Loading the Data
#path <- "C:/Users/Dani/DataScience/Reproducible Research/StormAnalysis/repdata-data-StormData.csv"
#path <- "./DataScience/repdata-data-StormData.csv"
#stormData <- read.csv(file = path)


library(plyr)
library(ggplot2)



#qplot(data = incidents, 
#      x=EVTYPE, y=cumsum(fatalities)) + geom_step



#Top 10 events that caused largest number of deaths are

fatalData[, c("EVTYPE", "fatalities")]

#Top 10 events that caused most number of injuries are

injurData[, c("EVTYPE", "injuries")]

#Processing the date for evaluating Economic effects

#Results

library(ggplot2)
library(gridExtra)



ggplot(health_plot, aes(x=EVENT, y=CASUALITIES, fill=TYPE)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = c("red", "orange")) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    ggtitle("Health Damage") +
    ylab("Casualities") +    
    xlab("Event")

ggplot(econ_plot, aes(x=EVENT, y=DAMAGE, fill=TYPE)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = c("dark green", "brown")) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    ggtitle("Economic losses") +
    ylab("Losses") +    
    xlab("Event")


ggplot(data = stormTimeLine, aes(x=DATE, y= CUMDMG/1000000000, color=EVTYPE)) + 
  geom_step(stat="identity") +
  ggtitle("Total loss evolution in XXI century") +
  ylab("Losses ($bn)") +    
  xlab("")

stormDots <- subset(stormData, EVTYPE %in% health_top10_names[1:5])
set.seed(69)
ggplot(data = stormDots, aes(x=FATALITIES+INJURIES, y=runif(length(FATALITIES), max=0.1))) + 
  geom_point(aes(colour=EVTYPE), size=3) +
  scale_y_continuous(limits = c(-1, 1))

ggplot(data = stormDots, aes(x=PROPDMG+CROPDMG, y=runif(length(FATALITIES), max=0.5))) + 
  geom_point(aes(colour=EVTYPE), size=3) +
  scale_y_continuous(limits = c(-1, 1))



