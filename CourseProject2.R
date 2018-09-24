## ----results='asis', echo=FALSE, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
#The R.Utils is required for the bunzip2 
library(R.utils)
library(dplyr)
library(mosaic)

## ------------------------------------------------------------------------
if(!file.exists("data")) {  
        dir.create("data")  
}  
if(!file.exists("./data/StormData.csv.bz2")) {  
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"  
        download.file(fileUrl, destfile="./data/StormData.csv.bz2")  
        if(!file.exists("./data/StormData.csv"))   
                bunzip2("./data/StormData.csv.bz2", "./data/StormData.csv", remove = FALSE, skip = TRUE) 
}
StormsData<-read.csv("./data/StormData.csv")

## ------------------------------------------------------------------------
dim(StormsData)

## ------------------------------------------------------------------------
names(StormsData)

## ------------------------------------------------------------------------
StormDF<-data.frame(Event.Type=StormsData$EVTYPE)

## ------------------------------------------------------------------------
StormDF<-cbind(StormDF,years = with(StormsData,format(as.Date(BGN_DATE, format="%m/%d/%Y"),"%Y")))

## ------------------------------------------------------------------------
Fatalities.No<-StormsData$FATALITIES
Injuries.No<-StormsData$INJURIES
StormDF<-cbind(StormDF,data.frame(Fatalities.No))
StormDF<-cbind(StormDF,data.frame(Injuries.No))

## ------------------------------------------------------------------------
#Copy the Two Damage Columns to the new DataFrame
StormDF$Damage.Cost<-StormsData$PROPDMG
StormDF$Damage.Factor<-StormsData$PROPDMGEXP
# Build a new Column with (K - 1000 , M - 1000000 , B - 1000000000)
#Any other value assume error and put zero 
StormDF<-StormDF %>%  
      mutate(Damage.Cost.Factor = as.numeric(case_when(
      Damage.Factor =="K" ~ 1000,  Damage.Factor =="M" ~ 1000000,
      Damage.Factor =="B" ~ 1000000000,(Damage.Factor !="M" |Damage.Factor !="B" | Damage.Factor !="K")~0)))
#REplace the Damage cost column with the the Damage.Cost * Damage.Cost.Factor
StormDF$Damage.Cost<-StormDF$Damage.Cost * StormDF$Damage.Cost.Factor
#view how table of the cost factor 
table(StormDF$Damage.Cost.Factor)
#remove the temporary columns
StormDF<-select(StormDF,Event.Type,years,Fatalities.No,Injuries.No,Damage.Cost)
head(StormDF)

## ------------------------------------------------------------------------
#Copy the Two CROP Damage Columns to the new DataFrame
StormDF$Crop.Damage.Cost<-StormsData$CROPDMG
StormDF$Crop.Damage.Factor<-StormsData$CROPDMGEXP
# Build a new Column with (K - 1000 , M - 1000000 , B - 1000000000)
#Any other value assume error and put zero 
StormDF<-StormDF %>%  
      mutate(Crop.Damage.Cost.Factor = as.numeric(case_when(
      Crop.Damage.Factor =="K" ~ 1000,  Crop.Damage.Factor =="M" ~ 1000000,
      Crop.Damage.Factor =="B" ~ 1000000000,(Crop.Damage.Factor !="M" |Crop.Damage.Factor !="B" | Crop.Damage.Factor !="K")~0)))
#REplace the Damage cost column with the the Damage.Cost * Damage.Cost.Factor
StormDF$Crop.Damage.Cost<-StormDF$Crop.Damage.Cost * StormDF$Crop.Damage.Cost.Factor
#view how table of the cost factor 
table(StormDF$Crop.Damage.Cost.Factor)
#remove the temporary columns
StormDF<-select(StormDF,Event.Type,years,Fatalities.No,Injuries.No,Property.Damage.Cost,Crop.Damage.Cost)
head(StormDF)

