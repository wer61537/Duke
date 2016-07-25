#clear all objects
rm(list = ls())

#load libraries
library(dplyr)
library(ggplot2)

#helper function to trim leading and ending spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#get wd
wd<-trim(getwd())

#open the Rdata file that was previously unzipped
#load puts the Rdata into a dataframe named brfss2013
load(paste(wd,"/brfss2013.Rdata",sep=""))
names(brfss2013)

#limit the data to MN
brfss2013_MN<-filter(brfss2013, X_state == "Minnesota")
#get only the following 16 columns of interest
col_names=c(
  "physhlth",
  "menthlth",
  "poorhlth",
  "veteran3",
  "marital",
  "children",
  "educa",
  "employ1",
  "X_incomg",
  "sex",
  "X_race",
  "seatbelt",
  "flushot6",
  "flshtmy2",
  "tetanus",
  "pneuvac3"
    )

#pull out the columns of interest
brfss2013_MN2<-brfss2013_MN[, col_names]
