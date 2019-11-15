#===================================
#### Dependencies and Functions ####
#===================================
#Set WD
setwd("/home/rstudio/import")
source("../app/Functions4ASE.R")
library(dplyr)
library(tidyverse)
library(data.table)
library(tidyverse)
library(data.table)
library(BMS)
library(DBI)
library(RSQLite)
library(devtools)
install_github("52north/sensorweb4R")
library(sensorweb4R)
install_github("rundel/timezone")
library(timezone)
library(lubridate)
library(reshape)
library(plyr)

#========================
#### Start of script ####
#========================

# Set config parameters
# PROXY parameters
PROXY    <- FALSE
URL      <- NULL
PORT     <- NULL
LOGIN    <- NULL
PASSWORD <- NULL

# INFLUXDB parameters
Host            <- "influxdb1.liberaintentio.com"
User            <- "52North"
Pass            <- "JmaxNBTS"
name.SQLite     <- "airsenseur.db"
name.SQLite.old <- "airsenseur.db.old"
Db              <- "jrcispra"
Dataset         <- "AirSensEUR16"
Influx.TZ       <- "UTC"
use_google      <- FALSE
Page            <- 10000
Mean            <- 10

# checking if data are already downlaoded
downloadStatus <- Check_Download(Influx.name = Dataset, WDinput = getwd(), UserMins = Mean)
# Configuration of electrochemical sensors:
sensorConfiguration         <- ASEPanel04Read(ASEPanel04File = c("./170604 ASE_R24 NO2B43F_COA4_OXA431_NOB4_Training2017.asc"))

# Downloading data and saving files
INFLUXDB(WDoutput = getwd(), DownloadSensor = downloadStatus, UserMins = Mean,
         PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD, Down.Influx = TRUE,
         Host = Host  , Port = 8086, User = User, Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
         Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, sens2ref = NULL, asc.File = sensorConfiguration)
