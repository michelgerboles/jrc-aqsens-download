#================================= =
#### Dependencies and Functions ####
#================================= =
#Set WD
# TODO set the path to the correct folder!
source("../../ec-jrc/airsenseur-calibration/Functions4ASE.R")
library(dplyr)
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
#======================== =
#### Config parameters ####
#======================== =
#
# PROXY
#
PROXY    <- FALSE
URL      <- NULL
PORT     <- NULL
LOGIN    <- NULL
PASSWORD <- NULL
#
# INFLUXDB
#
Host            <- "influxdb1.liberaintentio.com"
Port            <- 8086
User            <- "52North"
Pass            <- "JmaxNBTS"
Db              <- "jrcispra"
# Table in influxdb -> sensor to download
Dataset         <- "AirSensEUR16"
configFile      <- "./170604 ASE_R24 NO2B43F_COA4_OXA431_NOB4_Training2017.asc"
# Timezone in influxdb
Influx.TZ       <- "UTC"
#
# How many records per HTTP request to influxdb to be returned
# used in Down_Influx() in Functions4ASE.R:1618
# Hidden by INFLUXDB()
Page            <- 10000
#
# time average for the download of Influx data
#
Mean            <- 1
#
# OTHER
#
name.SQLite     <- "airsenseur.db"
name.SQLite.old <- "airsenseur.db.old"
# default = TRUE, if TRUE the google API is used for detecting time zone from coordinates (require port 443)
use_google      <- FALSE
#====================== =
#### Start of script ####
#====================== =
#
# Request METADATA
#
# checking if data are already downlaoded
# Influx.name := Name of for AirSensEUR in airsenseur.db, default Value NULL
# WDinput     := Sub directory of DisqueFieldtest where are the Refdata and InfluxData Rdata files
# UserMins    := periodicity of data requested after final data treatment
downloadStatus <- Check_Download(Influx.name = Dataset, WDinput = getwd(), UserMins = Mean)
# Configuration of electrochemical sensors:
sensorConfiguration <- ASEPanel04Read(ASEPanel04File = c(configFile))
#
# Download DATA
#
# Downloading data and saving files
INFLUXDB(WDoutput = getwd(), DownloadSensor = downloadStatus, UserMins = Mean,
         PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD, Down.Influx = TRUE,
         Host = Host  , Port = Port, User = User, Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
         Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, sens2ref = NULL, asc.File = sensorConfiguration)
