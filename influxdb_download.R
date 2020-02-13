files <- list("Functions4ASE", "151016 Sensor_Toolbox", "global")
for (file in files) {
    file <- paste0(file, ".R")

    if (file.exists(file)) {
        unlink(file)
        futile.logger::flog.info(paste0("Deleted previous version of '", file, "'"))
    }
    returnCode <- utils::download.file(
        url = paste0("https://raw.githubusercontent.com/ec-jrc/airsenseur-calibration/master/", URLencode(file)),
        destfile = file,
        method = "auto"
    )
    if (returnCode != 0L) {
        stop(paste0("\n\n\t\tCould not download latest version of '", file, "'\n\n"))
    }
    futile.logger::flog.info(paste0("Downloaded latest version of '", file, "'"))
}
rm(files)
library(raster)
rootWorkingDirectoy <- "/home/rstudio/aqsens/jrc-aqsens-download"
setwd(rootWorkingDirectoy)

#   1.d Loading packages (global.R)
source("global.R")
source("Functions4ASE.R")

############### at Eike: you should select here boxName in the way you prefer ###############################################
# AirSensEUR name: The 1st one selected in the list of configured AirSensEURs
boxName      <- "40458D" # <-- will be set by environment in the final version
boxDirectory <- file.path(rootWorkingDirectoy, "ASE_Boxes", boxName)
subDirData   <- "General_data"
subDirConfig <- "Configuration"
subDirModels <- "Models"

# Defining Initial values ----
DT.NULL    <- FALSE
DT.General <- NULL
Influx     <- NULL
Sos        <- NULL
Ref        <- NULL

General.file            <- file.path(boxDirectory, subDirData, "General.csv")
InfluxData.file         <- file.path(boxDirectory, subDirData, "InfluxData.csv")
SOSData.file            <- file.path(boxDirectory, subDirData, "SOSData.csv")
RefData.file            <- file.path(boxDirectory, subDirData, "RefData.csv")
ind.warm.file           <- file.path(boxDirectory, subDirData, "ind_warm.RDS")
ind.TRh.file            <- file.path(boxDirectory, subDirData, "ind_TRh.RDS"  )
ind.Invalid.file        <- file.path(boxDirectory, subDirData, "ind_Invalid.RDS")
ind.sens.out.file       <- file.path(boxDirectory, subDirData, "ind_sens_out.RDS")
ind.ref.out.file        <- file.path(boxDirectory, subDirData, "ind_ref_out.RDS")
# cfg_file     : The cfg file  of the Selected AirSensEUR "
cfg_file           <- file.path(boxDirectory, subDirConfig, paste0(boxName,".cfg"))
SETTIME_file       <- file.path(boxDirectory, subDirConfig, paste0(boxName,"_SETTIME.cfg"))
Servers_file       <- file.path(boxDirectory, subDirConfig, paste0(boxName,"_Servers.cfg"))

# Configuration and reading of data
Config <- CONFIG(DisqueFieldtestDir = boxDirectory,
                 DisqueFieldtest = rootWorkingDirectoy,
                 shiny = FALSE)
# Returning a list with 4 elements see below
# Config[["Server"]]   : server parameters
# Config[["sens2ref"]] : cfg parameters
# Config[["CovPlot"]]  : covariates for plotting
# Config[["CovMod"]]   : covariates for calibrating
# initial data
if (file.exists(InfluxData.file)) {
    if (extension(InfluxData.file) == ".csv") {
        Influx <- fread(file = InfluxData.file, na.strings = c("","NA", "<NA>"))
        if (!"" %in% Config$Server$Influx.TZ) {
            data.table::set(Influx, j = "date", value =  ymd_hms(Influx[["date"]], tz = Config$Server$Influx.TZ))
        } else data.table::set(Influx, j = "date", value =  ymd_hms(Influx[["date"]], tz = "UTC"))
    } else if (extension(InfluxData.file) == ".Rdata") {
        Influx <- load_obj(InfluxData.file)
        if (!"data.table" %in% class(Influx)) Influx <- data.table(Influx, key = "date")
    }
    if ("V1" %in% names(Influx)) Influx[, V1 := NULL]
}
if (file.exists(SOSData.file)) {
    if (extension(SOSData.file) == ".csv") {
        Sos <- fread(file = SOSData.file, na.strings = c("","NA", "<NA>"))
        if (!"" %in% Config$Server$SOS.TZ) {
            data.table::set(Sos, j = "date", value =  ymd_hms(Sos[["date"]], tz = Config$Server$SOS.TZ))
        } else data.table::set(Sos, j = "date", value =  ymd_hms(Sos[["date"]], tz = "UTC"))
    } else if (extension(SOSData.file) == ".Rdata") {
        Sos <- load_obj(SOSData.file)
        if (!"data.table" %in% class(Sos)) Sos <- data.table(Sos, key = "date")
    }
    if ("V1" %in% names(Sos)) Sos[, V1 := NULL]
}
if (file.exists(RefData.file)) {
    if (extension(RefData.file) == ".csv") {
        Ref <- fread(file = RefData.file, na.strings = c("","NA", "<NA>"))
        if (!"" %in% Config$Server$ref.tzone) {
            data.table::set(Ref, j = "date", value =  ymd_hms(Ref[["date"]], tz = Config$Server$ref.tzone))
        } else data.table::set(Ref, j = "date", value =  ymd_hms(Ref[["date"]], tz = "UTC"))
    } else if (extension(SOSData.file) == ".Rdata") {
        Ref <- load_obj(RefData.file)
        if (!"data.table" %in% class(Ref)) Ref <- data.table(Ref, key = "date")
    }
    if ("V1" %in% names(Ref)) Ref[, V1 := NULL]
    # Message in case coordinates are not included
    if (!all(c("Ref.Long",  "Ref.Lat") %in% names(Ref))) cat("ERROR Coordinates of reference station missing")
}
if (file.exists(General.file)) {
    if (extension(General.file) == ".csv") {
        DT.General <- data.table::fread(General.file) #, na.strings = getOption("","NA")
        # Convert date to POSIXct
        if (!all("" %in% c(Config$Server$Influx.TZ, Config$Server$SOS.TZ))) {
            if (!"" %in% c(Config$Server$Influx.TZ)) {
                data.table::set(DT.General, j = "date"         , value =  ymd_hms(DT.General[["date"]]         , tz = Config$Server$Influx.TZ))
                data.table::set(DT.General, j = "date_PreDelay", value =  ymd_hms(DT.General[["date_PreDelay"]], tz = Config$Server$Influx.TZ))
            } else {
                data.table::set(DT.General, j = "date"         , value =  ymd_hms(DT.General[["date"]]         , tz = Config$Server$SOS.TZ))
                data.table::set(DT.General, j = "date_PreDelay", value =  ymd_hms(DT.General[["date_PreDelay"]], tz = Config$Server$SOS.TZ))
            }
        } else {
            data.table::set(DT.General, j = "date"         , value =  ymd_hms(DT.General[["date"]]         , tz = "UTC"))
            data.table::set(DT.General, j = "date_PreDelay", value =  ymd_hms(DT.General[["date_PreDelay"]], tz = "UTC"))
        }
    } else if (extension(General.file) == ".Rdata") {
        DT.General <- load_obj(General.file)
        if (!"data.table" %in% class(DT.General)) DT.General <- data.table(DT.General, key = "date")
    }
    if ("V1" %in% names(DT.General)) DT.General[, V1 := NULL]
    # Checking that dew point deficit and absolute humidity are included for old download of AirSensEUR boxes
    # adding absolute humidity is relative humidity and temperature exist
    if (!all(c("Absolute_humidity", "Td_deficit") %in% names(DT.General))) {
        if (all(c("Temperature", "Relative_humidity") %in% names(DT.General))) {
            DT.General$Absolute_humidity <- NA_real_
            DT.General$Td_deficit        <- NA_real_
            both.Temp.Hum <- complete.cases(DT.General[, c("Temperature", "Relative_humidity")])
            DT.General[both.Temp.Hum, "Absolute_humidity"] <- threadr::absolute_humidity(DT.General[["Temperature"]][both.Temp.Hum], DT.General[["Relative_humidity"]][both.Temp.Hum])
            Td <- weathermetrics::humidity.to.dewpoint(rh = DT.General[["Relative_humidity"]][both.Temp.Hum], t = DT.General[["Temperature"]][both.Temp.Hum], temperature.metric = "celsius")
            DT.General[both.Temp.Hum, Td_deficit := DT.General[both.Temp.Hum, "Temperature"] - Td]
        }
    }
    if (!all(c("Ref.Absolute_humidity", "Ref.Td_deficit") %in% names(DT.General))) {
        if (all(c("Ref.Temp", "Ref.RH") %in% names(DT.General))) {
            DT.General$Ref.Absolute_humidity <- NA_real_
            DT.General$Ref.Td_deficit        <- NA_real_
            Ref.both.Temp.Hum <- complete.cases(DT.General[, c("Ref.Temp", "Ref.RH")])
            DT.General[Ref.both.Temp.Hum, "Ref.Absolute_humidity"] <- threadr::absolute_humidity(DT.General[["Ref.Temp"]][Ref.both.Temp.Hum], DT.General[["Ref.RH"]])
            Td <- weathermetrics::humidity.to.dewpoint(rh = DT.General[Ref.both.Temp.Hum, "Ref.RH"], t = DT.General[["Ref.Temp"]][Ref.both.Temp.Hum], temperature.metric = "celsius")
            DT.General[Ref.both.Temp.Hum, Ref.Td_deficit := DT.General[["Ref.Temp"]][Ref.both.Temp.Hum] - Td]
        }
    }

    # se c'e' General.Rdata ma alcuni sensori o referenze di Ref e Influx non sono in DT.General le si combina
    if (!all(c(names(Ref)[grep(pattern = paste(c("Bin.", "boardTimeStamp", "gpsTimestamp"), collapse = "|"),
                               x = names(Ref), invert = T)], names(Influx)) %in% names(DT.General)) ) {
        DT.General   <- NULL
        DT.NULL <- TRUE
    }
} else {
    DT.General   <- NULL
    DT.NULL <- TRUE
}
# Initial DownloadSensor
Download.Sensor <- Check_Download(Influx.name = Config$Server$Dataset,
                                  WDinput     = file.path(boxDirectory, subDirData),
                                  UserMins    = if (!is.null(Config$Server$UserMins)) Config$Server$UserMins else Config$Server$UserMins,
                                  General.df  = if (!is.null(DT.General))  DT.General else NA,
                                  RefData     = if (!is.null(Ref))    Ref else NA,
                                  InfluxData  = if (!is.null(Influx)) Influx else NA,
                                  SOSData     = if (!is.null(Sos))    Sos else NA)
# Initial SetTime
Set.Time        <- SETTIME(DisqueFieldtestDir = boxDirectory,
                           General.t.Valid    = DT.General,
                           Influx.TZ          = Config[["Server"]]$Influx.TZ,
                           SOS.TZ             = Config[["Server"]]$SOS.TZ,
                           Ref.TZ             = Config[["Server"]]$ref.tzone,
                           DownloadSensor     = Download.Sensor,
                           Config             = Config,
                           sens2ref.shield    = Config$sens2ref.shield,
                           shiny              = FALSE)
# Indexes of data discarded during warming time of sensors
Warm.Forced <- FALSE
if (file.exists(ind.warm.file)) ind.warm.out <- list.load(ind.warm.file) else {
    ind.warm.out <- NULL
    Warm.Forced   <- TRUE
}
# Indexes of data discarded outside temperature and humidity tolerance
TRh.Forced <- FALSE
if (file.exists(ind.TRh.file)) ind.TRh.out <- list.load(ind.TRh.file) else {
    ind.TRh.out <- NULL
    TRh.Forced   <- TRUE
}
# Flagging the sensor data for Invalid sensor data
Inv.Forced <- FALSE
if (file.exists(ind.Invalid.file)) ind.Invalid.out <- list.load(ind.Invalid.file) else {
    ind.Invalid.out <- NULL
    Inv.Forced <- TRUE
}
# Indexes of outliers for sensor data
Outliers.Sens.Forced <- FALSE
if (file.exists(ind.sens.out.file)) ind.sens.out <- list.load(ind.sens.out.file) else {
    ind.sens.out <- NULL
    Outliers.Sens.Forced <- TRUE
}
# Indexes of outliers for sensor data
Outliers.Ref.Forced <- FALSE
Neg.Forced <- FALSE
if (file.exists(ind.ref.out.file) && file.size(ind.ref.out.file) > 0) ind.ref.out <- list.load(ind.ref.out.file) else {
    ind.ref.out <- NULL
    Outliers.Ref.Forced <- TRUE
}
# Initialising for conversion and calibration
Conv.Forced <- FALSE
Cal.Forced  <- FALSE

# Reactive i.sensors Once AirSensEUR is Selected
# Returning the indexes of valid sensors in boxName.cfg taking into account NAs
# avoid na and names of sensor in the checmical shield
i.sensors          <- which(!is.na(Set.Time$name.gas) & Set.Time$name.gas %in% Config$sens2ref$name.gas)
list.gas.sensor       <- Config[["sens2ref"]]$gas.sensor[!is.na(Config[["sens2ref"]]$gas.sensor) &
                                                             Config[["sens2ref"]]$gas.sensor  != "" &
                                                             Config[["sens2ref"]]$name.sensor %in% Config$sens2ref.shield$name.sensor]
list.name.sensor   <- Config[["sens2ref"]]$name.sensor[!is.na(Config[["sens2ref"]]$name.sensor) &
                                                           Config[["sens2ref"]]$name.sensor != "" &
                                                           Config[["sens2ref"]]$name.sensor %in% Config$sens2ref.shield$name.sensor]
list.name.gas      <- Config[["sens2ref"]]$name.gas[!is.na(Config[["sens2ref"]]$name.sensor) &
                                                        Config[["sens2ref"]]$name.sensor != "" &
                                                        Config[["sens2ref"]]$name.sensor %in% Config$sens2ref.shield$name.sensor]
# return a vector with the names of gas reference using the file ASE.cfg
list.gas.reference2use <- Config[["sens2ref"]]$gas.reference2use[!is.na(Config[["sens2ref"]]$gas.reference2use) &
                                                                     Config[["sens2ref"]]$gas.reference2use != "" &
                                                                     Config[["sens2ref"]]$gas.sensor %in% list.gas.sensor]
# return a vector with the names of gas reference using the file ASE.cfg
list.gas.reference <- Config[["sens2ref"]]$gas.reference[!is.na(Config[["sens2ref"]]$gas.reference) &
                                                             Config[["sens2ref"]]$gas.reference != "" &
                                                             Config[["sens2ref"]]$gas.sensor %in% list.gas.sensor]

# setting the current directory to the root of the file system with the name of the AirSensEUR
setwd(boxDirectory)
# InfluxDB ----
# var.names.meteo     <- INFLUX[[2]]
# var.name.GasSensors <- INFLUX[[3]]
# var.names.sens      <- INFLUX[[4]]
# InfluxData          <- INFLUX[[1]]
if (!is.null(Influx)) InfluxData <- Influx[] else InfluxData <- NA_real_
INFLUX <- INFLUXDB(
    WDoutput        = file.path(boxDirectory,subDirData),
    DownloadSensor  = Download.Sensor,
    UserMins        = Config$Server$UserMins,
    PROXY           = Config$Server$PROXY,
    URL             = Config$Server$URL,
    PORT            = Config$Server$PORT,
    LOGIN           = Config$Server$LOGIN,
    PASSWORD        = Config$Server$PASSWORD,
    Down.Influx     = TRUE,
    Host            = Config$Server$Host,
    Port            = Config$Server$Port,
    User            = Config$Server$User,
    Pass            = Config$Server$Pass,
    Db              = Config$Server$Db,
    Dataset         = Config$Server$Dataset,
    Influx.TZ       = Config$Server$Influx.TZ,
    name.SQLite     = file.path(boxDirectory,subDirData,"airsenseur.db"),
    name.SQLite.old = file.path(boxDirectory,subDirData,"airsenseur_old.db"),
    sens2ref        = Config[["sens2ref"]],
    InfluxData      = InfluxData)
Influx          <- INFLUX[[1]]
var.names.meteo <- INFLUX[[2]]
rm(INFLUX, InfluxData)
# updating Download.Sensor with new INflux data
Download.Sensor <- Check_Download(Influx.name = Config$Server$Dataset,
                                  WDinput     = file.path(boxDirectory, subDirData),
                                  UserMins    = if (!is.null(Config$Server$UserMins)) Config$Server$UserMins else 1,
                                  General.df  = if (!is.null(DT.General))  DT.General else NA,
                                  RefData     = if (!is.null(Ref))    Ref else NA,
                                  InfluxData  = if (!is.null(Influx)) Influx else NA,
                                  SOSData     = if (!is.null(Sos))    Sos else NA)

# REFDATA
# Checking if there are several ftp url
if (any(grepl(pattern = ",", x = Config$Server$urlref))) urlref = unlist(strsplit(gsub(pattern = " ","",x = Config$Server$urlref), split = ",")  ) else urlref = gsub(pattern = " ","",x = Config$Server$urlref)
# Setting REFDATA
if (!is.null(Ref)) RefData <- Ref else RefData <- NA_real_
if (Download.Sensor$ExistFil.data.Ref) {
    Ref.start <- as.Date(Download.Sensor$DateEND.Ref.prev, format = "%Y-%m-%d")
} else {
    if (Download.Sensor$ExistFil.data.Influx) {
        Ref.start <- as.Date(Download.Sensor$DateEND.Influx.prev, format = "%Y-%m-%d")
    } else {
        Ref.start <- as.Date(format(Sys.time(), tz = Config$Server$ref.tzone))
    }
}
# Set Down.Ref to FALSE if you do not want to download reference data
# REFDATA <- REF(DownloadSensor     = Download.Sensor,
#                AirSensEur.name    = Config$Server$AirSensEur.name,
#                DisqueFieldtestDir = boxDirectory,
#                UserMins           = Config$Server$UserMins,
#                Down.Ref           = TRUE,
#                ref.tzone          = Config$Server$ref.tzone,
#                InfluxData         = Influx,
#                SOSData            = Sos,
#                Reference.name     = Config$Server$Reference.name,
#                urlref             = urlref,
#                sens2ref           = Config$all[["sens2ref"]],
#                FTPMode            = Config$Server$FTPMode,
#                Ref.SOS.name       = Config$Server$Ref.SOS.name,
#                RefSOSname         = Config$Server$RefSOSname,
#                RefSOSDateIN       = Ref.start,
#                RefSOSDateEND      = as.Date(format(Sys.time(), tz = Config$Server$ref.tzone)) + 1,
#                Ref__a_i_p__name         = Config$Server$Ref__a_i_p__name,
#                User__a_i_p__            = Config$Server$User__a_i_p__,
#                Pass__a_i_p__            = Config$Server$Pass__a_i_p__,
#                Ref__a_i_p__Organisation = Config$Server$Ref__a_i_p__Organisation,
#                Ref__a_i_p__Station      = Config$Server$Ref__a_i_p__Station,
#                Ref__a_i_p__Pollutants   = "Sample_air!temperature!ozone!carbon monoxide!nitrogen oxides!nitrogen dioxide!sulfur dioxide!nitrogen monoxide",
#                Ref__a_i_p__DateIN       = Ref.start,
#                Ref__a_i_p__DateEND      = as.Date(format(Sys.time(), tz = Config$Server$ref.tzone)) + 1,
#                csvFile            = Config$Server$file1,
#                csvFile.sep        = Config$Server$sep,
#                csvFile.quote      = Config$Server$quote,
#                coord.ref          = Config$Server$coord.ref,
#                Ref.Type           = Config$Server$Ref.Type,
#                RefData            = RefData,
#                shiny              = FALSE)

# updating Ref
if (exists("REFDATA") && !is.null(REFDATA[[1]])) {
    Ref <- REFDATA[[1]]
    Download.Sensor <- Check_Download(Influx.name = Config$Server$Dataset,
                                      WDinput     = file.path(boxDirectory, subDirData),
                                      UserMins    = if (!is.null(Config$Server$UserMins)) Config$Server$UserMins else 1,
                                      General.df  = if (!is.null(DT.General))  DT.General else NA,
                                      RefData     = if (!is.null(Ref))    Ref else NA,
                                      InfluxData  = if (!is.null(Influx)) Influx else NA,
                                      SOSData     = if (!is.null(Sos))    Sos else NA)
    Outliers.Ref.Forced <- TRUE
    rm(REFDATA, RefData)
} else {
    Ref <- NULL
}

# Creating General Data
# Checking that parameters for sensor download are complete or that they are new data
if (DT.NULL ||
    isTRUE(Download.Sensor$DateEND.General.prev < Download.Sensor$DateEND.Ref.prev) ||
    isTRUE(Download.Sensor$DateEND.General.prev < Download.Sensor$DateEND.Influx.prev)) {

    # getting what it would be to put sensor and reference data together to later compare with what is in DT.General
    D <- GENERAL(WDoutput            = file.path(boxDirectory, subDirData),
                 UserMins            = Config$Server$UserMins,
                 Delay               = Config$Server$Delay,
                 RefData             = Ref,
                 InfluxData          = Influx,
                 SOSData             = Sos,
                 var.name.GasSensors = list.gas.sensor  ,
                 DownloadSensor      = Download.Sensor,
                 Change.Delay        = FALSE,
                 Change.UserMins     = FALSE)
    # saving New General data if needed
    save.General.df <- FALSE
    if (is.null(DT.General) || nrow(DT.General) == 0 ||
        (!is.null(DT.General) && !isTRUE(all.equal(D[,.SD, .SDcols = intersect(names(D),names(DT.General))],
                                                   DT.General[,.SD, .SDcols = intersect(names(D),names(DT.General))],
                                                   , check.attributes = FALSE)))) {
        save.General.df <- TRUE
        DT.General <- D
    }
    if (save.General.df) {
        # Saving downloaded data in General_data Files
        # replacing nan by NA before saving
        Cols.for.Avg <- names(DT.General)[-which(names(DT.General) == "date")]
        data.table::set(DT.General, j = Cols.for.Avg, value = lapply(DT.General[,..Cols.for.Avg], nan.to.na))
        # saving New General data
        if (extension(General.file) == ".csv") {
            fwrite(DT.General, file = General.file, na = "NA")
        } else if (extension(General.file) == ".Rdata") save(DT.General, file = General.file)
        # if general is saved, it is necessary to run the detection of warming, T/RH out of tolerance, Negative Ref., Invalids and outlier detection, sensor data conversion and calibration.
        # It is sufficient to set to TRUE to run ind.warm then in.TRH ...
        Warm.Forced          <- TRUE
        Outliers.Sens.Forced <- TRUE
        # Updating Download.Sensor
        Download.Sensor <- Check_Download(Influx.name = Config$Server$Dataset,
                                          WDinput     = file.path(boxDirectory, subDirData),
                                          UserMins    = if (!is.null(Config$Server$UserMins)) Config$Server$UserMins else 1,
                                          General.df  = if (!is.null(DT.General))  DT.General else NA,
                                          RefData     = if (!is.null(Ref))    Ref else NA,
                                          InfluxData  = if (!is.null(Influx)) Influx else NA,
                                          SOSData     = if (!is.null(Sos))    Sos else NA)
    }
    rm(D)
}

# Running filtering if needed
# Flagging the sensor data for warming time
# This dataTreatment can only works if boardTimeStamp exists, meaning only in InfluxData. It will not work with SOSData
# output: a list of 4 character vectors, corresponding to sensors with row index of DT.General corresponding to warming time of sensor data,
#       the names of the 4 elements are the ones of list.gas.sensor   in the same order
if (Warm.Forced) {
    # Create a Progress object
    # setting index for warming
    if (!is.null(DT.General[,"boardTimeStamp"]) ) { # use to be class(DT.General) == "data.frame"
        # replace everythin boardTimeStamp which does not change with NA so na.locf will works
        # Index of boardtimeStamp similar for consecutive boardtimeStamp
        Consecutive <- which(diff(DT.General$boardTimeStamp, lag = 1) == 0)
        # Values of indexes whith previous values that are non consecutive (Re-start)
        Re_start <- Consecutive[diff(Consecutive, lag = 1) > 1]
        # Setting NA boardTimeStamp to the last non-NA boardTimeStamp
        data.table::set(DT.General,  j = "boardTimeStamp", value = na.locf(DT.General[["boardTimeStamp"]], na.rm = FALSE, fromLast = FALSE))
        # detecting when boardTimeStamp decreases suddenly (re-boot)
        Re_boot <- which(diff(DT.General$boardTimeStamp, lag = 1) < 0)
        # Combining Re_start and reboot
        ind <- unique(c(Re_start, Re_boot))
    } else {
        # This is for SOS
        ind <- apply(DT.General[, list.gas.sensor, with = FALSE  ], 1, function(i) !all(is.na(i)))
        ind <- which(ind[2:length(ind)] & !ind[1:(length(ind) - 1 )])
    }
    ind = ind + 1
    # Adding the first switch-on
    ind <- c(1,ind)
    for (n in seq_along(list.gas.sensor)) {
        indfull <- integer(length(ind)* Config$sens2ref$hoursWarming[n] * 60 / as.integer(Config$Server$UserMins))
        # developing IndFull
        for (i in seq_along(ind)) {
            indfull[((i - 1) *  Config$sens2ref$hoursWarming[n]*60/as.integer(Config$Server$UserMins) + 1):((i)* Config$sens2ref$hoursWarming[n]*60 / as.integer(Config$Server$UserMins))] <- ind[i]:(ind[i] +  Config$sens2ref$hoursWarming[n] * 60 / as.integer(Config$Server$UserMins) - 1)
        }
        # removing  indexes outside the number of rows of DT.General
        indfull <- indfull[indfull <= length(DT.General[[list.gas.sensor[n]]])]
        if (exists("return.ind.warm")) return.ind.warm[[n]] <- indfull else return.ind.warm <- list(indfull)
    }
    names(return.ind.warm) <- list.gas.sensor
    ind.warm.out <- return.ind.warm
    # Setting TRh.Forced to TRUE to be sure that it is done before ind.Sens
    TRh.Forced <- TRUE
    rm(return.ind.warm, ind)
}

# Flagging the sensor data for temperature and humidity outside interval of tolerance
# Output:                : list of NAs for discarded temperature and humidity with as many elements as in list.gas.sensor
#                          consisting of vector of integers of the index of rows of DT.General dataframe
if (TRh.Forced) {
    # Always starting detection of outleirs for T and RH from the dataframe set in DT.General
    index.temp <- which(colnames(DT.General) %in% "Temperature")
    index.rh   <- which(colnames(DT.General) %in% "Relative_humidity")
    return.ind.TRh    <- list()
    return.ind.T.min  <- list()
    return.ind.T.max  <- list()
    return.ind.Rh.min <- list()
    return.ind.Rh.max <- list()
    for (l in list.gas.sensor) {
        # Global index of temperature/humidity exceeding thresholds
        ind <- (DT.General[, index.temp, with = FALSE]   < Config$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)]  |
                    DT.General[, index.temp, with = FALSE]   > Config$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)]) |
            (DT.General[, index.temp, with = FALSE]   < Config$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)]  |
                 DT.General[, index.temp, with = FALSE]   > Config$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)])
        # Global index of temperature/humidity exceeding thresholds
        T.min  <- DT.General[, index.temp, with = FALSE] < Config$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)]
        T.max  <- DT.General[, index.temp, with = FALSE] > Config$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)]
        Rh.min <- DT.General[, index.rh, with = FALSE]   < Config$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)]
        Rh.max <- DT.General[, index.rh, with = FALSE]   > Config$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)]
        # if (exists("return.ind.TRh")) {
        return.ind.TRh[[Config[["sens2ref"]][which(gas.sensor == l), name.sensor]]] <- which(ind)
        return.ind.T.min[[ paste0(Config[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. < ",Config$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(T.min)
        return.ind.T.max[[ paste0(Config[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. > ",Config$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(T.max)
        return.ind.Rh.min[[paste0(Config[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH < "   ,Config$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(Rh.min)
        return.ind.Rh.max[[paste0(Config[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH > "   ,Config$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(Rh.max)
    }
    ind.TRh.out <- list(ind.TRh = return.ind.TRh, T.min = return.ind.T.min, T.max = return.ind.T.max, Rh.min = return.ind.Rh.min, Rh.max = return.ind.Rh.max)
    # Setting Invalid$Forced to True to be sure that it is carried out before ind.sens
    Inv.Forced <- TRUE
    rm(return.ind.TRh, return.ind.T.min, return.ind.T.max, return.ind.Rh.min, return.ind.Rh.max, T.min,T.max,Rh.min,Rh.max)
}

# output: a list of 4 character vectors, corresponding to sensors with row index of DT.General corresponding to Invalid sensor data,
#       the names of the 4 elements are the ones of list.gas.sensor   in the same order
# min.General.date and max.General.date----
if (!is.null(DT.General)) min.General.date <- min(DT.General$date, na.rm = TRUE) else min.General.date <- NULL
if (!is.null(DT.General)) max.General.date <- max(DT.General$date, na.rm = TRUE) else max.General.date <- NULL
if (Inv.Forced) {
    if (!is.null(DT.General)) {
        # reading the files with period of valid data
        for (i in seq_along(list.name.sensor)) {
            nameFile <- file.path(boxDirectory,subDirData,paste0(boxName,"_Valid_",list.name.sensor[i],".cfg"))
            if (file.exists(nameFile)) {
                assign(paste0("Valid_",list.name.sensor[i]), read.table(file = nameFile, header = TRUE, row.names = NULL, comment.char = "#", stringsAsFactors = FALSE))
            } else {
                # There are no Valid files. Creates files with IN = END = min(General$date)
                assign(paste0("Valid_",list.name.sensor[i]), rbind(c(strftime(min(DT.General$date, na.rm = TRUE)), strftime(min(DT.General$date, na.rm = TRUE)))))
                write.table(x         = data.frame(In = gsub(" UTC", "",strftime(min.General.date)),
                                                   End = gsub(" UTC", "",strftime(min.General.date)),
                                                   stringsAsFactors = FALSE),
                            file      = nameFile,
                            row.names = FALSE
                )
            }
        }
        # Creating one list with invalid periods for all sensors
        Valid <- list()
        for (i in paste0("Valid_",list.name.sensor)) Valid[[i]] <- get(i)
        # Function to convert charater strings to POSIX
        NewValid <- function(x) {
            # making each element a dataframe of POSIXct
            x <- data.frame( x, stringsAsFactors = FALSE)
            colnames(x) <- c("In", "End")
            x$In  <- parse_date_time(x$In , tz = threadr::time_zone(DT.General$date[1]), orders = "YmdHMS")
            x$End <- parse_date_time(x$End, tz = threadr::time_zone(DT.General$date[1]), orders = "YmdHMS")
            return(x)
        }
        Valid.date <- lapply(Valid, NewValid)
        # Set inital date for data retrieving (i.e. maximum length time period for data retrieval).
        # These dates may change according to the data availability
        # UserDateIN.0, SOS.TZ is set in ASEConfig_MG.R
        # Set correct time zone
        # list of invalid date to be used for sensor Evaluation with reference values, a kind of life cycle of each sensor - time zone shall be the same as SOS.TZ (UTC?)
        # seting invalid to NA and create a list for plotting invalids
        ind.Inval <- list()
        for (i in gsub(pattern = "Valid_", replacement = "", names(Valid.date))) {
            for (j in 1:nrow(Valid.date[[paste0("Valid_",i)]])) {
                Valid.interval.j <- which(DT.General$date %within% interval(Valid.date[[paste0("Valid_",i)]]$In[j], Valid.date[[paste0("Valid_",i)]]$End[j]))
                if (length(Valid.interval.j) > 0) {
                    if (!(i %in% names(ind.Inval))) {
                        ind.Inval[[i]] <- DT.General$date[Valid.interval.j]
                    } else {
                        ind.Inval[[i]] <- c(ind.Inval[[i]], DT.General$date[Valid.interval.j])
                    }
                }
            }
        }
    }

    ind.Invalid.out <- list(Valid.date,ind.Inval)
    # make sure that Outliers.Sens$Forced is run after Invalid, to discard outliers again and to apply invalid and outliers to DT.General
    Outliers.Sens.Forced <- TRUE
    rm(Valid.date,ind.Inval)
    rm(Valid)
}
for (i in list.name.sensor) assign(paste0("Valid_",i),NULL)

# discard outliers of sensors
if (Outliers.Sens.Forced) {
    if (!is.null(DT.General)) setalloccol(DT.General)
    for (i in list.gas.sensor) {
        # Checking if sensor data exists in DT.General
        if (i %in% names(DT.General)) {
            # Initialisation of columns of DT.General
            Sensor.i <- na.omit(Config[["sens2ref"]][[which(Config[["sens2ref"]][,"gas.sensor"] == i),"name.sensor"]])
            # resetting to initial values
            Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
            set(DT.General, j = Vector.columns, value = rep(list(DT.General[[i]]), times = length(Vector.columns)))
            if (!is.null(ind.warm.out[i][[1]])) {
                Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                i.Rows <- as.integer(ind.warm.out[[i]])
                set(DT.General, i = i.Rows, j = Vector.columns, value = rep(list(rep(NA, times = length(i.Rows))), times = length(Vector.columns)))
            }
            if (!is.null(ind.TRh.out$ind.TR[[Sensor.i]]) && length(ind.TRh.out$ind.TR[[Sensor.i]]) > 0) {
                Vector.columns <- paste0(c("Out.", "Out.TRh.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                set(DT.General,i = ind.TRh.out$ind.TRh[Sensor.i][[1]], j = Vector.columns,
                    value = rep(list(rep(NA, times = length(ind.TRh.out$ind.TRh[Sensor.i][[1]]))), times = length(Vector.columns)))
            }
            if (!is.null(ind.Invalid.out[[2]][[Sensor.i]]) && length(ind.Invalid.out[[2]][[Sensor.i]]) > 0) {
                Vector.columns <- paste0(c("Out.", "Out.Invalid." , "Out.Warm.TRh.Inv."),i)
                set(DT.General,i = which(DT.General$date %in% ind.Invalid.out[[2]][[Sensor.i]]), j = Vector.columns,
                    value = rep(list(rep(NA, times = length(which(DT.General$date %in% ind.Invalid.out[[2]][[Sensor.i]])))), times = length(Vector.columns)))
            }
            # index (1, 2,3, 4  or 1,2,3, 6 ... comng from  selection of control uiFiltering, Calib and SetTime)
            k <- match(x = i, table = list.gas.sensor)
            set(DT.General, j = paste0("Out.",i,".",1:Config$sens2ref$Sens.iterations[k]),
                value = rep(list(DT.General[[paste0("Out.",i)]]), times = Config$sens2ref$Sens.iterations[k]))
            # deleting bigger iterations
            j  <- Config$sens2ref$Sens.iterations[k]
            repeat (
                if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DT.General)))) {
                    set(DT.General, j = paste0("Out.",i,".",j + 1), value = NULL)
                    j <- j + 1
                } else break # leaving the Repeat if there are no higher iterations
            )
            if (Config$sens2ref$Sens.Inval.Out[k]) {
                for (j in 1:Config$sens2ref$Sens.iterations[k]) { # number of iterations
                    if (all(is.na(DT.General[[i]]))) {
                    } else {
                        # Setting the columns of sensor data previous to detect outliers
                        Y <- DT.General[[paste0("Out.Warm.TRh.Inv.",i)]]
                        # setting Y for the outliers of previous iterations to NA. If null then stop outlier detection
                        if (j > 1) {
                            if (length(which(return.ind.sens.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                if (class(Y)[1] == "tbl_df") {
                                    Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers))))),] <- NA
                                } else Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers)))))] <- NA
                            } else break
                        }
                        Outli <- My.rm.Outliers(ymin         = Config$sens2ref$Sens.Ymin[k],
                                                ymax         = Config$sens2ref$Sens.Ymax[k],
                                                ThresholdMin = Config$sens2ref$Sens.threshold[k],
                                                date         = DT.General[["date"]],
                                                y            = Y,
                                                window       = Config$sens2ref$Sens.window[k],
                                                threshold    = Config$sens2ref$Sens.threshold[k],
                                                plotting     = FALSE
                        )
                        nameInd      <- paste0(i,".",j)
                        OutlinameInd <- paste0(i,".",j,".Outli")
                        assign(nameInd , data.frame(date = Outli$date, Outliers = apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], 1, any), stringsAsFactors = FALSE))
                        if (exists("return.ind.sens.out")) return.ind.sens.out[[nameInd]] <- get(nameInd) else {
                            return.ind.sens.out <- list(get(nameInd)); names(return.ind.sens.out) <- nameInd
                        }
                        return.ind.sens.out[[OutlinameInd]] <- Outli
                    }
                    # Discarding outliers if requested for the compound
                    # Discading outliers
                    if (any(names(ind.sens.out) %in% paste0(i,".",j), na.rm = TRUE)) {
                        set(DT.General,i = which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out."      ,i),
                            value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                        set(DT.General,i = which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out.",i,".",j),
                            value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                    }
                }
            }
        }
    }
    if (exists("return.ind.sens.out"))  ind.sens.out <- return.ind.sens.out
    # reseting return.ind.sens.out
    if (exists("return.ind.sens.out")) rm(return.ind.sens.out)

    # Force conversion of sensors
    Conv.Forced <- TRUE
    rm(Outli)
}
for (i in 1:length(list.gas.sensor)) for (j in 1:Config$sens2ref$Sens.iterations[i]) assign(paste0(list.gas.sensor[i],".",j),NULL)

# discard outliers of reference data
if (!is.null(Ref) && Outliers.Ref.Forced) {
    # list of index of negative values
    ################################ ADD a Test to check that all reference parameters exists ######################
    ind.neg <- apply(X = DT.General[, .SD, .SDcols = list.gas.reference2use], MARGIN = 2, function(x) {which(x < 0)})
    for (i in list.gas.reference2use) {
        # resetting to initial values
        Vector.columns <- paste0(c("Out.", "Out.Neg."),i)
        set(DT.General, j = Vector.columns, value = rep(list(DT.General[[i]]), times = length(Vector.columns)))
        # discarding negative values if needed
        # number index of reference pollutant in the list of references
        k <- match(x = i, table = list.gas.reference2use)
        if (Config$sens2ref$Ref.rm.Out[k]) {
            if (length(ind.neg) > 0 && length(ind.neg[[i]]) > 0) { # exists(ind.neg[[i]]) &&
                set(DT.General,i = ind.neg[[i]], j = Vector.columns, value = rep(list(rep(NA, times = length(ind.neg[[i]]))), times = length(Vector.columns)))
            }
        }
        set(DT.General, j = paste0("Out.",i,".",1:Config$sens2ref$Ref.iterations[k]),
            value = rep(list(DT.General[[paste0("Out.Neg.",i)]]), times = Config$sens2ref$Ref.iterations[k]))
        # deleting bigger iterations
        j  <- Config$sens2ref$Ref.iterations[k]
        repeat (
            if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DT.General)))) {
                set(DT.General, j = paste0("Out.",i,".",j + 1), value = NULL)
                j <- j + 1
            } else break # leaving the Repeat if there are no higher iterations
        )
        # Index of outliers for reference data
        if (Config$sens2ref$Ref.rm.Out[k]) {
            for (j in 1:Config$sens2ref$Ref.iterations[k]) { # numver of iterations
                if (i %in% names(DT.General)) {
                    if (all(is.na(DT.General[[i]]))) {
                    } else {
                        Y <- DT.General[[paste0("Out.Neg.",i)]]
                        # setting the outliers of previous iterations to NA. If null then stop outlier detection
                        if (j > 1) {
                            if (length(which(return.ind.ref.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                Y[as.numeric(paste(unlist(sapply(return.ind.ref.out[c(paste0(i,".",1:(j - 1)))], function(x) which(x$Outliers)))))] <- NA
                            }  else break
                        }
                        Outli <- My.rm.Outliers(ymin         = Config$sens2ref$Ref.Ymin[k],
                                                ymax         = Config$sens2ref$Ref.Ymax[k],
                                                ThresholdMin = Config$sens2ref$Ref.ThresholdMin[k],
                                                date         = DT.General[["date"]],
                                                y            = Y,
                                                window       = Config$sens2ref$Ref.window[k],
                                                threshold    = Config$sens2ref$Ref.threshold[k],
                                                plotting     = FALSE
                        )
                        nameInd      <- paste0(i,".",j)
                        OutlinameInd <- paste0(i,".",j,".Outli")
                        assign(nameInd , data.frame(date = Outli$date,
                                                    Outliers = unlist(apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")],
                                                                            MARGIN = 1,
                                                                            function(x) any(x))),
                                                    stringsAsFactors = FALSE))
                        if (exists("return.ind.ref.out")) return.ind.ref.out[[nameInd]] <- get(nameInd) else {
                            return.ind.ref.out <- list(get(nameInd))
                            names(return.ind.ref.out) <- nameInd
                        }
                        return.ind.ref.out[[OutlinameInd]] <- Outli
                        # Discarding outliers
                        if (any(names(return.ind.ref.out) %in% paste0(i,".",j), na.rm = TRUE)) {
                            set(DT.General,i = which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out."      ,i),
                                value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers)))))
                            set(DT.General,i = which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out.",i,".",j),
                                value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers)))))
                        }
                    }
                } else cat("[Shiny, ind.ref.out()] ERROR, Warning no reference values impossible to discard outliers\n")
            }
        }
    }
    if (exists("return.ind.ref.out")) ind.ref.out <- return.ind.ref.out
    # reseting return.ind.ref.out
    if (exists("return.ind.ref.out")) rm(return.ind.ref.out)
    rm(Outli)
    for (i in 1:length(list.gas.reference2use)) for (j in 1:Config$sens2ref$Ref.iterations[i]) assign(paste0(list.gas.reference2use[i],".",j),NULL)
}

# Conversion and calibration
# list of possible model types
Models <- c("Linear", "Linear.Robust","MultiLinear", "exp_kT", "exp_kK", "T_power", "K_power", "RH_Hysteresis","gam", "Quadratic", "Cubic", "Michelis", "Sigmoid")
Shield <- if (!is.null(Config$Server$asc.File) & length(Config$Server$asc.File) != 0) {
    ASEPanel04Read(ASEPanel04File = file.path(rootWorkingDirectoy, "Shield_Files", Config$Server$asc.File))
} else return("ERROR, Config file of chemical shield not existing.\n")
Calib_data <- data.frame(
    name.gas           = list.name.gas,
    name.sensor        = list.name.sensor,
    gas.reference      = list.gas.reference,
    gas.reference2use  = list.gas.reference2use,
    gas.sensor         = list.gas.sensor,
    Sens.raw.unit      = as.character(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Sens.raw.unit[i])),
    Sens.unit          = as.character(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Sens.unit[i])),
    Cal.Line           = as.character(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Cal.Line[i])),
    Sync.Cal           = as.logical(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Sync.Cal[i])),
    Sync.Pred          = as.logical(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Sync.Pred[i])),
    Cal.func           = as.character(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Cal[i])),
    mod.eta.model.type = as.character(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Calibration[i])),
    Neg.mod            = as.logical(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Neg.mod[i])),
    Slope              = as.numeric(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Slope[i])),
    Intercept          = as.numeric(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Intercept[i])),
    ubsRM              = as.numeric(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$ubsRM[i])),
    ubss               = as.numeric(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$ubss[i])),
    eta.model.type     = as.character(sapply(seq_along(list.gas.sensor), function(i) Config$sens2ref$Comparison[i])),
    stringsAsFactors = FALSE)

if (Conv.Forced || Cal.Forced) {
    # Converting to nA or V
    if (Conv.Forced) {
        # Conversion to volts/A
        Sensors_Cal <- merge(x = Calib_data[c("name.gas","gas.sensor","name.sensor","Sens.raw.unit")], # if we use CaliB_data the file is saved every time we update Calid_data
                             y = Shield[,c("name.gas","name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")],
                             by = c("name.gas", "gas.sensor", "name.sensor"),
                             all = TRUE)
        # order Sensors_Cal as Calib_dataGoo
        #Sensors_Cal <- Sensors_Cal[na.omit(match(list.gas.sensor(),Sensors_Cal$gas.sensor)),]
        # Values converted in volt or nA of sensors in Shield only if sensor data exist
        data.table::set(DT.General,  j = paste0(Shield$name.sensor,"_volt"),
                        value = ASEDigi2Volt(Sensors_Cal = Sensors_Cal[Sensors_Cal$name.gas %in% Shield$name.gas,],
                                             Digital = DT.General[,paste0("Out.",Shield$gas.sensor), with = FALSE]))
        # Values converted in volt or nA - Board zero in Volt? change to V or nA
        # # https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
        data.table::set(DT.General,  j = paste0(Shield$name.sensor,"_DV"),
                        value = lapply(Shield$name.sensor, function(i) rep(Shield$Ref[Shield$name.sensor == i] - Shield$RefAD[Shield$name.sensor == i],
                                                                           times = nrow(DT.General))))
        # No conversion for the sensors which are not in the Shield only if sensor data exist
        No.Shield.gas.Sensors <- setdiff(list.gas.sensor, Shield$gas.sensor)
        No.Shield.gas.Sensors <- No.Shield.gas.Sensors[which(c(paste0("Out.",No.Shield.gas.Sensors) %in% names(DT.General) ))]
        if (length(No.Shield.gas.Sensors) > 0) {
            No.Shield.name.Sensors <- setdiff(list.name.sensor, Shield$name.sensor)
            x <- DT.General[,paste0("Out.",No.Shield.gas.Sensors), with = FALSE]
            data.table::set(DT.General,  j =  paste0(No.Shield.name.Sensors,"_volt"),
                            value = lapply(seq_len(ncol(x)), function(i) x[,i]))
            rm(x)
        }
        Cal.Forced <- TRUE
    }
    # Starting calibration
    if (Cal.Forced) {
        # Application of Calibration function to Complete data set
        if (!is.null(DT.General)) {
            # initial Calibration with values in input[[paste0("Cal",j)]])) provided that "Method of Prediction" is "Prediction with previous calibration"
            for (k in seq_along(list.name.sensor)) {
                if (Config$sens2ref$Cal.Line[k] == "Prediction with previous calibration") {
                    if (nchar(Config$sens2ref$Cal.func[k]) > 0) {
                        # reading file
                        name.Model.i <- file.path(boxDirectory,subDirModels,
                                                  paste0(boxName,"__",list.name.sensor[k],"__",Config$sens2ref$Cal.func[k]))
                        if (file.exists(name.Model.i)) {
                            Model.i <- load_obj(name.Model.i)
                            # sensor gas in volt or nA or Count
                            nameGasVolt <- paste0(list.name.sensor[k],"_volt")
                            # modelled sensor gas
                            nameGasMod  <- paste0(list.gas.sensor[k],"_modelled")
                            # Detecting the model type of the selected calibration model
                            Mod_type <- Models[grep(pattern = paste0("_",strsplit(name.Model.i, split = "__")[[1]][4],"_"),
                                                    x = paste0("_",Models,"_"))]
                            # Preparing the matrix of covariates
                            # Removing na for nameGasMod for nameGasVolt missing
                            is.not.NA.y <- which(!is.na(DT.General[[nameGasVolt]]))
                            is.NA.y     <- which( is.na(DT.General[[nameGasVolt]]))
                            if (Mod_type == "MultiLinear") {
                                CovMod  <- unlist(strsplit(x = unlist(strsplit(x = sub(pattern = paste(c(".rds",".rdata"), collapse = "|"),
                                                                                       replacement = "",
                                                                                       x =  name.Model.i),
                                                                               split = "__"))[7],
                                                           split = "&",
                                                           fixed = T))
                                # Checking if there are "-" in the CovMod, deleting degrees of polynomial
                                if (any(grepl(pattern = "-", x = CovMod[1]))) {
                                    Model.CovMod  <- unlist(strsplit(x = CovMod , split = "-"))
                                    CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
                                    Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
                                }
                                # checking that all CovMod are included in DT.General
                                if (!all(CovMod %in% names(DT.General))) {
                                    my_message <- paste0("ERROR, not all Covariates are available, something missing in ", CovMod,".\n")
                                    cat(my_message)
                                } else {
                                    # take only the one that is nor NA of y = DT.General[!is.na(DT.General[, nameGasVolt]), nameGasVolt]
                                    is.not.NA.y <- which(complete.cases(DT.General[, .SD, .SDcols = c(nameGasVolt,CovMod)]))
                                    is.NA.y     <- setdiff(1:nrow(DT.General), is.not.NA.y)
                                    Matrice <- data.frame(DT.General[is.not.NA.y, CovMod, with = FALSE],
                                                          row.names = row.names(DT.General[is.not.NA.y,]),
                                                          stringsAsFactors = FALSE)
                                    names(Matrice) <- CovMod
                                }
                            } else if (Mod_type %in% c("exp_kT","exp_kK","T_power", "K_power")) {
                                # take only the one that is nor NA of y = DT.General[!is.na(DT.General[, nameGasVolt]), nameGasVolt]
                                is.not.NA.y <- which(complete.cases(DT.General[, .SD, .SDcols = c(nameGasVolt, "Temperature")]))
                                is.NA.y     <- setdiff(1:nrow(DT.General), is.not.NA.y)
                                Matrice <- data.frame(DT.General[is.not.NA.y, Temperature],
                                                      row.names = row.names(DT.General[is.not.NA.y,]),
                                                      stringsAsFactors = FALSE)
                                names(Matrice) <- "Temperature"
                            } else {
                                Matrice <- NULL
                            }
                            # Using the reverse calibration function (measuring function) to extrapolate calibration
                            if (Mod_type != "MultiLinear" || (Mod_type == "MultiLinear" && all(CovMod %in% names(DT.General)))) {
                                data.table::set(DT.General, i = is.not.NA.y, j = nameGasMod,
                                                value = list(Meas_Function(y          = DT.General[[nameGasVolt]][is.not.NA.y],
                                                                           Mod_type   = Mod_type ,
                                                                           covariates = CovMod,
                                                                           Degrees    = Degrees,
                                                                           Model      = Model.i,
                                                                           Matrice    = Matrice)))
                                # Removing na for nameGasMod either nameGasVolt missing or CovMod missing
                                data.table::set(DT.General, i = is.NA.y, j = nameGasMod, value = list(rep(NA, times = length(is.NA.y))))
                                # setting negative values to NA
                                if (Config$sens2ref$remove.neg[k]) {
                                    data.table::set(DT.General, i = which(DT.General[, nameGasMod, with = FALSE] < 0), j = nameGasMod,
                                                    value = list(rep(NA, times = length(which(DT.General[, nameGasMod, with = FALSE] < 0)))))
                                }
                            }
                        } else {
                            cat(paste0("ERROR, there is no calibration function file does not exist for sensors: ", list.name.sensor[k], "\n"))
                        }
                    } else {
                        cat(paste0("ERROR, there is no calibration function configured for sensors: ", list.name.sensor[k], "\n"))
                    }
                }
            }
        }
    } else cat("ERROR, THERE IS NO General.df or no sensor data converted to voltage or current.\n")
}
