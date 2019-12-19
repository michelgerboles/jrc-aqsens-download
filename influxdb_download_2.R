# Set Directory of the script
# This script shall be in the same directory as the App.R, functions4ASE.R ...
library(rChoiceDialogs)
if (!exists("DisqueFieldtest")) DisqueFieldtest <- jchoose.dir()
DirShiny        <- DisqueFieldtest
setwd(DisqueFieldtest)

#   1.d Loading packages (global.R)
source("global.R")
source("functions4ASE.R")

############### at Eike you should select here ASE_name in the way you prefer ###############################################
# AirSensEUR name: The 1st one selected in the list of configured AirSensEURs
Config_Files     <- list.files(path = getwd(), pattern = glob2rx("ASEconfig*.R"))[1]
ASE_name         <- basename(Config_Files)      ; for (i in c("\\.[[:alnum:]_]+$" ,"ASEconfig")) ASE_name     <- sub(pattern = i,replacement = '', basename(as.character(ASE_name)))
# Setting the  directory from whcih to copy the config files
old_Config_Files <- list.files(path = getwd(), pattern = glob2rx("ASEconfig*.R"))[1]
old_ASE_name     <- basename(old_Config_Files)  ; for (i in c("\\.[[:alnum:]_]+$" ,"ASEconfig")) old_ASE_name <- sub(pattern = i,replacement = '', basename(as.character(old_ASE_name)))

# DisqueFieldtestDir     : The one of the Selected AirSensEUR
DisqueFieldtestDir <- file.path(DirShiny, ASE_name)
if (!dir.exists(DisqueFieldtestDir)) dir.create(DisqueFieldtestDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

#  1.c Create file system structure.for new AirSensEUR boxes
if (!file.exists(file.path(DirShiny, ASE_name))) {

    # Check directories existence or create , create log file
    if (ASE_name != "") {
        # Creating the the working directory of the AirSensEUR box
        if (!dir.exists(DisqueFieldtestDir)) dir.create(DisqueFieldtestDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        # Creating File structure
        List.Dirs <- c("Calibration","Drift","Estimated_coef","General_data","Models","Modelled_gas","Outliers","scriptsLog",
                       "SensorData","Retrieved_plots","Statistics","Verification_plots", "MarkDown")
        for (i in List.Dirs) {
            if (!dir.exists(file.path(DisqueFieldtestDir, i))) {
                dir.create(file.path(DisqueFieldtestDir, i), showWarnings = TRUE, recursive = TRUE, mode = "0777")
            } else cat(paste0("Create.New] INFO Dir. already exists: ", file.path(DisqueFieldtestDir,i)), sep = "\n")
        }
        # Populating the configuration information, copy  old_ASE_name as new ASE_name
        file.copy(from = Config_Files, to = paste0("ASEconfig", ASE_name,".R"), overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
        # Populating the configuration intormation
        # cfg and effect files
        Old_General_dir <- file.path(DisqueFieldtest, old_ASE_name , "General_data")
        New_General_dir <- file.path(DisqueFieldtestDir            , "General_data")
        cfg_Files       <- list.files(path = Old_General_dir, pattern = ".cfg")
        for (i in cfg_Files) {
            cat(paste0("[shiny, Create.New] INFO, copying ", gsub(pattern = old_ASE_name, replacement = ASE_name, i), " at ", New_General_dir), sep = "\n")
            file.copy(from = file.path(Old_General_dir,i),
                      to   = file.path(New_General_dir, gsub(pattern = old_ASE_name, replacement = ASE_name, i)),
                      overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
        }
    }
}

# Defining Initial values ----
# unnecessary?
DT.NULL    <- FALSE
DT.General <- data.table
Influx     <- data.table
Sos        <- NULL
Ref        <- data.table


General.file            <- file.path(DisqueFieldtestDir, "General_data" , "General.csv")
InfluxData.file         <- file.path(DisqueFieldtestDir, "General_data" , "InfluxData.csv")
SOSData.file            <- file.path(DisqueFieldtestDir, "General_data" , "SOSData.csv")
RefData.file            <- file.path(DisqueFieldtestDir, "General_data" , "RefData.csv")
ind.warm.file           <- file.path(DisqueFieldtestDir, "General_data" , "ind_warm.RDS")
ind.TRh.file            <- file.path(DisqueFieldtestDir, "General_data" , "ind_TRh.RDS"  )
ind.Invalid.file        <- file.path(DisqueFieldtestDir, "General_data" , "ind_Invalid.RDS")
ind.sens.out.file       <- file.path(DisqueFieldtestDir, "General_data" , "ind_sens_out.RDS")
ind.ref.out.file        <- file.path(DisqueFieldtestDir, "General_data" , "ind_ref_out.RDS")
# cfg_file     : The cfg file  of the Selected AirSensEUR "
cfg_file           <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,".cfg"))
SETTIME_file       <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_SETTIME.cfg"))
Servers_file       <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Servers.cfg"))

# Configurationa nd reading of data
Config <- CONFIG(DirShiny, Config_Files)
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
                       WDinput     = file.path(DisqueFieldtestDir, "General_data"),
                       UserMins    = if (!is.null(Config$Server$UserMins)) Config$Server$UserMins else Config$Server$UserMins,
                       General.df  = if (!is.null(DT.General))  DT.General else NA,
                       RefData     = if (!is.null(Ref))    Ref else NA,
                       InfluxData  = if (!is.null(Influx)) Influx else NA,
                       SOSData     = if (!is.null(Sos))    Sos else NA)
# Initial SetTime
Set.Time        <- SETTIME(DisqueFieldtestDir = DisqueFieldtestDir,
                            General.t.Valid    = DT.General,
                            Influx.TZ          = Config[["Server"]]$Influx.TZ,
                            SOS.TZ             = Config[["Server"]]$SOS.TZ,
                            Ref.TZ             = Config[["Server"]]$ref.tzone,
                            DownloadSensor     = Download.Sensor,
                            Config             = Config,
                            sens2ref.shield    = Config$sens2ref.shield)
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
    Inv$Forced <- TRUE
}
# Indexes of outliers for sensor data
Outliers.Sens.Forced <- FALSE
if (file.exists(ind.sens.out.file)) ind.sens.out <- list.load(ind.sens.out.file) else {
    ind.sens.out <- NULL
    Outliers.Sens$Forced <- TRUE
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

# setting the current directory to the root of the file system with the name of the AirSensEUR
setwd(DisqueFieldtestDir)

# Reactive i.sensors Once AirSensEUR is Selected
# Returning the indexes of valid sensors in ASE_name.cfg taking into account NAs
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

# InfluxDB ----
# return(INFLUX)
# var.names.meteo     <- INFLUX[[2]]
# var.name.GasSensors <- INFLUX[[3]]
# var.names.sens      <- INFLUX[[4]]
# InfluxData          <- INFLUX[[1]]
if (!is.null(Influx)) InfluxData <- Influx[] else InfluxData <- NA_real_
Influx <- INFLUXDB(
    WDoutput        = file.path(DisqueFieldtestDir,"General_data"),
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
    name.SQLite     = file.path(DisqueFieldtestDir,"General_data","airsenseur.db"),
    name.SQLite.old = file.path(DisqueFieldtestDir,"General_data","airsenseur_old.db"),
    sens2ref        = Config[["sens2ref"]],
    InfluxData      = InfluxData)[[1]]
# updating Download.Sensor with new INflux data
Download.Sensor <- Check_Download(Influx.name = Config$Server$Dataset,
                                   WDinput     = file.path(DisqueFieldtestDir, "General_data"),
                                   UserMins    = if (!is.null(Config$Server$UserMins)) as.numeric(Config$Server$UserMins) else Config.Server$UserMins,
                                   General.df  = if (!is.null(DT.General))  DT.General else NA,
                                   RefData     = if (!is.null(Ref))    Ref else NA,
                                   InfluxData  = if (!is.null(Influx)) Influx else NA,
                                   SOSData     = if (!is.null(Sos))    Sos else NA)
