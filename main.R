#
#   Downloading dependencies
#
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
#
#   Loading packages and dependencies
#
library(raster)
source("global.R")
source("Functions4ASE.R")
source("influx.R")
#
#   Start Downloading data and extract configuration + x
#
wd <- getwd()
station <- "40458D"
config <- influx.getConfig(boxName = station)
tmp <- influx.downloadAndPredict(boxName = station, boxConfig = config)
data <- tmp$data
cols <- unlist(c("date","altitude","latitude","longitude", as.list(config$sens2ref$gas.sensor), names(which(sapply(names(data), function(n) { endsWith(n, "_modelled")})))))
data <- data[, ..cols]
timeCfg <- tmp$timeConfig
rm(tmp)
setwd(wd)
#
#   Clean-Up after script run
#
for (file in files) {
    file <- paste0(file, ".R")
    if (file.exists(file)) {
        unlink(file)
        futile.logger::flog.info(paste0("Deleted previous version of '", file, "'"))
    }
}
rm(files)
