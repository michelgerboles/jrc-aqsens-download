#========================
#### Needed functions ####
#========================

### Function Load.Packages
Load.Packages <- function(list.Packages, verbose = FALSE) {
    # This function install packages if needed and load them
    # list.Packages                 vector of names of the packages to load
    # verbose                       logical default FALSE, return info message about error and installed packages
    # dependence                    havingIP()
    
    cat("-----------------------------------------------------------------------------------\n")
    
    if (verbose) cat("[Load.Packages] INFO CHECK Installed packages and Toolbox to run the script\n")
    #
    # checking if internet is available and CRAN can be accessed
    # isInternet <- havingIP()
    # if (verbose) if (isInternet) cat("[Load.Packages] Info: internet is available\n") else cat("[Load.Packages] Info: internet is not available\n")
    
    for (i in list.Packages) {
        
        if (i %in% rownames(installed.packages()) == FALSE) {
            if (verbose) cat(sprintf("[Load.Packages] INFO Installing %s", i), sep = "\n")
            install.packages(i)
        } else {
            if (verbose) cat(sprintf("[Load.Packages] INFO Package %s already installed",i), sep = "\n")
        }
        
        do.call("library", as.list(i))
        if (verbose) cat(sprintf("[Load.Packages] INFO Package %s loaded",i), sep = "\n")
        
    }
    
    # List of loaded packages
    if (verbose) cat("[Load.Packages] INFO List of installed packages\n")
    if (verbose) print(search(), quote = FALSE)
    
    cat("-----------------------------------------------------------------------------------\n")
}

# 170609 MG : Pinging WEB site
PingThisSite <- function(test.site) {
    # this function returns TRUE if it is possible to ping a test.site
    # test.site                     the URL whose existence we are to test
    
    if (!require(RCurl)) {
        # RCurl needs to be installed, checking if internet is available
        install.packages("RCurl")
    }
    require(RCurl)
    url.exists(test.site)
}
# 170609 MG : Pinging WEB site
havingIP <- function() {
    
    #browser()
    if (.Platform$OS.type == "windows") {
        ipmessage <- system("ipconfig", intern = TRUE)
    } else {
        ipmessage <- system("/sbin/ifconfig", intern = TRUE)
    }
    
    # validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]) {3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    validIP <- "(?<=[^0-9.]|^)[1-9][0-9]{0,2}([.]([0-9]{0,3})){3}(?=[^0-9.]|$)"
    
    return(any(unlist(gregexpr( validIP, ipmessage, perl = TRUE) ) != -1))
}
# 161120 MG : Downloading AirSensEUR.db data using the Influx protocol, create or update the airsenseur.db SQLite database, get timezone
Json_To_df <- function(JSON, Numeric = NULL, verbose = FALSE, Discard = NULL) {
    # JSON      : class "response" as returned by function httr::GET for INFLUX
    # Numeric   : charater vector with the colname of df to be converted into numeric
    # Discard   : column to drop from the returned dataframe
    # 
    # Returns a df a query data coverting from JSON of INFLUX with conversion of data columns from string to numeric
    
    if (JSON$status_code != 200) {
        Error.Message <- gsub(pattern = "%20", replacement = " ", JSON$url)
        Error.Message <- gsub(pattern = "%3D", replacement = "= ", Error.Message)
        Error.Message <- gsub(pattern = "%3B", replacement = ";", Error.Message)
        Error.Message <- gsub(pattern = "%22", replacement = "\"", Error.Message)
        Error.Message <- gsub(pattern = "%2C", replacement = "'", Error.Message)
        print(JSON, quote = FALSE)
        if (verbose) cat("Response to ",Error.Message,"\n")
        return(cat(paste0("[Json_To_df] ERROR, query returning error status code ",JSON$status_code, ". The query is wrong or the Influx server may be down.\n")))
    } else {
        JSON <- jsonlite::fromJSON(content(JSON, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = T)
        
        # Checking that JSON is not empty, e. g. there was no data for the Key or tag selected
        if (any(names(JSON) %in% "results")) {
            if (any(names(JSON$results) %in% "series")) {
                # delete "mean_" in case of query mean
                JSON.Colnames <- gsub(pattern = "mean_", replacement= "",JSON$results$series[[1]]$columns[[1]])
                if (verbose) cat(paste0("columns in JSON object: ",paste0(JSON.Colnames, collapse = ", "),"\n"))
                JSON <- setNames(data.frame(JSON$results$series[[1]]$values, 
                                            row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = TRUE, stringsAsFactors = FALSE), JSON.Colnames)
                if (any(Numeric %in% JSON.Colnames)) JSON[,Numeric[Numeric %in% JSON.Colnames]] <- sapply(JSON[,Numeric[Numeric %in% JSON.Colnames]], as.numeric)
                if (verbose) str(JSON)
            } else {
                # if (JSON$results[1,] == "data frame with 0 columns and 1 row")
                JSON <- "data frame with 0 columns and 1 row"
            }
        } else {
            JSON <- "No JSON data returned"
        }
        
        if (!is.null(Discard)) if (any(colnames(JSON) %in% Discard)) JSON <- JSON[,-which(colnames(JSON) %in% Discard)]
        return(JSON)
    }
}
Down_Influx <- function(PROXY = FALSE, URL = NULL, PORT = NULL, LOGIN = NULL, PASSWORD = NULL,
                        Host, Port = 8086, User, Pass, name.SQLite,name.SQLite.old,  Db, Dataset, Influx.TZ = NULL, 
                        Page = 200, Mean = 1, use_google = TRUE) {
    # Down_Influx downloads AirSensEUR.db data from an Influx server using JSON (package Jsonlite)
    
    # INPUT:
    # PROXY                           : Logical, default value FALSE, PROXY info necessary
    # URL                             : Character, default value NULL, url of your proxy, jrc = "10.168.209.72";
    # PORT                            : numeric, default value NULL, open Port for the proxy, jrc = 8012; 
    # LOGIN                           : character, default value = NULL, login for the proxy server, JRC = NULL;
    # PASSWORD                        : character, default value = NULL, password for the proxy server, jrc = NULL;
    
    # Parameters for the Influx download
    # Host                            : character, mandatory, url of the Influx server, jrc = 'influxdb1.liberaintentio.com', without "http://"
    # Port                            : numeric, default value = 8086, port used for the Influx transfer, the port ust be an open in your browser,
    # User                            : character, your login at the Influx server, jrc = "jrcuser",
    # Pass                            : character, your password at the Influx server, jrc = "idbairsenseur",
    # name.SQLite                     : character, path + name of the airsenseur.Db file, it shall be in the General.data directory
    # name.SQLite.old                 : character, backup of name.SQLite
    # Db                              : character, name of the database at the Influx server, jrc = "jrcispra",
    # Dataset                         : character, name of the table(Dataset) in the database Db that you want to download, e. g. "AirSensEUR05"
    # Influx.TZ                       : character, default value NULL. if NULL or "local time" the function will try to determine the time zone otherwise Influx.TZ will be used
    # use_google                      : logical: default = TRUE, if TRUE the google API is used for detecting time zone from coordinates (require port 443)
    # Page                            : numeric, default value 200 (LImit Influx_query), if Null the size of the page of data to download from the influx server is : LIMIT 10000, as requested in the Influx
    # Mean                            : numeric, default value 1 (Group by time(1m)), time average for the download of Influx data, 
    #                                   if Null the size of the page of data to download from the influx server is : LIMIT 10000, as requested in the Influx
    # 
    # Return                          : the time zone Influx.TZ, create the local database airsenseur.db in name.SQLite and return the time zone determined by find_tz(LastLong, Lastlat, use_google = TRUE)
    # Dependences                     : Ping(), Load.Packages()
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Checking internet connection availability
    #------------------------------------------------------------------------------CR
    if (havingIP()) {
        if (PingThisSite(gsub('http://', '', Host))) {
            cat(paste0("[Down_Influx] INFO; ping to ", Host, " Ok"), sep = "\n")
        } else{
            # return(cat(paste0("[Down_Influx] ERROR: you have internet connection but cant ping to ",Host,". InfluxDB download cannot be carried out."), sep = "\n"))
        } 
    } else {
        return(cat(paste0("[Down_Influx] ERROR: no internet connection. SOS download cannot be carried out."), sep = "\n"))
    }
    
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("curl", "devtools", "sqldf", "zoo", "xts", "XML", "httr", "RJSONIO", "jsonlite", "lubridate","plyr")
    Load.Packages(list.Packages)
    
    #------------------------------------------------------------------------------CR
    # Downloading timezone from Github
    #------------------------------------------------------------------------------CR
    if (PROXY) {
        if (is.null(LOGIN)) set_config(use_proxy(url=URL, port=PORT)) else set_config( use_proxy(url=URL, port=PORT, username    = LOGIN, password = PASSWORD))
    } else reset_config()
    list.packages.github <- c("52North/sensorweb4R") #,"rundel/timezone"
    for (i in list.packages.github) {
        
        # removing author name anad version number
        lib.i <- tail(unlist(strsplit(i, split = "/")), n = 1)
        lib.i <- head(unlist(strsplit(lib.i, split = "@")), n = 1)
        
        if (!(lib.i %in% rownames(installed.packages()))) {
            devtools::install_github(i)
            cat(sprintf("Package ", lib.i, " installed"), sep = "\n")
        } else cat(paste0("[Down_Influx] INFO, Package ", i, " already installed"), sep = "\n")
        
        do.call("library", as.list(lib.i))
        cat(sprintf("[Down_Influx] INFO, Package %s loaded",i), sep = "\n")
    }
    
    #------------------------------------------------------------------------------CR
    # create influx connection object and getting number of records
    #------------------------------------------------------------------------------CR
    Influx.con <- httr::GET(paste0("http://",Host,":",Port,"/ping"), 
                            config = authenticate(user = User, password = Pass, type = "basic"))
    if (Influx.con$status_code != 204) {
        cat("[Down_Influx] ERROR Influx server is down. Stopping the script.", "/n") 
        return(cat("[Down_Influx] ERROR Influx server is down. Stopping the script.\n"))
    } else cat("[Down_Influx] Influx server is up; connected to server\n")
    
    # total number of rows
    Influx.Total.N <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SELECT count(latitude) FROM ", Dataset)))
    Influx.Total.N <- Json_To_df(Influx.Total.N, Numeric = "count")$count
    # Last GPS time (and the timestamp together)
    Influx.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT  LAST(gpsTimestamp) FROM ", Dataset)))
    Influx.Last <- Json_To_df(Influx.Last, Numeric = "last")
    # Downloading Influxdb data in airsenseur.db:
    # if airsenseur.db does not exist       -->     create database : now we are in the 2nd case, airsenseur.db exists!
    # if airsenseur.db exists:   
    # if the table Dataset does not exist   -->     create the table and download all influx data
    # if the table Dataset  exists          -->     download only new data to the table dataset
    
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? Make a copy if dates are different from Old db. Connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) { # airsenseur.db exists
        
        cat(paste0("[Down_Influx] INFO, ", name.SQLite, " already exists."), sep = "\n")
        
        # making a copy of airsenseur.db as airsenseur_old.db if airsenseur_old.db is older than airsenseur.db
        if (file.exists(name.SQLite.old)) {
            
            if (file.info(name.SQLite)$size != file.info(name.SQLite.old)$size) {
                
                cat(paste0("[Down_Influx] INFO, the current airsenseur.db is being copied into airsenseur_old.db, this may take some time."), sep = "\n")
                file.copy(from = name.SQLite, to = name.SQLite.old, overwrite = TRUE, copy.mode = TRUE)
                
            } else cat(paste0("[Down_Influx] INFO, the current airsenseur_old.db has the same date (saved) as airsenseur.db, no need to backup "), sep = "\n")
            
        } else {
            
            cat(paste0("[Down_Influx] INFO, there is no backup of airsenseur.db. Copying to airsenseur_old.db"), sep = "\n")
            file.copy(from = name.SQLite, 
                      to = name.SQLite.old, 
                      overwrite = TRUE, 
                      copy.mode = TRUE) # copy.date = TRUE
        }
    } else {
        
        # airsenseur.db does not exist
        cat(paste0("[Down_Influx] INFO, ", name.SQLite, " does not exist and it is going to be created."), sep = "\n")
        
    }
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    #------------------------------------------------------------------------------CR
    # table dataset exists?
    #------------------------------------------------------------------------------CR
    if (dbExistsTable(conn = SQLite.con, name = Dataset)) { # the table Dataset exists in airsenseur.db
        
        cat(paste0("[Down_Influx] INFO, the table ", Dataset, " already exists in airsenseur.db"), sep = "\n")
        
        # Counting the number of records in AirSensEUR$Dataset - This will work provided that all rowid exist.
        #Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT COUNT(gpsTimestamp) FROM ", Dataset))[1,1]
        Dataset.N               <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
        SQL.time.Last           <- dbGetQuery(SQLite.con, paste0("SELECT * FROM ", Dataset,"  order by rowid desc limit 1;"))$time
        SQL.gpsTimestamp.Last   <- dbGetQuery(SQLite.con, paste0("SELECT * FROM ", Dataset,"  order by rowid desc limit 1;"))$gpsTimestamp
        
        # Error Message and stop the script if there more data in the airsenseur.db than in InfluxDB
        if (difftime(Influx.Last$time, SQL.time.Last, units = "mins") < Mean) {
            cat("[Down_Influx] Warning,  Downloading is up to date. No need for data download.\n")
        } else cat(paste0("[Down_Influx] INFO, records between ",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(Influx.Last$time),"%Y-%m-%d %H:%M"), " are going to be added into the table ", Dataset, " in the local airsenseur.db.\n"))
        Dataset.index   <- FALSE # if airsenseur.db exists then the indexes were already created, then no need to create the indexes
        
    } else {# the table Dataset does not exist in airsenseur.db
        
        # There are no records in AirSensEUR$Dataset
        cat(paste0("[Down_Influx] INFO, the table ", Dataset, " does not exist in airsenseur.db. It is going to be created"), sep = "\n")
        Dataset.N       <- 0
        
        # Get SQL.time.Last as First GPS time (and the timestamp together) of InfluxDB
        SQL.time.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                   config = authenticate(user = User, password = Pass, type = "basic"),
                                   query = list(q = paste0("SELECT  FIRST(sampleEvaluatedVal) FROM ", Dataset)))
        if (SQL.time.Last$status_code != 200) cat("[Down_Influx] ERROR query last GPSTime in airsenseur.db, Influx server may be down. Stopping the script.", "/n")
        SQL.time.Last <- Json_To_df(SQL.time.Last)$time
        
        Dataset.index   <- TRUE # if true indexes will be created
        
    } # the table Dataset exists in airsenseur.db
    
    #------------------------------------------------------------------------------CR
    # Downloading InfluxDB data and add them to the airsenseur.db
    #------------------------------------------------------------------------------CR
    # List of sensors to download
    # Sensors names, it seems that I cannot query directly name or channel (strings). Adding SELECT of a float field it works. Selecting the first 50 ones. Use SHOW TAG VALUES INSTEAD
    Influx.Sensor <-  httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SHOW TAG VALUES FROM ", Dataset," WITH KEY IN ( \"name\") ; ")))
    Influx.Sensor <- Json_To_df(Influx.Sensor); 
    colnames(Influx.Sensor)[colnames(Influx.Sensor) == "value"] <- "name"; 
    
    # Adding channel for each Sensor names
    for (i in Influx.Sensor$name) {
        Influx.Channel.number <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                           config = authenticate(user = User, password = Pass, type = "basic"),
                                           query = list(q = paste0("SELECT altitude, channel, \"name\" FROM ", Dataset," WHERE \"name\" = '",i,"' LIMIT 1;"))) 
        Influx.Channel.number <- Json_To_df(Influx.Channel.number)
        Influx.Sensor[which(Influx.Sensor$name == i),"channel"] <- Influx.Channel.number$channel
        Influx.Sensor[which(Influx.Sensor$name == i),"time"]   <- Influx.Channel.number$time
    }
    Influx.Sensor <- Influx.Sensor[order(Influx.Sensor$time),]
    print(Influx.Sensor, quote = FALSE)
    
    
    # Downloading always in increasing date, max download data in InfuxDB: chunks of 10000
    NbofDays.For10000data <- 10000/(24*60/Mean)
    # Number of seconds corresponding to NbofDays.For10000data
    Step <- NbofDays.For10000data * 24 * 60 * 60
    while (difftime(Influx.Last$time, SQL.time.Last, units = "mins") > Mean) {
        
        # Trying to query 5 times all numeric field keys
        for (j in 1:length(Influx.Sensor$name)) { # Downloadding sensor by sensor
            
            cat(paste0("[Down_Influx] INFO, downloading averages every ",Mean," min Influx data between ", format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M"),
                       " and ",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M")," for sensor ",Influx.Sensor[j,"name"],"\n"))
            
            # Downloading from Inlfux server using httr
            Trial <- 1
            repeat{
                Mean.Query <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                        config = authenticate(user = User, password = Pass, type = "basic"),
                                        query = list(q = paste0("SELECT mean(*) FROM ", Dataset," WHERE \"name\" = '",Influx.Sensor[j,"name"],
                                                                "'  AND time >= '",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M:%S"),"' AND time < '",
                                                                format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M:%S"),"' GROUP BY time(",Mean,"m);"
                                                                # LIMIT " , format(round(1440/Mean), scientific = FALSE), " OFFSET ", format(0, scientific = FALSE)
                                                                # It is necessary to add :%S in the time tags
                                        ))) 
                if (Mean.Query$status_code == 200 || Trial > 5) break
                Trial <- Trial + 1
            }
            
            # Checking good query status code
            if (Mean.Query$status_code != 200) cat(paste0("[Down_Influx] ERROR, does not succed to query the influxDB with status_code <> 200. After 5 trials the script is stopped.\n")) else {
                
                # extracting lists from json
                if (length(colnames(Json_To_df(Mean.Query))) > 1 ) {
                    #Adding <- Json_To_df(Mean.Query, Numeric = c("altitude", "boardTimeStamp", "calibrated", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"))
                    # calibrated is not necessary, we will create a new table in influx in cloud
                    Adding <- Json_To_df(Mean.Query, 
                                         Numeric = c("altitude", "boardTimeStamp", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"),
                                         Discard = c("Calibrated","calibrated"))
                    
                    # Adding Channel and name
                    Adding$channel <- as.integer(Influx.Sensor[j,"channel"]); Adding$name <- Influx.Sensor[j,"name"]
                    
                } else next
                
            }
            
            # updating Adding
            if (exists("All.Sensors.Adding")) All.Sensors.Adding <- rbind.fill(All.Sensors.Adding,Adding) else All.Sensors.Adding <- Adding
            
        }
        
        #browser()
        # adding data to airSensEUR.db
        if (exists("All.Sensors.Adding")) {
            
            # discarding rows with all Na Values
            NA.values <- which(
                rowSums(
                    is.na(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name"))])) ==
                    ncol(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name"))]
                    )
            )
            if (length(NA.values) > 0) All.Sensors.Adding <- All.Sensors.Adding[-NA.values,]
            
            # add overwrite = TRUE to allow to recalculate the last 10 min values when some values are added
            RSQLite::dbWriteTable(conn = SQLite.con, name = Dataset, value = All.Sensors.Adding, append = TRUE) 
            cat(paste0("[Down_Influx] INFO, ", format(nrow(All.Sensors.Adding), scientific = FALSE), " records downloaded between ",
                       format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M")
                       ," added to table ", Dataset, " of airsenseur.db"), sep = "\n")
            remove(All.Sensors.Adding, Adding)
        } else cat(paste0("[Down_Influx] INFO, No influx data between ", format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M"),".\n"))
        
        # updating SQL.time.Last for while loop
        #browser()
        SQL.time.Last  <- ymd_hms(SQL.time.Last)+Step
        
    } 
    
    cat(paste0("[Down_Influx] INFO, the downloading of sensor data from the Influx server is finished.\n"))
    # I need to add index ?!?
    
    # Counting the number of records in AirSensEUR$Dataset
    Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    
    # getting the time zone, port 443 of the Browser shall be opened
    #browser()
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") { 
        
        cat(paste0("[Down_influx] INFO, determining the time zone with the last valid latitude and longitude of ", Dataset, " in airsenseur.db"), sep = "\n")
        
        Offset <- Dataset.N
        repeat{
            
            Coord.lat.long   <- dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude FROM ", Dataset, " WHERE rowid > ", Offset - 500, " AND rowid <= ", Offset, " ;"))
            # print(Coord.lat.long, quote = F)
            # browser()
            
            if (all(is.na.data.frame(Coord.lat.long[,c("longitude","latitude")])) || 
                all(Coord.lat.long[!is.na.data.frame(Coord.lat.long[,c("longitude")]),c("longitude","latitude")] ==0)) {
                
                if (Offset > 500) {
                    Offset <- Offset - 500   
                } else {
                    cat(paste0("[Down_influx] INFO, impossible to determine the time zone of the sensor data. TZ is kept as ", Influx.TZ), sep = "\n")
                    break  
                }   
            } else {
                
                Lastlat     <- tail(na.omit(Coord.lat.long$latitude[Coord.lat.long$latitude != 0]), n = 1)
                LastLong    <- tail(na.omit(Coord.lat.long$longitude[Coord.lat.long$longitude != 0]), n = 1) 
                Influx.TZ <- find_tz(LastLong, Lastlat, use_google = use_google)
                cat(paste0("[Down_influx] INFO, the time zone of the sensor data is ", Influx.TZ), sep = "\n")
                
                break
            }
            
        }
        
    } 
    
    # # getting the last date, latitude and longitude in name.SQLite.
    # Last date to add only new data, latitude and longitude to get the time zone 
    # checking for non zero values and no NA()
    #browser()
    LastDate <- dbGetQuery(SQLite.con, paste0("SELECT time FROM ", Dataset," ORDER BY rowid DESC LIMIT 1;"))$time
    # InfluxDB gives everything in UTC not in local time zone - Well by observation in grafana it seems that the dates are in Local Time
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") {
        LastDate <- ymd_hms(LastDate, tz = "UTC")
    } else {
        LastDate <- ymd_hms(LastDate, tz = Influx.TZ)
    } 
    
    # Creating index to speed up select in function SQLite2df, it seems that this is not used anymore so it is commented
    # if (Dataset.index) {
    #     dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON "   , Dataset, " (time);"))
    #     dbGetQuery(SQLite.con, paste0("CREATE INDEX IDchanne ON " , Dataset, " (channel);"))
    #     dbGetQuery(SQLite.con, paste0("CREATE INDEX IDname ON "   , Dataset, " (name);"))
    # } 
    
    # Disconnect SQLite.con
    dbDisconnect(conn = SQLite.con)
    cat(paste0("[Down_Influx] INFO, the airsenseur.db goes until ", format(LastDate, "%Y-%m-%d %H:%M"), ", with ", Dataset.N, " records for the table ", Dataset), sep = "\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    #browser()
    return(Influx.TZ)
}

# 161029 MG: Check General.R data file and the retrieve.data true or false
Check_Download <- function(Influx.name = NULL, WDinput, UserMins) {
    # Influx.name              = Name of for AirSensEUR in airsenseur.db, default Value NULL
    # WDinput                  = Sub directory of DisqueFieldtest where are the Refdata and InfluxData Rdata files
    # UserMins                 = periodicity of data requested after final data treatment
    # Return                   = a list with 
    #                              Ref.Rdata.file and Influx.Rdata.file, the name of the files with dataframe of reference and sensor downloaded data
    #                              WDinput, the directory where the Rdata are saved
    #                              Retrieve.data.Ref, true if reference data need be retrieved
    #                              Retrieve.data.Influx, true if sensor data need be retrieved (Influxdb)
    #                              Retrieve.data.SOS, true if sensor data need be retrieved (SOS)
    #                              Retrieve.data.General, true if sysdate is posterior to DateEND.General.prev
    #                              DateEND.Ref.prev, date to start download of reference data (last existing date), Null if "RefData.Rdata" does not exist
    #                              DateEND.Influx.prev, date to start download of sensor data (last existing date), Null if ""InfluxData.Rdata" does not exist
    #                              DateEND.SOS.prev, date to start download of sensor data (last existing date), Null if ""SOSData.Rdata" does not exist
    #                              DateEND.General.prev, last date  in General.Rdata, Null if ""General.Rdata" does not exist
    
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("dplyr")
    Load.Packages(list.Packages)
    
    # Set the Rdata file of input data
    airsenseur.db.file  = file.path(WDinput, "airsenseur.db")
    Ref.Rdata.file      = file.path(WDinput, "RefData.Rdata")
    Influx.Rdata.file   = file.path(WDinput, "InfluxData.Rdata")
    SOS.Rdata.file      = file.path(WDinput, "SOSData.Rdata")
    General.Rdata.file  = file.path(WDinput, "General.Rdata")
    cat("-----------------------------------------------------------------------------------\n")
    cat(paste0("[Check_Download] INFO, Checking ",
               airsenseur.db.file, "\n",
               General.Rdata.file, "\n",
               Ref.Rdata.file, "\n",
               SOS.Rdata.file, " \n",
               Influx.Rdata.file,"\n in ", WDinput, "\n"))
    
    # Checking if the directory exist
    if (!dir.exists(WDinput)) {
        
        cat(paste0("[Check_Download] INFO, Directory", WDinput, "does not exist. It is going to be created. All sensor and reference data are going to be downloaded."), sep = "\n")
        dir.create(WDinput, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        
        # in this case downloading of all sensor and reference data is necessary
        Retrieve.data.Ref     = TRUE
        ExistFil.data.Ref     = FALSE
        DateIN.Ref.prev       = NULL
        DateEND.Ref.prev      = NULL
        
        Retrieve.data.Influx  = TRUE
        ExistFil.data.Influx  = FALSE
        DateIN.Influx.prev    = NULL
        DateEND.Influx.prev   = NULL
        
        Retrieve.data.SOS     = TRUE
        ExistFil.data.SOS     = FALSE
        DateIN.SOS.prev       = NULL
        DateEND.SOS.prev      = NULL
        
        Retrieve.data.General = TRUE
        ExistFil.data.General = FALSE
        DateIN.General.prev   = NULL
        DateEND.General.prev  = NULL
        
        Retrieve.data.db      = TRUE
        ExistFil.data.db      = FALSE
        DateIN.db.prev        = NULL
        DateEND.db.prev       = NULL
        
    } else {
        
        # The directory exists, checking if RefData exists
        if (!file.exists(Ref.Rdata.file)) { # Ref.Rdata.file does not exist
            
            # RefData does not exist
            Retrieve.data.Ref = TRUE
            ExistFil.data.Ref = FALSE
            DateIN.Ref.prev   = NULL
            DateEND.Ref.prev  = NULL
            Var.Ref.prev      = NULL
            cat(paste0("[Check_Download] INFO, ", Ref.Rdata.file, " does not exist. It is going to be created, data will be retrieved."), sep = "\n")
            
        } else { 
            
            # Ref.Rdata.file exists
            ExistFil.data.Ref = TRUE
            
            cat(paste0("[Check_Download] INFO, ", Ref.Rdata.file, " exists."), sep = "\n")
            load(Ref.Rdata.file)
            
            if (!is.null(RefData)) {
                
                # RefData exists and is not NULL
                
                # Not considering end rows with only NA values for sensors
                ind <- apply(RefData[names(RefData)!= "date"], 1, function(x) !all(is.na(x)))
                DateIN.Ref.prev  <- min(RefData[ind,"date"], na.rm = TRUE)
                DateEND.Ref.prev <- max(RefData[ind,"date"], na.rm = TRUE)
                Var.Ref.prev     <- names(RefData)
                
                # Checking if Download of RefData is necessary
                if (difftime(Sys.time(), DateEND.Ref.prev, units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.Ref = TRUE
                    # re-assign initial date for data retrieval   
                    cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. Start new reference data at : ", DateEND.Ref.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.Ref = FALSE
                    #DateEND.Ref.prev   = NULL
                    cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # RefData exists but it is NULL
                
                Retrieve.data.Ref <- TRUE
                DateIN.Ref.prev   <- NULL
                DateEND.Ref.prev  <- NULL
                Var.Ref.prev      <- NULL
                
                cat(paste0("[Check_Download] INFO, ", Ref.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        } 
        
        if (!file.exists(Influx.Rdata.file)) {
            
            # InfluxData does not exist
            ExistFil.data.Influx  = FALSE
            
            Retrieve.data.Influx  = TRUE
            DateIN.Influx.prev    = NULL
            DateEND.Influx.prev   = NULL
            cat(paste0("[Check_Download] INFO, ", Influx.Rdata.file, " does not exist. It is going to be created, sensor data will be retrieved."), sep = "\n")
            
        } else {
            
            # Influx.Rdata.file exists
            ExistFil.data.Influx  = TRUE
            
            cat(paste0("[Check_Download] INFO, ", Influx.Rdata.file, " exists."), sep = "\n")
            load(Influx.Rdata.file)
            
            if (!is.null(InfluxData)) {
                
                # InfluxData exists and is not NULL
                DateIN.Influx.prev  <- min(InfluxData$date, na.rm = TRUE)
                DateEND.Influx.prev <- max(InfluxData$date, na.rm = TRUE)
                
                # Checking if Download of InfluxData is necessary
                if (difftime(Sys.time(), max(InfluxData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.Influx  = TRUE
                    cat(paste0("[Check_Download] INFO, sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.Influx  = FALSE
                    cat(paste0("[Check_Download] INFO, no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # InfluxData exists but it is NULL
                
                Retrieve.data.Ref   = TRUE
                DateIN.Influx.prev  = NULL
                DateEND.Influx.prev = NULL
                cat(paste0("[Check_Download] INFO, ", Influx.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
        
        if (!file.exists(General.Rdata.file)) {
            
            # General.Rdata.file does not exist
            ExistFil.data.General = FALSE
            
            Retrieve.data.General  = TRUE
            DateIN.General.prev    = NULL
            DateEND.General.prev   = NULL
            cat(paste0("[Check_Download] INFO, ", General.Rdata.file, " does not exist. It is going to be created, sensor data will be retrieved."), sep = "\n")
            
        } else {
            
            # General.Rdata.file exists
            ExistFil.data.General = TRUE
            
            cat(paste0("[Check_Download] INFO, ", General.Rdata.file, " exists."), sep = "\n")
            load(General.Rdata.file)
            
            if (!is.null(General.df)) {
                
                # General.df exists and is not NULL
                DateIN.General.prev  <- min(General.df$date, na.rm = TRUE)
                DateEND.General.prev <- max(General.df$date, na.rm = TRUE)
                
                # Checking if Download of General.df is necessary
                if (difftime(Sys.time(), max(General.df$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.General  = TRUE
                    cat(paste0("[Check_Download] INFO, sensor data are going to be retrieved. Start date for data download: ", DateEND.General.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.General  = FALSE
                    cat(paste0("[Check_Download] INFO, no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # General.df exists but it is NULL
                
                Retrieve.data.General  = TRUE
                DateIN.General.prev    = NULL
                DateEND.General.prev   = NULL
                cat(paste0("[Check_Download] INFO, ", General.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
        
        if (!file.exists(SOS.Rdata.file)) {
            
            # SOS.Rdata.file does not exist
            ExistFil.data.SOS     = FALSE
            
            Retrieve.data.SOS   = TRUE
            DateIN.SOS.prev     = NULL
            DateEND.SOS.prev    = NULL
            cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " does not exist. It should be created, SOS sensor data should be retrieved."), sep = "\n")
            
        } else {
            
            # General.Rdata.file exists
            ExistFil.data.SOS     = TRUE
            
            cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " exists."), sep = "\n")
            load(SOS.Rdata.file)
            
            if (!is.null(SOSData)) {
                
                # SOSDataSOSData exists and is not NULL
                DateIN.SOS.prev      <- min(SOSData$date, na.rm = TRUE)
                DateEND.SOS.prev     <- max(SOSData$date, na.rm = TRUE)
                
                # Checking if Download of SOSData is necessary
                if (difftime(Sys.time(), max(SOSData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.SOS   = TRUE
                    cat(paste0("[Check_Download] INFO, SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.SOS   = FALSE
                    cat(paste0("[Check_Download] INFO, no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # SOSData exists but it is NULL
                
                Retrieve.data.SOS   = TRUE
                DateIN.SOS.prev     = NULL
                DateEND.SOS.prev    = NULL
                cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
        
        if (!file.exists(airsenseur.db.file)) {
            
            # airsenseur.db.file does not exist
            ExistFil.data.db      = FALSE
            
            Retrieve.data.db      = TRUE
            DateIN.db.prev        = NULL
            DateEND.db.prev       = NULL
            cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " does not exist. It should be created, SOS sensor data should be retrieved."), sep = "\n")
            
        } else {
            
            # airsenseur.db.file exists
            ExistFil.data.db      = TRUE
            
            cat(paste0("[Check_Download] INFO, ", airsenseur.db.file, " exists."), sep = "\n")
            # Checking table Dataset in airsenseur.db
            SQLite.con <- dbConnect(SQLite(), dbname = airsenseur.db.file)
            
            # Checking if the SQLite.con database and the table Dataset exists? 
            test_db <- src_sqlite(airsenseur.db.file)
            list    <- src_tbls(test_db)
            if (length(list) > 0) {
                
                cat(paste0("[Check_Download] INFO, The database ", airsenseur.db.file, " includes the table ", list[1]," with the columns: ", 
                           paste0(dbListFields(SQLite.con, list[1]), collapse = ", ") ), sep = "\n")
                
                # DataSet in airsenseur.db exists and is not NULL
                DateIN.db.prev      <- dbGetQuery(SQLite.con, paste0("SELECT min(time) FROM ", list[1]))[1,1]
                DateEND.db.prev     <- dbGetQuery(SQLite.con, paste0("SELECT max(time) FROM ", list[1]))[1,1]
                dbDisconnect(conn = SQLite.con)    
                
                # Checking if Download of InfluxData is necessary
                if (difftime(Sys.time(), DateEND.db.prev , units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.db   = TRUE
                    cat(paste0("[Check_Download] INFO, SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.db   = FALSE
                    cat(paste0("[Check_Download] INFO, no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # DataSet in airsenseur.db does not exist
                
                Retrieve.data.db      = TRUE
                DateIN.db.prev        = NULL
                DateEND.db.prev       = NULL
                cat(paste0("[Check_Download] INFO, ", airsenseur.db.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
    }
    
    # Showing DownloadSens
    print(
        list(Ref.Rdata.file     = Ref.Rdata.file, 
             Influx.Rdata.file  = Influx.Rdata.file, 
             SOS.Rdata.file     = SOS.Rdata.file,
             General.Rdata.file = General.Rdata.file,
             airsenseur.db.file = airsenseur.db.file,
             WDinput            = WDinput,
             ExistFil.data.db     = ExistFil.data.db     , Retrieve.data.db     = Retrieve.data.db     , DateIN.db.prev       = DateIN.db.prev,      DateEND.db.prev       = DateEND.db.prev,
             ExistFil.data.Ref    = ExistFil.data.Ref    , Retrieve.data.Ref    = Retrieve.data.Ref    , DateIN.Ref.prev      = DateIN.Ref.prev,     DateEND.Ref.prev      = DateEND.Ref.prev, Var.Ref.prev = Var.Ref.prev,
             ExistFil.data.Influx = ExistFil.data.Influx , Retrieve.data.Influx = Retrieve.data.Influx , DateIN.Influx.prev   = DateIN.Influx.prev,  DateEND.Influx.prev   = DateEND.Influx.prev,
             ExistFil.data.SOS    = ExistFil.data.SOS    , Retrieve.data.SOS    = Retrieve.data.SOS    , DateIN.SOS.prev      = DateIN.SOS.prev,     DateEND.SOS.prev      = DateEND.SOS.prev,
             ExistFil.data.General= ExistFil.data.General, Retrieve.data.General= Retrieve.data.General, DateIN.General.prev  = DateIN.General.prev, DateEND.General.prev  = DateEND.General.prev))
    
    cat("-----------------------------------------------------------------------------------\n")
    
    return(list(Ref.Rdata.file       = Ref.Rdata.file, 
                Influx.Rdata.file    = Influx.Rdata.file, 
                SOS.Rdata.file       = SOS.Rdata.file,
                General.Rdata.file   = General.Rdata.file,
                airsenseur.db.file   = airsenseur.db.file,
                WDinput              = WDinput,
                ExistFil.data.db     = ExistFil.data.db     , Retrieve.data.db     = Retrieve.data.db     , DateIN.db.prev       = DateIN.db.prev,      DateEND.db.prev       = DateEND.db.prev,
                ExistFil.data.Ref    = ExistFil.data.Ref    , Retrieve.data.Ref    = Retrieve.data.Ref    , DateIN.Ref.prev      = DateIN.Ref.prev,     DateEND.Ref.prev      = DateEND.Ref.prev, Var.Ref.prev = Var.Ref.prev,
                ExistFil.data.Influx = ExistFil.data.Influx , Retrieve.data.Influx = Retrieve.data.Influx , DateIN.Influx.prev   = DateIN.Influx.prev,  DateEND.Influx.prev   = DateEND.Influx.prev,
                ExistFil.data.SOS    = ExistFil.data.SOS    , Retrieve.data.SOS    = Retrieve.data.SOS    , DateIN.SOS.prev      = DateIN.SOS.prev,     DateEND.SOS.prev      = DateEND.SOS.prev,
                ExistFil.data.General= ExistFil.data.General, Retrieve.data.General= Retrieve.data.General, DateIN.General.prev  = DateIN.General.prev, DateEND.General.prev  = DateEND.General.prev))
}

# 161123 MG : Sqlite2df                 converting a local airsenseur.db into a General dataframe
Sqlite2df <- function(name.SQLite, Dataset, Influx.TZ, UserMins = NULL, DownloadSensor = NULL, Page = NULL, Complete = FALSE, asc.File=NULL) {
    # Sqlite2df transforms an airsenseur.db table into a General data frame. airsenseur.db shall be created previously with Down_Influx 
    
    # Inputs:
    # name.SQLite       : character, path of the airsenseur.Db file, it shall be in the General.data directory
    # Dataset           : character, name of the table (Dataset) in the database Db that you want to download, e. g. "AirSensEUR05"
    # Influx.TZ         : character, the time zone for variable time in Dataset of the InfluxDB
    # UserMins          : numeric, default is NULL, if UserMins is not NULL aveaging of sensor dated with UserMins averaging time is performed, the periodicity of data requested for the returned dataframe,
    # DownloadSensor    : a list with 
    #                     character, Influx.Rdata.file, the path.file/name of an existing InfluxData.Rdata file
    #                     chrater WDinput, the directory where to save Rdata and csv files
    #                     logical Retrieve.data.Influx, wether it is necessary to retrive sensor data (not used)
    #                     character DateEND.Influx.prev, last date in Influx.Rdata.file
    #                   The time zone is the one of InfluxDB and SOS (GMT). 
    #                   Default value for DownloadSensor$Influx.Rdata.file is NULL, nothing passed. In this case, SQLite2df
    #                   creates new Rdata/csv
    # Page              : numeric, default value NULL, if Null the size of the page of data to download from the influx server is LIMIT to 200000
    # complete          : Logical, default is FALSE, If TRUE the Sqlite2df function will return a dataFrame concatenating the existing data in name.Sqlite with the new ones in Values_db
    # asc.File          : dataframe, default is NULL, used for giving the correct name of the sensor
    # 
    # Return            : A Values_db (existing data added if any) dataframe with date (as.POSIXct) to be used by openair, coordinates
    #                     , 7 sensor values as downloaded from Influx. Data are averaged with UserMins averaging time if Averaging is TRUE
    # Dependence        : Load.Packages
    ### Still need adding when the AirSensEUR is switched on and off, when the name of sensors are changed and when it is at the Reference Stations
    
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("tidyverse", "data.table")
    Load.Packages(list.Packages)
    
    #------------------------------------------------------------------------------CR
    # Set time interval - Not used anymore this is commented
    #------------------------------------------------------------------------------CR
    #minSec <- UserMins*60.
    
    # set range time for data retrieving, either from origin or last date in previous DataFrame
    #if (any(grepl(pattern = "DateIN.0.General.prev", x = objects(DownloadSensor)))) {
    #    DateIN  <- DownloadSensor$DateIN.0.General.prev + minSec
    #} else DateIN  <- as.POSIXct("2015-12-01 00:00.00 UTC")
    # Setting end date to curent date
    #DateEND <- as.POSIXct(Sys.Date())
    #
    # Set time interval, with function interval of package lubridate
    # InterVal <- lubridate::interval(DateIN, DateEND, tzone= "UTC")
    # Creating the General Data.Frame
    # General <- data.frame(date = seq(InterVal@start, length = InterVal@.Data/minSec, by = paste0(toString(UserMins)," ","min")),                     
    #                      row.names = NULL, check.rows = FALSE,
    #                      check.names = TRUE,
    #                     stringsAsFactors = FALSE)
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("sqldf", "openair", "reshape")
    Load.Packages(list.Packages)
    
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? creating the db or just the connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) # airsenseur.db exists
        cat(paste0("[Sqlite2df] INFO, ", name.SQLite, " exists."), sep = "\n") else  # airsenseur.db does not exist
            stop(cat(paste0("[Sqlite2df] INFO, ", name.SQLite, " does not exist. The script is stopped."), sep = "\n"))
    
    #------------------------------------------------------------------------------CR
    # Checking table Dataset in airsenseur.db
    #------------------------------------------------------------------------------CR
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    
    # Checking if the SQLite.con database and the table Dataset exists? 
    if (dbExistsTable(SQLite.con, Dataset)) {
        
        cat(paste0("[Sqlite2df] INFO, The database ", name.SQLite, " includes the table ", Dataset," with the columns: ", 
                   paste0(dbListFields(SQLite.con, Dataset), collapse = ", ") ), sep = "\n")
        
    } else stop(cat(paste0("[Sqlite2df] ERROR There is no table called ", Dataset, " in ", name.SQLite, ". The scipt is stoped."), sep = "\n"))
    
    #------------------------------------------------------------------------------CR
    # Reading local airsenseur.db in slice of Page records - checking if InfluxData.Rdata and InfluxData.csv already exist to only add the necessary data
    #------------------------------------------------------------------------------CR
    cat(paste0("[SQLite2df] INFO, reading table ", Dataset), sep = "\n")
    # Initial values
    Download.N  <- 0
    SQL.Total.N     <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    if (!is.null(DownloadSensor$DateEND.Influx.prev)) { # the table Dataset exists in airsenseur.db
        
        cat(paste0("[SQLite2df] INFO, the files InfluxData.Rdata and InfluxData.csv already exist."), sep = "\n")
        
        # Counting the row number where to add records in AirSensEUR$Dataset
        cat(paste0("[SQLite2df] INFO, looking for the first data in airsenseur.db to add to InfluxData.Rdata. This can be very long with large datasets ... TZ must be set"), sep = "\n")
        if (!exists("Influx.TZ")) stop(cat("[SQLite2df] ERROR: you must set the parameter TZ in the ASEConfig_xx.R file. The script is stopped."))
        # changing FirstDate timezone from the value in DownloadSensor to the local timezone called Influx.TZ
        FirstDate <- as.POSIXct(DownloadSensor$DateEND.Influx.prev, tz = "UTC", usetz = TRUE) # attr(FirstDate, "tzone") <- "UTC"
        FirstDate <- format(FirstDate, tz = Influx.TZ, usetz = TRUE)
        Dataset.N <- dbGetQuery(SQLite.con, paste0("SELECT min(rowid) FROM ", Dataset, " WHERE datetime(time) >= '", # >= instead of > to recalculate the last average
                                                   FirstDate,"';"))[1,1]
        #Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT rowid FROM ", Dataset, " WHERE datetime(time) > '2016-11-14 17:18:00';"))
        if (is.na(Dataset.N)) return(cat("[SQLite2df] ERROR, there are no new data in airSenseur.db to add to InfluxData.Rdata and InfluxData.csv. The script is stopped"))
        
    } else {# the table Dataset exists in airsenseur.db
        
        # There are no records in AirSensEUR$Dataset
        cat(paste0("[SQLite2df] INFO, files InfluxData.Rdata and InfluxData.csv do not exist. Reading all data of airsenseur.db."), sep = "\n")
        Dataset.N  <- 0
        
    } 
    #dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON ",Dataset, "(time);"))
    
    # getting the default Page of data to download
    if (is.null(Page)) Page <- 200000
    
    while (Download.N < (SQL.Total.N - Dataset.N)) {
        
        # For the last Page to read used modulo instead of Page in order not to download twice the same records
        if ((Download.N + Page) > (SQL.Total.N - Dataset.N)) {Old_Page <- Page ; Page <- (SQL.Total.N - Dataset.N) %% Page } 
        
        # Downloading a slice Page
        Adding  <- dbGetQuery(SQLite.con, paste0("SELECT time, boardTimeStamp, gpsTimestamp, 
                                                 channel, name, 
                                                 cast(altitude AS DECIMAL) AS altitude, 
                                                 cast(latitude AS DECIMAL) AS latitude, 
                                                 cast(longitude AS DECIMAL) AS longitude, 
                                                 sampleEvaluatedVal
                                                 FROM ", Dataset, " LIMIT ", format(Page, scientific = FALSE), 
                                                 " OFFSET ", format(Dataset.N + Download.N, scientific = FALSE)
        ))
        #browser()
        
        # adding data to Values_db
        cat(paste0("[SQLite2df] INFO, ", format(Download.N + nrow(Adding), scientific = FALSE), "/",(SQL.Total.N - Dataset.N)," records were read "), sep = "\n")
        if (exists("Values_db")) Values_db <- rbind.fill(Values_db, Adding) else Values_db <- Adding
        # updating counter
        Download.N  <- Download.N + Page
        
    }
    # resuming Page for tabulating values
    if (exists("Old_Page")) Page <- Old_Page
    # dbReadTable(SQLite.con, Dataset) # dbReadTable loses the decimal of coordinates
    cat(paste0("[SQLite2df] INFO, Disconnecting ", name.SQLite), sep = "\n")
    cat(paste0("[SQLite2df] INFO, The table ", Dataset, " has ", nrow(Values_db), " new records and ", length(unique(Values_db$time)), " new unique TimeStamps"), sep = "\n")
    # Closing connection
    dbDisconnect(SQLite.con)
    rm(Adding, Download.N, SQL.Total.N)
    
    #------------------------------------------------------------------------------CR
    # Defining names and variables for meteo
    #------------------------------------------------------------------------------CR
    Meteo.names.change  <- data.frame(Influx.names  = c("Humid", "Tempe","Temp","Press"), 
                                      General.names = c("Relative_humidity", "Temperature", "Temperature", "Atmospheric_pressure"), stringsAsFactors = FALSE)
    #------------------------------------------------------------------------------CR
    # Adding final name (Temperature, Relative ...) 
    #------------------------------------------------------------------------------CR
    Channel.names <- unique(Values_db[,c("channel","name")])
    for (i in Meteo.names.change$Influx.names) {
        
        if (i %in% Channel.names$name) {
            
            # setting column variables in Channel.names with names of Meteo.names.change
            Channel.names[Channel.names$name == i,"Variables"] <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"]  
            
            # setting correct colnames in values_db for Meteo.names.change if chang are requested
            Sensor.rows <- which(Values_db$name==i)
            Values_db[Sensor.rows,"Pollutants"]   <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"] 
            #Values_db[Sensor.rows,"name"]         <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"] 
        }
    } 
    
    cat(paste0("[SQLite2df] INFO, looking for the names of all sensors and channel numbers from a list of sensor names, using best guess"), sep = "\n")
    if (is.null(asc.File)) {
        
        # In case of several sensor name on the same channel number
        for (i in Channel.names$channel) {
            
            # Taking the last name if sensors have been replaced
            if (anyDuplicated.array(Channel.names[Channel.names$channel == i-1,c("channel","name")])) {
                Channel.names$name[i+1] <- paste0(unique(Values_db[Values_db$channel == i, "name"]),collapse = "!")
                if (grepl(pattern = "!", x = Channel.names$Sensor.names[i+1])) {
                    
                    Channel.names$Sensor.names[i+1] <- tail(unlist(strsplit(Channel.names$name[i+1], split = "!")), n = 1)
                    cat(paste0("[SLite2df] WARNING the name of the sensor of channel ", i, " has been changed. The script is assuming that the sensor model type was not changed during use and it is  ", Channel.names$Sensor.names[i+1], ", the last one."), sep = "\n")
                    
                }
            }
        }
    } else { 
        
        # Set Channel.names$name, Values_db$name and Values_db$Pollutants based on the shield config file of the chemical sensor board. Sensor shall be in channel 1, 2, 3 and 4!!!
        for (i in which(!is.na(asc.File$name.sensor))) { 
            
            # Setting name of sensor from file base on channel number - 1
            cat(paste0("[SLite2df] INFO setting name.sensor and gas.sensor using the shield config file for sensor ", i), sep = "\n")
            Sensor.rows                             <- which(Values_db$channel==(i-1))
            if (length(Sensor.rows) > 0) {
                
                Values_db[Sensor.rows,"name"]       <- asc.File$name.sensor[i]
                Values_db[Sensor.rows,"Pollutants"] <- asc.File$gas.sensor[i]
            }
            
            # updating the model of sensor in df Channel.names corresponding to sensor in asc.File based on channel number
            Channel.names[Channel.names$channel==i-1 ,"name"] <- asc.File$name.sensor[i]
        }
    }
    
    # Defining names and variables for gas sensors - Used the same names of variables as in SOS for compatibility reasons
    Sensor.names        <- list(Nitrogen_dioxide      = c("no2_b43f", "NO2-B43F", "NO2B43F", "NO2/C-20", "NO23E50", "NO2_3E50", "NO2", "S1"),
                                Carbon_monoxide       = c("CO-B4", "CO-A4", "CO/MF-200", "CO/MF-20", "CO-MF200", "CO3E300", "CO_3E300", "CO","COMF200","CO-A4 O","COA4", "S2"),
                                Ozone                 = c("O3/M-5", "O3-B4", "AX-A431", "OX-A431", "OX_A431", "O3-A431", "O33EF1", "O3_3E1F", "O3", "O3E100", "o3_m_5", "O3_M5", "O3-M5", "S3"),
                                Nitric_oxide          = c("NO-B4", "NOB4_P1","NOB4", "NO/C-25", "NO3E100", "NO_3E100", "NO", "No Sensor", "S4"),
                                Particulate_Matter_1  = c("OPCN2PM1"  , "OPCN3PM1"), 
                                Particulate_Matter_25 = c("OPCN2PM25" , "OPCN3PM25"),
                                Particulate_Matter_10 = c("OPCN2PM10" , "OPCN3PM10"),
                                Bin0                  = c("OPCN2Bin0" , "OPCN3Bin0"),
                                Bin1                  = c("OPCN2Bin1" , "OPCN3Bin1"),
                                Bin2                  = c("OPCN2Bin2" , "OPCN3Bin2"),
                                Bin3                  = c("OPCN2Bin3" , "OPCN3Bin3"),
                                Bin4                  = c("OPCN2Bin4" , "OPCN3Bin4"),
                                Bin5                  = c("OPCN2Bin5" , "OPCN3Bin5"),
                                Bin6                  = c("OPCN2Bin6" , "OPCN3Bin6"),
                                Bin7                  = c("OPCN2Bin7" , "OPCN3Bin7"),
                                Bin8                  = c("OPCN2Bin8" , "OPCN3Bin8"),
                                Bin9                  = c("OPCN2Bin9" , "OPCN3Bin9"),
                                Bin10                 = c("OPCN2Bin10", "OPCN3Bin10"),
                                Bin11                 = c("OPCN2Bin11", "OPCN3Bin11"),
                                Bin12                 = c("OPCN2Bin12", "OPCN3Bin12"),
                                Bin13                 = c("OPCN2Bin13", "OPCN3Bin13"),
                                Bin14                 = c("OPCN2Bin14", "OPCN3Bin14"),
                                Bin15                 = c("OPCN2Bin15", "OPCN3Bin15"),
                                Bin16                 = c("OPCN3Bin16"),
                                Bin17                 = c("OPCN3Bin17"),
                                Bin18                 = c("OPCN3Bin18"),
                                Bin19                 = c("OPCN3Bin19"),
                                Bin20                 = c("OPCN3Bin20"),
                                Bin21                 = c("OPCN3Bin21"),
                                Bin22                 = c("OPCN3Bin22"),
                                Bin23                 = c("OPCN3Bin23"),
                                OPCHum                = c("OPCN3Hum"),
                                OPCLsr                = c("OPCN3Lsr"),
                                OPCTsam               = c("OPCN3TSam"),
                                OPCVol                = c("OPCN2Vol" , "OPCN3Vol"),
                                OPCTemp               = c("OPCN2Temp", "OPCN3Temp"),
                                MOx                   = c("MOX"),
                                Carbon_dioxide        = c("D300"),
                                Bin1_PMS              = c("53PT003"),
                                Bin2_PMS              = c("53PT005"),
                                Bin3_PMS              = c("53PT010"),
                                Bin4_PMS              = c("53PT025"),
                                Bin5_PMS              = c("53PT050"),
                                Bin6_PMS              = c("53PT100"),
                                PM1_PMSraw            = c("5301CST"),
                                PM1_PMSCal            = c("5301CAT"),
                                PM25_PMSraw           = c("5325CST"),
                                PM25_PMSCal           = c("5325CAT"),
                                PM10_PMSraw           = c("5310CST"),
                                PM10_PMSCal           = c("5310CAT")
                                
    ) # Add new sensor model to be recognized if needed
    
    #------------------------------------------------------------------------------CR
    # Adding sensor model type (Nitic_Oxide...) if more than 1 model of sensors then the model type are separated with "!" - 
    # The last sensor mdel type is used
    #------------------------------------------------------------------------------CR
    for (i in 1:length(Sensor.names)) {
        
        for (j in 1: length(Sensor.names[[i]])) {
            
            if (Sensor.names[[i]][j] %in% Channel.names$name) {
                
                Channel.names[which(Channel.names$name == Sensor.names[[i]][j]), "Variables"] <- names(Sensor.names)[i]
                break
            } 
        }
    }
    
    cat("[SQLite2df] INFO, sensors found in the airsenseur.db\n")
    print(cbind(Channel.names, lubridate::ymd_hms(Values_db$time[as.numeric(row.names(Channel.names))]) ))
    
    # Setting Values_db$Pollutants that gives the correct Polluants names even if the sensors are changed of position during use, not for chemical sensors, already done
    for (i in unique(Channel.names$name)) {
        
        # setting correct colnames in values_db for Meteo.names.change if chang are requested
        cat(paste0("[SLite2df] INFO setting Values_db$Pollutants to ", unique(Channel.names[Channel.names$name == i,"Variables"])," using the shield config file for sensor ", i), sep = "\n")
        Sensor.rows <- which(Values_db$name==i)
        if (length(Sensor.rows) > 0) {
            
            Values_db[Sensor.rows,"Pollutants"]   <- unique(Channel.names[Channel.names$name == i,"Variables"])
            #Values_db[Sensor.rows,"name"]         <- unique(Channel.names[Channel.names$name == i,"Variables"])
        }
    } 
    
    # Checking if some sensors were not recognized before aggregating, these data are discarded
    if (any(is.na(Values_db$Pollutants))) {
        
        cat("[SQLite2df] ERROR, At least one sensor name was not recognized. Check variable Sensor.names in function SQLite2df.\n")
        is.NA <- which(is.na(Values_db$Pollutants))
        Name.is.NA <- unique(Values_db[is.NA,"name"])
        for (i in Name.is.NA) {
            Channel.is.NA   <- unique(Values_db[Values_db$name == i, "channel"])
            min.time.is.NA <- min(lubridate::ymd_hms(Values_db[Values_db$channel == Channel.is.NA,"time"]), na.rm = T)
            max.time.is.NA <- max(lubridate::ymd_hms(Values_db[Values_db$channel == Channel.is.NA,"time"]), na.rm = T)
            cat(paste0("[SQLite2df] ERROR, sensor name ",i," channel number ", Channel.is.NA, " is not recognized between ", min.time.is.NA, " and ", max.time.is.NA, " these data are deleted\n"))
        }
        Values_db <- Values_db[-is.NA,]
    } 
    
    #------------------------------------------------------------------------------CR
    # Putting data in tabulated dataframe
    #------------------------------------------------------------------------------CR
    cat("[SQLite2df] INFO, Putting data in tabulated form and aggregatting on the time column values, this can be long with large datasets\n")
    # removing sampleEvaluatedVal, name and channel. Aggregating in tabulated form. Discarding 0s in coordinates and altitude to avoid error when averaging
    Values_db[which(Values_db$altitude  == 0), "altitude"]  <- rep(NA, length(which(Values_db$altitude  == 0)))
    Values_db[which(Values_db$longitude == 0), "longitude"] <- rep(NA, length(which(Values_db$longitude == 0)))
    Values_db[which(Values_db$latitude  == 0), "latitude"]  <- rep(NA, length(which(Values_db$latitude  == 0)))
    
    # Aggregating in tabulated form.
    i <- 0
    if (nrow(Values_db) < Page) Page <- nrow(Values_db)
    while (i < nrow(Values_db)) {
        # Checking for a correct Page value for paging
        if ((i + Page) > nrow(Values_db)) Page <- nrow(Values_db) - i
        cat(paste0("[SQLite2df] INFO, aggregating airsenseur.db in tabulated rows ", format(i + Page, scientific = FALSE),"/", nrow(Values_db)), sep = "\n" )
        # casting data according to channel names
        # Buffer <- cast(Values_db[(i + 1):(i + Page),], time + boardTimeStamp + gpsTimestamp + altitude + latitude + longitude  ~ Pollutants, 
        #                value = "sampleEvaluatedVal", fun.aggregate = 'mean' , fill = NA, na.rm = TRUE)
        Buffer <- spread(data = Values_db[(i + 1):(i + Page), c("time", "boardTimeStamp","gpsTimestamp", "altitude", "latitude", "longitude", "Pollutants", "sampleEvaluatedVal")], 
                         key = Pollutants, value = sampleEvaluatedVal) %>% arrange(time)
        
        # aggregating in Values_db_cast
        # if (exists("Values_db_cast")) Values_db_cast <- rbind.fill(Values_db_cast, Buffer) else Values_db_cast <- Buffer
        if (exists("Values_db_cast")) Values_db_cast <- rbindlist(list(Values_db_cast, Buffer), use.names = TRUE, fill = TRUE) else Values_db_cast <- Buffer
        i <- i + Page
    }
    Values_db <- data.frame(Values_db_cast)
    
    #for (i in Channel.names$channel) colnames(Values_db)[which(colnames(Values_db) ==i)] <- Channel.names$Variables[which(Channel.names$channel == i)]
    remove(Values_db_cast, Buffer)
    remove(Sensor.names, Channel.names)
    remove(Meteo.names.change)
    
    # Transforming column time in POSIX with the correct time zone (UTC), changing name to date
    # I thought that all values from Influx were in tz UTC but it rather seems that they are in local time
    #browser()
    if (is.null(Influx.TZ)) {
        cat("[SQLite2df] INFO, Converting Values_db$time from character to POSIX format, ERROR time zone is not set for InfluxDB.\n")
        cat("[SQLite2df] ERROR: you must set the parameter TZ in the ASEConfig_xx.R file. The script should be  stopped.")
    } else{
        cat(paste0("[SQLite2df] INFO, Converting Values_db$time from character to POSIX format, timezone is ", Influx.TZ), sep = "\n")
        #Values_db$time <- as.POSIXct(strptime(Values_db$time, format = "%Y-%m-%d %H:%M:%S", tz = Influx.TZ)) 
        Values_db$time <- lubridate::ymd_hms(Values_db$time, tz = Influx.TZ)
    }
    # convert Values_DB$time to UTC to be consistent with reference data
    #if (any(base::format(Values_db$time, format= "%Z") != "UTC")) attr(Values_db$time, "tzone") <- "UTC"
    
    # Change to date to use OpenAir
    names(Values_db)[colnames(Values_db) == "time"] <- "date"
    
    #browser()
    # Averaging with UserMIns averaging time, creating Values_db_Mins
    if (!is.null(UserMins)) {
        
        cat(paste0("[SQLite2df] INFO, averaging each ", UserMins, " mins. This can be long with large datasets."), sep = "\n")
        
        # we will have to wait UserMins mins to have new values
        i <- lubridate::floor_date(Values_db$date[1],  unit = paste0(toString(UserMins)," ","min"))
        while (difftime(max(Values_db$date), i, units = "mins") > UserMins) {
            #browser()
            cat(paste0("[SQLite2df] INFO, timeAverage of Values_db on ", i), sep = "\n" )
            SelectedRows    <- which(Values_db$date >= i & Values_db$date < i + 86400) # taking interval of one day in seconds
            if (length(SelectedRows) > 0) { # avoid the error of timeAverage when there are 0 rows
                
                # timeAverage with UserMins
                # Using SelectByDate creates troubles with the class of i (Posix or string of charater), it creates a tibble
                # subset looks without problem but on line it is suggested not to use it
                # not using boardTimeStamp in the time average, in case of 2 shields or more the boardTimeStamp values may be completely independent and different
                # Adding boardTimesTamp afterwards
                #browser()
                Real.Sensors        <- colnames(Values_db)[!colnames(Values_db) %in% c("date","boardTimeStamp", "gpsTimestamp", "altitude", "latitude", "longitude")]
                SelectedColumns     <- colnames(Values_db)[which(colnames(Values_db) != "boardTimeStamp")]
                SelectedRowsSensors <- as.numeric(row.names(Values_db[SelectedRows,])[as.vector(rowSums(!is.na(Values_db[SelectedRows, Real.Sensors])) > 0)])
                if (length(SelectedRowsSensors) > 0 ) {
                    
                    Buffer <- timeAverage( # selectByDate(Values_db, 
                        #              start = lubridate::as_date(i), 
                        #              end   = lubridate::as_date(i), 
                        #              hour = 0:23
                        # ),
                        mydata     = Values_db[SelectedRowsSensors, SelectedColumns], # SelectedColumns, the -2 should be boardTimeStamp
                        avg.time   = paste0(toString(UserMins)," ","min"), 
                        statistic  = "mean", 
                        start.date = lubridate::floor_date(i,  unit = paste0(toString(UserMins)," ","min")), 
                        fill = FALSE
                    )
                    
                    # Adding the boardTimesStamp only of the chemical shield to avoid confusion with the boardTimeStamp of the other shield
                    # Selecting only the pollutants of asc.File to avoid to average with timeBoardStamps of other shield (OPC)
                    SelectedRowsStamp <- as.numeric(row.names(Values_db[SelectedRowsSensors,])[as.vector(rowSums(!is.na(Values_db[SelectedRowsSensors, asc.File$gas.sensor])) >0)])
                    if (length(SelectedRowsStamp) == 0) {
                        
                        SelectedRowsStamp <- as.numeric(row.names(Values_db[SelectedRowsSensors,])[as.vector(rowSums(is.na(Values_db[SelectedRowsSensors, asc.File$gas.sensor])) >0)])
                    } 
                    
                    boardTimeStamp    <- timeAverage( mydata     = Values_db[SelectedRowsStamp,c("date",'boardTimeStamp')], 
                                                      avg.time   = paste0(toString(UserMins)," ","min"), 
                                                      statistic  = "mean", 
                                                      start.date = lubridate::floor_date(i,  unit = paste0(toString(UserMins)," ","min")), 
                                                      fill = FALSE)
                    
                    # Merging Buffer and boardTimeStamp
                    Buffer <- merge(x = Buffer, y = boardTimeStamp, by = "date", all.x = TRUE )
                    rm(boardTimeStamp)
                    
                    # aggregating in Values_db_cast
                    if (exists("Values_db_Mins")) Values_db_Mins <- rbind.fill(Values_db_Mins, Buffer) else Values_db_Mins <- Buffer
                    rm(Buffer)
                }
            }
            i <- i + 86400
            
        }
        # we could use package data.table
    } else stop(cat(paste0("[SQLite2df] ERROR, UserMins is not set in ASEConfig_xx.R. Please set it, default 10 mins, the script is stopped."), sep = "\n"))
    
    #browser()
    # returning data if any
    if (exists("Values_db_Mins")) {
        if (nrow(Values_db_Mins)>0) {
            
            # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
            if (file.exists(DownloadSensor$Influx.Rdata.file) & Complete) { 
                
                load(DownloadSensor$Influx.Rdata.file)
                # avoiding the last common date beetween InfluxData and Values_db
                if (InfluxData[nrow(InfluxData),"date"] == Values_db[1,"date"])
                    Values_db_Mins <- rbind.fill(InfluxData[1:(nrow(InfluxData)-1),], Values_db_Mins) else Values_db <- rbind.fill(InfluxData, Values_db_Mins)
            }
            
            # We coud use data.table if we need speeding up
            #TimeStampDT         <- data.table(TimeStamp)
            #TimeStamp           <- as.data.frame(TimeStampDT)
            #TimeStampDT <- TimeStampDT[,.(time, gpsTimestamp = mean(gpsTimestamp, na.rm = TRUE), boardTimeStamp = mean(boardTimeStamp, na.rm = TRUE), 
            #                              latitude = mean(latitude, na.rm = TRUE),longitude = mean(longitude, na.rm = TRUE),altitude = mean(altitude, na.rm = TRUE)), by = .(time)]
            
            #=====================================================================================CR
            #=== 3. Ploting - Enter your time slot (From and To) - preferably only the new data
            #=====================================================================================CR
            # Select the time period you want to plot: only the sensor date added to InfluxData
            # in the future try to use timeSpan from lubridate TimeSpan <- as.POSIXct("2015-12-21 01:00:00") %--% as.POSIXct("2016-01-20 01:00:00")
            From <- min(Values_db_Mins$date, na.rm = TRUE)
            To   <- max(Values_db_Mins$date, na.rm = TRUE)
            
            cat("[SQLite2df] INFO, preparing time series to plot")
            # timePlot(selectByDate(Values_db_Mins, start = From, end = To), 
            #          pollutant = Channel.names$Variables, y.relation = "free", date.pad = TRUE, auto.text = FALSE)
            
            cat("[SQLite2df] INFO, returning dataframe with sensor data in column with 1st column named 'date'\n")
            print(str(Values_db_Mins))
            return(Values_db_Mins)
            
        } else {
            #S There is no Values_db_Mins
            cat("[SQLite2df] WARNING, no new INfluxDB data downloaded to be tabulated and averaged. Adding existing InFuxData.Rdata is existing.\n")
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
            if (file.exists(DownloadSensor$Influx.Rdata.file) & Complete) { 
                load(DownloadSensor$Influx.Rdata.file)
                return(InfluxData)
            }
        }
    } else {
        #S There is no Values_db_Mins
        cat("[SQLite2df] WARNING, no new INfluxDB data downloaded to be tabulated and averaged. Adding existing InFuxData.Rdata is existing.\n")
        # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
        cat("-----------------------------------------------------------------------------------\n")
        cat("\n")
        if (file.exists(DownloadSensor$Influx.Rdata.file) & Complete) { 
            load(DownloadSensor$Influx.Rdata.file)
            return(InfluxData)
        }
    }
    #browser()
}

# 170609 MG : Reading the sensor config file
ASEPanel04Read <- function(ASEPanel04File = NULL , dirASEPanel = c("AirSensEURPanelR04"), dirCurrent = getwd()) {
    # This function read the config parameters of the AirSensEURPanel version 0.4 - All values are in Hexadecimal and need be converted
    # ASEPanel04File    : the filepath to be read, character vector
    # return            : a dataFrame SensorConfig with all sensor parameters of a AirSensEURPanel config file
    # dependences       : Load.Packages
    # 
    # Returns a data frame with sensor config parameters
    
    #browser()
    ASEFile <- read.table(file = ASEPanel04File, header = FALSE, sep = ":", stringsAsFactors = FALSE)
    
    # dataFrame of sensor config
    # typical order of sensors, that may be changed after reading the the shield config file
    name.gas              <- c("NO2", "CO" , "O3",  "NO" ) 
    sens2ref                <- data.frame(name.gas          = name.gas,
                                          Ref               = c(-999,-999,-999,-999),
                                          RefAD             = c(-999,-999,-999,-999),
                                          RefAFE            = c(-999,-999,-999,-999),
                                          check.names       = FALSE, 
                                          stringsAsFactors  = FALSE)
    
    # load packages for alphanumeric operations
    Load.Packages("BMS")
    
    for (i in 1:4) {
        # Name of Sensor 
        Command <- ASEFile[which(ASEFile[,2] == paste0("Write Preset Name for channel ",i-1))[1], 1]
        # Discarding curly bracket at the begining and end of the string under linux
        Command <- gsub("[{}]", "", Command)
        # discarding W at the begining and end
        Command <- substring(Command,2, nchar(Command))
        hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
        # discarding "00" trayling elements
        if (hCommand[1]                == "00") hCommand <- hCommand[-1]
        if (hCommand[length(hCommand)] == "00") hCommand <- hCommand[-length(hCommand)]
        # https://stackoverflow.com/questions/29251934/how-to-convert-a-hex-string-to-text-in-r
        sens2ref$name.sensor[i] <- gsub('[^[:print:]]+', '', rawToChar(as.raw(strtoi(hCommand, 16L)))) 
        
        # LMP9100 Register 
        Command <- ASEFile[which(ASEFile[,2] == paste0("LMP9100 Register Setup for channel ",i-1))[1], 1]
        # Discarding curly bracket at the begining and end of the string under linux
        Command <- gsub("[{}]", "", Command)
        # discarding R at the begining 
        Command <- substring(Command,2, nchar(Command))
        hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
        #sens2ref$TIA[i]    <- hCommand[2] not useful and confusing
        # The bits seem to be in opposite order as written in the datasheet
        if (all(hex2bin(hCommand[2])[7:8] == c(0,0)))     sens2ref$Rload[i]     <- 10
        if (all(hex2bin(hCommand[2])[7:8] == c(0,1)))     sens2ref$Rload[i]     <- 33
        if (all(hex2bin(hCommand[2])[7:8] == c(1,0)))     sens2ref$Rload[i]     <- 50
        if (all(hex2bin(hCommand[2])[7:8] == c(1,1)))     sens2ref$Rload[i]     <- 100
        if (all(hex2bin(hCommand[2])[4:6] == c(0,0,0)))   sens2ref$TIA_Gain[i]  <- 1000000
        if (all(hex2bin(hCommand[2])[4:6] == c(0,0,1)))   sens2ref$TIA_Gain[i]  <- 2750
        if (all(hex2bin(hCommand[2])[4:6] == c(0,1,0)))   sens2ref$TIA_Gain[i]  <- 3500
        if (all(hex2bin(hCommand[2])[4:6] == c(0,1,1)))   sens2ref$TIA_Gain[i]  <- 7000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,0,0)))   sens2ref$TIA_Gain[i]  <- 14000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,0,1)))   sens2ref$TIA_Gain[i]  <- 35000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,1,0)))   sens2ref$TIA_Gain[i]  <- 120000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,1,1)))   sens2ref$TIA_Gain[i]  <- 350000
        sens2ref$GAIN[i] <- 1 + (sens2ref$TIA_Gain[i]/sens2ref$Rload[i])
        
        #sens2ref$REF[i] <- hCommand[3] not useful and confusing
        # The bits seem to be in opposite order as written in the datasheet
        if (all(hex2bin(hCommand[3])[1]   == c(0)))       sens2ref$Ref_Source[i]  <- "Internal"
        if (all(hex2bin(hCommand[3])[1]   == c(1)))       sens2ref$Ref_Source[i]  <- "External"
        if (all(hex2bin(hCommand[3])[2:3] == c(0,0)))     sens2ref$Int_Z[i]       <- 0.20
        if (all(hex2bin(hCommand[3])[2:3] == c(0,1)))     sens2ref$Int_Z[i]       <- 0.50
        if (all(hex2bin(hCommand[3])[2:3] == c(1,0)))     sens2ref$Int_Z[i]       <- 0.67
        if (all(hex2bin(hCommand[3])[2:3] == c(1,1)))     sens2ref$Int_Z[i]       <- 11
        if (all(hex2bin(hCommand[3])[4]   == c(0)))       sens2ref$Bias_Sign[i]   <- -1
        if (all(hex2bin(hCommand[3])[4]   == c(1)))       sens2ref$Bias_Sign[i]   <- 1
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,0,0))) sens2ref$Bias[i]   <- 0
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,0,1))) sens2ref$Bias[i]   <- 0.01
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,1,0))) sens2ref$Bias[i]   <- 0.02
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,1,1))) sens2ref$Bias[i]   <- 0.04
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,0,0))) sens2ref$Bias[i]   <- 0.06
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,0,1))) sens2ref$Bias[i]   <- 0.08
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,1,0))) sens2ref$Bias[i]   <- 0.10
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,1,1))) sens2ref$Bias[i]   <- 0.12
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,0,0))) sens2ref$Bias[i]   <- 0.14
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,0,1))) sens2ref$Bias[i]   <- 0.16
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,1,0))) sens2ref$Bias[i]   <- 0.18
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,1,1))) sens2ref$Bias[i]   <- 0.20
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,0,0))) sens2ref$Bias[i]   <- 0.22
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,0,1))) sens2ref$Bias[i]   <- 0.24
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,1,0))) sens2ref$Bias[i]   <- 0
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,1,1))) sens2ref$Bias[i]   <- 0
        
        if (substring(hCommand[4],1,1) ==0) sens2ref$Fet_Short[i] <- "Disabled"
        if (substring(hCommand[4],1,1) ==1) sens2ref$Fet_Short[i] <- "Enabled"
        sens2ref$Mode[i]      <- substring(hCommand[4],2,2)
        
        # DAC5694R Register 
        for (j in 1:3) {
            Command <- ASEFile[which(ASEFile[,2] == paste0("DAC5694R Register Setup for channel ",i-1," subchannel ",j-1))[1], 1]
            # Discarding curly bracket at the begining and end of the string under linux
            Command <- gsub("[{}]", "", Command)
            # discarding W atthe begining
            Command <- substring(Command,2, nchar(Command))
            # Discarding curly bracket at the begining and end of the string under linux
            Command <- gsub("[{}]", "", Command)
            hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
            if (hCommand[5] == "00") Vtotal = 2.5 else Vtotal = 5 
            if (j==1) sens2ref$Ref[i]     <- as.numeric(strtoi(paste0(hCommand[3],hCommand[4]), base = 16L)/4095 * Vtotal)
            if (j==2) sens2ref$RefAD[i]   <- as.numeric(strtoi(paste0(hCommand[3],hCommand[4]), base = 16L)/4095 * Vtotal)
            if (j==3) sens2ref$RefAFE[i]  <- as.numeric(strtoi(paste0(hCommand[3],hCommand[4]), base = 16L)/4095 * Vtotal)
        }
        # Calculate the zero signal and Bias voltage
        sens2ref$board.zero.set[i] <- sens2ref$Int_Z[i] * sens2ref$RefAFE[i]
        sens2ref$BIAIS[i]          <- sens2ref$Bias_Sign[i] * sens2ref$Bias[i] * sens2ref$RefAFE[i]
    }
    
    # Find correct name of compounds according to the name of sensor
    name.gas <- list(NO2 = c("NO2", "no2_b43f","NO2-B43F", "NO2B43F", "NO2/C-20"                                  , "NO23E50", "NO2_3E50"         , "S1"),
                     CO  = c("CO" , "CO-B4", "CO-A4","COA4"         , "CO/MF-200", "CO/MF-20", "COMF200"          , "CO3E300", "CO_3E300"         , "S2"),
                     O3  = c("O3" , "O3-A431","OX_A431", "O3-B4", "OX-A431", "AX-A431", "O3/M-5", "o3_m_5","O3_M5", "O33EF1" , "O3E100" ,"O3_3E1F", "S3"),
                     NO  = c("NO" , "NOB4_P1","NOB4","NO-B4"        , "NO/C-25"                                   , "NO3E100", "NO_3E100"         , "S4")
    ) # Add new sensor model to be recognized if needed
    # Finding the sensor model 
    for (i in 1:length(sens2ref$name.gas)) for (j in 1:length(name.gas)) {
        if (any(sens2ref$name.sensor[i] %in% name.gas[[j]])) {sens2ref$name.gas[i] <- names(name.gas)[j]; break()}
    } 
    # Adding gas.sensor for use with SOS protocol
    gas.sensor.df <- data.frame(Nitrogen_dioxide  = c("NO2"),
                                Carbon_monoxide   = c("CO"),
                                Ozone             = c("O3"),
                                Nitric_oxide      = c("NO"), # this name is wrong but Alex used it in SOS
                                stringsAsFactors  = FALSE
    ) # Add new compound to be recognized if needed
    for (i in 1:length(sens2ref$name.gas)) sens2ref[i,"gas.sensor"] <- names(gas.sensor.df)[which(gas.sensor.df[1,]==sens2ref$name.gas[i])]
    
    # reordering as c("NO2","CO" , "O3",  "NO")
    # sens2ref <- cbind(sens2ref[,which(names(sens2ref) == "NO2")],
    #                   sens2ref[,which(names(sens2ref) == "CO")],
    #                   sens2ref[,which(names(sens2ref) == "O3")],
    #                   sens2ref[,which(names(sens2ref) == "NO")])
    
    print(sens2ref, quote = FALSE)
    return(sens2ref)
}

INFLUXDB <- function(WDoutput,DownloadSensor,UserMins,
                     PROXY,URL, PORT, LOGIN, PASSWORD,
                     Down.Influx, Host, Port, User ,Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ = NULL,
                     sens2ref, asc.File=NULL) {
    
    # Parameters PROXY:  PROXY, URL, PORT, LOGIN, PASSWORD
    # Parameters Influx: Down.Influx, Host, Port, User, Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ
    # Sqlite database  : name.SQLite,name.SQLite.old
    # Configuration sensors: sens2ref
    # InfluxDB data
    # asc.File          : dataframe, default is NULL, used for giving the correct name of the sensor
    
    #browser()
    cat("-----------------------------------------------------------------------------------\n")
    cat("[INFLUXDB] INFO: Downloading InfluxDB data\n")
    # Saving Influx Sensor data
    Influx.Rdata.file  = file.path(WDoutput, "InfluxData.Rdata")
    Influx.csv.file    = file.path(WDoutput, "InfluxData.csv"  )
    if (DownloadSensor$Retrieve.data.Influx) {
        
        if (Down.Influx) {
            
            # downloading data from InfluxDB and updating airsenseur.db
            Influx.TZ <- Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD,
                                     Host = Host  , User = User, Port = as.numeric(Port), Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
                                     Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, use_google = FALSE, Page = 10000, Mean = as.numeric(UserMins)) 
            # if there are problems accessing port 443 for the google api to determine time zone add , use_google = FALSE
            # Sqlite2df returns only the new data from the AirSensEUR.db, if the whole set is needed add: Complete = TRUE in function Down_Influx
            #browser()
            InfluxData <- Sqlite2df(name.SQLite = name.SQLite, Dataset = Dataset, Influx.TZ = Influx.TZ, UserMins = UserMins, DownloadSensor = DownloadSensor, asc.File = asc.File)
            
            var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            if (!is.null(InfluxData) && is.data.frame(InfluxData)) {
                # setting the name of sensors
                if (exists("InfluxData")) {
                    # List of Pollutant/sensor installed in the AirSensEUR
                    var.names.sens <- colnames(InfluxData)[-grep(pattern = paste0(c("date","_raw","gpsTimestamp","boardTimeStamp",  "channel", "latitude", "longitude", "altitude"),collapse = "|"), x = colnames(InfluxData))]
                    # 
                    if (length(var.names.sens) == 0) {
                        stop(paste0("[INFLUXDB] ERROR: no sensor variable downloaded for ", Dataset," InFluxDB. Please check in the INfluxDB client -> STOP"))
                    } else cat(paste0("[INFLUXDB] INFO: Sensor variables existing in airsenseur.db: ", paste0(var.names.sens, collapse = ", "), ", with date timestamp and coordinates."), sep = "\n")
                    #
                    # Setting the Sensor names
                    var.name.GasSensors      <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
                } else { # if we do not have new data for sensors we use the names of sensors in sens2ref
                    var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                }
                InfluxDataNew <- InfluxData
            } else {
                remove(InfluxData) # removing  influxData if empty  
            } 
            
            if (file.exists(DownloadSensor$Influx.Rdata.file)) {
                # loading the existing data in InfluxData
                load(DownloadSensor$Influx.Rdata.file) 
                var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                # merging old and new data
                if (exists("InfluxDataNew")) {
                    InfluxData <- rbind.fill(InfluxData,InfluxDataNew)
                    rm(InfluxDataNew)    
                } 
            } 
            readr::write_csv(x = InfluxData, path = Influx.csv.file, na = "NA", append = FALSE)
            save(InfluxData, file = Influx.Rdata.file)
            cat(paste0("[INFLUXDB] INFO: Influx Sensor data saved in ", Influx.Rdata.file, " & ", Influx.csv.file,". Updating copies in .old files."), sep = "\n")
            Make.Old(File = Influx.Rdata.file)
            Make.Old(File = Influx.csv.file)
            
        } else { # Trying to use the existing Influx.Rdata.file
            
            if (file.exists(file.path(Influx.Rdata.file))) {
                cat(paste0("[INFLUXDB] INFO: Down.Influx set to FALSE in ASEConfig.R  (no request of sensor data download from InfluxDB). Using previously saved Influx.Rdata.file ."), sep = "\n")
                load(file.path(Influx.Rdata.file))
                var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
            } else {
                cat(paste0("[INFLUXDB] INFO: there is no previously saved Influx.Rdata.file. Missing InfluxData and Down.Influx request of sensor data download set to FALSE in ASEConfig.R ."), sep = "\n")
            }
        } 
    } else {
        if (file.exists(file.path(Influx.Rdata.file))) {
            load(file.path(Influx.Rdata.file))
            var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
            var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
        } else {
            cat(paste0("[INFLUXDB] INFO: sensor data download from InfluxDB already updated, DownloadSensor$Retrieve.data.Influx set to FALSE"), sep = "\n")
        }
    }
    
    if (exists("InfluxData")) {
        
        cat("[INFLUXDB] INFO INFLUXDB returning list with InfluxData, var.names.meteo, var.name.GasSensors and var.names.sens\n")
        return(list(InfluxData, var.names.meteo, var.name.GasSensors, var.names.sens)) 
        
    } else return(cat("[INFLUXDB] ERROR no Influx data available\n"))
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("[INFLUXDB] INFO: Downloading InfluxDB data\n")
    
}

#========================
#### Start of script ####
#========================

# Set config parameters
#Set WD
setwd(choose.dir())
# "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/For52North/General_data"

# PROXY parameters
PROXY = TRUE     
URL      = "10.168.209.72"; PORT     = 8012; LOGIN    = NULL; PASSWORD = NULL

# INFLUXDB parameters
Host            = "influxdb1.liberaintentio.com" 
User            = "52North"
Pass            = "JmaxNBTS"
name.SQLite     = "airsenseur.db"
name.SQLite.old = "airsenseur.db.old"
Db              = "jrcispra"
Dataset         = "AirSensEUR16" 
Influx.TZ       = "UTC"
use_google      = FALSE
Page            = 10000
Mean            = 10

# checking if data are already downlaoded
DownloadSensor <- Check_Download(Influx.name = Dataset, WDinput = getwd(), UserMins = Mean)
# Configuration of electrochemical sensors:
Shield         <- ASEPanel04Read(ASEPanel04File = choose.files())     

# Downloading data and saving files
INFLUXDB(WDoutput = getwd(), DownloadSensor = DownloadSensor, UserMins = Mean,
         PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD, Down.Influx = TRUE, 
         Host = Host  , Port = 8086, User = User, Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
         Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, sens2ref = NULL, asc.File = Shield)

#========================
# The function INFLUXDB is equivalent to:
#========================
# # Influx.TZ <- Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD,
#                          Host = Host  , User = User, Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
#                          Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, use_google = FALSE, Page = 10000, Mean = Mean)
# if there are problems accessing port 443 for the google api to determine time zone add , use_google = FALSE

# Sqlite2df returns only the new data from the AirSensEUR.db, if the whole set is needed add: Complete = TRUE in function Sqlite2df
# InfluxData <- Sqlite2df(name.SQLite = name.SQLite, Dataset = Dataset, Influx.TZ = Influx.TZ, UserMins = Mean, DownloadSensor = DownloadSensor, asc.File = Shield)

