library(rdwd)

# read the DWD station information and reduce to suitable records
data("metaIndex")
metaIndex <-
  metaIndex[metaIndex$hasfile &
              metaIndex$per == "historical" &
              metaIndex$var == "cloud_type" &
              metaIndex$res == "hourly",]

# we also only want stations that have at least some data after 2001-01-03
metaIndex <-
  metaIndex[as.Date(metaIndex$bis_datum, format = "%Y-%m-%d", tz = "UTC") >= as.Date("2001-02-01", tz =
                                                                                       "UTC"),]

# download data for all stations
stationdownload <-
  unlist(
    selectDWD(
      id = metaIndex$Stations_id,
      var = "cloud_type",
      res = "hourly",
      per = "historical",
      current = TRUE
    )
  )

# download if not already done
stationfiles <-
  dataDWD(url = stationdownload, dir = "data/DWD/", read = FALSE)

# save the file path into the metaIndex
metaIndex$file <- stationfiles
rm(stationfiles)

# first check if all downloaded files can be opened
for (stationfile in 1:dim(metaIndex)[1]) {
  datafileinarchive <-
    grep(
      'produkt_cs',
      unzip(metaIndex$file[stationfile], list = TRUE)$Name,
      ignore.case = TRUE,
      value = TRUE
    )
}

# now we will read all station data, concatenate it to one list and trim it to the needed time intervals
stationdataCS <- list()
for (stationfile in 1:dim(metaIndex)[1]) {
  print(paste(
    stationfile,
    metaIndex$Stationsname[stationfile],
    metaIndex$Stations_id[stationfile]
  ))
  
  # first we need to find out the file name of the data file in the zip archive
  datafileinarchive <-
    grep(
      'produkt_cs',
      unzip(metaIndex$file[stationfile], list = TRUE)$Name,
      ignore.case = TRUE,
      value = TRUE
    )
  stationfiledata <-
    read.table(
      unz(metaIndex$file[stationfile], datafileinarchive),
      header = T,
      sep = ";",
      na.strings = "-999"
    )
  rm(datafileinarchive)
  # create a proper datetime column
  stationfiledata$datetime <-
    as.POSIXct(as.character(stationfiledata$MESS_DATUM),
               format = "%Y%m%d%H",
               tz = "UTC")
  
  # select only records within the timespan and the months that the radar data cover
  # march is included here because we want the measurement n hours before the event
  stationfiledata <-
    stationfiledata[stationfiledata$datetime >= as.POSIXct("2001-02-01 00:00") &
                      stationfiledata$datetime <= as.POSIXct("2020-12-31 23:55") &
                      format(stationfiledata$datetime, "%m") %in% c(months, "02"),]
  
  # we don't need all of the columns; only keep
  # QN_8, TT, TD and datetime
  stationid <- stationfiledata$STATIONS_ID[1]
  stationfiledata <-
    subset(
      stationfiledata,
      select = c(
        "V_N",
        "V_S1_CS",
        "V_S2_CS",
        "V_S3_CS",
        "V_S4_CS",
        "datetime"
      )
    )
  
  # now concatenate to a list of data frames; each element one station
  if (is.na(stationid)) {
    print("stationid not found")
  } else{
    stationdataCS[[as.character(stationid)]] <-
      stationfiledata
  }
}
rm(stationfiledata, stationid)

# get the final list of stations with data
stationswithdata <- as.numeric(names(stationdataCS))

# save the station data to disk
save(stationdataCS,
     file = paste(dir_data, "stationdata_CS_DWD.RData", sep = ""))

# change the index and save it
metaIndex <-
  metaIndex[metaIndex$Stations_id %in% stationswithdata,]
save(metaIndex, file = paste(dir_data, "station_metaIndex_cloud_type.RData", sep = ""))
