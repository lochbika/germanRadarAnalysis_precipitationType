library(geosphere)
library(profvis)
library(doFuture)
library(abind)
library(reshape2)

# load saved tracks file and station data
load(file = paste(dir_data, "station_metaIndex_cloud_type.RData", sep = ""))
load(file = paste(dir_data, "stationdata_CS_DWD.RData", sep = ""))

# load the grid description file
grid.definition <-
  read.csv(file = paste(dir_data, "grid_CS_type_aggregatoion.csv", sep = ""))

# calculate the distance matrix
gridstationdistance <- distm(as.matrix(cbind(
  metaIndex$geoLaenge, metaIndex$geoBreite
)), as.matrix(cbind(grid.definition$X,
                    grid.definition$Y)))

# now create a matrix with the station IDs for each track
gridstationids <-
  matrix(
    rep(seq(1, 6), each = nrow(gridstationdistance)),
    ncol = dim(gridstationdistance)[2],
    nrow = dim(gridstationdistance)[1]
  )

# sort each row of the two matrices by the distance
{
  gridstationids.ordered <- gridstationids
  
  for (i in seq_len(nrow(gridstationids)))
    gridstationids.ordered[i, ] <-
    gridstationids[i, order(gridstationdistance[i,])]
  }
{
  gridstationdistance.ordered <- gridstationdistance
  
  for (i in seq_len(nrow(gridstationdistance)))
    gridstationdistance.ordered[i, ] <-
    gridstationdistance[i, order(gridstationdistance[i,])]
}
rm(gridstationdistance, gridstationids)

# save number of stations per grid point
nStationsGridpoint <- table(gridstationids.ordered[, 1])

# transform list of station data frames to a single data frame with id column (stationID)
stationdataCS.df <-
  as.data.frame(data.table::rbindlist(stationdataCS, idcol = TRUE))

stationdataCS.df$.id <- as.numeric(stationdataCS.df$.id)

stationdataCS.df <-
  stationdataCS.df[order(stationdataCS.df$datetime),]

# remove unused months
stationdataCS.m <-
  as.matrix(stationdataCS.df[format(stationdataCS.df$datetime, "%m") %in% months &
                               stationdataCS.df$V_S1_CS %in% c(5, 6, 7, 8, 9, NA,-1), c(".id", "V_N", "V_S1_CS", "V_S2_CS", "V_S3_CS", "V_S4_CS")])
stationdataCS.dates <-
  stationdataCS.df[format(stationdataCS.df$datetime, "%m") %in% months &
                     stationdataCS.df$V_S1_CS %in% c(5, 6, 7, 8, 9, NA,-1), c("datetime")]
rm(stationdataCS.df, stationdataCS)

# remove missing values
stationdataCS.dates <-
  stationdataCS.dates[!is.na(stationdataCS.m[, "V_S1_CS"])]
stationdataCS.m <-
  stationdataCS.m[!is.na(stationdataCS.m[, "V_S1_CS"]), ]

# create date index for the gridded data; hourly
grid.dates <-
  seq.POSIXt(
    from = as.POSIXct("2001-01-01 00:00", tz = "UTC"),
    to = as.POSIXct("2020-12-31 23:00", tz = "UTC"),
    by = "hour"
  )
grid.dates <- grid.dates[format(grid.dates, "%m") %in% months]

#
# for debugging the function below
# grid_date <- grid.dates[788]
# stationdata <- stationdataCS.m
# station_dates <- stationdataCS.dates
# station_index <- metaIndex
# station_closest_gridpoint <- gridstationids.ordered[, 1]
# nmissStat <- nStationsGridpoint
# define function to create gridded data for one time step
gridCloudTypes <-
  function(grid_date,
           stationdata,
           station_dates,
           station_index,
           station_closest_gridpoint,
           nmissStat) {
    stationdata.ts <-
      stationdata[station_dates %in% grid_date,]
    
    # define output matrix
    output <- matrix(NA, ncol = 6, nrow = 6)
    dimnames(output) <-
      list(
        gridpoint = seq(1, 6),
        criterion = c("Q1",
                      "Q2",
                      "coverage",
                      "nStations",
                      "nNA",
                      "nInstrument")
      )
    
    output[, c(3, 4, 5, 6)] <- 0
    output[, 5] <- nmissStat
    
    # next iteration if no data is available
    if (is.matrix(stationdata.ts)) {
      if (all(is.na(stationdata.ts[, "V_S1_CS"])) |
          dim(stationdata.ts)[1] == 0) {
        return(output)
      }
    } else{
      return(output)
    }
    
    # and mark which stations are in the selected records
    presentstations <-
      station_index$Stations_id %in% stationdata.ts[, ".id"]
    
    # which is the closest grid point for each station
    closest.gridpoint <- station_closest_gridpoint[presentstations]
    
    # frequency table of cloud types for each grid point
    stationdata.ts.df <- as.data.frame(stationdata.ts)
    stationdata.ts.df <-
      melt(stationdata.ts.df[, c(".id", "V_S1_CS", "V_S2_CS", "V_S3_CS", "V_S4_CS")], id.vars = c(".id"))
    cloudTypeFrequencies <-
      as.data.frame.matrix(table(stationdata.ts.df[, ".id"], stationdata.ts.df[, "value"]))
    cloudTypeNames <-
      colnames(cloudTypeFrequencies)[colnames(cloudTypeFrequencies) %in% c('5', '6', '7', '8', '9', '-1')]
    cloudStationNames <- rownames(cloudTypeFrequencies)
    if (length(cloudTypeNames) == 1) {
      cloudTypeFrequencies <-
        as.data.frame(cloudTypeFrequencies[, colnames(cloudTypeFrequencies) %in% c('5', '6', '7', '8', '9', '-1')])
      colnames(cloudTypeFrequencies) <- cloudTypeNames
      rownames(cloudTypeFrequencies) <- cloudStationNames
    } else if (length(cloudTypeNames) == 0) {
      return(output)
    } else{
      cloudTypeFrequencies <-
        cloudTypeFrequencies[, colnames(cloudTypeFrequencies) %in% c('5', '6', '7', '8', '9', '-1')]
    }
    # check out instrument column here
    if ("-1" %in% colnames(cloudTypeFrequencies)) {
      cloudTypeFrequencies.instrument <-
        subset(cloudTypeFrequencies, select = "-1")
      cloudTypeFrequencies <-
        subset(cloudTypeFrequencies, select = --c(-1))
    } else {
      cloudTypeFrequencies.instrument <-
        data.frame("-1" = rep(0, dim(cloudTypeFrequencies)[1]))
      rownames(cloudTypeFrequencies.instrument) <-
        rownames(cloudTypeFrequencies)
    }
    
    # give it the cloud type classification label; convective
    l.conv <-
      colnames(cloudTypeFrequencies) %in% as.character(convective.cloudtypes)
    if (sum(l.conv) >
        1) {
      cloudTypeFrequencies$conv <-
        rowSums(cloudTypeFrequencies[, l.conv])
    } else if (sum(l.conv) ==
               1) {
      cloudTypeFrequencies$conv <-
        cloudTypeFrequencies[, l.conv]
    } else{
      cloudTypeFrequencies$conv <- rep(0, dim(cloudTypeFrequencies)[1])
    }
    # give it the cloud type classification label; stratiform
    l.strat <-
      colnames(cloudTypeFrequencies) %in% as.character(stratiform.cloudtypes)
    if (sum(l.strat) >
        1) {
      cloudTypeFrequencies$strat <-
        rowSums(cloudTypeFrequencies[, l.strat])
    } else if (sum(l.strat) ==
               1) {
      cloudTypeFrequencies$strat <-
        cloudTypeFrequencies[, l.strat]
    } else{
      cloudTypeFrequencies$strat <- rep(0, dim(cloudTypeFrequencies)[1])
    }
    # give it the cloud type classification label; instrument
    cloudTypeFrequencies[, "instrument"] <-
      cloudTypeFrequencies.instrument
    
    
    # now label each station with convective (1), stratiform (2) or mixed (0)
    cloudTypeFrequencies$stationClass <-
      rep(NA, dim(cloudTypeFrequencies)[1])
    cloudTypeFrequencies$stationClass[cloudTypeFrequencies$conv > 0 &
                                        cloudTypeFrequencies$strat == 0] <-
      1
    cloudTypeFrequencies$stationClass[cloudTypeFrequencies$conv == 0 &
                                        cloudTypeFrequencies$strat > 0] <-
      2
    cloudTypeFrequencies$stationClass[cloudTypeFrequencies$conv > 0 &
                                        cloudTypeFrequencies$strat > 0] <-
      0
    
    cloudTypeFrequencies$gridpoint <-
      rep(NA, dim(cloudTypeFrequencies)[1])
    for (i in 1:dim(cloudTypeFrequencies)[1]) {
      cloudTypeFrequencies$gridpoint[i] <-
        closest.gridpoint[station_index$Stations_id[presentstations] == rownames(cloudTypeFrequencies)[i]]
    }
    
    #
    # generate Q1 and Q2 criteria
    grid.raintype <-
      as.data.frame.matrix(table(
        cloudTypeFrequencies$gridpoint,
        cloudTypeFrequencies$stationClass
      ))
    # count number of instrument records in each grid point
    grid.instrument <-
      aggregate(
        cloudTypeFrequencies$instrument > 0,
        by = list(cloudTypeFrequencies$gridpoint),
        FUN = sum
      )
    
    if (all(colnames(grid.raintype) != '0'))
      grid.raintype$`0` <- rep(0, dim(grid.raintype)[1])
    if (all(colnames(grid.raintype) != '1'))
      grid.raintype$`1` <- rep(0, dim(grid.raintype)[1])
    if (all(colnames(grid.raintype) != '2'))
      grid.raintype$`2` <- rep(0, dim(grid.raintype)[1])
    
    grid.raintype$Q1 <- rep(0, dim(grid.raintype)[1])
    grid.raintype$Q2 <- rep(0, dim(grid.raintype)[1])
    
    
    grid.raintype$Q1[grid.raintype$`1` > 0 &
                       grid.raintype$`2` == 0] <- 1
    grid.raintype$Q1[grid.raintype$`1` == 0 &
                       grid.raintype$`2` > 0] <- 2
    grid.raintype$Q1[grid.raintype$`1` > 0 &
                       grid.raintype$`2` > 0] <- 0
    
    grid.raintype$Q2[grid.raintype$`1` > 0 &
                       grid.raintype$`2` == 0 &
                       (grid.raintype$`0` / (grid.raintype$`1` + grid.raintype$`0`)) < 0.5] <-
      1
    grid.raintype$Q2[grid.raintype$`1` == 0 &
                       grid.raintype$`2` > 0 &
                       (grid.raintype$`0` / (grid.raintype$`2` + grid.raintype$`0`)) < 0.5] <-
      2
    grid.raintype$Q2[grid.raintype$`1` > 0 &
                       grid.raintype$`2` > 0] <-
      0
    
    #
    # fill in data for grid points
    output[rownames(grid.raintype), 1] <-
      grid.raintype$Q1
    output[rownames(grid.raintype), 2] <-
      grid.raintype$Q2
    output[as.character(grid.instrument$Group.1), 6] <-
      grid.instrument$x
    
    #
    # fill in number of present and missing stations
    nStat <- table(cloudTypeFrequencies$gridpoint)
    output[names(nStat), 4] <- nStat
    output[names(nStat), 5] <- nmissStat[names(nStat)] - nStat
    
    #
    # fill in average coverage
    stationdata.ts[, "V_N"][stationdata.ts[, "V_N"] < 0] <- NA
    stationdata.ts <-
      cbind(stationdata.ts, rep(0, dim(stationdata.ts)[1]))
    colnames(stationdata.ts)[4] <- "gridpoint"
    for (i in 1:dim(stationdata.ts)[1]) {
      stationdata.ts[i, "gridpoint"] <-
        closest.gridpoint[station_index$Stations_id[presentstations] == stationdata.ts[i, ".id"]]
    }
    coverage <-
      aggregate(
        stationdata.ts[, "V_N"],
        by = list(stationdata.ts[, "gridpoint"]),
        FUN = mean,
        na.rm = T
      )
    output[coverage$Group.1, 3] <-
      coverage$x
    
    return(output)
  }

registerDoFuture()
plan(multisession)
acomb <- function(...)
  abind(..., along = 3)
options(future.globals.maxSize = 2097152000 * 2)

library(tictoc)
library(progressr)

tic()
# start filling it
# function(grid_date,
# stationdata,
# station_dates,
# station_index,
# station_closest_gridpoint)
# Q1
# Q2
# nStations: the total number of station records in each gridpoint per record (4 timesteps);incl. instrument
# nStationwithclouds: the number of station records with a coverage > 0
# nNA: the total number of station records with missing values
# nInstrument: the number of station records with instrument measurements
ts.iter <- 1:length(grid.dates)
with_progress({
  p <- progressor(along = ts.iter)
  grid.precipitationType.timeseries <-
    foreach (ts = ts.iter,
             .combine = "acomb",
             .errorhandling = "pass") %dopar% {
               p()
               gridCloudTypes(
                 grid.dates[ts],
                 stationdataCS.m,
                 stationdataCS.dates,
                 metaIndex,
                 gridstationids.ordered[, 1],
                 nStationsGridpoint
               )
             }
})
toc()

# dimension names with dates and variable names..
dimnames(grid.precipitationType.timeseries) <-
  list(
    gridpoint = seq(1, 6),
    variable = c("Q1",
                 "Q2",
                 "coverage",
                 "nStations",
                 "nNA",
                 "nInstrument"),
    datetime = format(grid.dates)
  )

# save it as RData
save(
  grid.precipitationType.timeseries,
  file = paste(
    dir_data,
    "gridded_precipitationType_Q1_Q2_timeseries.RData",
    sep =
      ""
  )
)

# save it as a csv file
grid.precipitationType.timeseries <-
  melt(grid.precipitationType.timeseries)
grid.precipitationType.timeseries$datetime <-
  as.POSIXct(grid.precipitationType.timeseries$datetime, tz = "UTC")
grid.precipitationType.timeseries$variable <-
  as.character(grid.precipitationType.timeseries$variable)
grid.precipitationType.timeseries$variable[grid.precipitationType.timeseries$variable ==
                                             "Q1"]
write.csv(grid.precipitationType.timeseries, file = paste(
  dir_data,
  "gridded_precipitationType_Q1_Q2_timeseries.csv",
  sep =
    ""
), row.names = F)
