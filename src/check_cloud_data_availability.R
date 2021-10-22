library(ggplot2)
library(reshape2)

# load saved tracks file
load(file = paste(
  dir_data,
  "parallel_hourly_gridded_cloudtypes_Q1_Q2_timeseries.RData",
  sep = ""
))

#
#
# gridded data set

# generate dates for gridded data from dimnames
grid.cloudtypes.date <-
  as.POSIXct(as.numeric(dimnames(grid.cloudtypes.timeseries)$date), tz = "UTC",
             origin = "1970-01-01 00:00")

grid.cloudtypes.date.month <-
  format(grid.cloudtypes.date, format = "%Y-%m")

# plot number of stations with data available
monthly.nStations <- aggregate(
  grid.cloudtypes.timeseries[1, "nStations",],
  by = list(grid.cloudtypes.date.month),
  FUN = mean,
  na.rm = T
)
monthly.nStations$Group.1 <-
  paste(monthly.nStations$Group.1, "-15", sep = "")
monthly.nStations$Group.1 <-
  as.Date(monthly.nStations$Group.1, format = "%Y-%m-%d")
colnames(monthly.nStations) <- c("date", paste("grid point", 1))

for (i in 2:6) {
  tmp <- aggregate(
    grid.cloudtypes.timeseries[i, "nStations",],
    by = list(grid.cloudtypes.date.month),
    FUN = mean,
    na.rm = T
  )
  monthly.nStations[, paste("grid point", i)] <- tmp$x
}

monthly.nStations <- melt(monthly.nStations, id.vars = "date")

pl <- ggplot(monthly.nStations,
             aes(
               x = date,
               y = value,
               group = variable,
               color = variable
             )) + geom_path() +
  xlab("date") +
  ylab("average number of stations [-]") +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
pl

ggsave(
  filename = paste(
    dir_plots,
    "plot_average_number_of_stations_in_gridpoints.png",
    sep = ""
  ),
  plot = pl,
  width = 12,
  height = 8 ,
  units = "cm"
)

# plot number of stations with instrument measurements
monthly.nStations <- aggregate(
  grid.cloudtypes.timeseries[1, "nInstrument",],
  by = list(grid.cloudtypes.date.month),
  FUN = mean,
  na.rm = T
)
monthly.nStations$Group.1 <-
  paste(monthly.nStations$Group.1, "-15", sep = "")
monthly.nStations$Group.1 <-
  as.Date(monthly.nStations$Group.1, format = "%Y-%m-%d")
colnames(monthly.nStations) <- c("date", paste("grid point", 1))

for (i in 2:6) {
  tmp <- aggregate(
    grid.cloudtypes.timeseries[i, "nInstrument",],
    by = list(grid.cloudtypes.date.month),
    FUN = mean,
    na.rm = T
  )
  monthly.nStations[, paste("grid point", i)] <- tmp$x
}

monthly.nStations <- melt(monthly.nStations, id.vars = "date")
pl <- ggplot(monthly.nStations,
             aes(
               x = date,
               y = value,
               group = variable,
               color = variable
             )) + geom_path() +
  xlab("date") +
  ylab("average number of instrument measurements [-]") +
  theme_bw(base_size = pl.basesize) +
  theme(
    panel.spacing = unit(0, "line"),
    strip.background = element_blank(),
    strip.placement = 'outside',
    strip.text = element_text(size = pl.basesize),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.background = element_blank(),
    plot.margin = margin(0.1, 0.05, 0.1, 0.1, "cm"),
    axis.text = element_text(size = pl.basesize),
    legend.title = element_blank(),
    legend.text = element_text(size = pl.basesize),
    legend.key.height = unit(.5, "line"),
    axis.title.y = element_text(size = pl.basesize)
  )
pl

ggsave(
  filename = paste(
    dir_plots,
    "plot_average_number_of_instrument_stations_in_gridpoints.png",
    sep = ""
  ),
  plot = pl,
  width = 12,
  height = 8 ,
  units = "cm"
)

#
#
# direct station data (closest station)

load(file = paste(dir_data, "tracks.lifecycles.RData", sep = ""))

# label events convective or stratiform
tracks.lifecycles$convORstrat <- rep(NA, dim(tracks.lifecycles)[1])
tracks.lifecycles$convORstrat[tracks.lifecycles$CS_value %in% convective.cloudtypes] <-
  "convective"
tracks.lifecycles$convORstrat[tracks.lifecycles$CS_value %in% stratiform.cloudtypes] <-
  "stratiform"
tracks.lifecycles$convORstrat[tracks.lifecycles$CS_value %in% -1] <-
  "instrument"

dates.months <-
  format(tracks.lifecycles$date.maxpeakVal, format = "%Y-%m")
tmp <-
  aggregate(tracks.lifecycles$convORstrat,
            FUN = table,
            by = list(dates.months))
tmp2 <- matrix(0, ncol = 3, nrow = dim(tmp)[1])
colnames(tmp2) <- c("convective", "stratiform", "instrument")
for (i in 1:dim(tmp)[1]) {
  tmp2[i, as.character(unlist(dimnames(tmp$x[[i]])))] <- tmp$x[[i]]
}
rownames(tmp2) <- tmp$Group.1

data.cloudtypes <- melt(tmp2)

data.cloudtypes$Var1 <-
  as.Date(paste(data.cloudtypes$Var1, "-15", sep = ""), format = "%Y-%m-%d")

ggplot(data.cloudtypes, aes(
  x = Var1,
  y = value,
  group = Var2,
  color = Var2
)) + geom_path()
