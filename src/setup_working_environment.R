#
# directories
#

setwd("/home/kai/Documents/germanRadarAnalysis_precipitationType/")

dir_plots <- "plots/"
dir_data <- "data/"

# for which years and months?
years  <- as.character(seq(2001, 2020))
months <- sprintf(seq(3, 11), fmt = "%02g")
months.names <-
  c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
seasons <- c("spring", "summer", "autumn")

# cloud type definitions
convective.cloudtypes <- c(8, 9)
stratiform.cloudtypes <- c(6, 7, 5)

#
# define some graphical parameters
#
pl.linewidth <- .5
pl.basesize <- 8

#
# functions
#
