# germanRadarAnalysis_precipitationType

This repository is part of the germanRADARanalysis project. Here, we create a gridded data set of precipitation types in Germany. More details about the project can be found on [my personal website](https://lochbihler.nl/?page_id=302).

## Steps to run

Open the RStudio project file in RStudio or use your favorite way of running R scripts.

1. run or source the src/setup_working_environment.R script
2. run the src/prepare_station_data_cloud_type.R script to download the DWD data and create a tailored station data set
3. finally run the src/create_gridded_CS_type_dataset.R script to produce the gridded precipitation type data set
