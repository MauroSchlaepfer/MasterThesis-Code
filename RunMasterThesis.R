############ The run everything file ###########
setwd("/home/ka4/Desktop/Dokumente Masterarbeit") # Set working directory to where the files are saved
source("AnalysisStart.R") # "requires Data in path /home/ka4/Desktop/Dokumente Masterarbeit/Dataset.xlsx"
source("Raster.R") # requires data from https://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-density/data-download in path
# "QGIS/eu_gpwv3_pdens_00_wrk_25/eudens00/euds00g/dblbnd.adf"

####### Exploratory Data and Graphs, also important for recoding
source("ExploratoryAnalysis.R")

####### Threat level classification #######
source("Hurdle.R")
source("IWLS.R") 

# Tests and Graphs
source("ClassificationStep2.R") # might have to be run manually

# Graphs from simulated values
source("ILWS_Graphics.R")
##############################################

####### Spatial classification #######
source("SpatialModels.R")
#####################################

# !!!!! long running time !!!!!
####### Spatio-Temporal classification #######
source("TrainSpatioTemporalModel.R")
# "Step1Optim.R", manually is recommended, very long running time (optimization)
##############################################

### Scripts that get called by other scripts
# "Step1FunctionToOptimise.R"
# "BinaryClass.R"

# spatial images are in the file that creates the data 'Raster.R', 'SpatialModels.R'