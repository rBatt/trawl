
library(data.table)
library(bit64)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")


wcann.fish.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NWFSC/2014-02-11/wcann2003_2012fish.csv")
wcann.haul.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NWFSC/2014-02-11/wcann2003_2012haul.csv")
wcann.invert.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NWFSC/2014-02-11/wcann2003_2012invert.csv")





