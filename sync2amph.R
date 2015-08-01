library(rbLib)
oldwd <- getwd()
setwd("~")

# Mirror
from <- "Documents/School&Work/pinskyPost/trawl/"
to <- "ryanb@amphiprion.deenr.rutgers.edu:'Documents/School&Work/pinskyPost/trawl/'"
mirror(from, to)

# Push, Run, Pull
path <- "./Documents/School&Work/pinskyPost/trawl/Scripts/Simulation/"
scriptName <- "sim.basic.R"
remoteName <- "ryanb@amphiprion.deenr.rutgers.edu"
prp(path, scriptName, remoteName, verbose=TRUE)

# Pull whole trawl
pull("./Documents/School&Work/pinskyPost/trawl/", remoteName)

setwd(oldwd)


