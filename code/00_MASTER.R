################################################################################
# MASTER file for data repository "An Introduction to Gravity in R"
# Ruben Dewitte
# March 2022
################################################################################


# SETUP -------------------------------------------------------------------

# Install (if necessary) and Load libraries

# source(here("code","01_packagemanagement.R"))
library(here) #easy file referencing, root is set at project root

# Setup - Define global variables -------------------------------------------------------------------

source(here("code","02_setup.R"))

# Dataprep - Prepare the data ----------------------------------------------------------------

source(here("code","04_dataprep.R"))

# Analysis ----------------------------------------------------------------

source(here("code","05_TraditionalGravityEstimates.R"))
source(here("code","06_DistancePuzzle.R"))
source(here("code","07_RTAs.R"))





