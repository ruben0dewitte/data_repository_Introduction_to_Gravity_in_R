##############################################################
# Data preparation
##############################################################

# SETUP -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(here) #easy file referencing, root is set at project root

#  Gravity data ------------------------------------------------------------

#Load Data 
load(here("input","agtpa_applications.rda"))

# Remove variables we will learn to create ourselves
x = agtpa_applications %>%
  select(-c(pair_id))

#Save the dataset to .rds format
saveRDS(x,file=here("output","agtpa_applications.RDS"))

#Save the dataset to .csv format
write_csv(x,file=here("output","agtpa_applications.csv"))
