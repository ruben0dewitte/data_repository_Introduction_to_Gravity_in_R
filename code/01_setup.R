##############################################################
# Data preparation
##############################################################

# SETUP -------------------------------------------------------------------

# Load packages
library(here) #easy file referencing, root is set at project root

# Define global variables -------------------------------------------------

#Set global variables
colorpalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # Colorblind-friendly color palette
linetypes = c("solid", "22", "42", "44", "13", "1343", "73", "2262", "12223242","F282", "F4448444", "224282F2", "F1")

#Save the intermediate dataset 
save(colorpalette,linetypes,file=here("output","globalvariables.Rdata"))
