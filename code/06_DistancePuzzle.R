##############################################################
# Data preparation
##############################################################

# SETUP -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(here) #easy file referencing, root is set at project root
library(fixest) # perform estimations with multiple fixed-effects
library(flextable) # Output tables to word
library(huxtable) # Outupt tables

# Load data ---------------------------------------------------------------

x = read_csv(here("output","agtpa_applications.csv"))

# 1a. OLS Gravity -----------------------------------------------------------

## Select necessary data
x_app2 = x %>%
  filter(year %in% seq(1986, 2006, 4))

## Construct symmetric pair id's
x_app2 = x_app2 %>%
  mutate(pair = paste(pmin(exporter,importer),pmax(exporter,importer),sep = "_")) %>%
  group_by(pair) %>%
  mutate(pair_id = cur_group_id())

## calculate logs
x_app2 = x_app2 %>%
  mutate(across(c(trade,dist),~log(.x),.names="ln_{.col}"))

## Create fixed effects 
x_app2 = x_app2 %>%
  unite("fe_exp_year",c(exporter,year),sep="_",remove=FALSE) %>%
  unite("fe_imp_year",c(importer,year),sep="_",remove=FALSE)

## Create a dummy for intra-national trade
## Create time-specific international distance variables
x_app2 = x_app2 %>%
  mutate(ln_dist_year = paste0("ln_dist_",year),
         ln_dist_copy = ln_dist) %>%
  pivot_wider(names_from="ln_dist_year", 
              values_from="ln_dist_copy",
              values_fill = 0) 

## Estimation
distance_ols = feols(ln_trade ~ ln_dist_1986 + ln_dist_1990 + ln_dist_1994 +
                       ln_dist_1998 + ln_dist_2002 + ln_dist_2006 +
                       cntg + lang + clny |
                       fe_exp_year + fe_imp_year,
                     data = x_app2 %>% filter(trade > 0 & exporter != importer),
                     vcov = cluster ~ pair_id)
summary(distance_ols)

# 1b. PPML Gravity ------------------------------------------------------------
distance_poisson = fepois(trade ~ ln_dist_1986 + ln_dist_1990 + ln_dist_1994 +
                           ln_dist_1998 + ln_dist_2002 + ln_dist_2006 +
                           cntg + lang + clny |
                           fe_exp_year + fe_imp_year,
                         data = x_app2 %>% filter(exporter != importer),
                         vcov = cluster ~ pair_id)
summary(distance_poisson)



# 2. Internal distance solution for the distance puzzle -----------------

## Create dummy variable for intra-national trade
## Create intra-national distance variable 
## Set international distance variable to zero for intra-national trade to avoid overlap
x_app2 = x_app2 %>%
  mutate(D_intra = ifelse(importer == exporter, 1, 0),
         ln_dist_intra = ln_dist * D_intra,
         across(ln_dist_1986:ln_dist_2006, ~ ifelse(D_intra==1,0,.x)))

distance_poisson_intra = fepois(trade ~ ln_dist_1986 + ln_dist_1990 + ln_dist_1994 +
                                 ln_dist_1998 + ln_dist_2002 + ln_dist_2006 +
                                 cntg + lang + clny + ln_dist_intra |
                                 fe_exp_year + fe_imp_year,
                               data = x_app2,
                               vcov = cluster ~ pair_id)

summary(distance_poisson_intra)


# 3. Internal distance and home bias solution for the distance puzzle --------

distance_poisson_intra_home = fepois(trade ~ ln_dist_1986 + ln_dist_1990 + ln_dist_1994 +
                                       ln_dist_1998 + ln_dist_2002 + ln_dist_2006 +
                                       cntg + lang + clny + ln_dist_intra + D_intra |
                                       fe_exp_year + fe_imp_year,
                                     data = x_app2,
                                     vcov = cluster ~ pair_id)
summary(distance_poisson_intra_home)

# 4. Fixed effects solution for the "distance puzzle" ---------------------

## Specify country-specific intra-national trade dummies
x_app2 = x_app2 %>%
  mutate(D_trade_ii = ifelse(exporter==importer,exporter,"international"))

distance_poisson_intra_fe = fepois(trade ~ ln_dist_1986 + ln_dist_1990 + ln_dist_1994 +
                                     ln_dist_1998 + ln_dist_2002 + ln_dist_2006 +
                                     cntg + lang + clny |
                                     fe_exp_year + fe_imp_year + D_trade_ii,
                                   data = x_app2,
                                   vcov = cluster ~ pair_id)
summary(distance_poisson_intra_fe)



# Overview ------------------------------------------------------------------

## Overview
tab_distance_gravity =  huxreg("(1) OLS" = distance_ols,
                       "(2) PPML" = distance_poisson,
                       "(3) INTRA" = distance_poisson_intra,
                       "(4) BRDR" = distance_poisson_intra_home,
                       "(5) FEs" = distance_poisson_intra_fe,
                       coefs = c("Log distance 1986" = "ln_dist_1986",
                                 "Log distance 1990" = "ln_dist_1990",
                                 "Log distance 1994" = "ln_dist_1994",
                                 "Log distance 1998" = "ln_dist_1998",
                                 "Log distance 2002" = "ln_dist_2002",
                                 "Log distance 2006" = "ln_dist_2006",
                                 "Contiguity" = "cntg",
                                 "Colony" = "clny",
                                 "Common language" = "lang",
                                 "Log intra-national distance" = "ln_dist_intra",
                                 "Intra-national trade dummy" = "D_intra"),
                       stars = NULL,
                      note = "Notes: All estimates are obtained with data for the years 1986, 1990, 1994, 1998, 2002, and 2006, and use exporter-time and importer-time fixed effects. The estimates of the fixed effects are omitted for brevity. Columns (1) and (2) use data on international trade flows only. Column (1) employs the OLS estimator and column (2) uses the PPML estimator. Column (3) adds internal trade observations and uses intra-national distance as an additional covariate. Column (4) adds an indicator covariate for international trade. Finally, column (5) uses country-specific dummies for intra-national trade. Standard errors are clustered by country pair and are reported in parentheses. The bottom panel of the table reports the percentage change in the estimates of the effects of bilateral distance between 1986 and 2006.") %>%
  insert_row(c("Intra-national trade", "No", "No", "Yes", "Yes","Yes"),
             after = 27) %>%
  insert_row(c("Country-specific intra-national fixed effects", "No", "No", "No", "No", "Yes"),
             after = 28) %>%
  set_number_format(26:27, everywhere, 0) %>%
  set_tb_borders(26:28,everywhere,0) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.35,rep(0.65/5,5))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("A simple solution to the ``distance puzzle'' in trade") %>%
  set_label("tab_distance_gravity") 


tab_distance_gravity[26,1] = c("logLik (in thousands)") 
tab_distance_gravity[26,-1] = as.numeric(tab_distance_gravity[26,-1])/1000
tab_distance_gravity[27,1] = c("AIC (in thousands)") 
tab_distance_gravity[27,-1] = as.numeric(tab_distance_gravity[27,-1])/1000

width(tab_distance_gravity) = 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(tab_distance_gravity),file=here("output","tables","tab_distance_gravity.tex"))

## Export table to word
tab_distance_gravity_docx = as_flextable(tab_distance_gravity)
save_as_docx(tab_distance_gravity_docx, path = here("output","tables","tab_distance_gravity.docx"))

