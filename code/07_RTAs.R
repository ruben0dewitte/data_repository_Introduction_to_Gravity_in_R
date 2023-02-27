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

## Source user-written functions
source(here("code","03_toolbox.R"))

## Load global variables
load(here("output","globalvariables.Rdata"))

# Load data ---------------------------------------------------------------

x = read_csv(here("output","agtpa_applications.csv"))

# 1a. Traditional Estimates (OLS) -----------------------------------------------------------

## Select necessary data
x_app3 = x %>%
  filter(year %in% seq(1986, 2006, 4))

## Construct symmetric pair id's
x_app3 = x_app3 %>%
  mutate(pair = paste(pmin(exporter,importer),pmax(exporter,importer),sep = "_")) %>%
  group_by(pair) %>%
  mutate(pair_id = cur_group_id())

## calculate logs
x_app3 = x_app3 %>%
  mutate(across(c(trade,dist),~log(.x),.names="ln_{.col}"))

## Create fixed effects 
x_app3 = x_app3 %>%
  unite("fe_exp_year",c(exporter,year),sep="_",remove=FALSE) %>%
  unite("fe_imp_year",c(importer,year),sep="_",remove=FALSE)

## Specify country-specific intra-national trade dummies
x_app3 = x_app3 %>%
  mutate(D_trade_ii = ifelse(exporter==importer,exporter,"international"))

#Estimation
rta_ols = feols(ln_trade ~ ln_dist + cntg + lang + clny + rta |
                  fe_exp_year + fe_imp_year, 
                data = x_app3 %>%
                  filter(trade > 0 & exporter != importer), 
                vcov = cluster ~ pair_id)
summary(rta_ols)

# 1b. Traditional estimation (PPML) -------------------------------------------
rta_poisson = fepois(trade ~ ln_dist + cntg + lang + clny + rta |
                       fe_exp_year + fe_imp_year,
                     data = x_app3 %>%
                       filter(exporter != importer), 
                     vcov = cluster ~ pair_id)
summary(rta_poisson)


# 2. Allowing for trade-diversion from domestic sales   -------------------

## Create international border dummy variable
x_app3 = x_app3 %>%
  mutate(intl_brdr = ifelse(exporter == importer, as.character(pair_id), "international"))

## Estimation
rta_poisson_intra = fepois(trade ~ ln_dist + cntg + lang + clny + rta |
                             fe_exp_year + fe_imp_year + D_trade_ii,
                           data = x_app3, 
                           vcov = cluster ~ pair_id)
summary(rta_poisson_intra)


# 3. Addressing potential endogeneity of RTAs------------------------------

## Estimation 
rta_endo = fepois(trade ~  rta |
                    fe_exp_year + fe_imp_year + pair_id,
                  data = x_app3, 
                  vcov = cluster ~ pair_id)

summary(rta_endo)


# 4. Testing for potential "reverse causality" --------

# ## Identify future RTAs
# x_app3 = x_app3 %>%
#   group_by(exporter,importer) %>%
#   mutate(rta_lead4 = tlead(rta,n=4,along_with = year))

## Estimation
rta_lead = fepois(trade ~  rta + rta_lead4 |
                    fe_exp_year + fe_imp_year + pair_id,
                  data = x_app3, 
                  vcov = cluster ~ pair_id)

summary(rta_lead)

# 5. Allowing for potential non-linear and phasing-in effects --------

# # Identify past RTAs
# x_app3 = x_app3 %>%
#   group_by(exporter,importer) %>%
#   mutate(rta_lag4 = tlag(rta,n=4,along_with = year),
#          rta_lag8 = tlag(rta,n=8,along_with = year),
#          rta_lag12 = tlag(rta,n=12,along_with = year))

rta_lags = fepois(trade ~  rta + rta_lag4 + rta_lag8 + rta_lag12 |
                    fe_exp_year + fe_imp_year + pair_id,
                  data = x_app3, 
                  vcov = cluster ~ pair_id)

summary(rta_lags)

#Visualise the evolution over time
coeff_rta_lags = coefficients(rta_lags)
coeff_rta_lags = cbind.data.frame(coeff_rta_lags, confint(rta_lags))
colnames(coeff_rta_lags) = c("coeff", "lower_ci", "upper_ci")
coeff_rta_lags = coeff_rta_lags %>%
  rownames_to_column(var="variable") %>%
  mutate(variable = factor(variable,levels = unique(variable), ordered=TRUE)) # Provide order for x-axis later

fig_rta_evo = ggplot(coeff_rta_lags) + 
  geom_point(aes(x=variable,y=coeff)) + 
  geom_line(aes(x=variable,y=coeff,group=1),linetype=linetypes[2]) + # the data was grouped by variable, to draw a line accross variables we need to specify a homogenous group, for instance 1
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci,x=variable), width=0.2) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),axis.title.y=element_text(angle = 0, vjust = 0.5)) +
  ylab("Coefficients") +
  xlab("Variable") 

fig_rta_evo
ggsave(file=here("output","figures","fig_rta_evo.png"), plot=fig_rta_evo,
       height=20/1.7,units="cm")

# 6. Addressing globalization effects -------------------------------------

## Create time-specific international border variables
x_app3 = x_app3 %>%
  mutate(D_inter = ifelse(importer != exporter, 1, 0),
         intl_brdr_year = paste0("intl_brdr_",year)) %>%
  pivot_wider(names_from="intl_brdr_year",
              values_from="D_inter",
              values_fill = 0)

rta_glob = fepois(trade ~  rta + rta_lag4 + rta_lag8 + rta_lag12 +
                    intl_brdr_1986 + intl_brdr_1990 + intl_brdr_1994 +
                    intl_brdr_1998 + intl_brdr_2002|
                    fe_exp_year + fe_imp_year + pair_id,
                  data = x_app3, 
                  vcov = cluster ~ pair_id)

summary(rta_glob)

# Overview ------------------------------------------------------------------

## Overview
tab_glob_gravity =  huxreg("(1) OLS" = rta_ols,
                               "(2) PPML" = rta_poisson,
                               "(3) INTRA" = rta_poisson_intra,
                               "(4) ENDG" = rta_endo,
                               "(5) LEAD" = rta_lead,
                               "(6) PHSNG" = rta_lags,
                               "(7) GLBZN" = rta_glob,
                               coefs = c("Log distance" = "ln_dist",
                                         "Contiguity" = "cntg",
                                         "Colony" = "clny",
                                         "Common language" = "lang",
                                         "RTA" = "rta",
                                         "RTA(t+4)" = "rta_lead4",
                                         "RTA(t-4)" = "rta_lag4",
                                         "RTA(t-8)" = "rta_lag8",
                                         "RTA(t-12)" = "rta_lag12",
                                         "Int. border 1986" = "intl_brdr_1986",
                                         "Int. border 1990" = "intl_brdr_1990",
                                         "Int. border 1994" = "intl_brdr_1994",
                                         "Int. border 1998" = "intl_brdr_1998",
                                         "Int. border 2002" = "intl_brdr_2002"),
                               stars = NULL,
                               note = "Notes: All estimates are obtained with data for the years 1986, 1990, 1994, 1998, 2002, and 2006, and use exporter-time and importer-time fixed effects. The estimates of the fixed effects are omitted for brevity. Columns (1) and (2) use data on international trade flows only. Column (1) applies the OLS estimator and column (2) uses the PPML estimator. Column (3) adds intra-national trade observations and uses country-specific dummies for internal trade. Column (4) adds pair fixed effects. The estimates of the pair fixed effects are omitted for brevity. Column (5) introduces RTA lead. Column (6) allows for phasing-in effects of RTAs. Finally, column (7) accounts for the effects of globalization. Standard errors are clustered by country pair and are reported in parentheses.") %>%
  insert_row(c("Intra-national trade", "No", "No", "Yes", "Yes","Yes", "Yes", "Yes"),after = 33) %>%
  set_number_format(32:33, everywhere, 0) %>%
  set_tb_borders(33,everywhere,0) %>%
  set_tb_padding(0) %>% 
  set_col_width(c(0.3,rep(0.7/7,7))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("Estimating the Effects of Regional Trade Agreements") %>%
  set_label("tab_glob_gravity")

tab_glob_gravity[32,1] = c("logLik (in thousands)") 
tab_glob_gravity[32,-1] = as.numeric(tab_glob_gravity[32,-1])/1000
tab_glob_gravity[33,1] = c("AIC (in thousands)") 
tab_glob_gravity[33,-1] = as.numeric(tab_glob_gravity[33,-1])/1000


width(tab_glob_gravity) = 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(tab_glob_gravity),file=here("output","tables","tab_glob_gravity.tex"))

## Export table to word
tab_glob_gravity_docx = as_flextable(tab_glob_gravity)
save_as_docx(tab_glob_gravity_docx, path = here("output","tables","tab_glob_gravity.docx"))

