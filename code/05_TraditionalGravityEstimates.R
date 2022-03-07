##############################################################
# Data preparation
##############################################################

# SETUP -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(here) # easy file referencing, root is set at project root
library(fixest) # perform estimations with multiple fixed-effects
library(flextable) # Output tables to word
library(huxtable) # Output tables

# Load data ---------------------------------------------------------------

x = read_csv(here("output","agtpa_applications.csv"))

# 1. Naive Gravity -----------------------------------------------------------

## Select necessary data
x_app1 = x %>%
  filter(year %in% seq(1986, 2006, 4))

## Construct symmetric pair id's
x_app1 = x_app1 %>%
  mutate(pair = paste(pmin(exporter,importer),pmax(exporter,importer),sep = "_")) %>%
  group_by(pair) %>%
  mutate(pair_id = cur_group_id())

## construct exporter output and importer expenditure
x_app1 = x_app1 %>%
  group_by(exporter,year) %>%
  mutate(Y_it = sum(trade)) %>%
  group_by(importer,year) %>%
  mutate(E_jt = sum(trade))

## calculate logs
x_app1 = x_app1 %>%
  mutate(across(c(trade,Y_it,E_jt,dist),~log(.x),.names="ln_{.col}"))

## Estimation
fit_ols = feols(ln_trade ~ ln_dist + cntg + lang + clny + ln_Y_it + ln_E_jt,
                data = x_app1 %>%
                  filter(trade > 0 & exporter != importer), 
                vcov = cluster ~ pair_id)
summary(fit_ols)


# 2. Proxy for multilateral resistance ------------------------------------

## Calculate remoteness indices
x_app1 = x_app1 %>%
  group_by(year) %>%
  mutate(Y_t = sum(Y_it), E_t = sum(E_jt)) %>%
  group_by(exporter, year) %>%
  mutate(remoteness_exp = sum(dist /(E_jt / E_t))) %>%
  group_by(importer, year) %>%
  mutate(remoteness_imp = sum(dist / (Y_it / Y_t))) %>%
  mutate(ln_remoteness_exp = log(remoteness_exp), 
         ln_remoteness_imp = log(remoteness_imp))

## Estimation
fit_remoteness = feols(ln_trade ~ ln_dist + cntg + lang + clny + ln_Y_it + 
                         ln_E_jt + ln_remoteness_exp + ln_remoteness_imp,
                       data = x_app1 %>% filter(trade > 0 & exporter != importer),
                       vcov = cluster ~ pair_id)
summary(fit_remoteness)


# 3. Fixed Effects -----------------------------------------------------------

## Create fixed effects 
x_app1 = x_app1 %>%
  unite("fe_exp_year",c(exporter,year),sep="_",remove=FALSE) %>%
  unite("fe_imp_year",c(importer,year),sep="_",remove=FALSE)

## Estimation 
fit_fixedeffects = feols(ln_trade ~ ln_dist + cntg + lang + clny |
                           fe_exp_year + fe_imp_year,
                         data = x_app1 %>% filter(trade > 0 & exporter != importer),
                         vcov = cluster ~ pair_id)
summary(fit_fixedeffects)

# 4. Fit gravity with PPML and fixed effects -----------------------------
fit_poisson = fepois(trade ~ ln_dist + cntg + lang + clny |
                       fe_exp_year + fe_imp_year,
                     data = x_app1 %>% filter(exporter != importer),
                     vcov = cluster ~ pair_id)
summary(fit_poisson)


# Overview ----------------------------------------------------------------
tab_traditional_gravity =  huxreg(" " = fit_ols,
                                  "Remoteness" = fit_remoteness,
                                  "Fixed Effects" = fit_fixedeffects,
                                  "Fixed Effects " = fit_poisson,
                                  coefs = c("Intercept" = "(Intercept)",
                                            "Log Distance" = "ln_dist",
                                            "Contiguity" = "cntg",
                                            "Common language" = "lang",
                                            "Colony" = "clny",
                                            "Log output" = "ln_Y_it",
                                            "Log expenditure" = "ln_E_jt",
                                            "Exporter remoteness" = "ln_remoteness_exp",
                                            "Importer remoteness" = "ln_remoteness_imp"),
                                  stars = NULL,
                                  note = "Notes: Statistics based on author's calculations. All estimates are obtained with data for the years 1986, 1990, 1994, 1998, 2002, and 2006. Columns (1)-(3) use the OLS estimator. Column (1) does not control for the multilateral resistances. Column (2) uses remoteness indexes to control for multilateral resistances. Column (3) uses importer-time and exporter-time fixed effects, whose estimates are omitted for brevity, to control for multilateral resistances. Finally, column (4) employs the PPML estimator. Standard errors are clustered by country pair and are reported in parentheses."
) %>%
  insert_row("","(1) OLS", "(2) OLS", "(3) OLS", " (4) PPML", after = 0) %>%
  set_top_border(1,everywhere,1) %>%
  set_align(1, everywhere, "center") %>%
  insert_row(c("Exporter-time fixed effects", "No", "No", "Yes", "Yes"),
             after = 24) %>%
  insert_row(c("Importer-time fixed effects", "No", "No", "Yes", "Yes"),
             after = 25) %>%
  set_number_format(23:24, everywhere, 0) %>%
  set_tb_borders(24:25,everywhere,0) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3,rep(0.7/4,4))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("Traditional Gravity Estimates") %>%
  set_label("tab_traditional_gravity") 



width(tab_traditional_gravity) = 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(tab_traditional_gravity),file=here("output","tables","tab_traditional_gravity.tex"))

## Export table to word
tab_traditional_gravity_docx = as_flextable(tab_traditional_gravity)
save_as_docx(tab_traditional_gravity_docx, path = here("output","tables","tab_traditional_gravity.docx"))

