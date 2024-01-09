########### UTLITIES : Packages ########
library(tidyverse)
library(haven)
library(measurements)
library(survey)
library(stargazer)
library(coefplot)
library(sjPlot)
library(splines)
library(patchwork)
library(car)
library(margins)

######### Dependent Variables ######## 

dvs_ols <- c("border_security_recoded")

dvs_binomial <- c("Increase_Border_Spending")

dvs_american <- c("citizenship_exp", "citizenship_ext", "Full_Citizen")

########## Binomial and OLS functions ##########################################

ols_function <- function(dv, vars, des, dat, out){
lm_mods1 <- list()

for (Y in dv) {
  mod1 <- list()
  for (i in 1:length(vars)) {
    form <- as.formula(paste(Y, " ~ ", paste(vars[[i]], collapse = " + ")))
    mod1[[i]] <- svyglm(form, design = des, family = gaussian(link = "identity"), data = dat) 
  }
  lm_mods1[[Y]] <- mod1
}
assign(out, lm_mods1, envir = .GlobalEnv)
}

bin_function <- function(dv, vars, des, dat, out){
  bin_mods <- list()
  
  for (Y in dv) {
    b_mod <- list()
    for (i in 1:length(vars)) {
      form <- as.formula(paste(Y, " ~ ", paste(vars[[i]], collapse = " + ")))
      b_mod[[i]] <- svyglm(form, design = des, family = "binomial", data = dat) 
    }
    bin_mods[[Y]] <- b_mod
  }
  assign(out, bin_mods, envir = .GlobalEnv)
}


