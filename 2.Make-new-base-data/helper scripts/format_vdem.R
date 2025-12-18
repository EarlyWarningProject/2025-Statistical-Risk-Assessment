# takes in a data.table version of VDEM and a list of variables to keep and renames and otherwise formats variables. Drops unused variables

format_vdem <- function(dat, keep = c("COWcode", "country_name", "year", used_vars)){
  dat <- dat[complete.cases(dat$COWcode), ]  # Remove rows where COWcode is NA
  dat <- dat[year >= 1934 & year <= 2024]   # Filter rows for years between 1934 and 2024
  colkeep = intersect(keep, colnames(dat))
  dat <- subset(dat, select = colkeep)
  add_placeholders = setdiff(keep, colnames(dat))
  dat[, (add_placeholders) := NA]
  dat$sftgcode <- cowtopitfit(cowcodedata = dat$COWcode, yeardata=dat$year)
  
  ### Some renaming to make things easier to remember:
  names(dat)[names(dat) == 'v2elrstrct'] <- 'candidaterestriction'
  names(dat)[names(dat) == 'v2psparban_ord'] <- 'partyban'
  names(dat)[names(dat) == 'v2psoppaut_ord'] <- 'barrierstoparties'
  names(dat)[names(dat) == 'v2jureform_ord'] <- 'judicialreform'
  names(dat)[names(dat) == 'v2clrelig_ord'] <- 'religiousfreedom'
  names(dat)[names(dat) == 'v2xcl_disc'] <- 'freediscussion'
  names(dat)[names(dat) == 'v2pepwrses'] <- 'ses_power_dist'
  names(dat)[names(dat) == 'v2clkill_ord'] <- 'pol_killing_approved'
  names(dat)[names(dat) == 'v2clsocgrp_ord'] <- 'social_inequality'
  names(dat)[names(dat) == 'v2clrgunev_ord'] <- 'even_civilrights'
  names(dat)[names(dat) == 'e_mipopula'] <- 'vdem_popsize' 

  
  # Transform population and import/export varables
  dat$vdem_popsize = as.numeric(dat$vdem_popsize) * 10^3 #vdem population is in thousands
  dat$vdem_popsize_log2 = log2(dat$vdem_popsize)
  dat$e_cow_exports = as.numeric(dat$e_cow_exports) * 10^6 #vdem imports/exports are in millions
  dat$e_cow_imports = as.numeric(dat$e_cow_imports) * 10^6
  
  # other variables
  dat$pol_killing_approved = as.numeric(dat$pol_killing_approved==0)
  dat$social_inequality = as.numeric(dat$social_inequality==0)
  dat$even_civilrights = as.numeric(dat$even_civilrights==2)
  dat$repress_civilsoc = as.numeric(dat$v2csreprss_ord==0) 
  dat$social_power_dist = as.numeric(is.element(el=dat$v2pepwrsoc_ord, set=c(0,1,2)))
  dat$minorityrule = 0
  dat$minorityrule[dat$v2pepwrsoc_ord<=1]=1
  dat$partyban.new.0 = as.numeric(is.element(el=dat$partyban, set = c(0, 1)))
  dat$partyban.new.1 = as.numeric(is.element(el=dat$partyban, set = c(2, 3)))
  dat$partyban.new.2 = as.numeric(dat$partyban==4)
  dat$v2csgender_binary = as.numeric(is.element(el=dat$v2csgender_ord, set=c(0,1,2)))
  dat$v2mecenefm_binary = as.numeric(is.element(el=dat$v2mecenefm_ord, set = c(0,1,2)))
  
  # dat <- subset(dat, select = -grep("v2", colnames(dat)))
  dat
}
