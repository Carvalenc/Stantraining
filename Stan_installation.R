##Note: Make sure R and RStudio are up to date

##Running linear mixed model in Stan

#Removing any previously existing packages 
remove.packages("rstan") 
if (file.exists(".RData")) file.remove(".Rdata")

#Installing the packages

install.packages("rstan", repos = "https://cloud.r-project.org", dependencies = TRUE)

pkgbuild::has_build_tools(debug = TRUE)

library("rstan") #observe startup messages

rstan_options(auto_write = TRUE)
