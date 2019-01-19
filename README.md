# Stantraining
Stan training 

This file was created by Carlos Valencia following the existing instructions for Rstan available on: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

`remove.packages("rstan") #Removing any previously existing packages 

if (file.exists(".RData")) file.remove(".Rdata")

install.packages("rstan", repos = "https://cloud.r-project.org", dependencies = TRUE) #Installing the packages

pkgbuild::has_build_tools(debug = TRUE) #The C++ R Tools package should be checked using this command

library("rstan") #observe startup messages

rstan_options(auto_write = TRUE) # Save a section of the Stan program to the hard disk`
