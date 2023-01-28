#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodriguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                                  Fuente: GEIH DANE                                   #
#**************************************************************************************#

rm(list = ls(all.names = TRUE))

### INITIAL CONFIGURATION ---------------------------------------------------------------------------------------

## install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

## require/install packages on this session
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret) # Classification And REgression Training

## set seed
set.seed(0000)

#Separate each section with labels
# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



# Load Packages -----------------------------------------------------------
pkg<-list("dplyr","here")
lapply(pkg, require, character.only=T)
rm(pkg)



# Load data ---------------------------------------------------------------
# I recomend you using the package here
dta<-read.table(here("stores","US90.txt"), sep="", header=TRUE)


# plot data ---------------------------------------------------------------

plot(dta$gdpgr, dta$gdpcapgr, pch="*")
