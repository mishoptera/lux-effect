# Author: Misha Leong
# Date: November 2018
# Project: Exploring presence of the luxury effect with North Carolina "Arthropods
# of our Homes dataset.


# *************************************************************
# FIRST THINGS FIRST
# *************************************************************

# load libraries
library(tidyverse)
library(vegan)
library(ggmap)
library(ggrepel)
library(ggpubr)
library(stringr)
library(acs)

# load files
load('data/aoh.Rdata')
load('data/houses.Rdata')


# *************************************************************
# REDUCE AOH DATA TO HOUSE LEVEL
# *************************************************************

# ////////////////////
# Function to create a matrix of taxa by city/landuse "site".  Analagous to the dune dataset


# *************************************************************
# PULL IN ACS DATA
# *************************************************************

# *************************************************************
# TEST FOR CORRELATED VARIABLES
# *************************************************************

# *************************************************************
# MODEL BUILDING
# *************************************************************