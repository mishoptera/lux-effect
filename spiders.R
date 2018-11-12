# Author: Misha Leong
# Date: November 2018
# Project: Using the NC AOH dataset to estimate spider density in houses


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
aoh <- read.csv('data/7roomsFamilies.csv') 
houses <- read.csv('data/houseEverything.csv') 



# *************************************************************
# FILTER DOWN DATASET
# *************************************************************
# Only need spider data. How many spider specimens per house?
houses2 <- houses %>%
  select(houseID, totalSpec, totalMS, totalFam, sqFeet, size, houseAge, acreage)

spiders <- aoh %>%
  filter(Order == "Araneae (spiders)") %>%
  group_by(houseID) %>%
  summarise(minSpi = sum(numMS)) %>%
  left_join(houses2, by = "houseID") %>%
  mutate(sqMeters = sqFeet / 10.764, spiPerMeter = minSpi/sqMeters) 
spiders


