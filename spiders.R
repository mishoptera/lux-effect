# Author: Misha Leong
# Date: November 2018
# Project: Using the NC AOH dataset to estimate spider density in houses


# *************************************************************
# FIRST THINGS FIRST
# *************************************************************

# load libraries
library(tidyverse)

# load files
aoh <- read.csv('data/7roomsFamilies.csv') 
houses <- read.csv('data/houseEverything.csv') 

# *************************************************************
# FILTER DOWN DATASET
# *************************************************************

# Pull out the necessary house variables
houses2 <- houses %>%
  select(houseID, totalSpec, totalMS, totalFam, sqFeet)

# Calculate spiders per square meters metric
spiders <- aoh %>%
  filter(Order == "Araneae (spiders)") %>%
  group_by(houseID) %>%
  summarise(minSpi = sum(numMS)) %>%
  left_join(houses2, by = "houseID") %>%
  mutate(sqMeters = sqFeet / 10.764, spiPerMeter = minSpi/sqMeters) 
spiders


