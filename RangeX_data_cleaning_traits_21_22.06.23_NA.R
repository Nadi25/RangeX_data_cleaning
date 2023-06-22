

# ## RangeX data cleaning for traits 2021 -------------------------------------------------

## Data used: RangeX_raw_traits_high_2021.csv / RangeX_raw_traits_low_2021.csv / 
## RangeX_Metadata.csv / RangeX_YearlyDemographics.csv
## Date: 22.06.23
## Author: Nadine Arzt
## Purpose: Clean the COMPLETE raw data file 
## Missing entries? Missing values? 
## Implausible values? Wrong column names? 
## Data classes defined? and add treatments & Plant_ID 

# load packages -----------------------------------------------------------

library(dplyr)
library(tidyr) # data manipulation
library(ggplot2) # test-plotting
library(stringr) # working with regex
library("tidyverse")
library(ggstatsplot)

# load data 2021 traits high ---------------------------------------------------------------
traits_high_21 <- read.csv2("RangeX_raw_traits_high_2021.csv")
head(traits_high_21)

# check structure of data set ---------------------------------------------
str(traits_high_21)
length(traits_high_21) ## 27 columns

## get column names
dput(colnames(traits_high_21))

summary(traits_high_21)

# delete superfluous columns ----------------------------------------------
## columns x.1- x.15 have only NAs --> delete
traits_high_21 <- traits_high_21 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )

length(traits_high_21) ## 20 columns



# rename columns ----------------------------------------------------------

## the headers are shifted to the left, because there are two additional columns in the beginning
## one is just continues numbers
## it's actually plotID and plantID on the paper sheet

traits_high_21 <- traits_high_21 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "A" = "species",
         "B" = "coord",
         "species" = "leaf_no_LIVE",
         "position_ID_original" = "leaf_no_DEAD",
         "number_leaves_LIVE" = "tillers",
         "number_leaves_DEAD" = "base.width",
         "tillers" = "height",
         "vegetative_width" = "petiole_1",
         "height_vegetative_str" = "petiole_2",
         "petiole_length_1" = "petiole_3",
         "petiole_length_2" = "leaf_L_1",
         "petiole_length_3" = "leaf_L_2",
         "leaf_length1" = "leaf_L_3",
         "leaf_length2" = "sam",
         "leaf_length3" = "date",
         "sam" = "notes",
         "date" = "date_measured",
         "notes" = "notes.1")

head(traits_high_21)


summary(traits_high_21)


# add columns -------------------------------------------------------------

## add column with region = NOR for Norway
traits_high_21 <- traits_high_21 %>%
  add_column(region = "NOR")
traits_high_21

## add column with site = hi for high
traits_high_21 <- traits_high_21 %>%
  add_column(site = "hi")
traits_high_21



# vegetative height ------------------------------------------------------

## what has been measured for height_vegetative_str
## in the read_me it says: stretched vegetative plant height (not including inflorescence)
## but on the paper sheet: stem height and plant height (this column is not digitized)
## why did they cross the column plant height - it would make more sense to have this 
## why is there no height for every plant for stem height?

## what does (s) and (f) mean in stem height?

traits_high_21$height_vegetative_str



# leaf length -------------------------------------------------------------

## 4e, cyncri, i4, leaf_length2 is a typo --> 171 instead of 1711



# load data 2021 traits low ---------------------------------------------------------------

traits_low_21 <- read.csv2("RangeX_raw_traits_low_2021.csv")
head(traits_low_21)

# check structure of data set ---------------------------------------------
str(traits_low_21)
length(traits_low_21) ## 19 columns
length(traits_low_21$block) ## 960 rows --> only supposed to have 600

# delete superfluous columns with NAs -------------------------------------
traits_low_21 <- traits_low_21 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )
head(traits_low_21)

# delete superfluous rows with NAs ----------------------------------------
## remove rows at the end, which have only NAs or nothing in it
traits_low_21 <- traits_low_21 %>% drop_na(block)
length(traits_low_21$block) ## 600 rows

## 



