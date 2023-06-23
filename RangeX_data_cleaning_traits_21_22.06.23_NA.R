

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
         "number_tillers" = "height",
         "vegetative_width" = "petiole_1",
         "height_vegetative_str" = "petiole_2",
         "petiole_length1" = "petiole_3",
         "petiole_length2" = "leaf_L_1",
         "petiole_length3" = "leaf_L_2",
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

## add column year
traits_high_21 <- traits_high_21 %>%
  add_column(year = 2021)
traits_high_21

colnames(traits_high_21)

# delete columns ----------------------------------------------------------

traits_high_21 <- traits_high_21 %>%
  dplyr::select(-A, -B)

length(traits_high_21)




# vegetative height ------------------------------------------------------

## what has been measured for height_vegetative_str
## in the read_me it says: stretched vegetative plant height (not including inflorescence)
## but on the paper sheet: stem height and plant height (this column is not digitized)
## why did they cross the column plant height - it would make more sense to have this 
## why is there no height for every plant for stem height?

## what does (s) and (f) mean in stem height?

traits_high_21$height_vegetative_str





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

 

# add columns -------------------------------------------------------------

## add column with region = NOR for Norway
traits_low_21 <- traits_low_21 %>%
  add_column(region = "NOR")
traits_low_21

## add column with site = lo for low
traits_low_21 <- traits_low_21 %>%
  add_column(site = "lo")
traits_low_21

## add column year
traits_low_21 <- traits_low_21 %>%
  add_column(year = 2021)
traits_low_21



# rename columns ----------------------------------------------------------
colnames(traits_low_21)

traits_low_21 <- traits_low_21 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "number_leaves_LIVE" = "leaf_no_LIVE",
         "number_leaves_DEAD" = "leaf_no_DEAD",
         "vegetative_width" = "base.width",
         "height_vegetative_str" = "height",
         "petiole_length1" = "petiole_1",
         "petiole_length2" = "petiole_2",
         "petiole_length3" = "petiole_3",
         "leaf_length1" = "leaf_L_1",
         "leaf_length2" = "leaf_L_2",
         "leaf_length3" = "leaf_L_3",
         "number_tillers" = "tillers")
traits_low_21


# merge data traits high with traits low ---------------------------------------

# check if data frames are equal
all.equal(traits_high_21, traits_low_21)

## get column names
dput(colnames(traits_high_21))
dput(colnames(traits_low_21))

## combine high and low site
traits_21 <- rbind(traits_high_21, traits_low_21)
head(traits_21)

# sort after site, block, plot, position
traits_21 <- traits_21 %>%
  group_by(site, block_ID_original , plot_ID_original) %>%
  arrange(block_ID_original,plot_ID_original, position_ID_original, .by_group = TRUE)

summary(traits_21)

# load metadata file for all countries ------------------------------------------------------
metadata <- read.csv2("RangeX_Metadata.csv")
head(metadata)
dput(colnames(metadata))

## filter only NOR
metadata_NOR <- metadata %>%
  filter(grepl('NOR', region))
head(metadata_NOR)


# improve traits_2021 data set --------------------------------------------

# change species name in one plot -----------------------------------------
## block 9, b, d3 = sucpra instead of leuvul

traits_21[traits_21$site == "lo" & traits_21$block_ID_original == "9" 
            & traits_21$plot_ID_original == "b" 
            & traits_21$position_ID_original == "d3", "species"] <- "sucpra"

# leaf length -------------------------------------------------------------
## 4e, cyncri, i4, leaf_length2 is a typo --> 171 instead of 1711
traits_21[traits_21$site == "hi" & traits_21$block_ID_original == "4" 
          & traits_21$plot_ID_original == "e" 
          & traits_21$position_ID_original == "i4", "leaf_length2"] <- 171


# merge metadata with trait data 21 ------------------------------------------

dput(colnames(metadata_NOR))
dput(colnames(traits_high_21))

traits_2021 <- left_join(traits_21, metadata_NOR,
                         by = c("region", "site", "block_ID_original", "plot_ID_original",
                                "position_ID_original", "species"))


# reorder column names ----------------------------------------------------
## to get it in the metadata format
dput(colnames(traits_2021))

col_order_21 <- c("region", "site", "block_ID_original", "plot_ID_original", 
               "position_ID_original", "species", "year", "treat_warming", "treat_competition", 
               "added_focals", "block_ID", "position_ID", "unique_plot_ID", 
               "unique_plant_ID", "height_vegetative_str", "vegetative_width", 
               "petiole_length1", "petiole_length2", "petiole_length3", 
               "leaf_length1", "leaf_length2", "leaf_length3", 
               "number_tillers", "sam", "date", "notes")

traits_2021 <- traits_2021[, col_order_21]
traits_2021


# vegetative height -------------------------------------------------------

## many values have (s) or (f) 
traits_2021$height_vegetative_str

traits_2021 <- traits_2021 %>% 
  separate(height_vegetative_str, c("height_vegetative_str", "B", "C", "D", "E", "F"))

class(traits_2021$height_vegetative_str)

## replace the "broken" in plot hi, 1, b, c6, plalan

traits_2021[traits_2021$site == "hi" & traits_2021$block_ID_original == "1" 
          & traits_2021$plot_ID_original == "b" 
          & traits_2021$position_ID_original == "c6", "height_vegetative_str"] <- NA


## delete columns B-G
traits_2021 <- traits_2021 %>%
  dplyr::select(-B, -C, -D, -E, -F, -G)

traits_2021$height_vegetative_str <- as.numeric(traits_2021$height_vegetative_str)
class(traits_2021$height_vegetative_str)

# date --------------------------------------------------------------------
## change format of date
traits_2021 <- traits_2021 %>% 
  mutate(date = as.Date(date, "%d.%m.%y"))



# data exploration --------------------------------------------------------

# add column treatment ----------------------------------------------------
## you need a column with the site + treatments: hi_warm_vege
traits_2021_exploration <- traits_2021

traits_2021_exploration$treatment <- paste(traits_2021_exploration$site, 
                                           traits_2021_exploration$treat_warming,
                                           traits_2021_exploration$treat_competition,
                                           sep = "_")

unique(traits_2021_exploration$treatment)


# plot figures to explore data --------------------------------------------
## vegetative height stretched for all species and all treatments
ggplot(data = traits_2021_exploration, aes(species, height_vegetative_str, fill = treatment))+
  geom_boxplot()

## we only have height values for hypmac and plalan

## that's not so good

## leaf length
ggplot(data = traits_2021_exploration, aes(species, leaf_length1, fill = treatment))+
  geom_boxplot()

## calculate a mean of leaf_length1, 2 and 3





# load metadata for data entry Yearly demographics ----------------------------------------------------------

yearly_demographics <- read.csv("RangeX_YearlyDemographics.csv")
head(yearly_demographics)
dput(colnames(yearly_demographics))


# adapt traits_2022 in the format of yearly demographics ------------------
## !! use traits_2021 !!

## write mail to Evelin about
## adding sam, petiole_length2 and 3 to the yearly demographics


















