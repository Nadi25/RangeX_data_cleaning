
# ## RangeX data cleaning for traits 2022 -------------------------------------------------

## Data used: RangeX_raw_traits_high_2022.csv / RangeX_raw_traits_low_2022.csv / RangeX_Metadata.csv
## Date: 14.06.23
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

# load data 2022 traits high ---------------------------------------------------------------
traits_high_22 <- read.csv2("RangeX_raw_traits_high_2022.csv")
head(traits_high_22)

# clean column names and data classes ---------------------------------------------
str(traits_high_22)
length(traits_high_22) ## 27 columns

## get column names
dput(colnames(traits_high_22))

# delete superfluous columns ----------------------------------------------
## columns x.1- x.15 have only NAs --> delete
traits_high_22 <- traits_high_22 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )

# traits_high_22 <- traits_high_22 %>%
#   dplyr::select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7, -X.8, -X.9, -X.10, -X.11,
#                 -X.12, -X.13, -X.14, -X.15)

length(traits_high_22) ## 12 columns

## X is a second column with notes, but there is only one note in it
## delete?
traits_high_22 <- traits_high_22 %>%
  dplyr::select(-X)
length(traits_high_22$block) ## 1200

# change column names -----------------------------------
## get column names
dput(colnames(traits_high_22))

## add column with region = NOR for Norway
traits_high_22 <- traits_high_22 %>%
  add_column(region = "NOR")
traits_high_22

## add column with site = hi for high
traits_high_22 <- traits_high_22 %>%
  add_column(site = "hi")
traits_high_22


## rename column names
traits_high_22 <- traits_high_22 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "height_vegetative_str" = "height",
         "petiole_length" = "petiole_L",
         "leaf_length" = "leaf_L",
         "leaf_width" = "leaf_W",
         "number_flowers" = "flowers")


# load data 2022 traits low ---------------------------------------------------------------

traits_low_22 <- read.csv2("RangeX_raw_traits_low_2022.csv")
head(traits_low_22)

# clean column names and data classes ---------------------------------------------
str(traits_low_22)
length(traits_low_22) ## 15 columns
length(traits_low_22$block) ## 960 rows --> only supposed to have 600

# delete superfluous columns with NAs -------------------------------------
traits_low_22 <- traits_low_22 %>%
  select(
    where(
      ~!all(is.na(.x))
    )
  )
head(traits_low_22)

# delete superfluous rows with NAs ----------------------------------------
## remove rows at the end, which have only NAs or nothing in it
traits_low_22 <- traits_low_22 %>% drop_na(block)
length(traits_low_22$block) ## 600 rows

# change column names -----------------------------------
## get column names
dput(colnames(traits_low_22))
## add column with region = NOR for Norway
traits_low_22 <- traits_low_22 %>%
  add_column(region = "NOR")
traits_low_22

## add column with site = hi for high
traits_low_22 <- traits_low_22 %>%
  add_column(site = "lo")
traits_low_22

## rename column names
traits_low_22 <- traits_low_22 %>%
  rename("block_ID_original" = "block",
         "plot_ID_original" = "treat",
         "position_ID_original" = "coord",
         "height_vegetative_str" = "height",
         "petiole_length" = "petiole_L",
         "leaf_length" = "leaf_L",
         "leaf_width" = "leaf_W",
         "number_flowers" = "flowers")
traits_low_22
#

# merge data traits high with traits low ---------------------------------------
## get column names
dput(colnames(traits_high_22))
dput(colnames(traits_low_22))

## combine high and low site
traits_22 <- rbind(traits_high_22, traits_low_22)
head(traits_22)

# sort after site, block, plot, position
traits_22 <- traits_22 %>%
  group_by(site, block_ID_original , plot_ID_original) %>%
  arrange(block_ID_original,plot_ID_original, position_ID_original, .by_group = TRUE)


# load metadata file for all countries ------------------------------------------------------
metadata <- read.csv2("RangeX_Metadata.csv")
head(metadata)
dput(colnames(metadata))

## filter only NOR
metadata_NOR <- metadata %>%
  filter(grepl('NOR', region))
head(metadata_NOR)



# merge metadata with trait data ------------------------------------------

dput(colnames(metadata))
dput(colnames(traits_high_22))

traits_2022 <- left_join(traits_22, metadata,
                         by = c("region", "site", "block_ID_original", "plot_ID_original",
                                "position_ID_original", "species"))




# reorder column names ----------------------------------------------------
## to get it in the metadata format
dput(colnames(traits_2022))

col_order <- c("region", "site", "block_ID_original", "plot_ID_original", 
               "position_ID_original", "species", "treat_warming", "treat_competition", 
               "added_focals", "block_ID", "position_ID", "unique_plot_ID", 
               "unique_plant_ID", "height_vegetative_str", "petiole_length", "leaf_length", "leaf_width", 
               "number_flowers", "date", "notes")

traits_2022 <- traits_2022[, col_order]
traits_2022



# load metadata for data entry Yearly demographics ----------------------------------------------------------

yearly_demographics <- read.csv("RangeX_YearlyDemographics.csv")
head(yearly_demographics)
dput(colnames(yearly_demographics))



# adapt traits_2022 in the format of yearly demographics ------------------

## add column with year
traits_2022 <- traits_2022 %>%
  add_column(year = "2022")
head(traits_2022)


## add all of these columns:
# "height_reproductive_str", "height_vegetative", "height_reproductive", 
# "vegetative_width", "vegetative_length", "stem_diameter", "leaf_length1", 
# "leaf_length2", "leaf_length3", "number_tillers", "number_branches", "number_leafclusters", 
# "mean_inflorescence_size", "herbivory"

## collector is not so easy, because its not recorded at the digitized table

traits_2022 <- traits_2022 %>%
  dplyr::mutate(
    collector = NA,
    height_reproductive_str = NA,
    height_vegetative = NA,
    height_reproductive = NA,
    vegetative_width = NA,
    vegetative_length = NA,
    stem_diameter = NA,
    leaf_length1 = NA,
    leaf_length2 = NA,
    leaf_length3 = NA,
    number_leaves = NA,
    number_tillers = NA,
    number_branches = NA,
    number_leafclusters = NA,
    mean_inflorescence_size = NA,
    herbivory = NA
  )

dput(colnames(traits_2022))

## delete "region", "site", "block_ID_original", "plot_ID_original", 
## "position_ID_original","treat_warming", "treat_competition", 
## "added_focals", "block_ID", "position_ID", "unique_plot_ID"

rangex_traits_22 <- traits_2022 %>%
  dplyr::select(-region, -site, -block_ID_original, -plot_ID_original, 
                -position_ID_original, -treat_warming, -treat_competition, 
                -added_focals, -block_ID, -position_ID, -unique_plot_ID) %>% 
  dplyr::ungroup()

dput(colnames(rangex_traits_22))
length(rangex_traits_22) # 28
length(yearly_demographics) # 23

## Adding missing grouping variables: `site`, `block_ID_original`, `plot_ID_original`
## WHY cant I delete them

## make correct order as in yearly_demographics
col_order_traits_22 <- c("site", "block_ID_original", "plot_ID_original","unique_plant_ID", 
                         "species", "year", "collector", "height_vegetative_str", 
                         "height_reproductive_str", "height_vegetative", "height_reproductive", 
                         "vegetative_width", "vegetative_length", "stem_diameter", "leaf_length", 
                         "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", 
                         "number_leaves", "number_tillers", "number_branches", "number_leafclusters", 
                         "number_flowers", "mean_inflorescence_size", "herbivory")

rangex_traits_22 <- rangex_traits_22[, col_order_traits_22]
rangex_traits_22

## put values from leaf_length in column leaf_length1
rangex_traits_22 <- rangex_traits_22 %>%
  dplyr::mutate(
    leaf_length1 = dplyr::coalesce(leaf_length1, leaf_length)
  ) %>%
  dplyr::select(-leaf_length)

## delete site, block_ID_original, plot_ID_original
rangex_traits_22 <- rangex_traits_22 %>%
  dplyr::select(-site, -block_ID_original, -plot_ID_original)

## now the data frame should have the correct format of yearly_demographics


# plot figures to explore data --------------------------------------------

ggplot(rangex_traits_22, aes(x= species, y = ))

























