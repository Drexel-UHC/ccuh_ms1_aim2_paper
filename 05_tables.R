library(tidyverse)
library(patchwork)
library(glue)
library(gnm); library(dlnm); library(splines)
library(SALURhelper)

# Load data
list_df_city_nbhd <- readRDS("data/list_df_city_nbhd.rds")


# Category count table ----------------------------------------------------

