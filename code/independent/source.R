# ______________________________________________
# source file for treatment plot preparation
# Goal: Project Goal
# Procedure: Step 1, Step 2, Step 3
# ______________________________________________
# Date:  Mon May 09 20:48:08 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(data.table)
library(here)
library(lubridate)

# generate survey dates - technically dependent variable
source(here("code/preprocessing/independent/01_surveydates.R"))

# run stm
source(here("code/preprocessing/independent/02a_stm.R"))

# label topics
source(here("code/preprocessing/independent/02b_stm_label.R"))

# generate daily estimates
source(here("code/preprocessing/independent/02c_stm_clean.R"))

# clean BERT
source(here("code/preprocessing/independent/03a_bert_crime_clean.R"))

# aggregate BERT crime estimates daily
source(here("code/preprocessing/independent/03b_bert_crime_aggregate.R"))


# merge estimates for individual exposure variable
source(here("code/preprocessing/independent/04_merge.R"))


