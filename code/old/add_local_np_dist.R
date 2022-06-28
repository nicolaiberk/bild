# ______________________________________________
# Emphasis framing in the wild
# Goal: Explore data newspaper distribution
# Procedure: load data, merge, vis
# ______________________________________________
# Date:  Thu Dec 02 10:37:28 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load data ####
path_2016 <- "data/auflage/local/Div VA/VA2016_1_1/Verbreitungsanalyse Tageszeitungen 2016/IVW VA 2016 Daten Vollversion/WIN/"

## distribution
local_dist <- read.fwf(here(paste0(path_2016, "IVWAUF.TXT")),
                       width = c(11, 8, 7, 7),
                       col.names = c("VA", "AGS", "sold_tot", "sold_epaper"))


## local id lookup
local_id   <- read.fwf(here(paste0(path_2016, "IVWORT.TXT")), 
                       width = c(8, (35-8), (43-35), (46-43), (47-46), (48-47), (53-48), (61-53), (69-61), (77-69), (85-77), (93-85), (101-93), (109-101), (117-109), (125-117), (133-125), (141-133)),
                       col.names = c("AGS", "Gebietsname", "leer", "NUTS21312", "SGTyp", "Webb", "PLZ", "Einwohner deutsch", "Einwohner ab 14 Jahre deutsch", "Einwohner 14-29 Jahre deutsch", "Einwohner 30-49 Jahre deutsch", "Einwohner ab 50 Jahre deutsch", "Haushalte deutsch", "Haushalte 14-29 deutsch", "Haushalte 30-49 deutsch", "Haushalte ab 50 deutsch", "Einwohner gesamt", "Haushalte gesamt"))

## paper id lookup
paper_id   <- read.fwf(here(paste0(path_2016, "IVWTIT.TXT")), 
                       width = c(11, (59-11), (107-59), (155-107), (185-155), (215-185), (245-215), (255-245), (261-255), (266-261), (274-261), (282-274), (290-282), (298-290), (306-298), (314-306), (315-314), (318-315)),
                       col.names = c("VA", "Maximale Einheit", "Titel/Gesamtbelegung", "Einzelbelegung", "Erscheinungsort maximale Einheit", "Erscheinungsort Titel/Gesamtbelegung", "Erscheinungsort Einzelbelegung", "IVW", "ZIS", "Erscheinungsweise", "Auflage maximale Einheit", "davon ePaper maximale Einheit", "Auflage/Titel Gesamtbelegung", "davon ePaper Titel/Gesamtbelegung", "Auflage Einzelbelegung", "davon ePaper Einzelbelegung", "belegbar J/N", "Kennzeichen ePaper"))


# merge ####
local_dist_id <- 
  local_dist %>% 
  full_join(local_id, by = "AGS")


## filter only BILD



# vis ####