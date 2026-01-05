library(tbeptools)
library(here)
library(dplyr)
library(sf)
library(lubridate)
library(readxl)

source(here('R/funcs.R'))

# EPC -----------------------------------------------------------------------------------------

##
# epc data

# local file path
xlsx <- here::here('data/data-raw', 'wq_data.xls')
# xlsx <- here::here('data/data-raw', 'Results_Provisional.xlsx')

# import and download if new
wqdat <- read_importwq(xlsx, download_latest = T)
# epcdata <- read_importwq(xlsx, download_latest = F)

epcchl <- epcdata %>%
  select(
    bay_segment,
    station = epchc_station,
    SampleTime,
    yr,
    mo,
    Latitude,
    Longitude,
    chla,
    chla_q
  ) %>%
  mutate(
    station = as.character(station)
  )

##
# BCBS, TCB, and MR chlorophyll data through 2021, reasonable assurance repo

# https://github.com/tbep-tech/reasonable-assurance-analysis/blob/main/R/dat_proc.R, line 27
olddatraw <- rdataload(
  'https://github.com/tbep-tech/reasonable-assurance-analysis/raw/main/data/chldat.RData'
)

olddat <- olddatraw %>%
  filter(bay_segment %in% c('BCBS', 'TCB', 'MR')) %>%
  select(-Level)

# Pinellas (BCBS) -----------------------------------------------------------------------------

##
# BCBS

# ra shapefile for bcbs spatial subset for pinellas data, includes areas W7 and parts of W6
bcbsseg <- st_read(here('data/data-raw/tampabay_ra_seg_watersheds.shp')) %>%
  st_transform(crs = 4326) %>%
  filter(BAY_SEGMEN == 5)

# # pinco data on WQP is about a year behind the data on water atlas
# pincodataraw <- read_importwqp(org = '21FLPDEM_WQX', type = 'wq', trace = T)

# from pinellas water atlas, https://pinellas.wateratlas.usf.edu/
# search by waterbody id (all bcb, narrows)
# chlorophyll only
# date range 2022 and partial 2023, all of 2023 was sent via email from Stacey and Alex on 2/29 so removed here and compiled below
pinchlraw1 <- read.csv(here('data/data-raw/pinchl2022.txt'), sep = '\t')

pinchl2022 <- pinchlraw1 %>%
  filter(Parameter == 'Chla_ugl') %>%
  select(
    station = StationID,
    SampleTime = SampleDate,
    Latitude = Actual_Latitude,
    Longitude = Actual_Longitude,
    chla = Result_Value,
    chla_q = QACode
  ) %>%
  mutate(
    bay_segment = 'BCBS',
    SampleTime = mdy_hms(SampleTime),
    yr = year(SampleTime),
    mo = month(SampleTime),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    station = gsub('\\=', '', station) # does not id stations with letter suffix as in 2023
  ) %>%
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
  .[bcbsseg, ] %>%
  st_set_geometry(NULL) |>
  filter(year(SampleTime) == 2022)

# 2023 BCB
# from Stacey Day, Alex Manos via email 2/29/24 (was not in Water Atlas on that date)
pinchlraw2 <- read_excel(here('data/data-raw/pinchl2023.xlsx'))

pinchl2023 <- pinchlraw2 %>%
  select(
    station = Site,
    SampleTime = Date,
    Latitude,
    Longitude,
    chla = `Chlorophyll a, uncorrected (µg/L)`
  ) %>%
  mutate(
    bay_segment = 'BCBS',
    SampleTime = mdy(SampleTime),
    yr = year(SampleTime),
    mo = month(SampleTime),
    chla_q = NA_character_ # no qualifiers for these data
  ) %>%
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
  .[bcbsseg, ] %>%
  st_set_geometry(NULL)

# 2024 BCB
# from Alex Manos via email 2/24/25
pinchlraw3 <- read_excel(here('data/data-raw/pinchl2024.xlsx'))

pinchl2024 <- pinchlraw3 %>%
  select(
    station = Site,
    SampleTime = Date,
    Latitude,
    Longitude,
    chla = `Chlorophyll a, uncorrected`
  ) %>%
  mutate(
    bay_segment = 'BCBS',
    SampleTime = as.Date(SampleTime),
    yr = year(SampleTime),
    mo = month(SampleTime),
    chla_q = NA_character_ # no qualifiers for these data
  ) %>%
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>%
  .[bcbsseg, ] %>%
  st_set_geometry(NULL)

# Manatee (MR, TCB) ---------------------------------------------------------------------------

##
# MR, TCB

# # manco data on WQP is about a year behind the data on FDEP WIN
# mancodataraw <- read_importwqp(org = '21FLMANA_WQX', type = 'wq', trace = T)

# from FDEP WIN, via WAVES interface
# https://prodenv.dep.state.fl.us/DearWin/public/wavesSearchFilter?calledBy=menu
# org as 21FLMANA, stations as those below for MR, TCB
# activity type as sample, sample-composite, field
# media as water
# analysis date from 2022 Jan 1 to 2023 Dec 31
# dep analyte name as all chlorophyll analytes
# according to GB, this is more updated than water atlas, which pulls from WIN
# manco data goes to win within 1 month at end of each quarter
manchlraw1 <- read.csv(
  here('data/data-raw/manchlthrough2023.txt'),
  sep = '|',
  skip = 10
)

MR <- c('431', '433', '434', '532', '535', 'LM4')
TCB <- c('395', '405', '408', '430')

# win data did not include lat/lon
locs <- olddat %>%
  filter(yr > 2020) %>%
  filter(bay_segment %in% c('MR', 'TCB')) %>%
  select(bay_segment, station, Latitude, Longitude) %>%
  distinct()

manchl20222023 <- manchlraw1 %>%
  select(
    station = Monitoring.Location.ID,
    SampleTime = Activity.Start.Date.Time,
    chla = DEP.Result.Value.Number,
    chla_q = Value.Qualifier
  ) %>%
  distinct() %>%
  mutate(
    bay_segment = case_when(
      station %in% MR ~ 'MR',
      station %in% TCB ~ 'TCB'
    ),
    SampleTime = mdy_hms(SampleTime),
    yr = year(SampleTime),
    mo = month(SampleTime),
    chla = as.numeric(chla)
  ) %>%
  filter(!is.na(SampleTime)) %>%
  filter(yr > 2021) %>%
  left_join(locs, by = c('bay_segment', 'station')) %>%
  select(bay_segment, station, SampleTime, yr, mo, everything())

# manchl 2024
# from FDEP WIN, via WAVES interface
# https://prodenv.dep.state.fl.us/DearWin/public/wavesSearchFilter?calledBy=menu
# org as 21FLMANA, stations as those below for MR, TCB
# activity type as sample, sample-composite, field
# media as water
# analysis date from 2024 Jan 1 to 2024 Dec 31
# dep analyte name as all chlorophyll analytes
# according to GB, this is more updated than water atlas, which pulls from WIN
# manco data goes to win within 1 month at end of each quarter
manchlraw2 <- read.csv(
  here('data/data-raw/manchl2024.txt'),
  sep = '|',
  skip = 10
)

MR <- c('431', '433', '434', '532', '535', 'LM4')
TCB <- c('395', '405', '408', '430')

manchl2024 <- manchlraw2 %>%
  # filter(DEP.Analyte.Name == 'Chlorophyll a- uncorrected') %>%
  select(
    station = Monitoring.Location.ID,
    SampleTime = Activity.Start.Date.Time,
    chla = DEP.Result.Value.Number,
    chla_q = Value.Qualifier
  ) %>%
  mutate(
    bay_segment = case_when(
      station %in% MR ~ 'MR',
      station %in% TCB ~ 'TCB'
    ),
    SampleTime = mdy_hms(SampleTime, tz = 'America/Jamaica'),
    yr = year(SampleTime),
    mo = month(SampleTime)
  ) %>%
  select(bay_segment, station, SampleTime, yr, mo, everything())

##
# combine all
chldat <- epcchl %>%
  bind_rows(olddat) %>%
  bind_rows(pinchl2022) %>%
  bind_rows(pinchl2023) %>%
  bind_rows(pinchl2024) %>%
  bind_rows(manchl20222023) %>%
  bind_rows(manchl2024)

save(chldat, file = here('data/chldat.RData'))
