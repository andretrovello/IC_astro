# importing libraries 
library(stringr)
library(tidyverse)
library(googledrive)
library(patchwork)
library(cowplot)
library(R.utils)

source('C:/Users/dedet/Desktop/IC_astro/codes/full_wrangling.R')

drive_auth() # authenticates drive 
loc_miles <- 'IC_Astro/Input/Models_July_2019/miles' # location of miles folder 
loc_syncomil <- 'IC_Astro/Input/Models_July_2019/syncomil' # location of syncomil folder
loc_cbc <- 'IC_Astro/Input/Models_July_2019/cbc' # location of cbc folder 


files_cbc <- download_df(loc_cbc, download = TRUE, spec_library = 'cbc', unzip = TRUE, cbc_filetype = 'colors.gz')


