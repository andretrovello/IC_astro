#authenticate drive
library(stringr)
library(tidyverse)
library(googledrive)

drive_auth()

loc <- 'IC_Astro/Input/Models_July_2019/miles'

folder <- drive_get(loc)

content <- drive_ls(folder)
#print(content)

sed_files <- content %>%
    filter(str_ends(name, '_sed'))

print(sed_files$name[1])

if (!grepl('\\.txt$', sed_files$name[1])) {
    file_name <- paste0(sed_files$name[1], '.txt')
}

# Baixar o primeiro arquivo da lista para um arquivo temporário
drive_download(
    sed_files[1, ],               # Seleciona a primeira linha do dataframe
    path = file_name,  # Nome do arquivo local
    overwrite = TRUE             # Sobrescreve se já existir
  )


txt <- read_lines(file_name)
head(txt)
