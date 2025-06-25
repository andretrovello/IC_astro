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

if (!grepl('\\.txt$', sed_files$name[1])) {
    file_name <- paste0(sed_files$name[1], '.txt')
}

# Baixar o primeiro arquivo da lista para um arquivo temporário
drive_download(
    sed_files[1, ],               # Seleciona a primeira linha do dataframe
    path = file_name,  # Nome do arquivo local
    overwrite = TRUE             # Sobrescreve se já existir
  )

#txt <- read_lines(file_name)
#getwd()

clean_text <- function(line) {
    line <- unlist(strsplit(line, "\\s+"))
    line <- line[line != "#"]
    return(line)

}

read_data <- function(file, skip_lines) {
    data <- read.table(file, skip = skip_lines+1, header = FALSE)
    header <- readLines(file)[skip_lines:(skip_lines + 1)]

    print(header[2])

    idx <- clean_text(header[1])
    unidade <- clean_text(header[2])

    header_final <- paste(idx, unidade)
    colnames(data) <- header_final
    return(data)
    
}

data <- read_data(file_name, 31)
print(data)

