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


data <- read.table(file_name, skip = 32, header = FALSE)

#txt <- read_lines(file_name)
print(data)

#getwd()

header <- readLines(file_name)[31:32]
print(header)

clean_text <- function(line) {
    line <- unlist(strsplit(line, "\\s+"))
    line <- line[line != "#"]

}

read_data <- function(file, skip_lines) {
    data <- read.table(file, skip = skip_lines, header = FALSE)
    header <- readLines(file)[skip_lines-1:skip_lines]

    print(header)

    idx <- clean_text(header[1])
    unidade <- clean_text(header[2])

    header_final <- paste(idx, unidade)
    colnames(data) <- header_final
    return(data)
    
}

data <- read_data(file_name, 32)
print(data)


colnames(data) <- header_final

print(data)


idx <- clean_text(header[1])
unidade <- clean_text(header[2])

print(idx)
print(unidade)

header_final <- paste(idx, unidade)
print(header_final)

colnames(data) <- header_final

print(data)