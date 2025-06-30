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

print(sed_files)

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
    
    metallicity <- str_split(file, '_')[[1]][2]
    metallicity <- sub('z', '0.', metallicity)
    
    data_colunas_filtradas <- data_colunas_filtradas %>%
      mutate(z = as.numeric(metallicity), # Primeiro, cria a coluna 'z'
             .before = 1) 
    
    return(data_colunas_filtradas)
    
}

data <- read_data(file_name, 31)
#print(data)


# Identifica a primeira coluna pelo nome, se for consistente, ou por índice
# Assumindo que a primeira coluna é '1 Wavelength /angstrom' ou a primeira em geral
primeira_coluna_nome <- names(data)[1]

data_colunas_filtradas <- data %>%
  select(
    all_of(primeira_coluna_nome), # Sempre manter a primeira coluna
    where(
      ~ !any(. == -999|. == -99 | . == 999) # Para as outras colunas, manter se NENHUM valor for -99 ou -999
    )
  )

print(data_colunas_filtradas)
print(file_name)

metallicity <- str_split(file_name, '_')[[1]][2]
print(metallicity)
meta <- sub('z', '0.', metallicity)
print(meta)

data_colunas_filtradas <- data_colunas_filtradas %>%
  mutate(z = as.numeric(meta), # Primeiro, cria a coluna 'z'
         .before = 1) 

# Verifique o resultado
print(head(data_colunas_filtradas)) # Mostra as primeiras linhas com a nova coluna 'z'



