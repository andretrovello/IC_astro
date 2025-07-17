#authenticate drive
library(stringr)
library(tidyverse)
library(googledrive)
library(patchwork)
library(cowplot)
library(R.utils)


drive_auth() # authenticates drive 
loc_cbc <- 'IC_Astro/Input/Models_July_2019/cbc' # location of miles folder 

folder <- drive_get(loc_cbc) # finds folder in google drive
content <- drive_ls(folder) # lists documents on that folder

print(content)

sed_files <- content %>% 
  filter(str_ends(name, 'lsindx.gz'))

print(sed_files[[1]][1])
metallicity <- str_split(sed_files[[1]][1], '_')[[1]][7]
print(metallicity)
meta <- str_remove(metallicity, "p")
print(meta)

base_path <- "C:/Users/dedet/Documents"

for (i in 1:nrow(sed_files)){
  current_file <- sed_files[i,]
  drive_file_name <- current_file$name 
  
  final_local_file_path <- file.path(base_path, drive_file_name)
  message(paste("Baixando:", drive_file_name))
  
  # drive_download espera:
  # - 'file': o identificador do arquivo no Drive (pode ser o tibble de 1 linha, o ID, ou o nome exato)
  # - 'path': O caminho completo local (UM STRING) para salvar o arquivo
  drive_download(
    file = current_file, # Passa o tibble de 1 linha que representa o arquivo do Drive
    path = final_local_file_path,  # O caminho local único para este arquivo
    overwrite = TRUE          # Sobrescreve se o arquivo já existir localmente
  )
  message("Download de", drive_file_name, "COMPLETO.")

}

output_files <- list()
for (i in 1:nrow(sed_files)) {
  current_file <- sed_files[i,]
  drive_file_name <- current_file$name
  
  full_path_to_gz <- file.path(base_path, drive_file_name)
  
  # --- Definir o caminho para o arquivo descompactado ---
  # Geralmente, você remove a extensão .gz do nome.
  # str_remove() é útil para isso.
  output_file_path <- str_remove(full_path_to_gz, "\\.gz$")
  output_files <- c(output_files, output_file_path)
  
  message(paste("Tentando descompactar:", full_path_to_gz, "para", output_file_path))
  
  tryCatch({
    gunzip(
      filename = full_path_to_gz,
      destname = output_file_path,
      remove = FALSE, # Mantenha o arquivo .gz original, não o exclua
      overwrite = TRUE # Sobrescreva o arquivo de destino se ele já existir
    )
    message(paste("Arquivo descompactado com sucesso para:", output_file_path))
  }, error = function(e) {
    message(paste("ERRO ao descompactar:", e$message))
    message("Verifique se o arquivo .gz existe no caminho e se você tem permissão de escrita no destino.")
  })
}

print(output_files)

clean_text <- function(line) {
  line <- unlist(strsplit(line, "\\s+")) # removes whitespaces (\\+s) between words
  line <- line[line != "#"] # removes hashtag
  return(line)
}

read_data_and_filter <- function(file, skip_lines) {
  data <- read.table(file, skip = skip_lines+1, header = FALSE) # reads data skiping skip_lines+1
  header <- readLines(file)[skip_lines] # gets header from lines [[skip_lines:(skip_lines + 1)]]
  
  # gathers idx and units in the same vector
  header_final <- clean_text(header)
  colnames(data) <- header_final # transforms vector in data header
  
  # finds metallicity value in file name and extracts it 
  metallicity <- str_split(file, '_')[[1]][7]
  print(metallicity)
  #metallicity <- str_remove(metallicity, "p")
  #print(metallicity)
  #metallicity <- sub('p', '.', metallicity)
  #print(metallicity)
  
  # Gets first column name in the file 
  primeira_coluna_nome <- names(file)[2] 
  
  # filters data to eliminate useless values
  data_colunas_filtradas <- data %>%
    select(
      all_of(primeira_coluna_nome),# maintains first column
      -all_of('#Isochrone_file_name'),
      where(
        ~ !any(. == -999|. == -99 | . == 999) # maintains other columns if all values differ fro  (-99, -999, 999)
      )
    )
  
  data_colunas_filtradas <- data_colunas_filtradas %>%
    mutate(z = metallicity, 
           .before = 1 ) # adds first column with the metallicity
  
  return(select(data_colunas_filtradas, -all_of("#Isochrone_file_name")))
  
}


skip_lines = 1
out_file <- str_remove(sed_files[[1]][1], "\\.gz$")
print(out_file)
data <- read.table(out_file, header = FALSE)
header <- readLines(out_file)[2]

header_final <- clean_text(header)
colnames(data) <- header_final # transforms vector in data header


skip_lines = 1
out_file <- str_remove(sed_files[[1]][1], "\\.gz$")
print(out_file)
data <- read_data_and_filter(out_file, 2)

print(names(data))

# creates dataframe with all metallicities 
create_df <- function(files_list) {
  outputs <- sort(unlist(output_files))
  
  df_combined <- data.frame() #creates new dataframe
  # iterates over file names skiping 2 in every iteration
  for (i in 1:length(outputs)) {
    # creates full data with both parts of the same metallicity 
    full_data <- read_data_and_filter(outputs[i], 2)
    
    df_combined <- bind_rows(df_combined, full_data) # add full data vertically in the end of the combined dataframe
    
  }
  return(df_combined)
}


full_data <- create_df(sed_files)
print(sed_files[[1]])

print(nrow(sed_files))
files_list <- sort(unlist(sed_files)) # sorts file names in order
print(files_list)

print(length(sort(unlist(output_files))))
      
