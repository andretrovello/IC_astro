#authenticate drive
library(stringr)
library(tidyverse)
library(googledrive)
library(patchwork)
library(cowplot)

mixture2metallicity <- function(mixture) {
  if (mixture == "m20p04"){metallicity = "0.0002"}
  else if (mixture == "m10p04"){metallicity = "0.004"}
  else if (mixture == "m05p02"){metallicity = "0.008"}
  else if (mixture == "p00p00"){metallicity = "0.017"}
  else if (mixture == "p02p00"){metallicity = "0.030"}
  
  return(metallicity)
}

clean_text <- function(line) {
  line <- unlist(strsplit(line, "\\s+")) # removes whitespaces (\\+s) between words
  #line <- line[line != "#"] # removes #
  line <- str_replace(line, "^#", "") # Substitui '#' no início (^) por nada ("")
  return(line)
}

read_data_and_filter <- function(base_path = "C:/Users/dedet/Desktop/IC_astro/input/", file, skip_lines, spec_library) {
  print(file)
  print(base_path)
  print(paste0(base_path,file))
  data <- read.table(paste0(base_path,file), skip = skip_lines+1, header = FALSE) # reads data skiping skip_lines+1
  header <- readLines(paste0(base_path,file))[skip_lines] # gets header from lines [[skip_lines:(skip_lines + 1)]]
  
  # gathers idx and units in the same vector
  header_final <- clean_text(header)
  colnames(data) <- header_final # transforms vector in data header
  
  if (spec_library == 'cbc') {
  
    # finds metallicity value in file name and extracts it 
    mixture <- str_split(file, '_')[[1]][7]
    print(mixture)
    metallicity <- mixture2metallicity(mixture)

    
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
      mutate(z = as.numeric(metallicity), 
             .before = 1 ) # adds first column with the metallicity
    
    return(select(data_colunas_filtradas, -all_of("#Isochrone_file_name")))
  }
  
  else if (spec_library == 'miles' | spec_library == 'syncomil'){
    # finds metallicity value in file name and extracts it 
    metallicity <- str_split(file, '_')[[1]][2]
    metallicity <- sub('z', '0.', metallicity)
    
    # Gets first column name in the file 
    primeira_coluna_nome <- names(file)[1] 
    
    # filters data to eliminate useless values
    data_colunas_filtradas <- data %>%
      select(
        all_of(primeira_coluna_nome), # maintains first column
        where(
          ~ !any(. == -999|. == -99 | . == 999) # maintains other columns if all values differ fro  (-99, -999, 999)
        )
      )
    
    data_colunas_filtradas <- data_colunas_filtradas %>%
      mutate(z = as.numeric(metallicity), 
             .before = 1 ) # adds first column with the metallicity
    
    return(data_colunas_filtradas)
    
  }
}

# downloads data 
download_df <- function(loc, download = FALSE, spec_library, unzip = FALSE, synco_miles_filetype = '_sed', cbc_filetype = 'lsindx.gz', base_path = "C:/Users/dedet/Desktop/IC_astro/input/"){
  
  folder <- drive_get(loc) # finds folder in google drive
  content <- drive_ls(folder) # lists documents on that folder
  #print(content)
  
  
  # crates list to store dataframes
  files_list <- list()
  
  if (spec_library == 'miles' | spec_library == 'syncomil') {
    sed_files <- content %>%
      filter(str_ends(name, synco_miles_filetype)) # selects only the files that end with "_sed"

    # Iterates over each file in sed_files 
    for (i in 1:nrow(sed_files)) {
      current_file_info <- sed_files[i, ] # stores current file in variable
      file_name_drive <- current_file_info$name # stores file name in variable 
      
      final_local_file_path <- file.path(base_path, file_name_drive)
      files_list <- c(files_list, file_name_drive)
      
      # Adds ".txt" to file name only if it doesn´t exist yet
      #if (!grepl('\\.txt$', final_local_file_path)) {
      #local_file_name <- paste0(file_name_drive, '.txt')
      #} else {
      #  local_file_name <- file_name_drive
      #}
      #files_list <- c(files_list, local_file_name) #adds new name to variable 
      
      message(paste("Processando arquivo:", file_name_drive)) # Progress info message 
      
      # if download = True, downloads the files from Google Drive to the computer
      if (download == TRUE) {
        
        drive_download(
          current_file_info,
          path = final_local_file_path,
          overwrite = TRUE
        )
      }
    }
  }
  else if (spec_library == 'cbc') {
    sed_files <- content %>% 
      filter(str_ends(name, cbc_filetype))
    
    for (i in 1:nrow(sed_files)){
      current_file <- sed_files[i,]
      drive_file_name <- current_file$name 
      
      
      final_local_file_path <- file.path(base_path, drive_file_name)
      output_file_path <- str_remove(final_local_file_path, "\\.gz$")
      files_list <- c(files_list, drive_file_name)
      
      if (download == TRUE) {
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
      if (unzip == TRUE) {
        # --- Definir o caminho para o arquivo descompactado ---
        # Geralmente, você remove a extensão .gz do nome.
        # str_remove() é útil para isso.
        
        #output_files <- c(output_files, output_file_path)
        
        message(paste("Tentando descompactar:", final_local_file_path, "para", output_file_path))
        
        tryCatch({
          gunzip(
            filename = final_local_file_path,
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
      
    }
    
  }
  return(files_list)
}


# creates dataframe with all metallicities 
create_df <- function(files_list, spec_library) {
  files_list <- sort(unlist(files_list)) # sorts file names in order
  df_combined <- data.frame() #creates new dataframe
  
  if (spec_library == 'miles' | spec_library == 'syncomil') {
    # iterates iver file names skiping 2 in every iteration
    for (i in seq(from = 1, to = length(files_list), by = 2)) {
      first_part <- read_data_and_filter(file = files_list[[i]], skip_lines = 31, spec_library = 'miles') # first half of indexes is in the even file 
      second_part <- read_data_and_filter(file = files_list[[i+1]], skip_lines = 31, spec_library = 'miles') # second half of indexes is in the odd file 
      # creates full data with both parts of the same metallicity 
      full_data <- inner_join(first_part, second_part, by = c('z', 'log-age')) # joins horizontally both dfs by 'z' and 'log age'
      
      df_combined <- bind_rows(df_combined, full_data) # add full data vertically in the end of the combined dataframe

    }
  }
  
  else if (spec_library == 'cbc'){
    for (i in 1:length(files_list)) {
      # creates full data with both parts of the same metallicity 
      full_data <- read_data_and_filter(file = files_list[i], skip_lines = 2, spec_library = 'cbc')
      
      df_combined <- bind_rows(df_combined, full_data) # add full data vertically in the end of the combined dataframe
      
    }
  }
  return(df_combined)
}


fix_syncomiles_header_colors <- function(header, spec_library) {
  if (spec_library == 'miles' | spec_library == 'syncomil'){
    for (i in 1:length(header)) {
      if (i >= 2 & i <= 6) {
        header[i] = paste0(header[i], '_SDSS')
      }
    }
  }
  else if (spec_library == 'cbc'){
    for (i in 1:length(header)) {
      if (i >= 3 & i <= 7) {
        header[i] = paste0(header[i], '_SDSS')
      }
    }
  }
  return (header)
}


# creates dataframe with all metallicities 
create_df_colors <- function(files_list, spec_library) {
  files_list <- sort(unlist(files_list)) # sorts file names in order
  df_combined <- data.frame() #creates new dataframe
  
  if (spec_library == 'miles' | spec_library == 'syncomil') {
    # iterates iver file names skiping 2 in every iteration
    for (i in 1:length(files_list)) {
      # creates full data with both parts of the same metallicity 
      full_data <- read_data_and_filter_colors(file = files_list[i], skip_lines = 30, spec_library = 'miles')
      
      df_combined <- bind_rows(df_combined, full_data) # add full data vertically in the end of the combined dataframe
      
    }
  }
  
  else if (spec_library == 'cbc'){
    for (i in 1:length(files_list)) {
      # creates full data with both parts of the same metallicity 
      full_data <- read_data_and_filter_colors(file = files_list[i], skip_lines = 1, spec_library = 'cbc')
      
      df_combined <- bind_rows(df_combined, full_data) # add full data vertically in the end of the combined dataframe
      
    }
  }
  return(df_combined)
}

read_data_and_filter_colors <- function(base_path = "C:/Users/dedet/Desktop/IC_astro/input/", file, skip_lines, spec_library) {
  data <- read.table(paste0(base_path,file), skip = skip_lines+1, header = FALSE) # reads data skiping skip_lines+1
  header <- readLines(paste0(base_path,file))[skip_lines] # gets header from lines [[skip_lines:(skip_lines + 1)]]
  
  # gathers idx and units in the same vector
  header_final <- clean_text(header)
  
  
  if (spec_library == 'cbc') {
    colnames(data) <- fix_syncomiles_header_colors(header_final, spec_library)
    
    # finds metallicity value in file name and extracts it 
    mixture <- str_split(file, '_')[[1]][7]
    metallicity <- mixture2metallicity(mixture)
    
    
    # Gets first column name in the file 
    primeira_coluna_nome <- names(file)[2]
    
    data_colunas_filtradas <- data %>%
      mutate(Z = as.numeric(metallicity), 
             .before = 1 ) # adds first column with the metallicity
    
    return(select(data_colunas_filtradas, -all_of("Isochrone")))
  }
  
  else if (spec_library == 'miles' | spec_library == 'syncomil'){
    colnames(data) <- fix_syncomiles_header_colors(header_final, spec_library)
    # finds metallicity value in file name and extracts it 
    metallicity <- str_split(file, '_')[[1]][2]
    metallicity <- sub('z', '0.', metallicity)
    
    # Gets first column name in the file 
    primeira_coluna_nome <- names(file)[1] 
    
    
    data_colunas_filtradas <- data %>%
      mutate(Z = as.numeric(metallicity), 
             .before = 1 ) # adds first column with the metallicity
    
    return(data_colunas_filtradas)
    
  }
}

