#authenticate drive
library(stringr)
library(tidyverse)
library(googledrive)

#functions

clean_text <- function(line) {
  line <- unlist(strsplit(line, "\\s+"))
  line <- line[line != "#"]
  return(line)
  
}

read_data_and_filter <- function(file, skip_lines) {
  data <- read.table(file, skip = skip_lines+1, header = FALSE)
  header <- readLines(file)[skip_lines:(skip_lines + 1)]
  
  print(header[2])
  
  idx <- clean_text(header[1])
  unidade <- clean_text(header[2])
  
  header_final <- paste(idx, unidade)
  colnames(data) <- header_final
  
  metallicity <- str_split(file, '_')[[1]][2]
  metallicity <- sub('z', '0.', metallicity)
  
  primeira_coluna_nome <- names(file)[1]
  
  data_colunas_filtradas <- data %>%
    select(
      all_of(primeira_coluna_nome), # Sempre manter a primeira coluna
      where(
        ~ !any(. == -999|. == -99 | . == 999) # Para as outras colunas, manter se NENHUM valor for -99 ou -999
      )
    )
  
  data_colunas_filtradas <- data_colunas_filtradas %>%
    mutate(z = as.numeric(metallicity), # Primeiro, cria a coluna 'z'
           .before = 1) 
  
  return(data_colunas_filtradas)
  
}

download_df <- function(loc, download = FALSE){
  
  folder <- drive_get(loc)
  content <- drive_ls(folder)
  #print(content)
  
  sed_files <- content %>%
    filter(str_ends(name, '_sed'))
  
  # ------------------------------------------------------------------
  # NOVO: Loop para processar cada arquivo SED
  # ------------------------------------------------------------------
  
  # Crie uma lista vazia para armazenar os dataframes processados de cada arquivo
  files_list <- list()
  
  # Itera sobre cada linha (que representa um arquivo) no dataframe sed_files
  for (i in 1:nrow(sed_files)) {
    current_file_info <- sed_files[i, ]
    file_name_drive <- current_file_info$name
    
    # Garante que o nome do arquivo local tenha a extensão .txt
    if (!grepl('\\.txt$', file_name_drive)) {
      local_file_name <- paste0(file_name_drive, '.txt')
    } else {
      local_file_name <- file_name_drive
    }
    files_list <- c(files_list, local_file_name)
    
    message(paste("Processando arquivo:", file_name_drive)) # Mensagem para acompanhar o progresso
    
    if (download == TRUE) {
      # Baixar o arquivo atual
      drive_download(
        current_file_info,
        path = local_file_name,
        overwrite = TRUE
      )
    }
  }
  return(files_list)
}

create_df <- function(files_list) {
  files_list <- sort(unlist(files_list))
  
  
  df_combined <- data.frame()
  for (i in seq(from = 1, to = length(files_list), by = 2)) {
    first_part <- read_data_and_filter(files_list[[i]], 31)
    second_part <- read_data_and_filter(files_list[[i+1]], 31)
    full_data <- inner_join(first_part, second_part, by = c('z', 'log-age (yr)'))
    
    df_combined <- bind_rows(df_combined, full_data)
    
  }
  return(df_combined)
}

drive_auth()
loc_miles <- 'IC_Astro/Input/Models_July_2019/miles'
loc_syncomil <- 'IC_Astro/Input/Models_July_2019/syncomil'



file_list_miles <- download_df(loc_miles, download = TRUE)
file_list_syncomil <- download_df(loc_syncomil, download = TRUE)


df_miles <- create_df(file_list_miles)
df_syncomil <- create_df(file_list_syncomil)


print(name(df_syncomil))
      
df_syncomil_filtered <- df_miles %>%
  filter(`log-age (yr)` >= 7 & `log-age (yr)` <= 10)
df_syncomil_filtered <- df_syncomil %>%
  filter(`log-age (yr)` >= 7 & `log-age (yr)` <= 10)


# Supondo que df_miles_filtered e df_syncomil_filtered já estão definidos
# e contêm a coluna `CN_1 (mag)` com dados compatíveis e alinhados.

ggplot() +
  geom_point(aes(y = df_miles_filtered$`CN_1 (mag)`, x = df_syncomil_filtered$`CN_1 (mag)`),
             color = "red", size = 1, alpha = 0.7) +
  # --- ADICIONA A LINHA 1:1 AQUI ---
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", size = 0.8) +
  # ---------------------------------
labs(title = "CN_1",
     x = "Miles",
     y = "Syncomil") +
  theme_classic()
      