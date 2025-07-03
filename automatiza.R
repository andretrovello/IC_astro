#authenticate drive
library(stringr)
library(tidyverse)
library(googledrive)
library(patchwork)
library(cowplot)


#functions


clean_text <- function(line) {
  line <- unlist(strsplit(line, "\\s+")) # removes whitespaces (\\+s) between words
  line <- line[line != "#"] # removes #
  return(line)
  
}

read_data_and_filter <- function(file, skip_lines) {
  data <- read.table(file, skip = skip_lines+1, header = FALSE) # reads data skiping skip_lines+1
  header <- readLines(file)[skip_lines:(skip_lines + 1)] # gets header from lines [[skip_lines:(skip_lines + 1)]]
  
  # gets spectral indexes and units
  idx <- clean_text(header[1])
  unidade <- clean_text(header[2])
  
  # gathers idx and units in the same vector
  header_final <- paste(idx, unidade)
  colnames(data) <- header_final # transforms vector in data header
  
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

# downloads data 
download_df <- function(loc, download = FALSE){
  
  folder <- drive_get(loc) # finds folder in google drive
  content <- drive_ls(folder) # lists documents on that folder
  #print(content)
  
  sed_files <- content %>%
    filter(str_ends(name, '_sed')) # selects only the files that end with "_sed"
  

  
  # crates list to store dataframes
  files_list <- list()
  
  # Iterates over each file in sed_files 
  for (i in 1:nrow(sed_files)) {
    current_file_info <- sed_files[i, ] # stores current file in variable
    file_name_drive <- current_file_info$name # stores file name in variable 
    
    # Adds ".txt" to file name only if it doesn´t exist yet
    if (!grepl('\\.txt$', file_name_drive)) {
      local_file_name <- paste0(file_name_drive, '.txt')
    } else {
      local_file_name <- file_name_drive
    }
    files_list <- c(files_list, local_file_name) #adds new name to variable 
    
    message(paste("Processando arquivo:", file_name_drive)) # Progress info message 
    
    # if download = True, downloads the files from Google Drive to the computer
    if (download == TRUE) {
      
      drive_download(
        current_file_info,
        path = local_file_name,
        overwrite = TRUE
      )
    }
  }
  return(files_list)
}

# creates dataframe with all metallicities 
create_df <- function(files_list) {
  files_list <- sort(unlist(files_list)) # sorts file names in order
  
  
  df_combined <- data.frame() #creates new dataframe
  # iterates iver file names skiping 2 in every iteration
  for (i in seq(from = 1, to = length(files_list), by = 2)) {
    first_part <- read_data_and_filter(files_list[[i]], 31) # first half of indexes is in the even file 
    second_part <- read_data_and_filter(files_list[[i+1]], 31) # second half of indexes is in the odd file 
    # creates full data with both parts of the same metallicity 
    full_data <- inner_join(first_part, second_part, by = c('z', 'log-age (yr)')) # joins horizontally both dfs by 'z' and 'log age'
    
    df_combined <- bind_rows(df_combined, full_data) # add full data vertically in the end of the combined dataframe
    
  }
  return(df_combined)
}

drive_auth() # authenticates drive 
loc_miles <- 'IC_Astro/Input/Models_July_2019/miles' # location of miles folder 
loc_syncomil <- 'IC_Astro/Input/Models_July_2019/syncomil' # location of syncomil folder


# gets file list of each spectral library 
file_list_miles <- download_df(loc_miles, download = FALSE)
file_list_syncomil <- download_df(loc_syncomil, download = FALSE)

# creates dataframes
df_miles <- create_df(file_list_miles)
df_syncomil <- create_df(file_list_syncomil)

# filters by log-age 
df_miles_filtered <- df_miles %>%
  filter(`log-age (yr)` >= 7 & `log-age (yr)` <= 10)
df_syncomil_filtered <- df_syncomil %>%
  filter(`log-age (yr)` >= 7 & `log-age (yr)` <= 10)

# Creates list to add plots 
all_plots <- list()
plot_index <- 1 # list index

# iterates over columns starting from the third one (skips z and log-age to get only indexes)
for (name in names(df_miles_filtered)[3:length(names(df_miles_filtered))]) {
  # creates dataframe with both dataframes for the respective index (name) and log-age
  dados_plot <- data.frame(
  Miles = df_miles_filtered[[name]],
  Syncomil = df_syncomil_filtered[[name]],
  Log_Age = df_miles_filtered[["log-age (yr)"]] # Assumindo que log-age vem de miles
  )
  
  # plots scatter point graph Syncomil x Miles for current idx (name) and uses log-age to color points
  plot <- ggplot(data = dados_plot, aes(x = Syncomil, y = Miles, color = Log_Age)) +
    geom_point(size = 1.5, alpha = 0.7) +
    # plots 1-1 line
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
    labs(title = name,
         x = NULL, #remove axis names 
         y = NULL,
         color = "log-age (yr)") + # labels colormap legend
    theme_classic() +
    scale_color_viridis_c(option = "plasma", direction = -1)
  
  all_plots[[plot_index]] <- plot # adds to plot list at index plot_index
  plot_index = plot_index+1 # increments index
}

print(all_plots[1])

# Creates grid with qall figures
final_fig <- wrap_plots(all_plots, ncol = 6, nrow = 6) +
  plot_layout(
    guides = "collect", # collects all identical legends
  ) +
  # add global features to figure
  plot_annotation(
    title = 'Comparação de SPS-Syncomil vs. SPS-Miles', # Figure title (super title)
    tag_levels = 'A', # Tags figures 
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.tag = element_text(face = 'bold', size = 12, margin = margin(t = 5, l = 5)),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  ) &
  # adds global axis labels (super labels)
  labs(x = "SPS-Syncomil", y = "SPS-Miles") &
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 9), 
    legend.box.margin = margin(0, 0, 0, 15, unit = "pt") 
  )
# adds global axis labels (super labels)
final_fig <- ggdraw(final_fig) +
  draw_label("SPS-S", x = 0.5, y = 0.03, vjust = 0, hjust = 0.5, size = 12, fontface = "bold") +
  draw_label("SPS-M", x = 0.03, y = 0.5, vjust = 0.5, hjust = 0, angle = 90, size = 12, fontface = "bold")

# saves figure with all plots
ggsave(
  filename = "todos_os_plots_combinados.png",
  plot = final_fig,
  width = 20, 
  height = 30, 
  units = "in",
  dpi = 300
)

# Boxplot

join_keys <- c('z', 'log-age (yr)') # inner join keys
common_cols <- intersect(names(df_miles_filtered), names(df_syncomil_filtered)) # finds common names in dataframes
common_cols <- setdiff(common_cols, join_keys) # removes columns that are not spectral indexes

# checks columns where all elements are numeric
numeric_common_cols <- common_cols[sapply(df_miles[common_cols], is.numeric) &
                                     sapply(df_syncomil[common_cols], is.numeric)]


# combines dataframes horizontaly
df_filtered_combined <- inner_join(df_miles_filtered, df_syncomil_filtered, by = join_keys, suffix = c('_Miles', '_Syncomil'))

# creates new dataframe for delta idx
df_delta_idx <- df_filtered_combined %>%
  mutate(
    across(
      # selects columns that end in '_Miles'
      all_of(paste0(numeric_common_cols, "_Miles")),
      ~ .x - df_filtered_combined[[str_replace(cur_column(), "_Miles", "_Syncomil")]], # subtracts them from Syncomil
      .names = "{.col}_Diff" # cretes new columns with the suffix '_Diff'
    )
  ) %>%
  # removes extra '_Miles'
  rename_with(~ str_replace(., "_Miles_Diff", "_Diff"), ends_with("_Miles_Diff")) %>% # <--- CORRIGIDO
  # selects 'join keys' and 'Diff' columns
  select(all_of(join_keys), ends_with("_Diff")) 



# identifies columns that need transformation
cols_to_transform <- names(df_delta_idx %>% select(ends_with("_Diff")))

# normalizes spectral index columns using x_new = (x-mean(x))/std(x)
df_delta_idx <- df_delta_idx %>% 
  mutate(
    across(
      all_of(cols_to_transform),
      ~ {
        col_media <- mean(.x, na.rm = TRUE)
        col_desvio_padrao <- sd(.x, na.rm = TRUE)
        (.x - col_media) / col_desvio_padrao
      },
      .names = "{str_replace(.col, '_Diff$', '')}_deltaidx"
    )
  )



# --- 1. Selecionar e Transformar para Formato Longo (para colunas _deltaidx) ---
# Pega apenas as colunas que terminam com '_deltaidx'
df_deltaidx_long <- df_delta_idx %>%
  select(ends_with("_deltaidx")) %>% # Inclui as chaves se quiser mantê-las
  pivot_longer(
    cols = ends_with("_deltaidx"), # <--- As colunas a serem empilhadas (terminam com _deltaidx)
    names_to = "Caracteristica_deltaidx", # <--- Nome da nova coluna para os NOMES originais
    values_to = "Valor_deltaidx" # <--- Nome da nova coluna para os VALORES de deltaidx
  ) %>%
  # Opcional: Limpar os nomes das características no formato longo (remover '_deltaidx')
  mutate(
    Caracteristica_deltaidx = str_replace(Caracteristica_deltaidx, "_deltaidx$", "")
  )

print(head(df_deltaidx_long))
print(tail(df_deltaidx_long))

# --- 2. Criar o Boxplot (para colunas _deltaidx) ---
boxplot <- ggplot(df_deltaidx_long, aes(x = Caracteristica_deltaidx, y = Valor_deltaidx)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7) + # Cores ajustadas novamente
  labs(
    title = "Boxplots dos Valores Delta por Característica", # <--- Título ajustado
    x = "Característica", # <--- Rótulo X
    y = "Delta idx (SPS-M - SPS-S)" # <--- Rótulo Y ajustado
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotaciona os rótulos X
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA), # Fundo geral do plot (fora do painel)
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel onde os dados são plotados
  
  )

print(boxplot)

# --- Salve a figura com DIMENSÕES MAIORES ---
# Se ainda estiver espremido, AUMENTE AINDA MAIS esses valores!
ggsave(
  filename = "boxplot.png",
  plot = boxplot,
  width = 30,  # <--- LARGURA AUMENTADA (de 25 para 30)
  height = 20, # <--- ALTURA AUMENTADA (de 15 para 20)
  units = "in",
  dpi = 300
)


# idx KDE plot

# iterates over columns starting from the third one (skips z and log-age to get only indexes)
for (name in names(df_miles_filtered)[3:length(names(df_miles_filtered))]) {
  # creates dataframe with both dataframes for the respective index (name) and log-age
  dados_plot <- data.frame(
    Miles = df_miles_filtered[[name]],
    Syncomil = df_syncomil_filtered[[name]],
    Log_Age = df_miles_filtered[["log-age (yr)"]] # Assumindo que log-age vem de miles
  )
  
  # plots scatter point graph Syncomil x Miles for current idx (name) and uses log-age to color points
  plot <- ggplot(data = dados_plot, aes(x = Syncomil, y = Miles, color = Log_Age)) +
    geom_point(size = 1.5, alpha = 0.7) +
    # plots 1-1 line
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
    labs(title = name,
         x = NULL, #remove axis names 
         y = NULL,
         color = "log-age (yr)") + # labels colormap legend
    theme_classic() +
    scale_color_viridis_c(option = "plasma", direction = -1)
  
  all_plots[[plot_index]] <- plot # adds to plot list at index plot_index
  plot_index = plot_index+1 # increments index
}

print(all_plots[1])

# Creates grid with qall figures
final_fig <- wrap_plots(all_plots, ncol = 6, nrow = 6) +
  plot_layout(
    guides = "collect", # collects all identical legends
  ) +
  # add global features to figure
  plot_annotation(
    title = 'Comparação de SPS-Syncomil vs. SPS-Miles', # Figure title (super title)
    tag_levels = 'A', # Tags figures 
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.tag = element_text(face = 'bold', size = 12, margin = margin(t = 5, l = 5)),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  ) &
  # adds global axis labels (super labels)
  labs(x = "SPS-Syncomil", y = "SPS-Miles") &
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 9), 
    legend.box.margin = margin(0, 0, 0, 15, unit = "pt") 
  )
# adds global axis labels (super labels)
final_fig <- ggdraw(final_fig) +
  draw_label("SPS-S", x = 0.5, y = 0.03, vjust = 0, hjust = 0.5, size = 12, fontface = "bold") +
  draw_label("SPS-M", x = 0.03, y = 0.5, vjust = 0.5, hjust = 0, angle = 90, size = 12, fontface = "bold")


# --- 1. Selecionar e Transformar para Formato Longo (para colunas _deltaidx) ---
# Pega apenas as colunas que terminam com '_deltaidx'
df_delta_idx_kde <- df_delta_idx %>%
  select(-ends_with("_Diff")) 

all_kde_plots <- list()
kde_plot_index = 1

for(name in names(df_delta_idx_kde)[3:length(names(df_delta_idx_kde))]) {
  plot_kde <- ggplot(df_delta_idx_kde, aes(x = .data[[name]])) +
    geom_density(alpha = 0.5, fill = "cadetblue", colour = "darkblue") +
    labs(
      title = str_split(name, ' ')[[1]][1],
      x = 'delta_idx',
      y = "Densidade"
    ) 
  
  all_kde_plots[[kde_plot_index]] <- plot_kde
  kde_plot_index = kde_plot_index + 1
}

print(all_kde_plots[6])


# Creates grid with qall figures
final_kde_fig <- wrap_plots(all_kde_plots, ncol = 6, nrow = 6) +
  plot_layout(
    guides = "collect", # collects all identical legends
  ) +
  # add global features to figure
  plot_annotation(
    title = 'KDE plots', # Figure title (super title)
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.tag = element_text(face = 'bold', size = 12, margin = margin(t = 5, l = 5)),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  ) &
  # adds global axis labels (super labels)
  labs(x = NULL, y = NULL) &
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 9), 
    legend.box.margin = margin(0, 0, 0, 15, unit = "pt") 
  )
# adds global axis labels (super labels)
final_kde_fig <- ggdraw(final_kde_fig) +
  draw_label("SPS-S", x = 0.5, y = 0.03, vjust = 0, hjust = 0.5, size = 12, fontface = "bold") +
  draw_label("SPS-M", x = 0.03, y = 0.5, vjust = 0.5, hjust = 0, angle = 90, size = 12, fontface = "bold")


print(final_kde_fig)

# saves figure with all plots
ggsave(
  filename = "kde_plots.png",
  plot = final_kde_fig,
  width = 40, 
  height = 20, 
  units = "in",
  dpi = 300
)