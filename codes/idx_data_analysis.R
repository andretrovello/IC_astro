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

# gets file list of each spectral library 
file_list_miles <- download_df(loc_miles, download = FALSE, 'miles', unzip = FALSE)
file_list_syncomil <- download_df(loc_syncomil, download = FALSE, 'syncomil', unzip = FALSE)
file_list_cbc <- download_df(loc_cbc, download = FALSE, 'cbc', unzip = FALSE)

# creates dataframes
df_miles <- create_df(file_list_miles, 'miles')
df_syncomil <- create_df(file_list_syncomil, 'syncomil')
df_cbc <- create_df(file_list_cbc, 'cbc')


# --- Definição do mapeamento de renomeação como um VETOR NOMEADO no R ---
# Os VALORES são os nomes ATUAIS das colunas
# Os NOMES do vetor são os NOVOS nomes desejados
new_columns_map_cbc <- c(
  'Fe4668' = 'C4668', 'H103798' = 'H10-3798',
  'H83889' = 'H8-3889', 'H93835' = 'H9-3835'
)

new_columns_map_syncomiles <- c(
  'log_age' ='log-age', 'CN1' = 'CN_1', 'CN2' = 'CN_2',
  'Mg1' = 'Mg_1', 'Mg2' = 'Mg_2', 'Mgb' = 'Mg-b', # <-- 'Mg-b' é o nome atual
  'NaD' = 'Na-D', 'H103798' = 'H10_3798', 'CaHK' = 'BH-HK', # <-- 'BH-HK' é o nome atual
  'B4000' = 'B4_VN', 'D4000vn' = 'D(4000)', # <-- 'D(4000)' é o nome atual
  'H83889' = 'H8_3889', 'H93835' = 'H9_3835',
  'TiO1' = 'TiO_1', 'TiO2' = 'TiO_2'
)

# --- Renomear as colunas usando o vetor nomeado ---
df_syncomil_renamed <- df_syncomil %>%
  rename(all_of(new_columns_map_syncomiles)) # <--- Use all_of() para lidar com o vetor nomeado
df_miles_renamed <- df_miles %>%
  rename(all_of(new_columns_map_syncomiles)) # <--- Use all_of() para lidar com o vetor nomeado
df_cbc_renamed <- df_cbc %>%
  rename(all_of(new_columns_map_cbc)) # <--- Use all_of() para lidar com o vetor nomeado


# filters by log-age 
df_cbc_filtered <- df_cbc_renamed %>%
  filter(`log_age` >= 7 & `log_age` <= 10)
df_miles_filtered <- df_miles_renamed %>%
  filter(`log_age` >= 7 & `log_age` <= 10)
df_syncomil_filtered <- df_syncomil_renamed %>%
  filter(`log_age` >= 7 & `log_age` <= 10)

join_keys = c('z', 'log_age')
df_miles_syncomil <- inner_join(df_miles_filtered, df_syncomil_filtered, by = join_keys, suffix = c('_Miles', '_Syncomil'))
df_cbc_syncomil <- inner_join(df_cbc_filtered, df_syncomil_filtered, by = join_keys, suffix = c('_CBC', '_Syncomil'))



# -----------Plot 1-1 (miles-syncomil) -----------
# Creates list to add plots 
all_plots <- list()
plot_index <- 1 # list index

# iterates over columns starting from the third one (skips z and log-age to get only indexes)
for (name in names(df_miles_syncomil)[3:length(names(df_miles_syncomil))]) {
  # creates dataframe with both dataframes for the respective index (name) and log-age
  if (str_ends(name, '_Miles')){
    name_Miles <- name
    print(name_Miles)
    name_Syncomil <- paste0(str_remove(name,'_Miles'), '_Syncomil') 
    print(name_Syncomil)
    dados_plot <- data.frame(
      Miles = df_miles_syncomil[[name_Miles]],
      Syncomil = df_miles_syncomil[[name_Syncomil]],
      Log_Age = df_miles_syncomil[["log_age"]] # Assumindo que log-age vem de miles
    )
    
    # plots scatter point graph Syncomil x Miles for current idx (name) and uses log-age to color points
    plot <- ggplot(data = dados_plot, aes(x = Syncomil, y = Miles, color = Log_Age)) +
      geom_point(size = 1.5, alpha = 0.7) +
      # plots 1-1 line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
      labs(title = str_remove(name, '_Miles'),
           x = NULL, #remove axis names 
           y = NULL,
           color = "log-age (yr)") + # labels colormap legend
      theme_classic() +
      scale_color_viridis_c(option = "plasma", direction = -1)
    
    all_plots[[plot_index]] <- plot # adds to plot list at index plot_index
    plot_index = plot_index+1 # increments index
  }
}

# Creates grid with all figures
final_fig <- wrap_plots(all_plots, ncol = 6, nrow = 6) +
  plot_layout(
    guides = "collect", # collects all identical legends
  ) +
  # add global features to figure
  plot_annotation(
    title = 'Comparação de SPS-S vs. SPS', # Figure title (super title)
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
  filename = "milesXsyncomil(1-1).png",
  plot = final_fig,
  width = 20, 
  height = 30, 
  units = "in",
  dpi = 300
)

# -----------Plot 1-1 (cbc-syncomil) -----------
# Creates list to add plots 
all_plots <- list()
plot_index <- 1 # list index

# iterates over columns starting from the third one (skips z and log-age to get only indexes)
for (name in names(df_cbc_syncomil)[3:length(names(df_cbc_syncomil))]) {
  # creates dataframe with both dataframes for the respective index (name) and log-age
  if (str_ends(name, '_CBC')){
    name_CBC <- name
    print(name_CBC)
    name_Syncomil <- paste0(str_remove(name,'_CBC'), '_Syncomil') 
    print(name_Syncomil)
    dados_plot <- data.frame(
      Miles = df_cbc_syncomil[[name_CBC]],
      Syncomil = df_cbc_syncomil[[name_Syncomil]],
      Log_Age = df_cbc_syncomil[["log_age"]] # Assumindo que log-age vem de miles
    )
    
    # plots scatter point graph Syncomil x Miles for current idx (name) and uses log-age to color points
    plot <- ggplot(data = dados_plot, aes(x = Syncomil, y = Miles, color = Log_Age)) +
      geom_point(size = 1.5, alpha = 0.7) +
      # plots 1-1 line
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) +
      labs(title = str_remove(name, '_CBC'),
           x = NULL, #remove axis names 
           y = NULL,
           color = "log-age (yr)") + # labels colormap legend
      theme_classic() +
      scale_color_viridis_c(option = "plasma", direction = -1)
    
    all_plots[[plot_index]] <- plot # adds to plot list at index plot_index
    plot_index = plot_index+1 # increments index
  }
}

# Creates grid with all figures
final_fig <- wrap_plots(all_plots, ncol = 6, nrow = 6) +
  plot_layout(
    guides = "collect", # collects all identical legends
  ) +
  # add global features to figure
  plot_annotation(
    title = 'Comparação de SPS-Syncomil vs. SPS-CBC', # Figure title (super title)
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.tag = element_text(face = 'bold', size = 12, margin = margin(t = 5, l = 5)),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  ) &
  # adds global axis labels (super labels)
  labs(x = "SPS-S", y = "SPS-C") &
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 10, face = "bold"), 
    legend.text = element_text(size = 9), 
    legend.box.margin = margin(0, 0, 0, 15, unit = "pt") 
  )
# adds global axis labels (super labels)
final_fig <- ggdraw(final_fig) +
  draw_label("SPS-S", x = 0.5, y = 0.03, vjust = 0, hjust = 0.5, size = 12, fontface = "bold") +
  draw_label("SPS-C", x = 0.03, y = 0.5, vjust = 0.5, hjust = 0, angle = 90, size = 12, fontface = "bold")

# saves figure with all plots
ggsave(
  filename = "milesXcbc(1-1).png",
  plot = final_fig,
  width = 20, 
  height = 30, 
  units = "in",
  dpi = 300
)

# Boxplot

# ----------------------- MilesXSyncomil ---------------------------------------------
common_cols <- intersect(names(df_miles_filtered), names(df_syncomil_filtered)) # finds common names in dataframes
common_cols <- setdiff(common_cols, join_keys) # removes columns that are not spectral indexes

# checks columns where all elements are numeric
numeric_common_cols <- common_cols[sapply(df_miles_filtered[common_cols], is.numeric) &
                                     sapply(df_syncomil_filtered[common_cols], is.numeric)]


# creates new dataframe for delta idx
df_delta_idx_synthetic <- df_miles_syncomil %>%
  mutate(
    across(
      # selects columns that end in '_Miles'
      all_of(paste0(numeric_common_cols, "_Miles")),
      ~ .x - df_miles_syncomil[[str_replace(cur_column(), "_Miles", "_Syncomil")]], # subtracts them from Syncomil
      .names = "{.col}_Diff" # cretes new columns with the suffix '_Diff'
    )
  ) %>%
  # removes extra '_Miles'
  rename_with(~ str_replace(., "_Miles_Diff", "_Diff"), ends_with("_Miles_Diff")) %>% # <--- CORRIGIDO
  # selects 'join keys' and 'Diff' columns
  select(all_of(join_keys), ends_with("_Diff")) 



# identifies columns that need transformation
cols_to_transform <- names(df_delta_idx_synthetic %>% select(ends_with("_Diff")))

# normalizes spectral index columns using x_new = (x-mean(x))/std(x)
df_delta_idx_synthetic <- df_delta_idx_synthetic %>% 
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
df_deltaidx_long <- df_delta_idx_synthetic %>%
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


# ----------------------------------- cbcXSyncomil ----------------------------------------------
common_cols <- intersect(names(df_cbc_filtered), names(df_syncomil_filtered)) # finds common names in dataframes
common_cols <- setdiff(common_cols, join_keys) # removes columns that are not spectral indexes

# checks columns where all elements are numeric
numeric_common_cols <- common_cols[sapply(df_cbc_filtered[common_cols], is.numeric) &
                                     sapply(df_syncomil_filtered[common_cols], is.numeric)]


# creates new dataframe for delta idx
df_delta_idx_coverage <- df_cbc_syncomil %>%
  mutate(
    across(
      # selects columns that end in '_Miles'
      all_of(paste0(numeric_common_cols, "_CBC")),
      ~ .x - df_cbc_syncomil[[str_replace(cur_column(), "_CBC", "_Syncomil")]], # subtracts them from Syncomil
      .names = "{.col}_Diff" # cretes new columns with the suffix '_Diff'
    )
  ) %>%
  # removes extra '_Miles'
  rename_with(~ str_replace(., "_CBC_Diff", "_Diff"), ends_with("_CBC_Diff")) %>% # <--- CORRIGIDO
  # selects 'join keys' and 'Diff' columns
  select(all_of(join_keys), ends_with("_Diff")) 



# identifies columns that need transformation
cols_to_transform <- names(df_delta_idx_coverage %>% select(ends_with("_Diff")))

# normalizes spectral index columns using x_new = (x-mean(x))/std(x)
df_delta_idx_coverage <- df_delta_idx_coverage %>% 
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

# idx KDE plot
# --- 1. Selecionar e Transformar para Formato Longo (para colunas _deltaidx) ---
# Pega apenas as colunas que terminam com '_deltaidx'
df_kde_synthetic <- df_delta_idx_synthetic %>%
  select(-ends_with("_Diff")) 
df_kde_coverage <- df_delta_idx_coverage %>%
  select(-ends_with("_Diff")) 

df_kde <- inner_join(df_kde_synthetic, df_kde_coverage, by = join_keys, suffix = c('_Synthetic', '_Coverage'))


all_kde_plots <- list()
kde_plot_index = 1

for(name in names(df_kde)[3:length(names(df_kde))]) {
  if (str_ends(name, 'Synthetic')) {
    col_synthetic <- name
    col_coverage <- paste0(str_remove(name, 'Synthetic'), 'Coverage')
    plot_kde <- ggplot(df_kde) +
      geom_density(aes(x = .data[[col_synthetic]], # Acessa a coluna _synthetic
                       fill = "Synthetic",       # Mapeia para legenda 'Synthetic'
                       color = "Synthetic",      # Mapeia para cor da linha 'Synthetic'
                       y = after_stat(density)), # Garante correta altura da densidade
                   alpha = 0.3) +
      # Densidade para 'coverage'
      geom_density(aes(x = .data[[col_coverage]], # Acessa a coluna _coverage
                       fill = "Coverage",        # Mapeia para legenda 'Coverage'
                       color = "Coverage",       # Mapeia para cor da linha 'Coverage'
                       y = after_stat(density)), # Garante correta altura da densidade
                   alpha = 0.3) +
      
      labs(
        title = str_remove(name, '_deltaidx_Synthetic'), # Título do subplot
        x = "Valor da Característica", 
        y = "Densidade",
        fill = "Tipo de Dado", # Título da legenda de preenchimento
        color = "Tipo de Dado" # Título da legenda de linha
      ) +
      scale_color_manual(
        name = "Tipo de Dado", # Título da legenda (será coletada)
        values = c("Synthetic" = "red", "Coverage" = "cadetblue")
      ) +
      scale_fill_manual(
        name = "Tipo de Dado", # Título da legenda (será coletada)
        values = c("Synthetic" = "red", "Coverage" = "cadetblue") # Use cores sólidas ou transparentes com alpha
      )
    
    all_kde_plots[[kde_plot_index]] <- plot_kde
    kde_plot_index = kde_plot_index + 1
  }
}

print(all_kde_plots[1])


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



