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

base_path = "C:/Users/dedet/Desktop/IC_astro/input/"

# gets file list of each spectral library 
file_list_miles <- download_df(loc_miles, download = FALSE, 'miles', unzip = FALSE)
file_list_syncomil <- download_df(loc_syncomil, download = FALSE, 'syncomil', unzip = FALSE)
file_list_cbc <- download_df(loc_cbc, download = TRUE, 'cbc', unzip = TRUE)

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
df_syncomil <- df_syncomil %>%
  rename(all_of(new_columns_map_syncomiles)) # <--- Use all_of() para lidar com o vetor nomeado
df_miles <- df_miles %>%
  rename(all_of(new_columns_map_syncomiles)) # <--- Use all_of() para lidar com o vetor nomeado
df_cbc <- df_cbc %>%
  rename(all_of(new_columns_map_cbc)) # <--- Use all_of() para lidar com o vetor nomeado


df_cbc_long <- pivot_longer(df_cbc, cols = -c('z', 'log_age'), names_to = 'idx_name', values_to = 'idx_value') |>
  mutate(spec_library = 'cbc')
df_miles_long <- pivot_longer(df_miles, cols = -c('z', 'log_age'), names_to = 'idx_name', values_to = 'idx_value') |>
  mutate(spec_library = 'miles')
df_syncomil_long <- pivot_longer(df_syncomil, cols = -c('z', 'log_age'), names_to = 'idx_name', values_to = 'idx_value') |>
  mutate(spec_library = 'syncomil')


# filters by log-age 
df_cbc_filtered <- df_cbc_long %>%
  filter(`log_age` >= 7 & `log_age` <= 10)
df_miles_filtered <- df_miles_long %>%
  filter(`log_age` >= 7 & `log_age` <= 10)
df_syncomil_filtered <- df_syncomil_long %>%
  filter(`log_age` >= 7 & `log_age` <= 10)

# FILTER USING THE SAME INDEX LIST AS THE ORIGINAL PAPER
# Read the predefined index list used in the original paper
idx_list_paper <- read_csv("C:/Users/dedet/Desktop/IC_astro/codes/Paula/idx_list.csv", show_col_types = FALSE)
selected_indices <- idx_list_paper$idx_name

cbc_syncomil <- inner_join(
  df_cbc_filtered,
  df_syncomil_filtered,
  by = c("z", "log_age", "idx_name"),
  suffix = c("_cbc", "_syncomil") # Sufixos para idx_value
) %>%
  # Selecionar apenas as colunas necessárias para o plot
  select(z, log_age, idx_name, idx_value_cbc, idx_value_syncomil) %>%
  filter(idx_name %in% selected_indices)

# --- 3. Criar o Scatter Plot com facet_wrap ---

fig9 <- ggplot(cbc_syncomil, aes(x = idx_value_syncomil, y = idx_value_cbc, color = log_age)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) + # Linha 1:1
  facet_wrap(~ idx_name, scales = "free", ncol = 5) + # <--- facet_wrap para cada índice
  labs(
    title = "Figure 9: Spectral indices predicted by SPS-C (y-axis) versus SPS-S (x-axis) models.",
    x = "Valor do Índice (SynMILES)",
    y = "Valor do Índice (CBC)",
    color = "log(Idade) (yr)" # Rótulo da legenda de cor
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 7), # Texto dos eixos (números)
    legend.position = "right", # Posição da legenda de cor
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    plot.background = element_rect(fill = "white", colour = NA), # Fundo de todo o gráfico
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel de dados
  ) +
  # Usar uma escala de cor contínua para log_age
  scale_color_viridis_c(option = "plasma", direction = -1) 

# Ou se quiser limites fixos para todos os plots
# coord_cartesian(xlim = c(-0.5, 2.0), ylim = c(-0.5, 2.0))

# Save the plot
ggsave("C:/Users/dedet/Desktop/IC_astro/output/fig9_cbcXsyncomil_(1-1).png", fig9, 
       width = 12, height = 16, units = "in", dpi = 300)


miles_syncomil <- inner_join(
  df_miles_filtered,
  df_syncomil_filtered,
  by = c("z", "log_age", "idx_name"),
  suffix = c("_miles", "_syncomil") # Sufixos para idx_value
) %>%
  # Selecionar apenas as colunas necessárias para o plot
  select(z, log_age, idx_name, idx_value_miles, idx_value_syncomil) %>%
  filter(idx_name %in% selected_indices)

# --- 3. Criar o Scatter Plot com facet_wrap ---

fig8 <- ggplot(miles_syncomil, aes(x = idx_value_syncomil, y = idx_value_miles, color = log_age)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", linewidth = 0.8) + # Linha 1:1
  facet_wrap(~ idx_name, scales = "free", ncol = 5) + # <--- facet_wrap para cada índice
  labs(
    x = "SPS-S", 
    y = "SPS-C",
    title = "Figure 8: Spectral indices predicted by SPS-M (y-axis) versus SPS-S (x-axis) models.",
    color = "log age (yr)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 7), # Texto dos eixos (números)
    legend.position = "right", # Posição da legenda de cor
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    plot.background = element_rect(fill = "white", colour = NA), # Fundo de todo o gráfico
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel de dados
    
  ) +
  # Usar uma escala de cor contínua para log_age
  scale_color_viridis_c(option = "plasma", direction = -1) 

# Ou se quiser limites fixos para todos os plots
# coord_cartesian(xlim = c(-0.5, 2.0), ylim = c(-0.5, 2.0))

# Save the plot
ggsave("C:/Users/dedet/Desktop/IC_astro/output/fig8_milesXsyncomil_(1-1).png", fig8, 
       width = 12, height = 16, units = "in", dpi = 300)



