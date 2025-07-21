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

base_path = "C:/Users/dedet/Desktop/IC_astro/input"
# gets file list of each spectral library 
file_list_miles <- download_df(loc_miles, download = TRUE, 'miles', unzip = FALSE, base_path = base_path)
file_list_syncomil <- download_df(loc_syncomil, download = TRUE, 'syncomil', unzip = TRUE, base_path = base_path)
file_list_cbc <- download_df(loc_cbc, download = TRUE, 'cbc', unzip = TRUE, base_path = base_path)

print(file_list_cbc)
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

df_cbc_long <- pivot_longer(df_cbc, cols = -c('z', 'log_age'), names_to = 'idx_name', values_to = 'idx_value')
df_miles_long <- pivot_longer(df_miles, cols = -c('z', 'log_age'), names_to = 'idx_name', values_to = 'idx_value')
df_syncomil_long <- pivot_longer(df_syncomil, cols = -c('z', 'log_age'), names_to = 'idx_name', values_to = 'idx_value')


# filters by log-age 
df_cbc_filtered <- df_cbc_long %>%
  filter(`log_age` >= 7 & `log_age` <= 10)
df_miles_filtered <- df_miles_long %>%
  filter(`log_age` >= 7 & `log_age` <= 10)
df_syncomil_filtered <- df_syncomil_long %>%
  filter(`log_age` >= 7 & `log_age` <= 10)



synthetic_effect <- inner_join(
  df_miles_filtered, df_syncomil_filtered,
  by = c("log_age", "z", "idx_name"), 
  suffix = c("_ref", "_comp")
) |>
  mutate(
    delta = idx_value_comp - idx_value_ref,
    question = "Synthetic effect"
  )


coverage_effect <- inner_join(
  df_cbc_filtered, df_syncomil_filtered,
  by = c("log_age", "z", "idx_name"), 
  suffix = c("_ref", "_comp")
) |>
  mutate(
    delta = idx_value_comp - idx_value_ref,
    question = "Coverage effect"
  )

# Combine both comparisons
indices_combined <- bind_rows(
  select(synthetic_effect, log_age, z, idx_name, delta, question),
  select(coverage_effect, log_age, z, idx_name, delta, question)
)

# FILTER USING THE SAME INDEX LIST AS THE ORIGINAL PAPER
# Read the predefined index list used in the original paper
idx_list_paper <- read_csv("C:/Users/dedet/Desktop/IC_astro/codes/Paula/idx_list.csv", show_col_types = FALSE)
selected_indices <- idx_list_paper$idx_name

# Filter to only include indices from the predefined list (ordered by central wavelength)
indices_combined <- indices_combined |>
  filter(idx_name %in% selected_indices) |>
  mutate(
    question = factor(question, levels = c("Synthetic effect", "Coverage effect")),
    idx_name = factor(idx_name, levels = selected_indices)
  ) |>
  drop_na()  # Remove any missing values


# Create the multi-panel density plot
fig7 <- ggplot(indices_combined, aes(delta, color = question, fill = question)) + 
  # Add vertical line at zero difference
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed") +
  # Create density curves for each effect
  geom_density(aes(y = after_stat(scaled)), alpha = 0.2, position = "identity") +  
  # Create separate panel for each index
  facet_wrap(~ idx_name, scales = "free_x", ncol = 5) +
  # Set axis labels and title
  labs(
    x = expression(paste(Delta, "idx")), 
    y = "Scaled density",
    title = "Figure 7: Spectral Index Differences Between Stellar Population Models",
    subtitle = "Density plots showing the distributions of index differences",
    color = "Effect Type",
    fill = "Effect Type"
  ) +
  # Apply clean theme
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.title = element_blank(), 
    legend.position = "bottom",
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    # --- ADICIONE OU VERIFIQUE ESTAS LINHAS ---
    plot.background = element_rect(fill = "white", colour = NA), # Fundo de todo o gráfico
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel de dados
    # ----------------------------------------
  ) +
  # Set colors for the two effects
  scale_color_manual(values = c("Synthetic effect" = "#F0ACA4", 
                                "Coverage effect" = "#99D495")) +
  scale_fill_manual(values = c("Synthetic effect" = "#F0ACA4", 
                               "Coverage effect" = "#99D495"))

# Display the plot
print(fig7)

# Save the plot
ggsave("C:/Users/dedet/Desktop/IC_astro/output/fig7_indices_density.png", fig7, 
       width = 16, height = 12, units = "in", dpi = 300)

cat("\nFigure 7 saved as 'figure7_indices_density.png'\n")
