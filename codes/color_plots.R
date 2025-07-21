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

file_list_miles <- download_df(loc_miles, download = FALSE, spec_library = 'miles', unzip = FALSE, synco_miles_filetype = '1ABmag')
file_list_syncomil <- download_df(loc_syncomil, download = FALSE, spec_library = 'syncomil', unzip = FALSE, synco_miles_filetype = '1ABmag')
file_cbc <- download_df(loc_cbc, download = FALSE, spec_library = 'cbc', unzip = FALSE, cbc_filetype = 'ABmag.gz')

# creates dataframes
df_miles <- create_df_colors(file_list_miles, 'miles')
df_syncomil <- create_df_colors(file_list_syncomil, 'syncomil')
df_cbc <- create_df_colors(file_cbc, 'cbc')

new_columns_map_syncomiles <- c(
  'log_age_yr' ='log-age-yr'
  )

# --- Renomear as colunas usando o vetor nomeado ---
df_syncomil <- df_syncomil %>%
  rename(all_of(new_columns_map_syncomiles)) # <--- Use all_of() para lidar com o vetor nomeado
df_miles <- df_miles %>%
  rename(all_of(new_columns_map_syncomiles)) # <--- Use all_of() para lidar com o vetor nomeado


# filters by log-age 
df_cbc_filtered <- df_cbc %>%
  filter(`log_age_yr` >= 7 & `log_age_yr` <= 10) %>%
  mutate(
    ug = u_SDSS - g_SDSS,
    ur = u_SDSS - r_SDSS,
    gr = g_SDSS - r_SDSS,
    gi = g_SDSS - i_SDSS,
    ri = r_SDSS - i_SDSS,
  ) %>%
  select(c(Z, log_age_yr, ug, ur, gr, gi, ri))
df_miles_filtered <- df_miles %>%
  filter(`log_age_yr` >= 7 & `log_age_yr` <= 10) %>%
  select(1:7) %>%
  mutate(
    ug = u_SDSS - g_SDSS,
    ur = u_SDSS - r_SDSS,
    gr = g_SDSS - r_SDSS,
    gi = g_SDSS - i_SDSS,
    ri = r_SDSS - i_SDSS,
  ) %>%
  select(c(Z, log_age_yr, ug, ur, gr, gi, ri))
df_syncomil_filtered <- df_syncomil %>%
  filter(`log_age_yr` >= 7 & `log_age_yr` <= 10) %>%
  select(1:7) %>%
  mutate(
    ug = u_SDSS - g_SDSS,
    ur = u_SDSS - r_SDSS,
    gr = g_SDSS - r_SDSS,
    gi = g_SDSS - i_SDSS,
    ri = r_SDSS - i_SDSS,
  ) %>%
  select(c(Z, log_age_yr, ug, ur, gr, gi, ri))

df_cbc_long <- pivot_longer(df_cbc_filtered,cols = -c('Z', 'log_age_yr'), names_to = 'color', values_to = 'color_value')
df_miles_long <- pivot_longer(df_miles_filtered, cols = -c('Z', 'log_age_yr'), names_to = 'color', values_to = 'color_value')
df_syncomil_long <- pivot_longer(df_syncomil_filtered, cols = -c('Z', 'log_age_yr'), names_to = 'color', values_to = 'color_value')

synthetic <- inner_join(df_miles_long, 
  df_syncomil_long,
  by = c('Z', 'log_age_yr', 'color'), 
  suffix = c('_Miles', '_Syncomil')
  ) %>%
  mutate(
    delta = color_value_Miles - color_value_Syncomil,
    question = 'Synthetic Effect'
  )

coverage <- inner_join(df_cbc_long, 
                        df_syncomil_long,
                        by = c('Z', 'log_age_yr', 'color'), 
                        suffix = c('_CBC', '_Syncomil')
) %>%
  mutate(
    delta = color_value_CBC - color_value_Syncomil,
    question = 'Coverage Effect'
  )

# Combine both comparisons
color_combined <- bind_rows(
  select(synthetic, log_age_yr, Z, color, delta, question),
  select(coverage, log_age_yr, Z, color, delta, question)
)

# Set factor levels for proper ordering
color_combined <- color_combined |>
  mutate(
    question = factor(question, levels = c("Synthetic Effect", "Coverage Effect")),
    color = factor(color, levels = c("ug", "ur", "gr", "gi", "ri")),
    Z = factor(Z, levels = sort(unique(Z)))
    )

color_labels <- c(
  ug = "u - g",
  ur = "u - r", 
  gr = "g - r",
  gi = "g - i",
  ri = "r - i"
)

# Create the density plot
fig4 <- ggplot(color_combined, aes(delta, fill = Z, color = Z)) + 
  # Create panels: colors as rows, questions as columns
  facet_grid(color ~ question, labeller = labeller(color = color_labels)) +
  # Set x-axis limits to match original figure
  coord_cartesian(xlim = c(-0.5, 0.28)) +
  # Add vertical line at zero difference
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed") +
  # Create density curves, one for each metallicity
  geom_density(aes(y = after_stat(scaled)), alpha = 0.3, position = "identity") + 
  # Set axis labels
  labs(
    x = expression(paste(Delta, " colour")), 
    y = "Scaled density",
    title = "Figure 4: Color Differences Between Stellar Population Models",
    fill = "Metallicity (Z)",
    color = "Metallicity (Z)"
  ) +
  # Apply clean theme
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    # --- ADICIONE OU VERIFIQUE ESTAS LINHAS ---
    plot.background = element_rect(fill = "white", colour = NA), # Fundo de todo o gráfico
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel de dados
    # ----------------------------------------
  )

# Save the plot
ggsave("C:/Users/dedet/Desktop/IC_astro/output/fig4_color_density.png", fig4, 
       width = 16, height = 12, units = "in", dpi = 300)


print(fig4)

fig5 <- ggplot(color_combined, aes(x = color, y = delta, fill = question)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = color_labels) +
  coord_cartesian(ylim = c(-0.3,0.3)) +
  labs(
    x = 'Color',
    y = expression(paste(Delta, 'color')),
    title = "Figure 5: Boxplot for Color Differences Between Stellar Population Models",
    fill = "",
    color = "Metallicity (Z)"
  ) +
  # Apply clean theme
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    # --- ADICIONE OU VERIFIQUE ESTAS LINHAS ---
    plot.background = element_rect(fill = "white", colour = NA), # Fundo de todo o gráfico
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel de dados
    # ----------------------------------------
  )


print(fig5)

# Save the plot
ggsave("C:/Users/dedet/Desktop/IC_astro/output/fig5_color_boxplot.png", fig5, 
       width = 16, height = 10, units = "in", dpi = 300)

fig6 <- ggplot(color_combined, aes(x = log_age_yr, y = delta, color = Z)) +
  facet_grid(color ~ question, labeller = labeller(color = color_labels)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.3,0.3)) +
  labs(
    x = 'log (age)', 
    y = expression(paste(Delta, 'color')),
    title = "Figure 6: Color Differences Between Stellar Population Models as a Function of Log Age ",
    color = "Metallicity (Z)"
  ) +
  # Apply clean theme
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    # --- ADICIONE OU VERIFIQUE ESTAS LINHAS ---
    plot.background = element_rect(fill = "white", colour = NA), # Fundo de todo o gráfico
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel de dados
    # ----------------------------------------
  )
  

print(fig6)

# Save the plot
ggsave("C:/Users/dedet/Desktop/IC_astro/output/fig6_color_differences.png", fig6, 
       width = 16, height = 10, units = "in", dpi = 300)
