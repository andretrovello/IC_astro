#authenticate drive
library(stringr)
library(tidyverse)
library(googledrive)
library(patchwork)
library(cowplot)

install.packages('cowplot')

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



file_list_miles <- download_df(loc_miles, download = FALSE)
file_list_syncomil <- download_df(loc_syncomil, download = FALSE)


df_miles <- create_df(file_list_miles)
df_syncomil <- create_df(file_list_syncomil)


print(name(df_syncomil))
      
df_miles_filtered <- df_miles %>%
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
      

dados_plot <- data.frame(
  CN1_Miles = df_miles_filtered$`CN_1 (mag)`,
  CN1_Syncomil = df_syncomil_filtered$`CN_1 (mag)`,
  Log_Age = df_miles_filtered$`log-age (yr)` # Assumindo que log-age vem de miles
)
# --------------------------------------------------------------------------

ggplot(data = dados_plot, aes(x = CN1_Syncomil, y = CN1_Miles, color = Log_Age)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", size = 0.8) +
  labs(title = "CN_1 (mag)",
       x = "SPS-S",
       y = "SPS-M",
       color = "log-age (yr)") + # Rótulo da legenda de cores
  theme_classic() +
  # --- ADICIONAR ESCALA DE CORES PARA PERSONALIZAR ---
  scale_color_viridis_c(option = "plasma", direction = -1)


print(df_miles_filtered['CN_1 (mag)'])
names(df_miles_filtered[1])


# Crie uma lista VAZIA para armazenar os objetos ggplot
all_plots <- list()
plot_index <- 1 # Para controlar o índice da lista

for (name in names(df_miles_filtered)[3:length(names(df_miles_filtered))]) {
  dados_plot <- data.frame(
  Miles = df_miles_filtered[[name]],
  Syncomil = df_syncomil_filtered[[name]],
  Log_Age = df_miles_filtered[["log-age (yr)"]] # Assumindo que log-age vem de miles
  )
  
  plot <- ggplot(data = dados_plot, aes(x = Syncomil, y = Miles, color = Log_Age)) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue", size = 0.8) +
    labs(title = name,
         x = NULL,
         y = NULL,
         color = "log-age (yr)") + # Rótulo da legenda de cores
    theme_classic() +
    # --- ADICIONAR ESCALA DE CORES PARA PERSONALIZAR ---
    scale_color_viridis_c(option = "plasma", direction = -1)
  
  all_plots[[plot_index]] <- plot
  plot_index = plot_index+1
}


# A figura combinada final
final_figure <- wrap_plots(all_plots, ncol = 6, nrow = 6) +
  plot_layout(
    guides = "collect", # <--- ESSENCIAL: Coleta todas as legendas idênticas em uma só
  ) +
  # --- ADICIONA RÓTULOS DE EIXO GLOBAIS E ESTILO GERAL À FIGURA COMBINADA ---
  plot_annotation(
    title = 'Comparação de SPS-Syncomil vs. SPS-Miles', # Título geral da figura
    tag_levels = 'A', # Adiciona rótulos A, B, C, etc. aos subplots
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.tag = element_text(face = 'bold', size = 12, margin = margin(t = 5, l = 5)),
      # Ajusta margens para toda a figura para dar espaço aos rótulos globais
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  ) &
  # <--- ESSENCIAL: Aplica rótulos de eixo para toda a figura combinada ---
  # O operador '&' aplica estas camadas a todos os subplots e, com 'labs()',
  # patchwork os interpreta como rótulos globais.
  labs(x = "SPS-Syncomil", y = "SPS-Miles") &
  # --- POSICIONAMENTO E ESTILO DA LEGENDA COLETADA ---
  # Este 'theme()' também é aplicado globalmente.
  theme(
    legend.position = "right", # Posição da legenda coletada (pode ser "bottom", "right", etc.)
    legend.title = element_text(size = 10, face = "bold"), # Estilo do título da legenda
    legend.text = element_text(size = 9), # Estilo do texto da legenda
    legend.box.margin = margin(0, 0, 0, 15, unit = "pt") # Margem da caixa da legenda
  )

final_fig <- ggdraw(final_fig) +
  draw_label("SPS-S", x = 0.5, y = 0.03, vjust = 0, hjust = 0.5, size = 12, fontface = "bold") +
  draw_label("SPS-M", x = 0.03, y = 0.5, vjust = 0.5, hjust = 0, angle = 90, size = 12, fontface = "bold")

# --- Salve a figura com DIMENSÕES MAIORES ---
# Se ainda estiver espremido, AUMENTE AINDA MAIS esses valores!
ggsave(
  filename = "todos_os_plots_combinados.png",
  plot = final_fig,
  width = 20,  # <--- LARGURA AUMENTADA (de 25 para 30)
  height = 30, # <--- ALTURA AUMENTADA (de 15 para 20)
  units = "in",
  dpi = 300
)

# Boxplot

# --- 1. Identificar Colunas Comuns e Numéricas para Subtração ---
# Encontre os nomes das colunas comuns (excluindo a coluna chave 'ID' se não for numérica)

join_keys <- c('z', 'log-age (yr)')
common_cols <- intersect(names(df_miles_filtered), names(df_syncomil_filtered))
common_cols <- setdiff(common_cols, join_keys) # Remove a coluna ID das comuns a serem subtraídas

# Filtra apenas as colunas que são numéricas e comuns
numeric_common_cols <- common_cols[sapply(df_miles[common_cols], is.numeric) &
                                     sapply(df_syncomil[common_cols], is.numeric)]


df_filtered_combined <- inner_join(df_miles_filtered, df_syncomil_filtered, by = join_keys, suffix = c('_Miles', '_Syncomil'))

# --- 3. Criar o Novo Dataframe com as Subtrações (CORRIGIDO) ---
df_delta_idx <- df_filtered_combined %>%
  # Não é necessário usar select aqui para pré-filtrar as colunas para o 'across'.
  # 'across' pode operar em um subconjunto de colunas diretamente.
  mutate(
    across(
      # Aplica a operação às colunas que são numéricas e têm o sufixo '_Miles'
      # e que correspondem às suas colunas numéricas comuns antes do sufixo.
      # Usamos 'paste0' para construir os nomes esperados das colunas '_Miles'.
      all_of(paste0(numeric_common_cols, "_Miles")), # <--- CORRIGIDO AQUI
      ~ .x - df_filtered_combined[[str_replace(cur_column(), "_Miles", "_Syncomil")]], # Subtrai
      .names = "{.col}_Diff" # Cria novas colunas com sufixo _Diff
    )
  ) %>%
  # Renomeia as colunas de diferença, removendo o sufixo "_Miles" extra
  rename_with(~ str_replace(., "_Miles_Diff", "_Diff"), ends_with("_Miles_Diff")) %>% # <--- CORRIGIDO
  # Seleciona as colunas chave (join_keys) e as novas colunas de diferença
  select(all_of(join_keys), ends_with("_Diff")) # <--- CORRIGIDO: usa join_keys, não ID



# --- 1. Identificar as colunas que precisam ser transformadas ---
cols_to_transform <- names(df_delta_idx %>% select(ends_with("_Diff")))

# --- 2. Aplicar a transformação usando mutate(across()) e adicionar ao DF EXISTENTE ---
df_delta_idx <- df_delta_idx %>% # Atribui o resultado de volta ao mesmo dataframe
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

# Agora, 'df_delta_idx' conterá as colunas originais '_Diff' E as novas '_ZScore'
print(head(df_delta_idx))

# Para verificar as novas colunas
print(names(df_delta_idx))


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
    y = "Valor Delta (Miles - Syncomil)" # <--- Rótulo Y ajustado
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotaciona os rótulos X
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA), # Fundo geral do plot (fora do painel)
    panel.background = element_rect(fill = "white", colour = NA) # Fundo do painel onde os dados são plotados
  
  )


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