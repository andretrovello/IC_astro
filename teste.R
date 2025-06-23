library(googledrive)

# Autenticação (se ainda não feita)
drive_auth()

# ID da pasta pai (substitua pelo seu ID)
id_pasta_pai <- "16CeeWb0m33latQL_sOCcLVPPv7Zn2p3X" 

# Listar subpastas
subpastas <- drive_find(
  q = paste0("'", id_pasta_pai, "' in parents and mimeType = 'application/vnd.google-apps.folder'")
)

# Extrair nomes e IDs das subpastas
lista_subpastas <- subpastas[, c("name", "id")]
print(lista_subpastas)

# Lista para armazenar os arquivos de cada subpasta
lista_arquivos_por_subpasta <- list()

for (i in 1:nrow(lista_subpastas)) {
  nome_subpasta <- lista_subpastas$name[i]
  id_subpasta <- lista_subpastas$id[i]
  
  # Listar arquivos dentro da subpasta (excluindo outras pastas)
  arquivos <- drive_find(
    q = paste0("'", id_subpasta, "' in parents and mimeType != 'application/vnd.google-apps.folder'")
  )
  
  # Armazenar no formato: nome_da_subpasta = lista_de_arquivos
  lista_arquivos_por_subpasta[[nome_subpasta]] <- arquivos$name
}

