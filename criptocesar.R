# Autor: Alfredo Marrucho
# Data: 2020-05-04
# Descrição: Substitui cada letra em uma frase pela sua correspondente em um alfabeto auxiliar

# Bibliotecas
library(httr)
library(jsonlite)
library(openssl)

# Variáveis
url_req <- "https://api.codenation.dev/v1/challenge/dev-ps/generate-data?token="
url_soluc <- "https://api.codenation.dev/v1/challenge/dev-ps/submit-solution?token="
token <- "SEU_TOKEN"
answer_json <- "answer.json"

# Aquisição do JSON com a frase cifrada
'
download.file(paste0(url, token), "teste.json")
conteudo <- fromJSON(json)
'
response <- GET(paste0(url_req, token))
conteudo <- content(response)

# Estabelece o alfabeto base
alphabet <- data.frame(base = letters, stringsAsFactors = FALSE)

# Cria uma nova coluna e desloca o alfabeto original de acordo com o numero de casas obtido do JSON
shift <- function(x, n){
  c(x[-(seq(n))], letters[(seq(n))])
}

alphabet$cipher <- shift(alphabet$base, conteudo$numero_casas)

# Decifra a mensagem cifrada com base na nova ordenação do alfabeto
conteudo$decifrado <- conteudo$cifrado

for (i in 1:nchar(conteudo$decifrado)) {
  
  if (substr(conteudo$decifrado, i, i)!=".") {
    index_num <- grep(substr(conteudo$decifrado, i, i), alphabet$cipher)
    
    if (length(index_num)>0)
      substr(conteudo$decifrado, i, i) <- alphabet$base[index_num]
  }
  
}

# Gera o resumo sha1 da mensagem decifrada
conteudo$resumo_criptografico <- as.character(sha1(conteudo$decifrado))

#conteudo$numero_casas <- as.numeric(conteudo$numero_casas)

# Cria o JSON com a mensagem decifrada e o resumo criptográfico
exportJSON <- toJSON(conteudo, auto_unbox = T)
write(exportJSON, answer_json)

# Submete o JSON com a resposta para a API
envio <- POST(paste0(url_soluc, token), body = list(answer = upload_file(answer_json, type = "application/json; charset=UTF-8")), add_headers("Content-Type" = "multipart/form-data"))

