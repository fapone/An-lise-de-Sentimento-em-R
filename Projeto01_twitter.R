# Análise de sentimento

# Analise de sentimento quanto a candidatura do Bolsonaro
# Este projeto tem como objetivo, coletar dados no twitter utilizando web scraping e fazer a análise de sentimento
# quanto a candidatura (positiva) de sua condidatura para 2018

# Instalação dos pacotes necessários para o projeto
install.packages("twitteR")
install.packages("plyr")
install.packages("stringr")
install.packages("tm")
install.packages("readr")
install.packages("dplyr")
install.packages("Rstem")
install.packages("ggplot2")

# Utilizando as bibliotecas necessárias 
library("twitteR")
library("plyr")
library("stringr")
library("tm")
library("readr")
library("dplyr")
library("Rstem")
library("ggplot2")


# Passo 1 - Obter conexão com twitter 

# Chaves de acesso ao API developer do twitter
consumer_key = ""
consumer_secret = ""
access_token = ""
access_secret = ""

# Função de autenticação de usuários na conta do twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


# Passo 2 - Coletar dados para análise e criar dataframe

# Funcao que faz os tratamentos para coleta, limpeza e alteração do Dados do Twitter
presidenciavel <- function(presidente){

  listtwr <-  searchTwitter(presidente, n=500)  
  
  # Passo 3 - Fazer a limpeza dos dados coletados
  
  # Fazendo fazendo limpeza dos dados
  listtwr = sapply(listtwr, function(x) x$getText())
  listtwr <- iconv(listtwr, to = "utf-8", sub="")
  
  # Transforma o dado coletado em Corpus
  auxCorpus = Corpus(VectorSource(listtwr))
  auxCorpus <- tm_map(auxCorpus, removePunctuation)
  auxCorpus <- tm_map(auxCorpus, function(x)removeWords(x, stopwords()))
  auxCorpus <- tm_map(auxCorpus, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
  auxCorpus <- tm_map(auxCorpus, content_transformer(tolower))
  
  return(auxCorpus)
  
}


# Função para calcular analise de sentimento
sentiments <- function(termo, texto_positivo, texto_negativo){
  termo = gsub("[[:punct:]]", "", termo)
  termo = gsub("[[:cntrl:]]", "", termo)
  termo = gsub('\\d+', '', termo)
  termo = sapply(termo, tolower)
  
  
  # Este trecho foi copiado do projeto01 da DSA - Não entendi este trecho do código
  word.list = str_split(termo, "\\s+")
  termo = unlist(word.list)
  
  positive = match(termo, texto_positivo)
  negative = match(termo, texto_negativo)
  
  positive = !is.na(positive)
  negative = !is.na(negative)
  
  score = sum(positive) - sum(negative)
  
  return(score)
}

# Passo 4 - Fazer a analise de sentimentos dos candidados a presidencia
# Criando variavel a partir do arquivo de palavras Positivas  
wpositiva = readLines("palavras_positivas.txt")
wnegativa = readLines("palavras_negativas.txt")

# Chama funcao que traz o score da analise de sentimento
pesqp1 = "Bolsonaro"
pesqp2 = "Lula"

twtext1 <- gettext(presidenciavel(pesqp1))
twtext2 <- gettext(presidenciavel(pesqp2))

presidente1 = sentiments(twtext1, wpositiva, wnegativa)
presidente2 = sentiments(twtext2, wpositiva, wnegativa)

dfpresidente <- data_frame(Score = c(presidente1, presidente2), Candidato = c(pesqp1, pesqp2))

# Passo 5 - Plotar no grafico o Score de cada candidato
ggplot(dfpresidente) + 
  geom_col(aes(y = Score, x = Candidato, fill = Score)) +
  theme_bw()



