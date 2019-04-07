#load library
library(twitteR)
library(RYandexTranslate)
library(stringr)
library(syuzhet)

#load credentials
consumer_key <- 'xcpEz9MegA020gd4FqAUYvt6Q'
consumer_secret<- 'YstKojiQExVSmNcaXGz8DPMjagH6Ut9q7WBJlce6Opkxc16fAP'
access_token <- '159664009-7p4Z7a1iIoO4TfKFHQR2dSz8CMb7WXeOrr8J1guu'
access_secret <- 'saQ8fKEfYlgZATISiObyZ90qtsl3bTObQHb0xFeovYwOT'

#set up to authenticate
setup_twitter_oauth(consumer_key ,consumer_secret,access_token ,access_secret)

dtInicial = as.Date('2019-01-07')
dtFinal = as.Date('2019-02-10')

dfSentimento = as.data.frame(seq(from = dtInicial, to = dtFinal, by="week"))
colnames(dfSentimento) <- c("dtInicio")

dfSentimento$dtFim = dfSentimento$dtInicio + 6

rev(seq(dtInicial, by="day", length.out = dtFinal - dtInicial + 1))

df = c()
dfT = c()
for (dt in rev(as.character(seq(dtInicial, by="day", length.out = dtFinal - dtInicial + 1)))){
  tweets <- twitteR::searchTwitter("petr4", n=1, lang = 'pt',
                                   since = dt)
  df <- twListToDF(tweets)
  dfT = rbind(dfT, df)
}

df = dfT

#passo 1
tweets <- twitteR::searchTwitter("petr4", n=1000000, lang = 'pt')
df <- twListToDF(tweets)
dfT = rbind(dfT, df)

# min(df$created);max(df$created)
#[1] "2019-02-10 13:22:01 UTC"
#[1] "2019-02-20 02:01:37 UTC"
# min(df$created);min(dfT$created)

df$text = gsub("http.*","",df$text)
df$text = gsub("https.*","",df$text)
df$text = gsub("#.*","",df$text)
df$text = gsub("@.*","",df$text)
df$text = str_replace_all(df$text, "
                          ", " ")
df$text = str_replace_all(df$text, "\n", " ")
df$text = str_replace_all(df$text, "[[:punct:]]", " ")

df = df[!df$text == "",]
df = df[!duplicated(df$text),]

api_key="trnsl.1.1.20190212T233847Z.fdbe2bba950bc3c2.3ccfa72aba8623326262b9593b6f9a13ed22381a"

translate = function (api_key, text = "", lang = "") 
{
  url = "https://translate.yandex.net/api/v1.5/tr.json/translate?"
  url = paste(url, "key=", api_key, sep = "")
  if (text != "") {
    url = paste(url, "&text=", text, sep = "")
  }
  if (lang != "") {
    url = paste(url, "&lang=", lang, sep = "")
  }
  url = gsub(pattern = " ", replacement = "%20", x = url)
  d = RCurl::getURL(url, ssl.verifyhost = 0L, ssl.verifypeer = 0L)
  d = jsonlite::fromJSON(d)
  d$code = NULL
  d
}

fazTraducao = function(texto){
  print(texto)
  textoTraduzido = translate(api_key, text = texto, 
                             lang="pt-en")$text
  #print(textoTraduzido)
  if (is.null(textoTraduzido)){textoTraduzido=""}
  return (textoTraduzido);
}

df$texto = sapply(df$text, fazTraducao)

getwd()
write.csv2(df, "twittesPETR42.csv")
df = read.csv2("twittesPETR4.csv")

dtInicial = as.Date(min(as.Date(df$created)))
dtFinal = as.Date(max(as.Date(df$created)))

dfSentimento = as.data.frame(seq(from = dtInicial, to = dtFinal, by="day"))
colnames(dfSentimento) <- c("dt")

calculaSentimento = function(dt){
  
  dfSent = df[as.Date(df$created) == dt,]
  
  word.df <- as.vector(dfSent$texto)
  
  sent.value <- get_sentiment(word.df)
  
  category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
  
  return (category_senti)
}

# sentimentos = mapply(calculaSentimento, dfSentimento$dtInicio, dfSentimento$dtFim)
sentimentos = sapply(dfSentimento$dt, calculaSentimento)

sumarizaSentimento = function (lsSent, texto){
  return(length(lsSent[lsSent == texto]))
}

dfSentimento$Positivo = sapply(sentimentos, sumarizaSentimento, "Positive")
dfSentimento$Negativo = sapply(sentimentos, sumarizaSentimento, "Negative")

# install.packages("quantmod")
library(quantmod)

pbr <- getSymbols("PETR4.SA", src = "yahoo", from = dtInicial, to = dtFinal + 1, auto.assign = FALSE)

pbr = as.data.frame(pbr)
pbr$dt = as.Date(row.names(as.data.frame(pbr)))

dfSentimento = merge(dfSentimento, pbr, by = "dt", all = TRUE )

getAberturaPosterior = function (dt){
  dfSentimento[
    dfSentimento$dt == min(dfSentimento[
      #dfSentimento$dt > dt & !is.na(dfSentimento$PBR.Open), c("dt")]), c("PBR.Open")]
      dfSentimento$dt > dt & !is.na(dfSentimento[4]), c("dt")]), c(4)]
}

dfSentimento$aberturaPosterior = as.numeric(sapply(dfSentimento$dt, getAberturaPosterior))

dfSentimento$percDif = (((dfSentimento$aberturaPosterior - dfSentimento[,4]) / 
                                                 dfSentimento[,4]) *
                                                100)

dfSentDif = as.data.frame(dfSentimento[c(1,2,3,11)])

colnames(dfSentDif) = c("Data", "Positivo", "Negativo", "Diferenca")

write.csv2(dfSentDif, "analisePETR.2.csv")
