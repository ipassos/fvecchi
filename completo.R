library(tidyverse)

library(purrr)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(plotly)
library(fs)
#cria vetor de nomes
file_path <- dir_ls("C:/Users/iarac/Documents/MEGA/R/fvecchi/extract")

#cria lista vazia
list_csv <- list()
completo <- data.frame()

for(i in seq_along(file_path)){
  list_csv[[i]] <- read.csv(file = file_path[[i]], 
                            stringsAsFactors = FALSE, encoding="UTF-8")
  completo <- rbind(completo, list_csv[[i]])
}

list_names <- vector()
for(i in seq_along(file_path)){
  list_names[i] <- str_extract(string = file_path[[i]], pattern = "([^/]*)$")
}

list_csv <- set_names(list_csv, list_names)

length(list_csv)


######################

completo$date <- as.Date(completo$date)



completo_selec <- completo %>% 
  select(id, date, time, user_id, username, name, tweet, 
         mentions, replies_count, retweets_count, likes_count, link, retweet) %>% 
  distinct(tweet, .keep_all = T)

completo_selec %>% 
  ggplot2::ggplot(aes(name)) + geom_bar()

completo_selec %>%
  group_by(username, date) %>% 
  count() %>% 
  ggplot( aes(x=date, y=n, group= username)) +
  geom_line(aes(color=username) )

by_date <- completo_selec %>%
  group_by(date) %>% 
  count()
  
plot_ly(x = by_date$date, y = by_date$n) %>%
  add_lines(alpha = 0.2, hoverinfo = "none") %>% 
layout(showlegend = F, xaxis = list(range = c('2019-01-01','2020-12-31'))) %>% 
  layout(xaxis = list(title = 'Date',color = '#364E6F', dtick = "M1"))


mynames = levels(as.factor(completo_selec$username))
myindex = c(1:21)
p = list()
cont = 1

for (i in myindex) {
  frame <- completo_selec %>% 
    group_by(username, date) %>% 
    count() %>% 
    filter(username == mynames[i]) 
  p[[cont]] <-
    plot_ly(x =frame$date, y = frame$n, 
            type = "scatter", mode = 'lines',  line = list(width = 1)) %>% 
    layout(title = mynames[[cont]],
           xaxis = list(title = 'Data',
                        zeroline = TRUE),
           yaxis = list(title = mynames[[cont]]))
  cont = cont + 1
}



completo_selec$tweet2 <- gsub("https\\S*", "", completo_selec$tweet) 
completo_selec$tweet2 <- gsub("@\\S*", "", completo_selec$tweet2) 
completo_selec$tweet2 <- gsub("amp", "", completo_selec$tweet2) 
completo_selec$tweet2 <- gsub("[\r\n]", "", completo_selec$tweet2)
completo_selec$tweet2 <- gsub("[[:punct:]]", "", completo_selec$tweet2)
completo_selec$tweet2 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  completo_selec$tweet2)
completo_selec$tweet2 <- str_replace(completo_selec$tweet2,"RT @[a-z,A-Z]*: ","")
completo_selec$tweet2 <- gsub("@\\w+", "", completo_selec$tweet2)
completo_selec$tweet2 <- str_replace_all(completo_selec$tweet2,"@[a-z,A-Z]*","")  
completo_selec$tweet2 <- gsub("[^[:alnum:][:blank:]]", " ", completo_selec$tweet2)
completo_selec$tweet2 <- gsub("[[:digit:]]", " ", completo_selec$tweet2)
completo_selec$tweet2 <- tolower(completo_selec$tweet2)



######################

write.csv2(completo, "tweets_2019-2020.csv")

############# Analise

arma <- completo_selec %>% 
   filter(
    str_detect(tweet2, "\\S+\\s+arma") & !str_detect(tweet2, "\\S+\\s+armadas") & !str_detect(tweet2, "\\S+\\s+armaz")
    & !str_detect(tweet2, "\\S+\\s+armaç") & !str_detect(tweet2, "\\S+\\s+biologica")
    )%>% 
  mutate(Key = "Arma")

arma %>% 
  group_by(username, date) %>% 
plot_ly(x = ~date, y = ~n) %>%
  add_lines(alpha = 0.2, hoverinfo = "none") %>% 
  layout(showlegend = F, xaxis = list(range = c('2019-01-01','2020-12-31'))) %>% 
  layout(xaxis = list(title = 'Date',color = '#364E6F', dtick = "M1"))

arma %>% 
  ggplot2::ggplot(aes(name)) + geom_bar()

bala <- completo_selec %>% 
   filter(
    str_detect(tweet2, "\\S+\\s+bala") & !str_detect(tweet2, "\\S+\\s+balai") & 
      !str_detect(tweet2, "\\S+\\s+balan")& !str_detect(tweet2, "\\S+\\s+balab")
  )%>% 
  mutate(Key = "Bala")

cidadao_bem <- completo_selec %>% 
  filter(
    str_detect(tweet2, "\\S+\\s+cidad.*") & str_detect(tweet2, "\\S+\\s+bem")
  )%>% 
  mutate(Key = "Cidadão de bem")




defesa <- completo_selec %>% 
  filter(
    str_detect(tweet2, "\\S+\\s+defes") 
  )%>% 
  mutate(Key = "Defesa")

esquerda<- completo_selec %>% 
  filter(
    str_detect(tweet2, "\\S+\\s+esquerd") 
  )%>% 
  mutate(Key = "Esquerda")

estados_unidos <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+estad") & str_detect(tweet2, "\\S+\\s+unidos")) 
    | str_detect(tweet2, "\\S+\\s+eua")
  ) %>% 
  mutate(Key = "EUA")

familia <- completo_selec %>% 
  filter(
    str_detect(tweet2, "\\S+\\s+famil") |     str_detect(tweet2, "\\S+\\s+famíl")
  )%>% 
  mutate(Key = "Família")

feminismo <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+femin")) 
  )%>% 
  mutate(Key = "Feminismo")

ideologia <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+ideol"))  ) %>% 
  mutate(Key = "Ideologia")

impunidade <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+impun")))%>% 
  mutate(Key = "Impunidade")

leg_def <-  completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+leg") & str_detect(tweet2, "defesa")))%>% 
  mutate(Key = "Legítima Defesa")
  
liberdade <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+liberda") ))%>% 
  mutate(Key = "Liberdade")

municao <- completo_selec %>% 
   filter(
    str_detect(tweet2, "\\S+\\s+munição") |  str_detect(tweet2, "\\S+\\s+munições"))%>% 
  mutate(Key = "Munição")

municao_tokens <- get_tokens(municao$tweet_2)
municao_df <- get_nrc_sentiment(municao_tokens, lang="portuguese")

seguranca <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+seguran") ))%>% 
  mutate(Key = "Segurança")

seguranca_tokens <- get_tokens(seguranca$tweet_2)
seguranca_df <- get_nrc_sentiment(seguranca_tokens, lang="portuguese") 

barplot(
  colSums(prop.table(seguranca_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Segurança",
  xlab="emoções", ylab = NULL, ylim = c(0,0.5))

terrorismo <- completo_selec %>% 
  filter(
    (str_detect(tweet2, "\\S+\\s+terror") )) %>% 
  mutate(Key = "Terrorismo")

terrorismo_tokens <- get_tokens(terrorismo$tweet_2)
terrorismo_df <- get_nrc_sentiment(terrorismo_tokens, lang="portuguese") 

barplot(
  colSums(prop.table(terrorismo_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Terrorismo",
  xlab="emoções", ylab = NULL, ylim = c(0,0.5))

junto <- rbind(arma, bala, cidadao_bem, comunismo, crime, defesa, esquerda, estados_unidos, familia, feminismo, ideologia, 
               impunidade, leg_def, liberdade, municao, seguranca, terrorismo )

library(ggplot2)
junto %>% 
  ggplot2::ggplot(aes(Key, group = username)) + geom_bar(aes(colour = username))

#
municao_tokens <- get_tokens(municao$tweet_2)
municao_df <- get_nrc_sentiment(municao_tokens, lang="portuguese")

seguranca_tokens <- get_tokens(seguranca$tweet_2)
seguranca_df <- get_nrc_sentiment(seguranca_tokens, lang="portuguese") 

barplot(
  colSums(prop.table(seguranca_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Segurança",
  xlab="emoções", ylab = NULL, ylim = c(0,0.5))



terrorismo_tokens <- get_tokens(terrorismo$tweet_2)
terrorismo_df <- get_nrc_sentiment(terrorismo_tokens, lang="portuguese") 

barplot(
  colSums(prop.table(terrorismo_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Terrorismo",
  xlab="emoções", ylab = NULL, ylim = c(0,0.5))

  