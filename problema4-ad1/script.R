library("ggplot2")
library("dplyr")
library("resample")

dados_filmes = read.csv("dados/movies.csv", encoding = "UTF-8")
dados_avaliacoes = read.csv("dados/ratings.csv", encoding = "UTF-8")

dados_q1 = dados_filmes %>% 
  left_join(dados_avaliacoes, by = "movieId") %>%
  select(-c(genre, timestamp)) %>%
  distinct() %>%
  filter(movieId %in% c(4993, 5952, 7153)) %>%
  droplevels()

dados_q1_agr = dados_q1 %>%
  group_by(title) %>%
  summarize(media = mean(rating),
            total = n())

dados_q1_sum = dados_q1 %>%
  group_by(title, rating) %>%
  summarize(freq = n()) %>%
  left_join(dados_q1_agr, by="title") %>%
  ungroup()
  

ggplot(dados_q1_sum, aes(x=rating, y=freq/total)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept=media)) +
  facet_grid(~ title)
