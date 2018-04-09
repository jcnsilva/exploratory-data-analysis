#Limpeza de dados
library("ggplot2")
library("dplyr")
library("tidyr")
library("ggfortify")
library("cluster")
theme_bw()

dados = read.csv("dados/emendas_detalhes_parlamentar.csv", encoding="UTF-8")

dados_ver = dados %>% group_by(DESC_ORGAO_SUP) %>%
  summarise(total = sum(VL_REPASSE_PROP))

ggplot(data=dados_ver, aes(x=reorder(DESC_ORGAO_SUP, total), y=total)) + 
  geom_bar(stat="identity") + 
  labs(x="Ministério", y="Total (R$)", title="Investimentos por ministério") +
  coord_flip()

dados2 = dados %>% 
  filter(ANO_PROP %in% c("2015", "2016")) %>%
  distinct(ID_PROPOSTA, NOME_PARLAMENTAR, .keep_all = TRUE) %>%
  select(NOME_PARLAMENTAR, DESC_ORGAO_SUP, VL_REPASSE_PROP, UF_PROPONENTE) %>%
  filter(DESC_ORGAO_SUP %in% c("MINISTERIO DAS CIDADES", "MINISTERIO DA SAUDE", 
                               "MINISTÉRIO DA AGRICULTURA,PECUARIA E ABASTECIMENTO", 
                               "MINISTERIO DO TURISMO",
                               "MINISTERIO DO ESPORTE"
                               )) %>%
  filter(!is.na(UF_PROPONENTE)) %>%
  group_by(UF_PROPONENTE, DESC_ORGAO_SUP) %>%
  summarise(GASTOS_POR_AREA = sum(VL_REPASSE_PROP)) %>%
  mutate(DESC_ORGAO_SUP = gsub(" ", "_", DESC_ORGAO_SUP)) %>%
  mutate(DESC_ORGAO_SUP = gsub(",", "_", DESC_ORGAO_SUP)) %>%
  spread(DESC_ORGAO_SUP, GASTOS_POR_AREA, fill = 0) %>%
  ungroup()


dados_escalados = dados2  %>% 
  select(-UF_PROPONENTE) %>%
  mutate_each(funs(scale))

row.names(dados_escalados) = dados2$UF_PROPONENTE


# 3 centers
dists = dist(dados_escalados)
km = kmeans(dados_escalados, centers = 3, nstart = 10)
dados_escalados$kmcluster = km$cluster
plot(silhouette(dados_escalados$kmcluster, dists))

#4 centers
dists = dist(dados_escalados)
km = kmeans(dados_escalados, centers = 4, nstart = 10)
dados_escalados$kmcluster = km$cluster
plot(silhouette(dados_escalados$kmcluster, dists))

#5 centers
dists = dist(dados_escalados)
km = kmeans(dados_escalados, centers = 5, nstart = 10)
dados_escalados$kmcluster = km$cluster
plot(silhouette(dados_escalados$kmcluster, dists))

#Replot com 3 centros
dists = dist(dados_escalados)
km = kmeans(dados_escalados, centers = 3, nstart = 10)
dados_escalados$kmcluster = km$cluster
autoplot(km, data = dados_escalados)

dados_escalados$UF = row.names(dados_escalados)
row.names(dados_escalados) = NULL

ggplot(data=dados_escalados %>% gather(MINISTERIO, TOTAL, 1:5), aes(x=MINISTERIO, y=TOTAL)) +
  geom_boxplot(aes(fill=MINISTERIO)) +
  labs(y="TOTAL (Desvios Padrão)", x="", title="Distribuição dos gastos por cluster") +
  facet_wrap(~kmcluster)
