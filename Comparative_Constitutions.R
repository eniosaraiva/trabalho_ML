# Carregar o pacote "haven"
library(haven)

# Carregar o arquivo do Stata
dados <- read_dta("ccpcnc_v4.dta")
save(dados, file = "dados.rdata")
#

#
dados_selecionados <- dados[, 2:3]
dados_selecionados <- cbind(dados_selecionados, dados[, 6])
dados_selecionados <- cbind(dados_selecionados, dados[, 8])
dados_selecionados <- cbind(dados_selecionados, dados[, 9])
dados_selecionados <- cbind(dados_selecionados, dados[, 11])
dados_selecionados <- cbind(dados_selecionados, dados[, 12])
dados_selecionados <- cbind(dados_selecionados, dados[, 35])
dados_selecionados <- cbind(dados_selecionados, dados[, 83])
# Remover linhas com valores ausentes de dados_selecionados usando complete.cases()
dados_selecionados <- dados_selecionados[complete.cases(dados_selecionados), ]
dados_selecionados <- na.omit(dados_selecionados)
# Remover linhas com valores ausentes ou vazias em qualquer coluna de dados_selecionados
dados_selecionados <- dados_selecionados[apply(dados_selecionados, 1, function(row) !any(is.na(row) | row == "")), ]
# Remover linhas em que a coluna "eventtype" seja igual a 4
dados_selecionados <- dados_selecionados[dados_selecionados$evnttype != 4, ]
dados_selecionados <- dados_selecionados[, -6]


worldcities <- read_csv("worldcities.csv")
worldcities <- worldcities %>%
  filter(capital == "primary")

dados_selecionados <- merge(dados_selecionados, worldcities, by = "country", all.x = TRUE)
dados_selecionados <- dados_selecionados[, -c(9, 10)]
dados_selecionados <- dados_selecionados[, -c(11, 12, 13, 14, 15, 16)]
rm(worldcities)
dados_selecionados$country <- as.factor(dados_selecionados$country)
dados_selecionados <- dados_selecionados %>%
  arrange(desc(year)) %>%
  distinct(country, .keep_all = TRUE)
save(dados_selecionados, file = "dados_selecionados.rdata")
dados_selecionados2 <- select(dados_selecionados, 1, 4, 7, 8, 9, 10)
dados_selecionados2$length <- as.numeric(dados_selecionados2$length)
dados_selecionados2$demonum <- as.numeric(dados_selecionados2$demonum)
save(dados_selecionados2, file = "dados_selecionados2.rdata")
str(sd) 

# Saída de regressão
regression_model <- lm(demonum ~ length, data = dados_selecionados2)
summary(regression_model)

# Saída de clusterização
cluster_data <- dados_selecionados2 %>% select(demonum)

# Determinar a melhor quantidade de clusters usando o método Elbow
sse <- c()
for (k in 1:10) {
  kmeans_result <- kmeans(cluster_data, centers = k)
  sse[k] <- kmeans_result$tot.withinss
}

# Plotar o gráfico de SSE em relação ao número de clusters
plot(1:10, sse, type = "b", pch = 19, frame.plot = FALSE,
     xlab = "Número de Clusters", ylab = "Soma dos Erros Quadrados (SSE)")

kmeans_result <- kmeans(cluster_data, centers = 4)
clustered_data <- cluster_data %>% mutate(cluster = as.factor(kmeans_result$cluster))

ggplot(clustered_data, aes(x = demonum, fill = cluster)) +
  geom_density(alpha = 0.5) +
  labs(x = "Quantidade de menções a democracia", y = "Densidade", title = "Clusterização da quantidade de menção a democracia")

regression_model <- glm(demonum ~ length, data = dados_selecionados2, family = gaussian(link = "identity"))

# Gráfico de dispersão com linha de regressão
ggplot(dados_selecionados2, aes(x = length, y = demonum)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = "identity")), se = FALSE, color = "red") +
  labs(x = "Tamanho da Constituição", y = "Menções a Democracia", title = "Regressão GLM") +
  theme_minimal()

str(dados_selecionados2)  # Verificar a estrutura dos dados

dados_selecionados2 <- na.omit(dados_selecionados2)  # Remover valores ausentes, se necessário
