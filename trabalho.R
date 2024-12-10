install.packages(tidyverse)
install.packages("patchwork")
install.packages("gridExtra")
install.packages("ggplot2")

library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)

#Primeiro vamos fazer as médias dos bairros de cada ano das notas do IDEB
#Médias de 2017
ideb_2017 = na.omit(ideb_2017)
notas_2017 = ideb_2017%>%dplyr::group_by(Bairro)%>%
  summarise(Média_Bairro_2017 = mean(IDEB_2017))

#Médias de 2019
ideb_2019 = na.omit(ideb_2019)
notas_2019 = ideb_2019%>%dplyr::group_by(Bairro)%>%
  summarise(Média_Bairro_2019 = mean(IDEB_2019))

#Médias de 2021
ideb_2021 = na.omit(ideb_2021)
notas_2021 = ideb_2021%>%dplyr::group_by(Bairro)%>%
  summarise(Média_Bairro_2021 = mean(IDEB_2021))

#Médias de 2023
ideb_2023 = na.omit(ideb_2023)
notas_2023 = ideb_2023%>%dplyr::group_by(Bairro)%>%
  summarise(Média_Bairro_2023 = mean(IDEB_2023))


#Agora faremos os Boxplots
p1 <- ggplot(notas_2017, aes(y = Média_Bairro_2017)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2017") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

p2 <- ggplot(notas_2019, aes(y = Média_Bairro_2019)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2019") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

p3 <- ggplot(notas_2021, aes(y = Média_Bairro_2021)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2021") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

p4 <- ggplot(notas_2023, aes(y = Média_Bairro_2023)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2023") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(),# Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

# Combine os gráficos em uma grade
(p1 | p2 | p3 | p4) +
  plot_annotation(title = "Distribuição Geral das Médias do IDEB por Bairro", 
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))

#Dados por Ano

#Dados 2017
# Estatísticas básicas
Q1_2017 <- quantile(notas_2017$Média_Bairro_2017, 0.25) # Primeiro quartil
Q3_2017 <- quantile(notas_2017$Média_Bairro_2017, 0.75) # Terceiro quartil
IQR_2017 <- Q3_2017 - Q1_2017             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2017 <- Q1_2017 - 1.5 * IQR_2017
limite_superior_2017 <- Q3_2017 + 1.5 * IQR_2017

# Identificando outliers
outliers_2017 <- notas_2017$Média_Bairro_2017[notas_2017$Média_Bairro_2017 < limite_inferior_2017 | notas_2017$Média_Bairro_2017 > limite_superior_2017]


#Dados 2019
# Estatísticas básicas
Q1_2019 <- quantile(notas_2019$Média_Bairro_2019, 0.25) # Primeiro quartil
Q3_2019 <- quantile(notas_2019$Média_Bairro_2019, 0.75) # Terceiro quartil
IQR_2019 <- Q3_2019 - Q1_2019             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2019 <- Q1_2019 - 1.5 * IQR_2019
limite_superior_2019 <- Q3_2019 + 1.5 * IQR_2019

# Identificando outliers
outliers_2019 <- notas_2019$Média_Bairro_2019[notas_2019$Média_Bairro_2019 < limite_inferior_2019 | notas_2019$Média_Bairro_2019 > limite_superior_2019]
 
#Dados 2021
# Estatísticas básicas
Q1_2021 <- quantile(notas_2021$Média_Bairro_2021, 0.25) # Primeiro quartil
Q3_2021 <- quantile(notas_2021$Média_Bairro_2021, 0.75) # Terceiro quartil
IQR_2021 <- Q3_2021 - Q1_2021            # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2021 <- Q1_2021 - 1.5 * IQR_2021
limite_superior_2021 <- Q3_2021 + 1.5 * IQR_2021

# Identificando outliers
outliers_2021 <- notas_2021$Média_Bairro_2021[notas_2021$Média_Bairro_2021 < limite_inferior_2021 | notas_2021$Média_Bairro_2021 > limite_superior_2021]

#Dados 2023
# Estatísticas básicas
Q1_2023 <- quantile(notas_2023$Média_Bairro_2023, 0.25) # Primeiro quartil
Q3_2023 <- quantile(notas_2023$Média_Bairro_2023, 0.75) # Terceiro quartil
IQR_2023 <- Q3_2023 - Q1_2023             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2023 <- Q1_2023 - 1.5 * IQR_2023
limite_superior_2023 <- Q3_2023 + 1.5 * IQR_2023

# Identificando outliers
outliers_2023 <- notas_2023$Média_Bairro_2023[notas_2023$Média_Bairro_2023 < limite_inferior_2023 | notas_2023$Média_Bairro_2023 > limite_superior_2023]

#Pegando os outliers
outliers_2017
outliers_2019
outliers_2021
outliers_2023

#Descobrindo os outliers

#2017
resultado_2017 = subset(notas_2017, notas_2017$Média_Bairro_2017 > limite_superior_2017 | notas_2017$Média_Bairro_2017 < limite_inferior_2017)
resultado_escolas_2017 = subset(ideb_2017, ideb_2017$IDEB_2017 > limite_superior_2017 | ideb_2017$IDEB_2017 < limite_inferior_2017)

#2019
resultado_2019 = subset(notas_2019, notas_2019$Média_Bairro_2019 > limite_superior_2019 | notas_2019$Média_Bairro_2019 < limite_inferior_2019)
resultado_escolas_2019 = subset(ideb_2019, ideb_2019$IDEB_2019 > limite_superior_2019 | ideb_2019$IDEB_2019 < limite_inferior_2019)

#2021
resultado_2021 = subset(notas_2021, notas_2021$Média_Bairro_2021 > limite_superior_2021 | notas_2021$Média_Bairro_2021 < limite_inferior_2021)
resultado_escolas_2021 = subset(ideb_2021, ideb_2021$IDEB_2021 > limite_superior_2021 | ideb_2021$IDEB_2021 < limite_inferior_2021)

#2023
resultado_2023 = subset(notas_2023, notas_2023$Média_Bairro_2023 > limite_superior_2023 | notas_2023$Média_Bairro_2023 < limite_inferior_2023)
resultado_escolas_2023 = subset(ideb_2023, ideb_2023$IDEB_2023 > limite_superior_2023 | ideb_2023$IDEB_2023 < limite_inferior_2023)

#Médias do Rio de Janeiro (anos finais)
media_2017 = round(mean(ideb_2017$IDEB_2017),1)
media_2019 = round(mean(ideb_2019$IDEB_2019),1)
media_2021 = round(mean(ideb_2021$IDEB_2021),1)
media_2023 = round(mean(ideb_2023$IDEB_2023),1)

# Crie um data.frame com as médias
dados_media <- data.frame(
  Ano = factor(c(2017, 2019, 2021, 2023), levels = c(2017, 2019, 2021, 2023)),
  Media_IDEB = c(media_2017, media_2019, media_2021, media_2023)
)

# Plotar o gráfico de linha com os valores acima dos pontos
ggplot(dados_media, aes(x = Ano, y = Media_IDEB)) +
  geom_line(group = 1, color = "black", size = 1) +  # Linha azul, mais espessa
  geom_point(color = "black", size = 3) +  # Pontos vermelhos
  labs(
    title = "Evolução da Média do IDEB no Rio de Janeiro (2017-2023)",
    x = "Ano",
    y = "Média IDEB"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2),  # Aumenta a distância entre o título e o gráfico
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona os rótulos do eixo X para melhor visualização
    panel.grid.major.x = element_blank(),   # Remove as grades verticais
    panel.grid.minor = element_blank(),     # Remove grades menores
    panel.grid.major.y = element_line(),# Grades horizontais leves
    plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
    )

#Bloxpot por escolas
p5 <- ggplot(ideb_2017, aes(y = ideb_2017$IDEB_2017)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2017") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

p6 <- ggplot(ideb_2019, aes(y = ideb_2019$IDEB_2019)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2019") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

p7 <- ggplot(ideb_2021, aes(y = ideb_2021$IDEB_2021)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2021") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

p8 <- ggplot(ideb_2023, aes(y = ideb_2023$IDEB_2023)) +
  geom_boxplot(fill ="grey" , color = "black") +
  labs(y = "Média IDEB 2023") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),   # Remove as grades verticais
        panel.grid.minor = element_blank(),     # Remove grades menores
        panel.grid.major.y = element_blank(), # Grades horizontais leves
        plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

(p5 | p6 | p7 | p8) +
  plot_annotation(title = "Distribuição Geral das Médias do IDEB por Escola", 
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))

#Criando um gráfico de medias e medianas

media_b_2017 = round(mean(notas_2017$Média_Bairro_2017),1)
media_b_2019 = round(mean(notas_2019$Média_Bairro_2019),1)
media_b_2021 = round(mean(notas_2021$Média_Bairro_2021),1)
media_b_2023 = round(mean(notas_2023$Média_Bairro_2023),1)

mediana_b_2017 = round(median(notas_2017$Média_Bairro_2017),1)
mediana_b_2019 = round(median(notas_2019$Média_Bairro_2019),1)
mediana_b_2021 = round(median(notas_2021$Média_Bairro_2021),1)
mediana_b_2023 = round(median(notas_2023$Média_Bairro_2023),1)

# Crie um data.frame com as médias
dados_media <- data.frame(
  Ano = factor(c(2017, 2019, 2021, 2023), levels = c(2017, 2019, 2021, 2023)),
  Media_IDEB = c(media_b_2017, media_b_2019, media_b_2021, media_b_2023),
  Mediana_IDEB = c(mediana_b_2017, mediana_b_2019, mediana_b_2021, mediana_b_2023)
)
# Transformar os dados para formato longo
dados_longos <- dados_media %>%
  pivot_longer(cols = c(Media_IDEB, Mediana_IDEB),
               names_to = "Tipo",
               values_to = "IDEB")

# Plotar o gráfico de linha com as médias e medianas
ggplot(dados_longos, aes(x = Ano, y = IDEB, color = Tipo, group = Tipo)) +
  geom_line(size = 1) +  # Linha para as médias e medianas
  geom_point(size = 3) +  # Pontos para as médias e medianas
  labs(
    title = "Evolução da Média e Mediana do IDEB por bairro no Rio de Janeiro (2017-2023)",
    x = "Ano",
    y = "Notas IDEB",
    color = "Legenda"  # Título da legenda
  ) +
  scale_color_manual(
    values = c("Media_IDEB" = "black", "Mediana_IDEB" = "red"),
    labels = c("Media_IDEB" = "Média", "Mediana_IDEB" = "Mediana")  # Renomeia as categorias
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2, size = 10, face = "bold"),  # Aumenta a distância entre o título e o gráfico
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona os rótulos do eixo X para melhor visualização
    legend.title = element_text(size = 12),  # Tamanho do título da legenda
    legend.text = element_text(size = 10),  # Tamanho do texto da legenda
    panel.grid.major.x = element_blank(),   # Remove as grades verticais
    panel.grid.minor = element_blank(),     # Remove grades menores
    panel.grid.major.y = element_line(),# Grades horizontais leves
    plot.background = element_rect(fill = "white")  # Fundo do gráfico branco
  )

#Dados 2017 Escolas
# Estatísticas básicas
Q1E_2017 <- quantile(ideb_2017$IDEB_2017, 0.25) # Primeiro quartil
Q3E_2017 <- quantile(ideb_2017$IDEB_2017, 0.75) # Terceiro quartil
IQRE_2017 <- Q3E_2017 - Q1E_2017             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2017E <- Q1E_2017 - 1.5 * IQRE_2017
limite_superior_2017E <- Q3E_2017 + 1.5 * IQRE_2017

# Identificando outliers
outliersE_2017 <- ideb_2017$IDEB_2017[ideb_2017$IDEB_2017 < limite_inferior_2017E | ideb_2017$IDEB_2017 > limite_superior_2017E]

#Dados 2019 Escolas
# Estatísticas básicas
Q1E_2019 <- quantile(ideb_2019$IDEB_2019, 0.25) # Primeiro quartil
Q3E_2019 <- quantile(ideb_2019$IDEB_2019, 0.75) # Terceiro quartil
IQRE_2019 <- Q3E_2019 - Q1E_2019             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2019E <- Q1E_2019 - 1.5 * IQRE_2019
limite_superior_2019E <- Q3E_2019 + 1.5 * IQRE_2019

# Identificando outliers
outliersE_2019 <- ideb_2019$IDEB_2019[ideb_2019$IDEB_2019 < limite_inferior_2019E | ideb_2019$IDEB_2019 > limite_superior_2019E]

#Dados 2021 Escolas
# Estatísticas básicas
Q1E_2021 <- quantile(ideb_2021$IDEB_2021, 0.25) # Primeiro quartil
Q3E_2021 <- quantile(ideb_2021$IDEB_2021, 0.75) # Terceiro quartil
IQRE_2021 <- Q3E_2021 - Q1E_2021             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2021E <- Q1E_2021 - 1.5 * IQRE_2021
limite_superior_2021E <- Q3E_2021 + 1.5 * IQRE_2021

# Identificando outliers
outliersE_2021 <- ideb_2021$IDEB_2021[ideb_2021$IDEB_2021 < limite_inferior_2021E | ideb_2021$IDEB_2021 > limite_superior_2021E]

#Dados 2023 Escolas
# Estatísticas básicas
Q1E_2023 <- quantile(ideb_2023$IDEB_2023, 0.25) # Primeiro quartil
Q3E_2023 <- quantile(ideb_2023$IDEB_2023, 0.75) # Terceiro quartil
IQRE_2023 <- Q3E_2023 - Q1E_2023             # Intervalo interquartil

# Limites para detectar outliers
limite_inferior_2023E <- Q1E_2023 - 1.5 * IQRE_2023
limite_superior_2023E <- Q3E_2023 + 1.5 * IQRE_2023

# Identificando outliers
outliersE_2017 <- ideb_2017$IDEB_2017[ideb_2017$IDEB_2017 < limite_inferior_2017E | ideb_2017$IDEB_2017 > limite_superior_2017E]


#2017
resultado_2017_escolas = subset(ideb_2017, ideb_2017$IDEB_2017 > limite_superior_2017E | ideb_2017$IDEB_2017 < limite_inferior_2017E)
resultado_2019_escolas = subset(ideb_2019, ideb_2019$IDEB_2019 > limite_superior_2019E | ideb_2019$IDEB_2019 < limite_inferior_2019E)
resultado_2021_escolas = subset(ideb_2021, ideb_2021$IDEB_2021 > limite_superior_2021E | ideb_2021$IDEB_2021 < limite_inferior_2021E)
resultado_2023_escolas = subset(ideb_2023, ideb_2023$IDEB_2023 > limite_superior_2023E | ideb_2023$IDEB_2023 < limite_inferior_2023E)


# Criar a tabela como um objeto gráfico
tabela_escolas_2017 <- tableGrob((resultado_2017_escolas))
tabela_escolas_2019 <- tableGrob((resultado_2019_escolas))
tabela_escolas_2021 <- tableGrob((resultado_2021_escolas))
tabela_escolas_2023 <- tableGrob((resultado_2023_escolas))

tabela_bairro_2017 <- tableGrob((resultado_2017))
tabela_bairro_2019 <- tableGrob((resultado_2019))
tabela_bairro_2021 <- tableGrob((resultado_2021))
tabela_bairro_2023 <- tableGrob((resultado_2023))

resultado_escolas_2017 <- as.data.frame(resultado_escolas_2017)

tabela_escolas_bairro_2017 <- tableGrob((resultado_escolas_2017))
tabela_escolas_bairro_2019 <- tableGrob((resultado_escolas_2019))
tabela_escolas_bairro_2021 <- tableGrob((resultado_escolas_2021))
tabela_escolas_bairro_2023 <- tableGrob((resultado_escolas_2023))

# Salvar a tabela como uma imagem
ggsave("escolas_IDEB2017.png", plot = tabela_escolas_2017, width = 9, height = 4, dpi = 300)
ggsave("escolas_IDEB2019.png", plot = tabela_escolas_2019, width = 9, height = 4, dpi = 300)
ggsave("escolas_IDEB2021.png", plot = tabela_escolas_2021, width = 9, height = 4, dpi = 300)
ggsave("escolas_IDEB2023.png", plot = tabela_escolas_2023, width = 11, height = 6, dpi = 300)

ggsave("bairro_IDEB2017.png", plot = tabela_bairro_2017, width = 5, height = 3, dpi = 300)
ggsave("bairro_IDEB2019.png", plot = tabela_bairro_2019, width = 5, height = 3, dpi = 300)
ggsave("bairro_IDEB2021.png", plot = tabela_bairro_2021, width = 5, height = 3, dpi = 300)
ggsave("bairro_IDEB2023.png", plot = tabela_bairro_2023, width = 5, height = 3, dpi = 300)

ggsave("escolas_bairro_IDEB2017.png", plot = resultado_escolas_2017, width = 10, height = 7, dpi = 300)
ggsave("escolas_bairro_IDEB2019.png", plot = tabela_escolas_bairro_2019, width = 9, height = 4, dpi = 300)
ggsave("escolas_bairro_IDEB2021.png", plot = tabela_escolas_bairro_2021, width = 9, height = 4, dpi = 300)
ggsave("escolas_bairro_IDEB2023.png", plot = tabela_escolas_bairro_2023, width = 11, height = 6, dpi = 300)

print(n=35,resultado_escolas_2017)
plot(p9)
summary(ideb_2023)
