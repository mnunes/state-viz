# pacotes necessarios

library(tidyverse)
library(rvest)
library(janitor)
library(stringi)
library(brazilmaps)

# extrair dados da populacao dos estados da wikipedia

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_popula%C3%A7%C3%A3o"

pagina <- read_html(url)

tabelas <- html_table(pagina, fill = TRUE)

dados <- tabelas[[1]]

# limpeza dos dados

dados <- clean_names(dados)

dados <- dados %>%
  select(unidade_federativa, populacao) %>%
  mutate(populacao = stri_replace_all_charclass(populacao, "\\p{WHITE_SPACE}", "")) %>%
  mutate(populacao = as.numeric(populacao))

# encontrar as proporcoes 

dados <- dados %>%
  mutate(proporcao = populacao/sum(populacao)*100) %>%
  print()

###########
### 33% ###
###########

v1 <- c(1, 2, 14)
sum(dados$proporcao[v1])

v2 <- c(3, 4, 5, 6, 10, 18, 19, 22)
sum(dados$proporcao[v2])

v3 <- (1:27)[!(1:27 %in% c(v1, v2))]
sum(dados$proporcao[v3])

# criar indices para as cores

id <- rep("", length(dados$proporcao))

id[v1] <- "a"
id[v2] <- "b"
id[v3] <- "c"

dados <- cbind(dados, id)

# informacoes sobre o mapa

mapa_br <- get_brmap("State")

names(mapa_br) <- c("unidade_federativa", "estado", "regiao", "geometry")

# colocar os nomes das unidades federativas no mesmo padrao

dados$unidade_federativa <- tolower(dados$unidade_federativa)
mapa_br$unidade_federativa <- tolower(mapa_br$unidade_federativa)

dados <- left_join(mapa_br, dados)

# criacao do mapa

ggplot(dados) +
  geom_sf(aes(fill = id)) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "none") +
  scale_fill_viridis_d() +
  labs(caption = "marcusnunes.me")

