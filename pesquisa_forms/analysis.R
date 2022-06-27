library(GGally)
library(ggpubr)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(ggalluvial)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(superheat)
library(stringr)

df_sim <- readr::read_csv("respostas_sim_cafe.csv") %>% dplyr::rename("Sexo"= Gênero)
df_nao <- readr::read_csv("respostas_nao_cafe.csv") %>% dplyr::rename("Sexo"= Gênero)
df_geral <- readr::read_csv("perfil_respostas.csv") %>% dplyr::rename("Sexo"= Gênero)

df_geral[df_geral == "Trabalho exclusivamente com afazeres domésticos"] <- "Doméstico"
df_nao[df_nao == "Trabalho exclusivamente com afazeres domésticos"] <- "Doméstico"
df_sim[df_sim == "Trabalho exclusivamente com afazeres domésticos"] <- "Doméstico"

# df_geral[df_geral == "grau_de_escolaridade"] <- "escolaridade"
# df_nao[df_nao == "grau_de_escolaridade"] <- "escolaridade"
# df_sim[df_sim == "grau_de_escolaridade"] <- "escolaridade"
dim(df_geral) # 111 respostas

names(df_geral)

df_geral$Idade <- factor(df_geral$Idade,
                         levels = c("Menor de 18 anos" ,"Entre 18 e 24" ,
                                    "Entre 25 e 30" ,   "Entre 31 e 40" ,
                                    "Entre 41 e 50"))
#----------------------------------------------------------------------------------------

# Características das pessoas que responderam o forms

#Consumo por gênero
df_geral %>%  
  ggplot(aes(Sexo,fill=consumo_café)) + 
    labs(fill= "Consome ou não café",y='Total',title = 'Consumo - Homens x mulheres') + 
    geom_bar(position = "dodge") + 
    geom_text(
      stat='count', aes(label=..count..),
      color='black', size = 5,vjust = 1.5, 
      position = position_dodge(.9)) +
    
    theme_classic()

#Consumo por idade
df_geral %>%  
  ggplot(aes(Idade,fill=consumo_café)) +
    labs(fill= "Consumo de café",x='Idade',y='Total',title = 'Consumo - Idade')+
    geom_bar() + 
    geom_text(
      stat='count', aes(label=..count..),
      color='black',size = 4,vjust = -1) +
    theme_classic()

#Ocupação
df_geral %>%  
  ggplot(aes(Ocupação,fill=consumo_café)) +
  labs(fill= "Consumo de café",x='Ocupação',y='Total',title = 'Consumo - Ocupação')+
  geom_bar() +  
  #theme_ipsum()
  theme_classic() + theme(axis.text.x = element_text(angle=45))

#faixa etária
df_geral %>%  
  ggplot(aes(x=Idade,fill=Sexo)) + 
  geom_bar(data = subset(df_geral,Sexo == "Feminino")) + 
  geom_bar(data = subset(df_geral,Sexo == "Masculino"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip() + 
  theme_ipsum()
  #labs(fill= "Sexo",x='Total',y='Idade',title = 'Faixa etária') + 
  #theme_classic()


#escolaridade
df_geral %>%  
  ggplot(aes(escolaridade,fill=consumo_café)) +
  labs(fill= "Consumo de café",x='Escolaridade',y='Total',title = 'Consumo - Idade')+
  geom_bar() +
  theme_classic()

#----------------------------------------------------------------------------------------
#Características das pessoas que não consomem café

names(df_nao)

#Razões pelas quais não consomem café

contagem_razoes <- df_nao %>% 
  dplyr::select(starts_with("razões_")) %>% 
  dplyr::summarize_all(sum) %>% 
  tidyr::pivot_longer(everything(), names_to = 'razoes',values_to = "Total")

contagem_razoes$prop <- round((contagem_razoes$Total / dim(df_nao)[1]) * 100,2)

contagem_razoes %>% 
  ggplot(aes(x=razoes,y=prop)) + 
  geom_col() +
  labs(x='Razões para não tomar café',y='Porporção (%)',title = 'Razões para não tomar café')+
  geom_text(
    aes(x = razoes, y = prop,label=prop),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4
  ) +
  theme_classic()
  
contagem_razoes %>% 
  ggplot(aes(x=razoes,y=Total)) + 
  geom_col(fill='brown') +
  labs(x='Razões para não tomar café',y='Total',title = 'Razões para não tomar café')+
  geom_text(
    aes(x = razoes, y = Total,label=Total),
    position = position_dodge(width = 1),
    vjust = -0.5, size = 4
  ) +
  theme_classic()
  
#não consigo pivotar essa tabela 
df_nao %>% 
  group_by(Sexo) %>% 
  dplyr::select("razões_hábito","razões_sabor_residual","razões_sabor_aroma", "razões_azia","razões_dor_cabeça","razões_saude") %>% 
  dplyr::summarize_all(sum)  %>% 
  pivot_longer(cols = Sexo, names_to = 'razoes',values_to = "Total")


#----------------------------------------------------------------------------------------
#Características das pessoas que consomem café

names(df_sim)


df_sim %>% 
  ggplot(aes(x=forma_preferida_consumo)) + 
  geom_bar(fill='brown') +
  labs(x='Formas preferidas de se beber café',y='Total',title = 'Formas preferidas de se beber café')+
  geom_text(
    stat='count', aes(label=..count..),
    color='black',size = 4,vjust = -0.75) + 
  theme_classic()



# Alluvium plot  Ocupação x etendimento x Tipo de varejo
df_sim %>%  
  ggplot(aes(axis1 = Ocupação,
                       axis2 = conhecimento_avançado,
                       axis3 = tipo_varejo )) +
  geom_alluvium(aes(fill = conhecimento_avançado)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Ocupação", "Tipo de varejo"),
                   expand = c(.1, .1)) +
  labs(title = "Tipo de varejo estratificado por Ocupação e grau de conhecimento",
       y = "Frequency") +
  theme_minimal()

# Grau 
colunas_grau <- df_sim %>% 
  dplyr::select(starts_with("grau_"))

df_sim[df_sim == "Muito fraco"] <- "-2"
df_sim[df_sim == "Moderadamente fraco"] <- "-1"
df_sim[df_sim == "Neutro"] <- "0"
df_sim[df_sim == "Moderadamente forte"] <- "1"
df_sim[df_sim == "Muito forte"] <- "2"

df_sim[c("grau_aroma","grau_sabor","grau_cor","grau_suavidade","grau_doçura")] <- sapply(df_sim[c("grau_aroma","grau_sabor","grau_cor","grau_suavidade","grau_doçura")], as.integer) %>%  as_tibble()

round(colMeans(df_sim[c("grau_aroma","grau_sabor","grau_cor","grau_suavidade","grau_doçura")]),1)

hist_grau_aroma <- df_sim %>%  
  ggplot(aes(x= grau_aroma)) +
  labs(x="Nível do Aroma",y="Quantidade") + 
  geom_histogram(fill='deepskyblue') + 
  theme_classic()

hist_grau_sabor <- df_sim %>%  
  ggplot(aes(x= grau_sabor)) +
  labs(x="Nível do Sabor",y="Quantidade") + 
  geom_histogram(fill='deepskyblue') + 
  theme_classic()

hist_grau_cor <- df_sim %>%  
  ggplot(aes(x= grau_cor)) +
  labs(x="Nível da Cor",y="Quantidade") + 
  geom_histogram(fill='deepskyblue') + 
  theme_classic()

hist_grau_suavidade <- df_sim %>%  
  ggplot(aes(x= grau_suavidade)) +
  labs(x="Nível de Suavidade",y="Quantidade") + 
  geom_histogram(fill='deepskyblue')+ 
  theme_classic()

hist_grau_doçura <- df_sim %>%  
  ggplot(aes(x= grau_doçura)) +
  labs(x="Nível de Doçura",y="Quantidade") + 
  geom_histogram(fill='deepskyblue')+ 
  theme_classic()

grid.arrange(hist_grau_aroma,hist_grau_cor,
             hist_grau_sabor,hist_grau_suavidade,hist_grau_doçura,
             nrow = 3, ncol = 2)
  
# Parallel coordinates --- um pouco confuso de ver
ggparcoord(df_sim[c("grau_aroma","grau_sabor","grau_cor","grau_suavidade","grau_doçura","Sexo")],
           columns = 1:5,groupColumn = 6,
           showPoints = TRUE,
           alphaLines = 0.3)  +
  theme_classic()

#media dos niveis para cada forma de consumo
df_sim %>%  
  group_by(forma_preferida_consumo) %>% 
  select(starts_with("grau_")) %>% 
  summarise_all(mean) 

# Correlação entre níveis dos atributos

df_sim %>% 
  dplyr::select("grau_aroma","grau_sabor","grau_cor","grau_suavidade","grau_doçura") %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^grau_","")) %>% 
  GGally::ggcorr(method = c("everything", "pearson"),label = TRUE)

# Relação Ocupação x Periodo
df_sim %>%
  dplyr::group_by(Ocupação) %>% 
  dplyr::select(starts_with("periodos_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^periodos_consumo_","")) %>% 
  dplyr::summarise_all(sum)


#Relação Ocupação e Número de xícaras
df_sim %>% 
  ggplot(aes(x=Ocupação,fill=qnt_xícaras)) +
  geom_bar(position = 'fill')+
  scale_y_continuous(labels = scales::percent) +
  labs("Número de xícaras consumidas para cada Ocupação ", y='Proporção') + 
  theme_classic()
  

# Local de consumo

df_sim %>% 
  ggplot(aes(x=local_consumo)) +
  geom_bar()


df_sim %>%  
  ggplot(aes(axis1 = local_consumo,
             axis2 = Ocupação )) +
  geom_alluvium(aes(fill = local_consumo)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Local de Consumo", "Ocupação"),
                   expand = c(.1, .1)) +
  labs(title = "Local de consumo estratificado por Ocupação e grau de conhecimento",
       y = "Proporção") +
  theme_minimal()


# Atributos 

## Agrupado por sexo
df_sim %>% 
  dplyr::group_by(Sexo) %>% 
  dplyr::select(starts_with("atributos_compra_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^atributos_compra_","")) %>% 
  dplyr::summarise_all(sum) 

## Agrupado por Ocupação
df_sim %>% 
  dplyr::group_by(Ocupação) %>% 
  dplyr::select(starts_with("atributos_compra_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^atributos_compra_","")) %>% 
  dplyr::summarise_all(sum) 

## Agrupado por atributos_compra
df_sim %>% 
  dplyr::select(starts_with("atributos_compra_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^atributos_compra_","")) %>% 
  dplyr::summarise_all(sum) 

# Perguntas diretas

#Geral 
  ### lembrar : quanto mais perto de 1 é concordo totalmente e mais perto de 2 é discordo totalmente
  ### Mediana nesse caso parece ser a métrica mais correta

df_sim %>% 
  dplyr::select(starts_with("café_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^café_","")) %>% 
  summary()

#----------------------------------------------------------------------------------------
# Data Science

