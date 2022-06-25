library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
readr::rea

df_sim <- readr::read_csv("respostas_sim_cafe.csv")
df_nao <- readr::read_csv("respostas_nao_cafe.csv")
df_geral <- readr::read_csv("perfil_respostas.csv")


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
  ggplot(aes(Gênero,fill=consumo_café)) + 
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
  labs(fill= "Consumo de café",x='Ocupação',y='Total',title = 'Consumo - Idade')+
  geom_bar() +  
  theme_ipsum()
  theme_classic() + theme(axis.text.x = element_text(angle=45))

#faixa etária
df_geral %>%  
  ggplot(aes(x=Idade,fill=Gênero)) + 
  geom_bar(data = subset(df_geral,Gênero == "Feminino")) + 
  geom_bar(data = subset(df_geral,Gênero == "Masculino"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip() + 
  theme_ipsum()
  #labs(fill= "Gênero",x='Total',y='Idade',title = 'Faixa etária') + 
  #theme_classic()


#escolaridade
df_geral %>%  
  ggplot(aes(grau_de_escolaridade,fill=consumo_café)) +
  labs(fill= "Consumo de café",x='Escolaridade',y='Total',title = 'Consumo - Idade')+
  geom_bar() +  
  theme_classic()

#----------------------------------------------------------------------------------------
#Características das pessoas que não consomem café

names(df_nao)

#Razões pelas quais não consomem café

contagem_razoes <- df_nao %>% 
  dplyr::select("razões_hábito","razões_sabor_residual","razões_sabor_aroma", "razões_azia","razões_dor_cabeça","razões_saude") %>% 
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
  theme_classic()
  
#não consigo pivotar essa tabela 
df_nao %>% 
  group_by(Gênero) %>% 
  dplyr::select("razões_hábito","razões_sabor_residual","razões_sabor_aroma", "razões_azia","razões_dor_cabeça","razões_saude") %>% 
  dplyr::summarize_all(sum)  %>% 
  pivot_longer(cols = Gênero, names_to = 'razoes',values_to = "Total")

