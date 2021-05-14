##### Análise das prioridades de recursos destinados a LOA em 2020 ############

#-------------Bando de dados, informações e pacote--------------------------

install.packages("dplyr")
install.packages("readxl")

library(gghighlight)
library(readxl)
library(dplyr)
library(ggplot2)

despesa_discriminacao <- read_xlsx("LOA2020BASE.xlsx", sheet = 1)
distribuicao_funcao_governo <- read_xlsx("LOA2020BASE.xlsx", sheet = 2)

#------------------Criando Variaveis para Análise------------------

despesa_discriminacao$Perc <- (despesa_discriminacao$Valor/sum(despesa_discriminacao$Valor))*100

distribuicao_funcao_governo$Perc <- (distribuicao_funcao_governo$Valor/sum(distribuicao_funcao_governo$Valor))*100


#-------------------------Observação gráficos-------------------
#Distribuição de acordo com a LOA
despesa_por_secretaria <- despesa_discriminacao%>%
  ggplot(aes(y= Perc, x= reorder(Orgao, -Perc)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  geom_label(aes(label = Valor), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)
  

despesa_por_funcao <- distribuicao_funcao_governo%>%
  ggplot(aes(y= Perc, x= reorder(Funcao, -Perc)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  geom_label(aes(label = Valor), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)



