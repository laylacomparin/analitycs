################################################
# CENTRO UNIVERSITÁRIO METODISTA IZABELA HENDRIX
# Analytics
# Aula 01
# por Layla Comparin
################################################

# Se as bibliotecas necessárias não estiveram instaladas, instale
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"haven" %in% installed.packages()) install.packages("haven")
if (!"descr" %in% installed.packages()) install.packages("descr")

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("haven")
install.packages("descr")

# Carregando as bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(descr)

# Carrega uma amostra da PNAD 2014

bd = read_sav("https://github.com/neylsoncrepalde/MODUS/blob/master/PNAD2014_30a50_novo4.sav?raw=true")

# Investigando o banco de dados

dim(bd) #tamanho
names(bd) #nomes das variáveis
head(bd) #primeiros casos

# Vamos investigar as variáveis isei88 e anos de escolaridade
# Que tipo de variáveis são?
class(bd$isei88)
class(bd$anosesco)

# Verificando estatísticas descritivas
summary(bd$isei88)
summary(bd$anosesco)

# Correlação entre variáveis - R Pearson
cor(bd$anosesco, bd$escpai)

# Gráfico de dispersão de anosesco e escpai
plot(bd$escpai, bd$anosesco)

# Gráfico de dispersão de anosesco e escpai usando o ggplot
ggplot(bd, aes(x = escpai, y =anosesco )) +
  geom_point() +
  labs (x = "Escolaridade do Pai",
        y = "Anos de Escolaridade",
        title = "Dispersão")

# Visualizar a distribuição de anosesco e escpai usando o R base (Histograma)
hist(bd$anosesco)
hist(bd$escpai)

ggplot(bd, aes(x = anosesco)) +
  geom_histogram(bins = 10) 
ggplot(bd, aes(x = escpai)) +
  geom_histogram(bins = 5) 

# Se formos estimar uma relação entre essas variáveis, o que obtemos?
ggplot(bd, aes(x = anosesco, y =anosesco)) +
  geom_point() + stat_smooth(method = "lm")
