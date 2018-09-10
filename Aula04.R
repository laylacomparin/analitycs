################################################
# CENTRO UNIVERSITÁRIO METODISTA IZABELA HENDRIX
# Analytics
# Aula 04
# por Layla Comparin
################################################

# Se as bibliotecas necessárias não estiveram instaladas, instale
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"haven" %in% installed.packages()) install.packages("haven")

# Carregando as bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(haven)

# Carrega uma amostra da PNAD 2014

bd = read_sav("https://github.com/neylsoncrepalde/MODUS/blob/master/PNAD2014_30a50_novo4.sav?raw=true")


# Estime um modelo de regressão com os seguintes parâmetros:
# Y = isei88
# X = isei88pai
# Interprete os resultados:
# a) Inteprertar os coeficientes.
# b) Interpretar os outputs de ajuste
# c) Avaliar os pressupostos do modelo

reg = lm(isei88 ~ isei88pai, data = bd)
summary(reg)
confint(reg) # Intervalo de Confiança

# para uma pessoa que tem o pai com o isei (status sócio economico) 0, o filho é esperado ter em media 24 de isei
# e a cada isei(status sócio economico) incremental do pai é esperado um acrescimo de 0.47715 no status socio economico 
# do filho em media
# com um intervalo de confianca de 95%, a hipotese nula é rejeitada e podemos inferir esse dado para a população


# Multiple R-squared:  0.2034,	Adjusted R-squared:  0.2032 #### 20% da variancia de Y é explicado pelo modelo.
# F-statistic:  1119 on 1 and 4381 DF,  p-value: < 2.2e-16 #### Se o p-value está próximo de zero, significa que a regressão é 
# estatisticamente significativa. 

bd$iseipaicent = bd$isei88pai - mean(bd$isei88pai)

mean(bd$isei88pai)
mean(bd$iseipaicent)
#-7.935851e-16 = 0

# Valor minimo na variável original:
min(bd$isei88pai)

# Valor minimo na variável criada:
min(bd$iseipaicent)


#Nova regressão:

reg = lm(isei88 ~ iseipaicent, data = bd)
summary(reg)
confint(reg) # Intervalo de Confiança

# para uma pessoa que tem o pai com o isei (status sócio economico) médio, o filho é esperado ter em media 38,91 de isei
# e a cada isei(status sócio economico) incremental do pai é esperado um acrescimo de 0.47715 no status socio economico 
# do filho em media
# com um intervalo de confianca de 95%, a hipotese nula é rejeitada e podemos inferir esse dado para a população

## Testar os pressupostos do modelo
# 1)  linearidade

b0 = reg$coefficients[1]
b1 = reg$coefficients[2]

ggplot(bd, aes(x=iseipaicent,
               y=isei88)) +
  geom_point() +
  geom_abline(intercept = b0,
              slope = b1,
              col = "red",
              lwd = 1)


# 2)  Normalidade dos erros

e = residuals(reg)
hist(e)
qqnorm(e)

# 2)  Homoscedasticidade e Indepedência dos erros:

plot(reg)



############ Explicando a renda:
# Y = renda
# X = anosesco

reg = lm(renda ~ anosesco, data = bd)
summary(reg) # 32% da renda é explicada pela escolaridade.
confint(reg) # Intervalo de Confiança

# para uma pessoa que tem o pai com o isei (status sócio economico) médio, o filho é esperado ter em media 38,91 de isei
# e a cada isei(status sócio economico) incremental do pai é esperado um acrescimo de 0.47715 no status socio economico 
# do filho em media
# com um intervalo de confianca de 95%, a hipotese nula é rejeitada e podemos inferir esse dado para a população

## Testar os pressupostos do modelo
# 1)  linearidade

b0 = reg$coefficients[1]
b1 = reg$coefficients[2]

ggplot(bd, aes(x=anosesco,
               y=renda)) +
  geom_point() +
  geom_abline(intercept = b0,
              slope = b1,
              col = "red",
              lwd = 1)
hist(bd$renda)

# 2)  Normalidade dos erros

e = residuals(reg)
hist(e)
qqnorm(e)

# 2)  Homoscedasticidade e Indepedência dos erros:

plot(reg)

# 3)  Criando o logaritmo da renda:

bd$logrenda = log(bd$renda)
hist(bd$logrenda)

reg = lm(logrenda ~ anosesco, data = bd)
e = residuals(reg)
hist(e)
summary(reg)
confint(reg) # Intervalo de Confiança
b0 = reg$coefficients[1]
b1 = reg$coefficients[2]
(exp(b1)-1)*100

#Se o individuo tem 10 anos de escolaridade na média, estimasse que ele ganha 6671.998 
renda10 = exp(b0)*1.1245*15

# para uma pessoa que tem escolaridade baixa, é esperado que a renda seja 593.33 reais em média
# e a cada aumento de escolaridade incremental da pessoa é esperado um acrescimo de 12,45% na renda em media
# com um intervalo de confianca de 95%, a hipotese nula é rejeitada e podemos inferir esse dado para a população

