---
title: "Analytics"
subtitle: "Avaliação 01"
author: "Ester Martins, Warley Gomes, Danielle Duarte, Edesio Junior, Layla Comparin"
output: html_notebook
---

# 1) Vamos explicar o estatus sócio econômico de uma pessoa em
# 2014. Para isso, estime uma regressão tendo como variável
# dependente isei88, e como covariáveis anosesco, isei88pai,
# escpai e escmãe. Interprete os resultados e teste o modelo 
# de acordo com os pressupostos de 
# linearidade, normalidade do erro, independência do erro,
# homoscedasticidade e multicolinearidade.

```{r setup, include=FALSE}
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"haven" %in% installed.packages()) install.packages("haven")

# Carregando as bibliotecas necessárias
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
bd = read_sav("https://github.com/neylsoncrepalde/MODUS/blob/master/PNAD2014_30a50_novo4.sav?raw=true")

#centralizar a renda do pai na média
bd$iseidopaicent = bd$isei88pai - mean(bd$isei88pai)


# criando a primeira regressão
reg0= lm (isei88 ~ anosesco + iseidopaicent+ escpai + escmãe, data = bd)
summary(reg0)
confint(reg0)

#normalidade dos erros
e= residuals(reg0)
b0 = reg0$coefficients[1]
b1 = reg0$coefficients[2]
b2 = reg0$coefficients[3]

ggplot(NULL, aes(e)) + geom_histogram() #Histograma
qqnorm(e) #Gráfico de probabilidade normal

# Linearidade 

ggplot(bd, aes(x = anosesco,escpai,escmãe,isei88pai, 
                 y = isei88)) + 
  geom_point()+ 
  geom_abline(intercept = b0,  
              slope = b1, #inclinação 
              col = "#556B2F", 
              lwd = 1) 

#independência do erro
plot(reg0)

```

### Explicando o b0 (isei88)

# Para um individuo que tem escolaridade 0, ou seja analfabeto, cujo pai tambem seja analfabeto e o status socio-ocupacional do pai do individuo esteja na media, e a mae tambem seja analfabeta, espera que o individuo tenha um indice socio ocupacional de 18.68.

### Explicando o b1 (anosesco)

# Para cada ano de escolaridade do individuo, espera-se um aumento mÃ©dio de 1.85 pontos no indice socio ocupacional do individuo.

### Explicando o b2 (iseipai_cent)

# Para o aumento de 1 ponto do indice socio-ocupacional do pai do individuo, espera-se um aumento médio 0.17 pontos no indice socio-ocupacional do individuo, mantendo constante as outras variaveis.

### Explicando o b3 (escpai)

# Para cada ano de escolaridade a mais do pai do indiviuo, espera-se um aumento medio de 0.28 pontos no indice socio-ocupacional do individuo, mantendo constante as outras variaveis.

### Explicando o b4 (escmãe)

# Para cada ano de escolaridade a mais da mãe do individuo, espera-se um aumento médio de 0.30 pontos no indice socio-ocupacional do individuo, mantendo constante as outras variaveis.

# As variaveis anosesco, isei88pai_cent, escpai, escmãe contidas nesta regressao explicam 39,05% da variancia do indice socio-ocupacional de um individuo.


# 2) Vamos explicar a renda do indivíduo a partir de sua 
# idade e de sua escolaridade. Lembre-se de que trabalhar 
# com idade requer um tratamento especial no modelo de renda.
# Lembre-se também que a variável renda exige uma 
# transformação matemática. Essa transformação nos dará um
# modelo log-lin que possui interpretação dos betas em termos
# %.

              
```{r setup1, include=FALSE}
bd1= read_sav("https://github.com/neylsoncrepalde/MODUS/blob/master/PNAD96_25a60_Modus.sav?raw=true")

# transformando a idade (idadecen)
bd1$idade = bd1$idadecen + abs(min(bd1$idadecen))
summary(bd1$idade)
bd1$idade = bd1$idade + 25
summary(bd1$idade)

# transformando a renda (lnrenda)
calc_renda = function(x) {
  med = mean(bd1$lnrenda[bd1$idade == x])
  return(med)
}
  
med_renda = sapply(25:60, calc_renda)
med_renda
plot(25:60, med_renda)

reg1 = lm(lnrenda ~ idadecen +I(idadecen^2) + anosesco, data = bd1)
summary(reg1)

options(scipen = 999)

exp(reg1$coefficients[1])
exp(reg1$coefficients[2])
exp(reg1$coefficients[4])

b0 = reg1$coefficients[1]
b1 = reg1$coefficients[2]
b3 = reg1$coefficients[4]

```              

# Para um indiviudo que tem escolaridade 0, é esperado que a renda seja 173.384 reais em média e a cada aumento de um ano de escolaridade da pessoa é esperado um acrescimo de 14,69% na renda em media com um intervalo de confianca de 95% controlando pelas outras variáveis, a hipotese nula é rejeitada e podemos inferir esse dado para a população
