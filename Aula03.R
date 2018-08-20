### Analytics
## Aula 03
## Prof. Neylson Crepalde
##########################

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

reg = lm(anosesco ~ escpai, bd)
summary(reg)

###
# Avaliando os pressupostos do modelo de regressão
###
# 1) Pressuposto de linearidade
ggplot(bd, aes(x = escpai, y = anosesco)) +
  geom_point()

# 2) Normalidade dos resíduos
e = reg$residuals

# Usando um histograma com curva de densidade
ggplot(NULL, aes(e)) + geom_histogram(aes(y = ..density..), color = 'white') +
  stat_function(fun = dnorm, args = list(mean = mean(e), sd = sd(e)),
                col = 'blue', lwd = 1)

# usando um qqplot (gráfico de probabilidade normal)
qqnorm(e)

##

# 3) Independência dos resíduos e 
# 4) Erro é homoscedástico
yhat = predict(reg)
ggplot(NULL, aes(x = yhat, y = e)) + geom_point() +
  stat_smooth(method = "lm")

# ou
# Podemos pedir os 4 gráficos de avaliação da regressão
par(mfrow = c(2,2))
plot(reg)
par(mfrow = c(1,1))

##### Monte a regressão e apresente os resultados.
# Y = anosesco - Váriavel dependente.
# X = escpai

## y = b0 + b1X + e
## E(Y) = 6,79 + 0,52X
# Pra cada aumento de uma unidade no X, eu vou ter um aumento no Y

reg = lm(anosesco ~ escpai, data = bd)
summary(reg)
confint(reg) # Intervalo de Confiança

#               2.5 %     97.5 %      # Significa que tem 95% de confiança que está entre esse número
#   (Intercept) 6.6327775 6.964649    -- b0 = 6.79871
#   escpai      0.5003156 0.551808    -- b1 = 0.52606

bd %>% 
  ggplot(aes(x = escpai, y = anosesco))+
  geom_point() +
  stat_smooth(method = 'lm', se = T, col = 'blue')

# Coefficients:
#              Estimate   Std. Error t value  Pr(>|t|)    
# (Intercept)  6.79871    0.08464    80.33    <2e-16 ***     
#   escpai     0.52606    0.01313    40.06    <2e-16 ***     
#   
#   --> Std. Error (Medida de quantos desvio padrão em média a curva se distancia dos erro padrão)
#   --> t value (Estimate/Std. Error) - Valor do Teste de Hipotese
#   -- H0 -> A diferença entre os valores de teste é estatisticamente igual a zero
#   -- H1 -> A diferença entre os valores de teste não é estatisticamente igual a zero
#   -- Se o Pr(>|t|) for menor que 0,05 (rejeita a H0) se for maior, não rejeita a H0
#   -- Se tiver do lado *** - é estatisticamente significativo. (É verdade pra amostra)

##### Monte outra regressão e apresente os resultados.
# Y = anosesco - Váriavel dependente.
# X = escmãe
# Estime a regressão, interprete os resultados e interprete o intervalo de confiança.

reg2 = lm(anosesco ~ `escmãe`, data = bd)
summary(reg2)

# Coefficients:
#               Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept)   6.83028     0.08669     78.79     <2e-16 ***
#   escmãe      0.51467     0.01345     38.27     <2e-16 ***

# para uma pessoa que tem uma mãe com 0 ano de escolaridade, o filho é esperado ter em media 6.83 anos de escolaridade
# e a cada ano de escolaridade incremental da mãe é esperado um acrescimo de 0.5146 ano de escolaridade na escolaridade do filho em media


confint(reg2) # Intervalo de Confiança

#               2.5 %     97.5 %      # Significa que tem 95% de confiança que está entre esse número
#   (Intercept) 6.660320  7.0002408    
#   escmãe      0.488307  0.5410346   

# com um intervalo de confianca de 95%, a hipotese nula é rejeitada e podemos inferir esse dado para a população

confint(reg2, level =.99) # Intervalo de Confiança 99%

##### Monte outra regressão e apresente os resultados.
# Y = isei88 - Váriavel dependente.
# X = anosesco
# Estime a regressão, interprete os resultados e interprete o intervalo de confiança.

reg3 = lm(isei88 ~ anosesco, data = bd)
summary(reg3)

confint(reg3)

bd %>% 
  ggplot(aes(x = anosesco, y = isei88))+
  geom_point() +
  stat_smooth(method = 'lm', se = T, col = 'blue')

