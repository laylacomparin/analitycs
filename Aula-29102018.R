################################################
# CENTRO UNIVERSITÁRIO METODISTA IZABELA HENDRIX
# Analytics
# Aula 29/10/2018
# por Layla Comparin
################################################

install.packages("dplyr")
install.packages("ggplot2")
install.packages("texreg")
install.packages("descr")

library(dplyr)
library(ggplot2)
library(texreg)
library(descr)

bd = mtcars
names(bd)

# Parte A - Regressão linear

# Monte 3 modelos de regressão. Todos eles devem explicar o consumo do carro (mpg) através de seus atributos.
# 1 - No primeiro, utilize o peso do carro (wt) para explicar o consumo. 
# 2 - No segundo, use a potência em cavalos (hp)
# 3 - Na terceira use as duas variáveis juntas. Apresente uma tabela com os resultados das 3 regressões juntas.

y = mpg

bd$wtmed = bd$wt - mean(bd$wt)
bd$hpmed = bd$hp - mean(bd$hp)
bd$ammed = bd$am - mean(bd$am)

reg1 = lm (mpg ~ wtmed, bd) #(Peso)
reg2 = lm (mpg ~ hpmed, bd) #(Cavalos)
reg3 = lm (mpg ~ wtmed+hpmed, bd) #(Peso + Cavalos)

screenreg(list(reg1, reg2, reg3))

# ============================================
#               Model 1    Model 2    Model 3  
# --------------------------------------------
#   (Intercept)  20.09 ***  20.09 ***  20.09 ***
#                (0.54)     (0.68)     (0.46)   
#   wtmed        -5.34 ***             -3.88 *** (Peso sobre o consumo controlando sobre as outras variaveis.)
#                (0.56)                (0.63)   
#   hpmed                   -0.07 ***  -0.03 ** (Cavalos sobre o consumo controlando sobre as outras variaveis.)
#                (0.01)     (0.01)   
# --------------------------------------------
#   R^2           0.75       0.60       0.83 
#   Adj. R^2      0.74       0.59       0.81 (81% - Pegar a maior porcentagem)    
#   Num. obs.     32         32         32       
#   RMSE          3.05       3.86       2.59  # (Quanto menor, melhor (Raiz da média do erro²))
# ============================================
#   *** p < 0.001, ** p < 0.01, * p < 0.05    


# 1) Interprete os resultados das 3 regressões
# Para cada peso a mais do carro, espera-se um aumento em média de 5,34 litros de consumo.(peso maior/consumo maior/menos km por litro ele faz)
# Para cada potência em cavalos a mais, espera-se um aumento em média de 0,07 de consumo.

# 2) Qual dos modelos melhor se ajusta aos dados? Por quê?
# Model 3, porque o percentual de variância explicada é maior.

# 3) Os pressupostos do modelo de regressão estão sendo respeitados no melhor modelo? Mostre.
#

b0 = reg1$coefficients[1]
b1 = reg1$coefficients[2]
b2 = reg1$coefficients[3]

ggplot(bd, aes(x=wt,
               y=mpg)) +
  geom_point() +
  geom_abline(intercept = b0,
              slope = b1,
              col = "red",
              lwd = 1)

# 4) Agora, ao melhor modelo, adicione a variável "transmissão automática" (am). Interprete os resultados 
# deste modelo. Ele é melhor ou pior que o modelo estimado anteriormente? Explique.
#

reg4 = lm (mpg ~ wtmed+hpmed+ammed, bd) 

summary(reg4)

# Parte B - Regressão logística

# Agora, vamos utilizar um modelo logístico para explicar as chances de um nenê recém-nascido ter 
# baixo peso ao nascer. Para isso, carregamos o banco de dados lowbt direto do repositório github
library(haven)
bd = read_dta("https://github.com/neylsoncrepalde/analytics/blob/master/lowbwt.dta?raw=true")
names(bd)

# A variável dependente é, portanto, lbw (baixo peso ao nascer). Utilize a variável idade (age) e a variável
# É fumante (smoke) para explicar a dependente. A seguir estime outro modelo utilizando as variáveis
# peso da mãe no último período menstrual (lwt) e É fumante (smoke). Apresente os resultados dos dois modelos
# numa única tabela:

reg = glm(lbw ~ age+smoke,bd, family = binomial())
summary(reg)

bd$pesokg = bd$lwt * 0.453

# Call:
#   glm(formula = lbw ~ age + smoke, family = binomial(), data = bd)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.1589  -0.8668  -0.7470   1.2821   1.7925  
# 
# Coefficients:
#              Estimate   Std. Error  z value Pr(>|z|)  
# (Intercept)  0.06091    0.75732     0.080   0.9359  
# age         -0.04978    0.03197     -1.557   0.1195  
# smoke        0.69185    0.32181     2.150   0.0316 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 234.67  on 188  degrees of freedom
# Residual deviance: 227.28  on 186  degrees of freedom
# AIC: 233.28
# 
# Number of Fisher Scoring iterations: 4

# 1) Qual dos dois modelos se ajusta melhor aos dados? Por quê? Explique quais foram os critérios que você adotou
# para se decidir.


# 2) Interprete os resultados de maneira substantiva com os coeficientes do jeito como eles vem na tabela.
# 3) Calcule as chances relativas a partir dos coeficientes estimados e interprete os resultados.
# 4) Calcule a probabilidade de uma criança ter baixo peso ao nascer dado que sua mãe tinha peso igual a 120 libras
# e seja não fumante.
# 5) Calcule a probabilidade de uma criança ter baixo peso ao nascer dado que mão tinha pesoa igual a 140 libras
# e seja fumante.

