##################################################
# Analytics - Aula 14 - Regressao Logistica 4"
# Prof. Neylson
# 26/10/2018
# por Larissa Fernandes
##################################################
install.packages("titanic")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("descr") # pacote de estatisticas descritivas
install.packages("texreg")
install.packages("lmtest")
install.packages("pROC")
library(ggplot2)
library(dplyr)
library(descr) 
library(texreg)
library(lmtest)
library(pROC)
library(titanic)

bd = titanic_train
bd

# Regressao com Idade, idade e sexo , sexo e classe

reg1 = glm (Survived ~ Age, data = bd, family = binomial())
reg1

reg2 = glm(Survived ~ Age + Sex,data = bd, family = binomial())
reg2

reg3 = glm(Survived ~ Age + Sex + factor (Pclass), data = bd, family = binomial())
reg3

screenreg(list(reg1,reg2,reg3)) #coloca as tres regressoes juntas

# Fazendo o teste de likelihood ( me diz se vale a pena o modelo maix complexo)
lrtest(reg2,reg1) #me diz se um modelo é melhor que outro - se não tiver ...
                  #é porque os modelos tem o mesmo resultado
                  # se o p < 0,05 mostra que tem diferença entre eles
                 # eu analiso pelo P valor (estrelas)

lrtest(reg3,reg2) # reg3 tem ajuste melhor 

#conclusão: todos eles mostratram que o 3º modelo, é o melhor modelo
#os testes indicaram que REG3 é o melhor modelo entre os estimados 

# vamos verificar os resultados

summary(reg3) # logit - escala logaritimica 

#transformar os logits em chances relativas
# percentuais

coeficientes = coef(reg3) [-1] # menos o do intercepto
coeficientes
(exp(coeficientes)-1) * 100 # chances

# quando eu aumento 01 ano da idade há uma diminuicao de -3,63% nas chances 
# homens tem 91,98% a menos de chances de sobrevienvia do que as mulheres
# quem está na 2ª classe tem 73,01 % menos chances em relação a 1ª clase
# -92 -- 73 = 19 % / Ou seja a 3ª 19% menos chance de sobrevivencia que a 2ª classe


# Avaliar o Desempenho  Nosso Modelo -  dado de teste

titanic_test
bdtest = titanic_test %>% as_tibble
dim(bd)
dim(bdtest)

# Para avaliar o resultado do modelo,
# vamos construir uma confusion matrix
# Vamos primeiro predizer a classificação
# da variavel survived com os dados de teste

pred = predict(reg3, bdtest, 
               type = "response")
pred

yhat = ifelse(pred > .5,1,0)
yhat

# Montar a confusion matrix
# tabela realidade vs predito

verdade = titanic_gender_model$Survived
t = table(verdade, yhat)
t
res = roc(verdade,yhat) #dado real e dado predito
# Area under the curve: 0.9192 - área debaixo da curva é um resultado excelente 
auc(res)
plot.roc(res,print.auc = T)

  
  
  





