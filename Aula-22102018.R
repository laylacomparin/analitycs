################################################
# CENTRO UNIVERSITÁRIO METODISTA IZABELA HENDRIX
# Analytics
# Aula 22/10/2018
# por Layla Comparin
################################################

install.packages("titanic")

library(titanic)
library(dplyr)
library(ggplot2)
library(descr)

bd = titanic_train 

names(bd)

freq(bd$Survived, plot=F)

#Vamos estimar as chances de sobrevivência a partir da idade:

summary(bd$Age)

reg1 = glm(Survived ~ Age, data = bd,
           family = binomial())

summary(reg1)

b0 = coef(reg1) [1]
b1 = coef(reg1) [2]
b1
(exp(b1) -1) * 100
coef(reg1)

# Call:
#   glm(formula = Survived ~ Age, family = binomial(), data = bd)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.1488  -1.0361  -0.9544   1.3159   1.5908  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -0.05672    0.17358  -0.327   0.7438  
# Age         -0.01096    0.00533  -2.057   0.0397 * (-0.01096  = b1 - Coeficiente da Idade (logit))
### Para cada um ano a mais eu tenho uma diminuição de 0.01 no log das chances de sobrevivência!
### Ou seja, quanto maior a idade, menor a chance de sobrevivência.
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 964.52  on 713  degrees of freedom
# Residual deviance: 960.23  on 712  degrees of freedom
# (177 observations deleted due to missingness)
# AIC: 964.23
# 
# Number of Fisher Scoring iterations: 4


#Qual a probabilidade de sobrevivência de uma pessoa de 30 anos ?
idade_quero = 70
exp(b0 + b1*idade_quero)/1 + exp(b0 + b1*idade_quero)


# MOdelando a sobrevivência por sexo
reg2 = glm(Survived ~ Sex, data = bd,
           family = binomial())

summary(reg2)

b0 = coef(reg2)[1]
b1 = coef(reg2)[2]

(exp(b1) - 1) * 100

# Call:
#   glm(formula = Survived ~ Sex, family = binomial(), data = bd)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6462  -0.6471  -0.6471   0.7725   1.8256  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.0566     0.1290   8.191 2.58e-16 ***
#   Sexmale      -2.5137     0.1672 -15.036  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1186.7  on 890  degrees of freedom
# Residual deviance:  917.8  on 889  degrees of freedom
# AIC: 921.8
# 
# Number of Fisher Scoring iterations: 4


# Estimar as chances de sobrevivencia por idade e sexo
reg3 = glm(Survived ~ Age + Sex, data = bd,
           family = binomial())

summary(reg3)

b0 = coef(reg3)[1]
b1 = coef(reg3)[2]
b2 = coef(reg3)[3]

(exp(b1) - 1) * 100
(exp(b1) - 1) * 100

# Call:
#   glm(formula = Survived ~ Age + Sex, family = binomial(), data = bd)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7405  -0.6885  -0.6558   0.7533   1.8989  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.277273   0.230169   5.549 2.87e-08 ***
#   Age         -0.005426   0.006310  -0.860     0.39    
# Sexmale     -2.465920   0.185384 -13.302  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 964.52  on 713  degrees of freedom
# Residual deviance: 749.96  on 711  degrees of freedom
# (177 observations deleted due to missingness)
# AIC: 755.96
# 
# Number of Fisher Scoring iterations: 4


# Probabilidade de sobrevivencia da Layla
exp(b0 + (b1*24) + (b2*0)) / (1 + exp(b0 + (b1*24) + (b2*0)))

#Reg4
# Y = Survived
#Explicado por Age, Sex, Pclass
bd$Pclass = as.factor(bd$Pclass)

reg4 = glm(Survived ~ Age + Sex + Pclass, data = bd,
           family = binomial())

summary(reg4)

b0 = coef(reg4)[1]
b1 = coef(reg4)[2]
b2 = coef(reg4)[3]
b3 = coef(reg4)[4]
b4 = coef(reg4)[5]

(exp(coef(reg4)) - 1)*100

# Probabilidade de sobrevivencia da Layla na 3ª classe
exp(b0 + (b1*24) + (b2*0) +   ) / (1 + exp(b0 + (b1*24) + (b2*0) + (b3)))

# Probabilidade de sobrevivencia da Layla na 1ª classe
exp(b0 + (b1*24) + (b2*0)) / (1 + exp(b0 + (b1*24) + (b2*0)))

# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  3.777013   0.401123   9.416  < 2e-16 ***
#   Age         -0.036985   0.007656  -4.831 1.36e-06 ***
#   Sexmale     -2.522781   0.207391 -12.164  < 2e-16 ***
#   Pclass2     -1.309799   0.278066  -4.710 2.47e-06 ***
#   Pclass3     -2.580625   0.281442  -9.169  < 2e-16 ***

install.packages("texreg")
library(texreg)                               

htmlreg(list(reg1, reg2, reg3, reg4),
        file = "tabela.html")


# Model 1 Model 2 Model 3 Model 4 
# (Intercept) -0.06 1.06*** 1.28*** 3.78*** 
#             (0.17) (0.13) (0.23) (0.40) 
# Age -0.01*  -0.01 -0.04*** 
#     (0.01)  (0.01) (0.01) 
# Sexmale  -2.51*** -2.47*** -2.52*** 
#           (0.17) (0.19) (0.21) 
# Pclass2    -1.31*** 
#             (0.28) 
# Pclass3    -2.58*** 
#             (0.28) 
# AIC         964.23 921.80 755.96 657.28 - Akaike Informatial Criteria
# BIC         973.37 931.39 769.67 680.14 - Bayesian Informatial Criteria
# Log Likelihood - 480.11 -458.90 -374.98 -323.64 
# Deviance    960.23 917.80 749.96 647.28 
# Num. obs. 714 891 714 714 
# ***p < 0.001, **p < 0.01, *p < 0.05 

####### Quanto menor o valor do modelo (melhor)
