### Analytics
## Aula
##########################

# instalar pacotes

install.packages("readr")
install.packages("dplyr")
install.packages("descr")
install.packages("ggplot2")
install.packages("haven")

# carregar pacotes

library(readr)
library(dplyr)
library(descr)
library(ggplot2)
library(haven)

######
data("diamonds")

diamonds %>% glimpse()

help("diamonds")
#####

#Vamos investigar nesse dataset as variaveis cut e clarity

diamonds %>% 
  group_by(cut) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(total = sum(freq)) %>%
  mutate(perc = (freq/total)*100) %>%
  select(-total)

#######
#Agora com comandos mais simples
table(diamonds$cut)
#prop.table(table(diamonds$cut))
prop.table(table(diamonds$cut)) *100
diamonds$caro = ifelse(diamonds$price >
                         mean(diamonds$price),
                       1 , 0 )

table(diamonds$caro)
prop.table(table(diamonds$caro))*100

#vamos fazer agora uma tambela cruzada das variaveis cut e caro
t = table(diamonds$cut, diamonds$caro)
t
prop.table(t, margin = 1)*100

prop.table(t, margin = 2)*100

#margin 1 = linha // margin 2 = coluna

t
chisq.test(t) # valor do P quadrado
### o valor de P calculado foi <0.001
### ou seja, podemos rejeitar a hipotese nula
### e concluo que ha associacao estat. sig.
### entre as variaveis.

#testar a relacao entre caro e carat
t.test(diamonds$carat ~ diamonds$caro)


#Rodando regressão logistica:

#Y = Expensive
#X = Carat

#glm

reg = glm(caro ~ carat, data = diamonds,
        family = binomial(link = "logit"))


summary(reg)

# MQO - Método que tenta reduzir ao máximo o quadrado dos erros. (Não sendo possível com variável binária.)

# MV (Máxima Verosimilhança) - Método de estimação que pressupõe simulação de vários dados e tenta calcular a 
# probabilidade que mais se aproxima dos dados simulados. (Necessita de convergir os dados, senão os dados não podem ser usados.)

# Call:
#   glm(formula = caro ~ carat, family = binomial(link = "logit"), 
#       data = diamonds)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.4050  -0.0582  -0.0117   0.0130   3.6174  
# 
# Coefficients:
#   Estimate    Std. Error   z        value   Pr(>|z|)    
# (Intercept)   -14.7437     0.1789  -82.42   <2e-16 ***
#   carat        16.0827     0.1896   84.83   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 70761  on 53939  degrees of freedom
# Residual deviance: 14139  on 53938  degrees of freedom
# AIC: 14143
# 
# Number of Fisher Scoring iterations: 8


# Rodar a mesma regressão, só que Y = caro e X= carat + cut

reg = glm(caro ~ carat + cut, data = diamonds,
          family = binomial(link = "logit"))


summary(reg)

# Call:
#   glm(formula = caro ~ carat + cut, family = binomial(link = "logit"), 
#       data = diamonds)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.4713  -0.0588  -0.0109   0.0163   4.1608  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -15.79000    0.19366 -81.534  < 2e-16 ***
#   carat        16.89129    0.20384  82.866  < 2e-16 ***
#   cut.L         1.49018    0.06795  21.932  < 2e-16 ***
#   cut.Q        -0.68539    0.06081 -11.270  < 2e-16 ***
#   cut.C         0.64827    0.05535  11.713  < 2e-16 ***
#   cut^4         0.27793    0.04805   5.784 7.27e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 70761  on 53939  degrees of freedom
# Residual deviance: 13514  on 53934  degrees of freedom
# AIC: 13526
# 
# Number of Fisher Scoring iterations: 8



