# "Análise de dados longitudinais (Parte 2): O pacote lavaan para equações estruturais"
# author: "Tiago Ferreira"

## Preparação de pacotes
library(lavaan)
library(semTools)
library(semPlot)

## Importação de dados  
DF_roth <-
  read.csv(
    "G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\Intro to Lavaan\\Intro_to_Lavaan\\Intro to Lavaan_DATA\\DF_roth.csv")

## Alguma estatística descritiva  
dim(DF_roth)
names(DF_roth)
head(DF_roth,4)
summary(DF_roth)
cor(DF_roth)
ggpairs(DF_roth, title="correlogram with ggpairs()")


## Regressão linear simples
## Estimação OLS do modelo
lm_fit.1 <- lm(illness ~ exercise, data = DF_roth)
summary(lm_fit.1)$coefficients # mostrar parametros

## Regressão linear simples no lavaan (Especificação)
model.1 <- "
illness ~ exercise" # illness em função de exercise
fit.1 <- sem(model.1, data=DF_roth, meanstructure=T)
summary(fit.1, standardized = TRUE, rsquare = TRUE)

## Regressão múltipla
## Estimação OLS do modelo
lm_fit.2 <- lm(formula = illness ~ exercise + hardy + fitness + stress, data = DF_roth)
summary(lm_fit.2)
summary(lm_fit.2)$coefficients # mostrar parâmetros

## Regressão múltipla no lavaan (Estimação)  
model.2 <- "
illness ~ exercise + hardy + fitness + stress"
fit.2 <- sem(model.2, data=DF_roth, meanstructure=TRUE)
summary(fit.2, standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit.2, rsquare = TRUE)[parameterEstimates(fit.2)$op=="~",]

## Modelos de Mediação
model.3 <- "
# regressões
fitness ~ exercise
stress ~ hardy
illness ~ fitness + stress
"
fit.3 <- sem(model.3, data=DF_roth, meanstructure=T)
summary(fit.3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
parameterestimates(fit.3)[parameterEstimates(fit.3)$op=="~",]

residuals(fit.3, type = "raw")
residuals(fit.3, type = "cor")
resid(fit.3, type="normalized")$cov # resíduo dividido pelo devios padrão estimado dos resíduos

fitMeasures(fit.3)[1:32]
fitMeasures(fit.3, c("chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "tli",  "srmr"))


modindices(fit.3, sort.=TRUE, minimum.value=4) # o primeiro argumento é um objeto gerado pelo lavaan, o segundo (sort.) indica que pretendemos uma lista ordenada com base no índice de modificação e o terceiro (minimum.value) permite filtrar a lista com base no valor mínimo

## Modelos de Mediação (Re-especificação) 
model.4 <- "
# regressões
fitness ~ a1*exercise
stress ~ a2*hardy
illness ~ b1*fitness + b2*stress + c1*exercise + c2*hardy

# covariações
fitness ~~ stress

ind1 := a1*b1
total1 := c1 + (a1*b1)

ind2 := a2*b2
total2 := c2 + (a2*b2)
"
fit.4 <- sem(model.4, data=DF_roth, meanstructure=T, se = "bootstrap", bootstrap = 100)
summary(fit.4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
parameterestimates(fit.4)[parameterEstimates(fit.4)$op=="~" | parameterEstimates(fit.4)$op==":=",] 
# se necessário intervalos de confiança
anova(fit.3, fit.4)
lavTestLRT(fit.3, fit.4)
compareFit(fit.3, fit.4, nested = TRUE) 
