### Alumni Coppead - DNS3
### Disciplina: S?ries Temporais
### Dupla: Camila Campos e Matheus Cadorini

# Preparando diret?rio de trabalho

path <- 'C:/Users/cador/OneDrive/?rea de Trabalho/Documentos COPPEAD/S?ries Temporais'
setwd(path)
getwd()

# Importando bibliotecas

library(devtools)
library(TSA)
library(RGraphics)
library(grDevices)
library(xts)
library(devtools)
library(tseries)
library(forecast)

# Importando bibliotecas de visualiza??o gr?fica

library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(hrbrthemes)

# Leitura da base de dados 

bd_fretes <- read.csv2("GN.csv")
dset <- data.frame(bd_fretes)
dset <- dset[-c(513),]
View(dset)

# An?lise explorat?ria inicial

dim(bd_fretes)
str(bd_fretes)
head(bd_fretes)
summary(bd_fretes)
objects(bd_fretes)
complete.cases(bd_fretes)
which(complete.cases(bd_fretes)==FALSE)
which(is.na(bd_fretes))
colnames(bd_fretes)

# Cria??o da S?rie Temporal - per?odo completo

ano <- 1979
mes <- 1
tituloy <- "Qtd Barris/dia (Milh?es)"
titulop <- "Produ??o de Derivados de Petr?leo - G?s Natural Jan/79 - Ago/21"

# Dataset de GN contendo apenas a coluna com a venda de barris de petr?leo por dia

GN <- dset[2]
GN <- ts(GN, start = c(ano, mes), frequency = 12)

#-------------------------------------------------------------------------------

#Plot s?rie temporal

par(mai = c(0.9,0.9,0.2,0.2), omi = c(0.2,0.2,0.2,0.2))
plot.ts(GN, ylab=tituloy, xlab="Ano", plot.type = c("single"), las = 1, col = c("blue"), lwd = 2, lty=c(1:3), main= titulop)

decomposedRes <- decompose(GN, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below

#-------------------------------------------------------------------------------

# Testes de estacionariedade

kmax <- trunc(12*(nrow(GN)/100)**(1/4))  # Determina??o do lag m?ximo atrav?s da "regra de ouro"
acf(GN, lag.max = kmax, main = "")
pacf(GN, lag.max = kmax, main = "")

#ACF indica s?rie n?o estacion?ria. PACF cai bastante depois da 1a defasagem.

# Teste da raiz unit?ria - ADF test (S?rie estacion?ria: p-value = 0.01)

adf.test(GN,k =12)

# Resultado: Indicativo de s?rie n?o estacion?ria: p-value = 0.88

# Teste de n?vel/tend?ncia - KPSS test (S?rie estacion?ria: p-value: 0.1)

kpss.test(GN, null = "Level", lshort=FALSE)
kpss.test(GN, null = "Trend", lshort=FALSE)

# Resultado: Indicativo de s?rie n?o estacion?ria: p-value = 0.01

#-------------------------------------------------------------------------------

## Identifica??o da s?rie temporal

# Teste Modelo ARIMA (p = 0, q = 1, d = 1)

arimamodel <- Arima(GN, order=c(0,1,0))
arimamodel

adf.test(arimamodel$residuals, k=12)
# p-value = 0.01
kpss.test(arimamodel$residuals, null = "Level", lshort=FALSE)
# p-value = 0.1
kpss.test(arimamodel$residuals, null = "Trend", lshort=FALSE)
# p-value = 0.1

acf(arimamodel$residuals, lag.max=kmax, main="")
pacf(arimamodel$residuals, lag.max=kmax, main="")
# AIC = 3302.8 AICc = 3302.8 BIC = 3307.1

# Teste Modelo ARIMA (p = 1, q = 1, d = 1 )

arimamodel <- Arima(GN, order = c(1,1,1), include.drift = TRUE)
arimamodel

adf.test(arimamodel$residuals, k =12)
# p-value: 0.01
kpss.test(arimamodel$residuals, null = "Level", lshort = FALSE)
# p-value: 0.0434
kpss.test(arimamodel$residuals, null = "Trend", lshort = FALSE)
# p-value: 0.1

acf(arimamodel$residuals, lag.max = kmax, main = "")
pacf(arimamodel$residuals, lag.max = kmax, main = "")
# AIC = 3220.2 AICc = 3220.2 BIC = 3232.9

# Teste Modelo ARIMA (p = 1, q = 2, d = 1 )

arimamodel <- Arima(bds, order=c(1,2,1))
arimamodel

adf.test(arimamodel$residuals, k=12)
# p-value 0.01
kpss.test(arimamodel$residuals, null = "Level", lshort=FALSE)
# p-value 0.0434
kpss.test(arimamodel$residuals, null = "Trend", lshort=FALSE)
# p-value 0.1

acf(arimamodel$residuals, lag.max=kmax, main="")
pacf(arimamodel$residuals, lag.max=kmax, main="")
# AIC=3220.2   AICc=3220.25   BIC=3232.91

# Teste Modelo ARIMA (p=2, q = 1, d = 1)

arimamodel <- Arima(GN, order = c(2,1,1), include.drift = TRUE)
arimamodel
adf.test(arimamodel$residuals, k =12)
# p-value = 0.01
kpss.test(arimamodel$residuals, null = "Level", lshort = FALSE)
# p-value = 0.0466 
kpss.test(arimamodel$residuals, null = "Trend", lshort = FALSE)
# p-value = 0.1

acf(arimamodel$residuals, lag.max = kmax, main = "")
pacf(arimamodel$residuals, lag.max = kmax, main = "")
# AIC = 3221.8 AICc = 3221.8 # BIC = 3238.7 

# Teste Modelo ARIMA (p = 1, q = 1, d = 2)

arimamodel <- Arima(GN, order = c(1,1,2))
arimamodel

adf.test(arimamodel$residuals, k =12)
# p-value = 0.01
kpss.test(arimamodel$residuals, null = "Level", lshort = FALSE)
# p-value = 0.0469 
kpss.test(arimamodel$residuals, null = "Trend", lshort = FALSE)
# p-value = 0.1

acf(arimamodel$residuals, lag.max = kmax, main = "")
pacf(arimamodel$residuals, lag.max = kmax, main = "")
# AIC=3221.02   AICc=3221.1   BIC=3237.97

# Teste Modelo ARIMA(p = 0, q = 1, d = 2)

arimamodel <- Arima(GN, order = c(0,1,2))
arimamodel

adf.test(arimamodel$residuals, k =12)
# p-value = 0.01
kpss.test(arimamodel$residuals, null = "Level", lshort = FALSE)
# p-value = 0.044 
kpss.test(arimamodel$residuals, null = "Trend", lshort = FALSE)
# p-value = 0.1

acf(arimamodel$residuals, lag.max = kmax, main = "")
pacf(arimamodel$residuals, lag.max = kmax, main = "")
# AIC=3219.77   AICc=3219.82   BIC=3232.48

# Teste Modelo ARIMA (p=1, q =2, d = 2)

arimamodel <- Arima(GN, order = c(1,2,2))
arimamodel

adf.test(arimamodel$residuals, k = 12)
# p-value = 0.01 
kpss.test(arimamodel$residuals, null = "Level", lshort = FALSE)
# p-value = 0.1 
kpss.test(arimamodel$residuals, null = "Trend", lshort = FALSE)
# p-value = 0.1

acf(arimamodel$residuals, lag.max = kmax, main = "")
pacf(arimamodel$residuals, lag.max = kmax, main = "")
#AIC=3176.69   AICc=3176.77   BIC=3193.63

# Teste Modelo ARIMA (p=2, q=2, d =2)

arimamodel <- Arima(GN, order = c(2,2,2))
arimamodel

adf.test(arimamodel$residuals, k = 12)
# p-value = 0.01 
kpss.test(arimamodel$residuals, null = "Level", lshort = FALSE)
# p-value = 0.1 
kpss.test(arimamodel$residuals, null = "Trend", lshort = FALSE)
# p-value = 0.1

acf(arimamodel$residuals, lag.max = kmax, main = "")
pacf(arimamodel$residuals, lag.max = kmax, main = "")
# AIC=3176.66   AICc=3176.78   BIC=3197.83

#-------------------------------------------------------------------------------

#AutoArima
mod.arima <- auto.arima(GN, seasonal = FALSE)
summary(mod.arima)
#resultado 1,1,1

#-------------------------------------------------------------------------------

## Divis?o da s?rie em treino e teste

# Fun??o de previs?o

## Produ??o de Derivados de Petr?leo - G?s Natural 

ano <- 1979
mes <- 1
tituloy <- "Qtd Barris/dia (Milh?es)"
titulop <- "Produ??o de Derivados de Petr?leo - G?s Natural Jan/79 - Ago/21"
ptreino = 60
pteste = 12

# Leitura da base de dados
  
bd_fretes <- read.csv2("GN.csv")
dset <- data.frame(bd_fretes)
dset <- dset[-c(513),]          # retirada da ?ltima linha 


# Transforma??o em s?rie temporal 

GN <- dset[2]
GN <- data.frame(GN)
GN <- ts(GN, start = c(ano, mes), frequency = 12)
View(GN)

# Cria??o do arquivo de sa?da

max <- nrow(GN) - ptreino - pteste
prev <- matrix(0,ncol = 4, nrow = max)
colnames(prev) <- c("Erro_treino", "Erro_teste", "Eabsp_tr", "Eabsp_ts")

for (index in 1:max)
{
  # Posi??o do ?ndice final
  fim <- ptreino + index - 1
  
  # Estabelecimento de um modelo arima para s?rie com per?odo de treino
  arima_model <- auto.arima(GN[index:fim,], D = 1)
  
  # Previs?o para o modelo arima para s?rie com in?cio em index e fim
  # com per?odo de treino
  fc <- forecast(GN[index:fim], h = pteste, model = arima_model)
  
  # C?lculo das estat?sticas
  
  # Erro da amostra treino
  prev[index,1] <- sqrt(mean(fc$residuals**2))
  
  # Erro da amostra teste
  prev[index,2] <- sqrt(mean((fc$mean - GN[eval(fim+1):eval(fim+pteste),])**2))
  
  # M?dulo do erro percentual da amostra treino
  
  prev[index,3] <- mean(abs(fc$residual/fc$x))
  
  # M?dulo do erro percentual da amostra Teste
  prev[index,4] <- mean(abs((fc$mean - GN[eval(fim+1):eval(fim+pteste),])/ GN[eval(fim+1):eval(fim+pteste),]))

}

arqprev <- ts(prev, start = c(eval(ano+trunc(ptreino/12)+1), mes), frequency = 12)

plot.ts(arqprev[,1], ylab = "Erro Treino", main = "Desempenho amostra treino no tempo")
plot.ts(arqprev[,2], ylab = "Erro Treino", main = "Desempenho amostra treino no tempo")
plot.ts(arqprev[,3], ylab = "Erro Treino", main = "Desempenho amostra treino no tempo")
plot.ts(arqprev[,4], ylab = "Erro Treino", main = "Desempenho amostra teste no tempo")

plot(Erro_treino~Erro_teste, data = arqprev, ylim = c(0,50), xlim = c(0,50),
    main = "Diagrama de Dispers?o Treino vs Teste")

plot(Eabsp_tr~Eabsp_ts, data = arqprev, ylim = c(0,0.5), xlim = c(0,0.5), 
     main = "Diagrama de Dispers?o Treino vs Teste")
               
  
  
