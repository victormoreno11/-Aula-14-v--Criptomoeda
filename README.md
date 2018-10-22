# -Aula-14-v--Criptomoeda

                    #Aula 14 - Quebra Estrural e Bolhas

install.packages("strucchange")

library(strucchange)
library(readxl)


BITCOIN <- read_excel("C:/Econometria/Bitcoin.xls")

Criptomoeda <- ts(criptomoeda$Fechamento, start = 2016, frequency = 365)

plot(Criptomoeda)

#Teste de Chow

chow <- Fstats(Criptomoeda~1)    #Executa o Teste de F de Chow
sctest(chow)                 #Retorna a Estatística de Teste e o p-valor

plot(Criptomoeda)
lines(breakpoints(chow))

plot(chow)
    
#Teste Bai Perron

bp_ts <- breakpoints(Criptomoeda ~ 1)

bp_ts

summary(bp_ts)

#ci_ts <- confint(bp_ts)

plot(Bitcoin)               
lines(bp_ts)            #Gráfico com os breakpoints


#Gráfico com as linhas de tendências para os três períodos

fm0 <- lm(Criptomoeda ~ 1)
fm1 <- lm(Criptomoeda ~ breakfactor(bp_ts, breaks = 1))
fm2 <- lm(Criptomoeda ~ breakfactor(bp_ts, breaks = 2))
fm3 <- lm(Criptomoeda ~ breakfactor(bp_ts, breaks = 3))
fm4 <- lm(Criptomoeda ~ breakfactor(bp_ts, breaks = 4))
fm5 <- lm(Criptomoeda ~ breakfactor(bp_ts, breaks = 5))

plot(Criptomoeda)
lines(ts(fitted(fm0), start = 2014, freq=365), col = 3)
lines(ts(fitted(fm1), start = 2014, frequency=365), col = 4)
lines(ts(fitted(fm2), start = 2014, frequency=365), col = 1)
lines(ts(fitted(fm3), start = 2014, frequency=365), col = 1)
lines(ts(fitted(fm4), start = 2014, frequency=365), col = 1)
lines(ts(fitted(fm5), start = 2014, frequency=365), col = 1)
lines(bp_ts)


#Estimar o Melhor Modelo ARIMA

#Modelo Integrado de Ordem 1

MIO1 <- diff(Criptomoeda)
plot(MIO1)
