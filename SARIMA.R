
{if(!require(tidyverse)) install.packages('tidyverse')
  require(tidyverse)}

{if(!require(ipeadatar)) install.packages('ipeadatar')
  require(ipeadatar)}

{if(!require(forecast)) install.packages('forecast')
  require(forecast)}

{if(!require(tseries)) install.packages('tseries')
  require(tseries)}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Importando os Dados
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

code <- available_series(language = 'br')

# O Índice de Atividade Econômica do Banco Central – Brasil (IBC-Br)

# SGS12_IBCBR12 IBC-Br - índice real (2002=100)

data  <- ipeadata("SGS12_IBCBR12" , "br" )
#names(data)

bd.ts <- ts(data = data$value, start = c(2003,1), frequency = 12) 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Modelo SARIMA
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Plotando a serie
autoplot(bd.ts)

# Autocorrelacao 
ggAcf(bd.ts)
ggPacf(bd.ts)

# teste de estacionaridade
kpss.test(bd.ts)
adf.test(bd.ts)
pp.test(bd.ts)

# modelo
md1 <- Arima(bd.ts, order = c(2,1,2), seasonal = list(order=c(1,1,2),period=12))

# Plotando a serie e o modelo
plot(bd.ts, type = 'l')
lines(md1$fitted, col = 2)

# resumo do modelo da pra olhar o MAPE
summary(md1)

# analise dos residuos 
checkresiduals(md1$residuals)

# teste para normalidade dos residuos 
jarque.bera.test(residuals(md1))
shapiro.test(residuals(md1))

# previsão
md1 %>% forecast(h = 4)

# previsão vizualmente
md1 %>% forecast(h = 4) %>% autoplot(xlim = c(2020,2023))


