
{if(!require(tidyverse)) install.packages('tidyverse')
  require(tidyverse)}

{if(!require(ipeadatar)) install.packages('ipeadatar')
  require(ipeadatar)}

{if(!require(forecast)) install.packages('forecast')
  require(forecast)}

{if(!require(tseries)) install.packages('tseries')
  require(tseries)}

{if(!require(fGarch)) install.packages('fGarch')
  require(fGarch)}

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
# Modelo GARCH
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# calculando o retorno da serie 
rt <- diff(data$value)/data$value[-length(data$value)]
rt.ts <- ts(rt, start = c(2003,2), frequency = 12) 

# plotando o retorno
autoplot(rt.ts)

# modelo
garch <- garchFit(~arma(4,2)+garch(2,2), data = rt.ts)

z <- garch@fit$series$z # estimação
x <- garch@fit$series$x # valor real
h <- garch@fit$series$h # nivel

x.ts <-ts(x, start = c(2003,2), frequency = 12)
z.ts <-ts(z, start = c(2003,2), frequency = 12) 

plot(x.ts, type = 'l')
lines(z.ts, col = 2)

# media do erro absoluto 
mean(abs(x-z))*100

# analise dos residuos 
checkresiduals(residuals(garch))

# teste para normalidade dos residuos 
jarque.bera.test(residuals(garch))
shapiro.test(residuals(garch))

# previsão do retorno
z.ts %>% forecast(h = 4)

# previsão vizualmente
z.ts %>% forecast(h = 4) %>% autoplot(xlim = c(2020,2023))

# Previsão do valor IBC-Br
#ago <- 149.17 # ultimo valor observado
sep <- 144.44 # ultimo valor observado
oct <- sep + 0.01028350 * sep
nov <- oct - 0.01219182 * oct
dez <- nov - 0.01591160 * nov
jan <- dez - 0.04006831 * dez

