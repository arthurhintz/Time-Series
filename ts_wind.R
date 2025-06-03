library(lubridate)

# Arthur directory
data_brasilia<-read.table("wind-brasilia.txt",header = T, sep = ";") # ler os dados


data_brasilia$Data <- as.Date(data_brasilia$Data)

data_brasilia$ano <- format(data_brasilia$Data, "%Y")

#==========/==========/==========/==========/==========/==========/==========/==========/
#Verificnado Tempo Faltante

# Criar sequência completa de meses no intervalo de datas
meses_completos <- seq.Date(from = floor_date(min(data_brasilia$Data, na.rm = TRUE), "month"), 
                            to = floor_date(max(data_brasilia$Data, na.rm = TRUE), "month"), 
                            by = "month")

# Identificar os mesmes da série
meses_dados <- unique(floor_date(data_brasilia$Data, "month"))

# Identificar os meses que estão ausentes
meses_faltando <- meses_completos[!meses_completos %in% meses_dados]

print(meses_faltando)

# Série Temporal Completa!
#==========/==========/==========/==========/==========/==========/==========/==========/
# Gráficos da Série
# Facetado por ano
ggplot(data_brasilia, aes(x = Data, y = VelocidadeVentoMedia)) +
  geom_line() +
  facet_wrap(~ano, scales = "free_x") +
  labs(title = "Graficos divididdos por Ano", x = "Data", y = "Valor")


plot.ts(data_brasilia$VelocidadeVentoMedia)

#==========/==========/==========/==========/==========/==========/==========/==========/

y_bra <- as.vector(data_brasilia[,4])
N_bra <- length(y_bra)

y <- y_bra[1:(N_bra-12)]
y <- ts(y,start=c(2001,01),frequency=12)

summary(y)
var(y)
hist(y)
monthplot(y)
acf(y)
pacf(y)

#==========/==========/==========/==========/==========/==========/==========/==========/
# As funções foram rodadas no arquivo ts.commodities
# Testes
resultado_tendencia <- tend_determ(y)
print(resultado_tendencia$Tabela)

resultado_raiz_unitaria <- raiz_unit(y)
print(resultado_raiz_unitaria$Tabela)

resultado_sazonalidade <- sazonalidade(y)
print(resultado_sazonalidade$Tabela)


#==========/==========/==========/==========/==========/==========/==========/==========/
# Devido a Tendência e Sazonalidade, ajustar Modelo ETS

#Modelos ETS 
library(fpp)

#----------------------------
#Modelo
mod <- ets(y)
summary(mod)

# Erro Aditivo, Sem Tendencia, Sazonalidade Aditiva
#----------------------------
#Grafico Previs?o +IC

autoplot(mod)+theme_minimal()
mod %>% forecast(h=12) %>%
  autoplot() +
  ylab("Velocidade Media")+
  theme_minimal()
#----------------------------
accuracy(mod)
mod.for<-forecast(mod)
autoplot(mod.for)+theme_minimal()
#----------------------------
#ComparaC'C#o Res?duos e erros de Previs?o
cbind('Residuals' = residuals(mod),
      'Forecast errors' = residuals(mod,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")+theme_minimal()
#----------------------------
#ResC-duo
res_ets<-residuals(mod)
#----------------------------
#AnC!lise dos Res?duos
ggtsdisplay(res_ets,plot.type="scatter", theme=theme_bw())
#----------------------------
#FAC dos Res?duos
ggAcf(res_ets, lag.max=100,type = c("correlation"))+labs(y = "FAC Amostral Res?duos SEH",title="")+
  theme_minimal()
#----------------------------
#QQ Plot dos Res?duos
ggqqplot(res_ets)+ggtitle("ResC-duo Modelo")
#----------------------------
#Densidade dos Res?duos
plot(density(res_ets),main="Random Error")
#----------------------------
#Teste de Normalidade dos Res?duos
shapiro.test(res_ets)
adf.test(res_ets)
#----------------------------
#Teste de CorrelaC'C5es
Box.test(res_ets,lag=10)
#----------------------------
hist(res_ets)
#==========/==========/==========/==========/==========/==========/==========/==========/
# Lixo RMD
# 
data <- data.frame(
  Tempo = as.Date(as.yearmon(time(serie_temporal))),
  Observada = as.numeric(serie_temporal),
  Ajustada = c(as.numeric(mod$fitted), rep(NA, length(b$mean))),
  Previsao = c(rep(NA, length(y)), as.numeric(b$mean))
)

# Criar o gráfico combinado
ggplot(data) +
  geom_line(aes(x = Tempo, y = Observada, color = "Série Observada"), size = 1) +
  geom_line(aes(x = Tempo, y = Ajustada, color = "Ajustada"), size = 1) +
  geom_line(aes(x = Tempo, y = Previsao, color = "Previsão"), size = 1) +
  geom_vline(
    xintercept = as.numeric(as.Date(as.yearmon(time(serie)[length(y)]))),
    color = "blue", linetype = "dotted", size = 1
  ) +
  labs(
    x = "Tempo",
    y = "Velocidade do Vento",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Série Observada" = "black", 
               "Ajustada" = "red", 
               "Previsão" = "blue")
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y"
  ) +
  tema_personalizado +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_blank()
  )


