# Pacotes utilizados --------------------------------------------------------- #

# install.packages("ggplot2")
library(ggplot2)

# Pacotes referenciados no codigo por "::", exp: readxl::read_excel()

# install.packages("readxl")
# install.packages("forecast")
# install.packages("randtests")
# install.packages("trend")
# install.packages("Kendall")
# install.packages("tseries")
# install.packages("seastests")
# install.packages("lmtest")
# install.packages("nortest")
# install.packages("ggpubr")
#
# library(readxl)
# library(forecast)
# library(randtests)
# library(trend)
# library(Kendall)
# library(tseries)
# library(seastests)
# library(lmtest)
# library(nortest)
# library(ggpubr)


# Link com series temporais:
# http://www.ipeadata.gov.br/Default.aspx

# Dados mensais ================================================================
# Leitura do banco de dados
dados<-readxl::read_excel("ipeadata[03-04-2024-01-55].xls")
y<-dados$`Exportações - preços - índice (média 2018 = 100) - - - Fundação Centro de Estudos do Comércio Exterior (Funcex) - FUNCEX12_XPT12 -`
min(dados$Data) # Data inicial
d1<-ts(y,start = c(1977,01),frequency = 12)# Mensal frequencia = 12


# Gráfico da Série
forecast::autoplot(d1)+labs(x="Tempo (meses)",y="Preços das exportações")+theme_minimal()

# Gráfico da Função de Autocorrelação (FAC)
forecast::ggAcf(d1,lag.max = 100, type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

## Decaimento lento, indicando a possibilidade de raiz unitaria, longa dependencia
## Um indicativo de uma serie nao estacionaria

# Gráfico da Função de Autocorrelação Parcial (FACP)
forecast::ggAcf(d1,lag.max = 100,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

## Dois lags significativos, ja dando uma ideia de um possivel modelo 
## autorregressivo de ordem 2 AR(2)

# analise descritiva 
summary(d1)
plot(decompose(d1))

# Divisão em 4 partes:
# 1 - Serie original (observada)
# 2 - tendencia: a tentativa de descrever uma tendencia para a serie, em que conseguimos
# olhar mais especificamente se existe um padrao visivel, neste caso, nao 
# conseguimos notar algo tao claramente, ja dando indicativo de raiz unitaria
# por comportamento da serie que ocila aleatoriamente  ao longo dos anos
# 3 - Sasonanidade: comportamento sazonal da serie, a tentativa de descever um comportamento.
# Mas olhando a serie não eh visto um comportamento sazonal neste caso, acabando
# em alguns momentos a funcao forcando uma sazonalidade. (sera idenficado no teste)
# 4 - Parte aleatória (residuos): representa as flutuações não explicadas pela 
# tendencia ou pela sazonalidade. Neste caso, podemos ver a diferença de 
# variancia apartir de apriximadamente 2008, dando um indicativo de volatilidade.

# Analise de tendencia deterministica: --------------------------------------- #
tend_determ<-function(ts){
  CS<-suppressWarnings(randtests::cox.stuart.test(ts,c("two.sided"))) #H0: NAO existe tendencia
  CeST<-suppressWarnings(trend::cs.test(ts)) #H0: NAO existe tendencia
  # Runs<-suppressWarnings(randtests::runs.test(ts)) #H0: NAO existe tendencia
  # WaldW<-suppressWarnings(trend::ww.test(ts)) #H0: NAO existe tendencia
  MannKT<-suppressWarnings(trend::mk.test(ts,continuity = TRUE)) #H0: a serie eh i.i.d. / NAO existe tendencia
  MannK<-suppressWarnings(Kendall::MannKendall(ts)) #H0: NAO existe tendencia
  KPSST<-suppressWarnings(tseries::kpss.test(ts, null = c("Trend"))) #H0: NAO existe tendencia
  #
  p_value<-c(CS$p.value,CeST$p.value,MannKT$p.value,MannK$sl,KPSST$p.value)
  p_value1<-p_value
  p_value1[p_value>=0.05]<-"NAO tendencia"
  p_value1[p_value<0.05]<-"Tendencia"
  tabela<-data.frame(Testes=c("Cox Stuart","Cox and Stuart Trend",
                              "Mann-Kendall Trend","Mann-Kendall","KPSS Test for Trend"),
                     H0=c(rep("NAO tendencia",5)),
                     p_valor=round(p_value,4),
                     Conclusao=c(p_value1))
  list(CS=CS,CeST=CeST,MannKT=MannKT,MannK=MannK,KPSST=KPSST,Tabela=tabela)
}

# Tendencia deterministica: Refere-se a mudanças de longo prazo na serie 
# temporal que seguem um padrao ou comportamento especifico e previsivel.
# Como por exemplo, uma tendencia crescente ou descrecente na serie.

#tend_determ(ts = d1)
tend_determ(ts = d1)$Tabela

# Os 5 testes apresentaram tendencia deterministica, a serie nao eh estacionária.

# Teste para raiz unitaria: -------------------------------------------------- #

raiz_unit<-function(ts){
  ADF<-suppressWarnings(tseries::adf.test(ts,alternative = c("stationary"))) #H0: raiz unitaria
  PP<-suppressWarnings(tseries::pp.test(ts,alternative = c("stationary"))) #H0: raiz unitaria
  KPSSL<-suppressWarnings(tseries::kpss.test(ts, null = c("Level"))) #H0: nao existe tendencia
  #
  p_value<-c(ADF$p.value,PP$p.value,KPSSL$p.value)
  p_value1<-p_value[1:2]
  p_value1[p_value[1:2]>=0.05]<-"Tendencia"
  p_value1[p_value[1:2]<0.05]<-"NAO tendencia"
  p_value2<-p_value[3]
  p_value2[p_value[3]>=0.05]<-"NAO tendencia"
  p_value2[p_value[3]<0.05]<-"Tendencia"
  tabela<-data.frame(Testes=c("Augmented Dickey-Fuller","Phillips-Perron Unit Root","KPSS Test for Level"),
                     H0=c(rep("Tendencia",2),"NAO tendencia"),
                     p_valor=round(p_value,4),
                     Conclusao=c(p_value1,p_value2))
  list(ADF=ADF,PP=PP,KPSSL=KPSSL,Tabela=tabela)
}

# Tendencia estocastica (raiz unitária): Refere-se a mudanças de longo prazo na 
# serie temporal que nao seguem um padrao previsivel ou deterministico.
# Aqui nao eh possivel identificar um comportamento padrao que nem no deterministico
# A serie apresenta ocilaçoes que nao conseguimos descrever, por seguirem de forma 
# aleatoria. Essa serie eh um exemplo disso, nao conseguimos descrever um 
# comportamento especifico ao longo do tempo.


#raiz_unit(ts=d1)
raiz_unit(ts=d1)$Tabela

# Os tres testes apresentaram tendencia estocastica (raiz unitaria). Portanto,
# uma tendencia deterministica como identificado nos testes acima, pode estar
# sendo descrevido pela raiz unitaria presente na serie, como identificado no
# grafico da serie e pelo decompose. Um comportamento que ocila aleatoriamente
# ao longo dos anos, sem um padrao definido. 

# Entao, devemos sempre que tiver raiz unitaria, remove-la primeiramente e 
# refazer todos os testes.

# Diferenciacao -------------------------------------------------------------- #

diff_d1<-diff(d1, differences = 1)
forecast::autoplot(diff_d1)+theme_minimal()

forecast::ggAcf(diff_d1,lag.max = 100,type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

## 3 legs significativos, indicando um modelo de medias moveis de ordem 3 (AR(3))
## ou 2 AR(2) devido ao grau de significancia do parametro.

forecast::ggAcf(diff_d1,lag.max = 100,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

## 2 legs significativos, indicando um modelo AR(2) ou AR(1) devido ao grau de 
## significancia do parametro.

## Os dois graficos FAC e FACP apresentaram comportamentos indicativos de 
## estacionariedade, não contendo mais um comportamento de decaimento.
## Desta forma, um modelo muito provavel seria um modelo ARMA(2,3) ou ARMA(2,2)
## ou ARMA (3,1) ou ARMA (1,2), conforme graus de significancia dos parametros.
## Esses parametros AR e MA serao definidos para descrever essa autocorrelacao,
## em outras palavras, retira-las, entao nos residuos do modelo, esperamos
## que a serie nao apresente autocorrelacao, nao tenham legs estatisticamente
## significativos, verificado pelo teste de autocorrelacao.

# Analise de tendencia deterministica (DIFF): -------------------------------- #

tend_determ(ts = diff_d1)$Tabela

## nao apresenta tendencia deterministica

# Teste para raiz unitaria (DIFF): ------------------------------------------- #

raiz_unit(ts= diff_d1)$Tabela

## nao apresenta raiz unitaria

# portanto, uma diferenciacao foi suficiente para tornar a serie estacionaria

# Sazonalidade --------------------------------------------------------------- #

sazonalidade<-function(ts,diff=0,freq){
  KrusW<-suppressWarnings(seastests::kw((ts),diff = diff, freq=12)) #H0: NAO Sazonal
  Fried<-suppressWarnings(seastests::fried((ts),diff = diff, freq=12)) #H0: NAO Sazonal
  #
  p_value<-c(KrusW$Pval,Fried$Pval)
  p_value1<-p_value
  p_value1[p_value>=0.05]<-"NAO Sazonal"
  p_value1[p_value<0.05]<-"Sazonal"
  tabela<-data.frame(Testes=c("Kruskall Wallis","Friedman rank"),
                     H0=c(rep("NAO Sazonal",2)),
                     p_valor=round(p_value,4),
                     Conclusao=c(p_value1))
  list(KrusW=KrusW,Fried=Fried,Tabela=tabela)
}

# Como a serie tornou-se estacionaria com uma diferenciacao, testaremos a 
# sazonalidade por meio dessa diferença, definindo no teste diff = 1, com a serie original (d1)
# caso a serie trabalhada fosse a original sem precisar realizar diferença
# para tornar estacionaria, usariamos diff=0

# sazonalidade(ts = d1,diff = 1,freq = 12)
sazonalidade(ts = d1,diff = 1,freq = 12)$Tabela

# A serie não apresenta sazonalidade.

## Portanto devemos buscar um modelo proximo a um ARIMA(3,1,2)
## a funcao auto.arima do pacote forecast faz essa busca aleatoriamente,
## mas devemos ter o cuidado de as especificacoes do modelo estao sendo
## cumpridas.

# Modelo ARIMA --------------------------------------------------------------- #

mod<-forecast::auto.arima(d1)
summary(mod)
lmtest::coeftest(mod)

## Neste caso, o auto.arima definiu um modelo ARIMA(2,1,2)(1,0,0)[12],
## em que a sazonalidade nao é significativa visto pela funcao coeftest(mod)
## Entao devemos ajustar isso na busca, colocando seasonal = F

mod<-forecast::auto.arima(d1,seasonal = F)
summary(mod)
lmtest::coeftest(mod)

# Agora sim, temos um modelo ARIMA(2,1,2) em que todos os parametros sao significativos
# E tambem a diferenciacao foi especificada em 1 diferença.
# Como foi visto nos graficos de FAC e FACP da serie com uma deferenciaçao
# o modelo esta cumprindo o que foi visualizado, com paremetros de ordem
# similares ao significativos nos graficos.

# Outro metodo eh uma busca manual pela funcao arima(), definindo a ordem
# do modelo em order = c(p,d,q). 
# mod<-arima(d1,order = c(2,1,2))

# ---------------------------------------------------------------------------- #

# Seguindo com o modelo do auto.arima() denifinido sem sazonalidade ARIMA(2,1,2)

summary(mod) # Conseguimos ver alguns criterios do modelo AIC, erros, os coeficientes.
confint(mod,level=0.95) # intervalo de confianca

# Coeficientes do modelo
lmtest::coeftest(mod)

#Verificacao das Suposições do Modelo
forecast::autoplot(mod)+ggtitle("Dados")+ xlab("Time") + ylab("Valores $")+theme_minimal()

# Inverso do circulo unitario do modelo, esperamos que estejam dentro do circulo
# por ser o inverso.

#Analise de Residuos: -------------------------------------------------------- #
res.mod<-mod$residuals
forecast::ggtsdisplay(res.mod)
## Tanto o FAC(ACF) e o FACP(PACF) apresentam legs dentro das bandas, apenas alguns
## legs significativos, fora das bandas de confianca, porem nao aparentam ser relevantes
## por estarem proximo dos limites, mas o teste de Ljung-Box confirmara isso..

Box.test(res.mod,lag = 15, type="Ljung") # H0: NAO Autocorrelacionado 
# P-valor maior do que 0.05, portanto nao rejeitamos a hipotese nula
# Os residuos nao apresentam autocorrelacao

nortest::ad.test(res.mod) # H0: Normalidade
# P-valor menor do que 0.05, portanto rejeitamos a hipotese nula
# Os residuos nao apresentam normalidadee

raiz_unit(ts = res.mod)$Tabela
# Os residuos nao apresentam raiz unitaria, sao estacionarios

ggpubr::ggqqplot(res.mod)

# Desconsiderando a normalidade, que nao eh tao relevante nesse caso.
# O modelo esta bem especificado, podemos realizar a previsao.
# Um dos possiveis motivos de nao ter apresentado normalidade eh pelo fato
# dos dados apresentarem uma mudança de variancia por volta de 2008,
# dando um indicativo de volatilidade, como visto no decompose no residuo. 
# Teriamos que usar modelos volateis (ARCH, GARCH, ...). Mas aqui, manteremos
# com o modelo ARIMA.

# Previsao ------------------------------------------------------------------- #
predict.mod<-forecast::forecast(mod,h=12)
predict.mod

forecast::autoplot(predict.mod) + forecast::autolayer(predict.mod$fitted,show.legend = F) + theme_minimal()


