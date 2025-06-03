# data
dados <- load("dados.RData")
summary(df)

d1<-ts(df,start = c(1800,01),frequency = 12)

# Gráfico da Série
forecast::autoplot(d1)+labs(x="Tempo (meses)",y="Valores")+theme_minimal()

# Gráfico da Função de Autocorrelação (FAC)
forecast::ggAcf(d1,lag.max = 100, type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

## Decaimento lento, indicando a possibilidade de raiz unitaria, longa dependencia
## Um indicativo de uma serie nao estacionaria

# Gráfico da Função de Autocorrelação Parcial (FACP)
forecast::ggAcf(d1,lag.max = 100,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()

plot(decompose(d1))

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

#raiz_unit(ts=d1)
raiz_unit(ts=d1)$Tabela

# Os tres testes apresentaram tendencia estocastica (raiz unitaria). Portanto,
# uma tendencia deterministica como identificado nos testes acima, pode estar
# sendo descrevido pela raiz unitaria presente na serie, como identificado no
# grafico da serie e pelo decompose. Um comportamento que ocila aleatoriamente
# ao longo dos anos, sem um padrao definido. 

#==========/==========/==========/==========/==========/==========/==========/==========/

diff_d1<-diff(d1, differences = 1)
forecast::autoplot(diff_d1)+theme_minimal()

forecast::ggAcf(diff_d1,lag.max = 100,type = c("correlation"))+labs(y = "FAC",title="")+
  theme_minimal()

forecast::ggAcf(diff_d1,lag.max = 100,type = c("partial"))+labs(y = "FACP",title="")+
  theme_minimal()


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


#Ajuste Tendência: ----------------------------------------------------------- #
n<-length(d1)
t<-c(1:n)
ff<-data.frame(y=d1,x1=t,x2=t^2)
a<-lm(d1~x1+x2,data=ff)
summary(a)
y<-a$coefficients[1]+a$coefficients[2]*t+a$coefficients[3]*t^2
plot.ts(cbind(d1,y),plot.type = c("single"),col=c("black","blue"),ylab="",xlab = "")
at<-d1-y
plot.ts(at,ylab="",xlab = "")
abline(h=mean(at),col="blue")
title(xlab = "Mês",main="",cex.lab=1,cex.main=1.5)
acf(at,lag.max=500,main="",ylab="FAC")


