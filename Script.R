#/////////////////////CLASES ANTERIORES///////////////////////////
#Generar los valores de un modelo AR(1)
w = rnorm(550,0,1) # 50 extra to avoid startup problems
y = filter(w, filter=0.9, method="recursive")[-(1:50)]
plot.ts(y, main="autoregression")
t = filter(w, filter=-0.9, method="recursive")[-(1:50)]
plot.ts(t, main="autoregression")

#Generar el gráfico de un modelo AR(1)
ma.1=arima.sim(list(order=c(0,0,1), ma=0.9), n=120)
acf(ma.1)
acf(ma.1,ci.type='ma',xaxp=c(0,20,10))
plot(ma.1, ylab="x",type="o",
     main=(expression(AR(1)~~~theta==0.9)))
ma.1=arima.sim(list(order=c(0,0,1), ma=-0.9), n=120)
acf(ma.1)
acf(ma.1,ci.type='ma',xaxp=c(0,20,10))
plot(modelo2, ylab="x",type="o",
     main=(expression(AR(1)~~~theta==-0.9)))

#Todo modelo no causal tiene una contraparte causal
w = rnorm(250,0,1) # 50 extra to avoid startup problems
y = filter(w, filter=2, method="recursive")[-(1:50)]
plot.ts(y, main="autoregression")
r = rnorm(250,0,1/4) # 50 extra to avoid startup problems
p = filter(r, filter=1/2, method="recursive")[-(1:50)]
plot.ts(p, main="autoregression")

#Generar el gráfico de un modelo AM(1)
plot(arima.sim(list(order=c(0,0,1), ma=0.5), n=100), type="o",ylab="x",
     main=(expression(MA(1)~~~theta==+0.5)))
plot(arima.sim(list(order=c(0,0,1), ma=-0.5), n=100),type="o", ylab="x",
     main=(expression(MA(1)~~~theta==-0.5)))

#******************************************************************
y=read.table("TS.csv",header=TRUE,sep=",")
y
w=ts(y$zt,start=1,end=4,deltat=4/12,names="Observaciones")
w=ts(y$zt,start=1,end=4,frequency=3,names="Observaciones")
w
#Note que solo debe introducir un parametro entre frequency y deltat.
#**************Obtener las frecuencias de autocorrelación y 
#autocorrelaciónparcial
acf(w)
pacf(w)

#Ejemplo
y=read.table("Bases/m-aaa-1911.txt",header=TRUE)
y
w=ts(y$yield,start=c(1,1),end=c(93,11),frequency = 12,names="Observaciones")
w
#/////////////////////////////////////////////////////////////////



#/////////////////SIMULACIONES///////////////////////////////////////
library("TSA", lib.loc="~/R/win-library/3.2") 
#***************Simulación MA(1)****************
#theta=0.9

data(ma1.1.s)
acf(ma1.1.s,xaxp=c(0,20,10))
pacf(ma1.1.s,xaxp=c(0,20,10))
#Cotas alternativas  
acf(ma1.1.s,ci.type='ma',xaxp=c(0,20,10))  
#theta=-0.9
data(ma1.2.s)
acf(ma1.2.s,xaxp=c(0,20,10))
pacf(ma1.2.s,xaxp=c(0,20,10))
#**********Simulación para MA(2)****************
#θ1 = 1 y θ2 = −0.6
data(ma2.s)
acf(ma2.s,xaxp=c(0,20,10))
#Cotas alternativas
acf(ma2.s,ci.type='ma',xaxp=c(0,20,10))

#######Simulación para AR(1)######################
#φ = 0.9
data(ar1.s)
acf(ar1.s,xaxp=c(0,20,10))
pacf(ar1.s,xaxp=c(0,20,10))    

#######Simulación para AR(2)######################
#φ1 = 1.5 and φ2 = −0.75
data(ar2.s)
acf(ar2.s,xaxp=c(0,20,10))
pacf(ar2.s,xaxp=c(0,20,10))

#######Simulación para ARMA(1,1)######################
#φ = 0.6 and θ = −0.3.
data(arma11.s)
plot(arma11.s, type='o',ylab=expression(Y[t]))
acf(arma11.s,xaxp=c(0,20,10))
pacf(arma11.s,xaxp=c(0,20,10))
eacf(arma11.s)
#///////////////////////////////////////////////////////////////////


#/////////////////////IMA(2,2)/////////////////////////////////////
data(ima22.s)
plot(ima22.s,ylab='IMA(2,2) Simulation',type='o')
#primera diferencia de la serie IMA(2,2)
plot(diff(ima22.s),ylab='First Difference',type='o')
#segunda diferencia de la serie IMA(2,2)
plot(diff(ima22.s,difference=2),ylab='DifferencedTwice',type='o')
#//////////////////////////////////////////////////////////////////




#//////////////////// OIL PRICES //////////////////////////////////
base=read.table("oil.price.dat",header = TRUE)
serieOP=ts(base,start = c(1986,1),end=c(2006,1),frequency = 12)
plot(serieOP,ylab = "Precio por barril",xlab = "Año",main="Precio
     mensual de la gasolina: Enero 1986-Enero 2006")
*****************Obteniendo el logaritmo de la variable***************
log_seriOP=log(serieOP)
plot(log_seriOP,ylab = "log(Precio por barril)",xlab = "Año",main="Precio
     mensual de la gasolina: Enero 1986-Enero 2006")
****************Primera diferencia de la variable********************
dif_log_serieOP=diff(log_seriOP)
plot(dif_log_serieOP,ylab = "log(Precio por barril)",xlab = "Año",main="Precio
     mensual de la gasolina: Enero 1986-Enero 2006")
****************Segunda diferencia de la variable *******************
dif2serieOP=diff(dif_log_serieOP)
plot(dif2serieOP)
#///////////////////////////////////////////////////////////////////





#///////////////// ELECTRICITY //////////////////////////////////////
base2=read.table("electricity.dat",header = TRUE)   
serieELCT=ts(base2,start = c(1973,1), end = c(2005,12),frequency = 12)
plot(serieELCT,ylab = "Kilowats hora", xlab = "Años",
     main ="Generación de electricidad(En millones de kilowats hora)")
log_serieELCT=log(serieELCT)
plot(log_serieELCT)
dif_log_seriesELCT=diff(log_serieELCT)
plot(dif_log_seriesELCT)
#///////////////////////////////////////////////////////////////////



#/////////////ESPECIFICACIONES DEL MODELO PARA LA SERIE OIL/////////
data(oil.price)
acf(as.vector(oil.price),xaxp=c(0,24,12))
#si quieren agregar su nombre agregar el parámetro "main"
pacf(as.vector(oil.price),xaxp=c(0,24,12))
#Las diferencia de los logaritmos de la serie
acf(diff(as.vector(log(oil.price))),xaxp=c(0,24,12))
pacf(diff(as.vector(log(oil.price))),xaxp=c(0,24,12))
#//////////////////////////////////////////////////////////////////

#///////////////////////Sobre Diferenciación//////////////////////
data(rwalk)
#Dos diferencias
acf(diff(rwalk,difference=2),ci.type='ma', xaxp=c(0,18,9))
#Una diferencia
acf(diff(rwalk),ci.type='ma',xaxp=c(0,18,9))
#//////////////////////////////////////////////////////////////


#///////////////The Dickey-Fuller Unit-Root Test/////////////////

library(uroot)
data(rwalk)
adf.test(rwalk)
ADF.test(rwalk,selectlags=list(mode=c(1,2,3,4,5,6,7,8),Pmax=8),
         itsd=c(1,0,0))
ADF.test(rwalk,selectlags=list(mode=c(1,2,3,4,5,6,7,8),Pmax=8),
         itsd=c(1,1,0))
ADF.test(rwalk,selectlags=list(Pmax=0),itsd=c(1,1,0))
#//////////////////////////////////////////////////////////////



#////BIC para simulacion de Yt = 0.8Y_(t−12) + e_t + 0.7e_(t−12)////
set.seed(92397)
test=arima.sim(model=list(ar=c(rep(0,11),.8),
                            ma=c(rep(0,11),0.7)),n=120)
res=armasubsets(y=test,nar=14,nma=14,y.name='test',
                  ar.method='ols')
plot(res)
#///////////////////////////////////////////////////////////////////////////////////



#/////////////////////////Especificación del modelo para larain/////
data(larain)
plot(larain,ylab='Inches',xlab='Year',type='o')
qqnorm(larain)
qqline(larain)
qqnorm(log(larain))
qqline(log(larain))
acf(log(larain),xaxp=c(0,20,10))
pacf(log(larain),xaxp=c(0,20,10))
#///////////////////////////////////////////////////////////////////


#////////Especificación del modelo para propiedades del color///////

data(color)
plot(color,ylab='Color Property',xlab='Batch',type='o')
plot(y=color,x=zlag(color),ylab='Color Property',
     xlab='Previous Batch Color Property')
acf(color,ci.type='ma')
acf(color)
pacf(color)

#///////////////////////////////////////////////////////////////////





#////////Especificación The Annual Abundance of Canadian hare///////
data(hare)
BoxCox.ar(hare)
acf(hare^.5)
pacf(hare^.5)
#///////////////////////////////////////////////////////////////////



#////////////////Especificando el modelo para precios de gasolina///////////
data(oil.price)
adf.test(oil.price)
adf.test(log(oil.price))
adf.test(diff(log(oil.price)))
eacf(diff(log(oil.price)))
res=armasubsets(y=diff(log(oil.price)),nar=7,nma=7,
                y.name='test', ar.method='ols')
plot(res)
acf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))
pacf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))
#/////////////////////////////////////////////////////////////////


#/////////////////////////////////////////////////////////////////

#/////////Especificación para la serie "Número de nuevos peces" ///////
library("astsa")
plot(rec, ylab="", xlab="", main="Recruitment")
plot(log(rec))
plot(diff(rec))
plot(diff(log(rec)))
adf.test(rec)
adf.test(diff(log(rec)))
acf(rec,ci.type="ma")
acf(rec,xaxp=c(0,22,11))
pacf(rec,xaxp=c(0,22,11))
eacf(rec)
res=armasubsets(y=rec,nar=7,nma=7,
                y.name='test', ar.method='ols')
plot(res)

#///////////////////////////////////////////////////////////////////


#//Especificación para la serie Producto nacional bruto de EE.UU.//
data(gnp)
plot(gnp)
acf(gnp,ci.type="ma")
adf.test(gnp)
BoxCox.ar(gnp)
plot(diff(gnp))
plot(diff(log(gnp)))
adf.test(diff(gnp))
acf(diff(gnp),ci.type="ma")
pacf(diff(gnp))
eacf(diff(gnp))
res=armasubsets(y=rec,nar=7,nma=7,
                y.name='test', ar.method='ols')
plot(res)
#////////////////////////////////////////////////////////////////

#///////////////Serie de devoluciones//////////////////////////////////
retorno=ts(m.ibm3dx2608$ewrtn,start = c(1926,1),end = c(2008,12),frequency = 12)
plot(retorno)
BoxCox.ar(retorno)
adf.test(retorno)
acf(retorno,ci.type="ma")
pacf(retorno)
eacf(retorno)
res=armasubsets(y=rec,nar=7,nma=7,
                y.name='test', ar.method='ols')
plot(res)

#//////////////////////////////////////////////////////////////////





#///////////////////ESTIMACIÓN DE PARAMETROS///////////////////////
#**********************Método de los momentos*********************
estimate.ma1.mom=function(x){r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) 
    return((-1+sqrt(1-4*r^2))/(2*r)) else return(NA)}
set.seed(1234)
ma1.3.s=arima.sim(list(ma=c(.9)),n=60)
estimate.ma1.mom(ma1.3.s)
ma1.4.s=arima.sim(list(ma=c(-0.5)),n=60) 
estimate.ma1.mom(ma1.4.s)
data(ma1.2.s); data(ma1.1.s) #data(ma1.3.s); data(ma1.4.s)
estimate.ma1.mom(ma1.2.s); estimate.ma1.mom(ma1.1.s)
estimate.ma1.mom(ma1.3.s); estimate.ma1.mom(ma1.4.s)
arima(ma1.4.s,order=c(0,0,1),method='CSS',include.mean=F)
data(ar1.s); data(ar1.2.s)
ar(ar1.s,order.max=1,AIC=F,method='yw')
ar(ar1.2.s,order.max=1,AIC=F,method='yw')
data(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw')
#*****************************************************************

#***********Ilustración de la estimación de parametros************
data(ar1.s); data(ar1.2.s)
ar(ar1.s,order.max=1,AIC=F,method='yw')
ar(ar1.s,order.max=1,AIC=F,method='ols')
ar(ar1.s,order.max=1,AIC=F,method='mle')
ar(ar1.2.s,order.max=1,AIC=F,method='yw')
ar(ar1.2.s,order.max=1,AIC=F,method='ols')
ar(ar1.2.s,order.max=1,AIC=F,method='mle')


data(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw')
ar(ar2.s,order.max=2,AIC=F,method='ols')
ar(ar2.s,order.max=2,AIC=F,method='mle')


data(arma11.s)
arima(arma11.s, order=c(1,0,1),method='CSS')
arima(arma11.s, order=c(1,0,1),method='ML')


data(color)
ar(color,order.max=1,AIC=F,method='yw')
ar(color,order.max=1,AIC=F,method='ols')
ar(color,order.max=1,AIC=F,method='mle')


data(hare)
arima(sqrt(hare),order=c(3,0,0))


data(oil.price)
arima(log(oil.price),order=c(0,1,1),method='CSS')
arima(log(oil.price),order=c(0,1,1),method='ML')

#*****************************************************************
#//////////////////////////////////////////////////////////////////


#////////////////////DIAGNOSTICO DEL MODELO///////////////////////
#*********************Análisis de los residuos*******************
win.graph(width=4.875,height=3,pointsize=8)
data(color)
m1.color=arima(color,order=c(1,0,0)); m1.color
plot(rstandard(m1.color),ylab ='Standardized Residuals',
       type='o'); abline(h=0)



data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0)); m1.hare
m2.hare=arima(sqrt(hare),order=c(3,0,0),fixed=c(NA,0,NA,NA))
m2.hare
# Note that the intercept term given in R is actually the mean
    in the centered form of the ARMA model; that is, if
y(t)=sqrt(hare)-intercept, then the model is
y(t)=0.919*y(t-1)-0.5313*y(t-3)+e(t)
# So the 'true' intercept equals 5.6889*(1-0.919+0.5313)=3.483
    > plot(rstandard(m2.hare),ylab='Standardized Residuals',type='o')
abline(h=0)


data(oil.price)
 m1.oil=arima(log(oil.price),order=c(0,1,1))
plot(rstandard(m1.oil),ylab='Standardized residuals',type='l')
abline(h=0)
#****************************************************************


#***********************NORMALIDAD DE LOS RESIDUOS****************
win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(residuals(m1.color)); qqline(residuals(m1.color))

qqnorm(residuals(m1.hare)); qqline(residuals(m1.hare))

qqnorm(residuals(m1.oil)); qqline(residuals(m1.oil))
#*****************************************************************



