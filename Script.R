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