# --------------------------------------------------------------------------------
# Universidad de San Carlos de Guatemala
# Facultad de Agronomía
# Área Tecnológica
# Subárea de Métodos de Cuantificación e Investigación
# Curso: Geoestadística
# Docente: Dr. Sc. Ezequiel López
# Apoyo: Dr. Byron González - CETE

#--------------------------------------------------------------------------------

# Uso de la biblioteca geoR
# El paquete geoR proporciona herramientas para el análisis de datos geoestadísticos en R 
# (otra alternativa es el paquete gstat, que fue visto anteriormente)

# Tesis de grado del Ing. Agr. Aroldo Yoc
# Disponible en el sitio web del CETE en http://cete.fausac.gt

#--------------------------------------------------------------------------------

# Materiales de consulta:

# https://rubenfcasal.github.io/post/introduccion-a-la-geoestadistica-con-geor/
# https://cran.r-project.org/web/packages/geoR/index.html
# http://www.leg.ufpr.br/geor/
# https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldataexamplespatial.html

# https://rdrr.io/cran/geoR/man/wolfcamp.html

#---------------------------------------------------------------------------------

# Desarrollo del script


# FASE 01

# 1) Cargar los paquetes 

if(!require(geoR)){install.packages("geoR")}
if(!require(MASS)){install.packages("MASS")} 
if(!require(modeest)){install.packages("modeest")}
if(!require(moments)){install.packages("moments")}
if(!require(nortest)){install.packages("nortest")}
if(!require(scatterplot3d)){install.packages("scatterplot3d")}
if(!require(car)){install.packages("car")}
if(!require(fBasics)){install.packages("fBasics")}

# 2) Crear un marco de datos no espaciales 
# indicar al R la ruta (path) donde se encuentra la carpeta con los datos
# es necesario cambiar el directorio en: Session ---->Set working directory ----> Choose Directory

suelo= read.table('Compac.txt', head=TRUE)

# 3) Crear un marco de datos espaciales y el borde del área de estudio

geosuelo <- as.geodata(suelo, coords.col=c(1, 2), data.col=3)
class(geosuelo)
attach(geosuelo)
bord = read.table('Bordes.txt')
geosuelo$borders <- with(bord, cbind(V1,V2))

# 4) Análisis exploratorio no espacial de los datos

## 4.1) Forma tabular

### 4.1.1) Visualización parcial de la tabla de datos y su estructura 

head(suelo)

### 4.1.2) Visualización total de la tabla de datos y su estructura 

suelo 
print(suelo)
names(suelo)

### 4.1.3) Diagrama de tallos y hojas (stem and leaf)  

stem(suelo$Com_psi, scale = 2)

## 4.2) Forma gráfica 

### 4.2.1) Gráfico de dispersión 2d 

x11()    #equivale a colocar windows(10,10)
points(geosuelo,xlab="Longitud",ylab="Latitud",main = "Gráfica de Coordenadas con Borde",col=gray(seq(1,0.1,l=0)))
savePlot('fig01_Grafico_Dispersion_2d.tiff',type="tiff")

points(geosuelo)
points(geosuelo, col = "gray", pt.divide = "equal")
points(geosuelo, col = "gray", pt.divide = "quintiles")

### 4.2.2) Gráfico de dispersión 3d
x11()
scatterplot3d(geosuelo$coords[,1], geosuelo$coords[,2],geosuelo$data,pch=16,xlab="Longitud (GTM)",ylab="Latitud (GTM)",zlab="Compactación del Suelo (psi)",highlight.3d = TRUE,main = "Gráfica de Dispersión")
savePlot('fig02_Grafico_Dispersion_3d.tiff',type="tiff")

## 4.3) Forma numérica

# Usando la biblioteca fBasics

round(basicStats(suelo$Com_psi, ci=0.95),3)

### 4.3.1) Medidas de tendencia central 

Media<-mean(suelo$Com_psi)
Mediana<-median(suelo$Com_psi)
Moda<-mfv(suelo$Com_psi)
descriptiva_01<-data.frame(Media,Mediana,Moda)
descriptiva_01

### 4.3.2) Medidas de dispersión

Valor_Maximo<-max(suelo$Com_psi)
Valor_Minimo<-min(suelo$Com_psi)
Rango<-Valor_Maximo-Valor_Minimo
descriptiva_02<-data.frame(Valor_Maximo,Valor_Minimo,Rango)
descriptiva_02

Varianza<-var(suelo$Com_psi)
Desviacion_Estandar<-sd(suelo$Com_psi)
Coeficiente_de_Variacion<-(((sd(suelo$Com_psi)/mean(suelo$Com_psi))*100))
descriptiva03<-data.frame(Varianza,Desviacion_Estandar,Coeficiente_de_Variacion )
descriptiva03

### 4.3.3) Medidas de posición relativa

Cuartiles<-quantile(suelo$Com_psi)
descriptiva04<-data.frame(Cuartiles)
descriptiva04

summary(suelo$Com_psi)

Rango_Intercuartilico<-IQR(suelo$Com_psi)
Desviacion_Cuartilica<-IQR(suelo$Com_psi)/2
descriptiva05<-data.frame(Rango_Intercuartilico,Desviacion_Cuartilica)
descriptiva05

### 4.3.4) Diagrama de Box-plot

x11()
boxplot(suelo$Com_psi, 
        ylim=c(0,200),
        col=c("51"),
        horizontal = T, 
        xlab="Compactación PSI", 
        main="Dist. Normal Range 1", 
        notch=TRUE,
        range=1.5, 
        boxwex=0.5, 
        outline = T, 
        plot = T)
savePlot('fig03_Diagrama_BoxPlot.tiff',type="tiff")

boxplot(suelo$Com_psi,plot = T, ylab="Compactación del suelo (PSI)") #Resumen del Box-plot
points(mean(suelo$Com_psi), col = 1, pch = 15)

### 4.3.5) Histograma con Diagrama de Box-plot

NC<-(1+(3.322*log10(66))) #calcular el número de clases para el histograma, ecuación de Sturges
NC

x11()
par(mfrow=c(2,1), mar=c(3,5,3,5))
hist(suelo$Com_psi,
     ylab="Densidad",breaks=8,freq=F, main="Distribución normal", xlim=c(30,200), col="51")
curve(dnorm, add=T)
rug(jitter(suelo$Com_psi))
boxplot(suelo$Com_psi, 
        ylim=c(30,200),
        col=c("51"),
        horizontal = T, 
        xlab="Densidad", 
        main="Dist. Normal Range 1.5", 
        notch=TRUE,
        range=1.5, 
        boxwex=1.5, 
        outline = T, 
        plot = T)
savePlot('fig04_Histogrma_BoxPlot.tiff',type="tiff")

## 4.4) Algunas medidas de dispersión de las coordenadas de los puntos de muestreo y del borde del área de estudio

### 4.4.1) Resumen de "geosuelo"

summary(geosuelo)

### 4.4.2) Gráfico de coordenadas y variable de estudio

x11()
plot(geosuelo, low=TRUE)
savePlot('fig05_CoordenadasVariable.tiff',type="tiff")

plot(geosuelo, trend=~coords)

# El comando points(geodata) (función points.geodata) genera un gráfico con las posiciones de los datos 
# (y por defecto con el tamaño de los puntos proporcional al valor):

points(geosuelo)
points(geosuelo, col = "gray", pt.divide = "equal")

## 4.5) Simetría y Curtosis

### 4.5.1) Forma gráfica 

x11()
hist(suelo$Com_psi,
     ylab="Densidad",xlab="Compactación de suelo",breaks=8,freq=F, main="Histograma", xlim=c(0,200), col="51")
rug(jitter(suelo$Com_psi))
savePlot('fig06_SimetriaCurtosis.tiff',type="tiff")

### 4.5.1) Forma gráfica

Curtosis<-kurtosis(suelo$Com_psi)
Sesgo<-skewness(suelo$Com_psi)
descriptiva06<-data.frame(Curtosis,Sesgo)
descriptiva06

# 5) Comprobación de supuestos

## 5.1) Supuesto de Normalidad

### 5.1.1) Gráfico de QQ-Plot

x11()
qqnorm(suelo$Com_psi,plot.it = TRUE, datax = FALSE)
qqline(suelo$Com_psi)
savePlot('fig07_QQplot.tiff',type="tiff")

### 5.1.2) Prueba de Shapiro-Wilk

shapiro.test(suelo$Com_psi)

# el valor de p es mayor a 0.05, la variable sigue una distribución normal

### 5.1.3) Prueba de Lilliefors (Kolmogorov-Smirnov)

lillie.test(suelo$Com_psi)
# Es necesaria la biblioteca nortest

# El valor de p es mayor a 0.05, la variable sigue una distribución normal

### 5.1.4) Evaluación de la "mejor "trasformación de datos" 
#Cuando no se presenta una distribucion normal, será necesaria la 
#trasformación de los datos.

# Según las pruebas anteriores, los datos presentan una distribución 
# normal, sin embargo, se presenta el ejemplo de como se deberían trasformar los datos. 
# Con el fin de ajustar la distribución normal a los datos
# se realiza la trasformación de los datos utilizando 
# la familia de trasformaciones potenciales Box-Cox.

x11()
boxcox(geosuelo)
abline(v=0.3,col="red")
savePlot('fig08_BoxCox.tiff',type="tiff")

x11()
x<-as.vector(suelo$Com_psi)
b <- boxcox(lm(x ~ 1))

# Valor exacto de lambda
lambda <- b$x[which.max(b$y)]
lambda

# Algunos casos particulares de la transformación Box-Cox
# Revise: https://www.r-bloggers.com/2022/10/box-cox-transformation-in-r/

# λ=1,   no hay necesidad de realizar la transformación
# λ=1/2, raíz cuadrada y transformación lineal
# λ=1/3, raíz cúbica y transformación lineal
# λ=0,   logaritmo natural
# λ=−1,  inversa
# λ=−1/2,inversa de la raíz cuadrada

# Como el intervalo de confianza incluye el 1, no es necesaria una 
# trasformación de los datos. 

# Como ejemplo se presentan las siguientes gráficas con distintos valores 
# de lambda
# Gráfica sin trasformación Lam=1 (lambda)

X11()
plot(geosuelo, low=T,lam=1)
savePlot('fig09_lam1.tiff',type="tiff")

# Gráfica con trasformación Lam=0 (equivale a la transformación logaritmo natural)
X11()
plot(geosuelo, low=T,lam=0)
savePlot('fig10_lam0.tiff',type="tiff")

# Gráfica con trasformación Lam=0.5 (equivale a raíz cuadrada)
x11()
plot(geosuelo, low=T,lam=0.5)
savePlot('fig11_lam0_5.tiff',type="tiff")

# Gráfica con tendencia
x11()
plot(geosuelo, low=T,trend="1st")
savePlot('fig11_lam0_5.tiff',type="tiff")

# En caso de ser necesaria la trasformación de datos, se deberá encontrar 
# el valor de lam, donde el gráfico inferior derecho muestre una distribución normal y esta trasformación deberá
# ser tomada en cuenta cuando se evalúen los modelos. 

## 5.2) Supuesto de dependencia espacial

x11()
var3 = variog(geosuelo,uvec=seq(0,190,l=70))
env.var = variog.mc.env(geosuelo, obj.v=var3, nsim=100) 
plot(var3, env=env.var)

savePlot('fig12_DependenciaEspacial.tiff',type="tiff")

# se comprobó la dependencia espacial de forma visual por medio de la 
# simulacion. Tomando en cuenta la distancia maxima entre coordenadas 
# calculada anteriormente, la cual es de 186.68278 (aproximado a 190), se toma el 70% de esta 
# distancia para el argumento `max.dist=` el cual es de 130, el número de 
# simulaciones se introduce en el argumento `nsim=` y para este caso se 
# trabajará con 100 simulaciones. 

# Si al menos uno de los puntos queda fuera del intervalo de confianza
# se dice que hay indicios de dependencia espacial. 

### 5.3) Supuesto de estacionariedad 

# Con el objetivo de estudiar si la media es estacionario o no, se estudiará
# el efecto de las coordenadas para el caso del gráfico superior derecho la 
# linea de la media deberá presentar un comportamiento vertical, para decir que el 
# proceso es estacionario en relación con la coordenada y para el caso del gráfico inferior izquierdo 
# la línea de la media deberá presentar un comportamiento horizontal, para decir que el proceso es 
# estacionario con relación a la coordenada x.

# Si anteriormente se definió trasformación de los datos deberá ser tomada en cuenta.
# para nuestro caso no fue necesaria la trasformación, con fines de ejemplo se incluirá 
# el valor de lam=1, el cual no implica alguna trasformación. 
# Se deberá elegir la superficie de tendencia que más se adecue a nuestros datos, en este caso 
# superficie de tendencia constante. 

### 5.3.1) Superficie de tendencia constante con datos trasformados

plot(geosuelo, trend=~coords)

x11()
plot(geosuelo,lowess=TRUE,trend="cte",lam=1)
savePlot('fig13_lam1_trendCTE.tiff',type="tiff")

### 5.3.2) Superficie de tendencia de primer orden con datos trasformados

x11()
plot(geosuelo,lowess=TRUE,trend="1st",lam=1)
savePlot('fig14_lam1_trend1ST.tiff',type="tiff")


### 5.3.1) Superficie de tendencia de segundo orden con datos trasformados

x11()
plot(geosuelo,lowess=TRUE,trend="2nd",lam=1)
savePlot('fig15_lam1_trend2ND.tiff',type="tiff")


# 6) Evaluación de la estructura de la covarianza 
# además de la superficie de tendencia es necesario estudiar 
# la estructura de la covarianza presente en los datos (esto si existe), 
# la función utilizada para obtener los variogramas es `variog()`, 

x11()
varc=variog(geosuelo,lowess=TRUE,trend="cte",lam=1,max.dist=130)
varct=variog(geosuelo,lowess=TRUE,trend="cte",lam=1,max.dist=150)
var1st=variog(geosuelo,lowess=TRUE,trend="1st",lam=1,max.dist=150)
var1stt=variog(geosuelo,lowess=TRUE,trend="2nd",lam=1,max.dist=130)

par(mfrow=c(2,2))
plot(varc,main="Tendencia constante",xlab="Distancia en grados",ylab="Semivarianza")
plot(varct,main="Tendencia constante, lam=1",xlab="Distancia en grados",ylab="Semivarianza")
plot(var1st,main="Tendencia de primer orden",xlab="Distancia en grados",ylab="Semivarianza")
plot(var1stt,main="Tendencia de segundo orden",xlab="Distancia en grados",ylab="Semivarianza")
savePlot('fig16_EstructuraCovarianza.tiff',type="tiff")

# 7) Evaluación de la anisotropía 
# Los variogramas empíricos en diferentes direcciones pueden ser utilizados para 
# investigar anisotropía. 
# las direcciones más comunes para la construcción de los variogramas empíricos son 
# a 0, 45, 90 y 135 grados.

## 7.1) Gráfica integradora

x11()
vario4c=variog4(geosuelo,lowess=TRUE,trend="cte",lam=1,max.dist=130)
vario4ct=variog4(geosuelo,lowess=TRUE,trend="cte",lam=1,max.dist=130)
vario41st=variog4(geosuelo,lowess=TRUE,trend="1st",lam=1,max.dist=130)
vario41stt=variog4(geosuelo,lowess=TRUE,trend="2nd",lam=1,max.dist=130)

par(mfrow=c(2,2))
plot(vario4c, omni=TRUE,legend=FALSE)  #Incluyendo el variograma omnidireccional
legend("bottomright", c("omnid.", "0º","45º","90º","135º"), lty=2, col=c("black","red","green","blue","aquamarine1"),xpd=0.1)
plot(vario4ct, omni=TRUE,legend=FALSE)
legend("bottomright", c("omnid.", "0º","45º","90º","135º"), lty=2, col=c("black","red","green","blue","aquamarine1"),xpd=0.1)
plot(vario41st, omni=TRUE,legend=FALSE)
legend("bottomright", c("omnid.", "0º","45º","90º","135º"), lty=2, col=c("black","red","green","blue","aquamarine1"),xpd=0.1)
plot(vario41stt, omni=TRUE,legend=FALSE)
legend("bottomright", c("omnid.", "0º","45º","90º","135º"), lty=2, col=c("black","red","green","blue","aquamarine1"),xpd=0.1)
savePlot('fig17_EvaluacionOmnidireccional.tiff',type="tiff")

# Cuando no se tiene efecto de dirección en el semivariograma, se espera que la gráfica de las 
# diferentes direcciones, sigan la tendencia de la gráfica omnidireccional y estén cerca de esta 
# si esto pasa no hay efecto de la dirección en el semivariograma 

## 7.2) Gráfica de superficie de tendencia constante
x11()
plot(vario4c, omni=TRUE, same=FALSE)
savePlot('fig18_EvaluacionOmnidireccional_TendenciaConstante.tiff',type="tiff")

## 7.3) Gráfica de superficie de tendencia constante, datos trasformados

x11()
plot(vario4ct, omni=TRUE, same=FALSE)
savePlot('fig19_EvaluacionOmnidireccional_TendenciaConstante_lam1.tiff',type="tiff")

## 7.4) Gráfica de superficie de tendencia de primer orden, datos trasformados

x11()
plot(vario41st, omni=TRUE, same=FALSE)
savePlot('fig20_EvaluacionOmnidireccional_Tendencia1st_lam1.tiff',type="tiff")

## 7.5) Gráfica de superficie de tendencia de segundo orden, datos trasformados

x11()
plot(vario41stt, omni=TRUE, same=FALSE)
savePlot('fig21_EvaluacionOmnidireccional_Tendencia2nd_lam1.tiff',type="tiff")

#-------------------------------------------------------------------------------------

#FASE 02

# 1) Ajuste visual del modelo para el variograma

# Para tener una idea de los valores del variograma, se construirá un variograma a 45 grados.  
# dir=pi/4, indica hacer el semivariograma a 45 grados, este variograma es al azar,
# para construir este variograma, se utilizará el doble de la distancia máxima la cual es de 186.68,


x11()
v1 <- variog(geosuelo, max.dist=150,estimator="classical")
plot(v1)

ef1 = eyefit(v1)

summary(ef1)

# 2) Evaluación de los modelos geoestadísticos 

## 2.1) Depuración de los modelos 

# Se probaron los 15 modelos disponibles en geoR, con el objetivo de depurar los que no se ajusten. 
# para el caso de nuestros datos, no se realizó trasformación de datos es decir que el valor de lam=1 
# y se ajustó mejor la superficie de tendencia constante es decir trend="cte"

ml01 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "cauchy",kappa=0.5,lam=1, trend="cte")

#ml02 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "gencauchy",lam=1, trend="cte")
ml03 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model="circular",lam=1, trend="cte")
ml04 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model="cubic",lam=1, trend="cte")
ml05 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model="exponential",lam=1, trend="cte")
ml06 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "gaussian",lam=1, trend="cte")
ml07 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "gneiting",lam=1, trend="cte")
#ml08 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "gneiting.matern",kappa=0.5,lam=1, trend="cte")
ml09 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "linear",lam=1, trend="cte")
ml10 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "matern",kappa=0.5,lam=1, trend="cte")
#ml11 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "power",lam=1, trend="cte")
ml12 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "powered.exponential",kappa=0.5,lam=1, trend="cte")
ml13 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "pure.nugget",lam=1, trend="cte")
ml14 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "spherical",lam=1, trend="cte")
#ml15 <- likfit(geosuelo, ini=c(997.43,48.04), nug=332.48,cov.model= "wave",lam=1, trend="cte")

# al ejecutar los siguientes comandos, deberá generar el nombre del modelo evaluado 
# si el resultado fuera "Error: object XXX", se deberá depurar el modelo en la siguiente prueba 
# en este caso los modelos a depurar fueron: ml02, ml08 y ml11

ml01$cov.model 
ml03$cov.model
ml04$cov.model
ml05$cov.model
ml06$cov.model
ml07$cov.model
ml08$cov.model
ml09$cov.model
ml10$cov.model
ml11$cov.model 
ml12$cov.model
ml13$cov.model
ml14$cov.model
ml15$cov.model

## 2.2) Evaluación de la dependencia espacial 
# para que un modelo evaluado presente dependencia espacial:
# los valores de AIC y BIC del modelo no espacial deberán ser más altos que los valores de AIC y BIC del modelo espacial
# evaluando los resultados de AIC y BIC, del modelo espacial y del modelo no espacial 
# se definió de forma numérica los modelos que presentan dependencia espacial 
# en el cuadro resumen se muestra la resta de AIC no espacial, menos el AIC espacial 
# y de la resta de BIC no espacial, menos el BIC espacial
# se depurarán los modelos que contengan valores negativos de la diferencia de AIC y BIC 
# y únicamente se tomarán los modelos que presenten valores positivos en la diferencia de AIC y BIC
# los modelos ml02, ml08 y ml11, no se tomarán en cuenta para esta prueba, ya que fueron depurados anteriormente 

Md01<-data.frame(Modelo=c("cauchy_ml01","circular_ml03","cubic_ml04","exponentia_lml05","gaussian_ml06","gneiting_ml07","linear_ml09","matern_ml10","powered.exponential_ml12","pure.nugget_ml13","spherical_ml14"), 
                 "Dif AIC"=c((ml01$nospatial$AIC.ns-ml01$AIC),(ml03$nospatial$AIC.ns-ml03$AIC),(ml04$nospatial$AIC.ns-ml04$AIC),(ml05$nospatial$AIC.ns-ml05$AIC),(ml06$nospatial$AIC.ns-ml06$AIC),(ml07$nospatial$AIC.ns-ml07$AIC),(ml09$nospatial$AIC.ns-ml09$AIC),(ml10$nospatial$AIC.ns-ml10$AIC),(ml12$nospatial$AIC.ns-ml12$AIC),(ml13$nospatial$AIC.ns-ml13$AIC),(ml14$nospatial$AIC.ns-ml14$AIC)),
                 "Dif BIC"=c((ml01$nospatial$BIC.ns-ml01$BIC),(ml03$nospatial$BIC.ns-ml03$BIC),(ml04$nospatial$BIC.ns-ml04$BIC),(ml05$nospatial$BIC.ns-ml05$BIC),(ml06$nospatial$BIC.ns-ml06$BIC),(ml07$nospatial$BIC.ns-ml07$BIC),(ml09$nospatial$BIC.ns-ml09$BIC),(ml10$nospatial$BIC.ns-ml10$BIC),(ml12$nospatial$BIC.ns-ml12$BIC),(ml13$nospatial$BIC.ns-ml13$BIC),(ml14$nospatial$BIC.ns-ml14$BIC)))
Md01

# el criterio de evaluación es: se aceptan los modelos que obtuvieron diferencias positivas en AIC y BIC
# por lo tanto únicamente serán objeto de la siguiente evaluación los modelos 
# ml03, ml04, ml06, ml07 y ml14

## 2.3) Selección del modelo espacial 

Md02<-data.frame(Modelo=c("circular_ml03","cubic_ml04","gaussian_ml06","gneiting_ml07","spherical_ml14"), 
                 AIC=c(ml03$AIC,ml04$AIC,ml06$AIC,ml07$AIC,ml14$AIC),
                 BIC=c(ml03$BIC,ml04$BIC,ml06$BIC,ml07$BIC,ml14$BIC))
Md02 <- Md02[order(Md02$AIC), ]
Md02

# El mejor modelo es el que tenga los valores mas bajos de AIC y BIC
# para este caso el mejor modelo espacial es: gaussian 

## 2.3) Visualización de los parámetros de ml06

ml06
summary(ml06)

plot(variog(geosuelo, max.dist=130,estimator="classical"))

lines.variomodel(ml06,col="black")

legend("bottomright", "gaussiano", lty=1, col="black")

title("Ajuste de variograma con la funcion de correlación")

# 3) Malla de puntos de predicción

# el objetivo de este procedimiento es definir la malla de puntos de predicción 
# en la función "by=XXX", se define las unidades mapa para la malla de predicción 
# tomar en cuenta las capacidades de procesador y memoria RAM, de la computadora 
# en este caso la computadora tiene 16 Gigas de Ram y un procesador INTEL Core-i7
# el valor óptimo para generar la malla de puntos de predicción fue de 3 unidades mapa,
# inferior a este valor el tiempo de procesamiento aumenta drásticamente.

x11()
gr <- pred_grid(geosuelo$bord, by=2)
points(geosuelo)
points(gr, col=2, pch=19, cex=0.3)
gr0 <- gr[.geoR_inout(gr, geosuelo$bord),]
points(gr0, col=3, pch=19, cex=0.3)
savePlot('fig23_MallaDePredic.tiff',type="tiff")

dim(gr)#indica el total de puntos en la cuadrícula 
dim(gr0)#indica el número de puntos dentro del área de estudio

# 3) Simulación

gr = pred_grid(geosuelo$borders, by=2)
s.out = output.control(n.predictive = 1000, n.post=1000, quant=0.95, thres=200)
geosuelo.kc = krige.conv(geosuelo, loc=gr, krige=krige.control(obj=ml06), output = s.out)

# obteniendo el mapa de ocurrencia de valores arriba de 200psi 
x11()
image(geosuelo.kc, col = terrain.colors(200), main="Mapa de P(Y > 200)",
      val=(1-geosuelo.kc$probabilities.simulations),
      x.leg=c(763230, 763350), y.leg =c(1612983,1612990),
      ylim=c(1612980,1613190),xlim=c(763240,763330))
savePlot('fig24_MapaDeSimulacion.tiff',type="tiff")

# obteniendo el mapa de ocurrencia de valores arriba de 100psi (mediana) 
s.out1 = output.control(n.predictive = 1000, n.post=1000, quant=0.95, thres=100)
geosuelo.kc1 = krige.conv(geosuelo, loc=gr, krige=krige.control(obj=ml06), output = s.out1)

x11()
image(geosuelo.kc1, col = terrain.colors(200), main="Mapa de P(Y > 100)",
      val=(1-geosuelo.kc1$probabilities.simulations),
      x.leg=c(763230, 763350), y.leg =c(1612983,1612990),
      ylim=c(1612980,1613190),xlim=c(763240,763330))

# 3) Mapa del kriging
x11()
KC <- krige.control(type="OK", obj.model=ml06)
kc1 <- krige.conv(geosuelo, loc=gr, krige=KC)
image(kc1, col=terrain.colors(200),
      x.leg=c(763230, 763350), y.leg =c(1612983,1612990),
      ylim=c(1612980,1613190),xlim=c(763240,763330), main="Mapa de interpolación")
savePlot('fig25_MapaDeKrigin.tiff',type)

#---------------------------------------------------------------------------------
# Falta la validación cruzada