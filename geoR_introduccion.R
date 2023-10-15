# --------------------------------------------------------------------------------
# Universidad de San Carlos de Guatemala
# Facultad de Agronomía
# Área Tecnológica
# Subárea de Métodos de Cuantificación e Investigación
# Curso: Geoestadística
# Docente: Dr. Sc. Ezequiel López
# Apoyo: Dr. Byron González - CETE
# 
# ---------------------------------------------------------------
# Práctica sobre uso de la biblioteca geoR


# Material de consulta:

# https://rubenfcasal.github.io/post/introduccion-a-la-geoestadistica-con-geor/
# https://sites.stat.washington.edu/peter/591/geoR_sln.html
# --------------------------------------------------------------------------------

# Uso de la biblioteca geoR
# El paquete geoR proporciona herramientas para el análisis de datos geoestadísticos en R 
# (otra alternativa es el paquete gstat, que fue visto anteriormente)

# Tesis de grado del Ing. Agr. Aroldo Yoc
# Disponible en el sitio del CETE en http://cete.fausac.gt

#--------------------------------------------------------------------------------
# Desarrollo del script

# FASE 01

# 1) Cargar los paquetes 
if(!require(geoR)){install.packages("geoR")}
if(!require(MASS)){install.packages("MASS")} 
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(scatterplot3d)){install.packages("scatterplot3d")}
if(!require(car)){install.packages("car")}

# 2) Crear un marco de datos no espaciales 
# indicar al R la ruta (path) donde se encuentra la carpeta con los datos
# se debe cambiar el directorio en: Session ---->Set working directory ----> Choose Directory

suelo= read.table('Compac.txt', head=TRUE)

# 3) Crear un marco de datos espaciales y el borde del área de estudio

geosuelo <- as.geodata(suelo, coords.col=c(1, 2), data.col=3)
class(geosuelo)

# Borde, orilla del terreno

bord = read.table('Bordes.txt')
geosuelo$borders <- with(bord, cbind(V1,V2))

# ----------------------------------------------------------------------------------------------               
# Veamos ahora los resultados, implementando algunas líneas del script Guía
# ----------------------------------------------------------------------------------------------

summary(geosuelo)
x11()
plot(geosuelo)

# The lowess argument adds a smooth line to the upper right scatter plot (data value vs y value) 
# and to the lower left scatter plot (x value vs data value).

plot(geosuelo, lowess = TRUE, scatter3d = TRUE) 

# Si se asume que hay una tendencia puede interesar eliminarla:

plot(geosuelo, lowess = TRUE,trend=~coords)

# El comando points(geodata) (función points.geodata) genera un gráfico con las posiciones de los datos 
# (y por defecto con el tamaño de los puntos proporcional al valor):

points(geosuelo)

points(geosuelo, col = "gray", pt.divide = "equal")

args(points.geodata)

# Modelado de la dependencia espacial
# En la primera parte de esta sección consideraremos un proceso espacial sin tendencia:

summary(geosuelo)
plot(geosuelo)

# Variogramas empíricos
x11()
oldpar <- par(mfrow=c(1,2)) 
plot(variog(geosuelo))
plot(variog(geosuelo, max.dist = 95)) #considerar aprox. 1/2 distancia máxima = 186.68278

# La recomendación es considerar solo saltos hasta la mitad de la máxima distancia (ver ‘Distance summary’ en resultados del sumario).

vario <- variog(geosuelo, max.dist = 95)

names(vario)
str(vario)

# Calculo de los variogramas empíricos
vario.b <- variog(geosuelo, max.dist = 95) #discretizado
vario.c <- variog(geosuelo, max.dist=95, op="cloud")  #nube
vario.bc <- variog(geosuelo, max.dist=95, bin.cloud=TRUE)  #discretizado+nube
vario.s <- variog(geosuelo, max.dist=95, op="sm", band=0.5)  #suavizado

# Representación gráfica
oldpar<-par(mfrow=c(2,2)) # Preparar para 4 gráficos por ventana
plot(vario.b, main="Variograma empírico")
plot(vario.c, main="Nube de puntos variograma")
plot(vario.bc, bin.cloud=TRUE, main="Graficos de cajas")
title("Gráficos de cajas") # Corregir fallo del comando anterior
plot(vario.s, ylim=c(0,4000),main="Variograma suavizado")

par(oldpar) # Restaurar opciones de gráficos

# Si hay valores atípicos (o la distribución de los datos es asimétrica) puede ser preferible utilizar el estimador robusto. 
# Se puede calcular este estimador estableciendo estimator.type = "modulus":

# Cálculo de los variogramas empíricos
vario.b <- variog(geosuelo, max.dist = 95) #discretizado
vario.bc <- variog(geosuelo, max.dist=95, bin.cloud=TRUE)  #discretizado+nube

varior.b <- variog(geosuelo, estimator.type = "modulus", max.dist=95)
varior.bc <- variog(geosuelo, estimator.type = "modulus", max.dist=95, bin.cloud=TRUE)

oldpar<-par(mfrow=c(2,2)) #Preparar para 4 gráficos por ventana
plot(vario.b, main="Estimador clásico")
plot(varior.b, main="Estimador robusto")
plot(vario.bc, bin.cloud=TRUE)
plot(varior.bc, bin.cloud=TRUE)

par(oldpar) #Restaurar opciones de gráficos

# En el caso de anisotropía, también se pueden obtener variogramas direccionales con la función variog 
# mediante los argumentos direction y tolerance. Por ejemplo, para calcular un variograma en la dirección de 60 grados 
# (con la tolerancia angular por defecto de 22.5 grados):

vario.60 <- variog(geosuelo, max.dist = 95, direction = pi/3) #variograma en la dirección de 60 grados

# Para estudiar si hay anisotropía, se pueden cálcular de forma rápida variogramas direccionales con la función variog4. 
# Por defecto calcula cuatro variogramas direccionales, correspondientes a los ángulos 0, 45, 90 y 135 grados:

vario.4 <- variog4(geosuelo, max.dist = 95)

oldpar <- par(mfrow=c(1,2))
plot(vario.60)
title(main = expression(paste("direccional, angulo = ", 60 * degree)))
plot(vario.4, lwd = 2)
par(oldpar)

# Ajuste de un modelo de variograma

vario.b <- variog(geosuelo, max.dist=100) #discretizado

# cov.pars = c(997.43,48). Sill(sigmasq = meseta), Range (phi = amplitud) = 43.48
# tausq (nugget = efecto pepita) = 332.48

plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(997.43,48), nugget = 332.48, max.dist = 100, lwd = 3)
legend(40, 600, c("empirico", "modelo exponencial"), lty = c(1, 2, 1), lwd = c(1, 1, 3))
par(oldpar)

# Otros ajustes

plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(997.43,48), nug = 332.48, max.dist = 100)
lines.variomodel(cov.model = "mat", cov.pars = c(997.43,48), nug = 332.48, kappa = 1, max.dist = 100,lty = 2)
lines.variomodel(cov.model = "sph", cov.pars = c(997.43,48), nug = 332.48, max.dist = 100, lwd = 2)
par(oldpar)

# En las versiones recientes de geoR está disponible una función para realizar el ajuste gráficamente de forma interactiva (cuadro de diálogo en tcl/tk):

eyefit(vario.b)

# Modelo exponencial
# sigmasq (sill = meseta) = 1311.78
# phi (range = rango) = 57.16
# tausq (nugget = efecto pepita) = 163.97

# Ejemplos de estimación por mínimos cuadrados (llamadas a variofit):

# Modelo exponencial 

vario.ols <- variofit(vario.b, ini = c(1300, 50), weights = "equal")  #ordinarios
vario.wls <- variofit(vario.b, ini = c(1300, 50), weights = "cressie")  #ponderados

vario.wls
summary(vario.wls)

# Ejemplo de estimación por máxima verosimilitud (llamada a likfit):

vario.ml <- likfit(geosuelo, ini = c(1300, 50)) #Modelo exponencial con par ini umbral y escala (1/3 rango)
vario.ml
summary(vario.ml)

# El modelo que Aroldo seleccionó
vario.mlg <- likfit(geosuelo, ini=c(1300,50), nug=160,cov.model= "gaussian",lam=1, trend="cte")
vario.mlg
summary(vario.mlg)

# Ejemplo de estimación por máxima verosimilitud restringida (opción de likfit):
vario.reml <- likfit(geosuelo, ini = c(1300,50), lik.method = "RML")
summary(vario.reml)
x11()
plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.ml, max.dist = 100)
lines(vario.reml, lwd = 2, max.dist = 100)
lines(vario.ols, lty = 2, max.dist = 100)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 100)
legend(60, 400, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1, 1, 2, 2), lwd = c(1, 2,1, 2)) 

plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.mlg,lty = 1,max.dist = 100)
lines(vario.ml, lty = 3,max.dist = 100)

# Inferencia sobre el variograma
env.indep <- variog.mc.env(geosuelo, obj.var = vario.b)
env.model <- variog.model.env(geosuelo, obj.var = vario.b, model = vario.ml)

oldpar <- par(mfrow = c(1, 2))
plot(vario.b, envelope = env.indep)
plot(vario.b, envelope = env.model)
lines(vario.ml, lty = 2, lwd = 2, max.dist = 100)
par(oldpar) 

if(!require(sm)){install.packages("sm")}
sm.variogram(geosuelo$coords, geosuelo$data, model = "independent")

# Test of spatial independence: p =  0.081 
# Hay dependencia espacial, significativa al 10%

sm.variogram(geosuelo$coords, geosuelo$data, model = "isotropic")
# Test of isotropy: p =  0.85 
# Se acepta la Ho de que es isotrópico

sm.variogram(geosuelo$coords, geosuelo$data, model = "stationary")

# Test of stationarity: p =  0.1
# Se acepta la Ho de que es estacionario

# Validación cruzada

# Para verificar si un modelo de variograma describe adecuadamente la dependencia espacial de los datos (p.e. comparar modelos), 
# se emplea normalmente la técnica de validación cruzada, función xvalid en geoR.

# Por defecto la validación se realiza sobre los datos eliminando cada observación (y utilizando las restantes para predecir), 
# aunque se puede utilizar un conjunto diferente de posiciones (o de datos) mediante el argumento location.xvalid (y data.xvalid).

xv.ml <- xvalid(geosuelo, model = vario.ml)
summary(xv.ml)

xv.reml <- xvalid(geosuelo, model = vario.reml)
summary(xv.reml)

xv.mlg <- xvalid(geosuelo, model = vario.mlg)
summary(xv.mlg)

x11()
oldpar <- par(mfrow = c(2, 5))
plot(xv.ml, ask = FALSE)

oldpar <- par(mfrow = c(2, 5))
plot(xv.reml, ask = FALSE)

oldpar <- par(mfrow = c(2, 5))
plot(xv.mlg, ask = FALSE)

#---------------------------------------------------------------------------------

# Predicción espacial (kriging)

# El paquete geoR dispone de opciones para los métodos kriging tradicionales, que dependiendo de las suposiciones 
# acerca de la función de tendencia se clasifican en:

# Kriging simple (KS): media conocida
# Kriging ordinario (KO): se supone que la media es constante y desconocida.
# Kriging universal (KU): también denominado kriging con modelo de tendencia, se supone que la media es una combinación lineal (desconocida) 
# de las coordenadas o de otras variables explicativas.

# Rejilla regular 

x11()
gr <- pred_grid(geosuelo$bord, by=2)
points(geosuelo)
points(gr, col=2, pch=19, cex=0.3)
gr0 <- gr[.geoR_inout(gr, geosuelo$bord),]
points(gr0, col=3, pch=19, cex=0.3)

# El comando para realizar kriging ordinario con variograma vario.ml sería:

gr = pred_grid(geosuelo$borders, by=2)
s.out = output.control(n.predictive = 1000, n.post=1000, quant=0.95, thres=200)
geosuelo.kc = krige.conv(geosuelo, loc=gr, krige=krige.control(obj=vario.ml), output = s.out)

oldpar <- par(mfrow = c(1, 2))
image(geosuelo.kc) #superficie de predicción
title("Predicciones")
points(geosuelo$coords, pch=20) #añadir posiciones datos
contour(geosuelo.kc,add=T) #añadir gráfico de contorno

# Superficie de varianzas
par(mfrow = c(1, 2))
image(geosuelo.kc, val = geosuelo.kc$krige.var) #superficie de varianzas
title("Superficie de varianzas")
points(geosuelo$coords, pch=20)
contour(geosuelo.kc,val=sqrt(geosuelo.kc$krige.var),add=T)

# Otras opciones
x11()
contour(geosuelo.kc,filled = TRUE)

image(geosuelo.kc, col=terrain.colors(200),
      x.leg=c(763230, 763350), y.leg =c(1612983,1612990),
      ylim=c(1612980,1613190),xlim=c(763240,763330), main="Mapa de interpolación")

ko.ml <- krige.conv(geosuelo, loc = pred.grid, krige = krige.control(obj.m = vario.ml))

# El resultado es una lista incluyendo predicciones (ko.ml$predict) y varianzas kriging (ko.ml$krige.var):
names(ko.ml)

# Para representar las superficies se puede utilizar la función image:

oldpar <- par(mfrow = c(1, 2))
image(ko.ml) #superficie de predicción
title("Predicciones")
points(geosuelo$coords, pch=20) #añadir posiciones datos
contour(ko.ml,add=T) #añadir gráfico de contorno
