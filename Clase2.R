library(ggplot2)
library(dplyr)

#TÉCNICAS DE MUESTREO Y REMUESTREO: BOOTSTRAP Y JACKNIFE

#Muestreo simple (Muestra aleatoria con reemplazo) 
#replace=TRUE: con reemplazo 
nrow(iris)
selec<-sample(1:nrow(iris),15,replace=TRUE)
muestra<-iris[selec,]
selec

muestra

alumnos<-c("GERARDO", "CALEB", "ALVARO", "MARIO", "ELISA",
           "ANDREA", "ALBERTO", "RODRIGO", "CARLOS", "DAVID",
           "ANTONIO", "JORGE", "LAURA", "GABRIELA", "VICTOR",
           "GUILLERMO", "RICARDO", "LORENA", "LILIANA")
length(alumnos)
sample(alumnos,4,replace=TRUE)

#set.seed para que nos de el mismo muestreo siempre 
#set.seed(31416)
sample(alumnos,4,replace=TRUE)

#Muestreo ponderado 
#prob: Vector de probabilidades/pesos para el muestreo ponderado
p<-c(1,2,1,2,1,2,1,1,3,1,2,1,1,3,2,1,1,5,2)
total<-sum(p)
sample(alumnos,4,replace=TRUE,prob=p/total)

#REMUESTREO: 
  
#BOOTSTRAPING
#Obtener una muestra aleatoria simple con reemplazo de tamaño n 
#Calcular el estadístico apropiado de interés para cada muestra 
#Repetir los pasos anteriores B veces de manera independiente, B es un número grande 
#Utilizar estos B resultados para calcular el estadístico y su distribución

#JACKNIFE
#Remuestreo omitiendo alguna observación 
#Se obtiene el estimador que se quiere evaluar de la muestra 
#Definir un vector tamaño n para almacenar los valores 
#Se realiza el remuestreo del conjunto de datos original eliminando una observación del conjunto 
#Repite el paso anterior para cada observación 
#Se calcula el estimador jacknife



#Ejemplo Bootstrap

#Datos 
x <-c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 5.2,
      7.01, 8.74, 7.78, 7.02, 6, 6.5, 5.8, 5.12, 7.41, 6.52, 6.21,
      12.28, 5.6, 5.38, 6.6, 8.74)

#Función que calcula el coeficiente de variación 
CV<-function(x){
  sqrt(var(x))/mean(x)
}
#Estimación puntual obtenido de la muestra 
CV(x)

#Muestras bootstrap
CV(sample(x,replace=TRUE))

#Generar muchas muestras y calcular el estimador  
boot<-numeric(1000) #vector de 1000 entradas 
for (i in 1:1000){
  boot[i]<-CV(sample(x,replace=TRUE))
}

head(boot)  
mean(boot)
var(boot)
max(boot)
min(boot)

#histograma para ver si es normal 
hist(boot, freq = FALSE)
curve(dnorm(x, mean = mean(boot), sd = sqrt(var(boot))), from = 0.10, to = 0.40, col = "red", add = TRUE)

#qqplot: Comparación de una distribución normal con la de tus datos 
qqnorm(boot,col="red")
qqline(boot,col="blue")


#Intervalo de confianza 95%
quantile(boot,c(.025,.975))
linf<-CV(x)-abs(qnorm(.025))*sqrt(var(boot))
lsup<-CV(x)+abs(qnorm(.025))*sqrt(var(boot))
(ic<-c(linf,lsup))

#Corregir el sesgo 
(bias<-mean(boot)-CV(x))
CV(x)-bias
linf<-CV(x)-bias-abs(qnorm(.025))*sqrt(var(boot))
lsup<-CV(x)-bias+abs(qnorm(.025))*sqrt(var(boot))
(ic<-c(linf,lsup))


#Ejemplo Jacknife
jack <- numeric(length(x) - 1)
pseudo <- numeric(length(x))

for (i in 1:length(x)) {
  for (j in 1:length(x)) {
    if (j < i) {
      jack[j] <- x[j]
    } else if (j > i) {
      jack[j - 1] <- x[j]
    }
  }
  pseudo[i] <- length(x) * CV(x) - (length(x) - 1) * CV(jack)
}
  
pseudo  

mean(pseudo)
var(pseudo)
(varjack<-var(pseudo/length(x)))

hist(pseudo,freq=FALSE)

#CVgorro<-mean(pseudo)
#linf<-CV(x)-qt(.975,sqrt(var(boot))
#lsup<-CV(x)+qt(.975,sqrt(var(boot))
               
set.seed(123)
library(boot)
               
               
               
               
