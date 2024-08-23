#() Parentésis externo es para que imprima y muestre
#resultados
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

#Distribuciones discretas 
#Binomial 
#f de probabilidad 
plot(dbinom(0:10,10,.6),
     type="h",xlab="k",ylab="P(X=k)",
     main="Función de Probabilidad B(10,0,6)")

#Función de distribución 
plot(stepfun(0:10,pbinom(0:11,10,.6)),
     xlab="k",ylab="F(k)",
     main="Función de distribución B(10,0,.6)")

#Verificación empírica 
x<-rbinom(10000,10,.6) #generación de valores aleatorios de una binomial 
(freqAbs=table(x)) #Frecuencias absolutas 
(freqRel=prop.table(freqAbs)) #Frecuencias relativas 

#Comparación con resultados teóricos 
probsTeo<-data.frame(x=0:10,Prob=dbinom(0:10,10,.6))
freqRel<-as.data.frame(freqRel)
str(freqRel)
freqRel$x<-as.integer(as.character(freqRel$x))
(compara<-merge(freqRel,probsTeo,all=TRUE))

#Comparación gráfica 
with(compara,{
  plot(x,Freq, type="b")
  points(x,Prob,col="red",pch=4)
  lines(x,Prob,col="red",lty=2,lwd=2)
  legend("topleft",c("frec.relativa","probabilidad"),
         col=c("black","red"),lty=1:2,pch=c(1,4))
})

#Distribución de probabilidad continua 
#Normal 
curve(dnorm(x,170,12),xlim=c(130,210),col="blue",lwd=2,
      xlab="x",ylab="f(x)",
      main="Función de Densidad N(170,12)")

curve(pnorm(x,170,12),xlim=c(130,210),col="blue",lwd=2,
      xlab="x",ylab="F(x)",
      main="Función de Distribución N(170,12)")

#¿Cuál es la probabilidad de que la variable aleatoria entre 150 y 168? 
pnorm(168,170,12)-pnorm(150,170,12) 

#Gráficamos el área de la probabilidad 
regionX=seq(150,168,0.01)
xP <- c(150,regionX,168)
yP <- c(0,dnorm(regionX,170,12),0)
curve(dnorm(x,170,12),xlim=c(130,210),
      yaxs="i",ylim=c(0,0.035),ylab="f(x)",
      main='Densidad N(170,12)')
polygon(xP,yP,col="orange1")
box()

#Histogramas Empirico (rojo) vs teórico (Azul) 
X<-rnorm(10000, 170, 12)
hist(X,freq=FALSE,col="red",
     main="Histograma",sub="Datos simulados N(170,12)")
curve(dnorm(x,170,12),xlim=c(110,220),
      col="blue",lwd=2,add=TRUE)

plot(ecdf(X))
curve(pnorm(x,170,12),xlim=c(110,220),
      col="red",lwd=2,lty=2,add=TRUE)
legend("topleft",lty=c(1,2),lwd=c(1,2),
       col=c("black","red"),
       legend=c("Distribuci.n emp.rica",
                "Distribuci.n te.rica"))

#Distribución F: Intervalo de confianza
df1 <- 25
df2 <- 30
qmin <- 0.5
qmax <- 1.5
region <- seq(0,7,0.01)
xT <- c(qmin,region,qmax)
yT <- c(0,df(region,df1,df2),0)
regionX=seq(qmin,qmax,0.01)
xP <- c(qmin,regionX,qmax)
yP <- c(0,df(regionX,df1,df2),0)
curve(df(x,df1,df2),xlim=c(0,7),col="black",
      main=paste("Distribuci.n F (",df1,",",df2,")"))
polygon(xT,yT,col="red")
polygon(xP,yP,col="white")
box() #te pone el cuadrado alrededor 


#Herramienta en R: Funciones 

#Media
die<-1:6
mean(die)

#Muestra aleatoria 
sample(x=die,size=6)

#Muestra aleatoria con reemplazo 
sample(x=die,size=6,replace=TRUE)

#Definición de funciones 
#Función que tira dos dados y suma el resultado
roll<-function(){
  die<-1:6
  dice<-sample(x=die,size=2,replace=TRUE)
  sum(dice)
}
roll()

#Argumento número de caras del dado 
roll2<-function(bones){
  dice<-sample(bones,size=2,replace=TRUE)
  print(dice)
  sum(dice)
}
roll2(bones=1:6)

#Argumento número de caras del dado y número de dados 
roll3<-function(bones,ndice){
  dice<-sample(bones,ndice,replace=TRUE)
  print(dice)
  sum(dice)
}
roll3(bones=1:6,2)

#Teorema Central del Límite

n<-25
muestra1<-rnorm(n,170,12)
(media1=mean(muestra1))

muestra2<-rnorm(n,170,12)
(media2=mean(muestra2))

muestra3<-rnorm(n,170,12)
(media3=mean(muestra3))

#Todo lo de arriba se puede resumir en una función 
#que nos calcula la media muestral 
mediaMuestral<-function(n){
  muestra=rnorm(n,170,12)
  media=mean(muestra)
  return(media)}

mediaMuestral(25)
mediaMuestral(25)
mediaMuestral(25)

#Test de la función 
m=10000
muchasMedias<-replicate(m,mediaMuestral(25))
muchasMedias[1:20]

mean(muchasMedias)
sd(muchasMedias)

#Comprobación gráfica del Teorema central del límite
muchasMedias50<-replicate(m,mediaMuestral(50))
muchasMedias100<-replicate(m,mediaMuestral(100))
muchasMedias500<-replicate(m,mediaMuestral(500))

#Histogramas con una curva normal encima 
hist(muchasMedias100,xlab="Media muestral",
     ylab="Frecuencia", col="lightcyan",
     xlim=c(160,180),freq=FALSE,ylim=c(0,0.75),
     main="Histograma de las medias muestrales observadas",
     sub="10000 muestras de tama.o 100")
curve(dnorm(x,170,sd(muchasMedias100)),
      xlim=c(160,180),col="blue",lwd=2,add=TRUE)

hist(muchasMedias50,xlab="Media muestral",
     ylab="Frecuencia", col="lightcyan",
     xlim=c(160,180),freq=FALSE,ylim=c(0,0.75),
     main="Histograma de las medias muestrales observadas",
     sub="10000 muestras de tama.o 100")
curve(dnorm(x,170,sd(muchasMedias50)),
      xlim=c(160,180),col="blue",lwd=2,add=TRUE)

hist(muchasMedias500,xlab="Media muestral",
     ylab="Frecuencia", col="lightcyan",
     xlim=c(160,180),freq=FALSE,ylim=c(0,0.75),
     main="Histograma de las medias muestrales observadas",
     sub="10000 muestras de tama.o 100")
curve(dnorm(x,170,sd(muchasMedias500)),
      xlim=c(160,180),col="blue",lwd=2,add=TRUE)


#Intervalos de confianza 
# Configurar parámetros de simulación
set.seed(123)  # para reproducibilidad
n_simulations <- 100  # número de simulaciones
sample_size <- 30  # tamaño de la muestra en cada simulación
true_mean <- 50  # media verdadera de la distribución
true_sd <- 10  # desviación estándar verdadera de la distribución
confidence_level <- 0.95  # nivel de confianza
z_score <- qnorm((1 + confidence_level) / 2)  # Z-score para el nivel de confianza

# Función para calcular el intervalo de confianza para la media
calculate_ci <- function(sample_data) {
  sample_mean <- mean(sample_data)
  sample_std <- sd(sample_data)
  margin_of_error <- z_score * (sample_std / sqrt(length(sample_data)))
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  return(c(lower_bound, sample_mean, upper_bound))
}

# Simular y calcular intervalos de confianza
simulations <- replicate(n_simulations, {
  sample_data <- rnorm(sample_size, mean = true_mean, sd = true_sd)
  calculate_ci(sample_data)
})

# Convertir los resultados en un data frame
ci_df <- data.frame(
  Simulation = 1:n_simulations,
  Lower = simulations[1, ],
  Mean = simulations[2, ],
  Upper = simulations[3, ]
)

# Visualizar los intervalos de confianza
ggplot(ci_df, aes(x = Simulation)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "blue") +
  geom_point(aes(y = Mean), color = "red", size = 0.5) +
  geom_hline(yintercept = true_mean, linetype = "dashed", color = "black") +
  labs(title = "Intervalos al 95% de confianza para la media",
       x = "Simulación",
       y = "Media") +
  theme_minimal()




