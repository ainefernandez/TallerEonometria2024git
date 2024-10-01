#Regresión lineal e interpretación de coeficientes
#Efectos marginales para modelos lineales y no lineales 
#Modelos logarítmicos 

#Cargar librerías 
library(ggplot2)
library(patchwork)
library(stargazer)
library(margins)

#Data
load("andy.rdata")
#sales: ventas (variable dependiente)
#price: precio
#advert: Gasto en publicidad 

View(andy)

#Modelo lineal
mod1<-lm(sales~price+advert,data=andy)
summary(mod1)

#Efectos marginales para un modelo lineal 
efecto<-margins(mod1)
margins_summary(mod1)
plot(efecto) #intervalos de confianza, si son comparables porque
            #efecto sobre la y (sales)
#Si se empalman dos efectos los dos efectos son iguales sobre la y (prueba de hipótesis visual)
#Linea horizontal en el 0 para la prueba de significancia: si cruza el cero podría no ser significativo 


#Modelo no lineal advert al cuadrado
mod2<-lm(sales~advert+I(advert^2),data=andy)
summary(mod2)
stargazer(mod2,type="text")

#Efectos marginales 
efecto2<-margins(mod2)
margins_summary(mod2)
plot(efecto2)

#Calculando efectos marginales a mano 
#Derivada del modelo con respecto a Advert
#Derivada=b1+2*b2*advert
#Calculamos en un valor fijo como el promedio o máximo
#En el promedio es para que te salga el mismo efecto que con margins 
coef<-mod2$coefficients
efm<-coef[2]+2*coef[3]*mean(andy$advert)

#Calculando efectos marginales en diferentes puntos 
efmultiple<-margins(mod2,at=list(advert=c(1,2,3)))
efmultiple2<-margins(mod2,at=list(advert=fivenum(andy$advert))) #Valores más importantes como la media, min, max
efmultipleplot<-cplot(mod2,"advert",what="effect")


#Modelo no lineal con más regresores
mod3<-lm(sales~price+advert+I(advert^2),data=andy)
stargazer(mod3,type="text")

#Efectos marginales 
efecto3<-margins(mod3)
margins_summary(mod3)
plot(efecto3)
cplot(mod3,"advert",what="effect",main="Advert")
cplot(mod3,"price",what="effect",main="Price") #Es constante porque price en el modelo está como término lineal,no cuadrado o similar 

#Sin Ceteris Paribus (variar las dos variables)
efmultiple3<-margins(mod3,at=list(advert=fivenum(andy$advert),
                                  price=fivenum(andy$advert)))
efmultiple3

paleta <- heat.colors(100)
image(mod3,col=paleta)
persp(mod3) #En Price es un triángulo escaleno porque es lineal 
#advert es una curva porque es un término cuadrático

#Modelos Logarítmicos
#Lineal
mod4<-lm(sales~price,data=andy)
#Lineal-Log
mod5<-lm(sales~log(price),data=andy)
#Log-Lineal
mod6<-lm(log(sales)~price,data=andy)
#Log-Log
mod7<-lm(log(sales)~log(price),data=andy)

stargazer(mod4,mod5,mod6,mod7, type="text")
#Lineal: CParibus, un aumento de un dólar en el precio conlleva una reducción de 7.829 miles de dólares en ventas
#Lineal-Log: CParibus, un aumento de 1%  en el precio está relacionado con una reducción de 0.4473 miles de dólares en ventas 
#Log-Lineal: CParibus, un aumento de un dólar en el precio conlleva a una reducción de 10.1% en ventas 
#Log-Log: CParibus,un aumento de 1%  en el precio está relacionado con una reducción de 0.575% en ventas

#Comparación de Scatter plots 
#Lineal
plot1<-ggplot(andy, aes(x = price, y = sales)) +
  geom_point(color = "blue") +
  geom_smooth(color="red",method="lm")+
  labs(title = "Sales vs Price",
       x = "Price",
       y = "Sales") +
  theme_bw()

#Lineal-Log
plot2<-ggplot(andy, aes(x = log(price), y = sales)) +
  geom_point(color = "blue") +
  geom_smooth(color="red",method="lm")+
  labs(title = "Sales vs Log(Price)",
       x = "log(Price)",
       y = "Sales") +
  theme_bw()
#Log-Lineal
plot3<-ggplot(andy, aes(x = price, y = log(sales))) +
  geom_point(color = "blue") +
  geom_smooth(color="red",method="lm")+
  labs(title = "Log(Sales) vs Price",
       x = "Price",
       y = "Log(Sales)") +
  theme_bw()

#Log-Log
plot4<-ggplot(andy, aes(x = log(price), y = log(sales))) +
  geom_point(color = "blue") +
  geom_smooth(color="red",method="lm")+
  labs(title = "Log(Sales) vs Log(Price)",
       x = "Log(Price)",
       y = "Log(Sales)") +
  theme_bw()

#Mostramos scatters 
(plot1|plot2)/(plot3|plot4)

