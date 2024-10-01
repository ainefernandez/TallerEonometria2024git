#Regresión lineal, interpretación de coeficientes 

#Librerías 
library(ggplot2)
library(stargazer)
library(patchwork)
library(margins)
library(tidyverse)
library(gridExtra)
library(corrplot)

#Cargamos los datos 
load("andy.rdata")

#Inspeccionamos los datos 
View(andy)

#EDA
#sales: ventas
#price: precio
#advert: Gasto en publicidad 
summary(andy)

#Estadísticos descriptivos 
stats<-stargazer(andy, header=FALSE, 
          type="text", title="Estadísticos descriptivos",
          digits=1,summary.stat=c("n","mean","p25","median","p75","sd","min","max"),
          flip=TRUE)

#Histogramas
hist1 <- ggplot(andy, aes(x = price)) + 
  geom_histogram(fill = "lightcyan", color = "black") + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_bw() + 
  labs(title = "Price", 
       x = "Price", 
       y = "Frecuencia")

hist2 <- ggplot(andy, aes(x = sales)) + 
  geom_histogram(fill = "lightcyan", color = "black") + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_bw() + 
  labs(title = "Sales", 
       x = "Sales", 
       y = "Frecuencia")

hist3 <- ggplot(andy, aes(x = advert)) + 
  geom_histogram(fill = "lightcyan", color = "black") + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_bw() + 
  labs(title = "Advertisement", 
       x = "Advertisement", 
       y = "Frecuencia")
#Mostrar plots
grid.arrange(hist1, hist2, hist3, ncol = 3)

#Scatterplots
plot1<-ggplot(andy, aes(x = price, y = sales)) +
  geom_point(color = "blue") +
  geom_smooth(color="red",method = "lm")+
  labs(title = "Sales vs Price",
       x = "Price",
       y = "Sales") +
  theme_bw()


plot2<-ggplot(andy, aes(x = advert, y = sales)) +
  geom_point(color = "blue") +
  geom_smooth(color="red",method="lm")+
  labs(title = "Sales vs Advertisement",
       x = "Advertisement",
       y = "Sales") +
  theme_bw()

#Paquete patchwork
plot1+plot2
  plot_layout(ncol=2)

(plot1|plot2)/(hist1|hist2)

#Mostrar plots 
grid.arrange(plot1, plot2, ncol = 2)

#Matriz de Correlaciones
correlaciones <- cor(andy)
matrizCor<-stargazer(correlaciones, header=FALSE, 
          type="text", title="Matriz de correlaciones",
          digits=1)

#Plot de correlaciones
corrplot(correlaciones)


#Análisis de regresión 
reg1<-lm(sales~advert,data=andy)
summary(reg1)
reg2<-lm(sales~price,data=andy)
summary(reg2)
reg3<-lm(sales~price+advert,data=andy)
summary(reg3)
stargazer(reg1,reg2,reg3,header=FALSE, 
                   type="text", title="Resultados de modelos de regresión",
                    digits=1,ci=T,ci.level = 0.99)




#Efectos marginales 
margins(reg1)
margins(reg2)
margins(reg3)

margins_summary(reg3)
plot(margins(reg3))
