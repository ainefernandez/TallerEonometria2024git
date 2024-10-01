#Modelos de Regresión lineal: Variables cualitativas 
#Variables dicotómicas: Solo pueden tomar dos valores 0 y 1 e.g UNIcompleta (SI/NO)
#Variables categóricas: Pueden tomar más categorías e.g. Nivel educativo 

#Cargamos paquetes
library(tidyverse)
library(broom)
library(fastDummies)
library(stargazer)
library(GGally)

#Construcción de datos 
set.seed(178486)
n<-30
muestra<-data.frame(id=1:n,
                    edad=round(rnorm(n,25,5),0),
                    sexo=sample(x=c("Hombre","Mujer"),size=n,replace = T),
                    region=sample(x=c("Norte","Centro","Sur"),size=n,replace=T),
                    salario=rchisq(n,7000))
View(muestra)

#Visualización de variables cualitativas (Boxplots,violinplots)
boxplotregion<-ggplot(muestra,aes(x=region,y=salario))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Salario por región")

violinregion<-ggplot(muestra,aes(x=region,y=salario))+
  geom_violin()+
  theme_bw()+
  labs(title = "Salario por región")

boxplotsexo<-ggplot(muestra,aes(x=sexo,y=salario))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Salario por sexo")

violinsexo<-ggplot(muestra,aes(x=sexo,y=salario))+
  geom_violin()+
  theme_bw()+
  labs(title = "Salario por sexo")

#NO sirven los scatter para variables cualitativas 
scatter<-ggplot(muestra,aes(x=region,y=salario))+
  geom_point()+
  theme_bw()+
  labs(title = "Salario por región")

scatter2<-ggplot(muestra,aes(x=region,y=sexo))+
  geom_point()+
  theme_bw()+
  labs(title = "Salario por región")


muestra<-muestra %>%
  mutate(joven=factor(ifelse(edad<24,1,0)))

muestracondummies<-dummy_cols(muestra,select_columns =c("sexo","region"),remove_first_dummy = T)
View(muestracondummies)

#Cargamos los datos Wage 
data("Wage", package = "ISLR")

reg1<-lm(wage~health+age,data=Wage)
stargazer(reg1,type="text")

reg2<-lm(wage~race+age,data=Wage)
stargazer(reg2,type="text")

reg3<-lm(wage~race+age+age*race,data=Wage)
stargazer(reg3,type="text")

reg4<-lm(wage~race+jobclass+race*jobclass,data=Wage)
stargazer(reg4,type="text")

#Gráfico de la regresión 
#Salud
reg1plot<-ggplot(augment(reg1),aes(x=age,y=wage,color=health))+
  geom_point(alpha = 0.5)+
  geom_smooth(aes(y= .fitted))+ #para ver Ceteris Paribus
  theme_bw()+
  labs(title = "Relación entre salario y edad: Salud")

#Raza
reg2plot<-ggplot(augment(reg2),aes(x=age,y=wage,color=race))+
  geom_point(alpha = 0.5)+
  geom_smooth(aes(y= .fitted))+#para ver Ceteris Paribus
  theme_bw()+
  labs(title = "Relación entre salario y edad: Raza")

#Coeficientes  
coefplot<-ggcoef(reg1, vline = FALSE, exclude_intercept = TRUE, errorbar_height = .2, color = "blue") 
coefplot2<-ggcoef(reg2, vline = FALSE, exclude_intercept = TRUE, errorbar_height = .2, color = "blue") 
coefplot3<-ggcoef(reg3, vline = FALSE, exclude_intercept = TRUE, errorbar_height = .2, color = "blue") 
coefplot4<-ggcoef(reg4, vline = FALSE, exclude_intercept = TRUE, errorbar_height = .2, color = "blue") 
