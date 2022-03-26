---
title: "parcial 1"
author: "jhon gutierrez"
date: '2022-03-25'
output: html_document
---

```{r}
#punto.1
hembras  <- c( 183.2 , 184.1 , 183.0 , 204.3 , 176.5 , 179.0 , 188.3 , 186.8 , 202.2 , 182.5 , 190.0 , 178.1
               , 193,2 , 180,4 , 184,3 , 189,2 , 189,1 , 203,1 , 166,8 , 196,3 , 193,3 , 187,3 , 185,8
               , 189.3 , 195.5 , 202.4 , 210.8 );

machos  <- c( 140.9  	, 173.9  	, 118.9
              , 121,7  	, 177,4  	, 140,0
              , 173,8  	, 154,8  	, 192,7
              , 154,5  	, 177,5  	, 134,4
              , 109.2  	, 153.4  	, 175.0
              , 150,7  	, 138,7  	, 169,8
              , 203.3  	, 136.7  	, 153.9
              , 163,0  	, 165,3  	, 176,7
              , 137,7  	, 126,7  	, 150,0 );
#1.a

hist(hembras , breaks = 5, main  =  " Distribución de tamaño en especies de langostinos hembra " ,
     xlab  =  " tamaño mm " , ylab  =  " Frecuencia " )

hist(machos ,breaks = 5 ,main  =  " Distribución de tamaño en especies de langostinos macho " ,
     xlab  =  " tamaño mm " , ylab  =  " Frecuencia ", col = "lightblue" )

#mirando resultado del histograma, se puede observar que los datos del tamaño de los machos están
# mas concentrados entre los 160-180 osea en una asimetrica positiva, y los datos de las
#hembras están más concentrados hacia la mediana y su distribución es normal


#1.b

promedioHembras  = mean( hembras );
promedioHembras

promedioMachos  = mean( machos );
promedioMachos

sdHembras  <- sd( hembras );
sdHembras

sdMachos  <- sd( machos );
sdMachos


#Se observa que los datos de las hembras están más agrupados,por lo tanto son más cercanos al valor que se espera,
#las muestras obtenidas de los machos, están más dispersas respecto a su media.

#1.c
#C
intervaloHembras <- t.test(hembras,conf.level = 0.97)$conf.int;
intervaloHembras
#El 97% de las hembras tienen una medida de entre los 184.7 mm a los 193.4 mm

intervaloMachos <- t.test(machos,conf.level = 0.97)$conf.int;
intervaloMachos
#El 97% de los machos tienen una medida de entre los 144.2 mm a los 164.6 mm

#1.D
bplotHembras = boxplot(hembras, main='tamaño de las hembras')
bplotMachos = boxplot(machos, main='tamaño de los machos')
#Las hembras tienen una distribucion simetrica donde el 25% de las hembras
#tiene medidas por debajo de 183mm y el 75% medidas por debajo de 185mm y
#Los machos tienen una asimetria positiva, en donde el 50% de los machos
#tiene una medida entre los 140mm y los 175mm

#1.E
shapiro.test(hembras);
#p>0.05, se tiene una distribución normal en las hembras
shapiro.test(machos)
#p>0.05, se tiene distribución normal en los machos



#punto.2


#2.a
pbinom(25,size = 900 ,prob = 0.015);
#La probabilidad de que a lo sumo salgan 25 falsos es de 99%


#2.b
probabilidadMayor20Falsos <- pbinom(20, size = 900, prob = 0.015, lower.tail = FALSE);
probabilidadMayor20Falsos
probabilidadMayor30Falsos <- pbinom(30, size = 900, prob = 0.015, lower.tail = FALSE);
probabilidadMayor30Falsos

probabilidadEntre20y30 <- probabilidadMayor20Falsos - probabilidadMayor30Falsos;
probabilidadEntre20y30
#La probabilidad de que salgan entre 20 y 30 falsos es de 0.033 -> 3.3%

#2.c
probMayor10Falsos <- pbinom(10, size = 900, prob = 0.015, lower.tail = FALSE);
probMayor10Falsos;
#La probabilidad de que mas de 10 sean falsos es de
#0.79 -> 79%

plot(dbinom(1:25, size = 900, prob = 0.015), type = "h", lwd = 2,
     main = "Función de probabilidad binomial",
     ylab = "P(X = x)", xlab = "Número de éxitos")

lines(dbinom(20:30, size = 900, prob = 0.015), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))

# n = 80, p = 0.3
lines(dbinom(10, size = 900, prob = 0.015), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))


```
