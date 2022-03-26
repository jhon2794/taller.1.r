#1
poblacion <- c(29.66, 7.61, 6.35, 4.10, 3.8, 2.75, 2.70, 2.69, 2.51, 2.44,
               77.81, 35.50, 24.79, 20.02, 16.70, 9.39, 9.38, 8.76, 7.63, 7.5,
               8.68, 7.31, 6.02, 2.64, 1.75, 1.72, 1.51, 1.42, 1.31, 1.29,
               110.21, 32.14, 18.88, 16.38, 13.37, 11.92, 10.71, 7.8, 7.7, 7);
paises <- c("argentina","argentina","argentina","argentina","argentina","argentina",
            "argentina","argentina","argentina","argentina","usa","usa","usa","usa","usa",
            "usa","usa","usa","usa","usa","holanda", "holanda", "holanda","holanda","holanda",
            "holanda","holanda","holanda","holanda","holanda","japon", "japon",
            "japon","japon","japon","japon","japon","japon","japon","japon");
paises <- factor(paises); paises
#a

bplot = boxplot(poblacion~paises, main='Población de ciudades por país en el año 1967');

mediageneral = tapply(poblacion, paises, mean); mediageneral
#b
xi <-  seq(bplot$n)
points(xi, mediageneral, col = 'green', pch=18);
desvest = tapply(poblacion, paises, sd); desvest

arrows(xi, mediageneral-desvest, xi, mediageneral+desvest,code =3, col='blue', angle = 75, length = .1)
#2
probInasistenciaPasajeros <- 0.02
lambda <- round(180-(180*probInasistenciaPasajeros)); lambda

probMenor181Pasajeros <- ppois(181, lambda = lambda); probMenor181Pasajeros*100;

plot = plot(ppois(0:181, 176), type = "s", lwd = 2,
            main = "Función de distribución de probabilidad para avión airbus A320",
            xlab = "Número  pasajeros", ylab = "F(x)")+
  arrows(180,probMenor181Pasajeros,0,probMenor181Pasajeros,col = 'red', pch=18);
#3

media <- 400;
desvEstandar <- 5;
PNC <- 415;

probPNC <- pnorm(PNC, mean = media, sd = desvEstandar, lower.tail = FALSE);
porcentajePNC<-probPNC*100;
porcentajePNC;

plot = plot(pnorm(0:500, mean=media, sd = desvEstandar, lower.tail = FALSE), type = "s", lwd = 2,
            main = "distribución de probabilidad para productos no conformes (coca cola)",
            xlab = "Cantidad de mL", ylab = "f(x)")+points(415:500,pnorm(415:500, mean=media, sd = desvEstandar, lower.tail = FALSE), col="blue");
