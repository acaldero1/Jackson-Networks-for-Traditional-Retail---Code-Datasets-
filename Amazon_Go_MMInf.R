###Amazon Go Store: Queueing Model M/M/Inf
## This code was created by Ivan Portnoy and Andres Calderon on 30/07/2021
#install.packages("queueing") #If required
library("queueing")
#Reading Tables
N1 <- read.csv('DemandaAltaNodo1.csv',header = T)
k <- 200
#Pre-locating the Outcomes to be calculated
Outcomes <- as.data.frame(matrix(c(rep(0,dim(N1)[1]*k*8)),dim(N1)[1]*k,5))
colnames(Outcomes) <- c("t","i","Throughput","L","W")
counter <- 0
for (t in 1:dim(N1)[1]) { #t is the time in hours
  for (i in 10) {  #i is the No. of customers
    counter <- counter+1
    lambda <- N1$Tasa_media_llegadas[t] #"Tasa media de llegadas" is Spanish for mean arrival rate
    mu <- N1$Tasa_media_.servicio[t] #"Tasa media de servicio" is Spanish for mean service rate
    #Setting up the MMInf model's inputs
    i_mmInf <- NewInput.MMInf(lambda, mu)  
    #Building the model
    o_mmInf <- QueueingModel(i_mmInf)
    #Retrieving Outcomes
    Outcomes[counter,] <- c(t,i,o_mmInf$Throughput,o_mmInf$L,o_mmInf$W)
  }
}

## Plotting period-wise throughput 
library(ggplot2)
ggplot(Outcomes, aes(t, Throughput)) +
  xlab("time (h)")+
  ylab("Throughput (Customers/h)")+
  geom_point()+
  geom_line()

## Plotting period-wise L 
ggplot(Outcomes, aes(t, L)) +
  xlab("time (h)")+
  ylab("L (Customers)")+
  geom_point()+
  geom_line()

## Plotting period-wise W 
ggplot(Outcomes, aes(t, W)) +
  xlab("time (h)")+
  ylab("W (hours)")+
  geom_point()+
  geom_line()
