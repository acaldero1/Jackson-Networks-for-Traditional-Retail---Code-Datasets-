library("queueing")

# Setting working directory
setwd("C:/Users/idpdl/Downloads/CUC/2021-I/Jackson Networks")

#Reading Tables
#N1 <- read.csv('DemandaAltaNodo1.csv', header = T) #"Demanda alta" is spanish for high demand, "Nodo" is Spanish for Node 
#N2 <- read.csv('DemandaAltaNodo2.csv', header = T)

N1 <- read.csv('DemandaBajaNodo1.csv', header = T) #"Demanda Baja" is spanish for low demand, "Nodo" is Spanish for Node 
N2 <- read.csv('DemandaBajaNodo2.csv', header = T)

k <- 250 #Max No. of Customers


#Pre-locating the Outcomes to be calculated
Outcomes <- as.data.frame(matrix(c(rep(0,dim(N1)[1]*k*13)),dim(N1)[1]*k,13))
colnames(Outcomes) <- c("t","i","Throughput","L","W", "Lq1", "Lq2", 
                        "RO1", "RO2", "Throughput1", "Throughput2","w1", "w2")
counter <- 0

#Creating Transition Probability Matrix
prob <- matrix(c(0,1,0,0), nrow=2, ncol=2, byrow=TRUE)

for (t in 1:dim(N1)[1]) { #t is the time in hours
  for (i in 600) {  #i is the No. of customers
    counter <- counter+1
    lambda <- N1$Tasa_media_llegadas[t] #"Tasa media de llegadas" is Spanish for mean arrival rate
    mu <- N1$Tasa_media_.servicio[t] #"Tasa media de servicio" is Spanish for mean service rate
    lambda2 <- N2$Lambda[t]
    mu2 <- N2$Muh[t]
    n <- round(N2$L[t], digits = 0)
    
    #Setting up the model's inputs
    #For Node 1: MMCC
    n1 <- NewInput.MMInf(lambda, mu, -1)
    #For node 2: MMC
    n2 <- NewInput.MMC(lambda2, N2$S[t]*mu2, c=N2$S[t], method=0)
    
    i_OJN <- NewInput2.OJN(prob, list(n1, n2))
    
    #Building the model
    o_OJN <- QueueingModel(i_OJN)
    #Retrieving Outcomes
    Outcomes[counter,] <- c(t,i,o_OJN$Throughput,o_OJN$L,o_OJN$W, o_OJN$Lk[1], 
                            o_OJN$Lk[2], o_OJN$ROk[1], o_OJN$ROk[2],
                            o_OJN$Throughputk[1], o_OJN$Throughputk[2],
                            o_OJN$Wk[1], o_OJN$Wk[2])
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
