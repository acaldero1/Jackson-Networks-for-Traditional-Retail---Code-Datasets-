library("queueing")

# Setting working directory
setwd("C:/Users/idpdl/Downloads/CUC/2021-I/Jackson Networks")

#Reading Tables
N1 <- read.csv('DemandaAltaNodo1.csv', header = T) #"Demanda alta" is spanish for high demand, "Nodo" is Spanish for Node 
N2 <- read.csv('DemandaAltaNodo2.csv', header = T)

#N1 <- read.csv('DemandaBajaNodo1.csv', header = T) #"Demanda Baja" is spanish for low demand, "Nodo" is Spanish for Node 
#N2 <- read.csv('DemandaBajaNodo2.csv', header = T)

k <- 400 #Max No. of Customers


#Pre-locating the Outcomes to be calculated
Outcomes <- as.data.frame(matrix(c(rep(0,dim(N1)[1]*k*13)),dim(N1)[1]*k,13))
colnames(Outcomes) <- c("t","i","Throughput","L","W", "Lq1", "Lq2", 
                        "RO1", "RO2", "Throughput1", "Throughput2","w1", "w2")
counter <- 0

#Creating Transition Probability Matrix
prob <- matrix(c(0,1,1,0), nrow=2, ncol=2, byrow=TRUE)

for (t in 1:dim(N1)[1]) { #t is the time in hours
  for (i in 1:k) {  #i is the No. of customers
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
  
    i_CJN <- NewInput2.CJN(prob, n=i, z=0, operational=FALSE, 
                           method=0, tol=0.001, list(n1, n2))
    # i_CJN <- NewInput.CJN(prob, i, z=0, operational=FALSE, 
    #                        method=0, tol=0.001, n1, n2,)
    
    #Building the model
    o_CJN <- QueueingModel(i_CJN)
    #Retrieving Outcomes
    Outcomes[counter,] <- c(t,i,o_CJN$Throughput,o_CJN$L,o_CJN$W, o_CJN$Lk[1], 
                            o_CJN$Lk[2], o_CJN$ROk[1], o_CJN$ROk[2],
                            o_CJN$Throughputk[1], o_CJN$Throughputk[2],
                            o_CJN$Wk[1], o_CJN$Wk[2])
  }
}

##Determining optimum capacity (period-wise) and plotting throughput results
#Pre-locating the Optimum Capacity Vector
Optimum_Capacity <- as.data.frame(c(rep(0,dim(N1)[1])),)
par(mfrow=c(3,4))
for (t in 1:dim(N1)[1]) {
  temp <- Outcomes[(k*(t-1)+1):(k*t),]
  #Here, we use a cutoff of 99.33% of the maximum Throughput to define the optimum*
  x <- min(temp$i[temp$Throughput>=(1-exp(-5))*max(temp$Throughput)]) 
  Optimum_Capacity[t,1] <- x
  plot(temp$i, temp$Throughput, main=paste("t =",toString(t),"h"),
       xlab="Capacity (Customers)", ylab="Throughput")
  abline(v=x, col="green") #Green vertical line for optimum capacity
  abline(v=177, col="red") #Red vertical line for epidemiology-based capacity threshold
}
colnames(Optimum_Capacity) <- "Opt_Cap"
Optimum_Capacity$time <- 1:dim(N1)[1]
Optimum_Capacity <- as.data.frame(Optimum_Capacity)
## Plotting period-wise optimum capacity + epidemiology-based capacity threshold
library(ggplot2)
ggplot(Optimum_Capacity, aes(time, Opt_Cap)) +
  xlab("time (h)")+
  ylab("Optimum Capacity (Customers)")+
  geom_point()+
  geom_line()+
  #Adding horizontal dashed red line for the epidemiology-based capacity threshold
  geom_hline(yintercept=177, linetype="dashed", color = "red")

#write.csv(Outcomes, file = "OutcomesHD_Paper3.csv")


##Plotting Cycle Time (W) results
#Pre-locating the Optimum Capacity Vector
Optimum_Capacity_W <- as.data.frame(c(rep(0,dim(N1)[1])),)
par(mfrow=c(3,4))
for (t in 1:dim(N1)[1]) {
  temp <- Outcomes[(k*(t-1)+1):(k*t),]
  #Here, we use a cutoff of 99.33% of the maximum Throughput to define the optimum*
  x <- min(temp$i[temp$W<=min(temp$W)]) 
  Optimum_Capacity_W[t,1] <- x
  plot(temp$i, temp$W, main=paste("t =",toString(t),"h"),
       xlab="Capacity (Customers)", ylab="Cycle Time (h)")
 # abline(v=x, col="green") #Green vertical line for optimum capacity
  abline(v=177, col="red") #Red vertical line for epidemiology-based capacity threshold
}
colnames(Optimum_Capacity_W) <- "Opt_Cap"
Optimum_Capacity_W$time <- 1:dim(N1)[1]
Optimum_Capacity_W <- as.data.frame(Optimum_Capacity_W)




##Plotting WIP (L) results
#Pre-locating the Optimum Capacity Vector
par(mfrow=c(3,4))
for (t in 1:dim(N1)[1]) {
  temp <- Outcomes[(k*(t-1)+1):(k*t),]
  #Here, we use a cutoff of 99.33% of the maximum Throughput to define the optimum*
  plot(temp$i, temp$L, main=paste("t =",toString(t),"h"),
       xlab="Capacity (Customers)", ylab="WIP (Customers)")
  abline(v=177, col="red") #Red vertical line for epidemiology-based capacity threshold
}

