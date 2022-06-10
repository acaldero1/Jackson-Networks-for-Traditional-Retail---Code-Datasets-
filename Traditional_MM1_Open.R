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

for (t in 1:dim(N1)[1]) { #t is the time in hours
  for (i in 1) {  #i is the No. of customers
    counter <- counter+1
    lambda <- N1$Tasa_media_llegadas[t] #"Tasa media de llegadas" is Spanish for mean arrival rate
    mu <- N1$Tasa_media_.servicio[t] #"Tasa media de servicio" is Spanish for mean service rate
    lambda2 <- N2$Lambda[t]
    mu2 <- N2$Muh[t]
    n <- round(N2$L[t], digits = 0)
    
    #Creating Transition Probability Matrix
    prob <- matrix(rep(0,((i+N2$S[t])*(i+N2$S[t]))), nrow=(i+N2$S[t]),
                   ncol=(i+N2$S[t]), byrow=TRUE)
    temp <- unlist(lapply(1:i, function(b){
      paste0("c", toString(b))
    }))
    temp2 <- unlist(lapply(1:N2$S[t], function(b){
      paste0("s", toString(b))
    }))
    colnames(prob) <- append(temp, temp2)
    rownames(prob) <- append(temp, temp2)
    prob[grepl("c", rownames(prob)), grepl("s", colnames(prob))] <- (1/(N2$S[t]))
    #prob[grepl("s", rownames(prob)), grepl("c", colnames(prob))] <- 1/i
    
    #Reading No. of servers
    S <- N2$S[t]
    
    #Creating List to be fed to the OJN model
    List_t <- {}
    for (j in 1:(i+S)) {
      #For node 1: MMInf ; For node 2: MM1
      if(j<=i){
        n_temp <- NewInput.MMInf(lambda, mu, i)
      }
      
      if(j>i){
        n_temp <- NewInput.MM1(lambda2, mu2, -1)  
      }
      
      assign(paste("n.", as.character(j), sep=""), n_temp)
      if(j< (S+i)){
        List_t <- paste(List_t, paste0("n.", as.character(j), ",", sep=""))
      }
      if(j== (S+i)){
        List_t <- paste(List_t, paste0("n.", as.character(j), sep=""))
      }
    }
    List_t <- paste0("(", List_t, ")")
    
    eval(parse(text = paste("List_t2 <-", paste0("list", List_t))))
    
    
    i_OJN <- NewInput2.OJN(prob, List_t2)
   
    #Building the model
    o_OJN <- QueueingModel(i_OJN)
    #Retrieving Outcomes
    Outcomes[counter,] <- c(t,i,o_OJN$Throughput,o_OJN$L,o_OJN$W, sum(o_OJN$Lk[1:i]), 
                            sum(o_OJN$Lk[(i+1):(S+i)]), sum(o_OJN$ROk[1:i]), o_OJN$ROk[i+1],
                            sum(o_OJN$Throughputk[1:i]), sum(o_OJN$Throughputk[(i+1):(S+i)]),
                            o_OJN$Wk[1], o_OJN$Wk[i+1])
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
