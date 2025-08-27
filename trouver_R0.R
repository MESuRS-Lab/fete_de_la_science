
library(deSolve)

find_R0 = function(){
  
  colours = c("red", "blue", "green", "orange", "purple",
              "slateblue", "green4", "grey20",
              "tomato", "steelblue", "tan4", "seagreen3")
  
  SIR.model <- function(t, pop, param) {
    with(as.list(c(pop, param)), {
      N=S+I+R
      dS <- -beta*S*I/N
      dI <- beta*S*I/N  - gamma*I
      dR <- gamma*I
      dnI <- beta*S*I/N 
      res<-c(dS, dI, dR, dnI)
      list(res)
    })
  }
  
  gamma=1/4
  beta = gamma*4.2
  dt=1
  Tmax=11
  N=1500
  
  I0=10
  S0=N-I0
  R0=0
  
  Time=seq(from=0,to=Tmax,by=dt)
  Init.cond=c(S=S0,I=I0, R=R0, nI=0) 
  param=c(beta,gamma)
  
  real_result <- as.data.frame(lsoda(Init.cond, Time, SIR.model, param))
  
  plot(c(1:5),real_result$I[1:5],type="p",col="black",
       xlab="Temps (heures)",ylab="Nombre de personnes malades",
       main="Objectif : faire passer la ligne à travers tous les points !",
       ylim=c(0,1000), bty="n", cex=1, pch=19,
       xlim = c(1,12), xaxt="n")
  axis(side = 1, at=c(1:12))
  
  attempt = 1
  
  R0_tried = c()
  
  repeat{
    
    cat("Quelle valeur de R0 doit-on essayer ?\n")
    R0_to_try = readline("")
    
    if(grepl("[A-Za-z]", R0_to_try)){
      cat("R0 doit être un nombre !\n")
    } else {
      
      R0_to_try = gsub(",", ".", R0_to_try)
      
      R0_to_try = as.numeric(R0_to_try)
      R0_tried = c(R0_tried, R0_to_try)
      
      beta = gamma*R0_to_try
      param=c(beta,gamma)
      
      test_result <- as.data.frame(lsoda(Init.cond, Time, SIR.model, param))
      
      lines(c(1:12),test_result$I, lwd = 2,
            col = colours[attempt])
      legend("topright", legend = R0_tried, col = colours[1:length(R0_tried)],
             lty = 1, lwd = 2)
      
      
      if(R0_to_try == 4.2){
        cat("Bravo ! C'est la bonne valeur")
        break
      }
      
      attempt = attempt+1
      
      if(attempt == 5) cat("Indice : c'est entre 4 et 5...\n")
      
    }
  }
  
}

find_R0()