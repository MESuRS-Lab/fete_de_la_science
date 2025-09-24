
library(deSolve)

find_perc = function(){
  
  colours = c("red", "blue", "green", "orange", "purple",
              "slateblue", "green4", "grey20",
              "tomato", "steelblue", "tan4", "seagreen3")
  
  SIR.model <- function(t, pop, param) {
    with(as.list(c(pop, param)), {
      
      if(t>=4) beta = beta*(1-perc_to_try/100)
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
  perc_to_try = 0
  dt=1
  Tmax=11
  N=1500
  
  I0=10
  S0=N-I0
  R0=0
  
  Time=seq(from=0,to=Tmax,by=dt)
  Init.cond=c(S=S0,I=I0, R=R0, nI=0) 
  param=c(beta,gamma, perc_to_try)
  
  real_result <- as.data.frame(lsoda(Init.cond, Time, SIR.model, param))
  
  plot(c(1:12),real_result$I,type="p",col="black",
       xlab="Temps (heures)",ylab="Nombre de personnes malades",
       main="Objectif : rester sous la ligne en pointillés !",
       ylim=c(0,1000), bty="n", cex=1, pch=19,
       xlim = c(1,12), xaxt="n")
  axis(side = 1, at=c(1:12))
  abline(a=real_result$I[5],b=0, lty = "dashed", lwd=1)
  
  attempt = 1
  
  perc_tried = c()
  
  repeat{
    
    cat("Quel pourcentage de visiteurs doivent se laver les mains AU MINIMUM ?\n")
    perc_to_try = readline("")
    
    if(grepl("[A-Za-z]", perc_to_try)){
      cat("Le pourcentage doit être un nombre !\n")
    } else {
      
      perc_to_try = gsub(",", ".", perc_to_try)
      
      perc_to_try = as.numeric(perc_to_try)
      
      if(perc_to_try < 0 | perc_to_try > 100){
        cat("Le pourcentage doit être entre 0 et 100 !\n")
      } else {
        
        perc_tried = c(perc_tried, perc_to_try)
        
        param=c(beta,gamma,perc_to_try)
        
        test_result <- as.data.frame(lsoda(Init.cond, Time, SIR.model, param))
        
        lines(c(1:12),test_result$I, lwd = 2,
              col = colours[attempt])
        legend("topright", legend = perc_tried, col = colours[1:length(perc_tried)],
               lty = 1, lwd = 2)
        
        if(perc_to_try %in% c(72:76)) cat("Un tout petit peu plus...\n")
        if(perc_to_try %in% c(78:82)) cat("Un tout petit peu moins...\n")
        
        if(perc_to_try == 77){
          cat("Bravo ! C'est la bonne valeur")
          break
        }
        
        attempt = attempt+1

      }
    }
  }
  
}

find_perc()