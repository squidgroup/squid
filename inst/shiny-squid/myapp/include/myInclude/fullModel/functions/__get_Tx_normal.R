### Generate the sampling time for each individual, each trait and each population

get_Tx_normal <- function(N, Time){  
  
  sdi    <- (Time$Te-Time$Ts)*Time$Vit/2       # Standard deviation for the individuals mean time sampling
  sde    <- (Time$Te-Time$Ts)*(1-Time$Vit)/2   # Standard deviation for the individuals sampling time
  
  # mean total sampling time
  Mu     <- Time$Ts + ((Time$Te-Time$Ts)/2)
  
  
  if(!Time$Dtime_Ind){
    
    if(!Time$Dtime_Trait){
      
      # Difference in sampling time among individuals and traits
      # Generate individual mean time sampling
      dateti <- rtruncnorm(length(N$NRI), a=Time$Ts, b=Time$Te, mean=Mu, sd=sdi)
      
      # Generate Time sampling for each individual 
      Tx     <- array(NA,dim=c(length(N$NRI),N$NR), dimnames=c("Individuals","Time"))
      myFun  <- function(x){Tx[x,1:N$NRI[x]] <<- as.integer(sort(rtruncnorm(N$NRI[x], a=Time$Ts, b=Time$Te, mean=dateti[x], sd=sde)))}
      M      <- sapply(1:length(N$NRI), myFun)
      
    }else{
      
      # Difference in sampling time among individuals and same among traits
      # Generate individual mean time sampling
      dateti <- rtruncnorm(length(N$NRI)/N$NT, a=Time$Ts, b=Time$Te, mean=Mu, sd=sdi)
      
      # Generate Time sampling for each individual 
      Tx     <- array(NA,dim=c(length(N$NRI),N$NR), dimnames=c("Individuals","Time"))
      myFun1 <- function(i){
                  pos <- ((i-1)*N$NT) + 1
                  Tx0 <- as.integer(sort(rtruncnorm(N$NRI[pos], a=Time$Ts, b=Time$Te, mean=dateti[i], sd=sde)))
                  
                  myFun2  <- function(x){
                    pos2 <- pos+x-1
                    Tx[pos2,1:N$NRI[pos2]] <<- Tx0                
                  }
                  M <- sapply(1:N$NT, myFun2)
               }
      M <- sapply(1:length(dateti), myFun1)
    }
    
  }else{
    
    if(!Time$Dtime_Trait){
      
      # Difference in sampling time among Traits and  same among individuals
      Tx <- NULL
      myFun1  <- function(i){ 
        
        dateti <- rtruncnorm(N$NT, a=Time$Ts, b=Time$Te, mean=Mu, sd=sdi)
    
        Tx0    <- array(NA,dim=c(N$NR, N$NT))
        myFun2 <- function(x){
                    pos <- (i-1) * N$NT * N$NI
                    Tx0[1:N$NRI[pos+x],x] <<- as.integer(sort(rtruncnorm(N$NRI[pos+x], a=Time$Ts, b=Time$Te, mean=dateti[x], sd=sde)))
                  }
        M      <- sapply(1:N$NT, myFun2)
        
        # Generate Time sampling for each individual 
        Tx     <<- rbind(Tx, t(array(Tx0,dim=c(N$NR, N$NI*N$NT), dimnames=c("Individuals","Time"))))
      }
      M      <- sapply(1:N$NP, myFun1)  
      
    }else{
      
      # Same time of sampling among individuals and traits
      Tx <- NULL
      myFun  <- function(x){  
        # Generate individual mean time sampling
        dateti <- rtruncnorm(1, a=Time$Ts, b=Time$Te, mean=Mu, sd=sdi)
        Tx0    <- as.integer(sort(rtruncnorm(N$NRI[1], a=Time$Ts, b=Time$Te, mean=dateti, sd=sde)))
        
        # Generate Time sampling for each individual 
        Tx     <<- rbind(Tx, t(array(Tx0,dim=c(N$NR, N$NI*N$NT), dimnames=c("Individuals","Time"))))
      }
      M      <- sapply(1:N$NP, myFun)
      
    }
  }

  return(Tx)
  
}