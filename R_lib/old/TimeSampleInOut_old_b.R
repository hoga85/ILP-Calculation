t.SampleInOut <- function(t,f){



###Fdelta1###
Fdelta1 <- 0
gap<-1
gapdlt1 <- seq((1+gap),length(f),gap)

for(index in gapdlt1){
Fdelta1[index-(length(f)-length(gapdlt1))] <- f[index] - f[index-gap]
}

maskMax <- (Fdelta1 == max(Fdelta1))*1
IndexMax1 <- max(c(1,seq((1+gap),length(f),gap))*maskMax)

maskMin <- (Fdelta1 == min(Fdelta1))*1
IndexMin1 <- max(c(1,seq((1+gap),length(f),gap))*maskMin)


###Fdelta4###
Fdelta4 <- 0
gap<-4
k<-1
gapdlt4 <- seq((1+gap),length(f),gap)

for(index in gapdlt4){
Fdelta4[k] <- f[index] - f[index-gap]
k<-k+1
}

maskMax <- (Fdelta4 == max(Fdelta4))*1
IndexMax4 <- max(c(1,seq((1+gap),length(f),gap))*maskMax)

maskMin <- (Fdelta4 == min(Fdelta4))*1
IndexMin4 <- max(c(1,seq((1+gap),length(f),gap))*maskMin)


IndexMax1 == IndexMax4
IndexMin1 == IndexMin4



}