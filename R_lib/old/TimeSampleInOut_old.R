t.SampleInOut <- function(t,f){

difF <- range(f)[2] - range(f)[1]

NoData <- 4
layout(matrix(c(1:NoData),nrow =NoData, ncol =1)) # structure of plots
###Fdelta1###
Fdelta1 <- 0
gap<-1
gapdlt1 <- seq((1+gap),length(f),gap)

for(index in gapdlt1){
Fdelta1[index-(length(f)-length(gapdlt1))] <- f[index] - f[index-gap]
}

tf1 <-1:length(Fdelta1)
plot(tf1, Fdelta1)


###Fdelta2###
Fdelta2 <- 0
gap<-2
k<-1
gapdlt2 <- seq((1+gap),length(f),gap)

for(index in gapdlt2){

Fdelta2[k] <- f[index] - f[index-gap]
k<-k+1
}

tf2 <-1:length(Fdelta2)
plot(tf2, Fdelta2)

###Fdelta4###
Fdelta4 <- 0
gap<-4
k<-1
gapdlt4 <- seq((1+gap),length(f),gap)

for(index in gapdlt4){
Fdelta4[k] <- f[index] - f[index-gap]
k<-k+1
}

tf4 <-1:length(Fdelta4)
plot(tf4, Fdelta4)

###Fdelta8###
Fdelta8 <- 0
gap<-8
k<-1
gapdlt8 <- seq((1+gap),length(f),gap)

for(index in gapdlt8){
Fdelta8[k] <- f[index] - f[index-gap]
k<-k+1
}

tf8 <-1:length(Fdelta8)
plot(tf8, Fdelta8)






}