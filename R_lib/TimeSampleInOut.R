#########################
#This function spit out the index of f, when a sample is placed in and out the MACH
#########################


t.SampleInOut <- function(f){
f<-f
IndexMax <- 0
IndexMin <- 0

for(gap in 1:100){
	Fdelta <- 0
	k<-1
	gapdlt <- seq((1+gap),length(f),gap)
		for(index in gapdlt){
		Fdelta[k] <- f[index] - f[index-gap]
		k<-k+1
		}
      avgFdelta <- mean(Fdelta)
      thresholdMax <- 0.1*(max(Fdelta) - avgFdelta)
      thresholdMin <- 0.1*(min(Fdelta) - avgFdelta )

	maskMax <- (Fdelta > thresholdMax)*1
      PremaskMax <- seq((1+gap),length(f),gap)*maskMax
      minusIndex <- (1:length(PremaskMax))*((PremaskMax==0)*1)  
	PremaskMax <- PremaskMax[-minusIndex] 
      IndexMax[gap] <- min(PremaskMax)

	maskMin <- (Fdelta < thresholdMin)*1
	IndexMin[gap] <- max(seq((1+gap),length(f),gap)*maskMin)
}

t.SampleOut <- IndexMax[levels =max(table(IndexMax))]
t.SampleIn <- IndexMin[levels =max(table(IndexMin))]

return(c(t.SampleIn, t.SampleOut))

}

