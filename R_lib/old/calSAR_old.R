#t
#f
#T1
#Interval <- 60
#Concentration <- 10 
#specificC <- 4.18

calSAR <- function(t,T1,H,f,Interval, Concentration, specificC){

	endIndext<- length(t)
	i<-0
	deltaT1<-0
      NumCalculation <-0
      MeasureTime <- 0 
	Hi <- 0
	fi <- 0
	SARi <- rep(1,NumCalculation,0)
      ILP <- rep(1,NumCalculation,0)

		MeasureTime <- t(endIndext) - t(1)
		NumCalculation <-  floor(MeasureTime / Interval)*2 -2

	for (i in 1:NumCalculation){

	  meanT1 <- mean(T1[(0.5*Interval*(i-1)+1):(0.5*Interval*(i-1)+Interval)])
        deltaT1[i] <- meanT1 - T1[1]
        Hi[i] <- mean(H[(0.5*Interval*(i-1)+1):(0.5*Interval*(i-1)+Interval)])
        fi[i] <- mean(f[(0.5*Interval*(i-1)+1):(0.5*Interval*(i-1)+Interval)])

        tInterval <- t[(0.5*Interval*(i-1)+1):(0.5*Interval*(i-1)+Interval)]
        T1Interval <- T1[(0.5*Interval*(i-1)+1):(0.5*Interval*(i-1)+Interval)]

        #print(tInterval)
       
    	 	 fo <- T1Interval~tInterval
		 fit <-lm(formula = fo)
             slope <- coef(fit)[2]
             		names(slope) <- "SARi" 
             

        SARi[i]<- 1000*(specificC*slope)/Concentration 
        #ILPi[i]<- SARi[i]/((H[i]^2)*f[i])
	}
        
        FoSARi <- function(L,foSARi) {foSARi <- sd(SARi + L*deltaT1)}
        L <- optimize(FoSARi,c(0,1000))$minimum  # unit: mW/K.g

        Time <- (1:NumCalculation)*0.5*Interval
        SAR <- SARi + L*deltaT1
        ILP <- (SAR/(fi*(Hi^2)))*10^12
        
	layout(matrix(c(1:2),nrow = 1, ncol =2)) # structure of plots
        plot(Time,SAR,pch=1,col=1) 
        plot(Time,ILP,pch=2,col=2) 
        meanSAR <- mean(SAR)
        sdSAR <- sd(SAR)
        meanILP <- mean(ILP)
        sdILP <- sd(ILP)
        new <- data.frame(meanSAR,sdSAR,meanILP,sdILP,L)  
        return(new)  

}

	
