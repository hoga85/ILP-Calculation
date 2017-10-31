#t
#f
#Initial parameters:
#Interval <- 60 sec
#Concentration <- 10 mg/ml 
#specificC <- 4.18 J/k.g

InitialcalSAR <- function(t,T1,H,f,T3,Interval,FILENAME, Concentration, specificC,startTime,plotOn,saveOn){

	endIndext<- length(t)
	i<-0
      k<-0
      A<-0
	deltaT1<-0
	deltati<-0
      NumCalculation <-0
      MeasureTime <- 0 
	Hi <- 0
	fi <- 0
      T3i <- 0
      meanti<-0
      meanDeltaT1<-0
	SARi <- 0
      ILP <- 0

		MeasureTime <- t[endIndext] #- t[startTime]
		NumCalculation <-  MeasureTime-Interval
            deltati <- t - t[1]
            deltaT1 <- T1 - min(T1)
            ti <- t[startTime:NumCalculation]

	for (i in startTime:NumCalculation){
        k<-k+1
        #meanti[k] <- mean(t[seq(i,i+Interval-1,1)])

	  meanDeltaT1[k] <- mean(deltaT1[seq(i,i+Interval-1,1)])
        #Hi[k] <- mean(H[seq(i,i+Interval-1,1)])
        #fi[k] <- mean(f[seq(i,i+Interval-1,1)])
	      Hi[k] <- H[1]
	      fi[k] <- f[1]
	      T3i[k] <- mean(T3[seq(i,i+Interval-1,1)])
        tInterval <- deltati[seq(i,i+Interval-1,1)]
        T1Interval <- deltaT1[seq(i,i+Interval-1,1)]

        #print(tInterval)
       
    	 	  fo <- T1Interval~tInterval
            # fo <- tInterval~T1Interval
	        fit <-lm(formula = fo)
              slope <- coef(fit)[2]
             #print(slope)
             #names(slope) <- "SARi" 
             #slope <-  deltaT1[k]/deltati[k]

        SARi[k]<- 1000*(specificC*slope)/Concentration 
        #ILPi[i]<- SARi[i]/((H[i]^2)*f[i])
	}
        
        FoSARi <- function(A,foSARi) {foSARi <- sd(SARi + A*meanDeltaT1)}
        A <- optimize(FoSARi,c(0,1000))$minimum  # unit: mW/K.g
        L <- A*(Concentration/1000)
        Time<-ti
        SAR <- SARi + A*meanDeltaT1
        ILP <- (SAR/(fi*(Hi^2)))*10^6

		Time <- Time
        	SAR <- SAR
		ILP <- ILP

        if(plotOn){        
	  layout(matrix(c(1:2),nrow = 1, ncol =2)) # structure of plots
        plot(Time,SAR,pch=1,col=1) 
        plot(Time,ILP,pch=2,col=2) }


        meanSAR <- mean(SAR)
        sdSAR <- sd(SAR)
        meanILP <- mean(ILP)
        sdILP <- sd(ILP)
        newDeltaT1 <- meanDeltaT1
        
        feedback <- data.frame(FILENAME,Interval,mean(Hi),mean(fi), Concentration,meanSAR,sdSAR,meanILP,sdILP,L,mean(T3i),T1[1])
        names(feedback)[2]<- "Sample"
        names(feedback)[2]<- "Interval (sec)"
        names(feedback)[3]<- "H (kA/m)"
        names(feedback)[4]<- "f (Hz)"
      
        print(feedback)
        ILPcsvName<- paste0("ILP_",substr(FILENAME,1,nchar(FILENAME)-4),".csv")
        #write.csv(feedback,"CurrentILP.csv")
 
        if(saveOn){
        write.table(feedback,  ILPcsvName, row.names=F, na="NA",             
                    append = T, quote= FALSE, sep = ",", col.names = T)}

        new <- data.frame(Time,SAR,ILP,L,newDeltaT1,Hi,sdSAR)  
        return(new)  


}

	
