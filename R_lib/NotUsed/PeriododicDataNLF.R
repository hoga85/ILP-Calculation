#########################
#This function spit out the Nonlinear fitting of periodic signal
# Sgn <- Signal
# noHarmonic <- seq(2 or start harmonic apart from 1, end no of harmonic, gap)
#########################


PeriododicDataNLF <- function(Sgn,Sgn2,noHarmonic){
       #Sgn <- T1
       #Sgn2 <- f
	 #noHarmonic <- seq(1,10,1)

      noParameter <- length(noHarmonic)
	j <- 0
      p<- 0.1
      px <- 0.1
	q <- 0.1
	qx <- 0.1
      #initial guess of parameters
      #p[1:noParameter] <- rep(0.1,noParameter)
	#px[1:noParameter] <- rep(0.1,noParameter)
      #q[1:noParameter] <- rep(10,noParameter)
	#qx[1:noParameter] <- rep(10,noParameter)

########################################################      

	#ssp <- spectrum(Sgn)  
	#per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

      #equation1st <- sin(2*pi/per*t)+cos(2*pi/per*t)
	#equationNth <- 0
 
      # plotting the original data
	#rg <- diff(range(Sgn))
	#plot(Sgn~t,ylim=c(min(Sgn)-0.1*rg,max(Sgn)+0.1*rg))

      ########
      #reslm <- lm(Sgn ~ equation1st )
	#summary(reslm)
	#lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit
      #########
	
      ### including 1st to Nth harmonic #####
      
      #for (i in noHarmonic){
      #j<-j+1
      #equationNth <- equationNth+p[j]*sin(2*i*pi/per*t)+q[j]*cos(2*i*pi/per*t)
      #}

	#fit = nls(Sgn ~ a*cos(a1*sin(2*pi/per*t)) + b*sin(b1*sin(2*pi/per*t)), 
      #      start = list(a= p, a1 = px,b= p,b1 = qx))

########################################################
      fit = nls(Sgn2 ~ b*(Sgn)+a1, 
      	start = list( a1=px, b=q ))


      plot(Sgn,Sgn2,col=2)
	new = data.frame(Sgn = seq(min(Sgn),max(Sgn),len=100))
      par(new=T)
      plot(new$Sgn,predict(fit,newdata=new),col=1)
#########################################################
	#reslmNth <- lm(Sgn ~ equationNth)
	#summary(reslmNth)
	#lines(fitted(reslmNth)~t,col=3)    # solid green line is periodic with second harmonic
       return(fit)
}


      
    
