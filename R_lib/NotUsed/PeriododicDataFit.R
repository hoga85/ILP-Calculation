#########################
#This function spit out the fitted equation of periodic signal
# Sgn <- Signal
# noHarmonic <- seq(2 or start harmonic apart from 1, end no of harmonic, gap)
#########################


eq.periodicSgn <- function(Sgn,noHarmonic){
      #Sgn <- T1
      #noHarmonic <- seq(2,2,1)
	ssp <- spectrum(Sgn)  
	per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]

      equation1st <- sin(2*pi/per*t)+cos(2*pi/per*t)
	equationNth <- equation1st

      reslm <- lm(Sgn ~ equation1st )
	summary(reslm)

	rg <- diff(range(Sgn))
	plot(Sgn~t,ylim=c(min(Sgn)-0.1*rg,max(Sgn)+0.1*rg))
	lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit

	### including 2nd to Nth harmonic #####
      for (i in noHarmonic){
      equationNth <- equationNth+sin(2*i*pi/per*t)+cos(2*i*pi/per*t)
      }
	reslmNth <- lm(Sgn ~ equationNth)
	summary(reslmNth)
	lines(fitted(reslmNth)~t,col=3)    # solid green line is periodic with second harmonic
  

}

