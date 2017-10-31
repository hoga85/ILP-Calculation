###Before applying this fucntion, ##########################
###Parameters need to be defined as below in the main.R#####
#data1 <- read.table("A000.txt",header = T)
#data2 <- read.table("A001.txt",header = T)
#data3 <- read.table("C001.txt",header = T)
#data4 <- read.table("C002.txt",header = T)
#data5 <- read.table("C002b.txt",header = T)
#data6 <- read.table("C003.txt",header = T)
#dataAll <- c("data1","data2","data3","data4","data5","data6")
#TimeFactor <- 1000
#SampleName <- c("A000","A001","C001","C002","C002b","C003")
#dataReduceOn <- F or T
#plotOn <- F or T
############################################################


multiplePlot <- function(startData,EndData,TimeFactor,SampleName,dataAll, dataReduceOn, plotOn){


if (EndData - startData +1 > 4){
	noRow <- ceiling((EndData - startData +1)/2)
	noCol <- 2
	}else{
	noRow <- EndData - startData +1
	noCol <- 1
}



for(i in startData:EndData)
{data <- get(dataAll[i])

 f <- data$Frequency 


if (dataReduceOn ){
##Find the Index of Sample in and out##
t.SampleIn <- t.SampleInOut(f)[1] +5
t.SampleOut <- t.SampleInOut(f)[2] -30


##Redefine the range of data##
data<-data[t.SampleIn:t.SampleOut,]
}

 f <- data$Frequency 
 t <- (data$Time - min(data$Time))/TimeFactor
 T1 <- data$T1
 H <- data$H
 T2 <- data$T2
 T3 <- data$T3
#############################################
if (plotOn == TRUE){

#layout(matrix(c(1:(noRow*noCol)),nrow = noRow, ncol =noCol)) # structure of plots
par(mfrow=c(noRow,noCol))
par(mar=c(5, 4, 4, 5) + 0.05)

#plot(NA,NA,xlim=c(0,length(f)),ylim = range(T1))
plot(t,T1,pch = 1,col = 1, 
     xlim= range(t),ylim = range(T1),
     xlab = "", 
     ylab = "",
     main = SampleName[j])
par(new=T)
plot(t,f,pch = 2,col =2,
     xlim = range(t),ylim = range(f),
     axes = FALSE,
     bty = "n",
     xlab = " ", 
     ylab = "")
grid()

axis(side=4,
     at =range(f))

mtext("Frequency (Hz)", side=4, line=3, col="blue")

title(SampleName[j], 
       xlab="Time (sec)",
       ylab="Temperature (C)")
j=j+1}
#############################################


}


new <- data.frame(t,H,f,T1,T2,T3)

return(new)

}