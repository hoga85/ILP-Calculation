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


multiHeatingCurve <- function(startData,EndData,TimeFactor,SampleName,dataAll, dataReduceOn, plotOn){


if (EndData - startData +1 > 2){
	noRow <- ceiling((EndData - startData +1)/2)
	noCol <- 2
	}else{
	noRow <- EndData - startData +1
	noCol <- 1
}

layout(matrix(c(1:(noRow*noCol)),nrow = noRow, ncol =noCol)) # structure of plots


for(i in startData:EndData)
{data <- get(dataAll[i])

 f <- data$Frequency 


if (dataReduceOn ){
##Find the Index of Sample in and out##
t.SampleIn <- t.SampleInOut(f)[1] +5
#t.SampleOut <- t.SampleInOut(f)[2] -30


##Redefine the range of data##
data<-data[t.SampleIn:length(data$Time),]
}

 f <- data$Frequency 
 t <- (data$Time - min(data$Time))/TimeFactor
 T1 <- data$T1
 H <- data$H

#############################################
if (plotOn == TRUE){

#par(mfrow=c(noRow,noCol))
par(mar=c(5, 4, 4, 5) + 0.05)

#plot(NA,NA,xlim=c(0,length(f)),ylim = range(T1))
plot(t,T1,pch = 1,col = 1, 
     xlim=c(0,length(f)),ylim = range(T1),
     xlab = "", 
     ylab = "",
     main = SampleName[j])


#axis(side=4,
#     at =range(f))

#mtext("Frequency (Hz)", side=4, line=3, col="blue")

title(SampleName[j], 
       xlab="Time (sec)",
       ylab="Temperature (C)")
j=j+1}
#############################################


}


new <- data.frame(t,H,f,T1)

return(new)

}