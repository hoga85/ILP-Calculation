###Before applying this fucntion, ##########################
###Parameters need to be defined as below in the main.R#####
#data1 <- read.table("A000.txt",header = T)
#data2 <- read.table("A001.txt",header = T)
#data3 <- read.table("C001.txt",header = T)
#data4 <- read.table("C002.txt",header = T)
#data5 <- read.table("C002b.txt",header = T)
#data6 <- read.table("C003.txt",header = T)
#dataAll <- c("data1","data2","data3","data4","data5","data6")
#SampleName <- c("A000","A001","C001","C002","C002b","C003")
############################################################

multipleFDrop <- function(startData,EndData,SampleName,dataAll)
{

FDropAll <- matrix(rep(0,0,2*(EndData-startData+1)),ncol=2)

		for(i in startData:EndData){
				data <- get(dataAll[i])
				f <- data$Frequency 
 				FDropAll[i,1] <- FDropDetector(f)$InitialFDrop
                        FDropAll[i,2] <- FDropDetector(f)$FinalFDrop}
				
return(FDropAll)

}