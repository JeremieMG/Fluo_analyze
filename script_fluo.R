pdf("Fluo_200221.pdf", width = 16 , height = 10)
par(mfrow=c(2,2))


fv <- function(f,fm) {
  result = (fm-f)/fm
  return(result)
}

read.csv(file = "test.csv", row.names = 1) -> fluo
as.matrix(fluo) -> fluo
YIELD <- fv(as.numeric(fluo[1,]),as.numeric(fluo[2,]))
CONDITION = c()
for(value in fluo[1,]) {
  if(as.numeric(value)<50) {
    CONDITION <- c(CONDITION,0)
  }
  else {
    CONDITION <- c(CONDITION,1)
  }
}
fluo <- rbind(fluo, YIELD)
fluo <- rbind(fluo, CONDITION)
fluo <- fluo[,-(which(fluo["CONDITION",]=="0"))]

for(i in unique(fluo["SAMPLE",])){
  barplot(as.numeric(fluo["YIELD",which(fluo["SAMPLE",]==i)]),main = unique(fluo["TYPE",which(fluo["SAMPLE",]==i)]),sub = unique(fluo["MEDIUM",which(fluo["SAMPLE",]==i)]), names.arg =fluo["SPOT",which(fluo["SAMPLE",]==i)], col=("#0099FF"),ylim = c(0,1))
}

dev.off()