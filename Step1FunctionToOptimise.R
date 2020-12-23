Step1FunctionToOptimise2 <- function(opt,nTime, nEvents,Event,smat,long.grid,lat.grid){ # opt = c(rho,delta,phi)
    Weighted.Prob.Mat2 <- array(0,c(6561,nTime)) # array dim=6561:10348

    for(i in 1:nEvents){
        Space<- apply(smat,1,FUN = W, v=c(long.grid[i],lat.grid[i]),rho=opt[1])
        Weighted.Prob.Mat2[,(Event[i]+1)] <- c(exp(-1/opt[2])*t(Space))
    }
    
    for(j in 1:nTime) Weighted.Prob.Mat2[,j] <-  Weighted.Prob.Mat2[,j]+Weighted.Prob.Mat2[,max(1,(j-1))] * opt[3] 
    
    Weighted.Prob.Mat2<-Weighted.Prob.Mat2/sum(c(Weighted.Prob.Mat2),na.rm = T)
}