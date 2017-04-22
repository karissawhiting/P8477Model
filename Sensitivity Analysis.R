## 1. timing of intervention
source("Paper_Model_Working_420.R")


gammamat=seq((1/5), .99, by=.05); # 
res=matrix(0,15,length(gammamat)); # to save model outputs
times=1:106
for (i in 1:length(gammamat)){
  gamma = gammamat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM,
                 vL = vL, vM = vM, 
                 TLM = TLM, TML = TML, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, theta = theta, b1 = b1, q1 = q1, dL = dL)
  
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)

  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
    sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
}
  


######################
# Plots              #
######################

#matplot(x = 1:15, y=res,lty=2, lwd=2,col=rainbow(ncol(res)))


#GAMMA- plot of cumulative incidence sens.
resdf<- as.data.frame(res)
names(resdf)<- gammamat
index<- c(1:15)
resdf<- cbind(index, resdf)
df <- melt(resdf ,  id.vars = 'index')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(index,value)) + geom_line(aes(colour = variable))

