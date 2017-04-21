sens.func<- function(par, min, max) {
  
  statements
  return(something)
}


gammamat=seq((1/5), .99, by=.05); # 
res=matrix(0,15,length(gammamat)); # to save model outputs
times=1:106
function()
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