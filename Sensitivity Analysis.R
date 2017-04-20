## 1. timing of intervention
betalmat=seq(.0021,.2762 ,by=.01); # the number of weeks you wanna test
res=matrix(0,20,length(betalmat)); # to save model outputs
for (i in 1:length(betalmat)){
  beta.L=betalmat[i];  # the only thing that is different
  ## the same routine you run before ##
  times=1:(365*100);
  state = c(P1 = P1, Q1 = Q1, E1 = E1, I1 = I1, 
            S1 = S1, E2 = E2, I2 = I2, S2 = S2) #R2 = R2, N1 = N1, N2 = N2)
  parameters = c(b1 = b1, q1 = q1,
                 theta1 = theta1, d1 = d1, beta.M = beta.M, epsilon1 = epsilon1,
                 b2 = b2, d2 = d2, beta.L = beta.L, epsilon2 = epsilon2, 
                 gamma2 = gamma2, mu2 = mu2)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  
  
  
  # get weekly incidence
#  res[,i]=sim[seq(8,nrow(sim),by=7),'Inci']-
    sim[seq(1,nrow(sim)-7,by=7),'Inci']
  ## the same routine you run before ##
}
# Note: Each column in the res matrix stores the incidence for each run
# (column#1: WkOfCtrl=6; column#2: WkOfCtrl=8; …)
ymax=max(res)*1.2 # make sure the y-axis covers the range of all simulations
par(mar=c(3,3,1,1),mgp=c(2,.5,0),cex=1)
df.bar=barplot(obs,col='white',xlab='Date of onset (week)',
               ylab='Weekly incidence',ylim=c(0,ymax), main = "Comparing Control Implemented at Different Weeks", cex.main = 1)
axis(1,at=df.bar,lab=1:20)
matlines(x=df.bar, y=res,lty=1, lwd=2,col=rainbow(ncol(res))) # use 'x=df.bar' to match with weeks
legend(0,ymax/1.2,c(paste('WkOfCtrl=',WkOfCtrl,sep='')),
       lty=1, lwd=2,col=rainbow(ncol(res)),bty='n',cex=.8)


## 2. reduction in transmission within the community:
## MODIFY THE FOR-LOOP ABOVE TO DO THE NEW SENSISTIVITY TEST
## NOW YOU ARE CHANGING z
# remember to change other parameters back
times1=seq(0,9*7,by=1); # first wk weeks with out intervention
times2=seq(9*7+1,20*7,by=1);

zs=seq(.2,.8,by=.2)
res=matrix(0,20,length(zs))
for (i in 1:length(zs)){
  z=z=(1-.12); # intervention in the community
  z.H= (.5+1)/2; # intervention in the hospital
  z.F=zs[i]; # intervention in safe burial
  parmsCtrl=c(alpha=alpha, # incubation period: 7 days
              gamma.h=gamma.h, # from onset to hopspitalization: 5 days
              gamma.d=gamma.d, # from onset to death: 9.6 days
              gamma.i=gamma.i, # from onset to end of infectiousness for survivors: 10 days
              gamma.f=gamma.f, # from death to traditional burial 2 days
              gamma.ih=gamma.ih, # from hospitalization to end of infectiousness for survivors
              gamma.dh=gamma.dh, # from hospitalization to death
              theta1=theta1, # proportion infectious in the hospital
              delta1=delta1, # CFR for unhospitalized
              delta2=delta2, # CFR for hospitalize
              beta.I=beta.I * (1-z), # transmission rate in the community
              beta.H=beta.H * (1-z.H), # transmission rate in the hospital
              beta.F=beta.F * (1-z.F) # transmission rate at funerals,
  );
  simNoCtrl=ode(y=state,times=times1,func=EBOLA,parms=parmsNoCtrl);
  state2=c(S=tail(simNoCtrl[,'S'],1),E=tail(simNoCtrl[,'E'],1),I=tail(simNoCtrl[,'I'],1),
           H=tail(simNoCtrl[,'H'],1),FF=tail(simNoCtrl[,'FF'],1),R=tail(simNoCtrl[,'R'],1),
           Inci=tail(simNoCtrl[,'Inci'],1));
  simCtrl=ode(y=state2,times=times2,func=EBOLA,parms=parmsCtrl);
  sim=rbind(simNoCtrl,simCtrl)
  res[,i]=sim[seq(8,nrow(sim),by=7),'Inci']-sim[seq(1,nrow(sim)-7,by=7),'Inci'] # get weekly incidence
}
ymax=max(res)*1.2
par(mar=c(3,3,1,1),mgp=c(2,.5,0),cex=1.1)
df.bar=barplot(obs,col='white',xlab='Date of onset (week)',
               ylab='Weekly incidence',ylim=c(0,ymax), main = "Adjusting Levels of Funeral Intervention at Week 9", cex.main = .95)
axis(1,at=df.bar,lab=1:20)
matlines(x=df.bar, y=res,lty=1, lwd=2,col=rainbow(ncol(res))) # use 'x=df.bar' to match with weeks
legend(0,ymax/1.2,c(paste('z.F=',zs,sep='')),
       lty=1, lwd=2,col=rainbow(ncol(res)),bty='n',cex=.8)


res[20,]
