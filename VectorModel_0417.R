library(deSolve)

# function for a simple mosquito borne disease
SIRMosVec = function(time, state, parms) {
  with(as.list(c(state, parms)), {
    # infection in livestocks
    dSL = vL - (SL* r*(TLM * IM)) - muL * SL
    dEL=(SL *r*(TLM * IM))-(alpha*EL)-(muL*EL)
    dIL = alpha*EL - gamma * IL - muL * IL
    #dRH = gamma * IH - muH * RH
    
    # infection in mosquitoes
    #dP
    #dQ
    dSM = vM - SM*r*(TML * IL) - muM * SM
    dEM = SM*r*(TML * IL) - alpha2*EM-muM*EM
    dIM = alpha2*EM- muM * IM
    list(c(dSL,dIL, dSM, dIM, dEL, dEM))
  })
}

#new parameters
EL=50;
EM= 01;
alpha=.6667; 
alpha2 = 1/8;
NL=1e7; IL =1; SL=NL-IL;
muL=.00277; # livestock life span: 10 yr
vL=NL*muL;
TML = 0.81; # prob infection from livestock to mosquito;
TLM = 0.51; # prob infection from mosquito to livestock;
gamma=.166666666; # infectious period: 7 days
NM=1e8; IM=1; SM=NM-IM;
muM = 1/7; # 1 week life span for mosquito
vM=NM*muM; # births in mosquito population
b=(256/365); # number of bite per mosquito per day;
r = b / NL; # bite rate per livestock per mosquito

parameters = c(muL = muL, muM = muM,
               vL = vL, vM = vM, 
               TLM = TLM, TML = TML, 
               gamma = gamma, alpha = alpha, alpha2 = alpha2)

state = c(SL = SL, IL = 1,SM = SM,IM = 1, EL = EL, EM = EM)

times=1:100;
## solve the odes using R ode sovler:
sim=ode(y=state,times=times,func=SIRMosVec,parms = parameters)

plot(sim)

par(mfrow=c(2,1),mar=c(3,3,1,1), cex=1, mgp=c(1.8,.5,0))
matplot(sim[,'time'],sim[,c('SM','IM')],type='l', 
        log='y', # NOTE: THE Y-AXIS IS ON LOG SCALE
        lwd=1,col=c('blue','red'),lty=1, main='Mosquitoes', cex.main=1,
        ylab='Numbers (on log scale)',xlab='Time (days)')
legend('bottomright',c('SM','IM'),col=c('blue','red'),
       lty=1, cex=1, lwd=1, bty='n')

par(mfrow=c(2,1),mar=c(3,3,1,1), cex=1, mgp=c(1.8,.5,0))
matplot(sim[,'time'],sim[,c('SL','IL')],type='l', 
        log='y', # NOTE: THE Y-AXIS IS ON LOG SCALE
        lwd=1,col=c('blue','red'),lty=1, main='Livestock', cex.main=1,
        ylab='Numbers (on log scale)',xlab='Time (days)')
legend('bottomright',c('SL','IL'),col=c('blue','red'),
       lty=1, cex=1, lwd=1, bty='n')