library(deSolve)
library(dplyr)
library(ggplot2)


# function for a simple mosquito borne disease
SIRMosVec = function(time, state, parms) {
  with(as.list(c(state, parms)), {
    # infection in livestocks
    dSL = vL - SL* r*(TLM * IM)/NM - muL * SL
    dEL=(SL *r* (TLM * IM))-alpha*EL-muL*EL
    dIL = SL * r * (TLM * IM) - gamma * IL - muL * IL
    #dRH = gamma * IH - muH * RH
    
    # infection in mosquitoes
    dP = b1*(NM-q1*IM)- theta*P #susceptible
    dQ =b1*q1*IM-theta*Q         #Infected Egg
    dSM = theta*P - SM*(TML * IL)/NL - muM * SM
    dEM = SM*(TML * IL)/NL - alpha2*EM-muM*EM
    dIM = theta*Q + alpha2*EM-muM*EM - muM * IM
    list(c(dSL,dIL, dSM, dIM, dEL, dEM, dP, dQ))
  })
}

#new parameters

NL=1000; IL =0; 
EL=0;
SL=NL-IL;

P = 999;
Q = 1;

NM=1; IM=0; SM=1;
EM= 0;

b1 = 1/3;
q1 = .02;
theta = (1/10);
alpha=.6667; 
alpha2 = 1/8;
muL=.00277; # livestock life span: 10 yr
TML = 0.81; # prob infection from livestock to mosquito;
TLM = 0.51; # prob infection from mosquito to livestock;
gamma=.166666666; # infectious period: 7 days
muM = 1/60; # 1 week life span for mosquito
b=(256/365); # number of bite per mosquito per day;
r = b / NL; # bite rate per livestock per mosquito
vL=NL*muL;
vM=NM*muM; # births in mosquito population

parameters = c(muL = muL, muM = muM,
               vL = vL, vM = vM, 
               TLM = TLM, TML = TML, 
               gamma = gamma, alpha = alpha, alpha2 = alpha2, theta = theta, b1 = b1, q1 = q1)

state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q)

times=1:(365*.5);
## solve the odes using R ode sovler:
sim=ode(y=state,times=times,func=SIRMosVec,parms = parameters)
simdf<- as.data.frame(sim)
plot(sim)


ggplot(dfsim, aes(x = time, y= SL), )