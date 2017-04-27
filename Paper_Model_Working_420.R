library(deSolve)
library(ggplot2)
library(reshape2)
library(gridExtra)

# function for a simple mosquito borne disease
SEIRMosVec = function(time, state, parms) {
  with(as.list(c(state, parms)), {
    # infection in mosquitoes
    dP = b1*(NM-q1*IM)- theta*P #susceptible
    dQ = b1*q1*IM-theta*Q         #Infected Egg
    dSM = theta*P - (BML*SM*IL)/NL - muM * SM
    dEM = (BML*SM*IL)/NL - alphaM*EM-muM*EM
    dIM = theta*Q + alphaM*EM - muM * IM
    
    # infection in livestocks
    dSL = NL*muL - (BLM*SL*IM)/NM - muL * SL
    dEL=(BLM*SL*IM)/NM-alphaL*EL-muL*EL
    dIL = alphaL*EL - gamma*IL - muL*IL - dL*IL
    dInci = alphaL*EL

    list(c(dP, dQ, dSM, dEM, dIM, dSL,dEL, dIL, dInci))
           #dIL, dSM, dIM, dEL, dEM, dP, dQ, dInci))
  })
}

#state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, Inci = 0)
######################
# State Variables    #
######################
#Egg
P = 5000# 999;
Q = 0;
#Mosquito
NM=5000; 
IM=1; SM=NM-IM;
EM= 0;
#Livestock
NL=1000; IL =0; 
EL=0;
SL=NL-IL-EL;


######################
# Parameters         #
######################
#Mosquito
b1 = 1/20;       #number of eggs laid per day
q1 = .05;        #transovarial transmission rate
theta = (1/10);  #maturation rate
alphaM = 1/6;    #transition from exposed to infected mosquitos
muM = 1/20;      #Mosquito death rate, 1 week life span for mosquito
BML = .2762       #transmission rate to mosquito from livestock

#Livestock
alphaL=(1/4);     #transition from exposed to infected livestock
muL= (1/10/365);    #livestock death rate, life span: 10 yr
gamma= (1/5);     # recovery rate livestock, infectious period: 7 days
#vL=NL*muL;       #birth rate livestock
BLM= BML           #transmission rate to livestock from mosquito
dL = .07          #death rate in livestock due to RVF

parameters = c(muL = muL, muM = muM, 
               gamma = gamma, alphaL = alphaL, alphaM = alphaM, 
               theta = theta, b1 = b1, q1 = q1, dL = dL, BML = BML, BLM = BLM)

state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, Inci = 0)
state = c(P=P, Q=Q, SM=SM, EM=EM, IM=IM, SL=SL, EL=EL, IL=IL, Inci=0)

times=1:(365*10); # months

######################
# Simulation         #
######################
sim=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)



######################
# Plots              #
######################
plot(sim)


######################
# R0             #
######################

((BLM*BML)/ (muM*(gamma + muL))*(NM/NL))

