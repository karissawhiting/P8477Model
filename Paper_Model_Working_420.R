library(deSolve)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
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

#NOTE, if we held b1 = theta = MuM, we would have P+Q+NM be constant over time. We are letting it vary

#Mosquito
b1 = 1/31.5;       #number of eggs laid per day
q1 = .1;        #transovarial transmission rate
theta = (1/15);  #maturation rate
alphaM = 1/6;    #transition from exposed to infected mosquitos
muM = 1/31.5;      #Mosquito death rate, 1 week life span for mosquito
BML = .2429      #transmission rate to mosquito from livestock

#Livestock
alphaL=.58;     #transition from exposed to infected livestock
muL= (1/1980);    #livestock death rate, approx 5.5 years (average)
gamma= .6;     # recovery rate livestock, infectious period: 7 days
#vL=NL*muL;       #birth rate livestock
BLM= 0.2762           #transmission rate to livestock from mosquito
dL = .0625         #death rate in livestock due to RVF

parameters = c(muL = muL, muM = muM, 
               gamma = gamma, alphaL = alphaL, alphaM = alphaM, 
               theta = theta, b1 = b1, q1 = q1, dL = dL, BML = BML, BLM = BLM)

#state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, Inci = 0)
state = c(P=P, Q=Q, SM=SM, EM=EM, IM=IM, SL=SL, EL=EL, IL=IL, Inci=0)

times=1:(365*15); # months

######################
# Simulation         #
######################
sim=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)



######################
# Plots              #
######################
plot(sim)
df
#new<- mutate(simdf, NM = (SM + EM + IM+P+Q))
#plot(new$NM)

simdf<- as.data.frame(sim)
simdf2<- simdf %>%
  rename("Uninfected Eggs" = P, "Infected Eggs" = Q, 
         "Susceptible Mosquitos" = SM, "Exposed Mosquitos" = EM, "Infected Mosquitos" =IM,
         "Susceptible Livestock" = SL, "Exposed Livestock" = EL, "Infectious Livestock" = IL,"Cumulative Inci" = Inci) 

simdf2melt<- simdf2 %>% select(`Susceptible Mosquitos`, `Exposed Mosquitos`,`Infected Mosquitos`, 
                               `Susceptible Livestock`, `Exposed Livestock`, `Infectious Livestock`, time) %>% melt(., id = 'time')

jpeg('dynamics.jpeg', units="in", width=8, height=4, res=300)
ggplot(data=simdf2melt, aes(x=time, y= value, group=variable, colour=factor(variable))) + 
  geom_line(size=.5) + ylim(c(0,6500)) + xlim(c(0, 400)) + ylab("Number of Individuals")
dev.off()
######################

#totmost <- simdf2 %>% 
  #select(time, `Infected Eggs`, `Susceptible Mosquitos`, `Exposed Mosquitos`, `Infected Mosquitos`, `Uninfected Eggs`, `Infected Eggs`) %>% 
  #mutate(`Total Mosquito` = `Susceptible Mosquitos` + `Infected Mosquitos`) %>% 
  #mutate(`Total Eggs` = `Uninfected Eggs` + `Infected Eggs`) %>%
  #mutate(All = `Total Eggs` + `Total Mosquito`)
#totmostmelt <- totmost %>% melt(., id = 'time')

#################################
#TOTAL EGG PLOTS
totegg<- totmost %>% select(`Infected Eggs`, `Uninfected Eggs`, `Total Eggs`, time)
toteggmelt<- melt(totegg, id = 'time')

jpeg('toteggs.jpeg', units="in", width=15, height=5, res=300)
ptotegg<- ggplot(toteggmelt, aes(time, value)) + 
  geom_line(size = 1, color = "light blue") + 
  facet_wrap(~variable, scales="free_y") + 
  xlab("Time (Days)") + ylab("Number of Eggs") +
  theme_minimal() + theme(strip.text = element_text(size=15))
ptotegg
dev.off()

filter(totegg, totegg$`Infected Eggs` > 15)
totegg[3100:3500,]
#################################
# TOTAL MOSQUITO PLOTS
totmos<- totmost %>% select(`Susceptible Mosquitos`, `Exposed Mosquitos`, `Infected Mosquitos`, `Total Mosquito`, time)
totmosmelt<- melt(totmos, id = 'time')

jpeg('totmosquit.jpeg', units="in", width=15, height=10, res=300)
ptotmos<- ggplot(totmosmelt, aes(time, value)) + 
  geom_line(size = 1, color = "pink") + 
  facet_wrap(~variable, scales="free_y") + 
  xlab("Time (Days)") + ylab("Number of Mosquitos") +
  theme_minimal() + theme(strip.text = element_text(size=15))
ptotmos
dev.off()

totmos2<- totmos %>% mutate(., EnM = `Exposed Mosquitos` + `Infected Mosquitos`)
tail(totmos2)
totmos2[170:222,]

##################
###TOTAL LIVESTOCK PLOTS

totliv <- simdf2 %>% 
  select(time, `Susceptible Livestock`, `Exposed Livestock`, `Infectious Livestock`, `Cumulative Inci`)

totlivmel <- totliv %>% melt(., id = 'time')

#jpeg('ptotliv.jpeg', units="in", width=15, height=10, res=300)
ptotliv<- ggplot(totlivmel, aes(time, value)) + 
  geom_line(size = 1, color = "light green") + 
  facet_wrap(~variable, scales="free_y") + 
  xlab("Time (Days)") + ylab("Number of Livestock") +
  theme_minimal() + theme(strip.text = element_text(size=15))
ptotliv
#dev.off()

tail(totliv)

totliv[150:250,]
summarise(totliv$`Infected Livestock`)

ggplot(data=totmostmelt, aes(x=time, y= value, group=variable, colour=factor(variable))) + 
  geom_line(size=.5) + ylim(c(0,1000)) + xlim(c(0, 1000))

ggplot(data=simdf4, aes(x=time, y= value, group=variable, colour=factor(variable))) + 
  geom_line(size=.5) + ylim(c(0,1000)) + xlim(c(0, 1000))
###################


p3<- ggplot(simdf2, aes(time, value)) + 
  geom_line(size = 1, color = "navy blue") + 
  facet_wrap(~variable, scales="free_y") + 
  xlab("Time (Days)") + ylab("Number of Livestock") +
  theme_minimal() + theme(strip.text = element_text(size=12))
p3
######################
# R0             #
######################

((BLM*BML)/ (muM*(gamma + muL))*(NM/NL))

