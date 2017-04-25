library(deSolve)
library(dplyr)
library(ggplot2)
library(gridExtra)

install.packages("sensitivity")
install.packages("dplyr")

# function for a simple mosquito borne disease
SEIRMosVec = function(time, state, parms) {
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

b1 = 1/3; #number of eggs laid per day
q1 = .02;
theta = (1/10);
alpha=.6667; 
alpha2 = 1/8;
muL=.00277; # livestock life span: 10 yr
TML = 0.81; # prob infection from livestock to mosquito;
TLM = 0.51; # prob infection from mosquito to livestock;
gamma= 0.1428571; # infectious period: 7 days
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
sim=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
simdf<- as.data.frame(sim)
plot(sim)

#plots
plot1<-ggplot(simdf, aes(x=time, y=IL)) + geom_line() 
plot1

#code for combining graphs(?)
plot2<-ggplot(simdf, aes(x=time, y=IM)) + geom_line()
visuals=rbind(plot1,plot2)
ggplot(visuals)
#nope didn't work

#trying again
arrange(plot1,plot2)
#nope

#trying again
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

g1<-ggplotGrob(plot1)
g2<-ggplotGrob(plot2)
g<-rbind(g1,g2)
grid.newpage()
grid.draw(g)

#try try again
g<-ggplot(simdf,aes(x=time))+geom_line(aes(y=P),color="red")+geom_line(aes(y=Q),color="green")+ylab("Number of Mosquito Eggs") + xlab("Time")
g
#yay it worked!!

#now adding legend
g<-ggplot(simdf,aes(x=time))+geom_line(aes(y=P,color="Uninfected"))+geom_line(aes(y=Q,color="Infected"))+ylab("Number of Mosquito Eggs") + xlab("Time")+scale_color_manual(name="Egg Type",values=c(Uninfected="red",Infected="green"),labels=c("Uninfected","Infected"))
g
#success!

#removing background grid
g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#removing everything
g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))
#hmm don't know why gray background on legend
#just grid
g+theme_bw()

#extra plots

plot1 + ggplot(simdf, aes(y=IM)) + geom_line()

plot1 + ggplot(simdf, aes(x=time, y=IM)) + geom_line() + ylab("Infectious Mosquito") + ggtitle("Number of Infectious Mosquitoes over Time")

plot2<-ggplot(simdf, aes(x=time, y=IM)) + geom_line() + ylab("Infectious Mosquito") + ggtitle("Number of Infectious Mosquitoes over Time")

plot3<-ggplot(simdf, aes(x=time, y=EM)) + geom_line() + ylab("Exposed Mosquito") 
+ ggtitle("Number of Exposed Mosquitoes over Time")

plot4<-ggplot(simdf, aes(x=time, y=EL)) + geom_line() + ylab("Exposed Livestock") 
+ ggtitle("Number of Exposed Livestock over Time")

plot5<-ggplot(simdf, aes(x=time, y=SM)) + geom_line() + ylab("Susceptible Mosquito") 
+ ggtitle("Number of Susceptible Mosquitoes over Time")

plot6<-ggplot(simdf, aes(x=time, y=SL)) + geom_line() + ylab("Susceptible Livestock") 
+ ggtitle("Number of Susceptible Livestock over Time")

plot7<-ggplot(simdf, aes(x=time, y=P)) + geom_line() + ylab("Uninfected Mosquito Eggs") 
+ ggtitle("Number of Uninfected Mosquito Eggs over Time")

plot8<-ggplot(simdf, aes(x=time, y=Q)) + geom_line() + ylab("Infected Mosquito Eggs") 
+ ggtitle("Number of Infected Mosquito Eggs over Time")

#taking random sample (Beta12)
sample(0:0.32, 2, replace=TRUE)
plotprcc


