library(deSolve)

# function for a simple mosquito borne disease
SEIRMosVec = function(times, state, parms) {
  with(as.list(c(state, parms)), {
    
    # infection in mosquitoes
    
    dP = b1 * (N1 - q1 * I1) - theta1 * P1
    dQ = b1 * q1 * I1 - theta1 * q1
    dSM = theta1 * P1 - d1 * S1 - ((beta.M * S1 *I1) / (N2))
    dEM = -d1 * E1 + ((beta.M * S1 * I2) / (N2)) - epsilon1 * E1
    dIM = theta1 * Q1 - d1 * I1 + epsilon1 * E1
    dNM = (b1 - d1) * N1
    
     # infection in livestock
    
    dSL = b2 * N2 - (d2 * S2) - ((beta.L * S2 * I1) / (N1)) 
    dEL = -d2 * E2 * N2 + ((beta.L * S2 * I1) / (N1)) - epsilon2 * E2
    dIL = -d2 * I2 + epsilon2 * E2 - gamma2 * I2 - mu2 * I2
    dRL = -d2 * R2 + gamma2 * I2 
    dNL = N2 * (b2 - d2) - mu2 * I2 
    
    list(c(dSH, dIH, dSM, dIM))
  })
}

#Parameters
# Mosquits
N1=1e8; I1 = 1;  E1= 1; S1 = N1-I1-E1; #starting with 1e8 mosquitos
b1 = 1/3
q1 = .1
theta1 = .2 #should this be 1/.2? i think this is right
d1= .3333333 #1/d1 = lifespand of Aedes#used 1/3 (min, maybe we should use mean)

beta.M= 0.21
epsilon1 = .25 # used 1/4, minimm

#Livestock
N2=1e7; I2 = 1; E2 = 1; S2=N2-I2-E2; #
beta.L = .51 #Beta12 - adequate contact rate from livestock to Aedes#
b2= .002777778 #daily birthrate in livestock
d2= .002777778 #1/d2 = lifespan of livestock animals# 
epsilon2 = .333333 # 1/epsilon2 = incubation period in Aedes#
gamma2 = .166666666
mu2 = .1 #higher limit 

parameters = c(b1 = b1, q1 = q1,
               theta1 = theta1, d1 = d1, beta.M = beta.M, epsilon1 = epsilon1,
               b2 = b2, beta.L = beta.L, d2 = d2, epsilon2 = epsilon2, 
               gamma2 = gamma2)

state = c(S2 = S2, I2 = I2, E2 = E2, S1 = S1, E1 = E1,  I1 = I1)


times=1:(365*100);
## solve the odes using R ode sovler:
sim=ode(y=state,times=times,func=SIRMosVec,parms = parameters)
simdf<- as.data.frame(sim)

plot.new()
par(mfrow=c(2,1),mar=c(3,3,1,1), cex=1, mgp=c(1.8,.5,0))

matplot(sim[,'time'],sim[,c('SH','IH')],type='l',
        log='y', # NOTE: THE Y-AXIS IS ON LOG SCALE
        lwd=1,col=c('blue','red'),lty=1, main='Humans', cex.main=1,
        ylab='Numbers (on log scale)',xlab='Time (days)')
legend('bottomright',c('SH','IH'),col=c('blue','red'),
       lty=1, cex=1, lwd=1, bty='n')



