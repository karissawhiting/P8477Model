library(deSolve)

# function for a simple mosquito borne disease
SEIRMosVec = function(times, state, parms) {
  with(as.list(c(state, parms)), {
    
    # infection in mosquitoes
    
    dP1 = b1 * (N1 - q1 * I1) - theta1 * P1
    dQ1 = b1 * q1 * I1 - theta1 * Q1
    dS1 = theta1 * P1 - d1 * S1 - ((beta.M * S1 *I1) / (N2))
    dE1 = -d1 * E1 + ((beta.M * S1 * I2) / (N2)) - epsilon1 * E1
    dI1 = theta1 * Q1 - d1 * I1 + epsilon1 * E1
#    dN1 = (b1 - d1) * N1
    
     # infection in livestock
    
    dS2 = b2 * N2 - (d2 * S2) - ((beta.L * S2 * I1) / (N1)) # doesn't include K (Carrying capacity)
    dE2 = -d2 * E2 * N2 + ((beta.L * S2 * I1) / (N1)) - epsilon2 * E2
    dI2 = -d2 * I2 + epsilon2 * E2 - gamma2 * I2 - mu2 * I2
#    dR2 = -d2 * R2 + gamma2 * I2 
#    dN2 = N2 * (b2 - d2) - mu2 * I2 
    
    list(c(dP1, dQ1, dS1, dE1, dI1, dS2, dE2, dI2)) #N1, N2, R2
  })
}

#States
times=1:(365*100);
P1 = 1e7 #uninfected eggs, what is total number of eggs?
Q1 = 1 #infected eggs
E1 = 1
I1 = 1
N1 = 1e8
S1 =  N1 - I1 - E1

N2 = 1e7
E2 = 1
I2 = 1
#R2 = 1
S2 = N2 - I2 - E2

state = c(P1 = P1, Q1 = Q1, E1 = E1, I1 = I1, 
          S1 = S1, E2 = E2, I2 = I2, S2 = S2) #R2 = R2, N1 = N1, N2 = N2)

# Mosquito Parameters
b1 = 1/3
q1 = .1
theta1 = .2 #should this be 1/.2? i think this is right
d1= .3333333 #1/d1 = lifespand of Aedes#used 1/3 (min, maybe we should use mean)
beta.M= 0.21
epsilon1 = .25 # used 1/4, minimm

#Livestock Parameters
b2= .002777778 #daily birthrate in livestock
d2= .002777778 #1/d2 = lifespan of livestock animals# 
beta.L = .51 #Beta12 - adequate contact rate from livestock to Aedes#
epsilon2 = .333333 # 1/epsilon2 = incubation period in Aedes#
gamma2 = .166666666
mu2 = .1 #death due to disease .1 (#higher limit)

parameters = c(b1 = b1, q1 = q1,
               theta1 = theta1, d1 = d1, beta.M = beta.M, epsilon1 = epsilon1,
               b2 = b2, d2 = d2, beta.L = beta.L, epsilon2 = epsilon2, 
               gamma2 = gamma2, mu2 = mu2)


## solve the odes using R ode sovler:
sim=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)

simdf<- as.data.frame(sim)










plot.new()
par(mfrow=c(2,1),mar=c(3,3,1,1), cex=1, mgp=c(1.8,.5,0))

matplot(sim[,'time'],sim[,c('SH','IH')],type='l',
        log='y', # NOTE: THE Y-AXIS IS ON LOG SCALE
        lwd=1,col=c('blue','red'),lty=1, main='Humans', cex.main=1,
        ylab='Numbers (on log scale)',xlab='Time (days)')
legend('bottomright',c('SH','IH'),col=c('blue','red'),
       lty=1, cex=1, lwd=1, bty='n')



