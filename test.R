library(deSolve)

# function for a simple mosquito borne disease
SEIRMosVec = function(times, state, parms) {
  with(as.list(c(state, parms)), {
    
    # infection in mosquitoes
    
    #    dP1 = b1 * (N1 - q1 * I1) - theta1 * P1
    #    dQ1 = b1 * q1 * I1 - theta1 * Q1
    dS1 = b1*N1 -d1 * S1 - ((beta.M * S1 *I2) / (N2))
    dE1 = -d1 * E1 + ((beta.M * S1 * I2) / (N2)) - epsilon1 * E1
    dI1 =  -d1 * I1 + epsilon1 * E1
    #    dN1 = (b1 - d1) * N1
    
    # infection in livestock
    
    dS2 = b2 * N2 - (d2 * S2) - ((beta.L * S2 * I1) / (N1)) # doesn't include K (Carrying capacity)
    dE2 = -d2 * E2 + ((beta.L * S2 * I1) / (N1)) - epsilon2 * E2
    dI2 = (-d2 * I2) + (epsilon2 * E2) - (gamma2 * I2)
    #    dR2 = -d2 * R2 + gamma2 * I2 
    #    dN2 = N2 * (b2 - d2) - mu2 * I2 
    
    list(c(dS1, dE1, dI1, dS2, dE2, dI2)) #N1, N2, R2
  })
}

#States
times=1:(365*100);
#P1 = 1e7 #uninfected eggs, what is total number of eggs?
#Q1 = 1 #infected eggs

N1 = 1e8
E0 = 10
I0 = 0
S0 =  N1 - I0 - E0


N2 = 1e5
E02 = 10
I02 = 0
#R2 = 1
S02 = N2 - I02 - E02

state = c(E1 = E0, I1 = I0, S1 = S0, E2 = E02, I2 = I02, S2 = S02) #R2 = R2, N1 = N1, N2 = N2)

# Mosquito Parameters
b1 = 1/3
#q1 = .1
#theta1 = .2 #should this be 1/.2? i think this is right
d1= .3333333 #1/d1 = lifespand of Aedes#used 1/3 (min, maybe we should use mean)
beta.M= 0.21
epsilon1 = .25 # used 1/4, minimm

#Livestock Parameters
b2= .002777778 #daily birthrate in livestock
d2= .002777778 #1/d2 = lifespan of livestock animals# 
beta.L = .51 #Beta12 - adequate contact rate from livestock to Aedes#
epsilon2 = .333333 # 1/epsilon2 = incubation period in Aedes#
gamma2 = .166666666
#mu2 = .1 #death due to disease .1 (#higher limit)

parameters = c(b1 = b1, d1 = d1, beta.M = beta.M, epsilon1 = epsilon1,
               b2 = b2, d2 = d2, beta.L = beta.L, epsilon2 = epsilon2, 
               gamma2 = gamma2)


## solve the odes using R ode sovler:
sim=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)

simdf<- as.data.frame(sim)









