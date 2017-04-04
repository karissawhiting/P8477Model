library(deSolve)

# function for a simple mosquito borne disease
SEIRMosVec = function(time, state, parms) {
  with(as.list(c(state, parms)), {
    
    # infection in mosquitoes
    
    dP = b1 * (N1 - q1 * I1) - theta1 * P1
    dQ = b1 * q1 * I1 - theta1 * Q1
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

# states and parameters

b1 #number of Aedes eggs laid per day#
N1 #size of mosquito population# 
q1 #transovarial transmission rate in Aedes#
I1 #Infectious in mosquitoes#
theta1 #1/theta1 = development time of Aedes#
P1 #uninfected Aedes eggs#
Q1 #infected Aedes eggs#
d1 #1/d1 = lifespand of Aedes#
S1 #Aedes susceptibles# 
beta.M #Beta21 - adequate contact rate from Aedes to livestock#
I2 #Infectious in livestock#
N2 #total livestock population size#
E1 #Aedes exposed#
epsilon1 #1/epsilon1 = incubation period in livestock#
b2 #daily birthrate in livestock# 
d2 #1/d2 = lifespan of livestock animals# 
S2 #livestock susceptibles#
beta.L #Beta12 - adequate contact rate from livestock to Aedes#
E2 #livestock exposed#
epsilon2 #1/epsilon2 = incubation period in Aedes#
gamma2 #1/gamma2 = infectiousness period in livestock#
mu2 #RVF mortality rate in livestock#
R2 #infected livestock death or recovery with immunity from RVF#


  
