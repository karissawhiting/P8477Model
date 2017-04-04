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


