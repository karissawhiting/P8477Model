library(deSolve)

# function for a simple mosquito borne disease
SIRMosVec = function(time, state, parms) {
  with(as.list(c(state, parms)), {
    # infection in livestock
    
    dS = b2N2 - ((d2S2N2)/(K2))
    
    # infection in mosquitoes
    dSM = vM - SM * r * (TMH * IH) - muM * SM
    dIM = SM * r * (TMH * IH) - muM * IM
    list(c(dSH, dIH, dSM, dIM))
  })
}

This is Karissa's line'