library(deSolve)

# function for a simple mosquito borne disease
SIRMosVec = function(time, state, parms) {
  with(as.list(c(state, parms)), {
    # infection in humans
    dSH = vH - SH * r * (THM * IM) - muH * SH
    dIH = SH * r * (THM * IM) - gamma * IH - muH * IH
    # dRH = gamma * IH - muH * RH
    
    # infection in mosquitoes
    dSM = vM - SM * r * (TMH * IH) - muM * SM
    dIM = SM * r * (TMH * IH) - muM * IM
    list(c(dSH, dIH, dSM, dIM))
  })
}