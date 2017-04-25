source("Paper_Model_Working_420.R")

dev.off()

############################
# Parameter Sensitivity    #
############################
############################
#GAMMA

gammamat=seq((1/5), .99, by=.2); # 
res=matrix(0,15,length(gammamat)); # to save model outputs
times=1:106
for (i in 1:length(gammamat)){
  gamma = gammamat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, theta = theta, b1 = b1, q1 = q1, dL = dL)
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
    sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
}

#GAMMA- plot of cumulative incidence sens.
resdf<- as.data.frame(res)
names(resdf)<- gammamat
index<- c(1:15)
resdf<- cbind(index, resdf)
df <- melt(resdf ,  id.vars = 'index')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(index,value)) + geom_line(aes(colour = variable))

############################
#BETAS- KARISSA
#BLM
betaLMmat=seq(.0021, .2762, by=.03); # 
resInc=matrix(0,106,length(betaLMmat)); # to save model outputs
resEx=matrix(0,106,length(betaLMmat)); # to save model outputs
resIn=matrix(0,106,length(betaLMmat)); # to save model outputs
times=1:106
for (i in 1:length(betaLMmat)){
  BLM = betaLMmat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, 
                 theta = theta, b1 = b1, q1 = q1, dL = dL, BML = BML, BLM = BLM)
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  resInc[,i] = sim2[, 'dInci']
  resEx[,i] = sim2[, 'EL']
  resIn[,i] = sim2[, 'IL']
  #res[,i]= sim2[, 'IL']
 # res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
  #  sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
}

#alpha (L)—Erin
alphaLmat=seq(.17, 1, by=.13); # 
resInc=matrix(0,106,length(alphaLmat)); # to save model outputs
resEx=matrix(0,106,length(alphaLmat)); # to save model outputs
resIn=matrix(0,106,length(alphaLmat)); # to save model outputs
times=1:106
for (i in 1:length(alphaLmat)){
  alphaL = alphaLmat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, 
                 theta = theta, b1 = b1, q1 = q1, dL = dL, BML = BML, BLM = BLM)
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  resInc[,i] = sim2[, 'dInci']
  resEx[,i] = sim2[, 'EL']
  resIn[,i] = sim2[, 'IL']
  #res[,i]= sim2[, 'IL']
  # res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
  #  sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
}

#alpha (M)—Erin
alphaMmat=seq(.125, 0.25, by=.005); # 
resInc=matrix(0,106,length(alphaMmat)); # to save model outputs
resEx=matrix(0,106,length(alphaMmat)); # to save model outputs
resIn=matrix(0,106,length(alphaMmat)); # to save model outputs
times=1:106
for (i in 1:length(alphaMmat)){
  alphaM = alphaMmat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, 
                 theta = theta, b1 = b1, q1 = q1, dL = dL, BML = BML, BLM = BLM)
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  resInc[,i] = sim2[, 'dInci']
  resEx[,i] = sim2[, 'EL']
  resIn[,i] = sim2[, 'IL']
  #res[,i]= sim2[, 'IL']
  # res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
  #  sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
}

#PLOT
#alphaL-plot of cumulative incidence sens.
resdfInc<- as.data.frame(resInc)
names(resdfInc)<- alphaLmat
index<- c(1:106)
resdfInc<- cbind(index, resdfInc)
dfInc <- melt(resdfInc ,  id.vars = 'index')
inc<-ggplot(dfInc, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Cumulative Incidence") + xlim(0,50)
inc

#PLOT
#alphaM-plot of cumulative incidence sens.
resdfInc<- as.data.frame(resInc)
names(resdfInc)<- alphaMmat
index<- c(1:106)
resdfInc<- cbind(index, resdfInc)
dfInc <- melt(resdfInc ,  id.vars = 'index')
inc<-ggplot(dfInc, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Cumulative Incidence") + xlim(0,50)
inc

#alphaL-plot of Ex sens.
resdfEx<- as.data.frame(resEx)
names(resdfEx)<- alphaLmat
index<- c(1:106)
resdfEx<- cbind(index, resdfEx)
dfEx <- melt(resdfEx ,  id.vars = 'index')
Ex<- ggplot(dfEx, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Exposed")+ xlim(0,50)
Ex

#alphaM-plot of Ex sens.
resdfEx<- as.data.frame(resEx)
names(resdfEx)<- alphaMmat
index<- c(1:106)
resdfEx<- cbind(index, resdfEx)
dfEx <- melt(resdfEx ,  id.vars = 'index')
Ex<- ggplot(dfEx, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Exposed")+ xlim(0,50)
Ex

#BETA- plot of Ex sens.
resdfEx<- as.data.frame(resEx)
names(resdfEx)<- betaLMmat
index<- c(1:106)
resdfEx<- cbind(index, resdfEx)
dfEx <- melt(resdfEx ,  id.vars = 'index')
Ex<- ggplot(dfEx, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Exposed")+ xlim(0,50)
Ex

#alphaL-plot of Inf sens.
resdfIn<- as.data.frame(resIn)
names(resdfIn)<- alphaLmat
index<- c(1:106)
resdfIn<- cbind(index, resdfIn)
dfIn <- melt(resdfIn ,  id.vars = 'index')
In<- ggplot(dfIn, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Infectious") +ylim(c(0,400)) + xlim(0,50)
In
#plot(sim2)

#PLOT
#BETA- plot of cumulative incidence sens.
resdfInc<- as.data.frame(resInc)
names(resdfInc)<- betaLMmat
index<- c(1:106)
resdfInc<- cbind(index, resdfInc)
dfInc <- melt(resdfInc ,  id.vars = 'index')
Inc<- ggplot(dfInc, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Cumulative Incidence") + xlim(0,50)
Inc

#BETA- plot of Inf sens.
resdfIn<- as.data.frame(resIn)
names(resdfIn)<- betaLMmat
index<- c(1:106)
resdfIn<- cbind(index, resdfIn)
dfIn <- melt(resdfIn ,  id.vars = 'index')
In<- ggplot(dfIn, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Infectious") +ylim(c(0,400)) + xlim(0,50)
In
#plot(sim2)

#this code puts all plots together
grid.arrange(Inc,Ex, In, nrow = 3, ncol = 1)

###################
#BETAS- KARISSA
#BML
betaMLmat=seq(.0021, .2429, by=.03); # 
resInc=matrix(0,106,length(betaMLmat)); # to save model outputs
resEx=matrix(0,106,length(betaMLmat)); # to save model outputs
resIn=matrix(0,106,length(betaMLmat)); # to save model outputs
times=1:106
for (i in 1:length(betaMLmat)){
  BML = betaMLmat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, 
                 theta = theta, b1 = b1, q1 = q1, dL = dL, BML = BML, BLM = BLM)
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  resInc[,i] = sim2[, 'dInci']
  resEx[,i] = sim2[, 'EL']
  resIn[,i] = sim2[, 'IL']
  #res[,i]= sim2[, 'IL']
  # res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
  #  sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
}

#PLOT
#BETA- plot of cumulative incidence sens.
resdfInc<- as.data.frame(resInc)
names(resdfInc)<- betaMLmat
index<- c(1:106)
resdfInc<- cbind(index, resdfInc)
dfInc <- melt(resdfInc ,  id.vars = 'index')
Inc<- ggplot(dfInc, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Cumulative Incidence") + xlim(0,50)
Inc

#BETA- plot of Ex sens.
resdfEx<- as.data.frame(resEx)
names(resdfEx)<- betaMLmat
index<- c(1:106)
resdfEx<- cbind(index, resdfEx)
dfEx <- melt(resdfEx ,  id.vars = 'index')
Ex<- ggplot(dfEx, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Exposed")+ xlim(0,50)
Ex

#BETA- plot of Inf sens.
resdfIn<- as.data.frame(resIn)
names(resdfIn)<- betaMLmat
index<- c(1:106)
resdfIn<- cbind(index, resdfIn)
dfIn <- melt(resdfIn ,  id.vars = 'index')
In<- ggplot(dfIn, aes(index,value)) + geom_line(aes(colour = variable)) + xlab("days") + ylab("Infectious") +ylim(c(0,400)) + xlim(0,50)
In
#plot(sim2)

#this code puts all plots together
grid.arrange(Inc,Ex, In, nrow = 3, ncol = 1)
################
#NUMBER EGGS -PALLAVI
PNmat=seq(999, 10000, by= 250); # 
res=matrix(0,15,length(PNmat)); # to save model outputs
resM = matrix(0, 106, length(PNmat));
times=1:106
for (i in 1:length(PNmat)){
  P = PNmat[i];  # the only thing that is different
  ## the same routine you run before ##
  parameters = c(muL = muL, muM = muM, 
                 gamma = gamma, alphaL = alphaL, alphaM = alphaM, theta = theta, b1 = b1, q1 = q1, dL = dL)
  state = c(SL = SL, IL = IL, SM = SM,IM = IM, EL = EL, EM = EM, P = P, Q = Q, dInci = 0)
  sim2=ode(y=state,times=times,func=SEIRMosVec,parms = parameters)
  res[,i]=sim2[seq(8,nrow(sim2),by=7),'dInci']-
    sim2[seq(1,nrow(sim2)-7,by=7),'dInci'] 
  resM[,i]= sim2[, 'Q']
}
#matplot(x = 1:15, y=res,lty=2, lwd=2,col=rainbow(ncol(res)))

#PN plot of cumulative incidence sens.
resdf<- as.data.frame(res)
names(resdf)<- PNmat
index<- c(1:15)
resdf<- cbind(index, resdf)
df <- melt(resdf ,  id.vars = 'index')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(index,value)) + 
  geom_line(aes(colour = variable)) +
  xlim(c(1, 4))


#############
resMdf<- as.data.frame(resM)
names(resMdf)<- PNmat
index<- c(1:106)
resMdf<- cbind(index, resMdf)
mdf <- melt(resMdf ,  id.vars = 'index')

ggplot(mdf, aes(index,value)) + 
  geom_line(aes(colour = variable))




