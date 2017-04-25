#code for putting graphs on one plot
#using code/parameters from VectorModel_0415 script 
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