# Task 1

NormDist <- rnorm(1000, 80, 20)
hist(NormDist)

ThreeVars<-function(NormDist,newmin,newmax){
  return(sum(NormDist>=newmin & NormDist<=newmax))
}

ThreeVars(NormDist,79,81)

head(rnorm(1000,80,20))
head(rnorm(1000,80,20))
head(rnorm(1000,80,20))
head(rnorm(1000,80,20))

# Task 2

library('VGAM')

FSApops<-rgpd(51)
FSApops

FSApops<-rpareto(51,572381,1)
FSApops

hist(FSApops)

mean(FSApops)
sd(FSApops)

max(FSApops)
min(FSApops)
  
                           