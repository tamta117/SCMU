#create prediction plots for models

library(effects)

#camera murrelet
plot(allEffects(m1),selection=1,xlab="Image type",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m1),selection=2,xlab="Time of day",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m1),selection=3,xlab="Day of the year",
     ylab="Probability of \n murrelet detection",rescale.axis=F)

#camera raven
plot(allEffects(m2.5),selection=1,xlab="Image type",
     ylab="Probability of \n raven detection",rescale.axis=F)
plot(allEffects(m2.5),selection=2,xlab="Time of day",
     ylab="Probability of \n raven detection",rescale.axis=F)
plot(allEffects(m2.5),selection=3,xlab="Day of the year",
     ylab="Probability of \n raven detection",rescale.axis=F)
plot(allEffects(m5),selection=3,xlab="Day of the year",
     ylab="Probability of murrelet detection", rescale.axis=F)

#acoustic
plot(allEffects(m5),selection=1,xlab="Time difference",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m5),selection=2,xlab="Time of day",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
plot(allEffects(m5),selection=3,xlab="Day of the year",
     ylab="Probability of \n murrelet detection",rescale.axis=F)
