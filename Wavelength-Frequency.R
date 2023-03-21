#
# Relation between wavelength and frequency
#
# https://nl.wikipedia.org/wiki/Geluidssnelheid#Geluidssnelheid_in_lucht
# https://en.wikipedia.org/wiki/Speed_of_sound
#
# Antoine van Kampen
#

library(ggplot2)

rm(list=ls())

T1 = 20 # temperature in Celsius
c = 20 * sqrt (273 + T1) # speed of sound (m/s)
print (c)

#c=lambda*freq  [m] * [1/s])
f = seq(from=20, to=25000, by= 10) #frequency
l = c/f  #lambda

fl1 = data.frame(cbind(f,l))
colnames(fl1) <- c('Freq','Lambda')
fl1


#make second curve at different temperature
#In the log-log plot the effect of Celsius cannot be distinguished.
T2 = 100 # temperature in Celsius
c = 20 * sqrt (273 + T2) # speed of sound (m/s)
print (c)

#c=lambda*freq  [m] * [1/s])
f = seq(from=10, to=25000, by= 10) #frequency
l = c/f  #lambda

fl2 = data.frame(cbind(f,l))
colnames(fl2) <- c('Freq','Lambda')
fl2




#Construct decades
df     <- data.frame(matrix(vector(), 0, 4), stringsAsFactors=F)
CNames <- c('f_low','f_center','f_high','BW')

fc   <- 1000
MultiplicationFactor <- 10
s    <- seq(from=-3,to=1,by=1)

for (i in s) {
  f    <- fc * MultiplicationFactor^i
  fl   <- f / sqrt(MultiplicationFactor)
  fh   <- f * sqrt(MultiplicationFactor)
  BW   <- 100*(fh-fl)/f
  r    <- round(c(fl,f,fh,BW),1)
  df   <- rbind(df, setNames(r,names(df)))
}
colnames(df) <- CNames
df

freq = c()
for (i in 1:dim(df)[1]) {
  FreqStart    = df$f_center[i]
  FreqInterval = df$f_center[i]
  s = seq(from=FreqStart,to=MultiplicationFactor*FreqStart,by=FreqInterval)
  freq=c(freq,s)
}
freq = unique(freq) #remove the frequencies that occur twice
df2  = data.frame(freq=as.integer(freq))
df2 = data.frame(df2[which(df2$freq>=10 & df2$freq <=30000),])
colnames(df2) <- c("freq")
df2

ggplot(NULL, aes(x=Freq,y=Lambda))+
  geom_point(aes(color=factor(T1)), data=fl1,cex=0.3)+
  geom_point(aes(color=factor(T2)),data=fl2,size=0.3)+
  geom_vline(xintercept=df2$freq, linetype="solid", 
             color = "gray", size=0.5)+
  labs(title="Wavelength vs Frequency", x ="Frequency (Hz)", y = "Wavelength (m)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(trans='log10',labels = df2$freq, breaks = df2$freq) +
  scale_y_continuous(trans='log10')+
  labs(color="Temperature")+
  theme(legend.title = element_text(face = "bold"))+
  theme(legend.key = element_rect(fill = "white"))+
  guides(color = guide_legend(override.aes = list(size = 5)))
