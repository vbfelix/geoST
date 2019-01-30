WMCC_HEATMAP<-function(inputDATA,
                       lmax=36,
                       J=6,
                       Wname="d4",
                       name="WMCC",
                       save=F,name1,name2,order_factor=1:J,print_corr=F){

  
# Libraries -----------------------------------------------------------------
library(waveslim)
library(wavemulcor)
library(reshape2)
library(ggplot2)
library(dplyr)
library(forcats)

NAMES <- colnames(inputDATA)
MNL <- dim(inputDATA)
M <- MNL[2]
N <- MNL[1]
xx <- seq(-lmax, lmax, 1)
Nx <- length(xx)
yy <- 1:J
Ny <- length(yy)
LIST <- as.list("NULL")

for (l in 1:M) {
  tmp.wt <- modwt(inputDATA[, l], Wname, J)
  tmp.bw <- brick.wall(tmp.wt, Wname)
  LIST[l] <- list(tmp.bw)
}

library(data.table)
DADOS<-rbindlist(LIST)
LS <- wave.multiple.cross.correlation(LIST, lmax)
returns.cross.cor <- as.matrix(LS$xy.mulcor[1:J, ])
YmaxR <- LS$YmaxR

lags <- length(-lmax:lmax)
lower.ci <- tanh(atanh(returns.cross.cor) - qnorm(0.975)/sqrt(matrix(trunc(N/2^(1:J)), 
                                                                     nrow = J, ncol = lags) - 3))
upper.ci <- tanh(atanh(returns.cross.cor) + qnorm(0.975)/sqrt(matrix(trunc(N/2^(1:J)), 
                                                                     nrow = J, ncol = lags) - 3))

VEClab <- seq(-lmax, lmax, 5)
j <- 1:J
VECJ <- paste(2^(j),"-",2^(j+1),sep="")

data<-t(returns.cross.cor)
colnames(data)<-1:J

data<-melt(data)
data$lag<-rep(xx,J)
data$lower<-as.vector(t(lower.ci))
data$upper<-as.vector(t(upper.ci))
colnames(data)<-c("Id",'Scale','WMC',"Lag","Lower","Upper")
data$Scale<-rep(VECJ,each=nrow(data)/J)

# data$Scale<-factor(data$Scale,levels(data$Scale)[order_factor])
data$aux <- data$Scale %>%  as.character() %>%  readr::parse_number() %>% as.numeric()
# print(levels(data$Scale))

head(data)

# Line plot ------------------------------------------------------------------
 WMCC1<- ggplot(data, aes(Lag))+ 
  geom_line(aes(y=WMC),col=2,size=1.2)+ 
  facet_wrap(~Scale,ncol = 2)+
  geom_line(aes(y=Lower),col=1,size=1.2,linetype="dashed") + 
  geom_line(aes(y=Upper),col=1,size=1.2,linetype="dashed") +
  labs(list(x="Lag",y=name))+theme(text=element_text(size = 15))



# Heatmap -----------------------------------------------------------------

mid<-0;li<- -1
count<- sum(data$WMC>=0)
n<-nrow(data)

if(count ==n){mid<-.5;li<-0}

data %>% 
  mutate(Scale = fct_reorder(Scale,aux)) %>% 
  ggplot(aes(y=Lag, x=Scale, fill = WMC))+
  geom_tile(color = "white")+ 
  labs(list(y="Lag",x="Scale")) +
  scale_fill_gradient2(low = "blue",
                                    high = "firebrick3",
                                    mid = "gold", 
                                    midpoint = mid,
                                    limit = c(li,1),
                                    space = "Lab", 
                                    name=name) +
  theme_minimal()+
  scale_y_continuous(limits = c(-(lmax +1),(lmax+1) ), expand = c(0,0),
                      breaks = seq(-lmax, lmax, by = J))+
  theme(legend.position="top",legend.key.width = unit(2, "cm")) -> WMCC3



# Out ---------------------------------------------------------------------

 
if(print_corr==T){ WMCC3<- WMCC3+ geom_text(aes(x=Lag, y=Scale,fill =WMC, label = round(WMC, 2)))}
 
 if(save==T){
   ggsave(filename = name1,plot = WMCC1,height = 10,width = 12)
   ggsave(filename = name2,plot = WMCC3,height = 10,width = 12)}

 PLOT<-list(WMCC1,WMCC3,data)
 return(PLOT)
 }



