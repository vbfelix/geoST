library(tidyverse)

x = cumsum(rnorm(100))
y = x^2
z = log(y) + x
u = rev(x) + rnorm(100)
w = x + y + z + u 
v = w/y + z - x
  

df <- data.frame(x,y,z,u,w,v) %>% 
  gather(ts,value) %>% 
  as_tibble()



# acf ---------------------------------------------------------------------

lag_max   <- 36
lag_break <- 6

df %>%
  nest(-ts) %>% 
  mutate(data = map(data,~acf(., lag.max=lag_max, type="correlation", plot=F))) %>% 
  mutate(data = map(data, ~as.data.frame(cbind(.x$acf,.x$lag)))) %>% 
  unnest(data) %>% 
  rename(acf = V1, lag = V2) %>% 
  filter(lag != 0) %>% 
  glimpse()-> df_acf

df_acf %>% 
  ggplot(aes(lag,acf))+
  theme_bw(18) +
  geom_col(aes(fill = acf),col = "black", show.legend = F) +
  facet_wrap(.~ts)+
  geom_hline(col = "black",yintercept = 0, size = 1)+
  geom_hline(col = "black",yintercept = seq(-.5,.5, by = .25), linetype = "dashed", alpha = .6)+
  labs(x = "Lag", y = "ACF")+
  scale_fill_gradient2(low = "royalblue3",
                       mid = "burlywood",
                       high = "firebrick3",
                       midpoint = 0,
                       limits = c(-1,1))+
  scale_x_continuous(breaks = c(1,seq(0,lag_max, by = lag_break)),
                     expand = c(0,0)) -> p1


df_acf %>% 
  ggplot(aes(ts,lag))+
  theme_bw(18) +
  theme(legend.position  = "top",
        legend.key.width = unit(2.5,"cm"),
        legend.key.size  = unit(.45,"cm"))+
  geom_tile(aes(fill = acf),col = "black") +
  labs(x = "Time Series",y = "Lag", fill = "ACF:")+
  scale_fill_gradient2(low = "royalblue3",
                       mid = "burlywood",
                       high = "firebrick3",
                       midpoint = 0,
                       limits = c(-1,1),
                       expand = c(0,0),
                       breaks = seq(-1,1,by =.25))+
  scale_y_continuous(breaks = c(1,seq(0,lag_max, by = lag_break)), expand = c(0,0))+
  scale_x_discrete(expand = c(0,0)) -> p2

library(cowplot)
plot_grid(p1,p2,labels = "AUTO") 
