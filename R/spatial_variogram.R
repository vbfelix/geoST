library(tidyverse)
library(geoR)

s100 <- s100 %>%
          as.data.frame() %>%
          rename(coord_x = X1,
                 coord_y = X2,
                 z = data) %>% 
          mutate(ts = "d")


df <- data.frame(coord_x = rnorm(300), 
                 coord_y = rnorm(300),
                 z       = rnorm(300),
                 ts      = rep(letters[1:3],each = 100)) %>% 
  as_tibble() %>% 
  full_join(s100)


df %>% 
  nest(-ts) %>% 
  mutate(data = map(data,~variog(coords = cbind(.$coord_x,.$coord_y),data = .$z ))) %>% 
  mutate(data = map(data,~as.data.frame(cbind(.x$v,.x$u)))) %>% 
  unnest(data) %>% 
  rename(semi_var = V1, distance = V2) -> df_semivar


# plot --------------------------------------------------------------------

df_semivar %>% 
  mutate(color = if_else(c(0,diff(semi_var)) >=0, "1", "0") ) %>% 
  ggplot(aes(distance,semi_var))+
  geom_line()+
  geom_point(aes(col = color),size = 2, show.legend = F)+
  facet_wrap(.~ts)+
  theme_bw(18)+
  labs(x = "Distance",
       y = "Semivariance")+
  scale_color_brewer(palette = "Set1") -> p1
         

df_semivar %>% 
  mutate(aux = 1) %>% 
  group_by(ts) %>% 
  mutate(distance = cumsum(aux)) %>% 
  ggplot(aes(as.factor(ts), as.factor(distance) ,fill = semi_var))+
  geom_tile(col = "white")+
  theme_bw(18)+
  labs(x = "Time Series",
       y = "Distance",
       fill = expression(gamma[s]))+
  theme(legend.position  = "top",
        legend.key.width = unit(2.5,"cm"),
        legend.key.size  = unit(.45,"cm"))+
  scale_fill_viridis_c() ->p2


library(cowplot)
plot_grid(p1,p2,labels = "AUTO") 
