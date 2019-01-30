
x <- rnorm(1000)
y <- x^2 + rnorm(1000)
z <- x + log(abs(y))

df <- data.frame(x, y, z)

out <- WMCC_HEATMAP(df,J = 6,lmax = 42)

out[[1]] -> p1

out[[2]] -> p2

out[[3]] %>% glimpse()

library(cowplot)
plot_grid(p1,p2,labels = "AUTO") 
