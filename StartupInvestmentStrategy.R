# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(tidyverse) # for data manipulations
library(cowplot) # for plotting in a grid
library(dplyr)
library(latex2exp)


### Prettier numbers for X-labels, credit here: https://github.com/fdryan/R/blob/master/ggplot2_formatter.r
human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}
human_num   <- function(x){human_numbers(x, smbl = "")} 

model <- cmdstan_model("Multilinear.stan")

data <- read.csv("50_startups.csv")
stan_data <- list(n = nrow(data), research = data$research, admin = data$administration,
                  marketing = data$marketing, profit = data$profit)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)

# diagnostics
mcmc_trace(fit$draws())
fit$summary()
mcse(df$ba)
#extracting betas for plot titles
df <- as_draws_df(fit$draws())
brest <- round(mcse(df$br)$est,2)
baest <- round(mcse(df$ba)$est,2)
bmest <- round(mcse(df$bm)$est,2)

a <- dnorm(mcse(df$br)$est, mcse(df$br)$se)


### 1000 samples
df_100 <- drop_na(sample_n(df, 100))

#plotting
p1 <- ggplot() + 
  geom_point(data=data, aes(x=research, y=profit), shape=16, color="#537fe0") +
  geom_smooth(data=data, aes(x=research, y=profit, color="research"), method='lm', color="#537fe0")+
  theme_cowplot() + labs(title=expr(beta[r] %~~% !!brest)) +scale_x_continuous(labels = human_num)


p2 <- ggplot() + 
  geom_point(data=data, aes(x=marketing, y=profit), shape=16, color="#e340b2") +
  geom_smooth(data=data, aes(x=marketing, y=profit, color="marketing"), method='lm', color="#e340b2") +
  theme_cowplot() + labs(title=expr(beta[m] %~~% !!bmest)) + theme(legend.position="none")+scale_x_continuous(labels = human_num)

p3<- ggplot() + 
  geom_point(data=data, aes(x=administration, y=profit), shape=16, color="#4bbf43") +
  geom_smooth(data=data, aes(x=administration, y=profit, color="admin"), method='lm', color="#4bbf43") +
  theme_cowplot() + theme(legend.position="none") + labs(title=expr(beta[a] %~~% !!baest))+scale_x_continuous(labels = human_num)
p4 <- ggplot() + 
  geom_point(data=data, aes(x=0.72*research + 0.33*administration + 0.08*marketing, y=profit), shape=16, color="#e6ca3e") +
  geom_smooth(data=data, aes(x=0.72*research + 0.33*administration + 0.08*marketing, y=profit, color="composite"),method='lm') +
  geom_abline(data=df_100, aes(slope=ba, intercept=intercept, color="admin"), alpha=0.05, size=1)+
  geom_abline(data=df_100, aes(slope=bm, intercept=intercept, color="marketing"), alpha=0.05, size=1)+
  geom_abline(data=df_100, aes(slope=br, intercept=intercept, color="research"), alpha=0.05, size=1)+
  theme_cowplot() +
    labs(title='Composite ML regression',
       x=expr(beta[r]*research + beta[m]*marketing + beta[a]*administration),
       color="Investment sector") + scale_color_manual(values=c("#4bbf43", "#e6ca3e", "#e340b2", "#537fe0")) + theme(legend.position="none")+
  scale_x_continuous(labels = human_num)

legend_b <- get_legend(
  p4 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_grid(plot_grid(p1, p2, p3, ncol = 3),p4,legend_b,ncol=1, rel_heights = c(0.8,1, .1))


### Task 2 - per state distributions
data <- read.csv("50_startups.csv")
dataNY <- data[data$state=='NewYork',]
dataCali <- data[data$state=='California',]
dataFlorida <- data[data$state=='Florida',]

#########NEWYORK################
data <- dataNY
stan_data <- list(n = nrow(data), research = data$research, admin = data$administration,
                  marketing = data$marketing, profit = data$profit)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)
df1 <- as_draws_df(fit$draws())




#########CALIFORNIA################
data <- dataCali
stan_data <- list(n = nrow(data), research = data$research, admin = data$administration,
                  marketing = data$marketing, profit = data$profit)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)
df2 <- as_draws_df(fit$draws())


#########FLORIDA################
data <- dataFlorida
stan_data <- list(n = nrow(data), research = data$research, admin = data$administration,
                  marketing = data$marketing, profit = data$profit)

# fit
fit <- model$sample(
  data = stan_data,
  seed = 1
)
df3 <- as_draws_df(fit$draws())

###Plotting of the per-state distribution plot
NY<-ggplot() + geom_density(data=df1, aes(x=br, fill="research"), alpha=0.7) + geom_density(data=df1, aes(x=ba, fill='admin'), alpha=0.7) + geom_density(data=df1, aes(x=bm, fill='marketing'), alpha=0.7) + ggtitle('New York')+
  xlim(-0.1, 1.2)+theme_cowplot() + xlab(expression(beta))+ scale_fill_manual(values = c("#4bbf43", "#e340b2","#537fe0")) + theme(legend.position="none")
Cali<-ggplot() + geom_density(data=df2, aes(x=br, fill='research'), alpha=0.7) + geom_density(data=df2, aes(x=ba, fill='admin'), alpha=0.7) + geom_density(data=df2, aes(x=bm, fill='marketing'), alpha=0.7) + ggtitle('California')+
  xlim(-0.1, 1.2)+theme_cowplot() + xlab(expression(beta))+ scale_fill_manual(values = c("#4bbf43", "#e340b2","#537fe0")) + theme(legend.position="none")
Flori<-ggplot() + geom_density(data=df3, aes(x=br, fill='research'), alpha=0.7) + geom_density(data=df3, aes(x=ba, fill='admin'), alpha=0.7) + geom_density(data=df3, aes(x=bm, fill='marketing'), alpha=0.7) + ggtitle('Florida')+
  xlim(-0.1, 1.2)+theme_cowplot() + xlab(expression(beta)) + 
  scale_fill_manual(values = c("#4bbf43", "#e340b2","#537fe0")) + theme(legend.position="none") + labs(fill='Investment sector')

legend_b <- get_legend(
  Flori + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
plot_grid(plot_grid(NY, Cali, Flori, ncol = 1),legend_b, rel_heights = c(1,0.1), ncol=1)
