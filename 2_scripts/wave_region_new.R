# Generating Figure 1-2, Supplement Figure S1-S2
#BC
pr_name <- 'BC'
data5_alpha <- data5[data5$sampledate >= '2020-12-14' & data5$sampledate < '2021-08-01',]
#data5_alpha <- data5_alpha[data5_alpha$fsa!='P4N',]
data5_delta <- data5[data5$sampledate >= '2021-08-01' & data5$sampledate < '2021-12-15',]
data5_omicron <- data5[data5$sampledate >= '2021-12-01' & data5$sampledate < '2022-12-01',]

data6_all <- data6
data6 <- data5[data5$sampledate >= '2020-12-14' & data5$sampledate < '2022-12-01',]
data6$wave <- 'omicron' 
data6[data6$sampledate >= '2020-12-14' & data6$sampledate < '2021-08-01', ]$wave <- 'alpha'
data6[data6$sampledate >= '2021-08-01' & data6$sampledate < '2021-12-15', ]$wave <- 'delta'

data6$region <- 'ON' 
data6[data6$Province %in% 'BC', ]$region <- 'BC'
data6[data6$Province %in% c('AB', 'SK', 'MB'), ]$region <- 'Prairies'
data6[data6$Province %in% c('NS', 'PE', 'NB', 'NL'), ]$region <- 'Atlantic'


#Check the low count cduid
pr_name <- 'ON'
pr_name <- 'BC'
pr_name <- c('AB', 'SK', 'MB')
pr_name <- c('NS', 'PE', 'NB', 'NL')
dat <- data5_delta[data5_delta$Province %in% pr_name, ]
dat <- data7[data7$wave == 'delta' & data7$region=='ON', ]
View(dat %>% 
       group_by(cduid) %>% 
       dplyr::summarise(Count = n()))

data7 <-
  data6 %>% 
  group_by(wave, region, cduid) %>% 
  filter(n()>=5)


df <- data.frame(
  group = rep(c("A", "B", "C"), each = 3),
  value = rnorm(9)
)
df <- rbind(df, c('D', 1))
# Filter the data frame to keep only rows where the mean value for each group is greater than 0
df_filtered <- df %>%
  group_by(group) %>%
  filter(n() == 1)

pr_name <- 'BC'
pr_name <- c('AB', 'SK', 'MB')
#### Nova Scotia, Prince Edward Island, New Brunswick, Newfoundland
pr_name <- c('NS', 'PE', 'NB', 'NL')
#pr_name <- c('NS', 'NB', 'NL')
loo_list_wave <- list()

pr_name <- 'ON'
dat <- data7[data7$wave == 'delta' & data7$region == 'BC', ]
dat <- dat[dat$cduid != 5949, ] #remove the island to run the esicar model
dat <- data7[data7$wave == 'alpha' & data7$region == 'Atlantic', ]
dat <- dat[dat$cduid != as.integer(names(rowSums(W))[rowSums(W)==0]), ] #1010
dat <- data7[data7$wave == 'delta' & data7$region == 'Atlantic', ]
dat <- dat[dat$cduid != as.integer(names(rowSums(W))[rowSums(W)==0]), ] #1217
#dat <- data7[data7$wave == 'alpha' & data7$Province %in% pr_name, ]
dim(dat)
no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
library(spdep)
library(sf)
require(plyr)
col_sp <- as(pr, "Spatial")
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)
as.integer(names(rowSums(W))[rowSums(W)==0])

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_on <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
              data = dat, data2 = list(W = W), family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
              control = list(adapt_delta = 0.9, max_treedepth = 10), cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_on)

start_time <- Sys.time()
mod_on_glm <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                  family = bernoulli(link = "logit"), chains = 4, iter = 2000, warmup = 1000, thin = 1,
                  data = dat, control = list(adapt_delta = 0.8, max_treedepth = 10), cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod_on_glm)

#loo_compare(loo(mod_on), loo(mod_on_glm))
#loo(mod_on, mod_on_glm, moment_match = TRUE)
a <- loo_compare(loo(mod_on), loo(mod_on_glm))[,1]
loo_list_wave <- append(loo_list_wave, a)
#waic_mod_wave <- NULL
#waic_glm_wave <- NULL
#waic_mod_wave <- append(waic_mod_wave, waic(mod_on)$estimates['waic', 'Estimate'])
#waic_glm_wave <- append(waic_glm_wave, waic(mod_on_glm)$estimates['waic', 'Estimate'])
waic_mod_wave_1 <- NULL
waic_glm_wave_1 <- NULL
waic_mod_wave_1 <- append(waic_mod_wave_1, waic(mod_on)$estimates['waic', 'Estimate'])
waic_glm_wave_1 <- append(waic_glm_wave_1, waic(mod_on_glm)$estimates['waic', 'Estimate'])


# If spatial model works
df_urban_w <- rbind(df_urban_w, fixef(mod_on, pars = c("Urban"))[1,])
df_QuintMat_w <- rbind(df_QuintMat_w, fixef(mod_on, pars = c("QuintMat"))[1,])
df_QuintSoc_w <- rbind(df_QuintSoc_w, fixef(mod_on, pars = c("QuintSoc"))[1,])
df_age_w <- rbind(df_age_w, fixef(mod_on, pars = c("age"))[1,])
df_Sex_w <- rbind(df_Sex_w, fixef(mod_on, pars = c("Sex"))[1,])
df_Race_w <- rbind(df_Race_w, fixef(mod_on, pars = c("Race"))[1,])

df_urban_glm_w <- rbind(df_urban_glm_w, fixef(mod_on_glm, pars = c("Urban"))[1,])
df_QuintMat_glm_w <- rbind(df_QuintMat_glm_w, fixef(mod_on_glm, pars = c("QuintMat"))[1,])
df_QuintSoc_glm_w <- rbind(df_QuintSoc_glm_w, fixef(mod_on_glm, pars = c("QuintSoc"))[1,])
df_age_glm_w <- rbind(df_age_glm_w, fixef(mod_on_glm, pars = c("age"))[1,])
df_Sex_glm_w <- rbind(df_Sex_glm_w, fixef(mod_on_glm, pars = c("Sex"))[1,])
df_Race_glm_w <- rbind(df_Race_glm_w, fixef(mod_on_glm, pars = c("Race"))[1,])
dim(df_urban_w)

# If spatial doesn't work
loo_list_wave <- append(loo_list_wave, list(0, 0))
waic_mod_wave <- append(waic_mod_wave, 0)
waic_glm_wave <- append(waic_glm_wave, waic(mod_on_glm)$estimates['waic', 'Estimate'])

df_urban_w <- rbind(df_urban_w, rep(0, 4))
df_QuintMat_w <- rbind(df_QuintMat_w, rep(0, 4))
df_QuintSoc_w <- rbind(df_QuintSoc_w, rep(0, 4))
df_age_w <- rbind(df_age_w, rep(0, 4))
df_Sex_w <- rbind(df_Sex_w, rep(0, 4))
df_Race_w <- rbind(df_Race_w, rep(0, 4))

df_urban_glm_w <- rbind(df_urban_glm_w, fixef(mod_on_glm, pars = c("Urban"))[1,])
df_QuintMat_glm_w <- rbind(df_QuintMat_glm_w, fixef(mod_on_glm, pars = c("QuintMat"))[1,])
df_QuintSoc_glm_w <- rbind(df_QuintSoc_glm_w, fixef(mod_on_glm, pars = c("QuintSoc"))[1,])
df_age_glm_w <- rbind(df_age_glm_w, fixef(mod_on_glm, pars = c("age"))[1,])
df_Sex_glm_w <- rbind(df_Sex_glm_w, fixef(mod_on_glm, pars = c("Sex"))[1,])
df_Race_glm_w <- rbind(df_Race_glm_w, fixef(mod_on_glm, pars = c("Race"))[1,])


df_urban_w <- NULL
df_QuintMat_w <- NULL
df_QuintSoc_w <- NULL
df_age_w <- NULL
df_Sex_w <- NULL
df_Race_w <- NULL

df_urban_glm_w <- NULL
df_QuintMat_glm_w <- NULL
df_QuintSoc_glm_w <- NULL
df_age_glm_w <- NULL
df_Sex_glm_w <- NULL
df_Race_glm_w <- NULL

#remove the last row
df_urban_w <- df_urban_w[1:(nrow(df_urban_w)-1),]
df_QuintMat_w <- df_QuintMat_w[1:(nrow(df_QuintMat_w)-1),]
df_QuintSoc_w <- df_QuintSoc_w[1:(nrow(df_QuintSoc_w)-1),]
df_age_w <- df_age_w[1:(nrow(df_age_w)-1),]
df_Sex_w <- df_Sex_w[1:(nrow(df_Sex_w)-1),]
df_Race_w <- df_Race_w[1:(nrow(df_Race_w)-1),]

df_urban_glm_w <- df_urban_glm_w[1:(nrow(df_urban_glm_w)-1),]
df_QuintMat_glm_w <- df_QuintMat_glm_w[1:(nrow(df_QuintMat_glm_w)-1),]
df_QuintSoc_glm_w <- df_QuintSoc_glm_w[1:(nrow(df_QuintSoc_glm_w)-1),]
df_age_glm_w <- df_age_glm_w[1:(nrow(df_age_glm_w)-1),]
df_Sex_glm_w <- df_Sex_glm_w[1:(nrow(df_Sex_glm_w)-1),]
df_Race_glm_w <- df_Race_glm_w[1:(nrow(df_Race_glm_w)-1),]

##
df_urban_glm_w <- rbind(df_urban_glm_w, rep(0, 4))
df_QuintMat_glm_w <- rbind(df_QuintMat_glm_w, rep(0, 4))
df_QuintSoc_glm_w <- rbind(df_QuintSoc_glm_w, rep(0, 4))
df_age_glm_w <- rbind(df_age_glm_w, rep(0, 4))
df_Sex_glm_w <- rbind(df_Sex_glm_w, rep(0, 4))
df_Race_glm_w <- rbind(df_Race_glm_w, rep(0, 4))

View(dat %>% 
       group_by(cduid, cur_result_n) %>% 
       dplyr::summarise(Count = n()))

i=10
waic_mod_wave[i] <- waic(mod_on)$estimates['waic', 'Estimate']
waic_glm_wave[i] <- waic(mod_on_glm)$estimates['waic', 'Estimate']

a <- loo_compare(loo(mod_on), loo(mod_on_glm))[,1]
loo_list_wave[(2*i-1):(2*i)] <- a

df_urban_w[i, ] <- fixef(mod_on, pars = c("Urban"))[1,]
df_QuintMat_w[i, ] <- fixef(mod_on, pars = c("QuintMat"))[1,]
df_QuintSoc_w[i, ] <- fixef(mod_on, pars = c("QuintSoc"))[1,]
df_age_w[i, ] <- fixef(mod_on, pars = c("age"))[1,]
df_Sex_w[i, ] <- fixef(mod_on, pars = c("Sex"))[1,]
df_Race_w[i, ] <- fixef(mod_on, pars = c("Race"))[1,]

df_urban_glm_w[i, ] <- fixef(mod_on_glm, pars = c("Urban"))[1,]
df_QuintMat_glm_w[i, ] <- fixef(mod_on_glm, pars = c("QuintMat"))[1,]
df_QuintSoc_glm_w[i, ] <- fixef(mod_on_glm, pars = c("QuintSoc"))[1,]
df_age_glm_w[i, ] <- fixef(mod_on_glm, pars = c("age"))[1,]
df_Sex_glm_w[i, ] <- fixef(mod_on_glm, pars = c("Sex"))[1,]
df_Race_glm_w[i, ] <- fixef(mod_on_glm, pars = c("Race"))[1,]
dim(df_urban_w)

library(ggplot2)
df8 <- df_urban_glm_w
df8 <- data.frame(df8)
df8$wave <- rep(c('pre-delta', 'delta', 'omicron'), 4)
df8$wave <- factor(df8$wave, levels=c('pre-delta', 'delta', 'omicron'))
df8$region <- c(rep('ON',3), rep('BC',3), rep('Prairies',3), rep('Atlantic',3))
ggplot(df8, aes(x=wave, y=Estimate, colour=region)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  ylab("Coefficient Estimate of Urban from regular GLM") +
  theme_bw()

df8 <- df_urban_w
df8 <- data.frame(df8)
df8$wave <- rep(c('pre-delta', 'delta', 'omicron'), 4)
df8$wave <- factor(df8$wave, levels=c('pre-delta', 'delta', 'omicron'))
df8$region <- c(rep('ON',3), rep('BC',3), rep('Prairies',3), rep('Atlantic',3))
ggplot(df8, aes(x=wave, y=Estimate, colour=region)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  ylab("Coefficient Estimate of Urban from GLM with Spatial Random effect") +
  theme_bw()

df8 <- rbind(df_urban_w, df_urban_glm_w)
df8 <- data.frame(df8)
df8$wave <- rep(c('pre-delta', 'delta', 'omicron'), 8)
df8$wave <- factor(df8$wave, levels=c('pre-delta', 'delta', 'omicron'))
df8$region <- rep(c(rep('ON',3), rep('BC',3), rep('Prairies',3), rep('Atlantic',3)),2)
df8$type <- c(rep('spatial',12), rep('glm',12))
pd <- position_dodge(0.3)
ggplot(df8, aes(x=wave, y=Estimate, colour=region, shape=type)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.2, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  ylab("Coefficient Estimate of Urban") +
  theme_bw()

df8 <- rbind(df_age_w, df_age_glm_w)
df8 <- data.frame(df8)
df8$wave <- rep(c('pre-delta', 'delta', 'omicron'), 8)
df8$wave <- factor(df8$wave, levels=c('pre-delta', 'delta', 'omicron'))
df8$region <- rep(c(rep('ON',3), rep('BC',3), rep('Prairies',3), rep('Atlantic',3)),2)
df8$model <- c(rep('spatial',12), rep('glm',12))
pd <- position_dodge(0.3)
ggplot(df8, aes(x=wave, y=Estimate, colour=region, shape=model)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.2, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  ylab("Coefficient Estimate of Age under different Models") +
  theme_bw()

#Areal Level Coefficient Estimation
df9 <- rbind(df_urban_w, df_urban_glm_w, df_QuintMat_w, df_QuintMat_glm_w, df_QuintSoc_w, df_QuintSoc_glm_w)
df9 <- data.frame(df9)
df9$wave <- rep(c('pre-delta', 'delta', 'omicron'), 24)
df9$wave <- factor(df9$wave, levels=c('pre-delta', 'delta', 'omicron'))
df9$region <- rep(c(rep('ON',3), rep('BC',3), rep('Prairies',3), rep('Atlantic',3)),6)
df9$model <- rep(c(rep('MLM spatial',12), rep('GLM',12)),3)
df9$cov <- c(rep('Urban', 24), rep('QuintMat', 24), rep('QuintSoc', 24))
#pd <- position_dodge(0.3)
fig2 <-
ggplot(df9, aes(x=wave, y=Estimate, colour=region, shape=model)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.15, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
#  ylab("Coefficient Estimate of Areal Level Covariates Under different Models") +
  labs(y = "") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~cov, nrow = 3, scales="free") +
  labs(subtitle = 'A. Coefficient Estimate of Areal Level Covariates')

df10 <- rbind(df_age_w, df_age_glm_w, df_Sex_w, df_Sex_glm_w, df_Race_w, df_Race_glm_w)
df10 <- data.frame(df10)
df10$wave <- rep(c('pre-delta', 'delta', 'omicron'), 24)
df10$wave <- factor(df10$wave, levels=c('pre-delta', 'delta', 'omicron'))
df10$region <- rep(c(rep('ON',3), rep('BC',3), rep('Prairies',3), rep('Atlantic',3)),6)
df10$model <- rep(c(rep('MLM spatial',12), rep('GLM',12)),3)
df10$cov <- c(rep('Age', 24), rep('Sex', 24), rep('Race', 24))
#pd <- position_dodge(0.3)
fig3 <-
ggplot(df10, aes(x=wave, y=Estimate, colour=region, shape=model)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.15, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
#  ylab("Coefficient Estimate of Individual Level Covariates Under different Models") +
  labs(y = "") +
  theme_bw() +
  facet_wrap(~cov, nrow = 3, scales="free") +
  labs(subtitle = 'B. Coefficient Estimate of Individual Level Covariates')

library(ggplot2)
library(cowplot)
fig1_A <- ggdraw() + draw_plot(fig2, x = -0, y = -0, scale = 1)
fig1_B <- ggdraw() + draw_plot(fig3, x = -0, y = -0, scale = 1)
fig1_AB <- plot_grid(fig1_A, fig1_B, 
                     ncol = 2, nrow = 1)
fig1_AB

fig1_A <- ggdraw() + draw_plot(fig2, x = -0, y = -0, scale = 1)
fig1_B <- ggdraw() + draw_plot(fig3, x = -0.1, y = -0, scale = 1)
fig1_AB <- plot_grid(fig1_A, fig1_B, 
                     ncol = 2, nrow = 1)


install.packages("ragg")
library(ragg)
ggsave("/home/yuanyu/projects/covid_donors_3waves/4_output/figs/panel_AB.png", fig1_AB, height = 7, width = 15, dpi= 320, bg='#ffffff')

##### Compare the estimation of the seropositivity at census division level
##############
library(dplyr)
data_df <- posterior_predict(mod2_on, ndraws = 1000)
data_df <- data.frame(t(data_df))
data_df$cur_result_n <- dat$cur_result_n
data_df$cduid <- dat$cduid

data_df <- data_df %>%
  select(cduid, cur_result_n, everything())

est_by_area <-
  data_df %>% 
  group_by(cduid) %>% 
  dplyr::summarise(across(c('cur_result_n', starts_with('X')), mean))

library(dplyr)
est_by_area_counts <-
  dat %>% 
  group_by(cduid) %>% 
  dplyr::summarise(counts=n())

est_by_area <- merge(est_by_area, est_by_area_counts, by = 'cduid', x.all = TRUE)

est_by_area$cduid <- factor(est_by_area$cduid)
#quantile(x, 0.95)

fun1 <- function(x) {sort(x)[0.025*length(x)]}

fun2 <- function(x) {sort(x)[0.975*length(x)]}

est_by_area$post_mean <- apply(est_by_area[,2:1002], 1, mean)
est_by_area$X_025 <- apply(est_by_area[,2:1002], 1, fun1)
est_by_area$X_975 <- apply(est_by_area[,2:1002], 1, fun2)

#glm
data_df <- posterior_predict(mod2_on_glm, ndraws = 1000)
data_df <- data.frame(t(data_df))
data_df$cur_result_n <- dat$cur_result_n
data_df$cduid <- dat$cduid

data_df <- data_df %>%
  select(cduid, cur_result_n, everything())

est_by_area_glm <-
  data_df %>% 
  group_by(cduid) %>% 
  dplyr::summarise(across(c('cur_result_n', starts_with('X')), mean))


est_by_area_counts <-
  dat %>% 
  group_by(cduid) %>% 
  dplyr::summarise(counts=n())

est_by_area_glm <- merge(est_by_area_glm, est_by_area_counts, by = 'cduid', x.all = TRUE)

est_by_area_glm$cduid <- factor(est_by_area_glm$cduid)
#quantile(x, 0.95)

fun1 <- function(x) {sort(x)[0.025*length(x)]}

fun2 <- function(x) {sort(x)[0.975*length(x)]}

est_by_area_glm$post_mean_glm <- apply(est_by_area_glm[,2:1002], 1, mean)
est_by_area_glm$X_025_glm <- apply(est_by_area_glm[,2:1002], 1, fun1)
est_by_area_glm$X_975_glm <- apply(est_by_area_glm[,2:1002], 1, fun2)
est_by_area_glm$post_mean <- apply(est_by_area_glm[,2:1002], 1, mean)

library(ggplot2)
#p <- ggplot(est_by_area, aes(x=cduid, y=post_mean))
pd <- position_dodge(0.4)
pd_1 <- position_dodge2(
  width = 0.4,
  preserve = "single",
  padding = 0.4,
  reverse = TRUE
)
p <- ggplot(est_by_area, aes(x = reorder(cduid, est_by_area_counts$counts), y = post_mean))
p +
  #  geom_point(color = "violetred") +
  theme_bw()+
  #  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #  geom_ribbon(aes(ymin = X_025, ymax = X_975), alpha = 0.2) +
  geom_errorbar(aes(ymin=X_025, ymax=X_975), width=0.3, position=pd_1, colour = "blue") +
  geom_point(data = est_by_area, aes(x=cduid, y=post_mean), position=pd_1,  colour = "blue", size = 0.7) +
  geom_errorbar(data = est_by_area_glm, aes(ymin=X_025_glm, ymax=X_975_glm), position=pd_1,width=0.3, colour = "green") +
  geom_point(data = est_by_area_glm, aes(x=cduid, y=post_mean_glm),position=pd_1, colour = "green", size = 0.7) +
  
  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1,colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Census Division (sample size ascending)") +
  ggtitle("Ontario, Delta") +
  theme(plot.title = element_text(vjust = 0)) +
  guides(color = guide_legend(title = "Legend Title",
                              override.aes = list(size = 2),
                              nrow = 1,
                              keywidth = unit(2, "cm"),
                              keyheight = unit(1, "cm"),
                              keysep = unit(0.5, "cm"),
                              label.theme = element_text(size = 12)))


p <- ggplot(est_by_area, aes(x = reorder(cduid, est_by_area_counts$counts), y = post_mean))
p +
  #  geom_point(color = "violetred") +
  theme_bw()+
  #  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #  geom_ribbon(aes(ymin = X_025, ymax = X_975), alpha = 0.2) +
  geom_errorbar(aes(ymin=X_025, ymax=X_975), width=0.3, position=pd_1, colour = "blue") +
  geom_point(data = est_by_area, aes(x=cduid, y=post_mean), position=pd_1,  colour = "blue", size = 0.7) +
  geom_errorbar(data = est_by_area_glm, aes(ymin=X_025_glm, ymax=X_975_glm), position=pd_1,width=0.3, colour = "green") +
  geom_point(data = est_by_area_glm, aes(x=cduid, y=post_mean_glm),position=pd_1, colour = "green", size = 0.7) +
  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1,colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Census Division (sample size ascending)") +
  ggtitle("Ontario, Delta") +
  theme(plot.title = element_text(vjust = 0)) +
  guides(color = guide_legend(title = "Legend Title",
                              override.aes = list(size = 2),
                              nrow = 1,
                              keywidth = unit(2, "cm"),
                              keyheight = unit(1, "cm"),
                              keysep = unit(0.5, "cm"),
                              label.theme = element_text(size = 12)))

#Check interaction terms age*QuintMat 
trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_on_urbanmat <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + 
                            Urban*QuintMat + car(W, gr = cduid, type = 'esicar'), 
              data = dat, data2 = list(W = W), family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
              control = list(adapt_delta = 0.9, max_treedepth = 10), cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_on_urbanmat)
waic(mod_on)
waic(mod2_on)
waic(mod2_on_glm)

dat %>% group_by(Month) %>% dplyr::summarise(n_post = sum(cur_result_n)) %>% mutate(seropisitivity = n_post/sum(n_post))

dat %>% group_by(Month) %>% dplyr::summarise(n= n())
#check if the vaccination will affect the significant negative effect of the urban indicator

View(data4[data4$Province == 'ON' & data4$Month >= '2021-08-01' & data4$Month <= '2021-12-01', ] %>% group_by(Month) %>% dplyr::summarise(n= n()))
View(data5[data5$Province == 'ON' & data5$Month >= '2021-08-01' & data5$Month <= '2021-12-01', ] %>% group_by(Month) %>% dplyr::summarise(n= n()))
View(data6[data6$Province == 'ON' & data6$Month >= '2021-08-01' & data6$Month <= '2021-12-01', ] %>% group_by(Month) %>% dplyr::summarise(n= n()))
View(data7[data7$Province == 'ON' & data7$Month >= '2021-08-01' & data7$Month <= '2021-12-01', ] %>% group_by(Month) %>% dplyr::summarise(n= n()))
View(data7[data7$Province == 'ON' & data7$Month >= '2021-08-01' & data7$Month <= '2021-12-01' & data7$cur_result_n ==1, ] %>% group_by(Month) %>% dplyr::summarise(n= n()))
View(data7[data7$Province == 'ON' & data7$sampledate >= '2021-12-01' & data7$sampledate < '2021-12-15', ] %>% group_by(sampledate) %>% dplyr::summarise(n= n()))
View(data7[data7$Province == 'ON' & data7$sampledate >= '2021-12-15' & data7$sampledate <= '2021-12-31', ] %>% group_by(sampledate) %>% dplyr::summarise(n= n()))


View(data6[data6$Province == 'ON' & data6$sampledate >= '2021-11-01' & data6$sampledate < '2021-12-15', ] %>% group_by(sampledate) %>% dplyr::summarise(n= n()))
View(data4[data6$Province == 'ON' & data4$sampledate >= '2021-11-01' & data4$sampledate < '2021-12-15', ] %>% group_by(sampledate) %>% dplyr::summarise(n= n()))

View(data7[data7$Province == 'ON' & data7$sampledate >= '2021-12-15' & data7$sampledate <= '2021-12-31', ] %>% group_by(sampledate) %>% dplyr::summarise(n= n()))
View(data7[data7$Province == 'BC' & data7$sampledate >= '2021-12-01' & data7$sampledate < '2021-12-31', ] %>% group_by(sampledate) %>% dplyr::summarise(n= n()))


fixef(mod2_on, pars = c("month"))
fixef(mod2_bc, pars = c("month"))
fixef(mod2_prai, pars = c("month"))
fixef(mod2_atl, pars = c("month"))

#plot the seropositivity per month during delta
dat <- data7[data7$wave == 'delta', ]

df9 <-
dat[dat$sampledate < '2021-12-01', ] %>% 
  group_by(region, sampledate) %>% 
  dplyr::summarise(n_post = sum(cur_result_n)) %>% 
  mutate(seropisitivity = n_post/sum(n_post))

pd <- position_dodge(0.3)
ggplot(df9, aes(x=sampledate, y=seropisitivity, colour=region)) + 
#  geom_line(data=df9, position=pd) +
  geom_point(position=pd, na.rm = TRUE, size = 2) + 
  geom_smooth(method = "loess") +
  ylab("Seropositivity") +
#  coord_cartesian(ylim=c(-1.4, 3)) + #c(-1.4, 3), c(-.75, .5), c(-.5, .75)
#  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  xlab('Delta (2021)') +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0))


df10 <-
  data7[data7$sampledate >= '2021-12-01' & data7$sampledate <= '2021-12-31', ] %>% 
  group_by(region, sampledate) %>% 
  dplyr::summarise(samplesize = n()) 

ggplot(df10, aes(x=sampledate, y=samplesize, colour=region)) + 
  geom_line(data=df10, position=pd) +
  geom_point(position=pd, na.rm = TRUE, size = 2) + 
  ylab("Seropositivity") +
  #  coord_cartesian(ylim=c(-1.4, 3)) + #c(-1.4, 3), c(-.75, .5), c(-.5, .75)
  #  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  xlab('2021 Dec') +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0))

# Urban*QuintMat
trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_on_urbanmat <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + 
                         Urban*QuintMat + car(W, gr = cduid, type = 'esicar'), 
                       data = dat, data2 = list(W = W), family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                       control = list(adapt_delta = 0.9, max_treedepth = 10), cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_on_urbanmat)

# Urban*QuintSoc
trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_on_urbansoc <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + 
                         Urban*QuintSoc + car(W, gr = cduid, type = 'esicar'), 
                       data = dat, data2 = list(W = W), family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                       control = list(adapt_delta = 0.9, max_treedepth = 10), cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_on_urbansoc)

# add region as a random effect
pr_name <- 'ON'
pr_name <- 'BC'
pr_name <- c('AB', 'SK', 'MB')
#### Nova Scotia, Prince Edward Island, New Brunswick, Newfoundland
pr_name <- c('NS', 'PE', 'NB', 'NL')

dat <- data7[data7$wave == 'delta' & data7$region == 'ON', ]
dim(dat)
no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
#library(spdep)
col_sp <- as(pr, "Spatial")
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod_ran_region <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + (1 | cduid), 
                      data = dat, family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                      control = list(adapt_delta = 0.99, max_treedepth = 10), cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_ran_region)
#waic(mod_ran_region)

#waic_ran_wave <- NULL
waic_ran_wave <- append(waic_ran_wave, waic(mod_ran_region)$estimates['waic', 'Estimate'])
waic_ran_wave <- c(10792.0930,   3212.3930, 160681.9930,   3494.6890,   1787.3300,  56580.2700,   
                   9885.0280,  5357.3848,  99358.8659, 500.2214,    500.2214,  31233.6387)

waic(mod3_on)$estimates['waic', 'Estimate']

install.packages(c('haven', 'sas7bdat'))
require(haven)
df <- read_sas("/home/yuanyu/projects/covid_donors_3waves/airline.sas7bdat")
head(df)

group_name <- 1
random_effect_name <- paste0("b_cduid[", group_name, "]")
rhat_result <- rhat(mod_ran_region, pars = random_effect_name)

# Fix Figure 1

df9$covariate <- "Neighborhood covariate"
df10$covariate <- "Individual covariate"
df11 <- rbind(df9, df10)

df9[df9$cov == 'QuintSoc',]$cov <- 'Social deprivation quintile'
df9[df9$cov == 'QuintMat',]$cov <- 'Material deprivation quintile'

df12[df12$model == 'glm',]$model <- 'GLM'
df12[df12$model == 'spatial',]$model <- 'MLM spatial'

fig4 <-
  ggplot(df12, aes(x=wave, y=Estimate, colour=region, shape=model)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.15, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  #  ylab("Coefficient Estimate of Areal Level Covariates Under different Models") +
  labs(y = "") +
  theme_bw() +
#  theme(legend.position = "none") +
  facet_wrap( ~ cov , nrow = 3, scales="free", dir = "v") +
#  facet_grid(vars(cov), vars(covariate)) +
#  facet_grid(cols  = vars(cov),rows  = vars(covariate),scales = "free")
  labs(subtitle = 'Covariates Coefficient Estimate')


ggsave("/home/yuanyu/projects/covid_donors_3waves/4_output/figs/panel_AB.png", fig4, height = 7, width = 15, dpi= 320, bg='#ffffff')

df12 <- df11
df12$cov <- factor 
df12$cov <- factor(df12$cov, levels = c('Age', "Race", "Sex",
                                        "Urban","Material deprivation quintile", "Social deprivation quintile" ))

library(coda)
modelposterior <- as.mcmc(mod1_on_glm) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
gelman.diag(modelposterior[, ])


dat$date1 <- dat$vacdate1 + as.Date('1960-01-01')
dat$date2 <- dat$vacdate2 + as.Date('1960-01-01')
dat$date3 <- dat$vacdate3 + as.Date('1960-01-01')

dat$date_max <- pmax(dat$date1, dat$date2, dat$date3, na.rm = TRUE)
dat$days_to_d1 <- as.double(difftime(dat$sampledate ,dat$date_max , units = c("days")))
dat$vac_ind <- ifelse(dat$days_to_d1 <= 90, 1, 0)

devtools::install_github("kupietz/kableExtra")


