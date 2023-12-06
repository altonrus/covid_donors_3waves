##### Compare the estimation of the seropositivity at census division level
library(brms)
library(dplyr)
data_df <- posterior_predict(mod_A_abc_on, ndraws = 1000)
data_df <- data.frame(t(data_df))
#pr_name <- 'BC'
dat <- mod_A_abc_on$data
#dat <- data7[data7$wave == 'alpha' & data7$region == 'BC', ]
#dat <- data7[data7$wave == 'alpha' & data7$region == 'BC' & data7$cduid!=5949, ]
data_df$cur_result_n <- dat$interp_roche_n
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

est_by_area_obs <- est_by_area[c("cduid", "cur_result_n", "counts", "post_mean", "X_025", "X_975")]
est_by_area_obs$post_mean <- est_by_area$cur_result_n
est_by_area_obs$X_025 <- NA
est_by_area_obs$X_975 <- NA


######## glm
data_df <- posterior_predict(mod_A_cbs_on, ndraws = 1000)
data_df <- data.frame(t(data_df))
dat <- mod_A_cbs_on$data
data_df$cur_result_n <- dat$interp_roche_n
data_df$cduid <- dat$cduid

data_df <- data_df %>%
  select(cduid, cur_result_n, everything())

est_by_area_glm <-
  data_df %>% 
  group_by(cduid) %>% 
  dplyr::summarise(across(c('cur_result_n', starts_with('X')), mean))

library(dplyr)
est_by_area_counts <-
  dat %>% 
  group_by(cduid) %>% 
  dplyr::summarise(counts=n())

est_by_area_glm <- merge(est_by_area_glm, est_by_area_counts, by = 'cduid', x.all = TRUE)

est_by_area_glm$cduid <- factor(est_by_area_glm$cduid)
#quantile(x, 0.95)

fun1 <- function(x) {sort(x)[0.025*length(x)]}

fun2 <- function(x) {sort(x)[0.975*length(x)]}

est_by_area_glm$post_mean <- apply(est_by_area_glm[,2:1002], 1, mean)
est_by_area_glm$X_025 <- apply(est_by_area_glm[,2:1002], 1, fun1)
est_by_area_glm$X_975 <- apply(est_by_area_glm[,2:1002], 1, fun2)
#est_by_area_glm$post_mean <- apply(est_by_area_glm[,2:1002], 1, mean)
#est_by_area_glm$post_sd <- apply(est_by_area_glm[,2:1002], 1, sd)
#est_by_area_glm$rab <- 
#est_by_area_glm$prmse <-  



est_by_area_obs2 <- est_by_area_glm[c("cduid", "cur_result_n", "counts", "post_mean", "X_025", "X_975")]
est_by_area_obs2$post_mean <- est_by_area_glm$cur_result_n
est_by_area_obs2$X_025 <- NA
est_by_area_obs2$X_975 <- NA

est_by_area_all <- rbind(est_by_area[c("cduid", "cur_result_n", "counts", "post_mean", "X_025", "X_975")],
                         est_by_area_glm[c("cduid", "cur_result_n", "counts", "post_mean", "X_025", "X_975")],
                         est_by_area_obs[c("cduid", "cur_result_n", "counts", "post_mean", "X_025", "X_975")],
                         est_by_area_obs2[c("cduid", "cur_result_n", "counts", "post_mean", "X_025", "X_975")])
est_by_area_all$model <- c(rep('ABC', nrow(est_by_area)), rep('CBS', nrow(est_by_area_glm)), 
                           rep('Obs_abc', nrow(est_by_area_obs)), rep('Obs_cbs', nrow(est_by_area_obs2)))

#check how many divisions that spatial model generate more accurate estimation compared to the glm
a <- (abs(est_by_area_all[est_by_area_all$model == 'MLM spatial',]$post_mean - est_by_area_all[est_by_area_all$model == 'MLM spatial',]$cur_result_n)
      -abs(est_by_area_all[est_by_area_all$model == 'GLM',]$post_mean - est_by_area_all[est_by_area_all$model == 'GLM',]$cur_result_n))
length(a)- sum(a>0)
length(a)

library(ggplot2)
#p <- ggplot(est_by_area, aes(x=cduid, y=post_mean))
pd <- position_dodge(0.4)
pd_1 <- position_dodge(0.8)
# pd_1 <- position_dodge2(
#   width = 0.4,
#   preserve = "single",
#   padding = 0.4,
#   reverse = TRUE
# )
#my_colors <- c('green3', 'blue2')

# Define the desired order of groups
desired_order <- c( "CBS", "Obs_cbs", "ABC", 'Obs_abc')

# Create a new column for ordering
est_by_area_all$model <- factor(est_by_area_all$model, levels = desired_order)

est_by_area_all <- est_by_area_all_atl_1
est_by_area_all$model <- factor(est_by_area_all$model, levels = desired_order)
p <- ggplot(est_by_area_all, aes(x = reorder(cduid, counts), y = post_mean, colour=model))
fig2_atl_A<-
  
  p +
  theme_bw()+
  geom_errorbar(aes(ymin=X_025, ymax=X_975), width=0.4, position=pd_1) +
  geom_point(position=pd_1,  size = 0.7) +
  #  scale_color_manual(values = my_colors) +
  #  geom_point(data = est_by_area, aes(x=cduid, y=cur_result_n), position=pd_1, colour = "violetred", size = 1) +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  ylab("Seropositivity") +
  xlab("Census Division (sample size ascending)") +
  ggtitle("Ontario, Pre-Delta") +
  theme(plot.title = element_text(vjust = 0)) + 
  scale_color_manual(values = c("CBS" = "blue", "Obs_cbs" = "violetred", "ABC" = "cadetblue", "Obs_abc" = "darkorange"),
                     name = "type",
                     labels = c( "CBS", "Obs_cbs", "ABC", "Obs_abc"), guide='legend')  
#  theme(legend.position = "none")
#  scale_shape_manual(values = c("obsevation" = "violetred"),
#                     labels = c("obsevation"), guide='legend')

# guides(color = guide_legend(title = "Legend Title",
#                             override.aes = list(size = 2),
#                             nrow = 2,
#                             keywidth = unit(0.5, "cm"),
#                             keyheight = unit(0.5, "cm"),
#                             keysep = unit(0.5, "cm"),
#                             label.theme = element_text(size = 12)))

est_by_area_all_atl_3 <- est_by_area_all
est_by_area_all <- est_by_area_all_atl_2
mod3_bc <- mod_on
mod3_bc_glm <- mod_on_glm

install.packages("performance")
library("performance")

fig1_A <- ggdraw() + draw_plot(fig2, x = -0, y = -0, scale = 0.75)
fig1_B <- ggdraw() + draw_plot(fig3, x = -0.3, y = -0, scale = 1)
fig1_AB <- plot_grid(fig1_A, fig1_B, 
                     ncol = 2, nrow = 1)

ggsave("4_output/figs/panel_AB.png", fig1_AB, height = 5, width = 10, dpi= 320, bg='#ffffff')

fig2_A <- ggdraw() + draw_plot(fig2_on_A, x = -0, y = -0, scale = 1)
fig2_B <- ggdraw() + draw_plot(fig2_on_B, x = -0, y = -0, scale = 1)
fig2_C <- ggdraw() + draw_plot(fig2_on_C, x = -0, y = -0, scale = 1)
fig2_A1 <- ggdraw() + draw_plot(fig2_bc_A, x = -0.12, y = -0, scale = 1)
fig2_B1 <- ggdraw() + draw_plot(fig2_bc_B, x = -0.12, y = -0, scale = 1)
fig2_C1 <- ggdraw() + draw_plot(fig2_bc_C, x = -0.12, y = -0, scale = 1)
fig2_ABC <- plot_grid(fig2_A, fig2_B, fig2_C, 
                      fig2_A1, fig2_B1, fig2_C1,
                      byrow = FALSE,
                      ncol = 2, nrow = 3)


ggsave("4_output/figs/panel_on.png", fig2_ABC, height = 12, width = 16, dpi= 320, bg='#ffffff')

#Figure 2 
fig2_A <- ggdraw() + draw_plot(fig2_on_C, x = -0, y = -0, scale = 1, width = 1.05)
fig2_B <- ggdraw() + draw_plot(fig2_prai_C, x = -0, y = -0, scale = 1, width = 1.05)
fig2_C <- ggdraw() + draw_plot(fig2_bc_C, x = 0.04, y = -0, scale = 1, width = .95)
fig2_D <- ggdraw() + draw_plot(fig2_atl_C, x = 0.04, y = -0, scale = 1, width = .95)

fig2_omicron <- plot_grid(fig2_A, fig2_B, fig2_C, fig2_D,
                          byrow = FALSE,
                          ncol = 2, nrow = 2)

ggsave("4_output/figs/panel_omicron.png", fig2_omicron, height = 12, width = 16, dpi= 320, bg='#ffffff')


#Figure 2 Sup1 Delta
fig2_A <- ggdraw() + draw_plot(fig2_on_B, x = -0, y = -0, scale = 1, width = 1.05)
fig2_B <- ggdraw() + draw_plot(fig2_prai_B, x = -0, y = -0, scale = 1, width = 1.05)
fig2_C <- ggdraw() + draw_plot(fig2_bc_B, x = 0.04, y = -0, scale = 1, width = .95)
fig2_D <- ggdraw() + draw_plot(fig2_atl_B, x = 0.04, y = -0, scale = 1, width = .95)

fig2_delta <- plot_grid(fig2_A, fig2_B, fig2_C, fig2_D,
                        byrow = FALSE,
                        ncol = 2, nrow = 2)

ggsave("4_output/figs/panel_delta.png", fig2_delta, height = 12, width = 16, dpi= 320, bg='#ffffff')

#Figure 2 Sup2 Pre Delta
fig2_A <- ggdraw() + draw_plot(fig2_on_A, x = -0, y = -0, scale = 1, width = 1.05)
fig2_B <- ggdraw() + draw_plot(fig2_prai_A, x = -0, y = -0, scale = 1, width = 1.05)
fig2_C <- ggdraw() + draw_plot(fig2_bc_A, x = 0.04, y = -0, scale = 1, width = .95)
fig2_D <- ggdraw() + draw_plot(fig2_atl_A, x = 0.04, y = -0, scale = 1, width = .95)

fig2_alpha <- plot_grid(fig2_A, fig2_B, fig2_C, fig2_D,
                        byrow = FALSE,
                        ncol = 2, nrow = 2)

ggsave("4_output/figs/panel_alpha.png", fig2_alpha, height = 12, width = 16, dpi= 320, bg='#ffffff')

sum((abs(est_by_area_all[est_by_area_all$model == 'spatial',]$post_mean - est_by_area_all[est_by_area_all$model == 'spatial',]$cur_result_n)
     -abs(est_by_area_all[est_by_area_all$model == 'glm',]$post_mean - est_by_area_all[est_by_area_all$model == 'glm',]$cur_result_n)) > 0)

a <- (abs(est_by_area_all[est_by_area_all$model == 'spatial',]$post_mean - est_by_area_all[est_by_area_all$model == 'spatial',]$cur_result_n)
      -abs(est_by_area_all[est_by_area_all$model == 'glm',]$post_mean - est_by_area_all[est_by_area_all$model == 'glm',]$cur_result_n))
length(a)- sum(a>0)
length(a)


c(25.5,21.4,23.0,22.5) +            
  c(17.3,15.2,14.0,14.8)  +           
  c(3.2,3.2,2.4,2.5)  
