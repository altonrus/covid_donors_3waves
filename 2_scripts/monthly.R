# Generating Supplement Figure S3 - S8
data6_all <- rbind(data5_alpha, data5_delta, data5_omicron)

df_counts <- data6 %>%
  group_by(Month) %>%
  summarize(count = n())

plot(df_counts)

data5_alpha %>%
  group_by(Month) %>%
  dplyr::summarize(count = n())

data5_delta %>%
  group_by(Month) %>%
  dplyr::summarize(count = n())

data5_omicron %>%
  group_by(Month) %>%
  dplyr::summarize(count = n())

data5_delta[data5_delta$Province == 'ON', ]

require(plyr)
library(spdep)
library(brms)
library(dplyr)
month_list <- sort(unique(data6$Month))
pr_name <- 'ON'
dat <- data6[data6$Province == 'ON' & data6$Month %in% month_list[22:24], ]

pr_name <- 'BC'
dat <- data6[data6$Province == 'BC' & data6$Month %in% month_list[22:24], ]

pr_name <- c('AB', 'SK', 'MB')
loo_list <- list()
start_time <- Sys.time()
for (i in 3:8){
  dat <- data6[data6$Province %in% pr_name & data6$Month %in% month_list[(3*i-2):(3*i)], ]
  dim(dat)
  no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
  no_show_cd
  pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
  col_sp <- as(pr, "Spatial")
  col_nb <- poly2nb(col_sp) # queen neighborhood
  col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
  W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
  rownames(W) = pr$CDUID
  colnames(W) = pr$CDUID
  dim(W)
  
  print(i)
  trials = rep(1,nrow(dat))
  dat$trials <- trials
#  start_time <- Sys.time()
  mod_on <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                data = dat, data2 = list(W = W),
                family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, 
                control = list(adapt_delta = 0.95, max_treedepth = 10), cores = 4) 
#  end_time <- Sys.time()
#  end_time - start_time
  print(mod_on)
  
  start_time <- Sys.time()
  mod_on_glm <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                    family = bernoulli(link = "logit"), chains = 4, iter = 2000, warmup = 1000, thin = 1,
                    data = dat, cores = 4, save_pars = save_pars(all = TRUE))
  end_time <- Sys.time()
  end_time - start_time
  print(mod_on_glm)
  
#  loo_compare(loo(mod_on), loo(mod_on_glm))
  #loo(mod_on, mod_on_glm, moment_match = TRUE)
  a <- loo_compare(loo(mod_on), loo(mod_on_glm))[,1]
  loo_list <- append(loo_list, a)
  
  df_urban <- rbind(df_urban, fixef(mod_on, pars = c("Urban"))[1,])
  df_QuintMat <- rbind(df_QuintMat, fixef(mod_on, pars = c("QuintMat"))[1,])
  df_QuintSoc <- rbind(df_QuintSoc, fixef(mod_on, pars = c("QuintSoc"))[1,])
  df_age <- rbind(df_age, fixef(mod_on, pars = c("age"))[1,])
  df_Sex <- rbind(df_Sex, fixef(mod_on, pars = c("Sex"))[1,])
  df_Race <- rbind(df_Race, fixef(mod_on, pars = c("Race"))[1,])
  
  df_urban_glm <- rbind(df_urban_glm, fixef(mod_on_glm, pars = c("Urban"))[1,])
  df_QuintMat_glm <- rbind(df_QuintMat_glm, fixef(mod_on_glm, pars = c("QuintMat"))[1,])
  df_QuintSoc_glm <- rbind(df_QuintSoc_glm, fixef(mod_on_glm, pars = c("QuintSoc"))[1,])
  df_age_glm <- rbind(df_age_glm, fixef(mod_on_glm, pars = c("age"))[1,])
  df_Sex_glm <- rbind(df_Sex_glm, fixef(mod_on_glm, pars = c("Sex"))[1,])
  df_Race_glm <- rbind(df_Race_glm, fixef(mod_on_glm, pars = c("Race"))[1,])
}
end_time <- Sys.time()
end_time - start_time

pr_name <- c('AB', 'SK', 'MB')
#### Nova Scotia, Prince Edward Island, New Brunswick, Newfoundland
pr_name <- c('NS', 'PE', 'NB', 'NL')
#pr_name <- c('NS', 'PE', 'NL')
loo_list <- list()

dat <- dat[!(dat$cduid %in% as.integer(names(rowSums(W))[rowSums(W)==0])), ]
i=3
dat <- data6[data6$Province %in% pr_name & data6$Month %in% month_list[(3*i-2):(3*i)], ]
dim(dat)
no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
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
mod_on <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'escar'), 
               data = dat, data2 = list(W = W),
               family = bernoulli(logit), chains = 4, iter = 4000, warmup = 3000, thin = 1, 
               control = list(adapt_delta = 0.999, max_treedepth = 10), cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod_on)

start_time <- Sys.time()
mod_on_glm <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                    family = bernoulli(link = "logit"), chains = 4, iter = 2000, warmup = 1000, thin = 1,
                    data = dat, control = list(adapt_delta = 0.99, max_treedepth = 10), cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod_on_glm)

#loo_compare(loo(mod_on), loo(mod_on_glm))
#loo(mod_on, mod_on_glm, moment_match = TRUE)
a <- loo_compare(loo(mod_on), loo(mod_on_glm))[,1]
loo_list <- append(loo_list, a)
#waic_mod <- NULL
#waic_glm <- NULL
waic_mod <- append(waic_mod, waic(mod_on)$estimates['waic', 'Estimate'])
waic_glm <- append(waic_glm, waic(mod_on_glm)$estimates['waic', 'Estimate'])

#waic_mod <- append(waic_mod, 0)
#waic_glm <- append(waic_glm, 0)
# If spatial model works
df_urban <- rbind(df_urban, fixef(mod_on, pars = c("Urban"))[1,])
df_QuintMat <- rbind(df_QuintMat, fixef(mod_on, pars = c("QuintMat"))[1,])
df_QuintSoc <- rbind(df_QuintSoc, fixef(mod_on, pars = c("QuintSoc"))[1,])
df_age <- rbind(df_age, fixef(mod_on, pars = c("age"))[1,])
df_Sex <- rbind(df_Sex, fixef(mod_on, pars = c("Sex"))[1,])
df_Race <- rbind(df_Race, fixef(mod_on, pars = c("Race"))[1,])

df_urban_glm <- rbind(df_urban_glm, fixef(mod_on_glm, pars = c("Urban"))[1,])
df_QuintMat_glm <- rbind(df_QuintMat_glm, fixef(mod_on_glm, pars = c("QuintMat"))[1,])
df_QuintSoc_glm <- rbind(df_QuintSoc_glm, fixef(mod_on_glm, pars = c("QuintSoc"))[1,])
df_age_glm <- rbind(df_age_glm, fixef(mod_on_glm, pars = c("age"))[1,])
df_Sex_glm <- rbind(df_Sex_glm, fixef(mod_on_glm, pars = c("Sex"))[1,])
df_Race_glm <- rbind(df_Race_glm, fixef(mod_on_glm, pars = c("Race"))[1,])
dim(df_urban)

# If spatial doesn't work
df_urban <- rbind(df_urban, rep(0, 4))
df_QuintMat <- rbind(df_QuintMat, rep(0, 4))
df_QuintSoc <- rbind(df_QuintSoc, rep(0, 4))
df_age <- rbind(df_age, rep(0, 4))
df_Sex <- rbind(df_Sex, rep(0, 4))
df_Race <- rbind(df_Race, rep(0, 4))

df_urban_glm <- rbind(df_urban_glm, fixef(mod_on_glm, pars = c("Urban"))[1,])
df_QuintMat_glm <- rbind(df_QuintMat_glm, fixef(mod_on_glm, pars = c("QuintMat"))[1,])
df_QuintSoc_glm <- rbind(df_QuintSoc_glm, fixef(mod_on_glm, pars = c("QuintSoc"))[1,])
df_age_glm <- rbind(df_age_glm, fixef(mod_on_glm, pars = c("age"))[1,])
df_Sex_glm <- rbind(df_Sex_glm, fixef(mod_on_glm, pars = c("Sex"))[1,])
df_Race_glm <- rbind(df_Race_glm, fixef(mod_on_glm, pars = c("Race"))[1,])

#########
# If both don't work
df_urban <- rbind(df_urban, rep(0, 4))
df_QuintMat <- rbind(df_QuintMat, rep(0, 4))
df_QuintSoc <- rbind(df_QuintSoc, rep(0, 4))
df_age <- rbind(df_age, rep(0, 4))
df_Sex <- rbind(df_Sex, rep(0, 4))
df_Race <- rbind(df_Race, rep(0, 4))

df_urban_glm <- rbind(df_urban_glm, rep(0, 4))
df_QuintMat_glm <- rbind(df_QuintMat_glm, rep(0, 4))
df_QuintSoc_glm <- rbind(df_QuintSoc_glm, rep(0, 4))
df_age_glm <- rbind(df_age_glm, rep(0, 4))
df_Sex_glm <- rbind(df_Sex_glm, rep(0, 4))
df_Race_glm <- rbind(df_Race_glm, rep(0, 4))

#remove the last row
df_urban <- df_urban[1:(nrow(df_urban)-1),]
df_QuintMat <- df_QuintMat[1:(nrow(df_QuintMat)-1),]
df_QuintSoc <- df_QuintSoc[1:(nrow(df_QuintSoc)-1),]
df_age <- df_age[1:(nrow(df_age)-1),]
df_Sex <- df_Sex[1:(nrow(df_Sex)-1),]
df_Race <- df_Race[1:(nrow(df_Race)-1),]

df_urban_glm <- df_urban_glm[1:(nrow(df_urban_glm)-1),]
df_QuintMat_glm <- df_QuintMat_glm[1:(nrow(df_QuintMat_glm)-1),]
df_QuintSoc_glm <- df_QuintSoc_glm[1:(nrow(df_QuintSoc_glm)-1),]
df_age_glm <- df_age_glm[1:(nrow(df_age_glm)-1),]
df_Sex_glm <- df_Sex_glm[1:(nrow(df_Sex_glm)-1),]
df_Race_glm <- df_Race_glm[1:(nrow(df_Race_glm)-1),]



df_urban <- NULL
df_urban <- rbind(df_urban, fixef(mod_on, pars = c("Urban"))[1,])
df_QuintMat <- NULL
df_QuintMat <- rbind(df_QuintMat, fixef(mod_on, pars = c("QuintMat"))[1,])
df_QuintSoc <- NULL
df_QuintSoc <- rbind(df_QuintSoc, fixef(mod_on, pars = c("QuintSoc"))[1,])
df_age <- NULL
df_age <- rbind(df_age, fixef(mod_on, pars = c("age"))[1,])
df_Sex <- NULL
df_Sex <- rbind(df_Sex, fixef(mod_on, pars = c("Sex"))[1,])
df_Race <- NULL
df_Race <- rbind(df_Race, fixef(mod_on, pars = c("Race"))[1,])

df_urban_glm <- NULL
df_urban_glm <- rbind(df_urban_glm, fixef(mod_on_glm, pars = c("Urban"))[1,])
df_QuintMat_glm <- NULL
df_QuintMat_glm <- rbind(df_QuintMat_glm, fixef(mod_on_glm, pars = c("QuintMat"))[1,])
df_QuintSoc_glm <- NULL
df_QuintSoc_glm <- rbind(df_QuintSoc_glm, fixef(mod_on_glm, pars = c("QuintSoc"))[1,])
df_age_glm <- NULL
df_age_glm <- rbind(df_age_glm, fixef(mod_on_glm, pars = c("age"))[1,])
df_Sex_glm <- NULL
df_Sex_glm <- rbind(df_Sex_glm, fixef(mod_on_glm, pars = c("Sex"))[1,])
df_Race_glm <- NULL
df_Race_glm <- rbind(df_Race_glm, fixef(mod_on_glm, pars = c("Race"))[1,])

#fit glm use r built-in package
start_time <- Sys.time()
mod_on_glm <- glm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                  family = binomial(link = "logit"), data = dat)
end_time <- Sys.time()
end_time - start_time
print(mod_on_glm)
summary(mod_on_glm)

library(ggplot2)
df8 <- df_urban_glm
df8 <- data.frame(df8)
df8$wave <- rep(c('q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8'), 4)
df8$region <- c(rep('ON',8), rep('BC',8), rep('Prairies',8), rep('Atlantic',8))
df8[25,] <- NA
ggplot(df8, aes(x=wave, y=Estimate, colour=region)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd, na.rm = TRUE) +
  ylab("Coefficient Estimate of Urban from GLM") +
  theme_bw()
#df8[25,] <- c(rep(NaN, 4), 'q1', 'Atlantic')
#df8[25,] <- NA
df8 <- df_urban_glm
df8 <- data.frame(df8)
df8$wave <- rep(c('q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8'), 4)
df8$region <- c(rep('ON',8), rep('BC',8), rep('Prairies',8), rep('Atlantic',8))
df8[25,] <- NA
ggplot(df8, aes(x=wave, y=Estimate, colour=region)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd, na.rm = TRUE) +
  ylab("Coefficient Estimate of Urban from GLM") +
  theme_bw()

dat <- data6[data6$Province %in% pr_name & data6$Month %in% month_list[7:12], ]
dat <- data6[data6$Province %in% pr_name & data6$Month %in% month_list[13:15], ]

i=3
dat <- data6[data6$Province %in% pr_name & data6$Month %in% month_list[(3*i-2):(3*i)], ]
dat %>% 
  group_by(Province, cur_result_n) %>% 
  dplyr::summarise(Count = n())


dat <- data5_omicron[data5_omicron$Province %in% pr_name, ]
dat %>% 
  group_by(Province, cur_result_n) %>% 
  dplyr::summarise(Count = n())

df1 <-
dat %>% 
  group_by(Province, cduid) %>% 
  dplyr::summarise(Count = n())

df2 <-
  dat %>% 
  group_by(Province, cduid) %>% 
  dplyr::summarise(Count = n())

df3 <-
  dat %>% 
  group_by(Province, cduid) %>% 
  dplyr::summarise(Count = n())

df4 <-
  dat %>% 
  group_by(fsa) %>% 
  dplyr::summarise(Count = n_distinct(pid))

View(fsa_to_cd[fsa_to_cd$pr %in% c(10, 11, 12, 13),])

df_urban_1 <- df_urban
df_urban_glm_1 <- df_urban_glm
start_time <- Sys.time()
mod_on_ri <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + (1 | cduid),
                 family = bernoulli(link = "logit"), chains = 4, iter = 2000, warmup = 1000, thin = 1,
                 data = dat, control = list(adapt_delta = 0.9, max_treedepth = 10), cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod_on_ri)

loo_compare(loo(mod_on), loo(mod_on_ri))[,1]
loo_compare(waic(mod_on), waic(mod_on_glm))

library(xlsx)
write.xlsx(df_urban, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="urban", row.names=FALSE)
write.xlsx(df_QuintMat, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="QuintMat", append=TRUE, row.names=FALSE)
write.xlsx(df_QuintSoc, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="QuintSoc", append=TRUE, row.names=FALSE)
write.xlsx(df_age, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="age", append=TRUE, row.names=FALSE)
write.xlsx(df_Sex, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="Sex", append=TRUE, row.names=FALSE)
write.xlsx(df_Race, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="Race", append=TRUE, row.names=FALSE)

write.xlsx(df_urban_glm, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="urban_glm", append=TRUE, row.names=FALSE)
write.xlsx(df_QuintMat_glm, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="QuintMat_glm", append=TRUE, row.names=FALSE)
write.xlsx(df_QuintSoc_glm, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="QuintSoc_glm", append=TRUE, row.names=FALSE)
write.xlsx(df_age_glm, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="age_glm", append=TRUE, row.names=FALSE)
write.xlsx(df_Sex_glm, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="Sex_glm", append=TRUE, row.names=FALSE)
write.xlsx(df_Race_glm, file="/home/yuanyu/projects/covid_donors_3waves/coefficients.xlsx", sheetName="Race_glm", append=TRUE, row.names=FALSE)
#write.xlsx(dataframe2, file="filename.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

View(
  data4 %>%
  group_by(Race) %>%
  summarise(Count=n()))

View(
  data4 %>%
    group_by(QuintMat) %>%
    summarise(Count=n()))

i=29
waic_mod[i] <- waic(mod_on)$estimates['waic', 'Estimate']
waic_glm[i] <- waic(mod_on_glm)$estimates['waic', 'Estimate']

a <- loo_compare(loo(mod_on), loo(mod_on_glm))[,1]
loo_list[(2*i-1):(2*i)] <- a

df_urban[i, ] <- fixef(mod_on, pars = c("Urban"))[1,]
df_QuintMat[i, ] <- fixef(mod_on, pars = c("QuintMat"))[1,]
df_QuintSoc[i, ] <- fixef(mod_on, pars = c("QuintSoc"))[1,]
df_age[i, ] <- fixef(mod_on, pars = c("age"))[1,]
df_Sex[i, ] <- fixef(mod_on, pars = c("Sex"))[1,]
df_Race[i, ] <- fixef(mod_on, pars = c("Race"))[1,]

df_urban_glm[i, ] <- fixef(mod_on_glm, pars = c("Urban"))[1,]
df_QuintMat_glm[i, ] <- fixef(mod_on_glm, pars = c("QuintMat"))[1,]
df_QuintSoc_glm[i, ] <- fixef(mod_on_glm, pars = c("QuintSoc"))[1,]
df_age_glm[i, ] <- fixef(mod_on_glm, pars = c("age"))[1,]
df_Sex_glm[i, ] <- fixef(mod_on_glm, pars = c("Sex"))[1,]
df_Race_glm[i, ] <- fixef(mod_on_glm, pars = c("Race"))[1,]


df8 <- df_urban
df8 <- data.frame(df8)
df8$wave <- rep(c('q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8'), 4)
df8$region <- c(rep('ON',8), rep('BC',8), rep('Prairies',8), rep('Atlantic',8))
df8[25,] <- NA
df8[26,] <- NA
df8[27,] <- NA
ggplot(df8, aes(x=wave, y=Estimate, colour=region)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd, na.rm = TRUE) +
  ylab("Coefficient Estimate of Urban from GLM with spatial random effect") +
  theme_bw()

#trend&error plot for each demographic variables
df8 <- rbind(df_QuintSoc, df_QuintSoc_glm)
df8 <- data.frame(df8)
df8$wave <- rep(c('q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8'), 8)
df8$region <- rep(c(rep('ON',8), rep('BC',8), rep('Prairies',8), rep('Atlantic',8)), 2)
df8[25,] <- NA
df8[26,] <- NA
df8[27,] <- NA
df8[25+32,] <- NA
df8$model <- c(rep('MLM spatial',32), rep('GLM',32))
pd <- position_dodge(0.4)
# ggplot(df8, aes(x=wave, y=Estimate, colour=region, shape=model)) + 
# #  geom_line(aes(group = region, type=model), na.rm = TRUE, position=pd) +
#   geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.2, position=pd) +
# #  geom_linerange(aes(y = Estimate), yrange = c(mean(data$ymin, data$ymax)), color = "red")
#   geom_point(position=pd, na.rm = TRUE) + 
# #  geom_line(aes(y = Estimate), position=pd, na.rm = TRUE) +
#   ylab("Coefficient Estimate of Urban from different Models") +
# #  ylim(-1.4, 3) +
#   coord_cartesian(ylim=c(-1.4, 3)) +
#   theme_bw()

month_list_cb <- c("Dec 2020-Feb 2021", "Mar-May 2021", "Jun-Aug 2021",
                   "Sep-Nov 2021", "Dec 2021-Feb 2022", "Mar-May 2022",
                   "Jun-Aug 2022", "Sep-Nov 2022")

df8$wave <- rep(month_list_cb, 8)
df8$wave <- factor(df8$wave, levels=month_list_cb)
df8_1 <- df8[df8$model == 'GLM',]
df8_2 <- df8[df8$model == 'MLM spatial',]

both_QuintSoc <-
ggplot(df8[complete.cases(df8), ], aes(x=wave, y=Estimate, colour=region, shape=model)) + 
  geom_line(data=df8_1[complete.cases(df8_1), ], aes(group = region), na.rm = TRUE, position=pd, linetype='dotted') +
  geom_line(data=df8_2[complete.cases(df8_2), ], aes(group = region), na.rm = TRUE, position=pd, linetype='dashed') +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.2, position=pd) +
  geom_point(position=pd, na.rm = TRUE, size = 2) + 
  ylab("Coefficient Estimate") +
  coord_cartesian(ylim=c(-.3, .6)) + #c(-1.4, 3), c(-.75, .5), c(-.5, .75)
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  ggtitle('Social Deprivation Quintile') +
  xlab('time') +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0))


df8 <- rbind(df_urban, df_urban_glm)
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

#only glm
df8 <- rbind(df_QuintSoc, df_QuintSoc_glm)
df8 <- data.frame(df8)
df8$wave <- rep(c('q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8'), 8)
df8$region <- rep(c(rep('ON',8), rep('BC',8), rep('Prairies',8), rep('Atlantic',8)), 2)
df8[25,] <- NA
df8[26,] <- NA
df8[27,] <- NA
df8[25+32,] <- NA
df8$model <- c(rep('spatial',32), rep('glm',32))

df8$wave <- rep(month_list_cb, 8)
df8$wave <- factor(df8$wave, levels=month_list_cb)

df8[df8$model == 'glm',]$model <- 'GLM'
df8[df8$model == 'spatial',]$model <- 'MLM spatial'
df8_1 <- df8[df8$model == 'GLM',]
df8_2 <- df8[df8$model == 'MLM spatial',]
month_soc_1 <-
ggplot(df8_1[complete.cases(df8_1), ], aes(x=wave, y=Estimate, colour=region)) + 
  geom_line(data=df8_1[complete.cases(df8_1), ], aes(group = region), na.rm = TRUE, position=pd, linetype='dotted') +
#  geom_line(data=df8_2[complete.cases(df8_2), ], aes(group = region), na.rm = TRUE, position=pd, linetype='dashed') +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.2, position=pd) +
  geom_point(position=pd, na.rm = TRUE, size = 2) + 
  ylab("Coefficient Estimate") +
  coord_cartesian(ylim=c(-.5, .55)) + #urban: c(-1.4, 3), c(-.75, .5), c(-.5, .75)
  #soc:c(-.7, .55) race:c(-.6, 1.1) mat:c(-.5, .5) age: c(-.06, .06) sex: c(1,1)
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  ggtitle('Social Deprivation Quintile (GLM)') +
  xlab('time') +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0))

month_soc_2 <-
  ggplot(df8_2[complete.cases(df8_2), ], aes(x=wave, y=Estimate, colour=region)) + 
  geom_line(data=df8_2[complete.cases(df8_2), ], aes(group = region), na.rm = TRUE, position=pd, linetype='dotted') +
  #  geom_line(data=df8_2[complete.cases(df8_2), ], aes(group = region), na.rm = TRUE, position=pd, linetype='dashed') +
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.2, position=pd) +
  geom_point(position=pd, na.rm = TRUE, size = 2, shape = 17) + 
  ylab("Coefficient Estimate") +
  coord_cartesian(ylim=c(-.5, .55)) + #urban: c(-1.4, 3), c(-.75, .5), c(-.5, .75)
  #soc:c(-.7, .55) race:c(-.6, 1.1) mat:c(-.5, .5) sex: c(-1, 1)
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  ggtitle('Social Deprivation Quintile (MLM spatial)') +
  xlab('time') +
  theme_bw() +
  theme(plot.title = element_text(vjust = 0))

sup1_age <- ggarrange(month_age_1, month_age_2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_sex <- ggarrange(month_sex_1, month_sex_2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_race <- ggarrange(month_race_1, month_race_2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_urban <- ggarrange(month_urban_1, month_urban_2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_mat <- ggarrange(month_mat_1, month_mat_2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_soc <- ggarrange(month_soc_1, month_soc_2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


sup1_urban_age <- ggarrange(both_urban, both_age, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_race_sex <- ggarrange(both_race, both_sex, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

ggsave("4_output/figs/sup1_age.png", sup1_age, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_sex.png", sup1_sex, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_race.png", sup1_race, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_urban.png", sup1_urban, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_mat.png", sup1_mat, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_soc.png", sup1_soc, height = 8, width = 20, dpi= 320, bg='#ffffff')




######### Breakline ############
sup1_A <- ggdraw() + draw_plot(month_QuintMat, x = -0, y = -0, scale = 1)
sup1_B <- ggdraw() + draw_plot(month_QuintSoc, x = -0, y = -0, scale = 1)
sup1_C <- ggdraw() + draw_plot(month_urban, x = -0, y = -0, scale = 1)
sup1_ABC <- plot_grid(sup1_A, sup1_B, sup1_C,
                      ncol = 1, nrow = 3)

ggsave("4_output/figs/sup1_areal.png", sup1_ABC, height = 12, width = 11, dpi= 320, bg='#ffffff')

sup2_A <- ggdraw() + draw_plot(month_age, x = -0, y = -0, scale = 1)
sup2_B <- ggdraw() + draw_plot(month_Race, x = -0, y = -0, scale = 1)
sup2_C <- ggdraw() + draw_plot(month_Sex, x = -0, y = -0, scale = 1)
sup2_ABC <- plot_grid(sup2_A, sup2_B, sup2_C,
                      ncol = 1, nrow = 3)

ggsave("4_output/figs/sup2_ind.png", sup2_ABC, height = 12, width = 11, dpi= 320, bg='#ffffff')

### New time trend plots with the results from both models #######

sup1_A <- ggdraw() + draw_plot(both_QuintMat, x = -0, y = -0, scale = 1)
sup1_B <- ggdraw() + draw_plot(both_QuintSoc, x = -0, y = -0, scale = 1)
sup1_C <- ggdraw() + draw_plot(both_urban, x = -0, y = -0, scale = 1)
sup1_ABC <- plot_grid(sup1_A, sup1_B, sup1_C,
                      ncol = 1, nrow = 3)

ggsave("4_output/figs/sup1_areal.png", sup1_ABC, height = 12, width = 11, dpi= 320, bg='#ffffff')

sup2_A <- ggdraw() + draw_plot(both_age, x = -0, y = -0, scale = 1)
sup2_B <- ggdraw() + draw_plot(both_race, x = -1, y = -0, scale = 1)
sup2_C <- ggdraw() + draw_plot(both_sex, x = -0, y = -0, scale = 1)
sup2_ABC <- plot_grid(sup2_A, sup2_B, sup2_C,
                      ncol = 1, nrow = 3)

ggsave("4_output/figs/sup2_ind.png", sup2_ABC, height = 12, width = 11, dpi= 320, bg='#ffffff')

sup1_A + theme(legend.position = "none")

sup_ABC <- plot_grid(sup1_ABC, sup2_ABC, 
                     ncol = 2, nrow = 1)


sup1_AB <- plot_grid(sup1_A, sup1_B, 
                      ncol = 2, nrow = 1)
sup1_AB

library(ggpubr)
sup1_mat_soc <- ggarrange(both_QuintMat, both_QuintSoc, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_urban_age <- ggarrange(both_urban, both_age, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
sup1_race_sex <- ggarrange(both_race, both_sex, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

ggsave("4_output/figs/sup1_mat_soc.png", sup1_mat_soc, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_urban_age.png", sup1_urban_age, height = 8, width = 20, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sup1_race_sex.png", sup1_race_sex, height = 8, width = 20, dpi= 320, bg='#ffffff')
