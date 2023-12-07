#Generating Supplement Table S4, Figure S13
library(brms)
library(dplyr)
library(lubridate)
data2 <- data1 %>% 
  group_by(pid) %>%
  sample_n(1)

data2 <- rbind(data1 %>% filter(sampledate >= '2020-12-14' & sampledate <= '2021-07-31') %>% group_by(pid) %>% sample_n(1),
               data1 %>% filter(sampledate >= '2021-08-01' & sampledate <= '2021-12-14') %>% group_by(pid) %>% sample_n(1),
               data1 %>% filter(sampledate >= '2021-12-15') %>% group_by(pid) %>% sample_n(1))
data1$week <- week(data1$sampledate)
mean(table((data1 %>% filter(sampledate >= '2020-12-14' & sampledate <= '2021-07-31'))$week))  
mean(table((data1 %>% filter(sampledate >= '2021-08-01' & sampledate <= '2021-12-14'))$week)) 
mean(table((data1 %>% filter(sampledate >= '2021-12-15'))$week)) 

data1 <- data1 %>% mutate(year_month = format(data1$sampledate, "%Y-%m"))
table(data1$year_month)

library(tableone)
library(kableExtra)
#Export tableone to csv
#xyz= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1,addOverall = T), showAllLevels = T,format="fp"))
x= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(sampledate < '2021-07-01'),addOverall = T), showAllLevels = T,format="fp"))
y= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(sampledate > '2021-07-01' & sampledate <= '2021-12-14'),addOverall = T), showAllLevels = T,format="fp"))
#z= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "omicron"),addOverall = T), showAllLevels = T,format="fp"))

tab_csv <- print(x, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_before.csv")
tab_csv <- print(y, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_after.csv")
dt <- read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_after.csv")
#dt[1, 1] <- cell_spec(dt[1, 1], "latex", underline = T)
#dt[2, 2] <- cell_spec(dt[2, 1], "latex", underline = T)
#dt[1,2] <- paste0("\\textcolor{red}{", dt[2,2], "}")
#dt[2,2] <- paste0("\\textbf{", dt[2,2], "}")
#Run this
kbl(dt[2:7], booktabs = T) %>%
  #kable_paper(bootstrap_options = "striped", full_width = F) %>%
  kable_classic_2(full_width = F)  %>%
  row_spec(0, bold = T) %>%
  pack_rows("Sex (%)", 2, 3) %>%
  pack_rows("Age Group (%)", 4, 10) %>%
  pack_rows("Race (%)", 11, 12) %>%
  pack_rows("Social Deprivation Quintile (%)", 13, 17) %>%
  pack_rows("Material Deprivation Quintile (%)", 18, 22) %>%
  pack_rows("Urban (%)", 23, 24)


####
require(plyr)
install.packages('spdep')
install.packages('sf')
library(spdep)
library(sf)
library(sp)
pr_name <- 'ON'
#dat <- data5_delta[data5_delta$Province == 'ON', ]
#pre-delta
dat <- data2[data2$wave == "pre-delta" & data2$Province == 'ON', ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod1_on_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
               data = dat, data2 = list(W = W),
               family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
               cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod1_on_u)

start_time <- Sys.time()
mod1_on_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                   family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                   data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod1_on_glm_u)

#delta
dat <- data2[data2$wave == "delta" & data2$Province == 'ON', ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod2_on_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.95),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod2_on_u)

start_time <- Sys.time()
mod2_on_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod2_on_glm_u)

#omicron
dat <- data2[data2$wave == "omicron" & data2$Province == 'ON', ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod3_on_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod3_on_u)

start_time <- Sys.time()
mod3_on_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod3_on_glm_u)


options(error=recover)
options(error=NULL)
reach_full_in <- reachability(krack_full, 'in')

## BC

pr_name <- 'BC'
#dat <- data5_delta[data5_delta$Province == 'BC', ]
#pre-delta
dat <- data2[data2$wave == "pre-delta" & data2$Province == 'BC', ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod1_bc_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod1_bc_u)

start_time <- Sys.time()
mod1_bc_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod1_bc_glm_u)

#delta
dat <- data2[data2$wave == "delta" & data2$Province == 'BC', ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod2_bc_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.95),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod2_bc_u)

start_time <- Sys.time()
mod2_bc_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod2_bc_glm_u)

#omicron
dat <- data2[data2$wave == "omicron" & data2$Province == 'BC', ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod3_bc_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod3_bc_u)

start_time <- Sys.time()
mod3_bc_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod3_bc_glm_u)


dim(dat[dat$age>=75,])
dat <- data2[data2$wave == "omicron" & data2$Province == 'BC', ]
dat <- dat[dat$age>=75,]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod3_bc_u_70 <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'icar'), 
                 data = dat[dat$age>=70,], data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod3_bc_u_70)

dat <- data1[data1$wave == "delta", ]
start_time <- Sys.time()
mod3_bc_glm_u_70 <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat[dat$age>=75,], cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod3_bc_glm_u_70)

df_month <- NULL
df_month <- rbind(df_month, fixef(mod1_on, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod2_on, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod3_on, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod1_bc, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod2_bc, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod3_bc, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod1_prai, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod2_prai, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod3_prai, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod1_atl, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod2_atl, pars = c("month"))[1,])
df_month <- rbind(df_month, fixef(mod3_atl, pars = c("month"))[1,])

df_month <- as.data.frame(df_month)
df_month$region <- c(rep('ON', 3), rep('BC', 3), rep('Prairies', 3), rep('Atlantic', 3))
df_month$wave <- rep(c('pre-delta','delta', 'omicron'), 4)
df_month[df_month$wave == 'pre-delta',]
df_month[df_month$wave == 'delta',]
df_month[df_month$wave == 'omicron',]


df_month_glm <- NULL
df_month_glm <- rbind(df_month_glm, fixef(mod1_on_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod2_on_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod3_on_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod1_bc_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod2_bc_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod3_bc_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod1_prai_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod2_prai_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod3_prai_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod1_atl_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod2_atl_glm, pars = c("month"))[1,])
df_month_glm <- rbind(df_month_glm, fixef(mod3_atl_glm, pars = c("month"))[1,])

df_month_glm <- as.data.frame(df_month_glm)
df_month_glm$region <- c(rep('ON', 3), rep('BC', 3), rep('Prairies', 3), rep('Atlantic', 3))
df_month_glm$wave <- rep(c('pre-delta','delta', 'omicron'), 4)
df_month_glm[df_month_glm$wave == 'pre-delta',]
df_month_glm[df_month_glm$wave == 'delta',]
df_month_glm[df_month_glm$wave == 'omicron',]


### Prairies
pr_name <- c('AB', 'SK', 'MB')
#dat <- data5_delta[data5_delta$Province == 'ON', ]
#pre-delta
dat <- data2[data2$wave == "pre-delta" & data2$Province %in% pr_name, ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod1_prai_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.99),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod1_prai_u)

start_time <- Sys.time()
mod1_prai_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod1_prai_glm_u)

#delta
dat <- data2[data2$wave == "delta" & data2$Province %in% pr_name, ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod2_prai_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod2_prai_u)

start_time <- Sys.time()
mod2_prai_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod2_prai_glm_u)

#omicron
dat <- data2[data2$wave == "omicron" & data2$Province %in% pr_name, ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod3_prai_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                 data = dat, data2 = list(W = W),
                 family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                 cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod3_prai_u)

start_time <- Sys.time()
mod3_prai_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                     family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                     data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod3_prai_glm_u)

## Atlantic
### Prairies
pr_name <- c('NL', 'NS', 'PE', 'NB')
#dat <- data5_delta[data5_delta$Province == 'ON', ]
#pre-delta
dat <- data2[data2$wave == "pre-delta" & data2$Province %in% pr_name, ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
as.integer(names(rowSums(W))[rowSums(W)==0])
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

PRIORS <- c(set_prior("normal(-0.5,.3)", class = "b", coef= "age"),
            set_prior("normal(0,1)",  class = "b", coef= "Sex"),
            set_prior("normal(-1,0.5)",   class = "b", coef= "Race"),
            set_prior("normal(0,1)",   class = "b", coef= "QuintMat"),
            set_prior("normal(0,5)",   class = "b", coef= "QuintSoc"),
            set_prior("normal(0,5)",   class = "b", coef= "Urban"),
            set_prior("normal(0,5)",   class = "b", coef= "month"),
            set_prior("cauchy(0,10)",  class = "Intercept", coef = "" ))

PRIORS1 <-PRIORS
trials = rep(1,nrow(dat))

dat$trials <- trials
start_time <- Sys.time()
mod1_atl_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                   data = dat[dat$cduid!=1010,], data2 = list(W = W),
                   family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9999999),
                   cores = 4, prior = PRIORS) 
end_time <- Sys.time()
end_time - start_time
print(mod1_atl_u)

start_time <- Sys.time()
mod1_atl_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                       family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                       data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod1_atl_glm_u)

#delta
dat <- data2[data2$wave == "delta" & data2$Province %in% pr_name, ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod2_atl_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                   data = dat, data2 = list(W = W),
                   family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.95),
                   cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod2_atl_u)

start_time <- Sys.time()
mod2_atl_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                       family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                       data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod2_atl_glm_u)

#omicron
dat <- data2[data2$wave == "omicron" & data2$Province %in% pr_name, ]

no_show_cd <- setdiff(unique(fsa_to_cd[which(fsa_to_cd$pr %in% pruid[pr_name]),]$cduid), unique(dat[dat$Province %in% pr_name,]$cduid))
no_show_cd
pr <- canada[canada$PRUID %in% pruid[pr_name] & !canada$CDUID %in% c(no_show_cd), ]
col_sp <- as(pr, "Spatial")
#col_sp <- as_Spatial(pr)
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure
rownames(W) = pr$CDUID
colnames(W) = pr$CDUID
dim(W)

trials = rep(1,nrow(dat))
dat$trials <- trials
start_time <- Sys.time()
mod3_atl_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month + car(W, gr = cduid, type = 'esicar'), 
                   data = dat, data2 = list(W = W),
                   family = bernoulli(logit), chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9),
                   cores = 4) 
end_time <- Sys.time()
end_time - start_time
print(mod3_atl_u)

start_time <- Sys.time()
mod3_atl_glm_u <- brm(cur_result_n ~ age + Sex + Race + QuintMat + QuintSoc + Urban + month,
                       family = bernoulli(link = "logit"),chains = 2, iter = 2000, warmup = 1000, thin = 1,
                       data = dat, cores = 4)
end_time <- Sys.time()
end_time - start_time
print(mod3_atl_glm_u)

#####
# If spatial model works
df_urban_w1 <- NULL
df_QuintMat_w1 <- NULL
df_QuintSoc_w1 <- NULL
df_age_w1 <- NULL
df_Sex_w1 <- NULL
df_Race_w1 <- NULL

df_urban_glm_w1 <- NULL
df_QuintMat_glm_w1 <- NULL
df_QuintSoc_glm_w1 <- NULL
df_age_glm_w1 <- NULL
df_Sex_glm_w1 <- NULL
df_Race_glm_w1 <- NULL

df_urban_w1 <- rbind(df_urban_w1, fixef(mod3_prai_u, pars = c("Urban"))[1,])
df_QuintMat_w1 <- rbind(df_QuintMat_w1, fixef(mod3_prai_u, pars = c("QuintMat"))[1,])
df_QuintSoc_w1 <- rbind(df_QuintSoc_w1, fixef(mod3_prai_u, pars = c("QuintSoc"))[1,])
df_age_w1 <- rbind(df_age_w1, fixef(mod3_prai_u, pars = c("age"))[1,])
df_Sex_w1 <- rbind(df_Sex_w1, fixef(mod3_prai_u, pars = c("Sex"))[1,])
df_Race_w1 <- rbind(df_Race_w1, fixef(mod3_prai_u, pars = c("Race"))[1,])

df_urban_glm_w1 <- rbind(df_urban_glm_w1, fixef(mod3_prai_glm_u, pars = c("Urban"))[1,])
df_QuintMat_glm_w1 <- rbind(df_QuintMat_glm_w1, fixef(mod3_prai_glm_u, pars = c("QuintMat"))[1,])
df_QuintSoc_glm_w1 <- rbind(df_QuintSoc_glm_w1, fixef(mod3_prai_glm_u, pars = c("QuintSoc"))[1,])
df_age_glm_w1 <- rbind(df_age_glm_w1, fixef(mod3_prai_glm_u, pars = c("age"))[1,])
df_Sex_glm_w1 <- rbind(df_Sex_glm_w1, fixef(mod3_prai_glm_u, pars = c("Sex"))[1,])
df_Race_glm_w1 <- rbind(df_Race_glm_w1, fixef(mod3_prai_glm_u, pars = c("Race"))[1,])
dim(df_urban_w1)

df9s <- rbind(df_urban_w1, df_urban_glm_w1, df_QuintMat_w1, df_QuintMat_glm_w1, df_QuintSoc_w1, df_QuintSoc_glm_w1)
df9s <- data.frame(df9s)
df9s$wave <- rep(c('pre-delta', 'delta', 'omicron'), 18)
df9s$wave <- factor(df9s$wave, levels=c('pre-delta', 'delta', 'omicron'))
df9s$region <- rep(c(rep('ON',3), rep('BC',3), rep('Prairies',3)),6)
df9s$model <- rep(c(rep('MLM spatial',9), rep('GLM',9)),3)
df9s$cov <- c(rep('Urban', 18), rep('QuintMat', 18), rep('QuintSoc', 18))

df10s <- rbind(df_age_w1, df_age_glm_w1, df_Sex_w1, df_Sex_glm_w1, df_Race_w1, df_Race_glm_w1)
df10s <- data.frame(df10s)
df10s$wave <- rep(c('pre-delta', 'delta', 'omicron'), 18)
df10s$wave <- factor(df10s$wave, levels=c('pre-delta', 'delta', 'omicron'))
df10s$region <- rep(c(rep('ON',3), rep('BC',3), rep('Prairies',3)),6)
df10s$model <- rep(c(rep('MLM spatial',9), rep('GLM',9)),3)
df10s$cov <- c(rep('Age', 18), rep('Sex', 18), rep('Race', 18))

df9s$covariate <- "Neighborhood covariate"
df10s$covariate <- "Individual covariate"
df12s <- rbind(df9s, df10s)

df9s[df9s$cov == 'QuintSoc',]$cov <- 'Social deprivation quintile'
df9s[df9s$cov == 'QuintMat',]$cov <- 'Material deprivation quintile'

df12s[df12s$model == 'glm',]$model <- 'GLM'
df12s[df12s$model == 'spatial',]$model <- 'MLM spatial'

df12s$cov <- factor(df12s$cov, levels = c('Age', "Race", "Sex",
                                        "Urban","Material deprivation quintile", "Social deprivation quintile" ))

fig4s <-
  ggplot(df12s, aes(x=wave, y=Estimate, colour=region, shape=model)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.15, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  scale_color_manual(values = c("#7CAE00","#00BFC4","#C77CFF")) +
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  #  ylab("Coefficient Estimate of Areal Level Covariates Under different Models") +
  labs(y = "") +
  theme_bw() +
  #  theme(legend.position = "none") +
  facet_wrap( ~ cov , nrow = 3, scales="free", dir = "v") +
  #  facet_grid(vars(cov), vars(covariate)) +
  #  facet_grid(cols  = vars(cov),rows  = vars(covariate),scales = "free")
  labs(subtitle = 'Covariates Coefficient Estimate (unique donor within each wave)')


data_all <- data_all %>% mutate(year_month = format(data_all$sampledate, "%Y-%m"))
table(data_all$year_month)


# 2021-03 2021-04 2021-05 2021-06 2021-07 2021-08 2021-09 2021-10
#Export tableone to csv
#xyz= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1,addOverall = T), showAllLevels = T,format="fp"))
x= as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-03'),addOverall = T), showAllLevels = T,format="fp"))
x= rbind(x,as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-04'),addOverall = T), showAllLevels = T,format="fp")))
x= rbind(x,as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-05'),addOverall = T), showAllLevels = T,format="fp")))
x= rbind(x,as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-06'),addOverall = T), showAllLevels = T,format="fp")))

y= as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-07'),addOverall = T), showAllLevels = T,format="fp"))
y= rbind(y,as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-08'),addOverall = T), showAllLevels = T,format="fp")))
y= rbind(y,as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-09'),addOverall = T), showAllLevels = T,format="fp")))
y= rbind(y,as.data.frame(print(CreateTableOne(vars = 'age_group', strata = "region" , data = data1%>%filter(year_month=='2021-10'),addOverall = T), showAllLevels = T,format="fp")))


tab_csv <- print(x, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_before_monthly.csv")
tab_csv <- print(y, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_after_monthly.csv")
dt <- read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_after.csv")
#dt[1, 1] <- cell_spec(dt[1, 1], "latex", underline = T)
#dt[2, 2] <- cell_spec(dt[2, 1], "latex", underline = T)
#dt[1,2] <- paste0("\\textcolor{red}{", dt[2,2], "}")
#dt[2,2] <- paste0("\\textbf{", dt[2,2], "}")
#Run this
kbl(dt[2:7], booktabs = T) %>%
  #kable_paper(bootstrap_options = "striped", full_width = F) %>%
  kable_classic_2(full_width = F)  %>%
  row_spec(0, bold = T) %>%
  pack_rows("Sex (%)", 2, 3) %>%
  pack_rows("Age Group (%)", 4, 10) %>%
  pack_rows("Race (%)", 11, 12) %>%
  pack_rows("Social Deprivation Quintile (%)", 13, 17) %>%
  pack_rows("Material Deprivation Quintile (%)", 18, 22) %>%
  pack_rows("Urban (%)", 23, 24)

tab_csv <- print(x, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_before_monthly.csv")
tab_csv <- print(y, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_after_monthly.csv")
dt <- read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_after_monthly.csv")
#dt[1, 1] <- cell_spec(dt[1, 1], "latex", underline = T)
#dt[2, 2] <- cell_spec(dt[2, 1], "latex", underline = T)
#dt[1,2] <- paste0("\\textcolor{red}{", dt[2,2], "}")
#dt[2,2] <- paste0("\\textbf{", dt[2,2], "}")
#Run this
kbl(dt[1:7], booktabs = T) %>%
  #kable_paper(bootstrap_options = "striped", full_width = F) %>%
  kable_classic_2(full_width = F)  %>%
  row_spec(0, bold = T) %>%
  pack_rows("2021-Jul", 1, 8) %>%
  pack_rows("2021-Aug", 9, 16) %>%
  pack_rows("2021-Sep", 17, 24) %>%
  pack_rows("2021-Oct", 25, 32)

#### 
library(dplyr)
df12$cohort <- "Full cohort with repeated donations"
df12s$cohort <- "Unique donations within each wave"

df12s <- df12s[,!(names(df12s) %in% c('covariate'))]

df14 <- rbind(df12 %>% filter(region != 'Atlantic' & model == 'MLM spatial'),
              df12s %>% filter(region != 'Atlantic' & model == 'MLM spatial'))
library(ggplot2)
fig14 <-
  ggplot(df14, aes(x=wave, y=Estimate, colour=region, shape=cohort)) + 
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=.15, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd) +
  scale_color_manual(values = c("#7CAE00","#00BFC4","#C77CFF")) +
  geom_hline(yintercept=0, linetype="longdash", color = "firebrick") +
  #  ylab("Sensitivity Analysis on Coefficient Estimate of Areal Level Covariates Under different Models") +
  labs(y = "") +
  theme_bw() +
  #  theme(legend.position = "none") +
  facet_wrap( ~ cov , nrow = 3, scales="free", dir = "v") +
  #  facet_grid(vars(cov), vars(covariate)) +
  #  facet_grid(cols  = vars(cov),rows  = vars(covariate),scales = "free")
  labs(subtitle = 'Sensitivity Analysis of Covariates Coefficient Estimate on Different Cohort (Donation/Donor Level) ')


repeat_donors <- repeat_donors %>% mutate(age = 2022 - dob, age_group = age_groups_fun(age))
data_demog <- data_demog %>% mutate(age = 2022 - dob, age_group = age_groups_fun(age))

(repeat_donors %>%
  group_by(age_group) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)))$n /(data_demog %>%
       group_by(age_group) %>%
       summarise(n = n()) %>%
       mutate(freq = n / sum(n)))$n

