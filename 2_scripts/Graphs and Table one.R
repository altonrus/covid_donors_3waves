# Generating Table 1; Supplement Table S1-S3, Figure S9 - S12
library(DBI)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(tableone)
theme_set(theme_bw())

#Functions------
province_fun <- function(var) {
  fsa_f = as.character(substr(var,1,1))
  prov =  ifelse(fsa_f == "A", "NL",
                 ifelse(fsa_f == "B", "NS",
                        ifelse(fsa_f == "C", "PE",
                               ifelse(fsa_f == "E", "NB",
                                      ifelse(fsa_f == "G" | fsa_f == "H" | fsa_f == "J", "QC",
                                             ifelse(fsa_f == "K" | fsa_f == "L" | fsa_f == "M" | fsa_f == "N" | fsa_f == "P", "ON",
                                                    ifelse(fsa_f == "R", "MB",
                                                           ifelse(fsa_f == "S", "SK",
                                                                  ifelse(fsa_f == "T", "AB",
                                                                         ifelse(fsa_f == "V", "BC",
                                                                                ifelse(fsa_f == "X", "NU/NT",
                                                                                       ifelse(fsa_f == "Y", "YT", NA
                                                                                       ))))))))))))
  
  return(prov)}

get_cduid <- function(x) {
  sample(fsa_to_cd_weights$cduid[[which(fsa_to_cd_weights == x)]], 1, replace = TRUE,
         fsa_to_cd_weights$cd_weights[[which(fsa_to_cd_weights == x)]])
}


#CBS Data Connection and Data organization---- 
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'cbs0',
  host = '132.216.183.71'
)

data_init <- dbGetQuery(con, "SELECT * FROM students.copy_cbs_combined") 
dbDisconnect(con)

#Import the table assigning the cduid to the fsa
match = read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/data6_map.csv")%>%mutate(
  sampledate = as.Date(sampledate)
)
data = match%>%left_join(data_init, by = c("pid", "sampledate", "fsa"))
data$wave <- factor(case_when(data$sampledate >= '2020-12-14' & data$sampledate < '2021-08-01'~"pre-delta",
                              data$sampledate >= '2021-08-01' & data$sampledate < '2021-12-15'~"delta", 
                              data$sampledate >= '2021-12-01' & data$sampledate < '2022-12-01'~"omicron",
                              TRUE~NA), levels=c("pre-delta", "delta", "omicron"))
data$fsa_f = as.character(substr(data$fsa,1,1))
data$province = province_fun(data$fsa)
data$region = factor(case_when(data$fsa_f %in% c("A", "B", "C", "E")~"Atlantic", 
                               data$fsa_f %in% c("K", "L", "M", "N", "P")~"Ontario",
                               data$fsa_f %in% c("R", "S", "T")~"Prairies",
                               data$fsa_f %in% c("V")~"BC",
                               TRUE~NA), levels = c("Atlantic","BC",  "Ontario", "Prairies"))
data$age = 2022-data$dob
data$age_group = cut(data$age, 
                     breaks = c(15,25,35,45,55,
                                65,75,Inf),
                     labels = c('16-24',
                                '25-34','35-44','45-54',
                                '55-64','65-74','75+'),
                     right = FALSE)
data$sex= factor(case_when(data$sex=="F"~"Female", 
                           data$sex=="M"~"Male"), levels=c("Female", "Male"))
data$anti_n =  factor(case_when(data$cur_result_n %in% c("Abbott Negative", "Roche Negative")~"negative", 
                                data$cur_result_n %in% c("Abbott Positive", "Roche Positive")~"positive",
                                TRUE~NA), levels=c("positive", "negative"))

data$anti_n =  factor(case_when(data$cur_result_n %in% c("Roche Negative")~"negative", 
                                data$cur_result_n %in% c("Roche Positive")~"positive",
                                TRUE~NA), levels=c("positive", "negative"))

data$ethnic = factor(case_when(data$ethnic1 %in% c("1 White")~"white", 
                               data$ethnic1 %in% c("2 Aborigin", "2 Aboriginal", "3 Asian", "4 Other", "4 Others")~"minority", 
                               TRUE~NA), levels = c("minority", "white"))
data$urban = factor(case_when(substr(data$fsa,2,2)==0~"Rural", 
                              substr(data$fsa,2,2)%in%c(1:9)~"Urban", 
                              TRUE~NA))
data$quintmat=factor(data$quintmat)
data$quintsoc=factor(data$quintsoc)

#Census Division ----

population_fsa <- read.csv("/home/yuanyu/projects/covid-donor-v-survey-antibody/1_data/T1201EN.CSV")

population_fsa <- population_fsa[!is.na(population_fsa$Population..2016),]

#filter out the provinces with few blood samples available
population_fsa$Province = province_fun(population_fsa$Geographic.code)
population_fsa <- population_fsa %>% 
  filter(Province != ("YT") & Province !=("NU/NT") & Province != ("QC"))

#Get population info at census division level
population_cd <- read.csv(file = 'data/98-401-X2016060_English_CSV_data.csv')
dim(population_cd)
population_cd <- population_cd[c("GEO_CODE..POR.", "DIM..Profile.of.Census.Divisions..2247.", "Dim..Sex..3...Member.ID...1...Total...Sex")]
colnames(population_cd) <- c("cduid", "type", "population_2016")
population_cd <- population_cd[population_cd$type == "Population, 2016", ]



#create one to one map from pid to cduid

column_widths <- c(6,3,2,4,7,70,3,3,3,1,7,2,4,5,4,1,8,3,1,11,13,1,1,30,1,1,8,8,1,3,1,1)

pccf_fwf <- read_fwf("data/pccf_fccp_V2209_2021.txt", col_positions = fwf_widths(column_widths))

#names follow order of pccf reference guide (pccf_202011-eng.pdf); some names include additional "_" ; "dauid" renamed to "prcdda" to match cimd nomenclature
column_names <- c("postal_code", "fsa", "pr", "cduid", "csduid", "csdname", "csdtype", "ccscode", "sac", "sactype", "ctname","er", "dpl", "fed13iud", "pop_cntr_ra", "pop_cntr_ra_type", "prcdda", "dissemination_block", "rep_pt_type", "lat", "long", "sli","pctype", "comm_name", "dmt", "h_dmt", "birth_date", "ret_date", "po", "qi", "source", "pop_centr_ra_size_class")

names(pccf_fwf) = column_names


fsa_to_cd <- pccf_fwf[c("fsa", "cduid", "pr")] %>% distinct() #2200

dim(fsa_to_cd)
dim(fsa_to_cd[duplicated(fsa_to_cd$fsa),])

head(fsa_to_cd[duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE),])

fsa_to_cd_dup <- fsa_to_cd[duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE),]

fsa_to_cd_uniq <- fsa_to_cd[!(duplicated(fsa_to_cd$fsa) | duplicated(fsa_to_cd$fsa, fromLast = TRUE)),]

population_all <- merge(fsa_to_cd, population_fsa[ ,c('Geographic.code', 'Population..2016')], by.x='fsa', by.y='Geographic.code', all.x = TRUE)
population_all <- merge(population_all, population_cd[ ,c('cduid', "population_2016")], by='cduid', all.x = TRUE)
population_all$population_2016 <- as.integer(population_all$population_2016)


#Data Pre-processing ----

data_dup <- merge(data, unique(fsa_to_cd_dup['fsa']), by ='fsa')
data_uniq <- merge(data, unique(fsa_to_cd_uniq['fsa']), by ='fsa')
dim(distinct(data_dup[c('fsa', 'pid')]))
length(unique(data_dup$fsa)) #210
length(unique(distinct(data_dup[c('fsa', 'pid')])$fsa)) #210
data_dup_cd <- distinct(data_dup[c('fsa', 'pid')])

#get pid with more than one fsa
dim(data_dup_cd[duplicated(data_dup_cd$pid) | duplicated(data_dup_cd$pid, fromLast = TRUE), ]) #682

dim(data_dup_cd[!(duplicated(data_dup_cd$pid) | duplicated(data_dup_cd$pid, fromLast = TRUE)),]) #61156

data_dup_cd1 <- data_dup_cd[!(duplicated(data_dup_cd$pid) | duplicated(data_dup_cd$pid, fromLast = TRUE)),]
#head(data_dup_cd1[order(data_dup_cd1$pid),])

data_dup_cd2 <- data_dup_cd[duplicated(data_dup_cd$pid) | duplicated(data_dup_cd$pid, fromLast = TRUE), ]

#
head(population_all[order(population_all$fsa),])

(population_all %>% 
    group_by(fsa) %>% 
    dplyr::summarize(values = list(cduid)))$values[[2]]

(population_all %>% 
    group_by(fsa) %>% 
    dplyr::summarize(cduid = list(cduid), cd_pop = list(population_2016)))$cd_pop[[2]]

fsa_to_cd_weights <-
  population_all %>% 
  group_by(fsa) %>% 
  dplyr::summarize(cduid = list(cduid), cd_weights = list(population_2016/sum(population_2016)))

# get the cduid from fsa according to the census division population

#fsa_to_cd_weights$cduid[[which(fsa_to_cd_weights == 'A0B')]]

#data_dup_cd$cduid <- sample(fsa_to_cd_weights$cduid[[which(fsa_to_cd_weights == 'A0B')]], 1, replace = TRUE,
#                             fsa_to_cd_weights$cd_weights[[which(fsa_to_cd_weights == 'A0B')]])


data_dup_cd <- mutate(data_dup_cd, cduid = get_cduid(fsa))
data_dup_cd$cduid <- apply(data_dup_cd['fsa'], 2, get_cduid)

a = NULL
n <- nrow(data_dup_cd)
for (i in 1:n) {
  a[i] = get_cduid(data_dup_cd[i, 'fsa'])
}
data_dup_cd$cduid_new <- a
data_dup_cd <- data_dup_cd[c('fsa', 'pid', 'cduid_new')]

#Join data_dup and data_dup_cd on (pid, fsa)
data_dup <- merge(data_dup, data_dup_cd, by=c('fsa', 'pid'), all.x=TRUE)
fsa_to_cd_uniq$cduid_new <- fsa_to_cd_uniq$cduid
data_uniq <- merge(data_uniq, fsa_to_cd_uniq, by='fsa', all.x=TRUE)
data_uniq <- data_uniq[, !(colnames(data_uniq) %in% c("cduid","pr"))]
#names(data_uniq)[names(data_uniq) == 'cduid.x'] <- 'cduid'
#names(data_uniq)[names(data_uniq) == 'pr.x'] <- 'pr'

data_uniq$cduid <- data_uniq$cduid_new
data_uniq <- within(data_uniq, rm(cduid.x, cduid.y, cduid_new))

data1 <- rbind(data_uniq, data_dup)
#data1$cduid <- data1$cduid_new
#data2 <- data1[, !(colnames(data1) %in% c("cduid_new"))]


data1 = data1%>%
  group_by(wave, region, cduid) %>% 
  filter(n()>=5)%>%ungroup()


match<- read.csv("data/pid_to_cd.csv")
data4 = left_join(data3, match, by =c("fsa", "pid"))

#Graphs-------
##Organize data into:
### Individual level data set without CI-----
ind_pp=data1%>%
  group_by(wave, region,age_group)%>%
  dplyr::summarize(type = "Age Group", pp = sum(anti_n =="positive")/n())%>%
  select(wave, region, type, "value"=age_group, pp)%>%
  rbind(
    data1%>%
      group_by(wave, region,sex)%>%
      dplyr::summarize(type = "Sex", pp = sum(anti_n =="positive")/n())%>%
      select(wave, region, type, "value"=sex, pp))%>%
  rbind(
    data1%>%
      group_by(wave, region,ethnic)%>%
      dplyr::summarize(type = "Ethnicity", pp = sum(anti_n =="positive")/n())%>%
      select(wave, region, type, "value"=ethnic, pp))%>%ungroup()
### Individual level data set with CI----
ind_pp=data1%>%
  group_by(wave, region,age_group)%>%
  dplyr::summarize(type = "Age Group", p = sum(anti_n =="positive"), num = n())%>%
  select(wave, region, type, "value"=age_group, p, num)%>%
  rbind(
    data1%>%
      group_by(wave, region,sex)%>%
      dplyr::summarize(type = "Sex", p = sum(anti_n =="positive"), num = n())%>%
      select(wave, region, type, "value"=sex, p, num))%>%
  rbind(
    data1%>%
      group_by(wave, region,ethnic)%>%
      dplyr::summarize(type = "Ethnicity", p = sum(anti_n =="positive"), num = n())%>%
      select(wave, region, type, "value"=ethnic, p, num))%>%ungroup()%>%
  rowwise() %>%
  dplyr::mutate(lb = prop.test(p, num, correct=F )$conf.int[1], 
         ub =  prop.test(p, num, correct=F )$conf.int[2], 
         pp = prop.test(p, num, correct=F )$estimate)
### Neighbourhood level with CI-----
nei_pp=data1%>%
  group_by(wave, region,urban)%>%
  dplyr::summarize(type = "Urban", p = sum(anti_n =="positive"), num =n())%>%
  select(wave, region, type, "value"=urban, p, num)%>%
  rbind(
    data1%>%
      group_by(wave, region,quintmat)%>%
      dplyr::summarize(type = "Material deprivation quintile",  p = sum(anti_n =="positive"), num =n())%>%
      select(wave, region, type, "value"=quintmat, p, num))%>%
  rbind(
    data1%>%
      group_by(wave, region,quintsoc)%>%
      dplyr::summarize(type = "Social deprivation quintile",  p = sum(anti_n =="positive"), num =n())%>%
      select(wave, region, type, "value"=quintsoc, p, num))%>%ungroup()%>%
  rowwise() %>%
  dplyr::mutate(lb = prop.test(p, num, correct=F )$conf.int[1], 
         ub =  prop.test(p, num, correct=F )$conf.int[2], 
         pp = prop.test(p, num, correct=F )$estimate)

##Plots without error bar----

#size 1000 by 600
ggplot(ind_pp, aes(x = value, y = pp,group = region, colour = region))+
  geom_point()+
  geom_line()+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Anti-N seropositivity by individual characteristics",x="", y = "")
ggplot(nei_pp, aes(x = value, y = pp,group = region, colour = region))+
  geom_point()+
  geom_line()+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Anti-N seropositivity by neighbourhood characteristics",x="", y = "")
pd <- position_dodge(0.15)
##Plots with error bar -----

#size 1000 by 600
fig3_ind <- 
ggplot(ind_pp, aes(x = value, y = pp,group = region, colour = region))+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0,position=pd)+
  geom_point(position=pd)+
  geom_line(position=pd)+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Anti-N seropositivity by individual characteristics",x="", y = "")
fig3_nei <- 
ggplot(nei_pp, aes(x = value, y = pp,group = region, colour = region))+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0,position=pd)+
  geom_point(position=pd)+
  geom_line(position=pd)+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Anti-N seropositivity by neighbourhood characteristics",x="", y = "")

ggsave("4_output/figs/sero_ind.png", fig3_ind, height = 12, width = 16, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/sero_nei.png", fig3_nei, height = 12, width = 16, dpi= 320, bg='#ffffff')

#Generate Table 1------ 

myVars= c("sex","age_group",  "ethnic","quintsoc", "quintmat", "urban")
x= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "pre-delta"),addOverall = T), showAllLevels = T,format="fp"))
y= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "delta"),addOverall = T), showAllLevels = T,format="fp"))
z= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "omicron"),addOverall = T), showAllLevels = T,format="fp"))

write.csv(x, "/home/yuanyu/projects/covid_donors_3waves/1_data/tableone_predelta.csv")
x = read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tableone_predelta.csv")

#investigate why the prairies have higher seroposotivity in rural compared to urban
data2 <- data1[!(data1$province %in% c('AB', 'MB')), ]
data2 <- data2[data1$covidvv == 1, ]
dim(data2)
data2 <- data1
data2$covidvv=factor(data2$covidvv)

nei_pp=data2%>%
  group_by(wave, region,urban)%>%
  dplyr::summarize(type = "Urban", p = sum(anti_n =="positive"), num =n())%>%
  select(wave, region, type, "value"=urban, p, num)%>%
  rbind(
    data2%>%
      group_by(wave, region,quintmat)%>%
      dplyr::summarize(type = "Material deprivation quintile",  p = sum(anti_n =="positive"), num =n())%>%
      select(wave, region, type, "value"=quintmat, p, num))%>%
  rbind(
    data2%>%
      group_by(wave, region,covidvv)%>%
      dplyr::summarize(type = "Vaccination",  p = sum(anti_n =="positive"), num =n())%>%
      select(wave, region, type, "value"=covidvv, p, num))%>%
  rbind(
    data2%>%
      group_by(wave, region,quintsoc)%>%
      dplyr::summarize(type = "Social deprivation quintile",  p = sum(anti_n =="positive"), num =n())%>%
      select(wave, region, type, "value"=quintsoc, p, num))%>%ungroup()%>%
  rowwise() %>%
  dplyr::mutate(lb = prop.test(p, num, correct=F )$conf.int[1], 
                ub =  prop.test(p, num, correct=F )$conf.int[2], 
                pp = prop.test(p, num, correct=F )$estimate)

fig3_nei_v <- 
ggplot(nei_pp, aes(x = value, y = pp,group = region, colour = region))+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0,position=pd)+
  geom_point(position=pd)+
  geom_line(position=pd)+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Anti-N seropositivity by neighbourhood characteristics and vaccination status",x="", y = "")


nei_pv=data2%>%
  group_by(wave, region,urban)%>%
  dplyr::summarize(type = "Urban", p = sum(covidvv ==1), num =n())%>%
  select(wave, region, type, "value"=urban, p, num)%>%
  rbind(
    data2%>%
      group_by(wave, region,quintmat)%>%
      dplyr::summarize(type = "Material deprivation quintile",  p = sum(covidvv ==1), num =n())%>%
      select(wave, region, type, "value"=quintmat, p, num))%>%
  # rbind(
  #   data2%>%
  #     group_by(wave, region,covidvv)%>%
  #     dplyr::summarize(type = "Vaccination",  p = sum(anti_n =="positive"), num =n())%>%
  #     select(wave, region, type, "value"=covidvv, p, num))%>%
  rbind(
    data2%>%
      group_by(wave, region,quintsoc)%>%
      dplyr::summarize(type = "Social deprivation quintile",  p = sum(covidvv ==1), num =n())%>%
      select(wave, region, type, "value"=quintsoc, p, num))%>%ungroup()%>%
  rowwise() %>%
  dplyr::mutate(lb = prop.test(p, num, correct=F )$conf.int[1], 
                ub =  prop.test(p, num, correct=F )$conf.int[2], 
                pp = prop.test(p, num, correct=F )$estimate)

fig3_nei_pv <- 
ggplot(nei_pv, aes(x = value, y = pp,group = region, colour = region))+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0,position=pd)+
  geom_point(position=pd)+
  geom_line(position=pd)+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Self-reported Vaccaination Proprotion Aggreagated by Neighborhood level characteristics and wave for each region", x="", y = "")+
  theme_bw()

ind_pv=data2%>%
  group_by(wave, region,age_group)%>%
  dplyr::summarize(type = "Age Group", p = sum(covidvv ==1), num = n())%>%
  select(wave, region, type, "value"=age_group, p, num)%>%
  rbind(
    data2%>%
      group_by(wave, region,sex)%>%
      dplyr::summarize(type = "Sex", p = sum(covidvv ==1), num = n())%>%
      select(wave, region, type, "value"=sex, p, num))%>%
  rbind(
    data2%>%
      group_by(wave, region,ethnic)%>%
      dplyr::summarize(type = "Ethnicity", p = sum(covidvv ==1), num = n())%>%
      select(wave, region, type, "value"=ethnic, p, num))%>%ungroup()%>%
  rowwise() %>%
  dplyr::mutate(lb = prop.test(p, num, correct=F )$conf.int[1], 
                ub =  prop.test(p, num, correct=F )$conf.int[2], 
                pp = prop.test(p, num, correct=F )$estimate)


fig3_ind_pv <- 
ggplot(ind_pv, aes(x = value, y = pp,group = region, colour = region))+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0,position=pd)+
  geom_point(position=pd)+
  geom_line(position=pd)+
  facet_grid(cols  = vars(type),rows  = vars(wave),scales = "free")+
  scale_y_continuous(labels=scales::percent)+
  labs(title = "Self-reported Vaccaination Proprotion Aggreagated by Individual level characteristics and wave for each region", x="", y = "")+
  theme_bw()
  
ggsave("4_output/figs/sero_nei_v.png", fig3_nei_v, height = 10, width = 16, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/vac_ind.png", fig3_ind_pv, height = 10, width = 16, dpi= 320, bg='#ffffff')
ggsave("4_output/figs/vac_nei.png", fig3_nei_pv, height = 10, width = 16, dpi= 320, bg='#ffffff')


xyz= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1,addOverall = T), showAllLevels = T,format="fp"))

library(kableExtra)
p <- print(CreateTableOne(vars = myVars, strata = "region" , data = data1, addOverall = T), showAllLevels = T,format="fp")
kbl(p, booktabs = TRUE, format = "latex")

#Check differnce between abbott-n, roche-n, cur_result_n
myVars1= c("interp_roche_n",  "cur_result_n")
print(CreateTableOne(vars = myVars1, strata = "region" , data = data1, addOverall = T), showAllLevels = T,format="fp")

data2 <- data1

data2$cur_result_n <- as.character(data2$cur_result_n) 
print(CreateTableOne(vars = myVars1, strata = "region" , data = data2%>%filter(wave == "pre-delta"), addOverall = T), showAllLevels = T,format="fp")
print(CreateTableOne(vars = myVars1, strata = "region" , data = data2%>%filter(wave == "delta"), addOverall = T), showAllLevels = T,format="fp")
print(CreateTableOne(vars = myVars1, strata = "region" , data = data2%>%filter(wave == "omicron"), addOverall = T), showAllLevels = T,format="fp")

#Abbott
c(16, 75, 187, 238)/
c(3075, 4880, 11249, 8256)

#Roche
c(10, 96, 242, 299)/
c(3085, 4867, 11227, 8213)

#cur_result_n (%)           Roche Negative 86703 (96.6)  10232 (99.6)  13665 (97.1)  37785 (96.7)  25021 (95.1)  <0.001     
#Roche Positive  3007 ( 3.4)     37 ( 0.4)    403 ( 2.9)   1284 ( 3.3)   1283 ( 4.9)     
install.packages("flextable")
library(tidyverse)
use_df_printer()

set_flextable_defaults(
  border.color = "#AAAAAA", font.family = "Arial",
  font.size = 10, padding = 2, line_spacing = 1.5
)
adsl <- select(data1, "sex","age_group",  "ethnic","quintsoc", "quintmat", "urban", "region")
View(adsl)
dat1 <- summarizor(adsl, by = "region")
dat1
ft <- as_flextable(dat1)
ft

xyz= CreateTableOne(vars = myVars, strata = "region" , data = data1,addOverall = T)

library(kableExtra)
tb <-  CreateTableOne(data = df)
print(xyz$ContTable)
print(tb$ContTable) %>%
  kbl() %>%
  kable_paper("hover", full_width = F)


tb <-  CreateTableOne(data = data1)
k <- print(tb$ContTable)


print(tb$ContTable) %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

kableone(xyz)

library(kableExtra)
dt <- mtcars[1:5, 1:4]

# HTML table
kbl(dt, caption = "Demo Table") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2[note]" = 2)) %>%
  footnote(c("table footnote"))

# LaTeX Table
kbl(xyz, booktabs = T, caption = "Demo Table") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2[note]" = 2)) %>%
  footnote(c("table footnote"))

#install.packages("readxl")
dt <- read_excel("/home/yuanyu/projects/covid_donors_3waves/1_data/Tableone.xlsx", sheet = 'Pre-delta')

dt <- read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_all.csv")

kbl(dt[2:7], booktabs = T) %>%
  kable_styling(latex_options = "striped") %>%
#  pack_rows("Total", 2, 3) %>%
  pack_rows("Sex (%)", 2, 3) %>%
  pack_rows("Age Group (%)", 4, 10) %>%
  pack_rows("Race (%)", 11, 12) %>%
  pack_rows("Social Deprivation Quintile (%)", 13, 17) %>%
  pack_rows("Material Deprivation Quintile (%)", 18, 22) %>%
  pack_rows("Urban (%)", 23, 24)
#  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 1)) %>%
#  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 1)) %>%
#  add_header_above(c(" ", "Group 6" = 5), bold = T, italic = T)


#Export tableone to csv
xyz= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1,addOverall = T), showAllLevels = T,format="fp"))
x= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "pre-delta"),addOverall = T), showAllLevels = T,format="fp"))
y= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "delta"),addOverall = T), showAllLevels = T,format="fp"))
z= as.data.frame(print(CreateTableOne(vars = myVars, strata = "region" , data = data1%>%filter(wave == "omicron"),addOverall = T), showAllLevels = T,format="fp"))

tab_csv <- print(z, printToggle = FALSE)
write.csv(tab_csv, file = "/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_omicron.csv")
dt <- read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_omicron.csv")
#dt[1, 1] <- cell_spec(dt[1, 1], "latex", underline = T)
#dt[2, 2] <- cell_spec(dt[2, 1], "latex", underline = T)
#dt[1,2] <- paste0("\\textcolor{red}{", dt[2,2], "}")
#dt[2,2] <- paste0("\\textbf{", dt[2,2], "}")
#Run this
kbl(dt[2:7], booktabs = T) %>%
  #kable_paper(bootstrap_options = "striped", full_width = F) %>%
  kable_classic_2(full_width = F)  %>%
  #kable_styling(latex_options = "striped") %>%
  #  pack_rows("Total", 2, 3) %>%
#  cell_spec(dt[1,1], "latex",bold = T) %>%
#  cell_spec(dt[1,2], "latex",bold = T) %>%
  row_spec(0, bold = T) %>%
#  pack_rows("Mean Seropositivity", 2, 2) %>%
  pack_rows("Sex (%)", 3, 4) %>%
  pack_rows("Age Group (%)", 5, 11) %>%
  pack_rows("Race (%)", 12, 13) %>%
  pack_rows("Social Deprivation Quintile (%)", 14, 18) %>%
  pack_rows("Material Deprivation Quintile (%)", 19, 23) %>%
  pack_rows("Urban (%)", 24, 25)

(data1%>%filter(wave == "pre-delta")%>%
  group_by(region)%>%
  dplyr::summarize(pp = sum(anti_n =="positive")/n()))$pp

(data1%>%filter(wave == "delta")%>%
       group_by(region)%>%
       dplyr::summarize(pp = sum(anti_n =="positive")/n()))$pp

(data1%>%filter(wave == "omicron")%>%
       group_by(region)%>%
       dplyr::summarize(pp = sum(anti_n =="positive")/n()))$pp

View(data1%>%filter(wave == "omicron")%>%
    group_by(region)%>%
    dplyr::summarize(pp = sum(anti_n =="positive")/n()))


(data1%>%filter(wave == "delta")%>%
    dplyr::summarize(pp = sum(anti_n =="positive")/n()))$pp


dt <- read.csv("/home/yuanyu/projects/covid_donors_3waves/1_data/tb1_predelta.csv")
#dt[1, 1] <- cell_spec(dt[1, 1], "latex", underline = T)
#dt[2, 2] <- cell_spec(dt[2, 1], "latex", underline = T)
#dt[1,2] <- paste0("\\textcolor{red}{", dt[2,2], "}")
#dt[2,2] <- paste0("\\textbf{", dt[2,2], "}")
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

View(data1%>%filter(wave == "delta")%>%
    group_by(region)%>%
    dplyr::summarize(pp = sum(anti_n =="positive")))


data_right = match%>%right_join(data_init, by = c("pid", "sampledate", "fsa"))


