# 
#  Fuantei LT for JapanR.2018
#                    
#                                                          2018.11.3 新規
# ----------------------------------------------------------------------------------

rm(list=ls())

# ---- load pakages ----
library("dplyr")
library("ggplot2")
library("data.table")
library("stringr")
library("tidyr")
library("rstan")
require("shinystan")

#### read data ####
# ---- rain read ----
df_r <- data_frame()
yy_set <- 1989:2018  # 30年分

for(ipnt in yy_set){
  df_r_tmp <- fread(paste0("../Data/IN/rain/rain_",ipnt,".csv"))
  df_r <- rbind(df_r,df_r_tmp)
}

# ---- sonde read ----
df_s <- data_frame()
yy_set <- c("89-98","99-08","09-18")  # 30年分

for(ipnt in yy_set){
  df_s_tmp <- fread(paste0("../Data/IN/sonde_",ipnt,".csv"))
  df_s <- rbind(df_s,df_s_tmp)
}

# ---- maxtemp read ----
df_mxt <- fread("../Data/IN/max_tmp.csv")  # 30年分

#---- tc read ----
df_tc <- data_frame()
yy_set <- 2001:2018  # 18年分

for(ipnt in yy_set){
  df_tc_tmp <- fread(paste0("../Data/IN/TC/table",ipnt,".csv"))
  df_tc <- rbind(df_tc,df_tc_tmp)
}

# ---- tuyu read ----
df_tuyu <- fread("../Data/IN/tuyu.csv")

#### make data set ####
# ---- make rain dataset (関東平野のアメダス32地点) ----
rain_pre <- df_r
colnames(rain_pre) <- c("amd_no","kana","yyyy","mm","dd","hh","rain","rain_rm")

rain_pre <- rain_pre %>%
  mutate(am_flg=if_else(hh %in% 1:12, 1, 0)) %>%
  mutate(pm_flg=if_else(hh %in% 13:24, 1, 0))
am_set <- rain_pre %>%
  filter(am_flg==1 & rain_rm %in% c(0,100)) %>%  # 正常値のみ集計
  group_by(yyyy,mm,dd) %>%
  summarise(am_max=max(rain))  # 全地点の最大値
pm_set <- rain_pre %>%
  filter(pm_flg==1 & rain_rm %in% c(0,100)) %>%  # 正常値のみ集計
  group_by(yyyy,mm,dd) %>%
  summarise(pm_max=max(rain))  # 全地点の最大値
rain_set <- am_set %>%
  full_join(pm_set,c("yyyy","mm","dd"))

# ---- make sonde dataset (KIの算出) ----
colnames(df_s) <- c("upp_no","yyyy","mm","dd","hh",
                    "lev","tem","tem_rm","hum","hum_rm",
                    "wdr","wdr_rm","wsp","wsp_rm")

t85 <- df_s %>%
  filter(hh==9) %>%
  filter(lev == 850) %>%
  filter(tem_rm == 8) %>%
  select(yyyy,mm,dd,hh,tem) %>%
  rename(t850=tem)

h85 <- df_s %>%
  filter(hh==9) %>%
  filter(lev == 850) %>%
  filter(hum_rm == 8) %>%
  select(hum,yyyy,mm,dd,hh) %>%
  rename(h850=hum) %>%
  mutate_at(vars(h850),as.numeric)

t70 <- df_s %>%
  filter(hh==9) %>%
  filter(lev == 700) %>%
  select(yyyy,mm,dd,hh,tem) %>%
  rename(t700=tem) 

h70 <- df_s %>%
  filter(hh==9) %>%
  filter(lev == 700) %>%
  select(hum,yyyy,mm,dd,hh) %>%
  rename(h700=hum) %>%
  mutate_at(vars(h700),as.numeric)

t50 <- df_s %>%
  filter(hh==9) %>%
  filter(lev == 500) %>%
  select(tem,yyyy,mm,dd,hh) %>%
  rename(t500=tem) 

# KIの算出
snd_pre <- t85 %>%
  left_join(h85, c("yyyy","mm","dd","hh")) %>%
  left_join(t70, c("yyyy","mm","dd","hh")) %>%
  left_join(h70, c("yyyy","mm","dd","hh")) %>%
  left_join(t50, c("yyyy","mm","dd","hh")) %>%
  select(-hh) %>%
  mutate(ews_85 = h850 * 0.01 * exp((17.502 * t850) / (240.9 + t850))) %>%
  mutate(ews_70 = h700 * 0.01 * exp((17.502 * t700) / (240.9 + t700))) %>%
  mutate(td850 = (240.9 * log(ews_85)) / (17.5 - log(ews_85))) %>%
  mutate(td700 = (240.9 * log(ews_70)) / (17.5 - log(ews_70))) %>%
  mutate(KI= t850 - t500 + td850 - (t700 - td700) )
  
KI_set <- snd_pre %>%
  select(yyyy,mm,dd,KI)

# ---- make max temp dataset ----
colnames(df_mxt) <-  c("sfc_no","yyyy","mm","dd","mx_tem","mx_tem_rm")

tmx_set <- df_mxt %>%
  filter(mx_tem_rm %in% c(0,100)) %>%  # 正常値のみ取得
  group_by(yyyy,mm,dd) %>%
  summarise(reg_max=max(mx_tem)) %>%  #３地点で最も高かった最高気温
  arrange(yyyy,mm,dd)

# --- make TC dataset ----
colnames(df_tc) <- c("yyyy","mm","dd","hh","TC_no","TC_name",
                     "rank","lat","lon","cen","max","dum1",
                     "dum2","dum3","dum4","dum5","dum6","dum7"
)
TC_set <- df_tc %>%
  select(yyyy,mm,dd,hh,lon,lat) %>% 
  mutate(day = as.Date(
    paste(!!!rlang::syms(c("yyyy", "mm", "dd")), sep="-"))) %>% # UTC
  filter(between(lon,130,150)) %>%
  filter(between(lat,30,40)) %>%
  distinct(yyyy,mm,dd) %>%
  mutate(TC=1)  # TCが関東甲信地方の近くにあったフラグ

# ---- make tuyu dataset ----
colnames(df_tuyu) <-  c("yyyy","iri","ake","dummy")
df_tuyu$yyyy <- gsub("年", "", df_tuyu$yyyy)
df_tuyu$iri <- gsub(" ", "", df_tuyu$iri)
df_tuyu$iri <- gsub("日", "", df_tuyu$iri)
df_tuyu$iri <- gsub("ごろ", "", df_tuyu$iri)
df_tuyu$ake <- gsub(" ", "", df_tuyu$ake)
df_tuyu$ake <- gsub("日", "", df_tuyu$ake)
df_tuyu$ake <- gsub("ごろ", "", df_tuyu$ake)

tuyu_set <- df_tuyu %>%
  separate(iri, c("iri_tuki","iri_niti"), sep="月") %>%
  separate(ake, c("ake_tuki","ake_niti"), sep="月") %>%
  filter(!is.na(ake_niti)) 

tuyu_set <- tuyu_set %>%
  mutate(iri_day = as.Date(paste(!!!rlang::syms(
    c("yyyy", "iri_tuki", "iri_niti")), sep="-"))) %>%
  mutate(ake_day = as.Date(paste(!!!rlang::syms(
    c("yyyy", "ake_tuki", "ake_niti")), sep="-"))) 

##### 晴れた暑い日を抽出 ####
# ---- データ結合 ----
pre_set <- tmx_set %>%
  left_join(KI_set,c("yyyy","mm","dd")) %>%
  left_join(rain_set,c("yyyy","mm","dd")) %>%
  left_join(TC_set,c("yyyy","mm","dd"))

# ---- 台風が近い日を省く ----
pre_set$TC[is.na(pre_set$TC)]<-0
data_set <- pre_set %>%
  filter(!is.na(KI)) %>%           # sonde観測の正常値のみ
  filter(TC!=1) %>%                # 台風が近い日を省く
  filter(between(yyyy,2001,2018))  # TCトラックは18年分のみ

data_set[is.na(data_set)]<-0       # NAは降水ゼロ

# ---- 梅雨期間を省く ----
data_set <- data_set %>% 
  mutate(day = as.Date(paste(!!!rlang::syms(c("yyyy", "mm", "dd")), sep="-")))

for (yy in tuyu_set$yyyy) {
  tuyu_st <- tuyu_set %>%
    filter(yyyy == yy) %>%
    select(iri_day) 
  tuyu_ed <- tuyu_set %>%
    filter(yyyy == yy) %>%
    select(ake_day) 
  data_set <- data_set %>%
    filter(!(between(day, tuyu_st, tuyu_ed)))
}

#---- AM降水なし、最高気温30度の日を抽出 ----
smp_set <- data_set %>%
  filter(am_max==0 & reg_max >= 30) %>%
  mutate(target=if_else(pm_max>=1,1,0)) %>%
  ungroup() %>%
  select(target,KI)

# check データ数、正負の割合
pos <- smp_set %>%
  filter(target==1)
nrow(pos) / nrow(smp_set)

# check box plot
imgdir <-"../Data/OUT/"

g <- ggplot(smp_set, aes(y=KI,as.factor(target))) +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15)) +
  ylim(0,39) +
   geom_boxplot()
g <- g + xlab("") 
plot(g)

ggsave(filename=paste0(imgdir,"box.png"),
       width=4,height=4,dpi=200)

#### modeling ####
# ---- まずは普通のロジスティック回帰 ----
fm = "target ~ KI"
result <- glm(formula(fm), data=smp_set, family=binomial)
summary(result)

coefwk <- coef(result)
d0 <- coefwk[1]; d1 <-coefwk[2]; 

logist <- function(x){
  y = exp(d0 + ( d1*x)) / (1 + exp(d0 + ( d1*x)))
}

g <-ggplot(smp_set, aes(KI, target)) + geom_point() +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15)) +
  xlim(10,40)

g <- g + stat_function(fun = logist) + xlab("K-index") +ylab("rain")
plot(g)

ggsave(filename=paste0(imgdir,"before.png"),
       width=5,height=4,dpi=200)

# ---- ロバストロジスティック回帰 ----
xName <- c("KI")
smp_set_mx <- as.matrix(smp_set)
XX <- as.matrix(smp_set_mx[,xName],ncol=length(xName))

datastan = list(
  x = XX,
  y = smp_set$target,
  dim_x = dim(XX)[2],
  n_total = dim(XX)[1]
)

stanmodel <- stan_model(file='../Model/RbstLgstc.stan')
fit <- sampling(stanmodel, data=datastan,
                chains = 2, iter = 6000,
                warmup = 1500,
                seed=1234)

# launch_shinystan(fit)  # 収束診断

a0<-rstan::extract(fit)$beta0%>%as.numeric() %>% mean
a1<-rstan::extract(fit)$beta%>%as.numeric() %>% mean
b <-rstan::extract(fit)$guess%>%as.numeric() %>% mean

logist <- function(x){
  y = b * 0.5 + (1 - b) * exp(a0 + ( a1*x)) / (1 + exp(a0 + ( a1*x)))
}

g <-ggplot(smp_set, aes(KI, target)) + geom_point() +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15)) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15)) +
  xlim(10,40) 

g <- g + stat_function(fun = logist) + xlab("K-index") +ylab("rain")
plot(g)

ggsave(filename=paste0(imgdir,"after.png"),
       width=5,height=4,dpi=200)

# ---- guessのヒストグラム ----
b <-rstan::extract(fit)$guess%>%as.numeric() 
res<-data.frame(sample=c(b),pars=rep(c("外れ値の確率"),each=length(b)))
Color=c("tomato")
g <- ggplot(res,aes(x=sample,y=..density..,colour=pars,fill=pars))+
  theme_gray (base_family = "HiraKakuPro-W3") +
  geom_histogram(position="identity",alpha=0.7)+
  geom_density(position="identity",alpha=0.4)+
  scale_color_manual(values=Color)+
  scale_fill_manual(values=Color) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15))+
  theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15)) +
  xlab("外れ値") +ylab("denstiy") +
  theme(legend.position = 'none')

plot(g)

ggsave(filename=paste0(imgdir,"guess.png"),
       width=5,height=4,dpi=200)






  
