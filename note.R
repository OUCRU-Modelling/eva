library(tidyverse)
library(lubridate)
library(janitor)
Sys.setlocale("LC_TIME", "English")


df <- readRDS("D:/OUCRU/vac_coverage/data/vaxreg_hcmc_measles.rds")

## https://www.vietnamplus.vn/thanh-pho-ho-chi-minh-het-vaccine-trong-chuong-trinh-tiem-chung-mo-rong-post909241.vnp#google_vignette

df$lackd <- as.Date("2022-05-01")
df$ageuntil <- interval(df$dob, df$lackd) / years(1)

## subset children aged from 9 months to 9 months 2 weeks at 1 May 2022
slec <- df[df$ageuntil >= 0.75 & df$ageuntil <= 0.75 + 0.5*1/12,]

slec$agevac <- interval(slec$dob,slec$date_m1) / years(1)

## timely vaccination within two months  
slec$vac_on_date <- ifelse(slec$agevac < slec$ageuntil + 2/12,1,0)
slec$vac_on_date1 <- ifelse(slec$agevac < slec$ageuntil + 1/12,1,0)
## fill na with 0
slec$vac_on_date <- replace(slec$vac_on_date, is.na(slec$vac_on_date),0)
slec$vac_on_date1 <- replace(slec$vac_on_date, is.na(slec$vac_on_date1),0)

outcome1 <- slec %>% group_by(district) %>% count(vac_on_date) %>% 
  pivot_wider(names_from = vac_on_date,values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  magrittr::set_colnames(c("district","late","ondate")) %>% 
  mutate(coverage = ondate/(ondate+late)) %>% 
  as.data.frame()  

outcome2 <- slec %>% group_by(district) %>% count(vac_on_date1) %>% 
  pivot_wider(names_from = vac_on_date1,values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  magrittr::set_colnames(c("district","late","ondate")) %>% 
  mutate(coverage = ondate/(ondate+late)) %>% 
  as.data.frame()  
  
ggplot(data = outcome1, 
       aes(x = district,y = coverage)) +
  geom_bar(stat = "identity")+
  labs(x = "District",y = "Timely vaccination percentage (%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,size = 8))

## one month 
ggplot(data = outcome2, 
       aes(x = district,y = coverage)) +
  geom_bar(stat = "identity")+
  labs(x = "District",y = "Timely vaccination percentage (%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,size = 8))

### vaccine coverage 9m - 5y

df$is_2d <- ifelse(df$is_m1 == 1 & df$is_m2 == 1,1,0)

dfcal <- df[,c("dob","is_2d")]

date <- seq(as.Date("2022-09-01"),as.Date("2024-11-20"),by = "month")

cov_df <- data.frame()

for (i in 1:length(date)){
  dfcal$trackd <- date[i]
  dfcal$ageuntil <- interval(dfcal$dob, dfcal$trackd) / years(1)
  dfcal <- dfcal[dfcal$ageuntil >= 0.75 & dfcal$ageuntil <= 5,]
  ddd <- dfcal %>% group_by(is_2d) %>% 
    count() 
  ou <- data.frame(date = date[i],
                   coverage = as.numeric(ddd[2,2]/(ddd[1,2]+ddd[2,2])))
  cov_df <- rbind(cov_df,ou)
}

main <- ggplot(data = cov_df,aes(x = date,y = coverage*100))+
  geom_line()+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,by = 10)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "2 month",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  theme_bw()


sub <-  ggplot(data = cov_df,aes(x = date,y = coverage*100))+
  geom_line()+
  # scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,by = 10)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "4 month",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  theme_bw()


final1 <- main + annotation_custom(ggplotGrob(sub),
                         xmin=as.Date("2023-07-01"), 
                         xmax=as.Date("2024-11-01"), 
                         ymin=5, ymax=80)

final1 + ggtitle("Vaccine coverage of children (5m-5y) who get vaccinated 2 doses from Sep 2022-Nov 2024")

## by districts

district <- unique(df$district)

dfcald <- df[,c("dob","district","is_2d")]

cov_df2 <- data.frame()

for (k in 1:length(district)){
  dt <- subset(dfcald, district == district[k]) 
  for (i in 1:length(date)){
    dt$trackd <- date[i]
    dt$ageuntil <- interval(dt$dob, dt$trackd) / years(1)
    dt <- dt[dt$ageuntil >= 0.75 & dt$ageuntil <= 5,]
    ddd <- dt %>% group_by(is_2d) %>% 
      count() 
    ou <- data.frame(district = as.character(district[k]),
                     date = date[i],
                     coverage = as.numeric(ddd[2,2]/(ddd[1,2]+ddd[2,2])))
    cov_df2 <- rbind(cov_df2,ou)
  }
}


final2 <- ggplot(data = cov_df2,aes(x = date,y = coverage*100))+
  geom_line()+
  scale_y_continuous(limits = c(85, 100), breaks = seq(85,100,by = 5)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "2 months",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  facet_wrap(vars(district),ncol = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,size = 8,
                                    hjust=1))

final2 + 
  ggtitle("Vaccine coverage by districts of children (5m-5y) who get vaccinated two doses from Sep 2022-Nov 2024")

## timely vaccination time series

week <- seq(as.Date("2022-05-01"),as.Date("2024-07-01"),by = "2 week")

dttlv <- df[,c("dob","district","date_m1","date_m2")]

out_timely <- data.frame()

for (k in 1:length(district)){
  td <- subset(dttlv, district == district[k]) 
  for (i in 1:length(week)){
    td$lackd <- week[i]
    td$ageuntil <- interval(td$dob, td$lackd) / years(1)
    
    ## subset children aged from 9 months to 9 months 2 weeks at chosen time
    slec <- td[td$ageuntil >= 0.75 & td$ageuntil <= 0.75 + 0.5*1/12,]
    
    slec$agevac <- interval(slec$dob,slec$date_m1) / years(1)
    slec$vac_on_date1 <- ifelse(slec$agevac < slec$ageuntil + 1/12,1,0)
    slec$vac_on_date1 <- replace(slec$vac_on_date, is.na(slec$vac_on_date1),0)
    
    re <- slec %>% group_by(vac_on_date1) %>% count()  
    cus <- data.frame(district = district[k],
                      date = week[i],
                      per = as.numeric(re[2,2])/(as.numeric(re[2,2])+as.numeric(re[1,2])))
    out_timely <- rbind(out_timely,cus)
  }
}

head(out_timely)

ggplot(data = out_timely,aes(x = date,y = per*100))+
  geom_line()+
  # scale_y_continuous(limits = c(50, 100), breaks = seq(50,100,by = 10)) +
  labs(x = "Date",y = "Timely vaccination percentage (%)")+
  scale_x_date(breaks = "2 months",
               date_labels= "%b %Y")+
  facet_wrap(vars(district),ncol = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,size = 8,
                                   hjust=1))

## compare with demographic data

meandf <- out_timely %>% group_by(district) %>% 
  summarise(mean = mean(per)) 

sorted <- meandf[order(-meandf$mean),]  

s_dis <- sorted$district

sorted %>% as.data.frame()

sorted %>% 
  ggplot(aes(x = mean*100,y = dis))+
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0,100,10),limits = c(0,100))+
  labs(x = "Timely vaccination percentage",y = "District")

sorted %>% order(-sorted$mean)

###

# IPSUM
library(stringi)
df <- readRDS("D:/OUCRU/IPSUM/ipumsi_hcmc_2019.rds")

df$qh <- case_when(
  df$GEO2_VN == 704079760 ~ "Quận 1",
  df$GEO2_VN == 704079761 ~ "Quận 12",
  df$GEO2_VN == 704079762 ~ "Thủ Đức",
  df$GEO2_VN == 704079763 ~ "Thủ Đức",
  df$GEO2_VN == 704079764 ~ "Gò Vấp",
  df$GEO2_VN == 704079765 ~ "Bình Thạnh",
  df$GEO2_VN == 704079766 ~ "Tân Bình",
  df$GEO2_VN == 704079767 ~ "Tân Phú",
  df$GEO2_VN == 704079768 ~ "Phú Nhuận",
  df$GEO2_VN == 704079769 ~ "Thủ Đức",
  df$GEO2_VN == 704079770 ~ "Quận 3",
  df$GEO2_VN == 704079771 ~ "Quận 10",
  df$GEO2_VN == 704079772 ~ "Quận 11",
  df$GEO2_VN == 704079773 ~ "Quận 4",
  df$GEO2_VN == 704079774 ~ "Quận 5",
  df$GEO2_VN == 704079775 ~ "Quận 6",
  df$GEO2_VN == 704079776 ~ "Quận 8",
  df$GEO2_VN == 704079777 ~ "Bình Tân", 
  df$GEO2_VN == 704079778 ~ "Quận 7",
  df$GEO2_VN == 704079783 ~ "Củ Chi",
  df$GEO2_VN == 704079784 ~ "Hóc Môn",
  df$GEO2_VN == 704079785 ~ "Bình Chánh",
  df$GEO2_VN == 704079786 ~ "Nhà Bè",
  df$GEO2_VN == 704079787 ~ "Cần Giờ") %>% 
  stri_trans_general("latin-ascii") %>%
  str_remove_all("Quan") %>% 
  trimws(which = "both")


df$agr = df$AGE2 %>% as.factor()

levels(df$agr) = c("0-4", "5-9", "10-14","15-19",
                   "20-24","25-29","30-34","35-39",
                   "40-44","45-49","50-54","55-59",
                   "60-64","65-69","70-74","75-79",
                   "80-84","85+")

df$sex <- ifelse(df$SEX == 1,"male","female")


agepyr <- df[,c("agr","sex","qh")] %>% group_by(qh,agr,sex) %>% count()


agepyr <- agepyr %>% mutate(
  population2 = case_when(
    sex == "female" ~ -n,
    TRUE ~ n
  )) %>% as.data.frame()


pop_range <- range(agepyr$population2 %>% na.omit())
pop_range_breaks1 <- pretty(pop_range, n = 10)

agepyr %>% 
  ggplot() +
  geom_col(aes(x = population2,
               y = agr,
               fill = sex)) +
  # geom_col(aes(x = population2.y,
  #              y = agr,
  #              fill = sex),alpha=0.5) +
  scale_x_continuous(breaks  = pop_range_breaks1,
                     labels = abs(pop_range_breaks1))+
  facet_wrap(vars(qh),
             # scales = "free",
             ncol = 5)+
  theme_light()+
  labs(x = "Population",y = "Age group")+
  theme(axis.text.x = element_text(size = 4.5),
        axis.text.y = element_text(size = 4))




