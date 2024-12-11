invisible(Sys.setlocale("LC_TIME", "English"))

df_raw <- readRDS("./data/vaxreg_hcmc_measles.rds")
# data cleaning

## vaccination date < dob
df_raw %>% filter(date_m1 < dob & date_m2 < dob)

## same date of first and second dose
df_raw %>% filter(date_m1 == date_m2)

## first subset

df <- df_raw %>% na.omit() %>% 
                 subset(date_m1 > dob & 
                        date_m2 > dob & 
                        date_m1 < date_m2 &
                        year(date_m2) <= 2024 &
                        year(date_m1) >= min(year(df_raw$dob))) 

dirty <- df_raw %>% na.omit() %>% subset(!(date_m1 > dob & 
         date_m2 > dob & 
         date_m1 < date_m2 &
         year(date_m2) <= 2024 &
         year(date_m1) >= min(year(df_raw$dob)))) 


date_compute <- seq(as.Date("2022-09-01"),as.Date("2024-11-20"),by = "weeks")

out_fn <- data.frame()

for (i in 1:length(date_compute)){
  cov <- df %>% 
    mutate(
      age = decimal_date(date_compute[i]) - decimal_date(dob),
      is_m2 = if_else(date_m2 <= date_compute[i], 1, 0))  %>% 
    filter(age >= 1, age <= 5) %>% 
    summarise(m2 = sum(is_m2, na.rm = T), n = n(), cov = m2/n)
  
  cov_2w <- df %>% 
    mutate(
      age = decimal_date(date_compute[i]) - decimal_date(dob),
      is_m2 = if_else(date_m2 <= date_compute[i] %m-% weeks(2), 1, 0))  %>% 
    filter(age >= 1 + 0.5/12, age <= 5 + 0.5/12) %>% 
    summarise(m2 = sum(is_m2, na.rm = T), n = n(), cov = m2/n)
  
  out <- data.frame(date = as.Date(date_compute[i]),
                    cov = as.numeric(cov$cov),
                    cov_2w = as.numeric(cov_2w$cov))
  
  out_fn <- rbind(out_fn,out)
}

ggplot(data = out_fn)+
  geom_line(aes(x = date,y = cov*100))+
  geom_line(aes(x = date,y = cov_2w*100))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,by = 10)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "2 month",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  theme(axis.text.x = element_text(angle = 45,size = 8,
                                   hjust=1))+
  theme_bw()

## first dose
out_fn1 <- data.frame()

for (i in 1:length(date_compute)){

  cova <- df %>% 
    mutate(
      age = decimal_date(date_compute[i]) - decimal_date(dob),
      is_m1 = if_else(date_m1 <= date_compute[i], 1, 0))  %>% 
    filter(age >= 0.75, age <= 5) %>% 
    summarise(m1 = sum(is_m1, na.rm = T), n = n(), cov = m1/n)
  
  cov_2wa <- df %>% 
    mutate(
      age = decimal_date(date_compute[i]) - decimal_date(dob),
      is_m1 = if_else(date_m1 <= date_compute[i] %m-% weeks(2), 1, 0))  %>% 
    filter(age >= 0.75 + 0.5/12, age <= 5 + 0.5/12) %>% 
    summarise(m1 = sum(is_m1, na.rm = T), n = n(), cov = m1/n)
  
  outa <- data.frame(date = as.Date(date_compute[i]),
                    cov = as.numeric(cova$cov),
                    cov_2w = as.numeric(cov_2wa$cov))
  
  out_fn1 <- rbind(out_fn1,outa)
}


vac_co <- ggplot()+
  geom_line(data = out_fn,aes(x = date,y = cov*100,color="2"))+
  geom_line(data = out_fn1,aes(x = date,y = cov*100,color="1"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,by = 10)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "4 month",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  theme(axis.text.x = element_text(angle = 45,size = 8,
                                   hjust=1))+
  scale_color_manual(name="Dose", values=c("1"="red", "2"="blue"))+
  theme_bw()

vac_co2w <-ggplot()+
  geom_line(data = out_fn,aes(x = date,y = cov_2w*100,color="2"))+
  geom_line(data = out_fn1,aes(x = date,y = cov_2w*100,color="1"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,by = 10)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "4 month",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  theme(axis.text.x = element_text(angle = 45,size = 8,
                                   hjust=1))+
  scale_color_manual(name="Dose", values=c("1"="red", "2"="blue"))+
  theme_bw()

library(patchwork)
vac_co | vac_co2w

## vaccine coverage in each districts

dis <- unique(df$district) 

out_d <- data.frame()
out_d1 <- data.frame()

for (k in 1:length(dis)){
  dfd <- df %>% filter(district == dis[k])
  for (i in 1:length(date_compute)){
    cov <- dfd %>% 
      mutate(
        age = decimal_date(date_compute[i]) - decimal_date(dob),
        is_m2 = if_else(date_m2 <= date_compute[i], 1, 0))  %>% 
      filter(age >= 1, age <= 5) %>% 
      summarise(m2 = sum(is_m2, na.rm = T), n = n(), cov = m2/n)
    
    cov_2w <- dfd %>% 
      mutate(
        age = decimal_date(date_compute[i]) - decimal_date(dob),
        is_m2 = if_else(date_m2 <= date_compute[i] %m-% weeks(2), 1, 0))  %>% 
      filter(age >= 1 + 0.5/12, age <= 5 + 0.5/12) %>% 
      summarise(m2 = sum(is_m2, na.rm = T), n = n(), cov = m2/n)
    
    out_d1 <- data.frame(district = as.character(dis[k]),
                      date = as.Date(date_compute[i]),
                      cov = as.numeric(cov$cov),
                      cov_2w = as.numeric(cov_2w$cov))
    
    out_d <- rbind(out_d,out_d1)
  }  
}


meancv <- out_d %>% 
  group_by(district) %>% 
  summarise(mean = mean(cov)) 

sorted <- meancv[order(-meancv$mean),]

vac_cov <- out_d %>% mutate(dis = factor(district,
                              levels = as.character(sorted$district))) %>% 
ggplot()+
  geom_line(aes(x = date,y = cov*100,color="2"))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,by = 10)) +
  labs(x = "Date",y = "Vaccine coverage (%)")+
  scale_x_date(breaks = "4 months",
               date_labels= "%b %Y",
               limits = c(as.Date("2022-09-01"),as.Date("2024-11-20")))+
  facet_wrap(vars(dis),ncol = 1)+
  scale_color_manual(name="Dose", values=c("1"="red", "2"="blue"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,size = 8,hjust=1),
        axis.text.y = element_text(size = 6))

###

district <- unique(df$district)
week <- seq(as.Date("2022-05-01"),as.Date("2024-07-01"),by = "month")

dttlv <- df[,c("dob","district","date_m1","date_m2")] 

out_timely <- data.frame()

for (k in 1:length(district)){
  td <- subset(dttlv, district == district[k]) 
  for (i in 1:length(week)){
    td$lackd <- week[i]
    td$ageuntil <- decimal_date(td$lackd) - decimal_date(td$dob)
    
    ## subset children aged from 9 months to 9 months 2 weeks at chosen time
    slec <- td[td$ageuntil >= 0.75 & td$ageuntil <= 0.75 + 0.5*(1/12),]
    
    slec$agevac <- decimal_date(slec$date_m1) - decimal_date(slec$dob)
    slec$vac_on_date1 <- ifelse(slec$agevac < slec$ageuntil + 1/12,1,0)
    slec$vac_on_date1 <- replace(slec$vac_on_date, is.na(slec$vac_on_date1),0)
    
    re <- slec %>% group_by(vac_on_date1) %>% count()  
    
    if(nrow(re) == 1){
      re[2,] <- re[1,]
      re[1,1] <- 0  
      re[1,2] <- 0  
    } 
    
    cus <- data.frame(district = district[k],
                      date = week[i],
                      per = as.numeric(re[2,2])/(as.numeric(re[2,2])+as.numeric(re[1,2])))
    out_timely <- rbind(out_timely,cus)
  }
}







