library(tidyverse)
library(haven)

mydata <- read_dta("election_panel_2008_2016.dta")


mydata %>% filter(state == 1) %>% View()



results <- data.frame()

vars <- c("voteshare_dem", 
          "voteshare_gop", 
          "voteshare_oth", 
          "turnout",
          "pctic_0400",
          "unemp_rate_cty",
          "percent_in_poverty",
          "perc_black",
          "perc_native",
          "perc_asian",
          "perc_hisp",
          "median_income",
          "dem_campaign_spending_gap",
          "total_campaign_spending",
          "pop_den_100k")


for(var in vars) {
  tmp <- mydata %>% 
    mutate(applied = mydata[[var]]) %>% 
    filter(!is.na(pop_total)) %>% 
    filter(!is.na(applied)) %>% 
    group_by(year) %>% 
    summarise(mean = weighted.mean(applied, pop_total))
  y08 <- filter(tmp, year == 2008)$mean
  y12 <- filter(tmp, year == 2012)$mean
  y16 <- filter(tmp, year == 2016)$mean
  p0812 <- t.test(filter(mydata, year == 2008)[[var]], filter(mydata, year == 2012)[[var]])$p.value
  p0812 <- t.test(filter(mydata, year == 2012)[[var]], filter(mydata, year == 2016)[[var]])$p.value
  tmp <- data.frame(var, y08, y12, y16, y08-y12, y12-y16, p0812)
  results <- rbind(results, tmp)

  }

results  


t.test()




 

      
      
mydata  %>% filter(year == 2008 | year == 2012) %>% lm(median_income ~ year, data = .) %>% summary %>% coef  %>% .["(Intercept)", "Pr(>|t|)"]
                                         
                                                          