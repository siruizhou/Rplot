# https://klein.uk/teaching/viz/datavis-pyramids/ 
# lollipop plot 

#install.packages('xlsx')
library(xlsx)
data <- read.xlsx("Documents/career/ADB-IT infrastructure/side proj/Gendered calculation.xlsx", 
                  sheetIndex = 3)

#install.packages('apyramid')
library(apyramid)

library(tidyr)
library(dplyr)
# pivot_longer(data=data[,1:3],
#              cols = c(Male.Count,female.count),#data[,2:3],#
#              names_to = 'Gender',
#              values_to= 'Counts')

colnames(data) = c('category','male','female',"count_all", "male.pct","female.pct",
                   "male_1male_count","female_1male_count","female.vs.male")

levels <- c("ADB Training Part 1"
                              ,"ADB Training Part 2"
                              ,"Alerts (Montors) Configured"
                              ,"All #Tags Applied"
                              ,"Application Performance Monitoring"
                              ,"Browser and API Tests Implemented"
                              ,"Dashboards Created"
                              ,"Doghouse Presentation"
                              , "Gravatar Created"
                              ,"Internal Team Training"
                              ,"Logging - Ingestion and Parsing"
                              ,"Real User Monitoring (RUM)"
                              ,"Written Examination"
                              ,"ADB Certified Practitioner"
                              ,"Total Champions")

data <- data%>% 
  mutate(category = factor(category,levels=levels))

g_count <- data[1:15,] %>% 
  pivot_longer(cols = c(male,female),
               names_to = 'gender',
               values_to = 'count') %>% 
  mutate(category = factor(category,levels=levels)) %>% 
  mutate(percent = case_when(
    gender == "male" ~ round(male.pct, 2),
    gender == "female" ~ round(female.pct, 2)
  ))

# %>%                 # ungroup so percents are not by group
#   mutate(percent = round(100*(count/sum(count, na.rm=T)), digits = 1), 
#          percent = case_when(
#            gender == "female" ~ percent,
#            gender == "male" ~ -percent,     # convert male to negative
#            TRUE          ~ NA_real_)) 



library(ggplot2)
apyramid::age_pyramid(
  data = g_count,
  age_group = "category",
  split_by = "gender",
  count = 'count',
  # proportional = TRUE,
  pal = c("#F8766D", "#00BFC4"),# can specify alt. colors here (but not labels)
  show_midpoint = FALSE,
) + 
  theme_minimal()  + 
  # geom_text(aes(label= paste0(g_count$count,'(',g_count$percent*100,'%)')),
  #           position = position_dodge(0.1),
  #           vjust=0.5,hjust=0.6) +
  geom_text(aes(label= paste0(100*g_count$percent,'%')),
            position = position_stack(0.5),
            vjust=0.5) +
  theme(#text = element_text(size=8),
    axis.text = element_text(size=15))


# Lollipop plot 
# value1 <- abs(rnorm(26))*2
# data.frame(
#   x=LETTERS[1:26], 
#   value1=value1, 
#   value2=value1+1+rnorm(26, sd=1) 
# )
ggplot(data[1:15,]) +
  geom_segment( aes(x=category, xend=category, y=male_1male_count, yend=female_1male_count), color="grey") +
  geom_point( aes(x=category, y=male_1male_count), color="#00BFC4", size=3 ) + #rgb(0.2,0.7,0.1,0.5)
  geom_point( aes(x=category, y=female_1male_count), color="#F8766D", size=3 ) + #rgb(0.7,0.2,0.1,0.5)
  coord_flip()+
  theme_minimal() +
  theme(
    legend.position = "none",
  ) +
  theme(axis.text = element_text(size=12)) +
  xlab("") +
  ylab("Percentage of champions passing the task within a gender group")
