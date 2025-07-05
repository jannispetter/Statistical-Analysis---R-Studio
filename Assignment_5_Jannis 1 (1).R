#Assignment 5

#Question 5

d1 <- news

d2 <- subset(d1, section != "business") 

aov1<-aov(y~section*day,data=d2) 
summary(aov1) 

#Question 6

tukey1<-TukeyHSD(aov1,"section:day") 
tukey1

#Question 7

ggplot(d2, aes(x = section, y = y, fill = day)) +
  geom_boxplot() +
  labs(title = "Interaction Effect of Section and Day on Ad Inquiries",
       x = "Newspaper Section",
       y = "Number of Inquiries") +
  theme_minimal()


#Question 8


d3 <- product

aov2 <- aov(hedonic ~ platform, data = d3)  
summary(aov2)


tukey2<-TukeyHSD(aov2) 
tukey2 

#Question 9

d3_long <- d3 %>%
  pivot_longer(cols = c(hedonic, utilitarian),names_to = "Type",values_to = 
                 "Intention") 


#Question 10

d3_long$platform <- as.factor(d3_long$platform)  
d3_long$Type <- as.factor(d3_long$Type)  

aov4 <- aov_car(Intention ~ platform * Type+
                     Error(id/Type), data = d3_long)


aov4 <- aov_ez(id = "id",dv = "Intention", between = c("platform"), within = 
                 c("Type"), data = d3_long)  
  anova(aov4)

#post-hoc
  aov4 %>% lsmeans(.,~ platform|Type)%>% pairs(.,adjust="tukey")



#Question 11
  
d4 <- brew
  #Remove first row:
  d4<-d4[-1,]
  #Select certain observations:
  d4<-subset(d4, attention==1)
  #Convert character variable into numeric:
  d4$taste<-as.numeric(d4$taste)
  

  #Question 12
  
  d4$sample <- as.factor(d4$sample)  
  d4$brew <- as.factor(d4$brew)  
  
  aov_brew <- aov_ez(id = "id",dv = "taste",data = d4,within = "brew",between 
                      = "sample") 
  anova(aov_brew) 
  
  #post-hoc
 aov_brew %>% emmeans(.,~ sample|brew)%>% pairs(.,adjust="tukey")

#Question 13
 
 P<-aov_car(taste~brew+Error(id/brew), data=subset(d4, d4$sample=="professional"))
 P
 CUST<-aov_car(taste~brew+Error(id/brew), data=subset(d4, d4$sample=="customer"))
 CUST
 COM<-aov_car(taste~brew+Error(id/brew), data=subset(d4, d4$sample=="company"))
 COM
 
 #Question 14
 
 lm1 <- lm(y ~ section * day, data = d2)
 summary(lm1) 