#Packages
library(tidyverse)
library(finalfit)
library(Hmisc)
library(mice)
library(lattice)
library(parameters)
library(summarytools)
library(psych)
library(effectsize)
library(ppcor)
library(BayesFactor)
library(performance)
library(see)
library(car)
library(ggplot2)
library(dplyr)
library(JSmediation)
library(ggpubr)

#Importing our data

DATA <- read.csv(file.choose()) 

#Data Cleaning-------------------------------------------

##Choosing only the relevant variables

Only_Relevant <- dplyr:: select(DATA, 
                        iid, gender, match, race, samerace, attr_o, age, field, race, imprace, from, career, attr1_1, sinc1_1, intel1_1,fun1_1, amb1_1, shar1_1, attr3_1, dec, attr, sinc, intel, fun, amb, shar, like)  
Only_Relevant$gender <- as.factor(Only_Relevant$gender)
Only_Relevant$samerace <- as.factor(Only_Relevant$samerace)
Only_Relevant$race <- as.factor(Only_Relevant$race)
Only_Relevant$dec <- as.factor(Only_Relevant$dec)



##Outlier detection
summary(Only_Relevant)

#we can see that the range of ratings corresponds to the range reported in the data key

#Let's have a look at age distribution
boxplot(Only_Relevant$age)

#We can see that most of the participants are between 20 to 30 while some are significantly older
Only_Relevant <- mutate(Only_Relevant,
                     age_z = scale(age))

Only_Relevant_no_ol <- Only_Relevant %>%
  mutate(age_z = case_when(age_z >  2 ~ max(age_z[age_z <= 2]),
                              age_z < -2 ~ min(age_z[age_z >= 2]),
                              TRUE      ~ age_z))

boxplot(Only_Relevant_no_ol$age_z)

#Dealing with missing data

ff_glimpse(Only_Relevant_no_ol)

#since most of the missing data are personal attributed we can not imputate them
#I have decided not to ommit entierly the participants with missing data because this might lead to big loss of data
#so the only variable we will imputate is age

# imputate missing age
Only_Relevant_no_ol$age_imp <- impute(Only_Relevant_no_ol$age, fun = mean)


#General Participants Descriptives-------------------------------------

#We have 552 participants and 8378 observations, so to present demographics we'll create a new data with removed duplicates
iid_deduped <-Only_Relevant_no_ol %>% distinct(iid, .keep_all = TRUE)

#Age
qplot(data=iid_deduped,x= age_imp,geom="histogram",colour=I("white"), fill=I("blue"))

#Descriptives of Age by Gender (F=0, M=1)
iid_deduped<- iid_deduped %>% mutate(gender_name = case_when(gender == "0" ~ "F", gender == "1" ~ "M"))   

iid_deduped %>%
  group_by(gender_name) %>%
  summarise(mean(age_imp),
            median(age_imp),
            min(age_imp),
            max(age_imp)) %>% 
  ungroup()

#Distribution of age by gender
data_age <- iid_deduped %>% dplyr:: select(gender, age_imp)

ggplot(data = data_age, aes(x = age_imp,fill = gender)) + coord_flip() + 
  geom_histogram(data = subset(data_age, gender == "0"), binwidth = 2, color = "white") +  
  geom_histogram(data = subset(data_age, gender == "1"), 
                 aes(y = ..count.. * (-1)), binwidth = 2, color = "white") + 
  scale_y_continuous(breaks = seq(-70, 70, 10), labels = abs(seq(-70, 70, 10)))+ 
  scale_x_continuous(breaks = seq(10, 45, 5), labels = seq(10, 45,5)) + 
  labs(title = "Distribution of Age by Gender", x = "Age", y = "Count") + 
  scale_fill_discrete("Gender", labels = c("Female", "Male"))

#Race of the participants
iid_deduped <- iid_deduped %>% mutate(race_name = case_when(race == "1" ~ "African American",
                                             race == "2" ~ "Caucasian",
                                             race == "3" ~ "Latino",
                                             race == "4" ~ "Asian",
                                             race == "5" ~ "Native American",
                                             race == "6" ~ "Other"))  


table_race <- table(iid_deduped$race_name)
table_race
pie(table_race, main="Race of Participants")

#Descriptives of Age by Race (Black/African American=1,European/Caucasian-American=2,Latino/Hispanic American=3,Asian/Pacific Islander/Asian-American=4,Native American=5,Other=6)
iid_deduped %>%
  group_by(race_name) %>%
  summarise(mean(age, na.rm = TRUE),
            median(age, na.rm = TRUE),
            min(age, na.rm = TRUE),
            max(age, na.rm = TRUE)) %>% 
  ungroup()

#Distribution of age by race and gender
ggplot(data=iid_deduped, aes(x=race_name, y=mean(age_imp), fill=gender_name)) +
  geom_bar(stat="identity") +
  labs(title = "Distribution of Age by Race and Gender", x = "Race", y = "Count") + 
  scale_fill_discrete("Gender", labels = c("Female", "Male"))
                                      

#Wordcloud of study fiels
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- iid_deduped$field %>% 
  na.omit()

docs <- Corpus(VectorSource(text))
inspect(docs)

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
   

#Question Number 1: who is more picky - men or women?---------------------------------
speedate <- Only_Relevant_no_ol %>% 
  mutate(gender_name = case_when(gender == "0" ~ "F", gender == "1" ~ "M")) 

picky <- chisq.test(speedate$gender_name, speedate$dec, correct = FALSE)
picky

picky$expected
picky$observed

barplot(picky$observed,picky$expected, beside = TRUE)

chisq_to_phi(chisq = picky$statistic, n = sum(picky$observed))

#Question Numer 2: How accurate are participants in their self-perception? 
#Variable "attr3_1" by the data key is -> How do you think you measure up? Please rate your opinion of your own attributes, on a scale of 1-10 (be honest!):
#and variable "attr_o" is -> rating by partner the night of the event
#So by corelating these two variables we will get information about how accuratley participants perecieve themselves

Self_awareness <- cor.test(speedate$attr_o, speedate$attr3_1)
Self_awareness


ggscatter(speedate, x = "attr3_1", y = "attr_o", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Self-Measure", ylab = "Rating by partner")

#Question numer 3:Is there any difference in Self-Perception between men and women? 
#To test this hypothesis I've created a new variable "sp" which is the difference between self perception and the perception of the partner
#Small difference indicates accurate self perception

speedate <- speedate %>% 
  mutate(sp = abs(attr3_1 - attr_o)) %>% 
  na.omit(sp)

#Normality
shapiro.test(speedate$sp) #The data is too big for shapiro test so let's see some graphs

qqnorm(speedate$sp, pch = 1, frame = FALSE)   
qqline(speedate$sp, col = "steelblue", lwd = 2)

#Homogeneity of variance
leveneTest(speedate$sp, as.factor(speedate$gender_name), center = mean)

#t test

t.test(speedate$sp[speedate$gender == "1"],
       speedate$sp[speedate$gender == "0"], var.equal = TRUE)

cohens_d(speedate$sp[speedate$gender == "1"],
         speedate$sp[speedate$gender == "0"])

#Bayesian t test
ttestBF(speedate$sp[speedate$gender == "1"],
        speedate$sp[speedate$gender == "0"])

#Descriptives
my.sum <- speedate %>%
  group_by(gender_name) %>%
  summarise(mean = mean(sp),
            sd = sd(sp),
            median = median(sp),
            min = min(sp),
            n = n(),
            max = max(sp)) %>% 
  ungroup()

ggplot(my.sum, aes(x=gender_name, y=mean, fill=gender_name)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(x=gender_name, ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  ggtitle("Using standard deviation")

#Question number 4: Which one of the following attributes has the biggest impact on liking the other person

attributes <- dplyr:: select(iid_deduped,
                     like, attr, sinc, intel, fun, amb, shar)

mul_reg <- lm(like ~ attr + sinc + intel + fun + amb + shar, attributes) #Model fitting
summary(mul_reg)

#OLS assumptions
crPlots(mul_reg) #Non-linearity Detection
qqPlot(mul_reg) #Non-normality Detecting
ncvTest(mul_reg)# Homoscedasticity evaluation
vif(mul_reg) #Collinearity Evaluation
plot(mul_reg)

reg_table <- model_parameters(mul_reg, standardize = "basic")
reg_table

plot(mul_reg)

#Question number 5: Does attraction mediate the relation between perceiving the other person as one with shared interests to mine and liking this person

med <- mdt_simple(data = speedate,  
                        IV = shar,
                        DV = like,
                        M  = attr)
#OLS assumptions
model_1 <- extract_model(med, step = "X -> M")

par(mfrow=c(1,1))
plot(model_1, ask = FALSE)

model_2 <- extract_model(med, step = 2)

plot(model_2, ask = FALSE)

model_3 <- extract_model(med, step = 3)

plot(model_3, ask = FALSE)

#Interpetation
med
add_index(med)
display_models(med)
