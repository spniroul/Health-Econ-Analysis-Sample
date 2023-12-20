
##########################################################################
################################################################################

##Setting up Directory
getwd()
setwd("SET DIRECTORY HERE IF NECESSARY")

#Importing population records

pop <- read.csv("psam_p33.csv")
pop1 <- pop

#Keeping the following variables for our purpose
#SERIALNO - Individual & Household IDs
#ST and PUMA - State ID and PUMA
#AGEP - Age
#SEX - Sex
#RAC1P - Race
#POVPIP - Income as measured to FPL
#HICOV - Health Insurance Coverage
#HINS1 - Insurance through Employer
#HINS3 - Medicare Eligibility
#HINS4 - Medicaid or other government assistance
#HINS5 - TRICARE
#Hins6 - VA Coverage

library(tidyverse)
pop1 <- pop1 %>% 
  select(SERIALNO, ST, PUMA,
         AGEP, SEX, RAC1P, POVPIP, HICOV,
         HINS1, HINS3, HINS4, HINS5, HINS6)

#Recoding SEX and RACE to dummy variables
#FEMALE = 1 - Female
#RACE = 1 - White
pop1 <- pop1 %>% 
  mutate(FEMALE = case_when(SEX == 1 ~ 0, SEX == 2 ~ 1)) %>% 
  mutate(RAC1P = case_when(RAC1P == 1 ~ 1, TRUE ~ 0))

#Generating tax-family variable
pop1 <- pop1 %>% group_by(SERIALNO, POVPIP) %>% 
  mutate(tax_family = n()) 


#Dropping observations where tax-family>6
pop1 <- pop1 %>% 
  filter((tax_family < 6))

#Dropping Individuals who currently have:
#Employer-sponsored insurance
#Medicare or eligible for Medicare due to age i.e. 65 or older
#TRICARE
#VA Coverage
#Medicaid
#None of the above but will be eligible for Medicaid
pop1 <- pop1 %>% 
  filter(HINS1 == 2,
         HINS3 == 2,
         HINS4 == 2,
         HINS5 == 2,
         HINS6 == 2) %>% 
  filter(!(AGEP<19 & POVPIP < 318)) %>%
  filter(!(AGEP >= 65))

#Generating Coverage-Family variable
pop1 <- pop1 %>% 
  group_by(SERIALNO, tax_family) %>% 
  mutate(coverage_family = n())

#Generating Family Income from POVPIP
family_func <- function(x, y){
  case_when(x == 1 ~
              y/400*11880,
            x == 2 ~
              y/400*16020,
            x == 3 ~
              y/400*20160,
            x == 4 ~
              y/400*24300,
            x == 5 ~
              y/400*28440,
            x == 6 ~
              y/400*32580,
  )
}

pop1 <- pop1 %>% 
  mutate(FAMILYINCOME=mapply(family_func,tax_family, POVPIP))


#Calculating Premium Tax Credit (PTC)
#Filtering the dataset with families whose income level is 
#beyond 400% of the Federal Poverty Level

pop1 <- pop1 %>% 
  filter(POVPIP<401)

contribution_func <- function(x, y){
  case_when((y < 133)~
              x*(0.0204),
            (y >= 133 & y < 150)~ 
              x*(0.0006*(y-133)+0.0306), 
            (y >= 150 & y < 200)~
              x*(0.00047*(y-150)+0.0408),
            (y >= 200 & y < 250)~ 
              x*(0.000356*(y-200)+0.0643),
            (y >= 250 & y < 300)~
              x*(0.000296*(y-250)+0.0821),
            (y >= 300)~
              x*0.0969)
}

pop1 <- pop1 %>% 
  mutate(CAT = mapply(contribution_func,FAMILYINCOME, POVPIP))

#Importing Silver Plan Premium File
library(readxl)
premium <- read_excel("premiums.xls")
premium <- rename(premium, AGEP = age)

#Merging premium file with the existing dataset
pop1 <- merge(pop1, premium[,c("AGEP","annual_2sls_premium_USD")],by="AGEP",all.x = T)

#Computing the subsidy level(SUBSIDY) and Average Subsidy at Family Level (AVGCOVERAGEFAM)
pop1 <- pop1 %>% 
  group_by(SERIALNO, coverage_family) %>% 
  mutate(sum_premium = sum(annual_2sls_premium_USD)) %>% 
  mutate(SUBSIDY = round(pmax(sum_premium - CAT, 0),2)) %>% 
  mutate(AVGCOVERAGEFAM = round(SUBSIDY/coverage_family,2)) %>% 
  ungroup()

#Exporting the final dataset into a csv file
export <- pop1 %>% 
  select(SERIALNO, PUMA, STATE = ST, AGE = AGEP, RACE = RAC1P, FEMALE, POVPIP, SUBSIDY, AVGCOVERAGEFAM)
write.csv(export,"Final_Niroula.csv", row.names = FALSE)

#Quantile, Mean and Standard Deviation for Subsidy Levels at Individual Level
t <- pop1

#Number of Observations at Individual LEvel
individualpop <- nrow(t)

quant_func <- function(data){
  data %>% 
    ungroup() %>% 
    summarize(quant10 = round(quantile(SUBSIDY, probs = c(0.1)),2), quant90 = round(quantile(SUBSIDY, probs = c(0.9)),2),mean = round(mean(SUBSIDY),2), sd = round(sd(SUBSIDY),2))
  
}

#Subsidy Levels by Race and Sex
t <- t %>% ungroup()
View(bind_rows(quant_func(t),quant_func(filter(t, FEMALE==1)), quant_func(filter(t, FEMALE==0)),
               quant_func(filter(t, RAC1P==1)),quant_func(filter(t, RAC1P==0))))

#Age group distribution across the dataset     
pop1 %>% 
  ungroup() %>% 
  summarize(quant10 = quantile(AGEP, probs = c(0.1)), quant90 = quantile(AGEP, probs = c(0.9)),mean = mean(AGEP), sd = sd(AGEP))


#Quantile, Mean and Standard Deviation for Subsidy Levels at Family Level
t <- pop1 %>% 
  group_by(SERIALNO) %>%
  summarize(SUBS = mean(SUBSIDY)) 

print(familypop1<- nrow(t))
t %>% 
  summarize(quant10 = quantile(SUBS, probs = 0.1), quant90 = quantile(SUBS, probs = 0.9), mean = mean(SUBS), sd = sd(SUBS))

#Average Coverage Family Size  
pop1 %>% 
  group_by(SERIALNO) %>%
  summarize(coverage_family = mean(coverage_family)) %>% 
  ungroup() %>% 
  summarize(mean_coverage_family = mean(coverage_family))


#Generating individual-level plot
t <- pop1 %>% 
  mutate(AVGSUBS = SUBSIDY/coverage_family)

#Generating binned scatter plot for average individual level subsidy change with age
library(binsreg)

a <- binsreg(y = t$AVGSUBS, x = t$AGEP) 
a$bins_plot +  labs(x="AGE", 
                    y="SUBSIDY LEVEL") +
  scale_y_continuous(breaks=c(0,500,1000,1500, 2000,2500, 3000,3500,
                              4000,4500, 5000, 5500, 6000, 6500, 7000,
                              7500))+
  ggtitle("Subsidy Level by Age Group") +
  theme(plot.title = element_text(hjust=0.5))

#install.packages("broom")
library(broom)

#Regression output between AVGSUBS and AGEP
reg1 <- lm(t$AVGSUBS~t$AGEP)
write.csv(tidy(reg1), "AGESUBSREG.csv")

#Generating binned scatter plot for average individual level subsidy change with income 

b <- binsreg(t$AVGSUBS, t$POVPIP)
b$bins_plot +  labs(x="INCOME LEVEL", 
                    y="SUBSIDY LEVEL") +
  ggtitle("Subsidy Level by Federal Poverty Line Income Level") +
  theme(plot.title = element_text(hjust=0.5))

#Regression output between AVGSUBS and POVPIP
reg2 <- lm(t$AVGSUBS~t$POVPIP)
write.csv(tidy(reg2), "INCOMESUBSREG.csv")


#Generating Pie-Chart to visualize Subsidy Level by FEMALE
t %>% 
  group_by(FEMALE) %>% 
  summarise(SUBSIDYFEMALE = sum(AVGSUBS, na.rm = T))  %>% 
  ggplot(aes(x="", y= SUBSIDYFEMALE, fill= as.character(FEMALE)))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0("", scales::percent(SUBSIDYFEMALE / sum(SUBSIDYFEMALE)),
                               "")), position = position_stack(vjust = 0.52)) +
  scale_fill_discrete(labels = c("Male", "Female"))+
  guides(fill=guide_legend(title="FEMALE")) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"))+
  ggtitle("Subsidy Level by Sex") +
  theme(plot.title = element_text(hjust=0.45, vjust = 0.1))

#Regression with Female
summary(lm(formula = t$AVGSUBS ~ t$FEMALE))

#Generating Pie-Chart to visualize Subsidy Level by Race
t %>% 
  group_by(RAC1P) %>% 
  summarise(SUBSIDYRACE = sum(AVGSUBS, na.rm = T))  %>% 
  ggplot(aes(x="", y= SUBSIDYRACE, fill= as.character(RAC1P)))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0("", scales::percent(SUBSIDYRACE / sum(SUBSIDYRACE)),
                               "")), position = position_stack(vjust = 0.52)) +
  scale_fill_discrete(labels = c("Others", "White"))+
  guides(fill=guide_legend(title="RACE")) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"))+
  ggtitle("Subsidy Level by Race") +
  theme(plot.title = element_text(hjust=0.45, vjust = 0.1))

#Regression with Race
summary(lm(formula = t$AVGSUBS ~ t$RAC1P))

#Top 10% families with highest and lowest subsidy levels
#Highest 10%
n <- nrow(pop1)
highest10per <- pop1 %>% 
  subset(SUBSIDY > quantile(SUBSIDY, prob = 1 - 10/100)) %>% 
  arrange(desc(SUBSIDY))

#Lowest 10%
lowest10per <- pop1 %>% 
  subset(SUBSIDY < quantile(SUBSIDY, prob = 10/100)) %>% 
  arrange(SUBSIDY)

#Top 10 percent Analysis
#Counting Sexes
nrow(filter(highest10per, FEMALE == 1))
nrow(filter(highest10per, FEMALE == 0))

#Grouping together by SERIALNO
View(highest10per %>%
       select(AGEP, SERIALNO, SEX, RAC1P, POVPIP, SUBSIDY, coverage_family) %>% 
       group_by(SERIALNO)) 

#Age Group Analysis
quantile(highest10per$AGEP)

#Counting Races
nrow(filter(highest10per, RAC1P == 1))
nrow(filter(highest10per, RAC1P == 0))

#Summarizing by family size
highest10per %>%
  ungroup() %>% 
  select(AGEP, SERIALNO, SEX, RAC1P, POVPIP, SUBSIDY, coverage_family) %>% 
  summarise(mean(coverage_family))

#Bottom 10 percent Analysis
#Counting Sexes
nrow(filter(lowest10per, FEMALE == 1))
nrow(filter(lowest10per, FEMALE == 0))

#Grouping together by SERIALNO
View(lowest10per %>%
       select(AGEP, SERIALNO, SEX, RAC1P, POVPIP, SUBSIDY, coverage_family) %>% 
       group_by(SERIALNO)) 

#Age Group Analysis
quantile(lowest10per$AGEP)

#Counting Races
nrow(filter(lowest10per, RAC1P == 1))
nrow(filter(lowest10per, RAC1P == 0))

#Summarizing by family size
lowest10per %>%
  ungroup() %>% 
  select(AGEP, SERIALNO, SEX, RAC1P, POVPIP, SUBSIDY, coverage_family) %>% 
  summarise(mean(coverage_family))

################################################################################
################################################################################

