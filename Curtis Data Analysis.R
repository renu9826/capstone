#Normalization of Data, Descriptive analysis (DA) and Regression
library(som)
library(mlbench)
library(reporttools)
devtools::install_github('hadley/ggplot2')
devtools::install_github("jcheng5/dashtutorial")
# DA
dim()
dim(Curtis_Skill2)
dim(EndorSkill11)
dim(EndorSkill21)
dim(EndorSkill31)
DIM<- as.data.frame(table(sapply(CurtisEE3, class)))
Variable<- as.data.frame(table(colnames(CurtisEE3)))
CurtisSumm2<- as.data.frame(summary(CurtisEE3))
CurtisDeviation<- as.data.frame(sapply(CurtisEE3 [,c(6,12,13,28,30,32,35,39)],sd))
colnames(CurtisEE3)
colnames(EndorSkill11)
CurtisEE3$Gender <- as.factor(CurtisEE3$Gender)
CurtisEE3$Race <- as.factor(CurtisEE3$Race)
CurtisEE3$Location <- as.factor(CurtisEE3$Location)
CurtisEE3$edu <- as.factor(CurtisEE3$edu)
CurtisEE3$Current_Title <- as.factor(CurtisEE3$Current_Title)
CurtisEE3$Title_Level <- as.factor(CurtisEE3$Title_Level)
CurtisEE3$Previous_Title1 <- as.factor(CurtisEE3$Previous_Title1)
CurtisEE3$Previous_Title2 <- as.factor(CurtisEE3$Previous_Title2)
CurtisEE3$Previous_Title3 <- as.factor(CurtisEE3$Previous_Title3)
CurtisEE3$Previous_Title4 <- as.factor(CurtisEE3$Previous_Title4)
CurtisEE3$Previous_Title5 <- as.factor(CurtisEE3$Previous_Title5)
CurtisEE3$Title_Level1 <- as.factor(CurtisEE3$Title_Level1)
CurtisEE3$Title_Level2 <- as.factor(CurtisEE3$Title_Level2)
CurtisEE3$Title_Level3 <- as.factor(CurtisEE3$Title_Level3)
CurtisEE3$Title_Level4 <- as.factor(CurtisEE3$Title_Level4)
CurtisEE3$Title_Level5 <- as.factor(CurtisEE3$Title_Level5)
CurtisEE3$Title_level_Endorcer1 <- as.factor(CurtisEE3$Title_level_Endorcer1)
CurtisEE3$Title_level_Endorcer2 <- as.factor(CurtisEE3$Title_level_Endorcer2)
CurtisEE3$Title_level_Endorcer3 <- as.factor(CurtisEE3$Title_level_Endorcer3)
write.csv()
#Normalize DemoEndor3

NorCDE3 <- CDE3
NorCDE3$Age<- as.vector(normalize(NorCDE3$Age))
NorCDE3$No_Connection <- as.vector(normalize(NorCDE3$No_Connection))
NorCDE3$Year_of_Exp <- as.vector(normalize(NorCDE3$Year_of_Exp))
NorCDE3$No_of_Job_changed <- as.vector(normalize(NorCDE3$No_of_Job_changed))
NorCDE3$No_of_titles <- as.vector(normalize(NorCDE3$No_of_titles))
NorCDE3$Current_Salary <- as.vector(normalize(NorCDE3$Current_Salary))
NorCDE3$SuccessScore <- as.vector(normalize(NorCDE3$SuccessScore))
NorCDE3$YOEEndorcer1 <- as.vector(normalize(NorCDE3$YOEEndorcer1))
NorCDE3$YOEEndorcer2 <- as.vector(normalize(NorCDE3$YOEEndorcer2))
NorCDE3$YOEEndorcer3 <- as.vector(normalize(NorCDE3$YOEEndorcer3))
NorCDE3$Total_Endorsements <- normalize(NorCDE3$Total_Endorsements)
NorCDE3$CS <-normalize(NorCDE3$CS)
NorCDE3$Total_Endo1 <- normalize(as.vector(NorCDE3$Total_Endo1))
NorCDE3$CS1 <- normalize(as.vector(NorCDE3$CS1))
NorCDE3$Total_Endo2<- normalize(as.vector(NorCDE3$Total_Endo2))
NorCDE3$CS2 <- normalize(as.vector(NorCDE3$CS2))
NorCDE3$Total_Endo3 <- normalize(as.vector(NorCDE3$Total_Endo3 ))
NorCDE3$CS3 <- normalize(as.vector(NorCDE3$CS3))


#Normalize CurtisSkill
NormoCurtis21 <- Curtis_Skill2
NormoCurtis21$No_of_Endorsed <- as.vector(normalize(NormoCurtis21$No_of_Endorsed))
NormoCurtis21$Title_Level <- as.factor(NormoCurtis21$Title_Level)

NormoCurtis21 <- NormoCurtis21[,c(2,3,1,4,5)]

#Normalize EndorSkill11
NormEndoSki11 <- EndorSkill11
NormEndoSki11$No_of_Endorsed1 <- as.vector(normalize(NormEndoSki11$No_of_Endorsed1))
NormEndoSki11$Title_LevelEndorcer1 <- as.factor(NormEndoSki11$Title_LevelEndorcer1)


NormEndoSki11 <- NormEndoSki11[,c(6,1,3,2,4,5)]

# Normalize EndorSkill21
NormEndoSki21 <- EndorSkill21
NormEndoSki21$Title_LevelEndorcer2 <- as.factor(NormEndoSki21$Title_LevelEndorcer2)



#Normalize EndorSkill31

NormEndoSki31 <- EndorSkill31
NormEndoSki31$Title_LevelEndorcer3 <- as.factor(NormEndoSki31$Title_LevelEndorcer3)

NormEndoSki31$No_of_Endorsed3 <- as.factor(normalize(NormEndoSki31$No_of_Endorsed3))
NormEndoSki31 <- NormEndoSki31[,c(6,1,3,2,4,5)]
# Statistics Descriptive 
# What is the distibution of data?
r<- ggplot(CDE3, aes(x=Age))+
  geom_density(fill="green")+
  facet_grid(~Race)

ggplotly(r)
prop.table(CDE3$Gender)

plot_ly(CDE3, x=CDE3$Age, y=CDE3$No_Connection, type = "scatter", 
       mode= "marker", color = CDE3$Gender)

CDE3$Gender <- as.factor(CDE3$Gender)


# Corrplot for finding variables affecting success score
CDE3_CorMatrix<- as.matrix(CDE3[,c(4,10:13,15,26,28:30)])
o<- cor(CDE3_CorMatrix)
corrplot(o, method = "pie", type = "upper")
NorCDE3_CorMatrix <- as.matrix(NorCDE3[,c(4,10:13,15,26,28:30)])
NorCDE3$Title_Level <- as.numeric(NorCDE3$Title_Level)
m<- cor(NorCDE3_CorMatrix)
corrplot(m,method = "pie", type = "upper")
# Simple regression Model 1 


l<- lm(NorCDE3$SuccessScore ~ NorCDE3$Total_Endorsements)
summary(l)
plot (l)

#Multivariate analysis

p <- lm(((NorCDE3$SuccessScore~NorCDE3$No_of_Job_changed+
            NorCDE3$No_Connection+NorCDE3$No_of_titles)))
summary(p)
#Scatter Plot
plot_ly(NorCDE3, x = ~No_of_titles, y = ~SuccessScore,
        colors = "green") %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'No.of Titles'),
                      yaxis = list(title = 'SuccessScore')))

plot_ly(NorCDE3, x = ~No_of_Job_changed, y = ~SuccessScore,
        colors = "green") %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'No.of Jobs'),
                      yaxis = list(title = 'SuccessScore')))





