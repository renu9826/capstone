library(generator)
library(usmap)
library(lubridate)
Demographic <-  data.frame(ID= integer (1000),
                           Name=character(1000),
                           Date_of_birth =(character(1000)),
                           Age = integer(1000),
                           gender = (character(1000)),
                           Race = (character(1000)),
                           Location= (character(1000)),
                           country = (character(1000)),
                           stringsAsFactors=FALSE)  



Demographic$Name <- r_full_names(1000)

unique(Demographic$Name)

Demographic$Date_of_birth <-as.Date(sample.int(as.Date("1995-01-01") - as.Date("1953-01-01"), size =1000,replace = TRUE), origin = as.Date("1953-01-01"))
Demographic$Age <- as.integer((Sys.Date()-Demographic$Date_of_birth)/365.25)


Demographic$Race <- sample (c ("Asian","African American", "Caucasian", "Native American","Hispanic"), 1000, replace = TRUE)
US<- data(statepov)
Demographic$Location <-sample (unique(statepop$abbr), 1000, replace = TRUE)

Demographic$gender <-sample (c("Female","Male"), 1000, replace = TRUE)
Demographic$country <- "USA"

Demographic$ID<- 1:nrow(Demographic)


Demographic$edu<- sample(c("Undergraduate", "Graduate", "Doctorate", "Associate"), size = 1000, replace = TRUE)
table(Demographic$edu)
Demographic$edu<-ifelse((Demographic$Age < 33 & Demographic$edu =="Doctorate"), "Graduate", 
                        ifelse((Demographic$Age < 24 & Demographic$edu =="Graduate"),"Undergraduate",
                               ifelse((Demographic$Age < 22 & Demographic$edu =="Undergraduate"), "Associate",
                                      ifelse((Demographic$Age < 20 & Demographic$edu =="Associate"), "Highschool", Demographic$edu))))
Demographic[(Demographic$Age < 33 & Demographic$edu == "Doctorate"), ]

Demographic$No_Connection <-sample(c(300:500), size = 1000, replace = TRUE)
table(Demographic$No_Connection)

value_yoe <- function(Age, edu){
  
  if(edu=="Undergraduate"){
    l <- (Age -22)
  }else if(edu=="Graduate"){
    l <- (Age -24)
  }else if(edu=="Highschool"){
    l <- (Age -18)
  }else if(edu=="Associate"){
    l <- (Age -20)
  }else {
    l <- (Age -33)
  }
  return(l);
}

for(i in 1:1000)
{
  Demographic$Year_of_Exp[i] <- value_yoe(Demographic$Age[i],Demographic$edu[i]);
}
table(Demographic$Year_of_Exp)

Demographic<- Demographic[!(Demographic$Year_of_Exp %in% c(-11, -10, -9)), ] 

value_Job_change <- function(year_of_experience){
  
  if(year_of_experience %in% c(0:4)){
    l <- 1
  }else if(year_of_experience %in% c(5:9)){
    l <-  (sample(c(1:3), size =1, replace = TRUE))
  }else if(year_of_experience %in% c(10:18)){
    l <- (sample(c(1:5), size =1, replace = TRUE))
  }else if (year_of_experience %in% c(18:25)){
    l <- (sample(any(1:8), size =1, replace = TRUE))
  }else {
    l <- (sample(c(1:12), size =1, replace = TRUE))
    return(l);
  }
}

for(i in 1:973)
{
  Demographic$No_of_Job_changed[i] <- value_Job_change(Demographic$Year_of_Exp[i]);
}
table(Demographic$No_of_Job_changed)
# Normalized Titles based on Watson Wyatt Global Grades.
Titles <- Title1

Title1 <- unique(grep("intern|worker|trainee|operator", Titles$Job_Title,ignore.case=TRUE,value=TRUE)) 
Title1<- Title1[-c(grep("Intern Software Engineer|Internship|Business Analyst Intern|Software intern|
                        Software Engineer Intern|Research intern|Intern Software Engineer|Product ManAgement Intern", 
                        Title1))]
Title2 <- unique(grep("Technician|Assistant|Clerk|supervisor",Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title2 <- Title2[-c(3,5,7)]
Title3 <- unique(grep("Junior Developer|Staff Engineer|Analyst|Business ManAger|System administration|Advisory Engineer|Team lead|Technical Writer",
                      Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title3 <- Title3[-c(grep("Sr.|Intern|Senior", Title3))]
Title3<- Title3[-c(grep("developer", Title3))]
Title3 <- Title3[-c(grep("Systems Analyst",Title3))]

Title4 <- unique(grep("Senior Consultant|Architect|Senior ManAger|Senior Developers|Senior advisor|General manAger|Sr|Senior",
                      Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title4 <- Title4[-c(grep("Senior engineer|Senior SW Engineer|Senior Engineer (Band 9)|sr. software engineer
                         |Sr Enginner|senior developer|senior software dev|Senior software Engineer
                         |Sr. Developer|Senior Developer|Senior Software engineer|Architect|Senior software developer
                         |Senior Developer|Senior Software Developer|Sr Software Engineer|sr engineer|systems architect|Sr systems analyst
                         |Director of Technology / Sr. Lead Developer|Sr Engineer|sr. software engineer|Sr. Software Engineer
                         |Sr Developer|Senior Engineer|Sr DevOps|Sr systems analyst|Sr. Systems Analyst|Sr Systems Engineer|
                         Sr. Director of Program ManAgement|Sr DevOps|General ManAger", Title4))]
Title5 <- unique(grep("Executive manAger|Vice President|Director|Managing director",
                      Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title6 <- unique(grep("CEO|CTO|founder|cofounder",Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title6 <- Title6[-c(grep("Director|ManAger|Inspector|contractor|Engineer|Developer", Title6))]
Title6<- Title6[-c(grep("Director|ManAger|Inspector|contractor|Engineer|Developer|Cto|CTO / Cofounder|CEO (Co-Founder)|
                        founder, CTO|CTO Google rail product|Consultant, CTO as needed|Co-founder", Title6))]
Title6<- Title6[-c(grep("founder, CTO|CEO (Co-Founder)", Title6))] 

table(Demographic1$Year_of_Exp)
table(Demographic$edu[Demographic$Year_of_Exp %in% c(-2)])
max(Demographic$Year_of_Exp)

# Assigning No of Titles.
Demographic$No_of_titles <- ifelse(Demographic$Year_of_Exp < 2 , 1, 
                                     ifelse(Demographic$Year_of_Exp %in% c(3:8),c(1:3),
                                            ifelse(Demographic$Year_of_Exp %in% c(9:20), c(3:5),
                                                   ifelse(Demographic$Year_of_Exp %in% c(21:44),c(4:6),c(3:6)
                                                   ))))

table(Demographic$No_of_titles )

Demographic$No_of_titles[Demographic$Year_of_Exp %in% c(3:7)]
#Assigning Current Titles - 

Current <- function(edu, yoe){
  if(edu %in% c ("Undergradute","Graduate")){
    if(yoe %in% c(0-2)) {
      m <- sample(c(Title2,Title1), size=1, replace = TRUE)
    }else if(yoe %in% c(3:8)){
      m <- sample(c(Title6,Title4,Title3,Title2), size=1, replace = TRUE)
    }else if(yoe %in% c(9:20)) {
      m <- sample(c(Title6,Title5,Title4), size=1, replace =TRUE)
    }else {
      m <- sample(c(Title6,Title4,Title5), size=1, replace =TRUE)
    }
  }
  else if(edu %in% c ("Highschool","Associate")){
    if(yoe %in% c(0-2)) {
      m <- sample(Title1, size=1, replace = TRUE)
    }else if(yoe %in% c(3:8)) {
      m <- sample(c(Title3,Title2), size=1, replace = TRUE)
    }else if(yoe %in% c(9:20)) {
      m <- sample(c(Title6,Title4,Title3), size=1, replace =TRUE)
    }else {
      m <- sample(c(Title6,Title3,Title4), size=1, replace =TRUE)
    }
  } 
  else {
    if(yoe %in% c(0-2)) {
      m <- sample(c(Title2,Title1), size=1, replace = TRUE)
    }else if(yoe %in% c(3:8)) {
      m <- sample(c(Title6,Title4,Title3), size=1, replace = TRUE)
    }else if(yoe %in% c(9:20)) {
      m <- sample(c(Title6,Title5,Title4), size=1, replace =TRUE)
    }else {
      m <- sample(c(Title4,Title5,Title6), size=1, replace =TRUE)
    }
  }
  return(m);
}

for (i in 1:973) {
  Demographic$Current_Title[i] <- Current(Demographic$edu[i],Demographic$Year_of_Exp[i]);
}
table(Demographic$Current_Title)
# Assigning Title Level 

Demographic$Title_Level<- ifelse(Demographic$Current_Title %in% Title6, 6,
                                   ifelse(Demographic$Current_Title %in% Title5, 5,
                                          ifelse(Demographic$Current_Title %in% Title4, 4,
                                                 ifelse(Demographic$Current_Title %in% Title3,3,
                                                        ifelse(Demographic$Current_Title %in% Title2, 2,1)))))
table(Demographic$Title_Level)

# Previous 1 Title and title level   Assignment 
Previous1<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title4,Title5), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size =1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2 & TL == 2)) {
      l <- sample(c(Title2,Title1), size = 1,replace = TRUE)
    }else {
      l <- sample (Title1, size = 1, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
      }else if (yoe %in% c(0:2) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title1), size = 1, replace = TRUE) 
    }
         
    }
  else{
    if (yoe %in% c(0:2) & TL == 1) {
      l <- sample(Title1, size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6 ) {
      l <- sample(c(Title6,Title4, Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title3,Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else {
      l <- sample (c(Title3), size = 1, replace = TRUE)
    } 
  }
  return (l);
}

for (i in 1: 973)
  if (Demographic$No_of_titles [i] > 1) {
    Demographic$Previous_Title1[i]  <- Previous1 (Demographic$edu[i],Demographic$Year_of_Exp[i],Demographic$Title_Level[i] )
  }else {
    Demographic$Previous_Title1[i] <- "NA"
  }

Demographic$Title_Level1<- ifelse(Demographic$Previous_Title1 %in% Title6, 6,
                                    ifelse(Demographic$Previous_Title1 %in% Title5, 5,
                                           ifelse(Demographic$Previous_Title1 %in% Title4, 4,
                                                  ifelse(Demographic$Previous_Title1 %in% Title3,3,
                                                         ifelse(Demographic$Previous_Title1 %in% Title2, 2,
                                                                ifelse(Demographic$Previous_Title1 %in% Title1,1,"NA"))))))


table(Demographic$Title_Level1)
table(Demographic$edu)


Previous2<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title4,Title5), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size =1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2 & TL == 2)) {
      l <- sample(c(Title2,Title1), size = 1,replace = TRUE)
    }else {
      l <- sample (Title1, size = 1, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title1), size = 1, replace = TRUE) 
    }
  }
  else{
    if (yoe %in% c(0:2) & TL == 1) {
      l <- sample(Title1, size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6 ) {
      l <- sample(c(Title6,Title4, Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title3,Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else {
      l <- sample (c(Title3), size = 1, replace = TRUE)
    } 
  }
  return (l);
}
for (i in 1: 973)
  if (Demographic$No_of_titles [i] > 2) {
    Demographic$Previous_Title2[i]  <- Previous2 (Demographic$edu[i],Demographic$Year_of_Exp[i],
                                                    Demographic$Previous_Title1[i] )
  }else {
    Demographic$Previous_Title2[i] <- "NA"
  }

Demographic$Title_Level2<- ifelse(Demographic$Previous_Title2 %in% Title6, 6,
                                    ifelse(Demographic$Previous_Title2 %in% Title5, 5,
                                           ifelse(Demographic$Previous_Title2 %in% Title4, 4,
                                                  ifelse(Demographic$Previous_Title2 %in% Title3,3,
                                                         ifelse(Demographic$Previous_Title2 %in% Title2, 2,
                                                                ifelse(Demographic$Previous_Title2 %in% Title1,1,"NA"))))))

table(Demographic$Previous_Title2)
table(Demographic$Title_Level2)


Previous3<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title4,Title5), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size =1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2 & TL == 2)) {
      l <- sample(c(Title2,Title1), size = 1,replace = TRUE)
    }else {
      l <- sample (Title1, size = 1, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title1), size = 1, replace = TRUE) 
    }
    
  }
  else{
    if (yoe %in% c(0:2) & TL == 1) {
      l <- sample(Title1, size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6 ) {
      l <- sample(c(Title6,Title4, Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title3,Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else {
      l <- sample (c(Title3), size = 1, replace = TRUE)
    } 
  }
  return (l);
}
for (i in 1: 973)
  if (Demographic$No_of_titles [i] > 3) {
    Demographic$Previous_Title3[i]  <- Previous3 (Demographic$edu[i],Demographic$Year_of_Exp[i],
                                                    Demographic$Previous_Title2[i] )
  }else {
    Demographic$Previous_Title3[i] <- "NA"
  }

Demographic$Title_Level3<- ifelse(Demographic$Previous_Title3 %in% Title6, 6,
                                    ifelse(Demographic$Previous_Title3 %in% Title5, 5,
                                           ifelse(Demographic$Previous_Title3 %in% Title4, 4,
                                                  ifelse(Demographic$Previous_Title3 %in% Title3,3,
                                                         ifelse(Demographic$Previous_Title3 %in% Title2, 2,
                                                                ifelse(Demographic$Previous_Title3 %in% Title1,1,"NA"))))))
table(Demographic$Title_Level3)

Previous4<-function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title4,Title5), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size =1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2 & TL == 2)) {
      l <- sample(c(Title2,Title1), size = 1,replace = TRUE)
    }else {
      l <- sample (Title1, size = 1, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title6,Title5,Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title5,Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title1), size = 1, replace = TRUE) 
    }
    
  }
  else{
    if (yoe %in% c(0:2) & TL == 1) {
      l <- sample(Title1, size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6 ) {
      l <- sample(c(Title6,Title4, Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title3,Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(c(Title4,Title3), size = 1, replace = TRUE)
    }else {
      l <- sample (c(Title3), size = 1, replace = TRUE)
    } 
  }
  return (l);
}

for (i in 1: 973)
  if (Demographic$No_of_titles [i] > 4) {
    Demographic$Previous_Title4[i]  <- Previous4(Demographic$edu[i],Demographic$Year_of_Exp[i],
                                                   Demographic$Previous_Title3[i] )
  }else {
    Demographic$Previous_Title4[i] <- "NA"
  }

Demographic$Title_Level4<- ifelse(Demographic$Previous_Title4 %in% Title6, 6,
                                    ifelse(Demographic$Previous_Title4 %in% Title5, 5,
                                           ifelse(Demographic$Previous_Title4 %in% Title4, 4,
                                                  ifelse(Demographic$Previous_Title4 %in% Title3,3,
                                                         ifelse(Demographic$Previous_Title4 %in% Title2, 2,
                                                                ifelse(Demographic$Previous_Title4 %in% Title1,1,"NA"))))))

table(Demographic$Title_Level4)

Previous5<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title5,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size =1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title3,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title2,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2 & TL == 2)) {
      l <- sample(c(Title2,Title1), size = 1,replace = TRUE)
    }else {
      l <- sample (Title1, size = 1, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(21:44) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL == 4) {
      l <- sample(c(Title3,Title4), size =1, replace = TRUE)
    }else if (yoe %in% c(3:20) & TL ==3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 3) {
      l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
    }else if (yoe %in% c(0:2) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title1), size = 1, replace = TRUE) 
    }
    
  }
  else{
    if (yoe %in% c(0:2) & TL == 1) {
      l <- sample(Title1, size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 3) {
      l <- sample(c(Title2,Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(3:8) & TL == 2) {
      l <- sample(Title2, size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 6 ) {
      l <- sample(c(Title3,Title4, Title6), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 4) {
      l <- sample(c(Title3,Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(9:20) & TL == 3) {
      l <- sample(c(Title3), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 6) {
      l <- sample(c(Title6,Title3,Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(21:44) & TL == 4) {
      l <- sample(c(Title3,Title4), size = 1, replace = TRUE)
    }else {
      l <- sample (c(Title3), size = 1, replace = TRUE)
    } 
  }
  return (l);
}


for (i in 1: 973)
  if (Demographic$No_of_titles [i] > 5) {
    Demographic$Previous_Title5[i]  <- Previous5(Demographic$edu[i],Demographic$Year_of_Exp[i],
                                                   Demographic$Previous_Title4[i] )
  }else {
    Demographic$Previous_Title5[i] <- "NA"
  }

Demographic$Title_Level5<- ifelse(Demographic$Previous_Title5 %in% Title6, 6,
                                    ifelse(Demographic$Previous_Title5 %in% Title5, 5,
                                           ifelse(Demographic$Previous_Title5 %in% Title4, 4,
                                                  ifelse(Demographic$Previous_Title5 %in% Title3,3,
                                                         ifelse(Demographic$Previous_Title5 %in% Title2, 2,
                                                                ifelse(Demographic$Previous_Title5 %in% Title1,1,"NA"))))))
table(Demographic$Title_Level5)

Current_Salary <- function(TL,loc){
  if (TL== 6) {
    l <- sample(c(300000:700000), size = 1, replace = TRUE)
  }else if (TL == 5 & loc %in% c("MA","NY","IL","WA","CA")) {
    l <- sample( c(300000:400000), size = 1, replace = TRUE)
  }else if (TL== 5){
    l <- sample( c(250000:310000), size = 1, replace = TRUE)
  }else if (TL == 4 & loc %in% c("MA","NY","IL","WA","CA")) {
    l <- sample( c(200000:250000), size = 1, replace = TRUE)
  }else if (TL== 4){
    l <- sample( c(180000:210000), size = 1, replace = TRUE)
  }else if (TL == 3 & loc %in% c("MA","NY","IL","WA","CA")) {
    l <- sample( c(120000:160000), size = 1, replace = TRUE)
  }else if (TL== 3){
    l <- sample( c(100000:140000), size = 1, replace = TRUE)
  }else if (TL == 2 & loc %in% c("MA","NY","IL","WA","CA")) {
    l<- sample( c(80000:120000), size = 1, replace = TRUE)
  }else if (TL== 2){
    l <- sample( c(50000:70000), size = 1, replace = TRUE)
  }else if (TL == 1 & loc %in% c("MA","NY","IL","WA","CA")) {
    l<- sample( c(35000:50000), size = 1, replace = TRUE)
  }else {
    l <- sample( c(25000:40000), size = 1, replace = TRUE)
  }
  return (l);
}

for (i in 1: 973){
  Demographic$Current_Salary[i]  <- Current_Salary(Demographic$Title_Level[i],Demographic$Location[i]);
}

write.csv(Demographic,file = "CurtisData.csv")

