library(generator)
Demographic <-  data.frame(ID= integer (50000),
                           Name=character(50000),
                           Date_of_birth =(character(50000)),
                           Age = integer(50000),
                           gender = (character(50000)),
                           Race = (character(50000)),
                           Location= (character(50000)),
                           country = (character(50000)),
                           stringsAsFactors=FALSE)  

                      
 
Demographic$Name <- r_full_names(50000)
                  
unique(Demographic$Name)

Demographic$Date_of_birth <-as.Date(sample.int(as.Date("1995-01-01") - as.Date("1953-01-01"), size =50000,replace = TRUE), origin = as.Date("1953-01-01"))
Demographic$age <- as.integer((Sys.Date()-Demographic$Date_of_birth)/365.25)
Demographic$Age <- NULL

Demographic$Race <- sample (c ("Asian","African American", "Caucasian", "Native American","Hispanic"), 50000, replace = TRUE)
US<- data(statepov)
Demographic$Location <-sample (unique(statepop$abbr), 50000, replace = TRUE)
 
Demographic$gender <-sample (c("Female","Male"), 50000, replace = TRUE)
Demographic$country <- "USA"

Demographic$ID<- 1:nrow(Demographic)

DemographicED <- Demographic
DemographicED$edu<- sample(c("Undergraduate", "Graduate", "Doctorate","Higschool", "Associate"), size = 50000, replace = TRUE)
table(DemographicED$edu)
DemographicED$edu<-ifelse(DemographicED$age<= 33 & DemographicED$edu =="Doctorate", 
                          sample(c("Undergraduate", "Graduate","Higschool", "Associate"),size= 50000), DemographicED$edu)
DemographicED[(DemographicED$age < 33 & DemographicED$edu == "Doctorate"), ]

DemographicED$No_Connection <-sample(c(300:500), size = 50000, replace = TRUE)
table(DemographicED$No_Connection)

value_yoe <- function(age, edu){
  
  if(edu=="Undergraduate"){
    l <- (age - 22)
  }else if(edu=="Graduate"){
    l <- (age - 24)
  }else if(edu=="Highschool"){
      l <- (age- 18)
  }else if(edu=="Associate"){
      l <- (age- 20)
  }else {
    l <- (age - 33)
  }
    return(l);
}

for(i in 1:50000)
{
  DemographicED$Year_of_Exp[i] <- value_yoe(DemographicED$age[i],DemographicED$edu[i]);
}
table(DemographicED$Year_of_Exp)

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

for(i in 1:50000)
{
  DemographicED$No_of_Job_changed[i] <- value_Job_change(DemographicED$Year_of_Exp[i]);
}
table(DemographicED$No_of_Job_changed)
# Normalized Titles based on Watson Wyatt Global Grades.
Titles <- Title1

Title1 <- unique(grep("intern|worker|trainee|operator", Titles$Job_Title,ignore.case=TRUE,value=TRUE)) 
Title1<- Title1[-c(grep("Intern Software Engineer|Internship|Business Analyst Intern|Software intern|
Software Engineer Intern|Research intern|Intern Software Engineer|Product Management Intern", 
                        Title1))]
Title2 <- unique(grep("Technician|Assistant|Clerk|supervisor",Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title2 <- Title2[-c(3,5,7)]
Title3 <- unique(grep("Junior Developer|Staff Engineer|Analyst|Business Manager|System administration|Advisory Engineer|Team lead|Technical Writer",
               Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title3 <- Title3[-c(grep("Sr.|Intern|Senior", Title3))]
Title3<- Title3[-c(grep("developer", Title3))]
Title3 <- Title3[-c(grep("Systems Analyst",Title3))]

Title4 <- unique(grep("Senior Consultant|Architect|Senior Manager|Senior Developers|Senior advisor|General manager|Sr|Senior",
               Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title4 <- Title4[-c(grep("Senior engineer|Senior SW Engineer|Senior Engineer (Band 9)|sr. software engineer
                         |Sr Enginner|senior developer|senior software dev|Senior software Engineer
                         |Sr. Developer|Senior Developer|Senior Software engineer|Architect|Senior software developer
|Senior Developer|Senior Software Developer|Sr Software Engineer|sr engineer|systems architect|Sr systems analyst
                         |Director of Technology / Sr. Lead Developer|Sr Engineer|sr. software engineer|Sr. Software Engineer
                         |Sr Developer|Senior Engineer|Sr DevOps|Sr systems analyst|Sr. Systems Analyst|Sr Systems Engineer|
                         Sr. Director of Program Management|Sr DevOps|General Manager", Title4))]
Title5 <- unique(grep("Executive manager|Vice President|Director|Managing director",
               Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title6 <- unique(grep("CEO|CTO|founder|cofounder",Titles$Job_Title,ignore.case=TRUE,value=TRUE))
Title6 <- Title6[-c(grep("Director|Manager|Inspector|contractor|Engineer|Developer", Title6))]
Title6<- Title6[-c(grep("Director|Manager|Inspector|contractor|Engineer|Developer|Cto|CTO / Cofounder|CEO (Co-Founder)|
               founder, CTO|CTO Google rail product|Consultant, CTO as needed|Co-founder", Title6))]
Title6<- Title6[-c(grep("founder, CTO|CEO (Co-Founder)", Title6))] 

table(DemographicED$Year_of_Exp)
table(DemographicED$edu[DemographicED$Year_of_Exp %in% c(-2)])
max(DemographicED$Year_of_Exp)

# Assigning No of Titles.
DemographicED$No_of_titles <- ifelse(DemographicED$Year_of_Exp < 4 , 1, 
                                     ifelse(DemographicED$Year_of_Exp %in% c(5:8),c(1:3),
                                            ifelse(DemographicED$Year_of_Exp %in% c(9:20), c(3:5),
                                                   ifelse(DemographicED$Year_of_Exp %in% c(21:25),c(4:5),c(4:6)
                                                           ))))



DemographicED$No_of_titles[DemographicED$Year_of_Exp %in% c(3:7)]
#Assigning Current Titles - 

Current <- function(edu, yoe){
  if(edu %in% c ("Undergradute","Graduate")){
    if(yoe %in% c(0-2)) {
      m <- sample(c(Title1,Title2), size=1, replace = TRUE)
    }else if(yoe %in% c(3:8)){
      m <- sample(c(Title2,Title3,Title4,Title6), size=1, replace = TRUE)
    }else if(yoe %in% c(9:20)) {
      m <- sample(c(Title4,Title5,Title6), size=1, replace =TRUE)
    }else {
      m <- sample(c(Title4,Title5,Title6), size=1, replace =TRUE)
    }
  }
  else if(edu %in% c ("Highschool","Associate")){
    if(yoe %in% c(0-2)) {
      m <- sample(c(Title1,Title2), size=1, replace = TRUE)
    }else if(yoe %in% c(3:8)) {
      m <- sample(c(Title1,Title2,Title3), size=1, replace = TRUE)
    }else if(yoe %in% c(9:20)) {
      m <- sample(c(Title3,Title4,Title6), size=1, replace =TRUE)
    }else {
      m <- sample(c(Title3,Title4,Title6), size=1, replace =TRUE)
    }
  } 
  else {
    if(yoe %in% c(0-2)) {
      m <- sample(c(Title1,Title2), size=1, replace = TRUE)
    }else if(yoe %in% c(3:8)) {
      m <- sample(c(Title3,Title4,Title6), size=1, replace = TRUE)
    }else if(yoe %in% c(9:20)) {
      m <- sample(c(Title4,Title5,Title6), size=1, replace =TRUE)
    }else {
      m <- sample(c(Title4,Title5,Title6), size=1, replace =TRUE)
    }
  }
  return(m);
}

for (i in 1:973)
{
  Demographic1$Current_Title[i] <- Current(Demographic1$edu[i],Demographic1$Year_of_Exp[i]);
}
table(DemographicED$Current_Title)
# Assigning Title Level 

DemographicED$Title_Level<- ifelse(DemographicED$Current_Title %in% Title6, 6,
       ifelse(DemographicED$Current_Title %in% Title5, 5,
              ifelse(DemographicED$Current_Title %in% Title4, 4,
                     ifelse(DemographicED$Current_Title %in% Title3,3,
                            ifelse(DemographicED$Current_Title %in% Title2, 2,1)))))
table(DemographicED$Title_Level)

# Previous 1 Title and title level   Assignment 
Previous1<- function(edu, yoe,TL) {
   if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(25:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size =1, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size =1, replace = TRUE)
      }else if (yoe %in% c(25:48) & TL == 4) {
        l <- sample(Title4, size =1, replace = TRUE)
        }else if (yoe %in% c(8:24) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =1, replace = TRUE)
      }else if (yoe %in% c(8:24) & TL == 4) {
        l <- sample(c(Title3,Title4), size =1, replace = TRUE)
        }else if (yoe %in% c(8:24) & TL ==3) {
          l <- sample(c(Title3), size = 1, replace = TRUE)
          }else if (yoe %in% c(2:7)) {
      l <- sample(Title3, size = 1,replace = TRUE)
    }else {
      l <- sample (Title1, size = 1, replace = TRUE)
    } 
 } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(8:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 3) {
      l <- sample(Title3, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 4) {
      l <- sample(c(Title3, Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 5) {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)
    }else {
    l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)    
      }
  } 
  else if (edu %in% c ("Highschool","Associate")) {
      if (yoe %in% c(25:44) & TL == 4) {
        l <- sample(c(Title2,Title3,Title4), size = 1, replace = TRUE)
      }else if (yoe %in% c(25:44) & TL == 3) {
        l <- sample(c(Title2,Title3), size = 1, replace = TRUE)
      }else if (yoe %in% c(25:44) & TL == 2) {
        l <- sample(Title2, size = 1, replace = TRUE)
      }else if (yoe %in% c(8:24) & TL == 3 ) {
        l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
      }else if (yoe %in% c(8:24) & TL == 2) {
        l <- sample(c(Title3,Title2), size = 1, replace = TRUE)
      }else if (yoe %in% c(4:7) & TL == 2) {
        l <- sample(c(Title2), size = 1, replace = TRUE)
    }else {
        l <- sample (Title1, size = 1, replace = TRUE)
      } 
    } 
  return (l);
  } 


for (i in 1: 50000)
  if (DemographicED$No_of_titles [i] > 1) {
    DemographicED$Previous_Title1[i]  <- Previous1 (DemographicED$edu[i],DemographicED$Year_of_Exp[i],DemographicED$Title_Level[i] )
  }else {
    DemographicED$Previous_Title1[i] <- "NA"
  }

DemographicED$Title_Level1<- ifelse(DemographicED$Previous_Title1 %in% Title6, 6,
                                   ifelse(DemographicED$Previous_Title1 %in% Title5, 5,
                                          ifelse(DemographicED$Previous_Title1 %in% Title4, 4,
                                                 ifelse(DemographicED$Previous_Title1 %in% Title3,3,
                                                        ifelse(DemographicED$Previous_Title1 %in% Title2, 2,
                                                               ifelse(DemographicED$Previous_Title1 %in% Title1,1,"NA"))))))


table(DemographicED$Previous_Titlel1)
table(DemographicED$edu)


Previous2<-function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(25:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 4) {
      l <- sample(Title4, size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 4) {
      l <- sample(c(Title3,Title4), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL ==3) {
      l <- sample(c(Title3), size = 50000, replace = TRUE)
    }else if (yoe %in% c(2:7)) {
      l <- sample(Title3, size = 50000,replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(8:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 3) {
      l <- sample(Title3, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 4) {
      l <- sample(c(Title3, Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 5) {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)    
    }
  } 
  else if (edu %in% c ("Highschool","Associate")) {
    if (yoe %in% c(25:44) & TL == 4) {
      l <- sample(c(Title2,Title3,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 3) {
      l <- sample(c(Title2,Title3), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 2) {
      l <- sample(Title2, size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 3 ) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 2) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(4:7) & TL == 2) {
      l <- sample(c(Title2), size = 50000, replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  return (l);
} 
for (i in 1: 50000)
  if (DemographicED$No_of_titles [i] > 2) {
    DemographicED$Previous_Title2[i]  <- Previous2 (DemographicED$edu[i],DemographicED$Year_of_Exp[i],
                                                   DemographicED$Previous_Title1[i] )
  }else {
    DemographicED$Previous_Title2[i] <- "NA"
  }

DemographicED$Title_Level2<- ifelse(DemographicED$Previous_Title2 %in% Title6, 6,
                                    ifelse(DemographicED$Previous_Title2 %in% Title5, 5,
                                           ifelse(DemographicED$Previous_Title2 %in% Title4, 4,
                                                  ifelse(DemographicED$Previous_Title2 %in% Title3,3,
                                                         ifelse(DemographicED$Previous_Title2 %in% Title2, 2,
                                                                ifelse(DemographicED$Previous_Title2 %in% Title1,1,"NA"))))))

table(DemographicED$Previous_Title2)
table(DemographicED$Title_Level2)


Previous3<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(25:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 4) {
      l <- sample(Title4, size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 4) {
      l <- sample(c(Title3,Title4), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL ==3) {
      l <- sample(c(Title3), size = 50000, replace = TRUE)
    }else if (yoe %in% c(2:7)) {
      l <- sample(Title3, size = 50000,replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(8:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 3) {
      l <- sample(Title3, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 4) {
      l <- sample(c(Title3, Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 5) {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)    
    }
  } 
  else if (edu %in% c ("Highschool","Associate")) {
    if (yoe %in% c(25:44) & TL == 4) {
      l <- sample(c(Title2,Title3,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 3) {
      l <- sample(c(Title2,Title3), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 2) {
      l <- sample(Title2, size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 3 ) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 2) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(4:7) & TL == 2) {
      l <- sample(c(Title2), size = 50000, replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  return (l);
} 

for (i in 1: 50000)
  if (DemographicED$No_of_titles [i] > 3) {
    DemographicED$Previous_Title3[i]  <- Previous3 (DemographicED$edu[i],DemographicED$Year_of_Exp[i],
                                                   DemographicED$Previous_Title2[i] )
  }else {
    DemographicED$Previous_Title3[i] <- "NA"
  }

DemographicED$Title_Level3<- ifelse(DemographicED$Previous_Title3 %in% Title6, 6,
                                    ifelse(DemographicED$Previous_Title3 %in% Title5, 5,
                                           ifelse(DemographicED$Previous_Title3 %in% Title4, 4,
                                                  ifelse(DemographicED$Previous_Title3 %in% Title3,3,
                                                         ifelse(DemographicED$Previous_Title3 %in% Title2, 2,
                                                                ifelse(DemographicED$Previous_Title3 %in% Title1,1,"NA"))))))
table(DemographicED$Title_Level3)

Previous4<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(25:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 4) {
      l <- sample(Title4, size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 4) {
      l <- sample(c(Title3,Title4), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL ==3) {
      l <- sample(c(Title3), size = 50000, replace = TRUE)
    }else if (yoe %in% c(2:7)) {
      l <- sample(Title3, size = 50000,replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(8:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 3) {
      l <- sample(Title3, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 4) {
      l <- sample(c(Title3, Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 5) {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)    
    }
  } 
  else if (edu %in% c ("Highschool","Associate")) {
    if (yoe %in% c(25:44) & TL == 4) {
      l <- sample(c(Title2,Title3,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 3) {
      l <- sample(c(Title2,Title3), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 2) {
      l <- sample(Title2, size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 3 ) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 2) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(4:7) & TL == 2) {
      l <- sample(c(Title2), size = 50000, replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  return (l);
} 

for (i in 1: 50000)
  if (DemographicED$No_of_titles [i] > 4) {
    DemographicED$Previous_Title4[i]  <- Previous4(DemographicED$edu[i],DemographicED$Year_of_Exp[i],
                                                   DemographicED$Previous_Title3[i] )
  }else {
    DemographicED$Previous_Title4[i] <- "NA"
  }

DemographicED$Title_Level4<- ifelse(DemographicED$Previous_Title4 %in% Title6, 6,
                                    ifelse(DemographicED$Previous_Title4 %in% Title5, 5,
                                           ifelse(DemographicED$Previous_Title4 %in% Title4, 4,
                                                  ifelse(DemographicED$Previous_Title4 %in% Title3,3,
                                                         ifelse(DemographicED$Previous_Title4 %in% Title2, 2,
                                                                ifelse(DemographicED$Previous_Title4 %in% Title1,1,"NA"))))))

table(DemographicED$Title_Level4)

Previous5<- function(edu, yoe,TL) {
  if (edu %in% c ("Undergradute","Graduate")) {
    if (yoe %in% c(25:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:48) & TL == 4) {
      l <- sample(Title4, size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 6) {
      l <- sample(c(Title3,Title4,Title5,Title6), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 5) {
      l <- sample(c(Title3,Title4,Title5), size =50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 4) {
      l <- sample(c(Title3,Title4), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL ==3) {
      l <- sample(c(Title3), size = 50000, replace = TRUE)
    }else if (yoe %in% c(2:7)) {
      l <- sample(Title3, size = 50000,replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  else if ( edu =="Doctorate") {
    if (yoe %in% c(8:48) & TL == 6) {
      l <- sample(c(Title4,Title5,Title6), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 5) {
      l <- sample(c(Title5,Title4), size = 1, replace = TRUE)
    }else if(yoe %in% c(8:48) & TL == 4) {
      l <- sample(Title4, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 3) {
      l <- sample(Title3, size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 4) {
      l <- sample(c(Title3, Title4), size = 1, replace = TRUE)
    }else if (yoe %in% c(2:7) & TL == 5) {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)
    }else {
      l <- sample(c(Title3, Title4, Title5, Title6), size = 1, replace = TRUE)    
    }
  } 
  else if (edu %in% c ("Highschool","Associate")) {
    if (yoe %in% c(25:44) & TL == 4) {
      l <- sample(c(Title2,Title3,Title4), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 3) {
      l <- sample(c(Title2,Title3), size =50000, replace = TRUE)
    }else if (yoe %in% c(25:44) & TL == 2) {
      l <- sample(Title2, size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 3 ) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(8:24) & TL == 2) {
      l <- sample(c(Title3,Title2), size = 50000, replace = TRUE)
    }else if (yoe %in% c(4:7) & TL == 2) {
      l <- sample(c(Title2), size = 50000, replace = TRUE)
    }else {
      l <- sample (Title1, size = 50000, replace = TRUE)
    } 
  } 
  return (l);
} 


for (i in 1: 50000)
  if (DemographicED$No_of_titles [i] > 4) {
    DemographicED$Previous_Title5[i]  <- Previous5(DemographicED$edu[i],DemographicED$Year_of_Exp[i],
                                                   DemographicED$Previous_Title4[i] )
  }else {
    DemographicED$Previous_Title5[i] <- "NA"
  }

DemographicED$Title_Level5<- ifelse(DemographicED$Previous_Title5 %in% Title6, 6,
                                    ifelse(DemographicED$Previous_Title5 %in% Title5, 5,
                                           ifelse(DemographicED$Previous_Title5 %in% Title4, 4,
                                                  ifelse(DemographicED$Previous_Title5 %in% Title3,3,
                                                         ifelse(DemographicED$Previous_Title5 %in% Title2, 2,
                                                                ifelse(DemographicED$Previous_Title5 %in% Title1,1,"NA"))))))
table(DemographicED$Title_Level5)

Current_Salary <- function(TL,loc){
  if (TL==6) {
    l <- sample(c(600000:700000), size = 1, replace = TRUE)
  }else if (TL == 5 & loc %in% c("MA","NY","IL","WA","CA")) {
    l <- sample( c(350000:400000), size = 1, replace = TRUE)
  }else if (TL== 5){
    l <- sample( c(300000:350000), size = 1, replace = TRUE)
  }else if (TL == 4 & loc %in% c("MA","NY","IL","WA","CA")) {
    l <- sample( c(240000:300000), size = 1, replace = TRUE)
  }else if (TL== 4){
    l <- sample( c(180000:250000), size = 1, replace = TRUE)
  }else if (TL == 3 & loc %in% c("MA","NY","IL","WA","CA")) {
    l <- sample( c(120000:160000), size = 1, replace = TRUE)
  }else if (TL== 3){
    l <- sample( c(100000:140000), size = 1, replace = TRUE)
  }else if (TL == 2 & loc %in% c("MA","NY","IL","WA","CA")) {
    l<- sample( c(80000:120000), size = 1, replace = TRUE)
  }else if (TL== 2){
    l <- sample( c(50000:100000), size = 1, replace = TRUE)
  }else if (TL == 1 & loc %in% c("MA","NY","IL","WA","CA")) {
    l<- sample( c(50000:80000), size = 1, replace = TRUE)
  }else {
    l <- sample( c(25000:50000), size = 1, replace = TRUE)
  }
  return (l);
}

for (i in 1: 50000){
  DemographicED$Current_Salary[i]  <- Current_Salary(DemographicED$Title_Level[i],DemographicED$Location[i]);
}
Demographic$Fortune500 <- sample(c(0,1),size = 973, replace = TRUE)

CurtisSkiEndo<- as.data.frame(join(Curtisdata,CurtisSkill))
write.csv (CurtisSkiEndo, file = "CurtisSkiEndo.csv")

# Success Score
Demographic$SuccessScore <- (sum(Demographic$Title_Level+(Demographic$Current_Salary/1000)+Demographic$Fortune500))/Demographic$Age
