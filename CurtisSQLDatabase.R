conCurtis <-dbConnect(MySQL(), user = 'root', password = 'aammrg11', host = 'localhost', dbname= 'Curtis')

Demo <- as.data.frame(CDE3)

#Writing Demo table in SQL 
dbWriteTable(conCurtis, value =Demo, name = "DEMO",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
#Creating Master Title table for Database purposes
MasterTitle <- as.list(paste(c(Title1,Title2,Title3,Title4,Title5,Title6),collapse = NULL))
Mastertitle1<- as.data.frame(t(unique(MasterTitle)))
Mastertitle1$Title_ID <- NULL
Mastertitle1<- t(Mastertitle1)
Mastertitle1<- as.data.frame(Mastertitle1)
Mastertitle1<- colnames(Mastertitle1) <- c("Title")
Mastertitle1$Title_ID <- 1:nrow(Mastertitle1)
Mastertitle1$TL <- ifelse(Mastertitle1$Title %in% Title6, 6,
                          ifelse(Mastertitle1$Title %in% Title5, 5,
                                 ifelse(Mastertitle1$Title %in% Title4, 4,
                                        ifelse(Mastertitle1$Title %in% Title3, 3,
                                               ifelse(Mastertitle1$Title %in% Title2,2,1)))))
colnames(Mastertitle2) <- c("Title","Title_ID","TL")
write.csv(Mastertitle2, file="Mastertitle.csv")

# Writing Title Table in SQL
dbWriteTable(conCurtis, value =Mastertitle, name = "TitleMain",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )

# Writing Endorsement table
dbWriteTable(conCurtis, value =Endorcer1, name = "Endorcer1",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
dbWriteTable(conCurtis, value =EndorSkill11, name = "ES1",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
dbWriteTable(conCurtis, value =Endorcer2, name = "Endorcer2",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
dbWriteTable(conCurtis, value =EndorSkill21, name = "ES2",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
dbWriteTable(conCurtis, value =Endorcer3, name = "Endorcer3",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
dbWriteTable(conCurtis, value =EndorSkill31, name = "ES3",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )

#Creating a table to join endorsement group tp primary table 
merge(Demo.df,Endorcer1)

#Writing Skill table in sql
dbWriteTable(conCurtis, value =Curtis_Skill2, name = "CSkill2",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )
dbWriteTable(conCurtis, value =Skill, name = "SkillMain",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE, append = TRUE )


#Writing queries 
CE = dbSendQuery(conCurtis,"Select distinct * from Curtis.Endorcer1" )

EndorPerID1<- dbReadTable(conn = conCurtis, name = 'EndorPerID1')
CurtisEE4 <- join(CurtisEE3, EndorPerID1)

Endorcer1_Endorsements<- dbReadTable(conn = conCurtis, name = 'endor1_perendor1id')
CurtisEE5<- merge(CurtisEE4,Endorcer1_Endorsements, by.x = "Endorcer1_ID", by.y = "Endorcer1_ID" )




CommonENDO1_2<- dbReadTable(conn = conCurtis, name = 'comendo1_2')
all_cons<- dbListConnections(MySQL())
dbDisconnect(conCurtis)


corrplot()














