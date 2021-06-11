library(readr)
SampleSuperstore <- read_csv("SampleSuperstore.csv")
View(SampleSuperstore)


#converting into data-frame:

sam_store=data.frame(SampleSuperstore)
View(sam_store)

#cleaning data set from empty data sets

any(is.na(sam_store)) # checking Na's values

# removing a row in a data frame if data set is empty in that cell
empty_spaces=complete.cases(sam_store)
sam_store1=sam_store[empty_spaces,]
View(sam_store1)

#practice
z=list(4,45,894,123,456,8,53)

for (x in 1:length(z)){
  if(z[x]> y){
    print(z[x])
  }
  else{
    print("hello")
  }
}

# now fair

library(dplyr)

# checking the uniques of the data:
a=sam_store%>%select(1:5,7:9)
View(a)
for (i in seq(1,nrow(a),1)){
  b=unique(a[i])
  print(b)
}
class(sam_store1$Sales) 

library(ggplot2)
ggplot(data=sam_store1,aes(x=Ship.Mode,fill=Segment))+geom_bar()+
  theme(axis.text=element_text(size=7),axis.title=element_text(size=10))+
  ylab("frequency")

table(sam_store1$Ship.Mode)
table(sam_store1$Segment)
table(sam_store1$Category)
table(sam_store1$Sub.Category)

#top 10 highest sales
b=sam_store1%>%top_n(10,sam_store1$Sales)%>%arrange(-Sales)
View(b)

b1=sam_store1%>%top_n(-10,sam_store1$Sales)%>%arrange(-Sales)
View(b1)

Change_col_names=sam_store1%>%mutate(Ship.Mode=recode(Ship.Mode,"Standard Class"="St.Class",
                                      "First Class"="F.Class",
                                      "Second Class"="Sec.Class"))
View(Change_col_names)
# 
ggplot(data=x1,aes(x=Ship.Mode,y=Sales,fill=Segment))+
  geom_bar(stat = "identity")+facet_grid(~Segment)+
  theme(axis.text=element_text(size=7))


#top 10 highest profit
c=sam_store1%>%top_n(10,sam_store1$Profit)%>%arrange(-Profit)
View(c)

#top 10 losses 

d=sam_store1%>%top_n(-10,sam_store1$Profit)%>%arrange(-Profit)
View(d)
ggplot(data=d,aes(x=City,y=Profit,fill=Sales))+
  geom_bar(stat = "identity")+theme(axis.text=element_text(size=7))

# distribution of Sales and Profit
ggplot(data=sam_store1,aes(x=Sales,y=Profit,col=Segment))+
  facet_grid(~Ship.Mode)+
  geom_point()+theme(axis.text=element_text(size=7))

sam_store1$Discount=as.character(sam_store1$Discount)
ggplot(data=sam_store1,aes(x=Sales,y=Profit,col=Discount))+
  facet_grid(~Ship.Mode)+
  geom_point()+theme(axis.text=element_text(size=7))

sam_store1$Discount=as.numeric(sam_store1$Discount)
ggplot(data=sam_store1,aes(x=Sales,y=Profit,col=Sub.Category))+
  facet_grid(~Ship.Mode)+
  geom_point()+theme(axis.text=element_text(size=7))

# distribution of Ship mode and Profit
ggplot(data=sam_store1,aes(x=Ship.Mode,y=Profit,col=Segment))+
  geom_boxplot()

ggplot(data=sam_store1,aes(x=Ship.Mode,y=Profit,col=Category))+
  geom_boxplot()

q1=ggplot(data=sam_store1,aes(x=Category,y=Profit,col=Discount))+
  geom_boxplot()+facet_grid(~Ship.Mode)+
  theme(axis.text=element_text(size=7))

q2=ggplot(data=sam_store1,aes(x=Category,y=Profit,col=Sub.Category))+
  geom_boxplot()+facet_grid(~Ship.Mode)+
  theme(axis.text=element_text(size=7))

library(ggpubr)
ggarrange(q1,q2,ncol=1,nrow=2)



