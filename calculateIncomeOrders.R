setwd("~/Dropbox/AoH/dataMain")
aoh<-read.csv("7roomsFamilies.csv", header=TRUE)
income<-read.csv("adonisEnv.csv", header=TRUE)

housesByOrder <-aoh %>%
  group_by(Order) %>%
  left_join(income, by="houseID")

# Number of events by room type. 554 TOTAL ROOMS (541 excluding no specimen events).
# low 104, lowMed 100, med 147, medHigh 83, high 107
uniqueRooms<-housesByOrder %>%
  filter (familyLong!="No specimens_No specimens") %>%
  filter(incomeClass=="high") %>%
  group_by(roomType) %>%
  summarize(count=n_distinct(roomUnique))
uniqueRooms
sum(uniqueRooms$count)

#combine Isoptera and Blattodea into Dicynoptera
#table of number of MS per room
MSrooms <-aoh%>%
  filter (familyLong!="No specimens_No specimens") %>%
  group_by(roomUnique) %>%
  summarize (MStotal=sum(numMS))
head(MSrooms)

#calculate proportions
lowHouse <- housesByOrder %>%
  filter (familyLong!="No specimens_No specimens") %>%
  filter(incomeClass=="low") %>%
  unite(roomOrder,roomUnique, Order, sep="&") %>%
  group_by(roomOrder) %>%
  summarize (MSroomOrder=sum(numMS)) %>%
  separate(roomOrder, c("roomUnique", "Order"),  sep="&") %>%
  left_join (MSrooms, by="roomUnique") %>%
  mutate (propOrderRoom=MSroomOrder/MStotal) %>%
  group_by (Order) %>%
  summarize (avgProp=(mean(propOrderRoom, na.rm=TRUE)*(n_distinct(roomUnique)))/104) %>%
  arrange(desc(Order))
lowHouse
sum(lowHouse$avgProp)
write.csv(lowHouse, "2low.csv")

lmHouse <- housesByOrder %>%
  filter (familyLong!="No specimens_No specimens") %>%
  filter(incomeClass=="lowMed") %>%
  unite(roomOrder,roomUnique, Order, sep="&") %>%
  group_by(roomOrder) %>%
  summarize (MSroomOrder=sum(numMS)) %>%
  separate(roomOrder, c("roomUnique", "Order"),  sep="&") %>%
  left_join (MSrooms, by="roomUnique") %>%
  mutate (propOrderRoom=MSroomOrder/MStotal) %>%
  group_by (Order) %>%
  summarize (avgProp=(mean(propOrderRoom, na.rm=TRUE)*(n_distinct(roomUnique)))/100) %>%
  arrange(desc(Order))
lmHouse
sum(lmHouse$avgProp)
write.csv(lmHouse, "2lm.csv")


medHouse<- housesByOrder %>%
  filter (familyLong!="No specimens_No specimens") %>%
  filter(incomeClass=="med") %>%
  unite(roomOrder,roomUnique, Order, sep="&") %>%
  group_by(roomOrder) %>%
  summarize (MSroomOrder=sum(numMS)) %>%
  separate(roomOrder, c("roomUnique", "Order"),  sep="&") %>%
  left_join (MSrooms, by="roomUnique") %>%
  mutate (propOrderRoom=MSroomOrder/MStotal) %>%
  group_by (Order) %>%
  summarize (avgProp=(mean(propOrderRoom, na.rm=TRUE)*(n_distinct(roomUnique)))/147) %>%
  arrange(desc(Order))
medHouse
sum(medHouse$avgProp)
write.csv(medHouse, "2med.csv")


mhHouse<- housesByOrder %>%
  filter (familyLong!="No specimens_No specimens") %>%
  filter(incomeClass=="medHigh") %>%
  unite(roomOrder,roomUnique, Order, sep="&") %>%
  group_by(roomOrder) %>%
  summarize (MSroomOrder=sum(numMS)) %>%
  separate(roomOrder, c("roomUnique", "Order"),  sep="&") %>%
  left_join (MSrooms, by="roomUnique") %>%
  mutate (propOrderRoom=MSroomOrder/MStotal) %>%
  group_by (Order) %>%
  summarize (avgProp=(mean(propOrderRoom, na.rm=TRUE)*(n_distinct(roomUnique)))/83) %>%
  arrange(desc(Order))
mhHouse
sum(mhHouse$avgProp)
write.csv(mhHouse, "2mh.csv")


highHouse<- housesByOrder %>%
  filter (familyLong!="No specimens_No specimens") %>%
  filter(incomeClass=="high") %>%
  unite(roomOrder,roomUnique, Order, sep="&") %>%
  group_by(roomOrder) %>%
  summarize (MSroomOrder=sum(numMS)) %>%
  separate(roomOrder, c("roomUnique", "Order"),  sep="&") %>%
  left_join (MSrooms, by="roomUnique") %>%
  mutate (propOrderRoom=MSroomOrder/MStotal) %>%
  group_by (Order) %>%
  summarize (avgProp=(mean(propOrderRoom, na.rm=TRUE)*(n_distinct(roomUnique)))/107) %>%
  arrange(desc(Order))
highHouse
sum(highHouse$avgProp)
write.csv(highHouse, "2high.csv")

orderIncome<- lowHouse %>%
  left_join(lmHouse, by="Order") %>%
  left_join(medHouse, by="Order") %>%
  left_join(mhHouse, by="Order") %>%
  left_join(highHouse, by="Order")
write.csv(orderIncome, "2all.csv")


## Whole other way of doing it.  By family.  And with only 3 income classes
#equivalent of Dune (with morphospecies richness for 50%+ subset per family)
subsetRoom<-read.csv("familiesByRoomMatrixSubset2.csv", header=TRUE)
subsetHouse<-subsetRoom %>%  separate(event, c("event2", "houseNum", "roomCat", "roomRep")) %>%
  unite("event", event2, houseNum, sep=".") %>%
  group_by(event) %>%
  summarize_each(funs(max))
subsetHouse<-subsetHouse[-c(1:3)]
  
  

#equipvalent of dune.env
env<-read.csv("adonisEnv.csv", header=TRUE, row.names=1)
env<-env%>%select(income, sqFeet, numFam)
env<-bind_cols(env,subsetHouse)

#filter size classes: low 17, med 16, high 17
low<-env%>%
  filter(income<74672) %>%
  select(-c(income,sqFeet,numFam)) %>%
  summarize_each(funs(mean))
write.csv(low,"3low.csv")

med<-env%>%
  filter((income>74672)&(income<99743)) %>%
  select(-c(income,sqFeet,numFam)) %>%
  summarize_each(funs(mean))
write.csv(med,"3med.csv")

high<-env %>%
  filter(income>99743) %>%
  select(-c(income,sqFeet,numFam)) %>%
  summarize_each(funs(sd))
write.csv(high,"3high.csv")

low<-env%>%
  filter(income<74672) %>%
  select(-c(income,sqFeet,numFam)) %>%
  summarize_each(funs(mean))
write.csv(low,"3lowsd.csv")

med<-env%>%
  filter((income>74672)&(income<99743)) %>%
  select(-c(income,sqFeet,numFam)) %>%
  summarize_each(funs(sd))
write.csv(med,"3medsd.csv")

high<-env %>%
  filter(income>99743) %>%
  select(-c(income,sqFeet,numFam)) %>%
  summarize_each(funs(sd))
write.csv(high,"3higsdh.csv")

