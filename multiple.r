library(mice)
library(ggplot2)





data1 = read.csv("Life Expectancy Data.csv")
dim(data1)
head(data1)
#summary(data1)
str(data1)
colnames(data1) = c("country","year","status","life.expect",
                    "adult.mor","infant.mor", "alcohol",
                    "percent.expend","H.B", "measles","BMI","under5mor",
                    "polio","tot.expend","diphtheria","HIV","GDP",
                    "population","thin.10.19","thin.5.9", "inc.resource",
                    "school")



str(data1)
#summary(data1)

colSums(is.na(data1))
sum(is.na(data1)) #2563 missing values

data = data1[-which(is.na(data1$life.expect)),]
dim(data)
sum(is.na(data))

data$adult.mor = as.numeric(data$adult.mor)
data$infant.mor  = as.numeric(data$infant.mor)
data$measles  = as.numeric(data$measles)
data$under5mor  = as.numeric(data$under5mor)
data$H.B = as.numeric(data$H.B)
data$diphtheria = as.numeric(data$diphtheria)
data$polio = as.numeric(data$polio) 

#summary(data)

data.num = data[,-c(1,2,3)]
dim(data.num)

developed = data[which(data$status == "Developed"),]
dim(developed)
developing = data[which(data$status == "Developing"),]
dim(developing)


data.impute.developed = mice(developed, method = "mean", seed = 1)
data.developed = complete(data.impute.developed)
sum(is.na(data.developed))
dim(data.developed)
data.impute.developing = mice(developing, method = "mean", seed = 1)
data.developing = complete(data.impute.developing)
sum(is.na(data.developing))
dim(data.developing)


developed.num = data.developed[,-c(1,2,3)]
developing.num = data.developing[,-c(1,2,3)]
final.data=rbind(data.developed,data.developing)












train = subset(final.data, year<2012)
test = subset(final.data, year>2011)
lm.train  = train
lm.test  = test
dim(test)
dim(train)

lm1 = lm(life.expect~year+adult.mor+infant.mor+alcohol+
            percent.expend+H.B+measles+BMI+under5mor+polio+tot.expend+
            diphtheria+HIV+GDP+population+thin.10.19+thin.5.9+inc.resource+
            school, data = lm.train)
# summary(lm1)

#subset
mydata_subset=subset(lm.train,select=c(life.expect,
   year,adult.mor,infant.mor,percent.expend,H.B,
   BMI,polio,tot.expend,diphtheria,HIV,inc.resource,school ))




lm2 = lm(life.expect~year+adult.mor+infant.mor+percent.expend+H.B+
           BMI+polio+tot.expend+diphtheria+HIV+inc.resource+school, 
         data = mydata_subset)
#summary(lm2)




print("##################################################################################")
dim(mydata_subset)
summary(mydata_subset)



#=========================================================================================#
# PLOT1
# plot1=qplot(life.expect, data = mydata_subset, geom="density", main="Density plot of life.expect")

# ggsave("plot1.pdf")


#Plot2
#  plot2=qplot(sample=life.expect, data = mydata_subset, main="QQ Plot(Life.expect)")
# ggsave("plot2.pdf")



#Final Plot

# qplot(fitted.values(lm2),life.expect, data = mydata_subset, main = "QQ Plot(Price)")+geom_abline(intercept=0,slope=1,color="red")

# ggsave("Final_Plot.pdf")



#=========================================================================================#
pre.data=predict(lm2,newdata=test)


print("***************************")
print(dim(test))
print(str(pre.data))
coefficients(lm1)
