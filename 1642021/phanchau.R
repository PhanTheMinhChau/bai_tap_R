setwd("C:\\Users\\DELL\\Desktop\\r\\1642021")
#cau 1
data <- read.csv("Temperature.csv")
tapply(data$Temperature,list(data$Month,data$Year), mean,na.rm = TRUE) #gia tri trung binh
tapply(data$Temperature,list(data$Month,data$Year), sd,na.rm = TRUE) #standard deviation
tapply(data$Temperature,list(data$Month,data$Year), length) #so quan sat
#cau 2
table(data$Year) #so quan sat tren 1 nam
table(list(data$Year,data$Station)) #so quan sat tren 1 nam/tram