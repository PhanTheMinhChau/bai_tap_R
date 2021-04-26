'Câu 1:
a)	12
b)	22
c)	600 '
'Câu 2:'
#1 (c): 
answer <- 10
answer <- prod(3:5) * answer
'Câu 3:'
#C1:
answer <- 0
for (j in 1:100){ answer <- j+answer }
#C2:
answer <- sum(1:100)
'Câu 4:'
#C1:
answer <- 1
for (j in 1:50){ answer <- j*answer }
#C2:
answer <- 1
answer <- prod(1:50)
'Câu 5:'
r <- 3:20
volume <- (4/3)*(r**3)
conversion <- data.frame(r=r, volume=volume)
print(conversion)
'Câu 6:'
index <- sapply(conversion,is.factor)
index
