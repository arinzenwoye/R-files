x <- -6
ifelse (x > 0, 5, 6)

x <- 0
check <- function(x){
  if (x > 0) {
    result = "positive number"
  } else if (x < 0) {
    result = "negative number"
  } else {
    result = "zero"
  }
  return(result)
}
check(2)

a <- c(5,7,2,9)
ifelse (a %% 2 == 0, "even", "odd")

x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x){
  if (val %% 2 == 0){
    count = count + 1
  }
}
print(count)


i <- 1
while (i < 6){
  print(i)
  i = i + 1
}


x <- 1:5
for (val in x){
  if (val == 3){
    next
  }
  print (val)
}

x <- 1
repeat{
  print(x)
  x = x + 1
  if (x == 6) break
}

pow <- function(x,y){
  #function the print the result of x raised to power y
  result <- x^y
  print(paste(x, "raised to the power", y, "is", result))
}
pow(2,8)

multi_return <- function() {
  my_list <- list("color" = "red", "size" = 20, "shape" = "round")
  return(my_list) 
}
a <- multi_return()
a$color










