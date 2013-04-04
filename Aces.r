x <- rep(-1, 24)
Aces <- rep(-8, 4)

insert.aces <- function(x , Aces){
  x[sample(length(x), length(Aces))] <- -Aces
  return(x)
}
cards <- c(1:10, "J", "Q", "K", "A")
deck <- sapply(c("C", "D", "H", "S"), function(x) paste(cards, x, sep="o"))

deshuffle <- function(x){
  x[(1:length(x))%%2 == 0] <- -x[(1:length(x))%%2 == 0]
  return(x)
}

cut <- function(x){
  cutter <- sample(2:(length(x)-1), 1)
  print(cutter)
  return(c(x[cutter:length(x)], x[1:(cutter-1)]))
}

drawflip <- function(x, divisor){
  stops <- which(1:length(x)%%divisor == 0)
  if(length(x)/divisor != length(stops))
    stop ("Divisor is not an actual divisor of the length")
  flipornot <- function(x){
    cat(x,"\n")
    cat("Do you want to flip? (y/n)")
    ans <- readLines(n=1)
    if(ans == "y"){
      cat("OK:", -rev(x),"\n")
      return(-rev(x))
    }
    else if(ans == "n"){
      return(x)
    }
    else{
      cat("!!!! SELECT y OR n !!!!\n\n")
      flipornot(x)
    }
  }
  return(unlist(lapply(stops, function(s) flipornot(x[(s-(divisor-1)):s]))))
}

twopile <- function(x){
  x1 <- x[1:length(x)%%2 == 1]
  x2 <- x[1:length(x)%%2 == 0]
  return(c(-x1, x2))
}

first.step <- function(){
	x <- insert.aces(x, Aces)
	return(x)
}


do_all <- function(){
  x <- rep(-1, 24)
  Aces <- rep(-8, 4)
  x <- insert.aces(x, Aces)
  x <- cut(x)
  x2 <- deshuffle(x)
  x3 <- drawflip(x2, 2)
  x3 <- cut(x3)
  x4 <- drawflip(x3, 4)
  x4 <- cut(x4)
  x5 <- twopile(x4)
  return(x5)
}
