x <- rep(-1, 24)
Aces <- rep(-8, 4)

insert.aces <- function(x , Aces){
  x[, sample(ncol(x), ncol(Aces))] <- Aces[2:1, ]
  cat(x[2, ], "\n")
  return(x)
}
cards <- c(1:10, "J", "Q", "K", "A")
deck <- sapply(c("C", "D", "H", "S"), function(x) paste(cards, x, sep="o"))

x <- sample(deck[-14, ], 20)
x <- rbind(x, ":3")
Aces <- deck[14, ]
Aces <- rbind(Aces, ":3")

deshuffle <- function(x){
  x[, (1:ncol(x))%%2 == 0] <- rev(x[, (1:ncol(x))%%2 == 0])
  cat(x[2, ], "\n")
  return(x)
}

cut <- function(x){
  cutter <- sample(2:(ncol(x)-1), 1)
  x <- cbind(x[, cutter:ncol(x)], x[, 1:(cutter-1)])
  cat("Cut at",cutter, ">>>\n", x, "\n")
  return(x)
}

drawflip <- function(x, divisor){
  stops <- which(1:ncol(x)%%divisor == 0)
  if(ncol(x)/divisor != length(stops))
    stop ("Divisor is not an actual divisor of the length")
  flipornot <- function(x){
    cat(x[2, ],"\n")
    cat("Do you want to flip? (y/n)")
    ans <- readLines(n=1)
    if(ans == "y"){
      cat("OK:", rev(x[1, ]),"\n")
      x <- rbind(rev(x[2, ]), rev(x[1, ]))
      return(x)
    }
    else if(ans == "n"){
      return(x)
    }
    else{
      cat("!!!! SELECT y OR n !!!!\n\n")
      flipornot(x)
    }
  }
  newx <- x
  lapply(stops, function(s) newx[, (s-(divisor-1)):s] <<- flipornot(x[, (s-(divisor-1)):s]))
  cat("\n\nVoila!\n", newx,"\n")
  return(newx)
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

