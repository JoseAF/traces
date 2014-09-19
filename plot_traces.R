## Reads data from .txt files in a given directory and returns averaged data from the files:
##     Data provided in 5 columns: 
##            col 1: Time (ms)
##            col 2: Current (pA) with prepulse to -100 mV
##            col 3: Current (pA) with prepulse to -40 mV
##            col 4: Voltage (mV) for trace 1 in protocol (prepulse to -100 mV)
##            col 5: Voltage (mV) for trace 2 in protocol (prepulse to -40 mV)
##     Returned data given as matrix in 3 columns:
##            col 1: Time (ms) for averaged data
##            col 2: Averaged current (pA) for all traces 1 over all .txt files
##            col 3: Averaged current (pA) for all traces 2 over all .txt files
aver_traces <- function(dir){
       old_wd <- getwd()
       setwd(dir)
       m2 <- numeric()
       m3 <- numeric()
       for (file in list.files(dir)){
              if (!(file.info(file)$isdir) && (strsplit(file, '\\.')[[1]][2] == 'txt')){
                     all <- read.delim(file)
                     m2 <- cbind(m2, all[,2])
                     m3 <- cbind(m3, all[,3])
              }
       }
       setwd(old_wd)
       cbind(all[,1], rowMeans(m2), rowMeans(m3))
}

plot_aver_diff <- function(dir=getwd()) {
       av <- aver_traces(dir)
       
       diff <- data.frame(av[,1],av[,2]-av[,3])
       
       x_init = 23000
       x_end = 30000
       
       plot(diff[x_init:x_end,],type='l',xlab='Time (ms)',ylab='Current (pA)',
            col = 'black')
       
       abline(h = 0, lty = 'dashed', col = "black")
}

## Plots the averaged data provided by aver_traces function (above). 
##     Black curve: Averaged currents (pA) for -100 mV prepulse data (traces 1)
##     Red curve: Averaged currents (pA) for -40 mV prepulse data (traces 2)
## It also includes an inset giving the zoomed currents at the pulse to +10 mV.
plot_aver_traces <- function(dir=getwd(), y_min=-Inf, y_max=Inf, ys_min=-Inf, ys_max=Inf, prepulse) {
       av <- aver_traces(dir)
       
       df1 <- data.frame(av[,1],av[,2])
       df2 <- data.frame(av[,1],av[,3])
       
       step_borders <- 1.2
       
       x_min <- min(av[,1])
       x_max <- max(av[,1])
       x_delta <- (x_max - x_min) / 5
       
       if (y_min==-Inf) y_min <- min(min(av[,2]),min(av[,3]))
       if (y_max==Inf) y_max <- max(max(av[,2]),max(av[,3]))
       y_delta <- (y_max - y_min) / 5
       
       if (ys_min==-Inf) ys_min <- -500 
       if (ys_max==Inf) ys_max <- 500
       
       plot(df1,type='l',xlab='Time (ms)',ylab='Current (pA)',
              ylim=c(y_min, y_max)) 
       lines(df2,col='red')
       
       if (prepulse=='long'){
              pp_min <- 53000
              pp_max <- 59000
       }
       else if (prepulse=='short'){
              pp_min <- 22500
              pp_max <- 29500
       }
       my_subplot <- function() { 
              plot(df1[pp_min:pp_max,],type='l',xlab="",ylab="",
                   ylim=c(ys_min,ys_max))
              lines(df2,col='red')
       }
       
       if (prepulse=='long')
              subplot(my_subplot(),x_min+step_borders*x_delta,
                     y_max-step_borders*y_delta,size=c(3,2))
       else
              subplot(my_subplot(),x_min+3.3*step_borders*x_delta,
                      y_max-step_borders*y_delta,size=c(3,2))
}

## Other functions for rough plotting of individual traces from a .txt file
plot_traces1 <- function(file="") {
       all <- read.delim(file)
         
       df1 <- data.frame(all[1],all[2])
       df2 <- data.frame(all[1],all[3])
         
       plot(df1,type='l',xlab='Time (ms)',ylab='Current (pA)',ylim=c(-200,200))
       lines(df2,col='red')
  
       my_subplot <- function() { 
              plot(df1[53000:57000,],type='l',xlab="",ylab="",ylim=c(-50,100))
              lines(df2,col='red')
       }

       subplot(my_subplot(),300,120,size=c(3,2))
  
}

plot_traces2 <- function(file="") {
       all <- read.delim(file)
  
       df1 <- data.frame(all[1],all[2])
       df2 <- data.frame(all[1],all[3])
  
       plot(df1,type='l',xlab='Time (ms)',ylab='Current (pA)',ylim=c(-200,200))
       lines(df2,col='red')
  
       my_subplot <- function() { 
              plot(df1[22500:26500,],type='l',xlab="",ylab="",ylim=c(-50,100))
              lines(df2,col='red')
       }
  
       subplot(my_subplot(),400,-90,size=c(3,2))
  
}
