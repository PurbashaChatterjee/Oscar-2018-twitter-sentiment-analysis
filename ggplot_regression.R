g <- ggplot(long, aes(x=value, fill=variable));
g <- g + geom_histogram(colour ="cyan", binwidth = 1)
g <- g + facet_grid(.~variable)

myHist <- function(){
  mse <- mean((table_final$Score - mu)^2)
  g <- ggplot(placed.df, aes(x=Salary)) + geom_histogram(fill="salmon", 
                                                         colour="black", 
                                                         binwidth = 1)
  g <- g + geom_vline(xintercept = mu, size=3)
  g <- g + ggtitle(paste("mu =",mu,",MSE =", round(mse, 2), sep = ""))
  
}

g <- ggplot(table_final, aes(x=Score)) + geom_histogram(fill="salmon", 
                                                        colour="black", 
                                                        binwidth = 1)
g <- g + geom_vline(xintercept = mean(table_final$Score), size = 3)
g