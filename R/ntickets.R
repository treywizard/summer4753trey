#' Number of Tickets function
#'
#' @param N Number of seats on the plane
#' @param gamma percentage of overflow for this flight based on # of seats
#' @param p percentage of passenger not showing up based on # of seats
#'
#' @return list of values
#' @importFrom graphics abline
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom stats qbinom
#' @importFrom stats qnorm
#' @export
#'
#' @examples ntickets(200, 0.02, 0.95)
ntickets <- function(N, gamma, p){
  #x will be used throughout our code to test finding our # of tickets to sell
  x = 0
  #While loop where we keep on going until we find the discrete estimate value
  while(x > -1)
  {
    #Checks to see how many tickets we can sell if p people show up and gamma overflows occur
    if(qbinom((1-gamma), (x + N), p) == N)
    {
      #Defining our number of tickets based on discrete
      nd = N + x
      #Creating our exit from the while loop
      x = -2
    }
    #Incrementing our x value to keep the while loop going
    x = x + 1
  }
  #Setting our x to 0 again.
  x = 0
  #While loop where we keep on going until we find the continous estimate value
  while(x > -1)
  {
    #Check using qnorm for when x + N gives us a value greater than N
    if(qnorm((1 - gamma), mean = (x + N) * p, sd = sqrt((x + N) * p * (1 - p))) > N)
    {
      #Define our continuous value estimate
      nc = N + x
      #Create our exit condition for the while loop
      x = -2
    }
    x = x + 0.01
  }
  print(paste("The number of tickets we should sell using our discrete method is:",nd))
  print(paste("The number of tickets we should sell using our continous method is:",nc))

  #Creates our 2 intervals for our plots to use
  x = (nd-5):(nd+5)
  xx = (nc-5):(nc+5)

  #Plotting the discrete value function
  plot(x, 1 - gamma - pbinom(N, x, p), xlim = c(nd - 5, nd + 5), type = 'l', xlab = "n", ylab = "Objective", main = paste("Objective vs n to find optimal tickets sold(", nd, ") gamma = ", gamma, " N = ", N, "discrete"))
  abline(v = nd)
  abline(h = 0.00)

  #Plotting the continous value function
  plot(xx, 1 - gamma - pnorm(N, mean = xx * p, sd = sqrt(xx * p * (1 - p))), xlim = c(nc - 5, nc + 5), type = 'l', xlab = "n", ylab = "Objective", main = paste("Objective vs n to find optimal tickets sold(", nc, ") gamma = ", gamma, " N = ", N, "continous"))
  abline(v = nc)
  abline(h = 0.00)

  #Creates our output list for all of our values
  list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
}
