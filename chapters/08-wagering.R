## Ryan Elmore
## Gambling Chapter

remotes::install_github("a-i-sports/bettoR")
library(bettoR)

bettoR::implied_odds(0.75, type = "all")
bettoR::hold_calc(-110, -110)
10*1.91 + 10*1.53 -20
convert_odds(-190)
convert_odds(-110)

bettoR::kelly(win_prob = 0.55, odds = -110, type = "us")
bettoR::kelly(win_prob = 0.55, odds = 1.91, type = "dec")

bettoR::kelly(win_prob = 0.54, odds = -110, type = "us")
bettoR::kelly(win_prob = 0.53, odds = -110, type = "us")
bettoR::kelly(win_prob = 0.52, odds = -110, type = "us")

us_to_decimal_odds <- function(us_odds){
  decimal_odds <- ifelse(us_odds <= -100, 
                         -100/us_odds + 1,
                         us_odds/100 + 1)
  return(decimal_odds)
}

CH8convertodds <- function(USodds){
  decimalodds <- USodds
  decimalodds[] <- NA_real_
  decimalodds[which(USodds <= -100)] <- -100 / USodds[which(USodds <= -100)] + 1
  decimalodds[which(USodds >= 100)] <- USodds[which(USodds >= 100)] / 100 + 1
  new_odds <- round(decimalodds, 4)
  return(new_odds)
}

1/(.5^4) 
1/((110/210)^4) 
1/13.28331 

handle <- 50000000 
profit110 <- handle/2 - handle/2 * 10/11
profit105 <- handle/2 - handle/2 * 20/21
profitdifferential <- profit110-profit105
profitdifferentialratio <- profit110/profit105

#calculate handle needed at -105 odds to equal profits at -110 odds

handle105breakeven <- handle * profitdifferentialratio

#calculate the difference in handle needed to equal profits at -110 odds

increasedhandleneeded <- handle105breakeven - handle

#create a vector for output

output <- c(profit110, profit105, profitdifferential, profitdifferentialratio, handle105breakeven, increasedhandleneeded)
#print the results to the terminal
output
[1] 2.272727e+06 1.190476e+06 1.082251e+06 1.909091e+00 9.545455e+07
[6] 4.545455e+07
