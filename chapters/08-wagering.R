## Ryan Elmore
## Gambling Chapter

remotes::install_github("a-i-sports/bettoR")
library(bettoR)
library(magrittr)
library(scales)

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

profitdifferential <- profit110 - profit105
profitdifferentialratio <- profit110/profit105

#calculate handle needed at -105 odds to equal profits at -110 odds

handle105breakeven <- handle * profitdifferentialratio

#calculate the difference in handle needed to equal profits at -110 odds

increasedhandleneeded <- handle105breakeven - handle

#create a vector for output

output <- c(profit110, profit105, profitdifferential, profitdifferentialratio, 
            handle105breakeven, increasedhandleneeded)
outputnames <- c("Profit at -110 odds", "Profit at -105 odds", "Difference in Profit in dollars", "Profit ratio difference", "Handle needed at -105 to earn same profit as at -110", "Increased handle needed at -105 to earn same profit as -110")
outputresults <- setNames(output, outputnames)
#print the results to the terminal
paste(names(outputresults), round(outputresults,2), sep = ": ")
# [1] "Profit at -110 odds: 2272727.27"                                         
# [2] "Profit at -105 odds: 1190476.19"                                         
# [3] "Difference in Profit in dollars: 1082251.08"                             
# [4] "Profit ratio difference: 1.91"                                           
# [5] "Handle needed at -105 to earn same profit as at -110: 95454545.45"       
# [6] "Increased handle needed at -105 to earn same profit as -110: 45454545.45"

profit_df <- data.frame(odds = c(-110, -105)) %>% 
  dplyr::mutate(profit = handle/2*(1 + 100/odds))

profit_df 

sprintf("Difference in profit: %s", 
        scales::dollar_format()(profit_df$profit[1] - profit_df$profit[2]))
sprintf("Profit ratio: %s", 
        scales::dollar_format()(profit_df$profit[1] / profit_df$profit[2]))
sprintf("Handle needed at -105 to earn same profit as at -110: %s", 
        scales::dollar_format(largest_with_cents = 1e+10)
        (handle * profit_df$profit[1] / profit_df$profit[2]))
sprintf("Increased handle needed at -105 to earn same profit as -110:: %s", 
        scales::dollar_format(largest_with_cents = 1e+10)
        ((profit_df$profit[1] / profit_df$profit[2] - 1) * handle))


