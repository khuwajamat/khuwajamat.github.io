---
layout: default
title: "Stock Price Prediction"
---



{% highlight ruby%}
###########################################################
#                    Advanced Programming in R
#                    academic year 2022/2023                 
#                       Duae Mariam (455855)
#                       Gizem Guleli (449872)   
########################################################### 


# Project Submission                                   
########################################################### 
#####################################
#Loading of libraries

library("R6")
library(Rcpp)
library(rbenchmark)

#####################################
#USE OF R6 to create a desired object type "class"


Stock_class <- R6Class("Stock_class",
                       public = list(
                         Stock_name = NULL, # we need to specify default values here
                         Price = NULL,
                         Dividend = NULL,
                         Volatility = NULL,
                         Traded_Currency = "USD",
                         RFree = 0.05,
                         
                         initialize = function(Stock_name,
                                               Price,
                                               Dividend,
                                               Volatility,
                                               Traded_Currency,
                                               RFree){
                           self$Stock_name = Stock_name
                           self$Traded_Currency = Traded_Currency
                           self$Price = Price
                           self$Dividend = Dividend
                           self$Volatility = Volatility
                           self$RFree =  RFree
                           
                           invisible(self) 
                         },
                         
                         Price_predicted =  cppFunction("#include <cstdlib>
                        #include <cmath>
    double Stock_path_averaged(
      double spot,
      double vol,
      double rf
        
      ){
    //function definition
    
      double nInt =1000000;
      double cumShocks = 0;
      
      
      int lb = 20, ub = 100;
      // This part will create some sequence of random numbers on every program run within range lb to ub
      double random = rand() % (ub - lb + 1) + lb ;
      
      // Monte carlo simulation  for predicting price for next day
      //nint is normaling the fluctuation when doing average
      for(int i = 0; i < nInt; i++){
      cumShocks += (spot * exp(rf - 0.5 * vol * vol ) +vol* random);
      }
      
      cumShocks= cumShocks/nInt;
      return cumShocks;
      
    }")
                         
                         
                       ))
Apple <- Stock_class$new("Apple", "USD", 120, 0.07, 0.04, 0.05 )

#using class can be very useful for adding other stocks to the portfolio
#and being able to get the attributes of the class in a newly created object
#without defining the class again and again


#####################################
#USE OF Price Predicted Function out side of the class

Price_predicted =  cppFunction("#include <cstdlib>
                        #include <cmath>
    double Stock_path_averaged(
      double spot,
      double vol,
      double rf
        
      ){
    //function definition
    
      double nInt =1000000;
      double cumShocks = 0;
      
      
      int lb = 20, ub = 100;
      // This part will create some sequence of random numbers on every program run within range lb to ub
      double random = rand() % (ub - lb + 1) + lb ;
      
      // Monte carlo simulation  for predicting price for next day
      //nint is normaling the fluctuation when doing average
      for(int i = 0; i < nInt; i++){
      cumShocks += (spot * exp(rf - 0.5 * vol * vol ) +vol* random/100);
      }
      
      cumShocks= cumShocks/nInt;
      return cumShocks;
      
    }")





#################################

#we can find the predicted price of any stock using 3 attributes, spot price , volatility and riskfree rate which are used in the monte carlo simulation 

#testing the function separately

Price_predicted(120,0.03,0.032)

#this price predicted is very scalable for those who want to make immediate profits 
#from speculation type of investments, who want to make huge profits in one or two days
#using Monte Carlo equation in this code of price predicted we are predicting the price of 
#stocks by giving only 3 main market attributes






#####################################

#creating important objects for collecting stock data in a loop

library("quantmod")

last_prices_portfolio <- list()
stock_objects <- list()

#here we can easily edit the portfolio we want to analyze
portfolio <- c("AAPL", "MSFT","TSLA", "JPM", "PG")

#using loop we can get the currently trading price for stocks
#Loop definition
for (i in portfolio){
  getSymbols(i)
  
  # Extract the last traded price for the current ticker and add it to our list
  #stock name is defined in list and latest stock price is coming from yahoo finance using library
  last_price <- tail(Cl(get(i)), 1)
  
  last_prices_portfolio[[i]] <- last_price
  
  # Create an instance of the Stock Info class with the ticker and price
  stock_obj<- Stock_class$new(Stock_name = i, Price = last_price, Dividend =  0.07,Volatility =  0.04,Traded_Currency =  "USD",RFree =  0.05 )
  
  # Add the stock object to the list
  stock_objects[[i]] <- stock_obj
}

print(stock_objects)

#this loop enables us to get the current prices for all the stocks we have in portfolio, using quantmod library
#and we are using the stock class again here to get all the attributed for each of the 
#stock in the portfolio along with the current prices. so this can be very scalable if
# anyone is intrested in analysis of stocks and he has to consider all the factors at a time , 
#so he can compare stocks and make adjustments in portfolio to increase the profit, using this loop





#####################################
#USE OF Lapply for vectorization of the code


#Using Vectorization to run code in one line for all the classes
lapply(stock_objects, function(instance) instance$Price_predicted(instance$Price,instance$Volatility, instance$RFree))

#Using Vectorization to run code in one line for all the classes to print the volatility
lapply(stock_objects, function(instance) print(instance$Volatility))

#Using Vectorization to run code in one line for all the classes to increase the volatility by 0.01
lapply(stock_objects, function(instance) (instance$Volatility= instance$Volatility +0.01))

# sometimes the volatility and other fators of stocks changes by some percentage points so this type of v
#lapply vectorization technique can be very useful to change market variables.




{% endhighlight %}
