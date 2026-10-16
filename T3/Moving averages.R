#Moving average

#For a sequence X {x1,x2,x3,x4..., xk}

xx <- c (x1, x2, x3, ... xk)

# Moving average of order n (odd)
  
stats::filter (xx, c ( 1, 1, 1, ... ) / k) 

#(We put as many ones as elements in X)  es cuestión de buscar las formulas y las funciones

#Moving average of order n (even)
  
stats :: filter (xx, c(0.5, 1,1... , 0.5) / k)

#(We put k+1 elements, with the extremes being 0.5, the rest 1)


# Plotting a seasonal time series
#Use this library

library ( tidyverse )
xx <- c (x1, x2... xk)

#Reading the data
#Indicate the frequency of the seasonal component

uk <- ts ( xx , start = c ( start of the table (year, whatever), 1("we want the first quarter")), frequency = 4 ("The season is quarterly"))uk
plot (uk)