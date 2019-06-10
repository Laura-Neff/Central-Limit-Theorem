#Laura Neff and Zachary Hong worked together for this assignment
rprobs = sample(1:5,5)
rprobs = rep(16*rprobs,each=20)
rprobs = rprobs + 20*runif(100)
rprobs = rprobs / sum(rprobs)
pop = sample(1:100,size=10000,replace=TRUE,prob=rprobs)

population.hist = function(pop) {
  #Next, write a function population.hist(pop) that displays a histogram of the population
  #represented by the vector pop that consists of some number of values, 
  #each between 1 and 100, inclusive.
  
  #Additionally:
    
  #Classes for the histogram should start at x=0.5 and be 5 units wide.
  #The bars of the histogram should be colored "skyblue".
  #The title of the histogram should be "Population".
  #There should be no label on the x-axis.
    
  hist(pop,
       breaks=seq(from=.5, by=5, to = 100.5),
       main="Population",
       xlab="",
       col="skyblue"
       )
  
}
population.hist(pop)

sample.means = function(pop, sample.size, n, title, show.overlay) {
  #Then, create a second function sample.means(pop,sample.size,n,title,show.overlay) 
  #that draws n samples of size sample.size from the population pop,
  #computes their means, and displays a histogram of these means.
  
  set.seed(19) #required so that middle and bottom plots match, enforces reproducibility
  
  data = replicate(n, mean(sample(pop,size=sample.size))) #resample pop for central limit theorem
  
  hist(data,
       breaks=seq(from = 0.5, to = 110, by=5),
       main = title,
       xlab="",
       col="green") #plot resampled pop to show central limit theorem
       
   if (show.overlay == TRUE) {
       pop = pop[order(pop)]
       abline(v=mean(pop), col="blue", lwd=3) #line centered at pop mean 
       pop_pdf = dnorm(pop,mean(pop),sd(pop)/sqrt(sample.size))*100*mean(pop) 
            #PDF for plot (normal distribution, mean=pop mean, std = pop stdev / sqrt(n))
            #Ask if it really is standard deviation over square root of n
       xs = c(mean(pop), mean(pop)+sd(pop)/sqrt(sample.size)) # this is the width (start and end points)
       ys = c(max(pop_pdf)*(dnorm(1)/dnorm(0)),
              max(pop_pdf)*(dnorm(1)/dnorm(0))) #this is the height of the line (start, end pts)
            
       lines(xs, ys,col="red",lwd=3) #draw the std line
       lines(pop,pop_pdf) #draw the probability distribution
       }
}

#make the required 3x3 plot
par(mfcol=c(3,3))
population.hist(pop)
sample.means(pop,2,1000,"Sample Means (n=2)",FALSE)
sample.means(pop,2,1000,"Sample Means (n=2)",TRUE)
population.hist(pop)
sample.means(pop,4,1000,"Sample Means (n=4)",FALSE)
sample.means(pop,4,1000,"Sample Means (n=4)",TRUE)
population.hist(pop)
sample.means(pop,15,1000,"Sample Means (n=15)",FALSE)
sample.means(pop,15,1000,"Sample Means (n=15)",TRUE)





