set.seed(1)
cleaned__tidy_data %>% 
  filter(metric =='EDF due to extreme cold',
         age == "All-Ages") %>% 
  pull(value) %>% 
  classIntervals(n=5, style = 'jenks')


### Stacked
library(classInt)
my_n <- 100
x <- mapply(rnorm, n = my_n, mean = (1:5) * 5) %>% unlist()
dfa = classIntervals(x, n = 5, style = "jenks")


## Documentation
classIntervals(var, n, style = "quantile", rtimes = 3, ...,
               intervalClosure = c("left", "right"), dataPrecision = NULL,
               warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1,
               gr = c("[", "]"))
# S3 method for classIntervals
plot(x ) 
# S3 method for classIntervals
print(x, digits = getOption("digits"), 
      under="under", over="over", between="-", cutlabels=TRUE, unique=FALSE) 
nPartitions(x) 
classIntervals2shingle(x)
