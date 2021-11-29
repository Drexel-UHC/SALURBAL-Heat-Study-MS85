set.seed(1)
cleaned__tidy_data %>% 
  filter(metric =='EDF due to extreme cold',
         age == "All-Ages") %>% 
  pull(value) %>% 
  classIntervals(n=5, style = 'jenks')


library(classInt)

my_n <- 100

x <- mapply(rnorm, n = my_n, mean = (1:5) * 5) %>% unlist()

system.time(classIntervals(x, n = 5, style = "jenks"))
R> system.time(classIntervals(x, n = 5, style = "jenks"))
user  system elapsed 