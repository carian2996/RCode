# Author: Ian C
# Date: Feb 11, 2017

# Who should start the conversation. Seed is a day of the years.

for (date in 11022017 + (0:17)*1000000){
    set.seed(date)
    who <- sample(c("ian", "rocio"), 1)
    print(paste("Primer saludo el día", date, "es para", who))
}
