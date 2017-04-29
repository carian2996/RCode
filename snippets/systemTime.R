system.time({
    for(i in 1:1000000){
        # Put some cool algorithm
        rnorm(100,0,1)
    }
 })

# user = Time used by the processing unit due to user instructions.
# time = Time used by the processing unit due to instructions from the same CPU.
# elapsed = 'Real' time used since the procedure began.
