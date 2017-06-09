local(addTaskCallback( function(...){ 
    rm( list = ls( globalenv(), all.names = TRUE ), envir = globalenv() ) 
    TRUE 
}))
