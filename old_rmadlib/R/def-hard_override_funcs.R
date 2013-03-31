
## Array of overriden functions
## List of function name and package that it belongs to

## Actually, maybe it is a good idea not to use hard override
## because this might cause some confusion

## However, in some cases, a hard override would be really nice.
## For example, a hard-overidden 'lm' function could bring the 
## user seemless experience.

.localConst.hard.override.funcs <- c("stats::lm" 

                                     ## ,"base::data.frame" # no! actually we are not going to override data.frame
                                 )


