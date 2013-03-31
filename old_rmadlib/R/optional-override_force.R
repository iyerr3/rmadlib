
## users can choose to override some of the basic functions

override.force <- function(choice = "all")
{
    print(paste("The following functions will be overridden to gain the ability to deal with", .db.data.class[1]), quote = FALSE)
    print(paste("However, when dealing with ", .db.data.class[1], ", not all arguments will be utilized.", sep = ""), quote = FALSE)
    if (choice == "all")
    {
        ## create back-ups for functions that might be overridden
        ## source("def-hard_override_funcs.R", local = TRUE)
        for (i in seq(along = .localConst.hard.override.funcs))
            .override.one(.localConst.hard.override.funcs[i])
    }
    else
    {
        for (i in seq(along = choice))
            .override.one(choice[i])
    }
}

.override.one <- function(pkg.func)
{
    if (is.valid.pkgfunc(pkg.func))
    {
        pkg.splits <- strsplit(pkg.func, "::")[[1]]
        pkg.name <- pkg.splits[1]
        func.name <- pkg.splits[2]
        ## for example, assign rmadlib::.hard.overridden.data.frame to base::data.frame
        ## and .hard.overridden.data.frame uses base::data.frame inside it
        pkg.string <- paste("package:", pkg.name, sep = "")
        unlockBinding(func.name, envir = as.environment(pkg.string))
        assign(func.name,
               eval(parse(text = paste(.this.pkg.name, "::.hard.overridden.", func.name, sep = ""))),
               envir = as.environment(pkg.string))
        lockBinding(func.name, envir = as.environment(pkg.string))
        ## will restore the original data.frame at unload
        ## users would not feel any difference
        print(paste("The function \"", func.name, "\" in ", pkg.string, " has been overridden. See rmad.", func.name, "'s manual for more info.", sep = ""), quote = FALSE)
    }
    else
    {
        stop(paste("The given function name should be in a form of \"package::func\" and be one of \n", .localConst.hard.override.funcs))
    }
}

override.restore <- function(choice = "all")
{
    print(paste("The following functions will be restore to the original version without the ability to deal with", .db.data.class[1]), quote = FALSE)
    if (choice == "all")
    {
        ## restore all hard overridden functions
        for (i in seq(along=.localConst.hard.override.funcs))
            .restore.one(.localConst.hard.override.funcs[i])
    }
    else
    {
        for (i in seq(along = choice))
            .restore.one(choice[i])
    }
}

.restore.one <- function(pkg.func)
{
    if (is.valid.pkgfunc(pkg.func))
    {
        pkg.splits <- strsplit(pkg.func, "::")[[1]]
        pkg.name <- pkg.splits[1]
        func.name <- pkg.splits[2]
        ## for example, use the backup .origin.base.data.frame
        ## to restore data.frame in package:base
        pkg.string <- paste("package:", pkg.name, sep = "")
        unlockBinding(func.name, envir = as.environment(pkg.string))
        assign(func.name,
               eval(parse(text = paste(.this.pkg.name, "::.origin.", pkg.name, ".", func.name, sep = ""))),
               envir = as.environment(pkg.string))
        lockBinding(func.name, envir = as.environment(pkg.string))
        print(paste("The function \"", func.name, "\" in ", pkg.string, " has been restored to its original version.", sep = ""), quote = FALSE)
    }
    else
    {
        stop(paste("The given function name should be in a form of \"package::func\" and be one of \n", .localConst.hard.override.funcs))
    }
}
