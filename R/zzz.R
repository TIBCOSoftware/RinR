.onLoad <- function(libname, pkgname)
{
    if (!isTRUE(getOption("RinR_DONT_CONFIGURE"))) {
        tryCatch(configureREvaluator(REvaluator), error=function(e)warning("while configuring REvaluator:", conditionMessage(e)))
        tryCatch(configureREvaluator(TERREvaluator), error=function(e)warning("while configuring TERREvaluator:", conditionMessage(e)))
    }
}
