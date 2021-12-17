# Copyright 2021. TIBCO Software Inc.
# This file is subject to the license terms contained
# in the license file that is distributed with this file.
{
    # Try to make these tests runnable on machines without the other interpreter
    myEvaluator <- switch(version$language,
        TERR = RinR::TERREvaluator,
        R = RinR::REvaluator)
    TRUE
}
{
    r2 <- normalizePath(RinR::REvaluate(getwd(), REvaluator=myEvaluator))
    e2 <- normalizePath(getwd())
    all.equal(e2, r2)
}
{
    r3 <- RinR::REvaluate({
            func <- function(x) {
                x^2 + 1
            }
            func(x)
        }, REvaluator=myEvaluator, data = list(x=20:23))
    e3 <- (20:23)^2 + 1
    all.equal(e3, r3)
}
{
    r4 <- RinR::RCompare({
        runif(10, 1, 2)
        }, REvaluators=list(One = myEvaluator, Two = myEvaluator))
    all(r4$One >= 1 & r4$One <= 2 & r4$two >= 1 & r4$Two <= 2) &
        all(grepl("relative difference", attr(r4, "all.equal")[[1]]))
}
assertionTest::atUsesRandom({
    x5 <- sample(1:17, size=3)
    r5 <- RinR::RCompare({
        sum(x5)
        },
        REvaluators=list(One = myEvaluator, Two = myEvaluator, Three = myEvaluator),
        data = "x5")
    all.equal(
        c(TRUE, TRUE),
        unlist(use.names = FALSE, attr(r5, "all.equal")))
})
{
    r6 <- RinR::REvaluate(Sys.getpid(), REvaluator=RinR::LocalEvaluator)
    e6 <- Sys.getpid()
    identical(e6, r6)
}
{
    # SNXT-6708 : deparse/parse cycle changes quote(!!x) to quote(!(!x))
    f7 <- function(x) {
        x <- substitute(x)
        if (is.call(x) && identical(x[[1]], as.name("!")) &&
            is.call(x[[2]]) && identical(x[[2]][[1]], as.name("!"))) {
            "bang-bang"
        } else {
            stop("expected '!!', got '", x[[1]], x[[2]][[1]], "'")
        }
    }
    r7 <- RinR::REvaluate(REvaluator=myEvaluator, f7(!!foo), data="f7")
    identical("bang-bang", r7)
}
