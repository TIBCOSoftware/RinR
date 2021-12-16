{
    # This will fail if R on this machine cannot run png().
    r1 <- RinR::RGraph(display=FALSE, plot(1:10))
    all.equal(as.raw(c(0x89, 0x50, 0x4e, 0x47)), r1[1:4])
}
{
    # SNXT-7435 check that deviceArgs is being used
    r7435 <- capture.output(invisible(tryCatch(RinR::RGraph(display=FALSE, verbose=TRUE, plot(1:10), deviceArgs=list(type=3021)), error=function(e)e)))
    any(grepl("type *= *3021", r7435))
}
