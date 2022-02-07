# Copyright © 2020. TIBCO Software Inc.
# This file is subject to the license terms contained
# in the license file that is distributed with this file.
{
    library(RinR)
    TRUE
}
{
    ss2 <- RinR::sideBySide(structure(rep(list(1:9), 4),
          names = c("The first object (with a pretty long name)", "Obj 2", "Obj 3", "Obj 4")))
    r2 <- capture.output(print(width = 50, ss2))
    e2 <- c("The first object (with a pretty long name)",
           "[1] 1 2 3 4 5 6                           ",
           "[7] 7 8 9                                 ",
           "",
           "Obj 2           | Obj 3          ",
           "[1] 1 2 3 4 5 6 | [1] 1 2 3 4 5 6",
           "[7] 7 8 9       | [7] 7 8 9      ",
           "",
           "Obj 4          ",
           "[1] 1 2 3 4 5 6",
           "[7] 7 8 9      ")
    all.equal(e2, r2)
}
{
    ss3 <- RinR::sideBySide(structure(rep(list("Backlashes (\\) are a pain\n"),2), names=c("One", "Two")))
    r3 <- capture.output(print(width=80, ss3))
    e3 <- c("One                                | Two                               ",
            "[1] \"Backlashes (\\\\) are a pain\\n\" | [1] \"Backlashes (\\\\) are a pain\\n\"")
    all.equal(e3, r3)
}
