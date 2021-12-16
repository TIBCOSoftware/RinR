set(PACKAGE_R_FILES
# Add files below in alphabetical order
R/all.equal.R
R/eval.R
R/RGraph.R
R/sideBySide.R
R/findR.R
R/zzz.R
)

set(PACKAGE_TESTS_FILES
# Add files below in alphabetical order
tests/REvaluate.t
tests/RGraph.t
tests/sideBySide.t
)

set(PACKAGE_DATA_FILES
# Add files below in alphabetical order
)

set(PACKAGE_SRC_FILES
# Add files below in alphabetical order
)

set(PACKAGE_HELP_FILES
# Add files below in alphabetical order
man/LocalEvaluator.Rd
man/makeREvaluator.Rd
man/multiEvaluator.Rd
man/pushPATH.Rd
man/RGraph.Rd
man/sideBySide.Rd
man/RinR-package.Rd
)

set(PACKAGE_VIGNETTE_FILES
# Add files below in alphabetical order
vignettes/RinR.Rmd
)

set(PACKAGE_OTHER_FILES
)

set(PACKAGE_ALL_FILES
${PACKAGE_R_FILES}
${PACKAGE_TESTS_FILES}
${PACKAGE_DATA_FILES}
${PACKAGE_SRC_FILES}
${PACKAGE_HELP_FILES}
${PACKAGE_VIGNETTE_FILES}
${PACKAGE_OTHER_FILES}
)

INCLUDE(SnextPackage)
SNEXT_PACKAGE(RinR
              R ${PACKAGE_R_FILES}
              FILES ${PACKAGE_OTHER_FILES} ${PACKAGE_TESTS_FILES}
              HELP ${PACKAGE_HELP_FILES}
              VIGNETTES ${PACKAGE_VIGNETTE_FILES} VIGNETTE_PREPROCESS vignettes/Preprocess${SNEXT_OS}.txt
             )
