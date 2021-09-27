library("NMOF")

S <- French("~/Downloads/French",
            "Siccodes12.zip")

save(S,
     file = "~/Packages/SIC.codes/data/French12.RData",
     version = 2)
