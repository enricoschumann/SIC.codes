.SIC <- list(division = .division,
             major_group = .major_group,
             industry_group = .industry_group,
             industry = .industry)
            
sic_description <- function(s, type = "industry", ...) {
    ## map sic code to description

    ans <- rep(NA, length(s))
    names(ans) <- s
    ii <- match(s, .SIC$code, nomatch = 0)
    ans[ii>0] <- .SIC$description[ii]
    ans
}

description_sic <- function(pattern, ...,
                            ignore.case = TRUE) {
    ## map descriptions to sic code

    L <- grepl(pattern, .SIC$description, ...,
               ignore.case = ignore.case)
    .SIC[L, ]
}
