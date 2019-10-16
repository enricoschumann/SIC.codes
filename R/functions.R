.SIC <- rbind(.SIC1, .SIC2, .SIC3, .SIC4,
              stringsAsFactors = FALSE)

sic_description <- function(s, ...) {
    ## map sic code to description

    if (!is.character(s))
        stop(sQuote("s"), "must be character")
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
