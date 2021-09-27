.SIC <- list(division = .division,
             major_group = .major_group,
             industry_group = .industry_group,
             industry = .industry)

sic_description <- function(s, type = "industry",
                            paste.code = TRUE, ...) {
    ## map sic code to description

    s <- as.character(s)
    ans <- rep(NA, length(s))
    names(ans) <- s

    if (type == "industry_group")
        s <- substr(s, 1, 3)
    else if (type == "major_group")
        s <- substr(s, 1, 2)
    else if (type == "division") {
        s <- substr(s, 1, 2)
    }
    ii <- match(s,
                .SIC[[type]]$code,
                nomatch = 0L)
    if (paste.code) {
        ans[ii > 0L] <- paste(.SIC[[type]]$code[ii],
                              .SIC[[type]]$description[ii])
    } else {
        ans[ii > 0L] <- .SIC[[type]]$description[ii]
    }
    ans
}

description_sic <- function(pattern, type = "industry",
                            ..., ignore.case = TRUE) {
    ## map descriptions to sic code

    L <- grepl(pattern, .SIC[[type]]$description, ...,
               ignore.case = ignore.case)
    .SIC[[type]][L, ]
}

sic_French <- function(s, industries = 12, ...) {

}
