myvalidate <- function (..., errorClass = character(0)) 
{
    results <- sapply(list(...), function(x) {
        if (is.null(x)) 
            return(NA_character_)
        else if (identical(x, FALSE)) 
            return("")
        else if (is.character(x)) 
            return(paste(as.character(x), collapse = "\n"))
        else stop("Unexpected validation result: ", as.character(x))
    })
    results <- stats::na.omit(results)
    if (length(results) == 0) 
        return(invisible())
    results <- results[nzchar(results)]
    shiny:::reactiveStop(paste(results, collapse = "\n"), c(errorClass, 
                                                    "validation"))
}

gg_color_hue <- function(vectColors) {
    n <- length(unique(vectColors))
    hue_pal()(n)[vectColors]
}
