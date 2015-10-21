getvarnames <- 
function(model)
{
    string1 <- deparse(model$terms[[3]])
    string2 <- unlist(strsplit(string1, split = NULL))
    string3 <- paste(string2[string2 != " "], collapse = "")
    predictors1 <- unlist(strsplit(string3, split = "+", fixed = TRUE))
    predictors2 <- unique(vapply(predictors1, cleanstring, character(1)))
    response <- unlist(deparse(model$terms[[2]]))
    list(response = response, predictors = predictors2)
}