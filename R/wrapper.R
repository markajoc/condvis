ceplot <- 
function (data, model, type = "default", response = NULL, S = NULL, C = NULL, 
    sigma = NULL, distance = "euclidean", cex.axis = NULL, cex.lab = NULL, 
    tck = NULL, view3d = FALSE, Corder = "default", selectortype = "minimal", 
    width = 9, height = 7)
{
    if (identical(type, "default")){
        interactiveceplot(data = data, model = model, response = response, 
            S = S, C = C, sigma = sigma, threshold = NULL, type = distance, 
            cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, view3d = view3d, 
            method = Corder, width = width, height = height)
    } else if (identical(type, "separate")){
        separate(data = data, model = model, response = response, S = S, C = C, sigma = sigma, 
            threshold = NULL, type = distance, cex.axis = cex.axis, cex.lab = 
            cex.lab, tck = tck, view3d = view3d, method = Corder, selectortype =
            selectortype)
    } else if (identical(type, "shiny")){
        shinyceplot(data = data, model = model, response = response, S = S, 
            C = C, sigma = sigma, threshold = NULL, type = distance, 
            cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, view3d = 
            view3d, method = Corder, selectortype = "minimal")
    }
}