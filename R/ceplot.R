ceplot <- 
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", type = "default", cex.axis = NULL, cex.lab = NULL, 
    tck = NULL, view3d = FALSE, Corder = "default", selectortype = "minimal", 
    width = 9, height = 7)
{
    if (identical(type, "default")){
        ceplot.interactive(data = data, model = model, response = response, 
            S = S, C = C, sigma = sigma, distance = distance, cex.axis = 
            cex.axis, cex.lab = cex.lab, tck = tck, view3d = view3d, Corder = 
            Corder, width = width, height = height)
    } else if (identical(type, "separate")){
        ceplot.separate(data = data, model = model, response = response, S = S, C = C, 
            sigma = sigma, distance = distance, cex.axis = cex.axis, 
            cex.lab = cex.lab, tck = tck, view3d = view3d, Corder = Corder, 
            selectortype = selectortype)
    } else if (identical(type, "shiny")){
        shinyceplot(data = data, model = model, response = response, S = S, 
            C = C, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, 
            Corder = Corder)
    }
}