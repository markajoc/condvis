ceplot <- 
function (data, model, response = NULL, S = NULL, C = NULL, sigma = NULL, 
    distance = "euclidean", type = "default", cex.axis = NULL, cex.lab = NULL, 
    tck = NULL, view3d = FALSE, Corder = "default", selectortype = "minimal", 
    conf = FALSE)
{
    if (identical(type, "default")){
        ceplot.interactive2(data = data, model = model, response = response, 
            S = S, C = C, sigma = sigma, distance = distance, cex.axis = 
            cex.axis, cex.lab = cex.lab, tck = tck, view3d = view3d, Corder = 
            Corder, conf = conf)
    } else if (identical(type, "separate")){
        ceplot.separate(data = data, model = model, response = response, S = S, C = C, 
            sigma = sigma, distance = distance, cex.axis = cex.axis, 
            cex.lab = cex.lab, tck = tck, view3d = view3d, Corder = Corder, 
            selectortype = selectortype)
    } else if (identical(type, "shiny")){
        ceplot.shiny(data = data, model = model, response = response, S = S, 
            C = C, cex.axis = cex.axis, cex.lab = cex.lab, tck = tck, 
            Corder = Corder)
    }
}