makepath <-
function (Xc, ncentroids, ninterp = 4)
{
    means <- colMeans(Xc)
    sds <- apply(Xc, 2L, sd)
    Xc <- scale(Xc)[, ]
    clustering <- kmeans(Xc, centers = ncentroids)
    centers <- clustering$centers
    o <- TSP(dist(centers))
    orderindex <- solve_TSP(o)
    centers <- centers[orderindex, , drop = FALSE]
    interp <- function(x, n = ninterp)
    {
        out <- vector()
        for (i in 1:(length(x) - 1)){
            out <- c(out, seq(x[i], x[i + 1], length.out = n))
        }
        out
    }
    centers <- as.data.frame(t(apply(t(apply(centers, 1L, `*`, sds)), 1L, `+`,
        means)))
    path <- as.data.frame(apply(centers, 2L, interp))
    list(centers = centers, path = path)
}