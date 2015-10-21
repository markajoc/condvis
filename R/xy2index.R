xy2index <-
function (x, y, screen.info)
{
    c1 <- x >= screen.info$xleft
    c2 <- x < screen.info$xright
    c3 <- y >= screen.info$ybottom
    c4 <- y < screen.info$ytop

    screen.info$xcplots.index[c1 & c2 & c3 & c4]
}
