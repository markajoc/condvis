# open a device suitable for interactivity with getGraphicsEvent
opendev <-
function (width = 7, height = 7)
{
    if (identical(.Platform$OS.type, "windows")){
        windows(width = width, height = height)
    } else {
        if (identical(version$os, "linux-gnu")){
            x11(type = "Xlib", width = width, height = height)
        } else {
            x11(width = width, height = height)
        }
    }
}