library("readxl")
data = read_excel("new_missing_dataframe_correlation.xlsx")
windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Symbol")
)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.1), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * 0.6,font=2)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 2.5),family="A",font=2,font.axis=2,font.lab=2,font.sub=2)
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)

}
panel.lm<-function(x,y,col=par("col"),bg=NA,pch=par("pch"),
                   cex=1,col.smooth="red",...){
  points(x,y,pch=pch,col=col,bg=bg,cex=cex)
  abline(stats::lm(y~x),col=col.smooth,...)
}
pairs(data[0:16], main = "",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)],
      diag.panel=panel.hist,
      upper.panel=panel.cor,
      lower.panel=panel.lm)
