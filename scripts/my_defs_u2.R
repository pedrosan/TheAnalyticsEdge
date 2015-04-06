
## ---- my_handy_defs

mypanel <- function(x, y, ...){
   cvec.bg <- c(rgb(1.0, 0.8, 0.9) , rgb(0.9, 0.9, 1.0), rgb(0.8, 1.0, 0.8))
   cvec.border <- c("red", "blue", rgb(0.0, 0.67, 0.0))
   r <- abs(cor(x, y))
   i.color <- 1+(r>0.7)+(r>0.8)
   ll <- par("usr") 
   rect(ll[1], ll[3], ll[2], ll[4], border=cvec.border[i.color], lwd=3)
   points(x, y, ... ) 
   ok <- is.finite(x) & is.finite(y)
   if( any(ok) ) { abline(lm(y[ok] ~ x[ok]), col="red", lty=2, ...) }
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, color.bg=FALSE, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    cvec.bg <- c(rgb(1.0, 0.8, 0.9) , rgb(0.9, 0.9, 1.0), rgb(0.8, 1.0, 0.8))
    cvec.text <- c("red", "blue", rgb(0.0, 0.67, 0.0))
    i.color <- 1+(r>0.4)+(r>0.6)
    if( color.bg ) {
       ll <- par("usr") 
       rect(ll[1], ll[3], ll[2], ll[4], col=cvec.bg[i.color])
       text(0.5, 0.5, txt, cex = cex.cor * r, col=cvec.text[i.color])
    } else {
       text(0.5, 0.5, txt, cex = cex.cor * r, col=cvec.text[i.color])
    }
}

printStats <- function (m, with.cx=TRUE) {
    if (class(m) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(m)$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
    attributes(p) <- NULL
    
    fml <- as.character(formula(m))
    cx <- summary(m)$coeff
    stars <- rep(" ", nrow(cx))
    stars[cx[,4] <= 0.1] <- "."
    stars[cx[,4] <= 0.05] <- "*"
    stars[cx[,4] <= 0.01] <- "**"
    stars[cx[,4] <= 0.001] <- "***"
    cat("MODEL        : ", sprintf("%s", paste(fml[c(2,1,3)], sep=" ", collapse=" ")), "\n", sep="")
    cat("SUMMARY STATS: ")
    cat("R^2 = ",sprintf("%6.4f",summary(m)$r.squared), 
        "  (adj. = ", sprintf("%6.4f",summary(m)$adj.r.squared), ")", sep="")
    cat("\n")
    cat("               ")
    cat("F-stats: ", sprintf("%.3f",f[1]), " on ", f[2], " and ", f[3], " DF,  p-value: ", p, "\n", sep="")
    if( with.cx ) {
        cat("\n")
        print(cbind(format(cx[,c(1,2,4)], scientific=TRUE, justify="right", digits=5), Signif=stars), 
              quote=FALSE, print.gap=3)
    }
}

printStats.cpt <- function (m, with.cx=TRUE) {
    if (class(m) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(m)$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail=FALSE)
    attributes(p) <- NULL
    
    fml <- as.character(formula(m))
    cx <- summary(m)$coeff
    stars <- rep(" ", nrow(cx))
    stars[cx[,4] <= 0.1] <- "."
    stars[cx[,4] <= 0.05] <- "*"
    stars[cx[,4] <= 0.01] <- "**"
    stars[cx[,4] <= 0.001] <- "***"
    cat("MODEL : ", sprintf("%s", paste(fml[c(2,1,3)], sep=" ", collapse=" ")), "\n", sep="")
    cat("      : ")
    cat("adj. R^2 = ", sprintf("%6.4f",summary(m)$adj.r.squared), 
        " /  F-stats: ", sprintf("%.3f",f[1]), " on ", f[2],",", f[3], " Df,  p-value: ", p, "\n", sep="")
    if( with.cx ) {
        print(cbind(format(cx[,c(1,2,4)], scientific=TRUE, justify="right", digits=5), Signif=stars), 
              quote=FALSE, print.gap=3)
        cat("\n")
    }
}

## ---- end-of-my_handy_defs
