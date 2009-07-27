
plot.zelig.arima <- function(x, xlab="", user.par=FALSE, pred.se=TRUE,
                             col=c("blue", "red", "green3", "black"),
                             lty=2, ...){
  if (length(col) > 4) col <- rep(col, 4)[1:4]
  k <- length(x$qi)
  if (k==3){
    if (is.null(x$qi$t.eff) & !user.par){
      par(mfrow=c(1,1))
    }	
    if (!is.null(x$qi$t.eff) & !user.par){
      par(mfrow=c(2,1))
    }
    if (user.par){
      par <- par(no.readonly = TRUE) 
    }
    ev.mean <- apply(x$qi$ev, 2, "mean")
    ev.quant <- apply(x$qi$ev, 2, "quantile", c(0.025, 0.975))
    if (pred.se){
      ev.total.up <- x$qi$ev + 1.96*x$qi$se
      ev.total.down <- x$qi$ev - 1.96*x$qi$se
      ev.total.up <- apply(ev.total.up, 2, "mean")
      ev.total.down <- apply(ev.total.down, 2, "mean")
      min.plot <- min(ev.quant[1,], ev.total.down, x$t.series[1:x$min.time])
      max.plot <- max(ev.quant[2,], ev.total.up, x$t.series[1:x$min.time])
    }
    if (!pred.se){
      min.plot <- min(ev.quant[1,], x$t.series[1:x$min.time])
      max.plot <- max(ev.quant[2,], x$t.series[1:x$min.time])
    }
    if (length(ev.mean)>1){
      ts.plot(x$t.series[1:x$min.time],
              ylim=c(min.plot, max.plot),
              col=col[4],ylab="Time Series Value",
              xlim=c(1, x$min.time + ncol(x$qi$ev)))
      lines(ev.mean~c((x$min.time+1):
                      (x$min.time + ncol(x$qi$ev))),
            col=col[1])
      lines(ev.quant[2,]~c((x$min.time+1):
                           (x$min.time + ncol(x$qi$ev))),
            lty=lty, col=col[1])
      lines(ev.quant[1,]~c((x$min.time+1):
                           (x$min.time + ncol(x$qi$ev))),
            lty=lty, col=col[1])
      if (pred.se){
        lines(ev.total.up~c((x$min.time+1):
                            (x$min.time + ncol(x$qi$ev))),
              lty=lty, col= col[3])
        lines(ev.total.down~c((x$min.time+1):
                              (x$min.time + ncol(x$qi$ev))),
              lty=lty, col=col[3])
      }
      lines(c(x$min.time, x$min.time + 1),
            c(x$t.series[x$min.time], ev.mean[1]), col=col[1])
      abline(v=x$min.time, lty=1, col=col[2])
      if(!is.null(x$qi$t.eff)){
        teff.mean<- apply(x$qi$t.eff, 2, "mean")
        teff.quant<- apply(x$qi$t.eff, 2, "quantile", c(0.025, 0.975))
        ts.plot(teff.mean, xlab="Time After Counterfactual",
                ylab="Difference", main="Y - E[Y|X]",
                ylim=c(min(teff.quant[1,]), max(teff.quant[2,])), col=col[1])
        lines(teff.quant[2,], lty=lty, col=col[1])
        lines(teff.quant[1,], lty=lty, col=col[1])
      }
    }
    if (length(ev.mean)==1){
      ts.plot(x$t.series[1:x$min.time],
              ylim=c(min.plot, max.plot),
              col=col[4],ylab="Time Series Value",
              xlim=c(1, x$min.time + ncol(x$qi$ev)), gpars=...)
      points(x$min.time + 1, ev.mean)
      if (pred.se){
        arrows(x$min.time + 1 , ev.total.down,
               x$min.time + 1, ev.total.up, col=col[2],
               code=3, length=0.1, angle=90)
      }
      arrows(x$min.time + 1, ev.quant[1,], x$min.time + 1,
             ev.quant[2,], col=col[1], code=3, length=0.1, angle=90)
      lines(c(x$min.time, x$min.time + 1),
            c(x$t.series[x$min.time], ev.mean[1]), col=col[1])
      if(!is.null(x$qi$t.eff)){
        teff.mean<- apply(x$qi$t.eff, 2, "mean")
        teff.quant<- apply(x$qi$t.eff, 2, "quantile", c(0.025, 0.975))
        plot(density(x$qi$t.eff), ylab="Density",
             xlab="Difference", main="Y-E[Y|X]", ...)
      }
    }
  }
  if (k==4){
    par(mfrow=c(2,1))
    ev.mean<- apply(x$qi$ev, 2, "mean")
    ev.quant<- apply(x$qi$ev, 2, "quantile", c(0.025, 0.975))
    if(pred.se){
      ev.total.up<- x$qi$ev + 1.96*x$qi$se
      ev.total.down<- x$qi$ev - 1.96*x$qi$se
      ev.total.up<- apply(ev.total.up, 2, "mean")
      ev.total.down<- apply(ev.total.down, 2, "mean")
      min.plot<- min(ev.quant[1,], x$t.series[1:x$min.time], ev.total.down)
      max.plot<- max(ev.quant[2,], x$t.series[1:x$min.time], ev.total.up)
    }
    if(!pred.se){
      min.plot<- min(ev.quant[1,], x$t.series[1:x$min.time])
      max.plot<- max(ev.quant[2,], x$t.series[1:x$min.time])
    }
    if(length(ev.mean)>1){
      ts.plot(x$t.series[1:x$min.time],
              ylim=c(min.plot, max.plot),
              col=col[4], ylab="Time Series Value",
              xlim=c(1, x$min.time + ncol(x$qi$ev)), gpars=...)
      lines(ev.mean~c((x$min.time+1):
                      (x$min.time + ncol(x$qi$ev))), col=col[1])
      lines(ev.quant[2,]~c((x$min.time+1):
                           (x$min.time + ncol(x$qi$ev))),
            lty=lty, col=col[1])
      lines(ev.quant[1,]~c((x$min.time+1):
                           (x$min.time + ncol(x$qi$ev))),
            lty=lty, col=col[1])
      if(pred.se){
        lines(ev.total.up~c((x$min.time+1):
                            (x$min.time + ncol(x$qi$ev))),
              lty=lty, col= col[3])
        lines(ev.total.down~c((x$min.time+1):
                              (x$min.time + ncol(x$qi$ev))),
              lty=lty, col=col[3])
      }
      lines(c(x$min.time, x$min.time + 1),
            c(x$t.series[x$min.time], ev.mean[1]))
      abline(v=x$min.time, lty=1, col=col[2])
    }
    if(length(ev.mean)==1){
      ts.plot(x$t.series[1:x$min.time], ylim=c(min.plot, max.plot),
              ylab="Time Series Value", col=col[4],
              xlim=c(1, x$min.time + ncol(x$qi$ev)), gpars=...)
      points(x$min.time + 1, ev.mean)
      if(pred.se){
        arrows(x$min.time + 1, ev.total.down, x$min.time + 1,
               ev.total.up, col=col[2], code=3, length=0.1, angle=90)
      }
      arrows(x$min.time + 1, ev.quant[1,], x$min.time + 1, ev.quant[2,],
             col=col[1], code=3, length=0.1, angle=90)
      lines(c(x$min.time, x$min.time + 1),
            c(x$t.series[x$min.time], ev.mean[1]), col=col[1])
    }
    fd.mean<- apply(x$qi$fd, 2, "mean")
    fd.quant<- apply(x$qi$fd, 2, "quantile", c(0.025, 0.975))
    if(length(fd.mean)>1){
      ts.plot(fd.mean, col=col[1], ylim=c(min(fd.quant[1,]), max(fd.quant[2,])),
              main="E[Y|X1] - E[Y|X]", xlab="Time From First Counterfactual",
              ylab="Difference", gpars=...)
      lines(fd.quant[2,], lty=lty, col=col[1])
      lines(fd.quant[1,], lty=lty, col=col[1])
    }
    if(length(fd.mean)==1){
      par(mfrow=c(2,1))
      ts.plot(x$t.series[1:x$min.time], ylim=c(min.plot, max.plot),
              ylab="Time Series Value", col=col[4],
              xlim=c(1, x$min.time + ncol(x$qi$ev)), gpars=...)
      points(x$min.time + 1, ev.mean)
      if(pred.se){
        arrows(x$min.time + 1, ev.total.down, x$min.time + 1, ev.total.up,
               col=col[2], code=3, length=0.1, angle=90)
      }
      arrows(x$min.time + 1, ev.quant[1,], x$min.time + 1, ev.quant[2,],
             col=col[1], code=3, length=0.1, angle=90)
      lines(c(x$min.time, x$min.time + 1),
            c(x$t.series[x$min.time], ev.mean[1]), col=col[1])
      plot(density(x$qi$fd), main="E[Y|X1] - E[Y|X]",
           xlab="Difference in Value", ylab="Density", ...)
    }
  }
}

