errors.permonth <- read.csv("errors_permonth.csv")
errors.accumulate <- read.csv("errors_accumulate.csv")
errors.accumulate.wmthyr <- read.csv("errors_accumulate_wmthyr.csv")

errors.permonth <- cbind(errors.permonth,data.frame(run=rep("Each Month",nrow(errors.permonth))))
errors.accumulate <- cbind(errors.accumulate,data.frame(run=rep("All History",nrow(errors.accumulate))))
errors.accumulate.wmthyr <- cbind(errors.accumulate.wmthyr,data.frame(run=rep("All History + yr mth",nrow(errors.accumulate.wmthyr))))

errors <- rbind(errors.accumulate,errors.permonth,errors.accumulate.wmthyr)

gg <- ggplot(errors,aes(x=in.sample.error,y=out.sample.error,color=run,shape=type))
gg <- gg + geom_point(alpha=1)
gg <- gg + coord_cartesian(ylim = c(0,200),xlim = c(0,200))
gg <- gg + geom_abline(intercept = 0, slope = 1, linetype = "dashed")
plot(gg)

