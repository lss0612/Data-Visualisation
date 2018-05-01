##Quick graphic for natural resources data

library(ggplot2)
fulldata <-read.delim(https://github.com/lss0612/Natural-Resources/blob/master/fulldata.csv)


p <- ggplot(data = fulldata,
            mapping = aes(x = oilproduction,
                          y = Real.GDP.capita)) 
p + geom_point(color = "purple") +
  geom_smooth(method = "loess") + scale_x_log10() + scale_y_log10()