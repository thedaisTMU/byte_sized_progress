library(DaisTheme)
library(stringr)
library(ggplot2)
library(data.table)

graph.data <- fread("Graph_spreadsheet.csv")
###########################
figure.1.data <- fread("Data/Figure_1.csv")
figure.1.data[,can_dum:="Not Canada"]
figure.1.data[Country=="Canada",can_dum:="Canada"]
figure.1 <- plot.column.dais(figure.1.data,
                             E_Govt_Index,
                             Country,
                             group.by=can_dum,
                             colours=set.colours(2,categorical.choice=c("hot.pink","black")),
                             order.bar="descending",
                             plot.title=graph.data[Figure_number=="Figure 1",Figure_title],
                             plot.fig.num=graph.data[Figure_number=="Figure 1",Figure_number],
                             caption=graph.data[Figure_number=="Figure 1",Caption]) +
  guides(fill="none")


######################################################
figure.2.data <- fread("Data/Figure_2.csv")
figure.2.data[,Index:=str_wrap(Index,30)]
figure.2 <- plot.column.dais(figure.2.data,
                             Value,
                             Index,
                             colours=set.colours(1,categorical.choice="hot.pink"),
                             plot.title=graph.data[Figure_number=="Figure 2",Figure_title],
                             plot.fig.num="Figure 2",
                             caption=graph.data[Figure_number=="Figure 2",Caption])

#######################################################
figure.3.data <- fread("Data/Figure_3.csv")
figure.3.data[,can_dum:="Not Canada"]
figure.3.data[Country=="Canada",can_dum:="Canada"]
figure.3 <- plot.column.dais(figure.3.data,
                             Online_Serv_Index,
                             Country,
                             group.by=can_dum,
                             y.axis=graph.data[Figure_number=="Figure 3",Y_Axis],
                             colours=set.colours(2,categorical.choice=c("hot.pink","black")),
                             order.bar = "descending",
                             plot.title=graph.data[Figure_number=="Figure 3",Figure_title],
                             plot.fig.num="Figure 3",
                             caption=graph.data[Figure_number=="Figure 3",Caption]) + guides(fill="none")

#######################################################
figure.4.data <- fread("Data/Figure_4.csv") #Need to reorder the columns
figure.4 <- plot.column.dais(figure.4.data,
                             Value,
                             Answer,
                             y.axis=graph.data[Figure_number=="Figure 4",Y_Axis],
                             plot.title = graph.data[Figure_number=="Figure 4",Figure_title],
                             plot.fig.num = "Figure 4",
                             caption=graph.data[Figure_number=="Figure 4",Caption])

















