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
                             label = TRUE,
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
figure.4.data[,Value:=Value*100]
figure.4.data[,Answer:=reorder(Answer,c(1,2,3,4,5))]
figure.4 <- plot.column.dais(figure.4.data,
                             Value,
                             Answer,
                             y.axis=graph.data[Figure_number=="Figure 4",Y_Axis],
                             label=TRUE,
                             label.unit = "%",
                             plot.title = str_wrap(graph.data[Figure_number=="Figure 4",Figure_title],80),
                             plot.fig.num = "Figure 4",
                             caption=graph.data[Figure_number=="Figure 4",Caption]) +
  scale_y_continuous(expand=c(0,0),limits=c(0,35),breaks=c(0,10,20,30),labels=c("0%","10%","20%","30%"))

########################################################
figure.5.data <- fread("Data/Figure_5.csv")
figure.5.data[,Year:=as.character(Year)]
figure.5.data <- figure.5.data[!(Category %in% c("(Other)","Travel","Human capital","Office management"))]
figure.5 <- plot.column.dais(figure.5.data,
                             Value,
                             Category,
                             group.by = Year,
                             order.bar = "descending",
                             colours = set.colours(4,categorical.choice = c("hot.pink","light.blue","blue","gold")),
                             y.axis = graph.data[Figure_number=="Figure 5",Y_Axis],
                             label.unit = "$",
                             plot.title = graph.data[Figure_number=="Figure 5",Figure_title],
                             plot.fig.num = "Figure 5",
                             caption=graph.data[Figure_number=="Figure 5",Caption])

########################################################
figure.6.data <- fread("Data/Figure_6.csv")
figure.6.data[,Share_digital:=Share_digital*100]
figure.6.data[,Industry:=str_wrap(Industry,40)]
figure.6.data[,fed:="Not Fed"]
figure.6.data[Industry=="Federal public service",fed:="Fed"]
figure.6 <- plot.column.dais(figure.6.data,
                             Share_digital,
                             Industry,
                             order.bar = "ascending",
                             group.by = fed,
                             colours = set.colours(2,categorical.choice=c("hot.pink","black")),
                             y.axis = graph.data[Figure_number=="Figure 6",Y_Axis],
                             label.unit = "%",
                             plot.title = graph.data[Figure_number=="Figure 6",Figure_title],
                             plot.fig.num = "Figure 6",
                             caption = graph.data[Figure_number=="Figure 6",Caption]) + guides(fill="none")

#########################################################
figure.7.data <- fread("Data/Figure_7.csv")
figure.7.data[,Value:=Value*100]
figure.7.data[,Activity:=str_wrap(Activity,30)]
figure.7 <- plot.column.dais(figure.7.data,
                             Value,
                             Activity,
                             group.by=Answer,
                             stacked=TRUE,
                             colours = set.colours(3,categorical.choice=c("hot.pink","light.blue","gold")),
                             y.axis = graph.data[Figure_number=="Figure 7",Y_Axis],
                             label.unit = "%",
                             label = TRUE,
                             plot.title = graph.data[Figure_number=="Figure 7",Figure_title],
                             plot.fig.num = "Figure 7",
                             caption = graph.data[Figure_number=="Figure 7",Caption])

#########################################################
figure.8.data <- fread("Data/Figure_8.csv")
figure.8.data[,can_dum:="Not Canada"]
figure.8.data[Country=="Canada",can_dum:="Canada"]
figure.8 <- plot.column.dais(figure.8.data,
                           Value,
                           Country,
                           group.by=can_dum,
                           colours = set.colours(2,categorical.choice=c("hot.pink","black")),
                           y.axis = graph.data[Figure_number=="Figure 8",Y_Axis],
                           plot.title = graph.data[Figure_number=="Figure 8",Figure_title],
                           order = "ascending",
                           plot.fig.num = "Figure 8",
                           caption = graph.data[Figure_number=="Figure 8",Caption])+ guides(fill="none") +
  theme(axis.text.x = ggplot2::element_text(size=8, margin=ggplot2::margin(t=2), family = "Replica-Light"))


#########################################################
figure.9.data <- fread("Data/Figure_9.csv")
figure.9 <- plot.column.dais(figure.9.data,
                             Value,
                             Type,
                             plot.title = graph.data[Figure_number=="Figure 9",Figure_title],
                             y.axis = graph.data[Figure_number=="Figure 9",Y_Axis],
                             order = "ascending",
                             label = TRUE,
                             plot.fig.num = "Figure 9",
                             caption = graph.data[Figure_number=="Figure 9",Caption]) +
  scale_y_continuous(expand=c(0,0),limits = c(0,1.1),breaks=c(0,0.25,0.5,0.75,1),labels=c("0","0.25","0.50","0.75","1.00"))


#########################################################
figure.10.data <- fread("Data/Figure_10.csv",header=TRUE)
figure.10.data[,Year:=as.character(Year)]
figure.10 <- plot.column.dais(figure.10.data,
                              Value,
                              Year,
                              group.by = Geography,
                              label.unit="%",
                              label = TRUE,
                              plot.title = graph.data[Figure_number=="Figure 10",Figure_title],
                              y.axis = graph.data[Figure_number=="Figure 10",Y_Axis],
                              colours = set.colours(3,categorical.choice = c("hot.pink","black","gold")),
                              plot.fig.num = "Figure 10",
                              caption = graph.data[Figure_number=="Figure 10",Caption])

#########################################################
figure.11.data <- fread("Data/Figure_11.csv")
figure.11.data[,Coverage:=Coverage*100]
figure.11 <- plot.column.dais(figure.11.data,
                              Coverage,
                              Geography,
                              label = TRUE,
                              label.unit = "%",
                              plot.title = graph.data[Figure_number=="Figure 11",Figure_title],
                              y.axis = graph.data[Figure_number=="Figure 11",Y_Axis],
                              plot.fig.num = "Figure 11",
                              order="ascending",
                              caption = graph.data[Figure_number=="Figure 11",Caption])


#########################################################
#Export all the graphs

export.dais.plot("Exported/Figure_1.pdf",figure.1)
export.dais.plot("Exported/Figure_2.pdf",figure.2)
export.dais.plot("Exported/Figure_3.pdf",figure.3)
export.dais.plot("Exported/Figure_4.pdf",figure.4)
export.dais.plot("Exported/Figure_5.pdf",figure.5)
export.dais.plot("Exported/Figure_6.pdf",figure.6)
export.dais.plot("Exported/Figure_7.pdf",figure.7)
export.dais.plot("Exported/Figure_8.pdf",figure.8)
export.dais.plot("Exported/Figure_9.pdf",figure.9)
export.dais.plot("Exported/Figure_10.pdf",figure.10)
export.dais.plot("Exported/Figure_11.pdf",figure.11)





