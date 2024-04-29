library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)



data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# Package
library(networkD3)

# I need a long format ----
data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, 
              colourScale=ColourScal, 
              nodeWidth=40, fontSize=13, nodePadding=20)



# oki let's go ----
# data_sankeykey <- read.csv("analysis/sankey.csv", header = TRUE)
# data_sankeykey <- data_sankeykey[1:18, 1:12]

data_long_sankeykey <- read.csv("analysis/sankey4.csv", header = TRUE)
data_long_sankeykey <- data_long_sankeykey[, 1:3]

data_long_sankeykey$target <- paste(data_long_sankeykey$target, " ", sep="")

# data_long_sankeykey_pos <- data_long_sankeykey %>%
#   filter(value > 0)

data_long_sankeykey$value <- abs(data_long_sankeykey$value)

nodes_key <- data.frame(name=c(as.character(data_long_sankeykey$source), as.character(data_long_sankeykey$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long_sankeykey$IDsource=match(data_long_sankeykey$source, nodes_key$name)-1 
data_long_sankeykey$IDtarget=match(data_long_sankeykey$target, nodes_key$name)-1


# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#44781E", "#7D9D33", "#CED38C", "#DCC949", "#BCA888", "#CD8862", "#775B24"])'


sankey <- sankeyNetwork(Links = data_long_sankeykey, Nodes = nodes_key,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, 
              colourScale=ColourScal, 
              nodeWidth=40, fontSize=13, nodePadding=20)

library(htmlwidgets)
library(webshot)
# webshot::install_phantomjs()

saveWidget(sankey, "sankey_plot.html")

# Capture screenshot of the HTML widget and save as PNG
webshot("sankey_plot.html", "sankey_plot.png")


# trying with colours 

data_long_sankeykey1 <- read.csv("analysis/sankey3.csv", header = TRUE)
data_long_sankeykey1 <- data_long_sankeykey1[, 1:4]

data_long_sankeykey1$target <- paste(data_long_sankeykey1$target, " ", sep="")

# absolute values 
data_long_sankeykey1$value <- abs(data_long_sankeykey1$value)

nodes_key1 <- data.frame(name=c(as.character(data_long_sankeykey1$source), as.character(data_long_sankeykey1$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long_sankeykey1$IDsource=match(data_long_sankeykey1$source, nodes_key1$name)-1 
data_long_sankeykey1$IDtarget=match(data_long_sankeykey1$target, nodes_key1$name)-1


ColourScal1 <- 'd3.scaleOrdinal().range(["red", "green"])'

get_color <- function(sign) {
  if (sign == "pos") {
    return("green")
  } else {
    return("red")
  }
}

sankeyNetwork(
  Links = data_long_sankeykey1,
  Nodes = nodes_key1,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  sinksRight = FALSE,
  colourScale = ColourScal1,
  nodeWidth = 40,
  fontSize = 13,
  nodePadding = 20
)





impact_colors <- data.frame(
  impact = c("earthworms", "vegetation diversity", "extractable organic N", "soil basification", "soil solution P", "N2O emissions", "leaching N", "leaching P", "nutrient N", "carbon emissions", "organic matter", "soil acidification", "soil structure reduction",  "nutrient P", "carbon sequestration", "CH4 sequestration", "runoff", "ant fauna"),  # Replace with your impact names
  color = c("#44781E", "#44781E", "#44781E", "#A7473A", "#A7473A", "#A7473A", "#A7473A", "#A7473A", "#44781E", "#A7473A", "#44781E", "#A7473A", "#A7473A", "#44781E", "#44781E", "#44781E","#A7473A","#44781E")   
)

map_impact_colors <- function(impact) {
impact_colors <- c(
  "earthworms" = "#44781E",
  "vegetation diversity" = "#44781E",
  "extractable organic N" = "#44781E",
  "soil basification" = "#A7473A",
  "soil solution P" = "#44781E",
  "N2O emissions" = "#44781E",
  "leaching N" = "#44781E",
  "leaching P" = "#44781E",
  "nutrient N" = "#44781E",
  "nutrient P" = "#44781E",
  "carbon emissions" = "#44781E",
  "organic matter" = "#44781E",
  "soil acidification" = "#44781E",
  "soil structure reduction" = "#44781E",
  "carbon sequestration" = "#44781E",
  "CH4 sequestration" = "#44781E",
  "runoff" = "#44781E",
  "ant fauna" = "#44781E")
return(ifelse(impact %in% names(impact_colors), impact_colors[impact], "purple"))
}

nodes_key$color <- ifelse(nodes_key$name %in% names(impact_colors), 
                          impact_colors[nodes_key$name],
                          "black")

nodes_key$color <- map_impact_colors(nodes_key$name)

sankeyNetwork(
  Links = data_long_sankeykey, 
  Nodes = nodes_key,
  Source = "IDsource", 
  Target = "IDtarget",
  Value = "value", 
  NodeID = "name",
  sinksRight = FALSE, 
  nodeWidth = 40, 
  fontSize = 13, 
  nodePadding = 20,
  NodeGroup = "color" # Specify the color column in the Nodes data frame
)
