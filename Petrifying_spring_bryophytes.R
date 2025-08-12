#Title: Bryophytes as Functional Regulators in Petrifying Springs: A Bibliometric Perspective on Ecosystem Services and Heritage Value
#Authors: Yuanyuan Zhou, Qiang Wei, Zhihui Wang and Zhaohui Zhang
#R code implementation and inspection: Yuanyuan Zhou (1181559954@qq.com); and Qiang Wei (wq_tendermoments@163.com)
#Date: July 2025
#Correspondence: Zhaohui Zhang (zhaozhang9@hotmail.com)


library(rnaturalearth) # The world map data is sourced from the built-in data of the package.
library(readxl)
library(sf) 
library(ggspatial)
library(ggplot2)
library(bibliometrix)
library(tidyverse)
library(cols4all)
library(ggwordcloud)
library(igraph)
library(treemapify)


#### 1 Mapping the Global Distribution of Travertine/Tufa Landscapes (fig.2 i/j/k) ####
#library(rnaturalearth)
world_map1 <- ne_countries(scale = 50, returnclass = "sf")
world_map <- dplyr::select(world_map1, geounit) %>% filter(geounit != "Antarctica")

#library(readxl)
moss <- read_excel("tufa_map.xlsx") #The data is collected from online sources and may not be complete.
moss$longitude <- as.numeric(moss$longitude)
moss$latitude  <- as.numeric(moss$latitude )

#library(sf)  
moss_GPS <-  st_as_sf(moss, coords = c("longitude", "latitude"), crs = 4326)
st_crs(world_map) == st_crs(moss_GPS) 

#library(ggspatial)
#library(ggplot2)
world <- ggplot() +
  geom_sf(data = world_map, color = "white", fill = "#E1E1E0", lwd = 0.1) +
  coord_sf(crs = 4326, xlim = c(-180, 180), ylim = c(-55, 83), clip = "on" )+
  geom_point(data = moss, aes(x = longitude, y = latitude, color = group),size = 2, shape = 16) +
  scale_color_manual(values = c("A" = "#FF6B6B", "B" = "#53AD65"))+
  annotation_north_arrow(location = "tl", which_north = "true",height = unit(0.6, "cm"),width = unit(0.6, "cm"), style = north_arrow_orienteering()) +
  xlab("Longitude") + ylab("Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#6D90BF"), plot.background = element_blank(),
        panel.grid = element_blank(),plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.position = "none")
world
#(fig.2 i) end

Europe <- world +coord_sf(crs = 4326, xlim = c(-10, 32), ylim = c(35, 60))
Europe
#(fig.2 j) end

Asia <- world +coord_sf(xlim = c(70, 140), ylim = c(15, 55))
Asia
#(fig.2 k) end

#ggsave(filename = "Europe.pdf", plot = Europe, device = "pdf", width = 12, height = 8, units = "cm", dpi = 300) 
#ggsave(filename = "world.pdf", plot = world, device = "pdf", width = 12, height = 6, units = "cm", dpi = 300) 
#ggsave(filename = "Asia.pdf", plot = Asia, device = "pdf", width = 12, height = 8, units = "cm", dpi = 300) 




#### 2 Descriptive analysis of metadata ####
library(bibliometrix)
biblioshiny() #Importantly, you can access data analysis results via the function’s interactive browsing window.
library(tidyverse)
metadata <- convert2df(file="D:/桌面/周园园博士资料/11 zyy_综述/tufa_moss_bibliometrix/scopus.bib",
                       dbsource = "scopus", format = "bibtex",remove.duplicates = TRUE)
#write_excel_csv(metadata, "scopus_articles_metadata.csv")
missingData(metadata)

description <- biblioAnalysis(M = metadata, sep = ';')  
description_sum <- summary(object = description, k = 15, pause = F)  
description_sum$MostProdCountries
plot(x=description, k = 10, pause = F)




#### 3 Annual Publication (fig. 4a) and Journal Productivity Analysis (fig. 4b) ####
produ <- data.frame(Year = description_sum$AnnualProduction$`Year   `,
                    Articles = description_sum$AnnualProduction$Articles,
                    cumArticles = cumsum(description_sum$AnnualProduction$Articles))
produ <- produ[-nrow(produ), ]

theme_bw1 <- function() {
  theme_bw() +
    theme(panel.background = element_blank(), plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(linewidth = 1, color = "black", fill = NA),
          axis.ticks = element_line(size = 0.6, color = "black"),
          axis.ticks.length = unit(-0.15, "cm"),
          axis.text.x = element_text(margin = margin(t = 5), size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"),
          axis.title = element_blank(),
          legend.position = "none")
} #A custom function to create a consistent image appearance.

Publication_count <- ggplot(produ, aes(x = as.numeric(as.character(Year)))) +
  #geom_line(aes(y = Articles), color = "red", linewidth = 1) +  
  geom_smooth(aes(y = Articles, color = "Annual publication number"), method = "loess", se = FALSE, color = "#66c2a5", span = 0.2,linewidth = 1) +  
  geom_smooth(aes(y = (cumArticles+5)/16, color = "Cumulative publication number"), method = "loess", se = FALSE, color = "#1f78b4", span = 0.5,linewidth = 1)+
  scale_y_continuous(name = "Annual publication number", breaks = seq(0,8,1), sec.axis = sec_axis(~.*16-5, name = "Cumulative publication number",breaks = seq(0,125,20)))+
  scale_x_continuous(breaks = seq(1990, 2025, 3))+
  theme_bw1()+
  theme(axis.text.x = element_text(angle = 90, size = 9, color = "black"),
        axis.text.y.left = element_text(margin = margin(r = 4)),
        axis.text.y.right =element_text(margin = margin(l = 4,r = 4)))
Publication_count

#ggsave(filename = "Publication_count.pdf", plot = Publication_count, device = "pdf", width = 9, height = 6, units = "cm", dpi = 300)
#(fig. 4a) end


journal_produ <- data.frame(Sources = as.character(description_sum$MostRelSources$`Sources       `),
                            Articles = as.numeric(description_sum$MostRelSources$Articles))
journal_produ$Sources <- factor(journal_produ$Sources, levels = rev(journal_produ$Sources))

library(cols4all)
c4a_gui()
c4a_palettes()  
c4a("sunset",5)
c4a_plot("sunset",20)
c4a_palettes(type = "seq")

Journal_productivity <- ggplot(journal_produ,aes(x = Articles,y = Sources)) +
  geom_col(aes(fill = Articles), width = 0.1)+
  geom_point(aes(size = Articles, color = Articles))+
  geom_text(aes(label = Articles),size = 3,color = "white",vjust = 0.4,fontface = "bold")+
  scale_size_continuous(range = c(4, 8)) +
  scale_x_continuous(breaks = seq(0, 10, 2))+
  scale_fill_continuous_c4a_div("sunset", mid = 0)+
  scale_color_continuous_c4a_div("sunset", mid = 0)+
  theme_bw1()
Journal_productivity

#ggsave(filename = "Journal_productivity .pdf", plot = Journal_productivity , device = "pdf", width =20, height = 11, units = "cm", dpi = 300)
#(fig. 4b) end




#### 4 Author productivity analysis(fig. 5b) and Authors' Production over Time (fig. 5a) ####
author_produ <- data.frame(Authors  = as.character(description_sum$MostProdAuthors$`Authors       `),
                           Articles = as.numeric(description_sum$MostProdAuthors$Articles))
author_produ$Authors <- factor(author_produ$Authors, levels = rev(author_produ$Authors))

Author_productivity <- ggplot(author_produ,aes(x = Articles,y = Authors)) +
  geom_col(aes(fill = Articles), width = 0.1)+
  geom_point(aes(size = Articles, color = Articles))+
  geom_text(aes(label = Articles),size = 3,color = "white",vjust = 0.4,fontface = "bold")+
  scale_size_continuous(range = c(4, 8)) +
  scale_x_continuous(breaks = seq(0, 10, 2))+
  scale_fill_continuous_c4a_div("sunset", mid = 0)+
  scale_color_continuous_c4a_div("sunset", mid = 0)+
  theme_bw1()
Author_productivity

#ggsave(filename = "Author_productivity .pdf", plot = Author_productivity , device = "pdf", width =20, height = 11, units = "cm", dpi = 300)
#(fig. 5b) end


topAU <- authorProdOverTime(metadata,k=15,graph=TRUE)
dfAU <- topAU$dfAU
authors_by_freq <- dfAU %>% group_by(Author) %>%
  summarise(total_freq = sum(freq)) %>%
  arrange(total_freq, desc(Author)) %>% pull(Author)

time_trends <- dfAU %>%
  ggplot(aes(x = year, y = Author)) +
  geom_line(aes(group = Author), color = "gray",linewidth=1, alpha = 0.5) + 
  geom_point(aes(size = freq, color = TCpY)) +  
  scale_color_gradient(low = "#FFB000", high = "#DC267F") + 
  scale_size_continuous(range = c(2,6)) +  
  scale_x_continuous(breaks = seq(1990, 2024, by = 2))+
  scale_y_discrete(limits = authors_by_freq) + 
  theme_bw1() +  
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) 
time_trends

#ggsave(filename = "Authors_Publication_Over_Time .pdf", plot = time_trends, device = "pdf", width =18, height = 12, units = "cm", dpi = 300)
#(fig. 5a) end



#### 6 Countries' Scientific Production (fig. 6a) and Top Cited Countries (fig. 6b/c) ####
Country_Production <- read_excel("Country_Production_bibliometrix.xlsx") #Data exported from the Biblioshiny window.
Country_Production1 <- Country_Production %>% left_join(world_map1, by = c("Country1" = "geounit")) %>% st_as_sf()

breaks <- c(1, 4, 10, 20, 30, 50)  
labels <- c("1-4", "5-10", "11-20", "21-30", "31-50")

Country_Production1 <- Country_Production1 %>% mutate(Freq_bin = cut(Freq, breaks = breaks, labels = labels, include.lowest = TRUE))

Country_scientific_production <- ggplot() +
  geom_sf(data = world_map, color = "white", fill = "#E1E1E0", lwd = 0.2) +
  geom_sf(data = Country_Production1, aes(fill = Freq_bin), color = "white", size = 0.2) +
  scale_fill_brewer(name = "Frequency",palette = "RdPu", na.value = "grey30", guide = guide_legend(title.position = "top", label.position = "bottom", override.aes = list(size = 1), keywidth = unit(0.3, "cm"))) +
  geom_sf_label(data = country_centers %>% filter(Freq >= 0), aes(label = Freq),fill = NA,color = "grey0", size = 2,label.size = 0,fontface = "bold") +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(0.6, "cm"),width = unit(0.6, "cm"), style = north_arrow_orienteering()) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#A8C1DC"), plot.background = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.width = unit(1, "cm"), plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))
Country_scientific_production

Europe1 <- Country_scientific_production +
  coord_sf(crs = 4326, xlim = c(-10, 26), ylim = c(36, 60))+
  theme(legend.position = "none")
Europe1

#ggsave(filename = "Country_scientific_production.pdf", plot = Country_scientific_production, device = "pdf", width = 14, height = 10, units = "cm", dpi = 300) 
#ggsave(filename = "Europe1.pdf", plot = Europe1, device = "pdf", width = 8, height = 7, units = "cm", dpi = 300) 
#(fig. 6a) end


library(treemapify)
ProdCountries <- data.frame(description_sum1$MostProdCountries)
ProdCountries1 <- data.frame(Country = as.character(ProdCountries$Country),
                             Articles = as.numeric(ProdCountries$Articles),
                             Freq =  as.numeric(ProdCountries$Freq),
                             SCP = as.numeric(ProdCountries$SCP),# Single Country Publications
                             MCP = as.numeric(ProdCountries$MCP),# Multiple Country Publications
                             MCP_Ratio = as.numeric(ProdCountries$MCP_Ratio))
ProdCountries1$Continent = c("Europe", "Europe", "Europe", "Europe", "North America",
                             "Europe", "South America", "Europe", "Asia", "Europe",
                             "Europe", "Europe", "Europe", "Africa", "Asia", "Europe",
                             "Asia", "Asia", "North America", "Europe", "Africa", "North America") 
ProdCountries1$Continent <- factor(ProdCountries1$Continent, levels = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"))

MostProdCountries <- ggplot(ProdCountries1, aes(area = Articles, fill = Continent, label = paste(Country, Articles, sep = "\n"), subgroup = Continent)) +
  geom_treemap(colour = "grey90", size = 1.5)+
  geom_treemap_text(colour = "white",place = "topleft",size = 8, fontface = "bold" ,grow = F, reflow = F)+
  geom_treemap_subgroup_border(colour = "white", size = 3.5) +
  geom_treemap_subgroup_text(place = "bottom", grow = F, alpha = 0.5, size = 14, colour = "black",fontface = "italic") +
  scale_fill_manual(values = c("Asia" = "#E41A1C","Europe" = "#377EB8","North America" = "#4DAF4A", "South America" = "#FF7F00", "Africa" = "#F781BF")) +
  theme_bw1()
MostProdCountries

#ggsave(filename = "Most_Production_Countries.pdf", plot =  MostProdCountries, device = "pdf", width =16, height = 12, units = "cm", dpi = 300)
#(fig. 6b) end


Corresponding_Author_Countries <- ProdCountries1 %>% arrange(desc(Articles)) %>%  
  mutate(Country = factor(Country, levels = rev(unique(Country)))) %>%  
  pivot_longer(cols = c(SCP, MCP), names_to = "Publication_Type", values_to = "Count") %>%  
  ggplot(aes(x = Count, y = Country, fill = Publication_Type)) +
  geom_col(position = "stack",width = 0.8) +
  geom_text(aes(label = ifelse(Count > 0, Count, "")), position = position_stack(vjust = 0.5), size = 2.5, color = "white",fontface = "bold") +
  scale_fill_manual(values = c("SCP" = "#377EB8", "MCP" = "#E41A1C"), labels = c("Single Country", "Multiple Countries"))+
  theme_bw1()
Corresponding_Author_Countries 

#ggsave(filename = "Corresponding_Author_Countries.pdf", plot =  Corresponding_Author_Countries, device = "pdf", width =14, height = 10, units = "cm", dpi = 300)
#(fig. 6c) end




#### 7 Keyword cloud (fig. 7a), Network (fig. 7b) and Trend Topics (fig. 7c) ####
description_sum1 <- summary(object = description, k = 124, pause = F)  

Keywords <- data.frame(description_sum1$MostRelKeywords)
Keywords1 <- data.frame(word = as.character(Keywords$Keywords.Plus..ID.....),
                        freq = as.numeric(Keywords$Articles.1),
                        word1 = as.character(Keywords$Author.Keywords..DE......),
                        freq1 = as.numeric(Keywords$Articles))


# Expanded core keywords
library(ggwordcloud)
Word_Cloud_ID <- ggplot(Keywords1, aes(label = word, size = freq, color = freq, label_content = sprintf("%s (%.0f)", word, freq)))+
  geom_text_wordcloud_area(family = "sans",fontface = "bold",max_steps = 1000, grid_size = 0.5, mask = png::readPNG(system.file("extdata/hearth.png", package = "ggwordcloud", mustWork = TRUE)), rm_outside = T) + 
  scale_size_area(max_size = 20) +
  scale_color_gradientn(colors = c4a("matplotlib.viridis",15, reverse = F))+ 
  theme_void()
Word_Cloud_ID

#ggsave(filename = "Word_Cloud_ID.pdf", plot =  Word_Cloud_ID, device = "pdf", width =10, height = 10, units = "cm", dpi = 300)
#(fig. 7a) end


net_matrix <- biblioNetwork(metadata, analysis = "co-occurrences", network = "keywords", sep = ";")
net_matrix <- as.matrix(net_matrix)
top_30_keywords <-names(sort(rowSums(net_matrix), decreasing = TRUE)[1:30])

net <- networkPlot(net_matrix, normalize="association",weighted=T, n = 30, 
                   type = "fruchterman", label = T, label.cex = F,
                   label.n = 30, halo = F,cluster = "walktrap",
                   community.repulsion = 0.5,
                   size=T, size.cex = T,curved = T, edgesize = 5,
                   remove.multiple = F, alpha = 0.7, labelsize=0.7)

library(igraph)
g <- net$graph
communities <- net$cluster_obj
num_communities <- length(unique(communities$membership))
custom_colors <- c("#6A5ACD", "#20B2AA", "#FF69B4")
V(g)$color <- custom_colors[communities$membership]

edge_communities <- ends(g, E(g), names = FALSE)
E(g)$is_cross <- V(g)$community[edge_communities[,1]] != V(g)$community[edge_communities[,2]]
E(g)$color <- ifelse(E(g)$is_cross, "grey90", "#EDC948") 
E(g)$lty <- ifelse(E(g)$is_cross, "dashed", "solid")     
E(g)$width <- ifelse(E(g)$is_cross, 0.5, 1)        

pdf("Cooccurrence_Network1.pdf", width = 8, height = 8)
Cooccurrence_Network1 <- plot(g,layout = layout_with_fr(g, niter = 5000, repulserad = vcount(g)^3),
                              vertex.color = V(g)$color, vertex.frame.color = V(g)$color, vertex.label.color = "grey20",   
                              vertex.label.cex = 0.8, edge.color = E(g)$color, edge.lty = E(g)$lty,             
                              edge.width = E(g)$width, edge.curved = 0.2, margin = c(0, 0, 0, 0))
dev.off()
#(fig. 7b) end


Thematic_trends <- read_excel("TrendTopic_bibliometrix.xlsx") #Data exported from the Biblioshiny window.
Thematic_trends$term_id <- 1:nrow(Thematic_trends)

Thematic_trends_p <- ggplot(Thematic_trends) +
  geom_segment(aes(x = Year_Q1, xend = Year_Q3, y = term_id, yend = term_id), color = "#2166AC", alpha = 0.8, size = 0.7) +
  geom_point(aes(x = Year_Median, y = term_id, size = Frequency), alpha = 0.7,color = "#377EB8") +
  scale_y_continuous(breaks = Thematic_trends$term_id, labels = Thematic_trends$Term) +
  scale_size_continuous(range = c(2, 4)) +
  scale_x_continuous(breaks = seq(1990, 2025, 3))+
  theme_bw1()+
  theme(axis.text.x = element_text(angle = 90, size = 9, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.text.y.left = element_text(margin = margin(r = 4)),
        axis.text.y.right =element_text(margin = margin(l = 4,r = 4)))
Thematic_trends_p

#ggsave(filename = "Thematic_trends.pdf", plot = Thematic_trends1, device = "pdf", width = 14, height = 18, units = "cm", dpi = 300) 
#(fig. 7c) end




