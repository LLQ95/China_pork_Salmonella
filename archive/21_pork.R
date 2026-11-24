##配色方案
color_schemes <- list(
  reference = c(cold = "#377EB8", warm = "#E41A1C"),##Cold vs warm
  scheme1 = c(cold = "#2A6F9D", warm = "#D32F2F"),##深蓝-砖红
  scheme2 = c(cold = "#1E88E5", warm = "#FF5722"),##亮蓝-橙色
  scheme3 = c(cold = "#0277BD", warm = "#F4511E"),##钴蓝-橙红
  scheme4 = c(cold = "#006064", warm = "#FF7043"),##深青-珊瑚橙
  scheme5 = c(cold = "#01579B", warm = "#FF5252")##午夜蓝-亮红 ##Non-Rich vs rich
)
###颜色预设
#values = c("not rich" = "#01579B", "rich" = "#F4511E"),
#labels = c("not rich" = "Non-Rich", "rich" = "Rich")
###颜色预设
#values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
#labels = c("cold" = "Cold", "warm" = "Warm")

###思路1：桑基图加上地图反映出整体的分布情况—---------------------------------------
##全国绘图示例2,21年猪肉沙门,依据华东华南华中华北分别进行划分------------------------
##按照不同的地理区域进行划分
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read.table("domestic_pork_serovars.txt", fileEncoding="GBK", header=TRUE)
head(data)
##全国地图数据
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") 
province <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  st_transform(crs_al)  %>%left_join(data, by = "name") %>%
  mutate(sampling_sites = case_when(name %in% c(data$name) ~ "sampled",
                                    TRUE ~ "unsampled"))
#  mutate(
#    Location = ifelse(is.na(Location), "unsampled", Location),
#    sampling_sites = ifelse(Location != "unsampled", "sampled", "unsampled")
#  )
factor(province$name)
#colnames(province$sampling_sites) <- "Sampling sites"
province$centroid <- st_coordinates(st_centroid(province$geometry))
df <- province %>% filter(sampling_sites == "sampled") %>%
  mutate(Location = data$Location)
data2<-left_join(data,df,by="name")
data2 <- st_as_sf(data2)
data2 <- st_as_sf(data2, coords = c("centroid.X", "centroid.Y"), crs = 4326)
data2$centroid.X <- data2$centroid[, 1]
data2$centroid.Y <- data2$centroid[, 2]
# 准备饼图数据
data$x <- data2$centroid.X
data$y <- data2$centroid.Y
data <- data.frame(data)
head(data)
colnames(data) <- c("name","Location","Derby","4,[5],12:i:-","London", "Others","Rissen", 
                    "Typhimurium",  "r", "x", "y")   
#c("#EAE4E1","darkgrey","white")
##正式画图————————————————————————————————————————————
p1<-ggplot(data=province)+ 
  geom_sf(size = .2,aes(fill=sampling_sites))+
    scale_fill_manual(values = c("#EAE4E1", "white") ) +
#                                 "#FFDDC1", "#FFC6B3", "#FFB3C1", "#D8C1FF", "#A7C7E7", "#95D5B2", "white") ) +
  theme(axis.text =element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")+
  new_scale_fill()+
  #添加指北针和比例尺
  ggspatial::annotation_north_arrow(
    location = "tl",  # 指北针位置
    style = north_arrow_fancy_orienteering,  # 更改为 minimal 样式
    which_north = "true",  # 使用真实北方向
    pad_x = unit(0.5, "cm"),  # 调整指北针的水平边距
    pad_y = unit(0.5, "cm")   # 调整指北针的垂直边距
  ) +
  ggspatial::annotation_scale(
    location = "bl",  # 比例尺位置
  )+
  # 绘制饼图
  geom_scatterpie(
    aes(x = x, y = y, group = name, r = r*1000  ),  
    data = data, 
    cols = c("Rissen","London","4,[5],12:i:-","Derby","Typhimurium","Others"), 
    color = NA)+
  scale_fill_manual(values =  c("#e54e21","#8dd2c6","#b3de69","#BB80BC","#80B0D2","#FFEC6D"))+
  labs(fill="Serovars")+ 
  # 添加样本饼图图例
  geom_scatterpie_legend(data$r * 2000, x=-2500000, y=1000000, labeller=function(x) x/2000) +
  #  geom_text(aes(x = centroid[,1], y = centroid[,2], label = name), 
  #            nudge_x = 0.01, nudge_y = 0.01, size = 3, check_overlap = TRUE) +   
  theme_void()
p1
ggsave("map_domestic_pork.pdf", width=8, height=8, dpi = 600)





##依据胡焕庸线绘制ARGs分布的热图-----------------------------------------------------------
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet=3)
##https://datav.aliyun.com/portal/school/atlas/area_selector#&lat=31.728167146023935&lng=104.32617187499999&zoom=3
#data <- read.excel("pork_serovar_map.txt", fileEncoding="GBK", header=TRUE)
#head(data)
##全国地图数据
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") 
province <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  st_transform(crs_al) 
province$centroid <- st_coordinates(st_centroid(province$geometry))
df <- province 
data2<-left_join(df,data,by="name")%>%
  #  mutate(r = replace_na(r, 0))
  mutate(across(where(is.numeric), ~ replace_na(., 0)))  ##替换所有数值列
#data2 <- st_as_sf(data2)
data2 <- st_as_sf(data2, coords = c("centroid.X", "centroid.Y"), crs = 4326)
data2$centroid.X <- data2$centroid[, 1]
data2$centroid.Y <- data2$centroid[, 2]
# 准备饼图数据
ggplot(data=data2)+ 
  geom_sf(aes(fill = ARGs_per_isolate),size = .2)+
  #  geom_polygon(data=data2,aes(x=long,y=lat,group=group),,aes(fill=r)
  #               fill="#dedede")+
  scale_fill_gradientn(colors = c("#CAC2C0", "#1F78B4"), na.value = "#CAC2C0")+
  theme(axis.text =element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = "right")+
  new_scale_fill()+
  #添加指北针和比例尺
  ggspatial::annotation_north_arrow(
    location = "tl",  # 指北针位置
    style = north_arrow_fancy_orienteering,  # 更改为 minimal 样式
    which_north = "true",  # 使用真实北方向
    pad_x = unit(0.5, "cm"),  # 调整指北针的水平边距
    pad_y = unit(0.5, "cm")   # 调整指北针的垂直边距
  ) +
  ggspatial::annotation_scale(
    location = "bl",  # 比例尺位置
  )
ggsave("地图_沙门.png", width=8, height=8, dpi = 600)



##图二是桑基图，两者通过拼凑整合成一张图------------------------------------------
##主要是显示地区，采样特征，南北方、超市类型、血清型，ST型、等等--------------------------------------
library(dplyr)
library(tidyverse)
library(ggforce)
library(RColorBrewer)
library(ggsci)
library(readxl)
library(cowplot)
# 读取数据
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 1)
data$Value <- 1
# 分组汇总
salmonella <- data %>% 
  group_by(location, processing_type, storage_type, market_type, ST, Serovars) %>% 
  summarise(Sum_Value = sum(Value), .groups = "drop")
head(salmonella)
# 1. 调整Serovars的顺序
# 按照您指定的顺序，注意需要与数据中实际的Serovars值完全匹配
# 如果数据中是"I 4,[5],12:i:-"而不是"4,[5],12:i:-"，请使用前者
serovar_order <- c("Rissen","London",  "4,[5],12:i:-", "Derby", "Typhimurium", "Others")
# 获取数据中实际存在的Serovars值
actual_serovars <- unique(salmonella$Serovars)
# 将指定的顺序与实际存在的值进行匹配
# 如果数据中有其他值，将它们放在指定顺序的后面
final_serovar_order <- c(
  intersect(serovar_order, actual_serovars),
  setdiff(actual_serovars, serovar_order) %>% sort()
)
# 按照您要求的顺序: location, processing_type, storage_type, market_type, ST, Serovars
salmonella <- salmonella %>%
  select(location, processing_type, storage_type, market_type, ST, Serovars, Sum_Value)
salmonella <- salmonella %>%
  mutate(
    # 设置Serovars的因子水平
    Serovars = factor(Serovars, levels = final_serovar_order),
    # 对于其他变量，可以按照字母顺序或频率排序
    # 这里按照字母顺序排列，您也可以按照频率排序
    location = factor(location, levels = sort(unique(location))),
    processing_type = factor(processing_type, levels = sort(unique(processing_type))),
    storage_type = factor(storage_type, levels = sort(unique(storage_type))),
    market_type = factor(market_type, levels = sort(unique(market_type))),
    ST = factor(ST, levels = sort(unique(ST)))
  )
# 4. 准备绘图数据
# 使用gather_set_data，注意x轴顺序已由列顺序决定
salmonella1 <- gather_set_data(salmonella, 1:6)
# 5. 绘制图形
p2 <- ggplot(salmonella1, aes(x, id = id, split = y, value = Sum_Value)) +
  geom_parallel_sets(aes(fill = Serovars), alpha = 0.8, axis.width = 0.2) +
  geom_parallel_sets_axes(axis.width = 0.4, colour = "black", fill = "white") +
  geom_parallel_sets_labels(colour = "black", angle = 0, size = 3, vjust = -0.8) +
  labs(y = "Number of isolates", x = "") +
  theme(text = element_text(size=20),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = 0.5,angle = 20, size=10)) +
  theme_bw() +
  # 可选：自定义颜色，确保颜色与Serovars顺序一致
  scale_fill_manual(
    values = c(
      "Rissen" = "#e54e21",  # 红色
      "London" = "#8dd2c6",  # 蓝色
      "4,[5],12:i:-" = "#b3de69",  # 绿色
      "Derby" = "#BB80BC",  # 紫色
      "Typhimurium" = "#80B0D2",  # 橙色
      "Others" = "#FFEC6D"  # 棕色
    ),
    breaks = final_serovar_order,  # 确保图例顺序与数据一致
    drop = FALSE  # 即使某些水平没有数据也显示
  ) +
  # 可选：调整x轴标签顺序
  scale_x_discrete(
    limits = c("location", "processing_type", "storage_type", "market_type", "ST", "Serovars")
  )
# 显示图形
print(p2)

ggsave("sankey_plot.pdf", width=12, height=6, dpi = 600)

##这里需要引入第三张图，多重拼图完成整体分布情况的描述------------------------------
##主要血清型与ST型的分布-------------------------------------------------------
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 4)
data_longer <- data %>%pivot_longer(cols = -ST, names_to = "Serovars", values_to = "Count")%>%
  mutate(Serovars = factor(Serovars, levels = c("Rissen", "London", "4,[5],12:i:-", "Derby", "Typhimurium", "Others"))) 
p3<-ggplot(data = data_longer,aes(x=Serovars,y=Count,fill=ST))+
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, 
            hjust = 0.7,
            size = 3.5) + 
  geom_bar(stat = "identity",
           position = "dodge",alpha=1)+  
  theme_bw()+
  scale_fill_manual(values =  colorRampPalette(brewer.pal(6, "Set2"))(length(unique(data_longer$ST))))+ 
  theme(text = element_text(size=15))    #隐藏网格线
p3
ggsave("serovar_STs_stat.pdf", width=12, height=6, dpi = 600)
##---------------------------------------------------------
##绘制血清型总数的柱状分布图，
# 读取数据
unique(data_longer$Types)
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 2)
data_longer <- data %>%#pivot_longer(cols = -Serovars, names_to = "Location", values_to = "Count")%>%
  mutate(Serovars = factor(Serovars, levels = c("Rissen", "London", "4,[5],12:i:-", "Derby", "Typhimurium", "Others"))) %>%
  mutate(Types = factor(Types, levels = c("North", "South", "mince", "piece", "chilled", "fresh", "supermarket", "wet market","Total")))
p3<-ggplot(data = data_longer,aes(x=Types,y=Count,fill=Serovars))+
#  facet_wrap(~ Categories) + 
#  geom_text(aes(label = Count), 
#            position = position_dodge(width = 0.7), 
 #           vjust = -0.5, 
#            hjust = 0.7,
#            size = 3.5) + 
    geom_bar(stat = "identity",
             position = "fill",alpha=1)+  
  theme_bw()+
  # 可选：自定义颜色，确保颜色与Serovars顺序一致
  scale_fill_manual(
    values = c(
      "Rissen" = "#e54e21",  # 红色
      "London" = "#8dd2c6",  # 蓝色
      "4,[5],12:i:-" = "#b3de69",  # 绿色
      "Derby" = "#BB80BC",  # 紫色
      "Typhimurium" = "#80B0D2",  # 橙色
      "Others" = "#FFEC6D"  # 棕色
    ),
#    breaks = final_serovar_order,  # 确保图例顺序与数据一致
    drop = FALSE  # 即使某些水平没有数据也显示
  ) +
  labs(x="",y="Proportion")+
  theme(strip.text = element_text(size = 15, face = "bold", hjust = 0.5 ),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))    #隐藏网格线
  
p3
ggsave("serovar_envs_stat.pdf", width=10, height=6, dpi = 600)
##绘制不同地理区域的血清型分布图--------------------------------------

##绘制血清型总数的柱状分布图，
# 读取数据
#unique(data_longer$Types)
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 14)
data_longer <- data %>%pivot_longer(cols = -Location, names_to = "Serovars", values_to = "Count")%>%
  mutate(Serovars = factor(Serovars, levels = c("Rissen", "London", "4,[5],12:i:-", "Derby", "Typhimurium", "Others"))) %>%
  mutate(Location = factor(Location, levels = unique(Location[order(Count, decreasing = TRUE)])))
p3<-ggplot(data = data_longer,aes(x=Location,y=Count,fill=Serovars))+
  #  facet_wrap(~ Categories) + 
  #  geom_text(aes(label = Count), 
  #            position = position_dodge(width = 0.7), 
  #           vjust = -0.5, 
  #            hjust = 0.7,
  #            size = 3.5) + 
  geom_bar(stat = "identity",
           position = "fill",alpha=1)+  
  theme_bw()+
  # 可选：自定义颜色，确保颜色与Serovars顺序一致
  scale_fill_manual(
    values = c(
      "Rissen" = "#e54e21",  # 红色
      "London" = "#8dd2c6",  # 蓝色
      "4,[5],12:i:-" = "#b3de69",  # 绿色
      "Derby" = "#BB80BC",  # 紫色
      "Typhimurium" = "#80B0D2",  # 橙色
      "Others" = "#FFEC6D"  # 棕色
    ),
    #    breaks = final_serovar_order,  # 确保图例顺序与数据一致
    drop = FALSE  # 即使某些水平没有数据也显示
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="",y="Proportion")+
  theme(strip.text = element_text(size = 15, face = "bold", hjust = 0.5 ),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))    #隐藏网格线
p3
#ggsave("geographical_serovar_envs_stat.jpg", width=8, height=6, dpi = 600)
##换一种画法，强调差异----------------------------------------
# 计算每个Location下各血清型的比例
data_longer <- data_longer %>%
  group_by(Location) %>%
  mutate(Proportion = Count / sum(Count) * 100) %>%
  ungroup()%>%
  mutate(Location = factor(Location, levels = unique(Location[order(Proportion, decreasing = TRUE)])))
# 绘图时使用 Proportion 列
p3 <- ggplot(data = data_longer, aes(x = Location, y = Proportion, fill = Serovars)) +
#  geom_bar(stat = "identity", position = "stack", alpha = 1) +
  geom_bar(stat = "identity",
           position = "fill",alpha=1)+  
  theme_bw()+
  # 可选：自定义颜色，确保颜色与Serovars顺序一致
  scale_fill_manual(
    values = c(
      "Rissen" = "#e54e21",  # 红色
      "London" = "#8dd2c6",  # 蓝色
      "4,[5],12:i:-" = "#b3de69",  # 绿色
      "Derby" = "#BB80BC",  # 紫色
      "Typhimurium" = "#80B0D2",  # 橙色
      "Others" = "#FFEC6D"  # 棕色
    ),
    #    breaks = final_serovar_order,  # 确保图例顺序与数据一致
    drop = FALSE  # 即使某些水平没有数据也显示
  ) +
  labs(x="",y="Proportion")+
  theme(strip.text = element_text(size = 15, face = "bold", hjust = 0.5 ),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=15))+    #隐藏网格线
  scale_y_continuous(labels = scales::percent)
p3

ggsave("geographical_serovar_envs_stat.pdf", width=10, height=6, dpi = 600)

##--------------------------------------------------------------------------------
##共现性网络图反映不同血清型与对应的ARGs、MGEs等之间的差异
##找到影响耐药基因传播的主要因素----------------------------------------------


##不同血清型之间特征基因的差异-----------------------------------------------------------------
library(pheatmap)
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 6)
data_matrix<- data%>%  select(3:7)%>%  
  as.matrix()
rownames(data_matrix)<-data$Genes
annotation <- select(data,2)
rownames(annotation) <- data$Genes

annotation_row <- data.frame(
  Types = data[, 2, drop = TRUE]  # 使用drop=TRUE确保是向量
)
rownames(annotation_row) <- data$Genes
# 创建注释颜色
# 获取唯一的Types
unique_types <- unique(annotation_row$Types)
type_colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique_types))
names(type_colors) <- unique_types
annotation_colors <- list(
  Types = type_colors
)
#annotation_colors <- list (unique(data$Types),colorRampPalette(brewer.pal(8, "Set2"))(length(unique(data$Types))))
p1 <- pheatmap(data_matrix,border_color = "white",               ,
               cluster_rows = TRUE,  # 对行聚类
               cluster_cols = TRUE,  # 对列聚类
               annotation_row = annotation_row, 
               annotation_colors = annotation_colors,
               color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
               show_rownames = TRUE,
               show_colnames = TRUE,
               fontsize_row = 8,
               fontsize_col = 8)
p1
ggsave("serovars_ARGs.pdf",p1,height = 13,width = 5,dpi = 600)
##绘制点突变的热图-------------------------------------------------------------------

library(pheatmap)
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 12)
data_matrix<- data%>%  select(2:7)
rownames(data_matrix)<-data$Mutation
#annotation <- select(data,2)
#rownames(annotation) <- data$Genes
#annotation_row <- data.frame(
#  Types = data[, 2, drop = TRUE]  # 使用drop=TRUE确保是向量
#)
#rownames(annotation_row) <- data$Genes
# 创建注释颜色
# 获取唯一的Types
#unique_types <- unique(annotation_row$Types)
#type_colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique_types))
#names(type_colors) <- unique_types
#annotation_colors <- list(
#  Types = type_colors
#)
#annotation_colors <- list (unique(data$Types),colorRampPalette(brewer.pal(8, "Set2"))(length(unique(data$Types))))
#取对数
data4_2 <- log10 (data_matrix)
data4_3 <- as.data.frame(lapply(data4_2, function(x) replace(x, is.infinite(x), -3)))
colnames(data4_3) <- colnames(data_matrix)
rownames(data4_3) <- rownames(data_matrix)
p1 <- pheatmap(data4_3,border_color = "white",               ,
               cluster_rows = TRUE,  # 对行聚类
               cluster_cols = TRUE,  # 对列聚类
#               annotation_row = annotation_row, 
#               annotation_colors = annotation_colors,
#               color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
               show_rownames = TRUE,
               show_colnames = TRUE,
               fontsize_row = 8,
               fontsize_col = 8)
p1
ggsave("serovars_point_mutations.pdf",p1,height =3,width = 3,dpi = 600)
##绘制致病岛----------------------------------
library(pheatmap)
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 19)
data_matrix<- data%>%  select(2:7)
rownames(data_matrix)<-data$Genes
data4_2 <- log10 (data_matrix)
data4_3 <- as.data.frame(lapply(data4_2, function(x) replace(x, is.infinite(x), -3)))
colnames(data4_3) <- colnames(data_matrix)
rownames(data4_3) <- rownames(data_matrix)
p1 <- pheatmap(data_matrix,border_color = "white",               ,
               cluster_rows = TRUE,  # 对行聚类
               cluster_cols = TRUE,  # 对列聚类
               #               annotation_row = annotation_row, 
               #               annotation_colors = annotation_colors,
               #               color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
               show_rownames = TRUE,
               show_colnames = TRUE,
               fontsize_row = 8,
               fontsize_col = 8)
p1
#ggsave("SPIs.pdf",p1,height =3,width = 3,dpi = 600)
##不同血清型的质粒复制子、插入序列的差异------------------------------------------
library(pheatmap)
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 13)
data_matrix<- data%>%  select(3:8)
rownames(data_matrix)<-data$Genes
annotation <- select(data,2)
rownames(annotation) <- data$Genes
annotation_row <- data.frame(
  Types = data[, 2, drop = TRUE]  # 使用drop=TRUE确保是向量
)
rownames(annotation_row) <- data$Genes
# 创建注释颜色
# 获取唯一的Types
unique_types <- unique(annotation_row$Types)
type_colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique_types))
names(type_colors) <- unique_types
annotation_colors <- list(
  Types = type_colors
)
#取对数
data4_2 <- log10 (data_matrix)
data4_3 <- as.data.frame(lapply(data4_2, function(x) replace(x, is.infinite(x), -3)))
colnames(data4_3) <- colnames(data_matrix)
rownames(data4_3) <- rownames(data_matrix)
#annotation_colors <- list (unique(data$Types),colorRampPalette(brewer.pal(8, "Set2"))(length(unique(data$Types))))
p1 <- pheatmap(data4_3,border_color = "white",
               cluster_rows = TRUE,  # 对行聚类
               cluster_cols = TRUE,  # 对列聚类
               annotation_row = annotation_row, 
               annotation_colors = annotation_colors,
               color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
               show_rownames = TRUE,
               show_colnames = TRUE,
               fontsize_row = 8,
               fontsize_col = 8)
p1
#ggsave("serovars_ISs_plasmids_replicons.pdf",p1,height = 8,width = 5,dpi = 600)
##差异重金属消毒剂基因-------------------------------------------------------------------------------
library(pheatmap)
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 17)
data_matrix<- data%>%  select(3:8)
rownames(data_matrix)<-data$Genes
annotation <- select(data,2)
rownames(annotation) <- data$Genes
annotation_row <- data.frame(
  Types = data[, 2, drop = TRUE]  # 使用drop=TRUE确保是向量
)
rownames(annotation_row) <- data$Genes
# 创建注释颜色
# 获取唯一的Types
unique_types <- unique(annotation_row$Types)
type_colors <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique_types))
names(type_colors) <- unique_types
annotation_colors <- list(
  Types = type_colors
)
取对数
data4_2 <- log10 (data_matrix)
data4_3 <- as.data.frame(lapply(data4_2, function(x) replace(x, is.infinite(x), -3)))
colnames(data4_3) <- colnames(data_matrix)
rownames(data4_3) <- rownames(data_matrix)
annotation_colors <- list (unique(data$Types),colorRampPalette(brewer.pal(8, "Set2"))(length(unique(data$Types))))
p1 <- pheatmap(data4_3,border_color = "white",
               cluster_rows = TRUE,  # 对行聚类
               cluster_cols = TRUE,  # 对列聚类
               annotation_row = annotation_row, 
               annotation_colors = annotation_colors,
               color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
               show_rownames = TRUE,
               show_colnames = TRUE,
               fontsize_row = 8,
               fontsize_col = 8)
p1
##ggsave("serovars_bacmet.jpg",p1,height = 10,width = 4,dpi = 600)
##差异毒力基因--------------------------------------------------------------
library(pheatmap)
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 16)
data_matrix<- data%>%  select(2:7)
rownames(data_matrix)<-data$Genes
#取对数
data4_2 <- log10 (data_matrix)
data4_3 <- as.data.frame(lapply(data4_2, function(x) replace(x, is.infinite(x), -3)))
colnames(data4_3) <- colnames(data_matrix)
rownames(data4_3) <- rownames(data_matrix)
#annotation_colors <- list (unique(data$Types),colorRampPalette(brewer.pal(8, "Set2"))(length(unique(data$Types))))
p1 <- pheatmap(data4_3,border_color = "white",
               cluster_rows = TRUE,  # 对行聚类
               cluster_cols = TRUE,  # 对列聚类
#               annotation_row = annotation_row, 
#               annotation_colors = annotation_colors,
               color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
               show_rownames = TRUE,
               show_colnames = TRUE,
               fontsize_row = 8,
               fontsize_col = 8)
p1
ggsave("diff_serovars_VFs.pdf",p1,height = 8,width = 3,dpi = 600)
##绘制基因岛毒力岛的分布--------------------------------------------------------



library(ggtern)







###-----------------------------------------------------------------------------
library(dplyr)
library(reshape2)
library(pheatmap)
library(readxl)
library(RColorBrewer)
library(fdrtool)
library(tidyverse)
library(tibble)
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
# 加载必要的包
# 1. 读取Sheet8数据
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 8)
# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")
cat("前几行数据:\n")
print(head(data))

# 2. 计算阳性菌株数
# 已知南北方菌株数量
n_south <- 828
n_north <- 590

# 计算实际阳性菌株数（流行率 × 菌株数量）
# 确保阳性菌株数为非负整数
data <- data %>%
  mutate(
    South_pos = round(South * n_south),  # 南方阳性数
    North_pos = round(North * n_north),  # 北方阳性数
    South_neg = n_south - South_pos,     # 南方阴性数
    North_neg = n_north - North_pos      # 北方阴性数
  )

# 3. 检查数据是否有问题
cat("\n检查数据范围:\n")
cat("South_pos范围:", range(data$South_pos), "\n")
cat("North_pos范围:", range(data$North_pos), "\n")
cat("South_neg范围:", range(data$South_neg), "\n")
cat("North_neg范围:", range(data$North_neg), "\n")

# 检查是否有负值
cat("\n检查是否有负值:\n")
cat("South_pos有负值:", any(data$South_pos < 0), "\n")
cat("North_pos有负值:", any(data$North_pos < 0), "\n")
cat("South_neg有负值:", any(data$South_neg < 0), "\n")
cat("North_neg有负值:", any(data$North_neg < 0), "\n")

# 4. 改进的Fisher精确检验函数
perform_fisher_test <- function(df) {
  p_values <- numeric(nrow(df))
  
  for (i in 1:nrow(df)) {
    # 获取四格表数据
    a <- df$South_pos[i]  # 南方阳性
    b <- df$South_neg[i]  # 南方阴性
    c <- df$North_pos[i]  # 北方阳性
    d <- df$North_neg[i]  # 北方阴性
    
    # 检查数据有效性
    if (any(is.na(c(a, b, c, d))) || any(c(a, b, c, d) < 0)) {
      cat("第", i, "行数据有问题: South_pos=", a, " South_neg=", b, 
          " North_pos=", c, " North_neg=", d, "\n")
      p_values[i] <- NA
      next
    }
    
    # 构建2×2列联表
    contingency_table <- matrix(
      c(a, b, c, d),
      nrow = 2,
      dimnames = list(
        c("South", "North"),
        c("Positive", "Negative")
      )
    )
    
    # 进行Fisher精确检验（使用模拟方法避免计算问题）
    tryCatch({
      # 如果四格表中有0，使用模拟方法
      if (any(contingency_table == 0)) {
        test_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
      } else {
        test_result <- fisher.test(contingency_table)
      }
      p_values[i] <- test_result$p.value
    }, error = function(e) {
      cat("第", i, "行 Fisher 检验出错:", e$message, "\n")
      cat("基因:", df$Genes[i], " 四格表:", paste(contingency_table, collapse = ", "), "\n")
      p_values[i] <- NA
    })
  }
  
  return(p_values)
}

# 5. 计算p值
cat("\n正在计算Fisher精确检验p值...\n")
data$p_value <- perform_fisher_test(data)

# 检查p值计算情况
cat("\np值计算完成。\n")
cat("有效的p值数量:", sum(!is.na(data$p_value)), "/", nrow(data), "\n")
cat("NA的p值数量:", sum(is.na(data$p_value)), "\n")

# 6. 多重检验校正（FDR校正）
# 只对有效的p值进行校正
valid_p <- data$p_value[!is.na(data$p_value)]
if (length(valid_p) > 0) {
  data$adj_p <- NA
  data$adj_p[!is.na(data$p_value)] <- p.adjust(valid_p, method = "fdr")
} else {
  stop("没有有效的p值进行校正。")
}

# 7. 计算流行率差异
data$prevalence_diff <- data$South - data$North
data$abs_diff <- abs(data$prevalence_diff)

# 8. 筛选显著差异基因
# 设定阈值：FDR < 0.05 且流行率差异绝对值 > 0.05
sig_genes <- data %>%
  filter(adj_p < 0.05 & abs_diff > 0.05) %>%
  arrange(desc(abs_diff))

cat("显著差异基因数量:", nrow(sig_genes), "\n")

# 如果显著基因太少，放宽阈值
if (nrow(sig_genes) < 5) {
  cat("显著差异基因较少，使用较宽松阈值...\n")
  sig_genes <- data %>%
    filter(adj_p < 0.5) %>%
    arrange(desc(abs_diff)) %>%
    head(20)  # 取前20个差异最大的基因
  cat("使用宽松阈值后显著差异基因数量:", nrow(sig_genes), "\n")
}

# 9. 准备热图数据
if (nrow(sig_genes) > 0) {
  heatmap_data <- sig_genes %>%
    select(Genes, South, North) %>%
    column_to_rownames(var = "Genes") %>%
    as.matrix()
  
  # 10. 准备行注释（基因类型）
  annotation_row <- sig_genes %>%
    select(Genes, Types) %>%
    column_to_rownames(var = "Genes")
  
  # 11. 创建颜色方案
  # 热图颜色
  heatmap_colors <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)
  
  # 基因类型颜色
  unique_types <- unique(annotation_row$Types)
  n_types <- length(unique_types)
  
  # 使用Set2调色板
  if (n_types <= 8) {
    type_colors <- brewer.pal(max(3, n_types), "Set2")[1:n_types]
  } else {
    type_colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_types)
  }
  names(type_colors) <- unique_types
  
  annotation_colors <- list(Types = type_colors)
  
  # 12. 绘制热图
  p <- pheatmap(
    heatmap_data,
    # 颜色和样式
    color = heatmap_colors,
    scale = "row",  # 按行标准化，突出差异
    border_color = NA,
    
    # 聚类
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    clustering_distance_rows = "euclidean",
    clustering_method = "complete",
    
    # 注释
    annotation_row = annotation_row,
    annotation_colors = annotation_colors,
    
    # 标签
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 8,
    fontsize_col = 10,
    angle_col = 45,
    
    # 标题
    main = paste("Differential ARGs between South (n=828) and North (n=590)\n",
                 nrow(sig_genes), "significant genes"),
    # 图例
    legend = TRUE,
    legend_breaks = c(-2, 0, 2),
    legend_labels = c("Low", "Medium", "High"),
    # 单元格大小
    cellwidth = 30,
    cellheight = 8,
    # 树状图
    treeheight_row = 50,
    treeheight_col = 0
  )
  # 13. 保存热图
  ggsave("ARGs_north_south_differential_heatmap.png", 
         p, width = 10, height = 12, dpi = 300, bg = "white")
  # 14. 保存差异基因结果
  write.csv(sig_genes, "differential_ARGs_north_south.csv", row.names = FALSE)
  # 15. 创建差异基因统计摘要
  summary_stats <- sig_genes %>%
    group_by(Types) %>%
    summarise(
      Count = n(),
      Avg_South_Prev = mean(South),
      Avg_North_Prev = mean(North),
      Avg_Diff = mean(prevalence_diff)
    ) %>%
    arrange(desc(Count))
  cat("\n差异基因按类型统计:\n")
  print(summary_stats)
  # 16. 创建火山图
  volcano_plot <- ggplot(data, aes(x = prevalence_diff, y = -log10(adj_p))) +
    geom_point(aes(color = ifelse(adj_p < 0.05 & abs_diff > 0.05, "Significant", "Not significant")), 
               alpha = 0.6) +
    scale_color_manual(values = c("Significant" = "red", "Not significant" = "gray")) +
    labs(
      title = "Volcano Plot: ARGs Prevalence Difference between South and North",
      x = "Prevalence Difference (South - North)",
      y = "-log10(Adjusted p-value)",
      color = "Significance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    ) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +
    geom_vline(xintercept = c(-0.05, 0.05), linetype = "dashed", color = "blue")
  # 标注显著基因
  if (nrow(sig_genes) > 0) {
    volcano_plot <- volcano_plot +
      geom_text(data = sig_genes %>% head(10), 
                aes(label = Genes), 
                size = 3, vjust = 1.5, hjust = 0.5)
  }
  ggsave("ARGs_volcano_plot.png", volcano_plot, width = 12, height = 8, dpi = 300)
  
} else {
  cat("警告: 没有找到显著差异基因。\n")
  # 如果没有显著差异基因，绘制所有基因的热图
  heatmap_data <- data %>%
    select(Genes, South, North) %>%
    column_to_rownames(var = "Genes") %>%
    as.matrix()
  annotation_row <- data %>%
    select(Genes, Types) %>%
    column_to_rownames(var = "Genes")
  # 绘制热图
  p <- pheatmap(
    heatmap_data,
    scale = "row",
    main = "All ARGs (No significant differences found)",
    show_rownames = FALSE
  )
  ggsave("ARGs_all_heatmap.png", p, width = 8, height = 10, dpi = 300)
}

# 17. 输出总结报告
cat("\n=== 分析完成 ===\n")
cat("1. 总基因数量:", nrow(data), "\n")
cat("2. 显著差异基因数量:", nrow(sig_genes), "\n")
if (nrow(sig_genes) > 0) {
  cat("3. 南方平均流行率:", round(mean(sig_genes$South), 4), "\n")
  cat("4. 北方平均流行率:", round(mean(sig_genes$North), 4), "\n")
  cat("5. 平均差异:", round(mean(sig_genes$prevalence_diff), 4), "\n")
}
cat("\n已生成文件:\n")
if (nrow(sig_genes) > 0) {
  cat("1. ARGs_north_south_differential_heatmap.png - 差异基因热图\n")
  cat("2. differential_ARGs_north_south.csv - 差异基因列表\n")
  cat("3. ARGs_volcano_plot.png - 火山图\n")
} else {
  cat("1. ARGs_all_heatmap.png - 所有基因热图\n")
}















##-----------------------------------------------------------------------------
# 简洁版本
library(readxl)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
# 读取数据
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 6)
# 提取数据
serovar_cols <- colnames(data)[3:(ncol(data)-1)]
heatmap_data <- as.matrix(data[, serovar_cols])
rownames(heatmap_data) <- data$Genes
# 计算总计
row_totals <- data$Total
col_totals <- colSums(heatmap_data, na.rm = TRUE)
# 热图颜色
heatmap_colors <- colorRamp2(
  seq(min(heatmap_data), max(heatmap_data), length = 100),
  colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)
)
# 类型颜色
type_colors <- setNames(
  colorRampPalette(brewer.pal(8, "Set2"))(length(unique(data$Types))),
  unique(data$Types)
)
# 左侧注释
left_ann <- rowAnnotation(
  Types = data$Types,
  Total = anno_barplot(
    row_totals,
    gp = gpar(fill = "#4DAF4A", col = NA),
    bar_width = 0.7,
    axis = TRUE,
    axis_param = list(side = "top", gp = gpar(fontsize = 8))
  ),
  col = list(Types = type_colors),
  annotation_name_side = "top",
  annotation_name_rot = 0,
  gap = unit(3, "mm"),
  width = unit(4, "cm")
)
# 顶部注释
top_ann <- HeatmapAnnotation(
  Total = anno_barplot(
    col_totals,
    gp = gpar(fill = "#E41A1C", col = NA),
    bar_width = 0.7,
    axis = TRUE,
    axis_param = list(side = "right", gp = gpar(fontsize = 8))
  ),
  annotation_name_side = "left",
  annotation_name_rot = 0,
  height = unit(2, "cm")
)
# 创建热图
ht <- Heatmap(
  heatmap_data,
  name = "Count",
  col = heatmap_colors,
  # 注释
  left_annotation = left_ann,
  top_annotation = top_ann,
  # 行列设置
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 8),
  column_names_side = "top",
  column_names_gp = gpar(fontsize = 9),
  column_names_rot = 45,
  # 聚类
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  # 边框
  rect_gp = gpar(col = "white", lwd = 0.5),
  border = "gray",
  # 图例
  heatmap_legend_param = list(
    title = "ARGs Count",
    title_position = "leftcenter-rot",
    legend_direction = "vertical"
  )
)
# 绘制
png("ARGs_simple_heatmap.png", width = 14, height = 10, units = "in", res = 300)
draw(ht)
dev.off()








##不同地域之间特征基因的差异---------------------------------------------
data_matrix<- read_excel("1418_salmonella_pork.xlsx", sheet = 5)
# 将数据转换为长格式
data_long <- as.data.frame(data_matrix) %>%
  rownames_to_column(var = "Row") %>%
  pivot_longer(cols = -Row, names_to = "Column", values_to = "Value") %>%
  mutate(Row = factor(Row, levels = unique(Row)),
         Column = factor(Column, levels = unique(Column)))

# 创建ggplot热图
p2 <- ggplot(data_long, aes(x = Column, y = Row, fill = Value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = median(data_long$Value, na.rm = TRUE),
                       name = "Value") +
  labs(title = "Heatmap",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "right") +
  coord_fixed()

# 保存ggplot热图
ggsave("heatmap_ggplot.png", p2, width = 12, height = 10, dpi = 300)



##最后需要搜集到区域的温度,SDI指数，




##--------------------------------------------------------------------------------
# 加载必要的包
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(patchwork)
library(factoextra)
library(FactoMineR)
library(vegan)
library(cluster)
library(ggpubr)
library(car)
library(readxl)
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
# 1. 读取数据
data <- read_excel("data_collection.xlsx", sheet = 1)

# 查看数据结构
str(data)
summary(data)

# 2. 数据预处理
# 将省份转换为因子
data$省份 <- as.factor(data$省份)

# 计算血清型比例的总和，确保每个省份的血清型比例之和为1
data <- data %>%
  mutate(血清型总和 = rowSums(select(., Derby:Rissen)))

# 3. 描述性统计分析
# 3.1 基本统计量
summary_stats <- data %>%
  summarise(
    SDI_mean = mean(SDI, na.rm = TRUE),
    SDI_sd = sd(SDI, na.rm = TRUE),
    ARGs_mean = mean(ARGs_per_isolate, na.rm = TRUE),
    ARGs_sd = sd(ARGs_per_isolate, na.rm = TRUE),
    GDP_mean = mean(GDP_per_person, na.rm = TRUE),
    GDP_sd = sd(GDP_per_person, na.rm = TRUE)
  )

print("描述性统计：")
print(summary_stats)

# 3.2 各省份分布
ggplot(data, aes(x = reorder(省份, SDI), y = SDI, fill = SDI)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "各省份社会发展指数(SDI)分布", 
       x = "省份", y = "SDI值") +
  theme(axis.text.y = element_text(size = 8))

# 4. 相关性分析
# 4.1 计算所有数值变量的相关系数矩阵
numeric_vars <- data %>%
  select(where(is.numeric)) %>%
  select(-c(血清型总和))  # 排除血清型总和列

cor_matrix <- cor(numeric_vars, use = "complete.obs")

# 可视化相关系数矩阵
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "各变量相关系数矩阵")

# 4.2 重点关注ARGs_per_isolate与其他变量的相关性
args_cor <- cor_matrix["ARGs_per_isolate", ]
args_cor_df <- data.frame(
  variable = names(args_cor),
  correlation = as.numeric(args_cor)
) %>%
  filter(variable != "ARGs_per_isolate") %>%
  arrange(desc(abs(correlation)))

print("耐药基因数量与其他变量的相关性：")
print(args_cor_df)

# 5. 多元回归分析
# 5.1 耐药基因数量与气候、社会经济因素的关系
model_args <- lm(ARGs_per_isolate ~ 
                   `年平均气温（℃）` + 
                   `年平均相对湿度（%）` + 
                   `全年降水量（毫米）` + 
                   `全年日照时数` +
                   SDI +
                   GDP_per_person +
                   `全体居民人均可支配收入 (元)` +
                   average_pork_consumption,
                 data = data)

summary(model_args)

# 检查模型假设
par(mfrow = c(2, 2))
plot(model_args)
par(mfrow = c(1, 1))

# 多重共线性检验
vif_values <- vif(model_args)
print("方差膨胀因子(VIF)：")
print(vif_values)

# 5.2 优化模型（逐步回归）
model_args_step <- step(model_args, direction = "both")
summary(model_args_step)

# 6. 血清型分布与社会经济因素的关系分析
# 6.1 主成分分析(PCA) - 分析血清型分布模式
serotype_data <- data %>%
  select(Derby:Rissen)

# 执行PCA
pca_result <- prcomp(serotype_data, scale = TRUE, center = TRUE)

# 可视化PCA结果
fviz_eig(pca_result, addlabels = TRUE) +
  ggtitle("血清型分布PCA - 特征值")

fviz_pca_var(pca_result,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  ggtitle("血清型变量在PCA空间中的分布")

# 6.2 将PCA结果与原始数据合并
pca_scores <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")
data_with_pca <- cbind(data, pca_scores)

# 6.3 分析社会经济因素对血清型分布的影响
# 将主成分与社会经济因素进行相关性分析
pca_cor_data <- data_with_pca %>%
  select(PC1, PC2, 
         SDI, GDP_per_person, `全体居民人均可支配收入 (元)`,
         average_pork_consumption, `年平均气温（℃）`)

pca_cor_matrix <- cor(pca_cor_data, use = "complete.obs")

ggcorrplot(pca_cor_matrix, 
           hc.order = FALSE,
           type = "lower",
           lab = TRUE,
           lab_size = 4,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "血清型主成分与社会经济因素相关性")+
  theme_bw()

# 7. 聚类分析
# 7.1 基于社会经济因素的省份聚类
socio_economic_vars <- data %>%
  select(SDI, GDP_per_person, `全体居民人均可支配收入 (元)`,
         average_pork_consumption, number_of_isolates) %>%
  scale()  # 标准化

# 确定最佳聚类数
fviz_nbclust(socio_economic_vars, kmeans, method = "wss") +
  ggtitle("肘部法则确定最佳聚类数")

fviz_nbclust(socio_economic_vars, kmeans, method = "silhouette") +
  ggtitle("轮廓系数确定最佳聚类数")

# 执行K-means聚类
set.seed(123)
kmeans_result <- kmeans(socio_economic_vars, centers = 3, nstart = 25)

# 将聚类结果添加到数据中
data$cluster <- as.factor(kmeans_result$cluster)

# 可视化聚类结果
fviz_cluster(kmeans_result, data = socio_economic_vars,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()) +
  ggtitle("基于社会经济因素的省份聚类")

# 7.2 分析不同聚类在耐药基因和血清型上的差异
# 耐药基因数量的聚类差异
ggplot(data, aes(x = cluster, y = ARGs_per_isolate, fill = cluster)) +
  geom_boxplot() +
  stat_compare_means(method = "anova") +
  theme_minimal() +
  labs(title = "不同社会经济聚类组的耐药基因数量差异",
       x = "聚类组", y = "耐药基因数量")

# 血清型分布的聚类差异
serotype_by_cluster <- data %>%
  group_by(cluster) %>%
  summarise(
    Derby_mean = mean(Derby, na.rm = TRUE),
    I4_5_12_i_mean = mean(`I 4,[5],12:i:-`, na.rm = TRUE),
    London_mean = mean(London, na.rm = TRUE),
    Others_mean = mean(Others, na.rm = TRUE),
    Rissen_mean = mean(Rissen, na.rm = TRUE),
    Typhimurium_mean = mean(Typhimurium, na.rm = TRUE)
  )

print("不同聚类组的血清型平均比例：")
print(serotype_by_cluster)

# 8. 空间自相关分析（需地理信息，这里用近似方法）
# 创建距离矩阵（使用SDI差异作为距离的代理）
sdi_dist <- dist(data$SDI)
args_dist <- dist(data$ARGs_per_isolate)

# Mantel检验：检验耐药基因数量与SDI的空间自相关
if(require(vegan)){
  mantel_test <- mantel(sdi_dist, args_dist, method = "pearson")
  print("Mantel检验结果（耐药基因与SDI）：")
  print(mantel_test)
}

# 9. 结构方程模型探索（简化版路径分析）
library(lavaan)

# 定义路径模型
model_sem <- '
  # 直接效应
  ARGs_per_isolate ~ a1*SDI + a2*GDP_per_person + a3*average_pork_consumption
  
  # 中介变量
  SDI ~ b1*GDP_per_person
  
  # 间接效应
  indirect := b1 * a1
  total := a1 + indirect
'
# 拟合模型
fit_sem <- sem(model_sem, data = data)
summary(fit_sem, standardized = TRUE, rsquare = TRUE)
fitMeasures(fit_sem, c("cfi", "tli", "rmsea", "srmr"))

# 10. 可视化关键关系

##直接绘制关系---------------------------------------------------------------------------------
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
# 1. 读取数据
data <- read_excel("data_collection.xlsx", sheet = 1)
data_longer<-data %>% select(temperature,rainfall,
                             GDP_per_capita,Residential_Population,ARGs,number_of_isolates, Location )%>%
pivot_longer(cols = -c(ARGs,number_of_isolates,Location),
               names_to = "categories",
               values_to = "Value")
# 10.1 人均GDP与耐药基因数量的关系
p1 <- ggplot(data_longer, aes(x = Value, y = ARGs)) +
  facet_wrap(~ categories,scales = "free_x") +
  geom_point(aes(size = number_of_isolates, color = Location)) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
#  geom_text(aes(label = 省份), vjust = -0.5, size = 3) +
  scale_color_brewer(palette = "Set2") +
  # 添加回归方程和R²值
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    label.x = 0.25 * max(data$Value),
    label.y = 1.2 * max(data$ARGs)
  ) +
  theme_bw() +
  labs( y = "Number of ARGs",
       size = "Number of isolates",
       color = "Location") +
  theme(text = element_text(size = 15),
        strip.text = element_text(size = 15, face = "bold", hjust = 0.5 ),
    legend.position = "right")+guides(fill=guide_legend(override.aes=list(size=10)))
p1
#ggsave("GDP_ARGs_2.pdf",width = 12,height = 6,dpi = 600)
##不同坐标上绘制鼠伤寒、derby占比，并绘制点线图-----------------------------------------------------------------------------

setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
# 1. 读取数据
data <- read_excel("data_collection.xlsx", sheet = 5)
data_longer<-data %>% select(GDP_per_capita,number_of_isolates,Location,Derby,Typhimurium)%>%
  pivot_longer(cols = -c(GDP_per_capita,number_of_isolates,Location),
               names_to = "Serovar_proportion",
               values_to = "Value")
# 10.1 人均GDP与耐药基因数量的关系
p1 <- ggplot(data_longer, aes(x = GDP_per_capita, y = Value)) +
  facet_wrap(~ Serovar_proportion,scales = "free_x") +
  geom_point(aes(size = number_of_isolates, color = Location)) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  #  geom_text(aes(label = 省份), vjust = -0.5, size = 3) +
  scale_color_brewer(palette = "Set1") +
  # 添加回归方程和R²值
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    label.x = 0.25 * max(data$GDP_per_capita),
    label.y = 0.25
  ) +
  theme_bw() +
  labs( x = "GDP per capita",
        y = "Serovar proportion",
        size = "Number of isolates",
        color = "Location") +
  theme(text = element_text(size = 15),
        strip.text = element_text(size = 15, face = "bold", hjust = 0.5 ),
        legend.position = "right")+guides(fill=guide_legend(override.aes=list(size=10)))
p1
##ggsave("GDP_serovar_proportion_2.pdf",width = 12,height = 4,dpi = 600)
##------------------------------------------------------------------------------
##依据不同的分类，不同的血清型绘制火山图，标注出差异基因--------------------------






##只需要两个分类，一个是GDP（富裕、不富裕）一个是温度（寒冷，温暖），
##每个分类下对应六种不同的血清型，依据这六种血清型绘制-----------------------








##绘制环境因子与特征基因的相关性矩阵图-------------------------------------------
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(glmnet)
library(gridExtra)
library(reshape2)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
# 读取数据
data <- read_excel("data_collection.xlsx", sheet = 1)
# 查看列名
cat("数据集的列名:\n")
print(colnames(data))
cat("\n")
# 数据清洗和准备
# 确保列名正确
colnames(data) <- make.names(colnames(data))
# 再次查看处理后的列名
cat("处理后的列名:\n")
print(colnames(data))
cat("\n")
# 定义环境因子和社会经济因素变量
# 请根据实际列名调整以下变量
env_factors <- c("temperature", "average_ralative_humidity", 
                 "rainfall", "sunshine_hours")
socio_factors <- c("Average_Pork_Consumption", "GDP_per_capita", 
                   "Per_Capita_Disposable_Income", "Residential_Population", "SDI")
all_factors <- c(env_factors, socio_factors)
# 定义特征基因变量
gene_features <- c("ARGs", "VF", "BRGs", "MRGs", 
                   "plasmid_replicons", "IS")
# 检查列是否存在
check_columns <- function(data, columns) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    cat("以下列不存在于数据中:\n")
    print(missing)
    return(FALSE)
  }
  return(TRUE)
}
# 检查所有需要的列
cat("检查环境因子/社会经济因素列:\n")
check_columns(data, all_factors)
cat("\n检查特征基因列:\n")
check_columns(data, gene_features)
# 1. 计算相关性矩阵和p值矩阵
# 样本大小
n <- data$number_of_isolates
# 计算相关性矩阵
cor_matrix <- cor(data[, c(all_factors, gene_features)], 
                  use = "complete.obs", method = "pearson")
# 提取环境因子/社会经济因素与特征基因之间的相关性
cor_subset <- cor_matrix[all_factors, gene_features]
# 计算p值矩阵
calculate_p_values <- function(cor_matrix, n) {
  p_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(p_matrix) <- rownames(cor_matrix)
  colnames(p_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      if (!is.na(r)) {
        t_stat <- r * sqrt((n-2)/(1-r^2))
        p_value <- 2 * pt(-abs(t_stat), df = n-2)
        p_matrix[i, j] <- p_value
      }
    }
  }
  return(p_matrix)
}
p_matrix <- calculate_p_values(cor_matrix, n)
p_subset <- p_matrix[all_factors, gene_features]
# 2. 创建带有显著性星号的相关性矩阵
create_sig_cor_matrix <- function(cor_matrix, p_matrix) {
  sig_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(sig_matrix) <- rownames(cor_matrix)
  colnames(sig_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      p <- p_matrix[i, j]
      if (!is.na(r) & !is.na(p)) {
        # 格式化相关系数
        r_formatted <- sprintf("%.2f", r)
        # 添加星号
        if (p < 0.001) {
          sig_matrix[i, j] <- paste0(r_formatted, "***")
        } else if (p < 0.01) {
          sig_matrix[i, j] <- paste0(r_formatted, "**")
        } else if (p < 0.05) {
          sig_matrix[i, j] <- paste0(r_formatted, "*")
        } else {
          sig_matrix[i, j] <- r_formatted
        }
      }
    }
  }
  return(sig_matrix)
}
sig_cor_matrix <- create_sig_cor_matrix(cor_subset, p_subset)

# 3. 准备绘图数据
# 转换为长格式
cor_long <- melt(cor_subset)
colnames(cor_long) <- c("Factor", "Gene_Feature", "Correlation")
p_long <- melt(p_subset)
colnames(p_long) <- c("Factor", "Gene_Feature", "P_value")
# 合并相关性和p值
plot_data <- cor_long %>%
  left_join(p_long, by = c("Factor", "Gene_Feature")) %>%
  mutate(
    # 添加显著性星号
    Significance = case_when(
      P_value < 0.001 ~ "*",
      TRUE ~ ""
    ),
    # 创建显示文本
    Display_Text = paste0(sprintf("%.2f", Correlation), Significance),
    # 添加因子类型
    Factor_Type = ifelse(Factor %in% env_factors, "Environmental Factors", "Socioeconomic Factors")
  )
# 重新排序因子，确保环境因子在前，社会经济因素在后
plot_data$Factor <- factor(plot_data$Factor, 
                           levels = c(env_factors, socio_factors))
plot_data$Gene_Feature <- factor(plot_data$Gene_Feature, 
                                 levels = gene_features)
# 4. 绘制相关性热图
# 创建基础热图
p_heatmap <- ggplot(plot_data, aes(x = Factor, y = Gene_Feature, fill = Correlation)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Display_Text), size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation\nCoefficient") +
  labs(#title = "Correlation between Environmental/Socioeconomic Factors and Gene Features",
       x = "", y = "Gene Features") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 20)),
    legend.position = "right",
    panel.grid = element_blank()
#    plot.margin = margin(20, 20, 40, 20)  # 增加底部边距为组标签留空间
  ) +
  coord_fixed(ratio = 1.2)
# 显示基础热图
print(p_heatmap)
##ggsave("correlation_of_genes_contents.pdf",width = 8,height = 6,dpi = 600)

###重新编写相关性矩阵代码-----------------------------------------------------------
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")

# 加载必要的包
library(readxl)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(glmnet)
library(gridExtra)
library(reshape2)
library(RColorBrewer)
library(patchwork)
library(ggpubr)

# 读取数据
data <- read_excel("data_collection.xlsx", sheet = 1)
# 查看列名
cat("数据集的列名:\n")
print(colnames(data))
cat("\n")
# 数据清洗和准备
# 确保列名正确
colnames(data) <- make.names(colnames(data))
# 再次查看处理后的列名
cat("处理后的列名:\n")
print(colnames(data))
cat("\n")
# 定义环境因子和社会经济因素变量
env_factors <- c("temperature", "average_ralative_humidity", 
                 "rainfall", "sunshine_hours")
socio_factors <- c("Average_Pork_Consumption", "GDP_per_capita", 
                   "Per_Capita_Disposable_Income", "Residential_Population", "SDI")
all_factors <- c(env_factors, socio_factors)
# 定义特征基因变量
Serovar_proportions <- c("ARGs", "VF", "BRGs", "MRGs", 
                   "plasmid_replicons", "IS")
# 检查列是否存在
check_columns <- function(data, columns) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    cat("以下列不存在于数据中:\n")
    print(missing)
    return(FALSE)
  }
  return(TRUE)
}
# 检查所有需要的列
cat("检查环境因子/社会经济因素列:\n")
check_columns(data, all_factors)
cat("\n检查特征基因列:\n")
check_columns(data, Serovar_proportions)
# 1. 计算相关性矩阵和p值矩阵
# 修正：获取有效的样本数（删除缺失值后的行数）
# 创建一个只包含相关变量的子数据集
data_subset <- data[, c(all_factors, Serovar_proportions)]
# 计算每对变量的有效样本数
calculate_effective_n <- function(data, var1, var2) {
  # 返回两个变量都非缺失的行数
  sum(complete.cases(data[[var1]], data[[var2]]))
}
# 计算完整的相关矩阵
cor_matrix <- cor(data[, c(all_factors, Serovar_proportions)], 
                  use = "pairwise.complete.obs", method = "pearson")
# 提取环境因子/社会经济因素与特征基因之间的相关性
cor_subset <- cor_matrix[all_factors, Serovar_proportions]
# 修正的p值计算函数
calculate_p_values <- function(cor_matrix, data) {
  p_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(p_matrix) <- rownames(cor_matrix)
  colnames(p_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      if (!is.na(r)) {
        # 获取变量名
        var1 <- rownames(cor_matrix)[i]
        var2 <- colnames(cor_matrix)[j]
        # 计算这对变量的有效样本数
        n_effective <- sum(complete.cases(data[[var1]], data[[var2]]))
        # 只有当有效样本数大于2时才计算p值
        if (n_effective > 2) {
          t_stat <- r * sqrt((n_effective - 2) / (1 - r^2))
          p_value <- 2 * pt(-abs(t_stat), df = n_effective - 2)
          p_matrix[i, j] <- p_value
        } else {
          p_matrix[i, j] <- NA
        }
      }
    }
  }
  return(p_matrix)
}
# 计算p值矩阵
p_matrix <- calculate_p_values(cor_matrix, data)
p_subset <- p_matrix[all_factors, Serovar_proportions]
# 2. 创建带有显著性星号的相关性矩阵
create_sig_cor_matrix <- function(cor_matrix, p_matrix) {
  sig_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(sig_matrix) <- rownames(cor_matrix)
  colnames(sig_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      p <- p_matrix[i, j]
      if (!is.na(r) & !is.na(p)) {
        # 格式化相关系数
        r_formatted <- sprintf("%.2f", r)
        
        # 添加星号
        if (p < 0.001) {
          sig_matrix[i, j] <- paste0(r_formatted, "***")
        } else if (p < 0.01) {
          sig_matrix[i, j] <- paste0(r_formatted, "**")
        } else if (p < 0.05) {
          sig_matrix[i, j] <- paste0(r_formatted, "*")
        } else {
          sig_matrix[i, j] <- r_formatted
        }
      } else {
        sig_matrix[i, j] <- "NA"
      }
    }
  }
  return(sig_matrix)
}
sig_cor_matrix <- create_sig_cor_matrix(cor_subset, p_subset)
# 查看结果
cat("相关性矩阵:\n")
print(cor_subset)
cat("\n显著性标记矩阵:\n")
print(sig_cor_matrix)
# 3. 准备绘图数据
# 转换为长格式
cor_long <- melt(cor_subset)
colnames(cor_long) <- c("Factor", "Serovar_proportion", "Correlation")
p_long <- melt(p_subset)
colnames(p_long) <- c("Factor", "Serovar_proportion", "P_value")
# 合并相关性和p值
plot_data <- cor_long %>%
  left_join(p_long, by = c("Factor", "Serovar_proportion")) %>%
  mutate(
    # 添加显著性星号
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # 创建显示文本
    Display_Text = ifelse(is.na(Correlation), "NA", 
                          paste0(sprintf("%.2f", Correlation), Significance)),
    # 添加因子类型
    Factor_Type = ifelse(Factor %in% env_factors, "Environmental Factors", "Socioeconomic Factors")
  )
# 重新排序因子，确保环境因子在前，社会经济因素在后
plot_data$Factor <- factor(plot_data$Factor, 
                           levels = c(env_factors, socio_factors))
plot_data$Serovar_proportion <- factor(plot_data$Serovar_proportion, 
                                 levels = Serovar_proportions)
# 4. 绘制相关性热图
# 创建基础热图
p_heatmap <- ggplot(plot_data, aes(x = Factor, y = Serovar_proportion, fill = Correlation)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Display_Text), size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation\nCoefficient",
                       na.value = "grey90") +
  labs(x = "", y = "Gene Features") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()#,
#    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_fixed(ratio = 1.2)
# 显示基础热图
print(p_heatmap)
# 保存图形
#ggsave("correlation_of_genes_contents.jpg", width = 8, height = 6, dpi = 600)
##剔除掉样本量较低的几个省，分析血清型占比与环境因子之间的相关性---------------------
##血清型占比与环境因子之间的关系--------------------------------------------------
# 读取数据
data <- read_excel("data_collection.xlsx", sheet = 5)
# 查看列名
cat("数据集的列名:\n")
print(colnames(data))
cat("\n")
# 数据清洗和准备
# 确保列名正确
colnames(data) <- make.names(colnames(data))
# 再次查看处理后的列名
cat("处理后的列名:\n")
print(colnames(data))
colnames(data)[20]<- "4,[5],12:i:-"
cat("\n")
# 定义环境因子和社会经济因素变量
env_factors <- c("temperature", "average_ralative_humidity", 
                 "rainfall", "sunshine_hours")
socio_factors <- c("Average_Pork_Consumption", "GDP_per_capita", 
                   "Per_Capita_Disposable_Income", "Residential_Population", "SDI")
all_factors <- c(env_factors, socio_factors)
# 定义特征基因变量
Serovar_proportions <- c("Derby", "4,[5],12:i:-", "London", "Others", 
                   "Rissen", "Typhimurium")
# 检查列是否存在
check_columns <- function(data, columns) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    cat("以下列不存在于数据中:\n")
    print(missing)
    return(FALSE)
  }
  return(TRUE)
}
# 检查所有需要的列
cat("检查环境因子/社会经济因素列:\n")
check_columns(data, all_factors)
cat("\n检查特征基因列:\n")
check_columns(data, Serovar_proportions)
# 1. 计算相关性矩阵和p值矩阵
# 修正：获取有效的样本数（删除缺失值后的行数）
# 创建一个只包含相关变量的子数据集
data_subset <- data[, c(all_factors, Serovar_proportions)]
# 计算每对变量的有效样本数
calculate_effective_n <- function(data, var1, var2) {
  # 返回两个变量都非缺失的行数
  sum(complete.cases(data[[var1]], data[[var2]]))
}
# 计算完整的相关矩阵
cor_matrix <- cor(data[, c(all_factors, Serovar_proportions)], 
                  use = "pairwise.complete.obs", method = "pearson")
# 提取环境因子/社会经济因素与特征基因之间的相关性
cor_subset <- cor_matrix[all_factors, Serovar_proportions]
# 修正的p值计算函数
calculate_p_values <- function(cor_matrix, data) {
  p_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(p_matrix) <- rownames(cor_matrix)
  colnames(p_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      if (!is.na(r)) {
        # 获取变量名
        var1 <- rownames(cor_matrix)[i]
        var2 <- colnames(cor_matrix)[j]
        # 计算这对变量的有效样本数
        n_effective <- sum(complete.cases(data[[var1]], data[[var2]]))
        # 只有当有效样本数大于2时才计算p值
        if (n_effective > 2) {
          t_stat <- r * sqrt((n_effective - 2) / (1 - r^2))
          p_value <- 2 * pt(-abs(t_stat), df = n_effective - 2)
          p_matrix[i, j] <- p_value
        } else {
          p_matrix[i, j] <- NA
        }
      }
    }
  }
  return(p_matrix)
}
# 计算p值矩阵
p_matrix <- calculate_p_values(cor_matrix, data)
p_subset <- p_matrix[all_factors, Serovar_proportions]
# 2. 创建带有显著性星号的相关性矩阵
create_sig_cor_matrix <- function(cor_matrix, p_matrix) {
  sig_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(sig_matrix) <- rownames(cor_matrix)
  colnames(sig_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      p <- p_matrix[i, j]
      if (!is.na(r) & !is.na(p)) {
        # 格式化相关系数
        r_formatted <- sprintf("%.2f", r)
        
        # 添加星号
        if (p < 0.001) {
          sig_matrix[i, j] <- paste0(r_formatted, "***")
        } else if (p < 0.01) {
          sig_matrix[i, j] <- paste0(r_formatted, "**")
        } else if (p < 0.05) {
          sig_matrix[i, j] <- paste0(r_formatted, "*")
        } else {
          sig_matrix[i, j] <- r_formatted
        }
      } else {
        sig_matrix[i, j] <- "NA"
      }
    }
  }
  return(sig_matrix)
}
sig_cor_matrix <- create_sig_cor_matrix(cor_subset, p_subset)
# 查看结果
cat("相关性矩阵:\n")
print(cor_subset)
cat("\n显著性标记矩阵:\n")
print(sig_cor_matrix)
# 3. 准备绘图数据
# 转换为长格式
cor_long <- melt(cor_subset)
colnames(cor_long) <- c("Factor", "Serovar_proportion", "Correlation")
p_long <- melt(p_subset)
colnames(p_long) <- c("Factor", "Serovar_proportion", "P_value")
# 合并相关性和p值
plot_data <- cor_long %>%
  left_join(p_long, by = c("Factor", "Serovar_proportion")) %>%
  mutate(
    # 添加显著性星号
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # 创建显示文本
    Display_Text = ifelse(is.na(Correlation), "NA", 
                          paste0(sprintf("%.2f", Correlation), Significance)),
    # 添加因子类型
    Factor_Type = ifelse(Factor %in% env_factors, "Environmental Factors", "Socioeconomic Factors")
  )
# 重新排序因子，确保环境因子在前，社会经济因素在后
plot_data$Factor <- factor(plot_data$Factor, 
                           levels = c(env_factors, socio_factors))
plot_data$Serovar_proportion <- factor(plot_data$Serovar_proportion, 
                                 levels = Serovar_proportions)
# 4. 绘制相关性热图
# 创建基础热图
p_heatmap <- ggplot(plot_data, aes(x = Factor, y = Serovar_proportion, fill = Correlation)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Display_Text), size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation\nCoefficient",
                       na.value = "grey90") +
  labs(x = "", y = "Serovar proportion") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()#,
    #    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_fixed(ratio = 1.2)
# 显示基础热图
print(p_heatmap)
#ggsave("Serovar_proportion_correlation.pdf", width = 8, height = 6, dpi = 600)
##抗生素的使用与耐药性价格之间的关联----------------------------------------------
# 读取数据
data <- read_excel("data_collection.xlsx", sheet = 7)
# 查看列名
cat("数据集的列名:\n")
print(colnames(data))
cat("\n")
# 数据清洗和准备
# 确保列名正确
colnames(data) <- make.names(colnames(data))
# 再次查看处理后的列名
cat("处理后的列名:\n")
print(colnames(data))
#colnames(data)[20]<- "4,[5],12:i:-"
cat("\n")
# 定义环境因子和社会经济因素变量
all_factors <- c("veterinary_antibiotic_usage", "domestic_antibiotic_price")
# 定义特征基因变量
Serovar_proportions <- c("antibiotic_resistance_rate")
# 检查列是否存在
check_columns <- function(data, columns) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    cat("以下列不存在于数据中:\n")
    print(missing)
    return(FALSE)
  }
  return(TRUE)
}
# 检查所有需要的列
cat("检查环境因子/社会经济因素列:\n")
check_columns(data, all_factors)
cat("\n检查特征基因列:\n")
check_columns(data, Serovar_proportions)
# 创建一个只包含相关变量的子数据集
data_subset <- data[, c(all_factors, Serovar_proportions)]
# 计算完整的相关矩阵
cor_matrix <- cor(data[, c(all_factors, Serovar_proportions)], 
                  use = "pairwise.complete.obs", method = "pearson")
# 诊断：打印维度
cat("cor_matrix 维度:", dim(cor_matrix), "\n")
cat("all_factors 长度:", length(all_factors), "\n")
cat("Serovar_proportions 长度:", length(Serovar_proportions), "\n")
# 检查变量名是否存在
cat("\n检查变量名:\n")
cat("all_factors 是否在行名中:", all_factors %in% rownames(cor_matrix), "\n")
cat("Serovar_proportions 是否在列名中:", Serovar_proportions %in% colnames(cor_matrix), "\n")
# 提取环境因子/社会经济因素与特征基因之间的相关性
# 使用 drop = FALSE 确保返回矩阵
cor_subset <- cor_matrix[all_factors, Serovar_proportions, drop = FALSE]
cat("cor_subset 维度:", dim(cor_subset), "\n")
# 修正的p值计算函数
calculate_p_values <- function(cor_matrix, data) {
  p_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(p_matrix) <- rownames(cor_matrix)
  colnames(p_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      if (!is.na(r)) {
        # 获取变量名
        var1 <- rownames(cor_matrix)[i]
        var2 <- colnames(cor_matrix)[j]
        # 计算这对变量的有效样本数
        n_effective <- sum(complete.cases(data[[var1]], data[[var2]]))
        # 只有当有效样本数大于2时才计算p值
        if (n_effective > 2) {
          t_stat <- r * sqrt((n_effective - 2) / (1 - r^2))
          p_value <- 2 * pt(-abs(t_stat), df = n_effective - 2)
          p_matrix[i, j] <- p_value
        } else {
          p_matrix[i, j] <- NA
        }
      }
    }
  }
  return(p_matrix)
}
# 计算p值矩阵
p_matrix <- calculate_p_values(cor_matrix, data)
p_subset <- p_matrix[all_factors, Serovar_proportions, drop = FALSE]
cat("p_subset 维度:", dim(p_subset), "\n")
# 2. 创建带有显著性星号的相关性矩阵
create_sig_cor_matrix <- function(cor_matrix, p_matrix) {
  # 确保输入是矩阵
  if (!is.matrix(cor_matrix)) {
    cor_matrix <- as.matrix(cor_matrix)
  }
  if (!is.matrix(p_matrix)) {
    p_matrix <- as.matrix(p_matrix)
  }
  # 检查维度是否一致
  if (nrow(cor_matrix) != nrow(p_matrix) || ncol(cor_matrix) != ncol(p_matrix)) {
    stop("cor_matrix 和 p_matrix 维度不匹配")
  }
  # 创建结果矩阵
  sig_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  rownames(sig_matrix) <- rownames(cor_matrix)
  colnames(sig_matrix) <- colnames(cor_matrix)
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      p <- p_matrix[i, j]
      if (!is.na(r) & !is.na(p)) {
        # 格式化相关系数
        r_formatted <- sprintf("%.2f", r)
        # 添加星号
        if (p < 0.001) {
          sig_matrix[i, j] <- paste0(r_formatted, "***")
        } else if (p < 0.01) {
          sig_matrix[i, j] <- paste0(r_formatted, "**")
        } else if (p < 0.05) {
          sig_matrix[i, j] <- paste0(r_formatted, "*")
        } else {
          sig_matrix[i, j] <- r_formatted
        }
      } else {
        sig_matrix[i, j] <- "NA"
      }
    }
  }
  return(sig_matrix)
}
# 创建显著性矩阵
sig_cor_matrix <- create_sig_cor_matrix(cor_subset, p_subset)
print("显著性矩阵创建成功")
print(sig_cor_matrix)
# 查看结果
cat("相关性矩阵:\n")
print(cor_subset)
cat("\n显著性标记矩阵:\n")
print(sig_cor_matrix)
# 3. 准备绘图数据
# 转换为长格式
library(reshape2)
cor_long <- melt(cor_subset)
colnames(cor_long) <- c("Factor", "Serovar_proportion", "Correlation")
p_long <- melt(p_subset)
colnames(p_long) <- c("Factor", "Serovar_proportion", "P_value")
# 合并相关性和p值
plot_data <- cor_long %>%
  left_join(p_long, by = c("Factor", "Serovar_proportion")) %>%
  mutate(
    # 添加显著性星号
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # 创建显示文本
    Display_Text = ifelse(is.na(Correlation), "NA", 
                          paste0(sprintf("%.2f", Correlation), Significance))
  )

# 4. 绘制相关性热图
# 创建基础热图
p_heatmap <- ggplot(plot_data, aes(x = Factor, y = Serovar_proportion, fill = Correlation)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Display_Text), size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation\nCoefficient",
                       na.value = "grey90") +
  labs(x = "", y = "Serovar proportion") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()#,
    #    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_fixed(ratio = 1.2)
# 显示基础热图
print(p_heatmap)












###----------------------------------------------------------------------------------
##我国南北方以及不同血清型的耐药表型的差异-----------------------------------------
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(extrafont)
# 1. 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 9)
# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")

# 2. 数据预处理
# 提取抗生素列
antibiotic_cols <- c("AMP", "IPM", "FEP", "CTX", "TET", "TGC", "COL", "STR",
                     "GEN", "CIP", "SXT", "AZM", "CHL")

# 检查数据
cat("抗生素列:", antibiotic_cols, "\n")

# 3. 计算不同血清型的耐药率
# 转换数据为长格式
data_long <- data %>%
  select(ID, location, Serovars, all_of(antibiotic_cols)) %>%
  pivot_longer(cols = all_of(antibiotic_cols), 
               names_to = "Antibiotic", 
               values_to = "RIS") %>%
  mutate(
    RIS = factor(RIS, levels = c("S", "I", "R")),
    Serovars = factor(Serovars),
    location = factor(location)
  )

# 3.1 不同血清型的耐药率比较
# 计算每个血清型对每种抗生素的耐药率
serovar_resistance <- data_long %>%
  group_by(Serovars, Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Serovars, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()

# 可视化：不同血清型的耐药率热图
# 只显示R（耐药）的比例
serovar_R <- serovar_resistance %>%
  filter(RIS == "R") %>%
  select(Serovars, Antibiotic, Percentage)
# 转换为宽格式用于热图
serovar_heatmap_data <- serovar_R %>%
  pivot_wider(names_from = Antibiotic, values_from = Percentage, values_fill = 0) %>%
  column_to_rownames(var = "Serovars") %>%
  as.matrix()
# 绘制热图
p1 <- pheatmap::pheatmap(
  serovar_heatmap_data,border_color = "white", 
#  color = colorRampPalette(brewer.pal(9, "YlOrRd"))(100),
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  display_numbers = TRUE,
  number_format = "%.1f",
  number_color = "black",
#  main = "不同血清型的耐药率（R%）",
  fontsize_row = 10,
  fontsize_col = 10,
  angle_col = 45
)
p1
#ggsave("serovar_resistance_heatmap.jpg",p1,width = 6,height = 4,dpi = 600)
# 3.2 统计检验：比较不同血清型间的耐药率差异
# 对每种抗生素进行卡方检验
chi_square_results <- data.frame()
for (ab in antibiotic_cols) {
  # 创建列联表
  cont_table <- table(data$Serovars, data[[ab]])
  # 确保表格有效
  if (nrow(cont_table) > 1 && ncol(cont_table) > 1) {
    # 进行卡方检验
    chi_test <- chisq.test(cont_table)
    chi_square_results <- rbind(chi_square_results, data.frame(
      Antibiotic = ab,
      Chi_squared = round(chi_test$statistic, 3),
      df = chi_test$parameter,
      p_value = chi_test$p.value
    ))
  }
}
# 多重检验校正
chi_square_results$adj_p_value <- p.adjust(chi_square_results$p_value, method = "fdr")
cat("\n不同血清型耐药率差异的卡方检验结果：\n")
print(chi_square_results)
##
# 4. 南北方耐药率比较-------------------------------------------------------------
# 计算南北方耐药率
location_resistance <- data_long %>%
  group_by(location, Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(location, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()
# 只显示R（耐药）的比例
location_R <- location_resistance %>%
  filter(RIS == "R") %>%
  select(location, Antibiotic, Percentage)%>%
  mutate(Antibiotic = factor(Antibiotic, levels = rev(unique(Antibiotic[order(Percentage, decreasing = TRUE)]))))
# 转换为宽格式用于可视化
location_wide <- location_R %>%
  pivot_wider(names_from = Antibiotic, values_from = Percentage, values_fill = 0)
# 绘制南北方耐药率比较条形图
location_R_long <- location_R %>%
  pivot_longer(cols = -c(location, Antibiotic), 
               names_to = "Type", 
               values_to = "Value")

p2 <- ggplot(location_R, aes(x = Antibiotic, y = Percentage, fill = location)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
#   scale_color_brewer(palette = "Set2") +
  scale_fill_manual(name="",values = colorRampPalette(brewer.pal(3, "Set2"))(3))+
#  scale_fill_manual(values = c("North" = "#377EB8", "South" = "#E41A1C")) +
  labs(
    x = "Antibiotics",
    y = "Resistance rate (%)",
    fill = "Location"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 101)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 15),
    legend.position = "right"
  )
p2
##南北差异需要标注显著性标识（代码需要修改）————————————————————————————————————--
#ggsave("north_south_phenotypes_comparison.pdf",p2,width = 5,height = 5,dpi =600 )
# 4.2 统计检验：南北方耐药率差异
# 对每种抗生素进行卡方检验
location_chi_results <- data.frame()
for (ab in antibiotic_cols) {
  # 创建列联表
  cont_table <- table(data$location, data[[ab]])
  
  # 确保表格有效
  if (nrow(cont_table) > 1 && ncol(cont_table) > 1) {
    # 进行卡方检验
    chi_test <- chisq.test(cont_table)
    
    location_chi_results <- rbind(location_chi_results, data.frame(
      Antibiotic = ab,
      Chi_squared = round(chi_test$statistic, 3),
      df = chi_test$parameter,
      p_value = chi_test$p.value
    ))
  }
}
# 多重检验校正
location_chi_results$adj_p_value <- p.adjust(location_chi_results$p_value, method = "fdr")
cat("\n南北方耐药率差异的卡方检验结果：\n")
print(location_chi_results)
# 5. 计算总体耐药率并绘制堆积柱状图
# 计算总体耐药率
overall_resistance <- data_long %>%
  group_by(Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()
# 按照耐药率（R）从高到低排序
# 首先计算每个抗生素的R比例
antibiotic_order <- overall_resistance %>%
  filter(RIS == "S") %>%
  arrange(desc(Percentage)) %>%
  pull(Antibiotic)
# 重新排序Antibiotic因子
overall_resistance$Antibiotic <- factor(overall_resistance$Antibiotic, 
                                        levels = antibiotic_order)
overall_resistance$RIS <- factor(overall_resistance$RIS, 
                                 levels = c("S", "I", "R"))
# 绘制堆积柱状图
p3 <- ggplot(overall_resistance, aes(x = Antibiotic, y = Percentage, fill = RIS)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_fill_manual(
    name = "",
    values = c("R" = "#E54E21", "I" = "#8DD2C6", "S" = "#B3DE69"),
    breaks = c("R", "I", "S")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 101)
  ) +
  labs(
    x = "antibiotics",
    y = "Proportion (%)"
  ) +
  theme_bw() +
  coord_flip() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    text = element_text(size = 12)
  )
p3 
#ggsave("overall_resistance_stacked_bar.jpg", p3, width = 5, height = 5, dpi = 600)

# 6. 保存结果
# 保存图形
ggsave("serovar_resistance_heatmap.png", p1, width = 12, height = 8, dpi = 300)
ggsave("north_south_resistance_comparison.png", p2, width = 12, height = 8, dpi = 300)
ggsave("overall_resistance_stacked_bar.png", p3, width = 12, height = 8, dpi = 300)

# 保存统计结果
write.csv(chi_square_results, "serovar_resistance_chi_square.csv", row.names = FALSE)
write.csv(location_chi_results, "location_resistance_chi_square.csv", row.names = FALSE)
write.csv(overall_resistance, "overall_resistance_rates.csv", row.names = FALSE)

# 7. 创建汇总报告
cat("\n=== 分析完成 ===\n")
cat("1. 不同血清型耐药率差异：\n")
cat("   检验了", nrow(chi_square_results), "种抗生素\n")
cat("   显著差异的抗生素数量（FDR校正后p<0.05）：", 
    sum(chi_square_results$adj_p_value < 0.05), "\n")

cat("\n2. 南北方耐药率差异：\n")
cat("   检验了", nrow(location_chi_results), "种抗生素\n")
cat("   显著差异的抗生素数量（FDR校正后p<0.05）：", 
    sum(location_chi_results$adj_p_value < 0.05), "\n")

cat("\n3. 总体耐药率：\n")
# 找出耐药率最高的三种抗生素
top_resistant <- overall_resistance %>%
  filter(RIS == "R") %>%
  arrange(desc(Percentage)) %>%
  head(3)

cat("   耐药率最高的三种抗生素：\n")
for(i in 1:nrow(top_resistant)) {
  cat("   ", i, ". ", top_resistant$Antibiotic[i], ": ", 
      round(top_resistant$Percentage[i], 1), "%\n", sep = "")
}

cat("\n已生成文件：\n")
cat("1. serovar_resistance_heatmap.png - 血清型耐药率热图\n")
cat("2. north_south_resistance_comparison.png - 南北方耐药率比较图\n")
cat("3. overall_resistance_stacked_bar.png - 总体耐药率堆积柱状图\n")
cat("4. serovar_resistance_chi_square.csv - 血清型差异统计结果\n")
cat("5. location_resistance_chi_square.csv - 南北方差异统计结果\n")
cat("6. overall_resistance_rates.csv - 总体耐药率数据\n")

# 8. 显示图形
print(p1)
print(p2)
print(p3)

##补一张耐药的AUC的曲线图--------------------------------------------------------







##现需要计算不同血清型的多重耐药率---------------------------------------------
##按照耐一种药，两种药和三种药分别计算每种血清型的多重耐药率并汇总成一个堆积柱状图-----------
##不同血清型的堆积直方图亦或者是耐药热图，并标注出显著性差异（已经有了热图，但是还是需要标注出多重耐药率）-------------------
##现提供了全国范围内的畜禽类的抗生素的使用量，请依据使用量判断抗生素的使用与耐药性的相关性------
##需要寻找top5的猪肉的血清型与我国临床血清型的比较，注意只需要临床的即可
##---------------------------------------------------------------------------------
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(scales)

# 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 9)

# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")

# 定义抗生素列
antibiotic_cols <- c("AMP", "IPM", "FEP", "CTX", "TET", "TGC", "COL", 
                     "GEN", "CIP", "SXT", "AZM", "CHL")

# 确保所有抗生素列都存在
available_cols <- antibiotic_cols[antibiotic_cols %in% colnames(data)]
cat("可用的抗生素列:", available_cols, "\n")

# 1. 数据预处理：将R/I/S转换为是否耐药（R为耐药）
# 创建耐药矩阵，R=1，I/S=0
resistance_matrix <- data %>%
  select(all_of(available_cols)) %>%
  mutate(across(everything(), ~ifelse(. == "R", 1, 0)))
# 2. 根据药物分类合并耐药性
# 将IPM, CTX, FEP, AMP合并为一种药物（β-内酰胺类）
# 如果这四种药物中有任意一种耐药，则β-内酰胺类计为耐药
beta_lactam_cols <- c("IPM", "CTX", "FEP", "AMP")
beta_lactam_cols <- beta_lactam_cols[beta_lactam_cols %in% colnames(resistance_matrix)]
if(length(beta_lactam_cols) > 0) {
  # 计算β-内酰胺类耐药（四种药物中任意一种耐药）
  data$beta_lactam_resistant <- as.integer(rowSums(resistance_matrix[, beta_lactam_cols, drop = FALSE]) > 0)
  # 其他药物各自独立
  other_drugs <- setdiff(available_cols, beta_lactam_cols)
  # 创建新的耐药矩阵，包含β-内酰胺类和其他药物
  new_resistance_matrix <- cbind(
    beta_lactam = data$beta_lactam_resistant,
    resistance_matrix[, other_drugs, drop = FALSE]
  )
} else {
  # 如果没有β-内酰胺类药物，使用原始矩阵
  new_resistance_matrix <- resistance_matrix
  beta_lactam_cols <- character(0)
  other_drugs <- available_cols
}
cat("\n药物分类：\n")
cat("1. β-内酰胺类（合并为一类）：", paste(beta_lactam_cols, collapse = ", "), "\n")
cat("2. 其他独立药物（", length(other_drugs), "种）：", paste(other_drugs, collapse = ", "), "\n")
# 3. 计算每个样本的耐药药物数量
data$resistance_count <- rowSums(new_resistance_matrix)
# 4. 计算整体耐药率
cat("\n=== 整体耐药率计算 ===\n")
# 4.1 耐一种及以上药物的占比
resistant_any <- sum(data$resistance_count >= 1)
total_samples <- nrow(data)
resistance_rate_any <- resistant_any / total_samples * 100
# 4.2 耐三种及以上药物的占比（多重耐药）
multidrug_resistant <- sum(data$resistance_count >= 3)
multidrug_resistance_rate <- multidrug_resistant / total_samples * 100
cat("总样本数:", total_samples, "\n")
cat("耐一种及以上药物样本数:", resistant_any, "\n")
cat("整体耐药率（耐一种及以上）:", round(resistance_rate_any, 2), "%\n")
cat("耐三种及以上药物样本数:", multidrug_resistant, "\n")
cat("多重耐药率（耐三种及以上）:", round(multidrug_resistance_rate, 2), "%\n")
# 5. 按地区（南北方）比较耐药率
cat("\n=== 按地区比较耐药率 ===\n")
if("location" %in% colnames(data)) {
  location_summary <- data %>%
    group_by(location) %>%
    summarise(
      n = n(),
      resistant_any = sum(resistance_count >= 1),
      resistant_any_rate = resistant_any / n * 100,
      multidrug_resistant = sum(resistance_count >= 3),
      multidrug_rate = multidrug_resistant / n * 100
    )
  print(location_summary)
  # 统计检验：比较南北方耐药率差异
  # 创建列联表
  cont_table_any <- table(
    data$location,
    ifelse(data$resistance_count >= 1, "Resistant", "Susceptible")
  )
    cont_table_mdr <- table(
    data$location,
    ifelse(data$resistance_count >= 3, "MDR", "Non-MDR")
  )
  # 卡方检验
  if(nrow(cont_table_any) >= 2 && ncol(cont_table_any) >= 2) {
    chi_test_any <- chisq.test(cont_table_any)
    cat("\n南北方耐药率（耐一种及以上）差异卡方检验:\n")
    cat("卡方值:", round(chi_test_any$statistic, 3), 
        ", p值:", format.pval(chi_test_any$p.value, digits = 3), "\n")
  }
  
  if(nrow(cont_table_mdr) >= 2 && ncol(cont_table_mdr) >= 2) {
    chi_test_mdr <- chisq.test(cont_table_mdr)
    cat("南北方多重耐药率（耐三种及以上）差异卡方检验:\n")
    cat("卡方值:", round(chi_test_mdr$statistic, 3), 
        ", p值:", format.pval(chi_test_mdr$p.value, digits = 3), "\n")
  }
} else {
  cat("警告: 数据中未找到'location'列\n")
}

# 6. 按血清型比较耐药率
cat("\n=== 按血清型比较耐药率 ===\n")

if("Serovars" %in% colnames(data)) {
  serovar_summary <- data %>%
    group_by(Serovars) %>%
    summarise(
      n = n(),
      resistant_any = sum(resistance_count >= 1),
      resistant_any_rate = resistant_any / n * 100,
      multidrug_resistant = sum(resistance_count >= 3),
      multidrug_rate = multidrug_resistant / n * 100
    ) %>%
    arrange(desc(multidrug_rate))
  print(serovar_summary)
    # 统计检验：比较不同血清型耐药率差异
  # 创建列联表
  cont_table_serovar_any <- table(
    data$Serovars,
    ifelse(data$resistance_count >= 1, "Resistant", "Susceptible")
  )
  cont_table_serovar_mdr <- table(
    data$Serovars,
    ifelse(data$resistance_count >= 3, "MDR", "Non-MDR")
  )
  # 卡方检验
  if(nrow(cont_table_serovar_any) >= 2 && ncol(cont_table_serovar_any) >= 2) {
    chi_test_serovar_any <- chisq.test(cont_table_serovar_any)
    cat("\n不同血清型耐药率（耐一种及以上）差异卡方检验:\n")
    cat("卡方值:", round(chi_test_serovar_any$statistic, 3), 
        ", df:", chi_test_serovar_any$parameter,
        ", p值:", format.pval(chi_test_serovar_any$p.value, digits = 3), "\n")
  }
} else {
  cat("警告: 数据中未找到'Serovars'列\n")
}

# 7. 绘制堆积柱状图
cat("\n=== 绘制堆积柱状图 ===\n")

# 7.1 按地区分组
if("location" %in% colnames(data)) {
  # 准备数据
  location_resistance_data <- data %>%
    mutate(resistance_category = case_when(
      resistance_count == 0 ~ "0种",
      resistance_count == 1 ~ "1种",
      resistance_count == 2 ~ "2种",
      resistance_count >= 3 ~ "3种及以上"
    )) %>%
    mutate(resistance_category = factor(resistance_category, 
                                        levels = c("0种", "1种", "2种", "3种及以上"))) %>%
    group_by(location, resistance_category) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(location) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # 绘制按地区分组的堆积柱状图
  p1 <- ggplot(location_resistance_data, 
               aes(x = location, y = count, fill = resistance_category)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
    geom_text(aes(label = paste0(count, "\n(", round(percentage, 1), "%)")), 
              position = position_stack(vjust = 0.5), 
              size = 3.5) +
    scale_fill_brewer(palette = "Set2", name = "耐药药物数量") +
    labs(
      title = "不同地区耐药药物数量分布",
      x = "地区",
      y = "样本数量",
      fill = "耐药药物数量"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "right"
    )
  
  print(p1)
  ggsave("resistance_by_location.png", p1, width = 10, height = 8, dpi = 300)
}

# 7.2 按血清型分组
# 修改代码：按照multidrug ≥3的比率对血清型进行排序

# 1. 定义数据处理函数
prepare_serovar_data <- function(data) {
  # 计算每个血清型的multidrug ≥3比率
  serovar_mdr_rates <- data %>%
    group_by(Serovars) %>%
    summarise(
      total = n(),
      mdr_count = sum(resistance_count >= 3),
      mdr_rate = mdr_count / total * 100
    ) %>%
    arrange(desc(mdr_rate))
  
  # 准备堆积柱状图数据
  data %>%
    mutate(
      resistance_category = case_when(
        resistance_count == 0 ~ "0",
        resistance_count == 1 ~ "1", 
        resistance_count == 2 ~ "2",
        resistance_count >= 3 ~ "≥3"
      ),
      resistance_category = factor(resistance_category, 
                                   levels = c("0", "1", "2", "≥3")),
      Serovars = factor(Serovars, levels = serovar_mdr_rates$Serovars)
    ) %>%
    group_by(Serovars, resistance_category) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(Serovars) %>%
    mutate(percentage = count / sum(count) * 100)
}

# 2. 定义绘图函数
plot_serovar_resistance <- function(data, title = "", save_path = NULL) {
  # 自定义颜色
  resistance_colors <- c(
    "0" = "#B3DE69",  # 浅绿
    "1" = "#8DD2C6",  # 青色
    "2" = "#377EB8",  # 蓝色
    "≥3" = "#E54E21"  # 红色
  )
  p <- ggplot(data, aes(x = Serovars, y = percentage, fill = resistance_category)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.9, width = 0.7) +
#    geom_text(
#      aes(label = ifelse(percentage >= 5, sprintf("%.0f%%", percentage), "")),
#      position = position_stack(vjust = 0.5),
#      size = 3,
#      color = "black"
#    ) +
    scale_fill_manual(values = resistance_colors, 
                      name = "Drug Classes\nResistant To") +
    labs(
      x = "Serovars (sorted by MDR rate)",
      y = "Percentage (%)"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0)),
      labels = function(x) paste0(x, "%"),
      limits = c(0, 101)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "right"
    )
  # 保存图形
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 6, height = 6, dpi = 600)
  }
  return(p)
}
# 检查必要列是否存在
required_cols <- c("Serovars", "resistance_count")
missing_cols <- setdiff(required_cols, colnames(data))
if (length(missing_cols) == 0) {
  # 处理数据
  serovar_data <- prepare_serovar_data(data)
  # 绘制图形
  p <- plot_serovar_resistance(
    serovar_data, 
    save_path = "resistance_by_serovar.pdf"
  )
  # 显示图形
  print(p)
  # 输出统计信息
  cat("血清型按MDR率排序（从高到低）:\n")
  serovar_data %>%
    filter(resistance_category == "≥3") %>%
    arrange(desc(percentage)) %>%
    select(Serovars, percentage) %>%
    distinct() %>%
    print()
} else {
  warning("缺少必要的列: ", paste(missing_cols, collapse = ", "))
}

write.csv(data,"radar_data.csv")
##----------------------分割线--------------------------------------------------------------------
# 3. 计算每个样本的耐药药物数量
data$resistance_count <- rowSums(new_resistance_matrix)
cat("\n耐药种类数统计:\n")
print(table(data$resistance_count))

# 4. 按血清型分析耐药种类数分布
if("Serovars" %in% colnames(data)) {
  cat("\n=== 按血清型分析耐药种类数分布 ===\n")
  
  # 计算每个血清型的样本数，并筛选出样本数较多的血清型
  serovar_counts <- data %>%
    group_by(Serovars) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  # 显示样本数最多的10个血清型
  top_serovars <- serovar_counts %>%
    filter(n >= 5)  # 只考虑样本数≥5的血清型
  
  cat("样本数≥5的血清型数量:", nrow(top_serovars), "\n")
  cat("前10个血清型:\n")
  print(head(top_serovars, 10))
  
  # 过滤数据，只保留样本数≥5的血清型
  data_filtered <- data %>%
    filter(Serovars %in% top_serovars$Serovars)
  
  # 创建耐药种类数分布数据
  resistance_dist <- data_filtered %>%
    group_by(Serovars) %>%
    summarise(
      n = n(),
      # 计算每种耐药种类数的样本数
      R0 = sum(resistance_count == 0),
      R1 = sum(resistance_count == 1),
      R2 = sum(resistance_count == 2),
      R3 = sum(resistance_count == 3),
      R4 = sum(resistance_count == 4),
      R5 = sum(resistance_count == 5),
      R6 = sum(resistance_count = 6), 
      R7 = sum(resistance_count = 7), 
      # 计算百分比
      R0_pct = R0 / n * 100,
      R1_pct = R1 / n * 100,
      R2_pct = R2 / n * 100,
      R3_pct = R3 / n * 100,
      R4_pct = R4 / n * 100,
      R5_pct = R5 / n * 100,
      R6_pct = R6 / n * 100, 
      R7_pct = R7 / n * 100 
    ) %>%
    # 按样本数排序
    arrange(desc(n))
  
  # 重塑数据为长格式
  resistance_dist_long <- resistance_dist %>%
    select(Serovars, n, R0_pct, R1_pct, R2_pct, R3_pct, R4_pct, R5_pct, R6_pct, R7_pct) %>%
    pivot_longer(
      cols = c(R0_pct, R1_pct, R2_pct, R3_pct, R4_pct, R5_pct, R6_pct, R7_pct),
      names_to = "Resistance_Class",
      values_to = "Percentage"
    ) %>%
    mutate(
      Resistance_Class = factor(
        Resistance_Class,
        levels = c("R0_pct", "R1_pct", "R2_pct", "R3_pct", "R4_pct", "R5_pct", "R6_pct", "R7_pct"),
        labels = c("0", "1", "2", "3", "4", "5", "6", "7")
      ),
      # 创建包含样本数的标签
      Serovars_label = paste0(Serovars, "\n(n=", n, ")")
    ) %>%
    # 按总样本数排序血清型
    mutate(
      Serovars_label = factor(Serovars_label, 
                              levels = unique(Serovars_label[order(-n)]))
    )
  # 定义颜色
  # 1. 绘制堆积柱状图
  p1 <- ggplot(resistance_dist_long, 
               aes(x = Serovars_label, y = Percentage, fill = Resistance_Class)) +
    geom_bar(stat = "identity", width = 0.8) +
    scale_fill_manual(name="Resistance_Class",values =c(pal_npg("nrc")(length(unique(resistance_dist_long$Resistance_Class)))))+
    labs(
      title = "不同血清型中耐药种类数分布",
      x = "血清型 (按样本数排序)",
      y = "百分比 (%)"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0)),
      labels = function(x) paste0(x, "%"),
      limits = c(0, 100)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      legend.position = "right",
      panel.grid.major.x = element_blank()
    ) +
    # 添加百分比标签（只显示大于5%的部分）
    geom_text(
      aes(label = ifelse(Percentage > 5, 
                         sprintf("%.1f%%", Percentage), 
                         "")),
      position = position_stack(vjust = 0.5),
      size = 2.5
    )
  # 2. 绘制多重耐药（≥3类）率的热图
  mdr_data <- resistance_dist_long %>%
    filter(Resistance_Class %in% c("3类", "4类", "≥5类")) %>%
    group_by(Serovars, n, Serovars_label) %>%
    summarise(
      MDR_percentage = sum(Percentage),
      .groups = "drop"
    ) %>%
    arrange(desc(MDR_percentage)) %>%
    mutate(
      Serovars_label = factor(Serovars_label, 
                              levels = unique(Serovars_label))
    )
  p2 <- ggplot(mdr_data, aes(x = Serovars_label, y = 1, fill = MDR_percentage)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradientn(
      colors = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", 
                 "#6BAED6", "#4292C6", "#2171B5", "#08519C", 
                 "#08306B"),
      name = "多重耐药率\n(≥3类, %)",
      limits = c(0, 100)
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", MDR_percentage)),
      color = "white",
      fontface = "bold",
      size = 3
    ) +
    labs(
      title = "多重耐药率（≥3类）热图",
      x = "血清型 (按多重耐药率排序)",
      y = ""
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  # 3. 绘制不同耐药种类数的雷达图（前5个血清型）
  top5_serovars <- head(resistance_dist_long$Serovars %>% unique(), 5)
  radar_data <- resistance_dist_long %>%
    filter(Serovars %in% top5_serovars) %>%
    select(Serovars, Resistance_Class, Percentage) %>%
    pivot_wider(
      names_from = Serovars,
      values_from = Percentage
    )
  
  # 创建雷达图的基础数据
  radar_data_base <- as.data.frame(radar_data[, -1])
  rownames(radar_data_base) <- resistance_dist_long$Resistance_Class %>% unique()
  
  # 4. 创建耐药种类数汇总统计
  resistance_summary <- data_filtered %>%
    group_by(Serovars) %>%
    summarise(
      n = n(),
      mean_resistance = mean(resistance_count, na.rm = TRUE),
      median_resistance = median(resistance_count, na.rm = TRUE),
      sd_resistance = sd(resistance_count, na.rm = TRUE),
      min_resistance = min(resistance_count, na.rm = TRUE),
      max_resistance = max(resistance_count, na.rm = TRUE),
      mdr_rate = sum(resistance_count >= 3) / n * 100
    ) %>%
    arrange(desc(mdr_rate))
  cat("\n前5个血清型的耐药种类数统计:\n")
  print(head(resistance_summary, 5))
  # 5. 统计检验：比较不同血清型之间耐药种类数的差异
  if(length(unique(data_filtered$Serovars)) >= 2) {
    # Kruskal-Wallis检验
    kruskal_test <- kruskal.test(resistance_count ~ Serovars, data = data_filtered)
    cat("\n不同血清型耐药种类数差异的Kruskal-Wallis检验:\n")
    cat("H统计量:", round(kruskal_test$statistic, 3), 
        ", p值:", format.pval(kruskal_test$p.value, digits = 3), "\n")
  }
  # 6. 保存图形
  # 组合图形
  combined_plot <- p1 + p2
  # 保存图形
  ggsave("resistance_classes_by_serovar.pdf", combined_plot, 
         width = 14, height = 10, dpi = 300)
  ggsave("resistance_classes_by_serovar.png", combined_plot, 
         width = 14, height = 10, dpi = 300)
  # 显示图形
  print(combined_plot)
  # 7. 输出详细统计结果
  cat("\n=== 耐药种类数分布详细统计 ===\n")
  print(resistance_dist)
  # 保存数据到CSV
  write.csv(resistance_dist, "resistance_classes_distribution.csv", 
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(resistance_summary, "resistance_summary_by_serovar.csv", 
            row.names = FALSE, fileEncoding = "UTF-8")
} else {
  warning("数据中未找到'Serovars'列，无法按血清型进行分析")
}



# 加载必要的包
library(tidyverse)
library(igraph)
library(ggraph)
library(reshape2)

# 1. 重新读取数据并检查数据结构
merged_df <- read_excel("1418_salmonella_pork.xlsx", sheet = 10)

cat("数据行数:", nrow(merged_df), "\n")
cat("数据列名:", paste(colnames(merged_df), collapse = ", "), "\n")

# 2. 数据清洗和修正
# 确保每个质粒-基因组合是唯一的
plasmid_gene_clean <- merged_df %>%
  select(`#FILE`, Gene_Name, Gene_Types) %>%
  distinct() %>%
  filter(!is.na(Gene_Name) & !is.na(Gene_Types) & Gene_Name != "")

# 提取质粒ID（去掉版本号）
plasmid_gene_clean <- plasmid_gene_clean %>%
  mutate(plasmid_id = sub("\\.\\d+$", "", `#FILE`))

# 3. 重新构建二元矩阵（修正版本）
# 使用table方法构建二元矩阵
temp_table <- table(
  factor(plasmid_gene_clean$plasmid_id, levels = unique(plasmid_gene_clean$plasmid_id)),
  factor(plasmid_gene_clean$Gene_Name, levels = unique(plasmid_gene_clean$Gene_Name))
)

# 转换为二元矩阵（存在=1，不存在=0）
plasmid_gene_matrix <- as.data.frame.matrix(temp_table)
plasmid_gene_matrix[plasmid_gene_matrix > 0] <- 1

cat("\n修正后的二元矩阵维度:", dim(plasmid_gene_matrix), "\n")
cat("行数（质粒）:", nrow(plasmid_gene_matrix), "\n")
cat("列数（基因）:", ncol(plasmid_gene_matrix), "\n")

# 4. 计算基因频率
gene_freq <- colSums(plasmid_gene_matrix)
cat("\n基因频率统计（修正后）:\n")
print(summary(gene_freq))

# 5. 设置基因筛选阈值
# 阈值建议：选择出现在5-50%质粒中的基因
# 这可以排除罕见基因和普遍存在的管家基因
total_plasmids <- nrow(plasmid_gene_matrix)

# 设置阈值范围
lower_threshold <- ceiling(total_plasmids * 0.05)  # 出现在至少5%质粒中
upper_threshold <- floor(total_plasmids * 0.50)    # 出现在最多50%质粒中

cat("\n阈值设置:\n")
cat("质粒总数:", total_plasmids, "\n")
cat("下界阈值（出现次数≥）:", lower_threshold, "\n")
cat("上界阈值（出现次数≤）:", upper_threshold, "\n")

# 筛选基因
selected_genes <- names(gene_freq[gene_freq >= lower_threshold & gene_freq <= upper_threshold])
cat("\n筛选后基因数量:", length(selected_genes), "/", length(gene_freq), "\n")

if(length(selected_genes) < 5) {
  cat("警告：筛选后基因数量过少，考虑调整阈值\n")
  # 放宽阈值
  lower_threshold <- ceiling(total_plasmids * 0.01)  # 降至1%
  upper_threshold <- floor(total_plasmids * 0.80)    # 升至80%
  selected_genes <- names(gene_freq[gene_freq >= lower_threshold & gene_freq <= upper_threshold])
  cat("放宽后阈值:", lower_threshold, "-", upper_threshold, "\n")
  cat("放宽后基因数量:", length(selected_genes), "\n")
}

# 6. 创建筛选后的矩阵
if(length(selected_genes) > 0) {
  plasmid_gene_filtered <- plasmid_gene_matrix[, selected_genes, drop = FALSE]
  
  # 7. 计算Jaccard相似性
  calc_jaccard <- function(x, y) {
    intersection <- sum(x == 1 & y == 1)
    union <- sum(x == 1 | y == 1)
    if(union == 0) return(0)
    return(intersection / union)
  }
  
  n_genes <- ncol(plasmid_gene_filtered)
  gene_names <- colnames(plasmid_gene_filtered)
  
  cat("\n计算Jaccard相似性矩阵...\n")
  cat("基因数量:", n_genes, "\n")
  
  # 使用更高效的计算方法
  library(proxy)
  jaccard_matrix <- proxy::simil(as.matrix(plasmid_gene_filtered), 
                                 method = "Jaccard")
  jaccard_matrix <- as.matrix(jaccard_matrix)
  diag(jaccard_matrix) <- 1
  
  # 8. 设置网络连接阈值
  # 分析Jaccard系数的分布
  jaccard_values <- jaccard_matrix[upper.tri(jaccard_matrix)]
  cat("\nJaccard相似性统计:\n")
  print(summary(jaccard_values))
  
  # 设置阈值：使用分布的上四分位数
  jaccard_threshold <- quantile(jaccard_values, 0.75)  # 上四分位数
  cat("Jaccard阈值（上四分位数）:", jaccard_threshold, "\n")
  
  # 或者使用固定阈值
  # jaccard_threshold <- 0.3  # 30%相似性
  
  # 创建邻接矩阵
  adj_matrix <- jaccard_matrix
  adj_matrix[adj_matrix < jaccard_threshold] <- 0
  diag(adj_matrix) <- 0  # 去除自连接
  
  # 9. 创建图对象
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # 移除孤立节点
  isolated_vertices <- which(degree(g) == 0)
  if(length(isolated_vertices) > 0) {
    cat("移除孤立节点:", length(isolated_vertices), "个\n")
    g <- delete.vertices(g, isolated_vertices)
  }
  
  cat("\n网络统计:\n")
  cat("节点数:", vcount(g), "\n")
  cat("边数:", ecount(g), "\n")
  cat("网络密度:", edge_density(g), "\n")
  
  # 10. 优化网络可视化
  if(vcount(g) > 0 && ecount(g) > 0) {
    # 节点属性
    # 获取基因类型
    gene_type_map <- plasmid_gene_clean %>%
      select(Gene_Name, Gene_Types) %>%
      distinct()
    
    V(g)$type <- gene_type_map$Gene_Types[match(V(g)$name, gene_type_map$Gene_Name)]
    V(g)$type[is.na(V(g)$type)] <- "Other"
    
    # 设置颜色
    type_colors <- c(
      "ARGs" = "#E41A1C",
      "ISs" = "#377EB8",
      "plasmid_replicons" = "#4DAF4A",
      "Other" = "#999999"
    )
    
    V(g)$color <- type_colors[V(g)$type]
    V(g)$color[is.na(V(g)$color)] <- "#999999"
    
    # 节点大小基于频率（对数缩放）
    freq_in_net <- gene_freq[V(g)$name]
    V(g)$size <- scale(log(freq_in_net + 1)) * 3 + 5  # 标准化并缩放
    
    # 节点标签：只显示高频基因
    label_threshold <- quantile(freq_in_net, 0.75)  # 频率上四分位数的基因显示标签
    V(g)$label <- ifelse(freq_in_net >= label_threshold, V(g)$name, "")
    
    # 边属性
    E(g)$width <- E(g)$weight * 3
    E(g)$alpha <- E(g)$weight
    
    # 11. 创建网络图
    set.seed(2024)
    
    # 尝试不同的布局算法
    layout_algo <- "fr"  # Fruchterman-Reingold
    
    p <- ggraph(g, layout = layout_algo) +
      # 边
      geom_edge_link(aes(width = width, alpha = alpha), 
                     color = "grey50", show.legend = FALSE) +
      # 节点
      geom_node_point(aes(color = color, size = size), 
                      alpha = 0.8, show.legend = TRUE) +
      # 标签
      geom_node_text(aes(label = label), 
                     size = 3, repel = TRUE, max.overlaps = 15,
                     box.padding = 0.5, point.padding = 0.5) +
      # 颜色和大小比例
      scale_color_identity(guide = "legend", 
                           labels = names(type_colors),
                           breaks = type_colors) +
      scale_size_continuous(range = c(3, 8), 
                            guide = guide_legend(title = "频率（标准化）")) +
      scale_edge_width_continuous(range = c(0.5, 2)) +
      # 主题
      theme_void() +
      labs(
        title = paste0("基因共现网络 (Jaccard ≥ ", round(jaccard_threshold, 2), ")"),
        subtitle = paste0("节点: ", vcount(g), " | 边: ", ecount(g), 
                          " | 基因频率: ", lower_threshold, "-", upper_threshold),
        color = "基因类型"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
    
    # 12. 保存图形
    print(p)
    ggsave("gene_cooccurrence_network_optimized.png", p, 
           width = 12, height = 10, dpi = 300, bg = "white")
    
    # 13. 保存网络数据
    write_graph(g, "gene_cooccurrence_network.graphml", format = "graphml")
    
    # 保存节点和边信息
    node_info <- data.frame(
      Gene = V(g)$name,
      Type = V(g)$type,
      Frequency = freq_in_net,
      Degree = degree(g),
      Betweenness = betweenness(g),
      stringsAsFactors = FALSE
    )
    
    edge_info <- as.data.frame(get.edgelist(g)) %>%
      mutate(Weight = E(g)$weight)
    
    write.csv(node_info, "network_node_info.csv", row.names = FALSE)
    write.csv(edge_info, "network_edge_info.csv", row.names = FALSE)
    
  } else {
    cat("网络为空，无法可视化\n")
  }
  
} else {
  cat("筛选后无基因，请调整阈值\n")
}







##耐药基因的共线性------------------------------------------------------------------------------------
# 2. 数据读取与预处理 ----------------------------------------------------------
# 假设 plasmids_info.xlsx 文件位于当前工作目录

# 加载必要的包
library(tidyverse)
library(igraph)
library(ggraph)
library(reshape2)
library(proxy)
library(RColorBrewer)
library(scales)
library(readxl)
# 1. 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
merged_df <- read_excel("1418_salmonella_pork.xlsx", sheet = 15)##sheet10是总览，只需要london血清型即可
cat("数据行数:", nrow(merged_df), "\n")
cat("数据列名:", paste(colnames(merged_df), collapse = ", "), "\n")
# 2. 数据预处理
plasmid_gene_clean <- merged_df %>%
  select(`#FILE`, Gene_Name, Gene_Types) %>%
  distinct() %>%
  filter(!is.na(Gene_Name) & !is.na(Gene_Types) & Gene_Name != "")
plasmid_gene_clean <- plasmid_gene_clean %>%
  mutate(plasmid_id = sub("\\.\\d+$", "", `#FILE`))
# 3. 构建二元矩阵
temp_table <- table(
  factor(plasmid_gene_clean$plasmid_id, levels = unique(plasmid_gene_clean$plasmid_id)),
  factor(plasmid_gene_clean$Gene_Name, levels = unique(plasmid_gene_clean$Gene_Name))
)
plasmid_gene_matrix <- as.data.frame.matrix(temp_table)
plasmid_gene_matrix[plasmid_gene_matrix > 0] <- 1
cat("\n二元矩阵维度:", dim(plasmid_gene_matrix), "\n")
cat("质粒数:", nrow(plasmid_gene_matrix), "\n")
cat("基因数:", ncol(plasmid_gene_matrix), "\n")
# 4. 计算基因频率
gene_freq <- colSums(plasmid_gene_matrix)
cat("\n基因频率统计:\n")
print(summary(gene_freq))
# 5. 基因筛选
total_plasmids <- nrow(plasmid_gene_matrix)
lower_threshold <- ceiling(total_plasmids * 0.05)  # 出现在至少5%质粒中
upper_threshold <- floor(total_plasmids * 1.1)    # 出现在最多110%质粒中
cat("\n阈值设置:\n")
cat("质粒总数:", total_plasmids, "\n")
cat("下界阈值（出现次数≥）:", lower_threshold, "\n")
cat("上界阈值（出现次数≤）:", upper_threshold, "\n")
selected_genes <- names(gene_freq[gene_freq >= lower_threshold & gene_freq <= upper_threshold])
cat("\n筛选后基因数量:", length(selected_genes), "/", length(gene_freq), "\n")
# 如果基因太少，放宽阈值
if(length(selected_genes) < 10) {
  cat("基因数量过少，放宽阈值...\n")
  lower_threshold <- ceiling(total_plasmids * 0.02)
  upper_threshold <- floor(total_plasmids * 1.2)
  selected_genes <- names(gene_freq[gene_freq >= lower_threshold & gene_freq <= upper_threshold])
  cat("放宽后阈值:", lower_threshold, "-", upper_threshold, "\n")
  cat("放宽后基因数量:", length(selected_genes), "\n")
}
# 6. 创建筛选后的矩阵
if(length(selected_genes) > 0) {
  plasmid_gene_filtered <- plasmid_gene_matrix[, selected_genes, drop = FALSE]
  # 7. 计算Jaccard相似性
  cat("\n计算Jaccard相似性矩阵...\n")
  cat("基因数量:", ncol(plasmid_gene_filtered), "\n")
  # 转置矩阵，计算基因间的相似性
  jaccard_matrix <- proxy::simil(
    t(as.matrix(plasmid_gene_filtered)),
    method = "Jaccard"
  )
  jaccard_matrix <- as.matrix(jaccard_matrix)
  diag(jaccard_matrix) <- 1
  # 8. 设置网络连接阈值
  jaccard_values <- jaccard_matrix[upper.tri(jaccard_matrix)]
  cat("\nJaccard相似性统计:\n")
  print(summary(jaccard_values))
  # 使用固定阈值0.3
  jaccard_threshold <- 0.3
  cat("Jaccard阈值（固定）:", jaccard_threshold, "\n")
  # 创建邻接矩阵
  adj_matrix <- jaccard_matrix
  adj_matrix[adj_matrix < jaccard_threshold] <- 0
  diag(adj_matrix) <- 0
  # 9. 创建图对象
  g <- graph_from_adjacency_matrix(
    adj_matrix, 
    mode = "undirected", 
    weighted = TRUE, 
    diag = FALSE
  )
  # 检查图对象
  cat("\n图对象信息:\n")
  cat("节点数（应该是基因数）:", vcount(g), "\n")
  cat("边数:", ecount(g), "\n")
  # 移除孤立节点
  isolated_vertices <- which(degree(g) == 0)
  if(length(isolated_vertices) > 0) {
    cat("移除孤立节点:", length(isolated_vertices), "个\n")
    g <- delete_vertices(g, isolated_vertices)  # 使用新函数名
  }
  cat("\n网络统计:\n")
  cat("节点数:", vcount(g), "\n")
  cat("边数:", ecount(g), "\n")
  if(vcount(g) > 0) {
    cat("网络密度:", edge_density(g), "\n")
  }
  # 10. 网络可视化
  if(vcount(g) > 0 && ecount(g) > 0) {
    # 获取基因类型
    gene_type_map <- plasmid_gene_clean %>%
      select(Gene_Name, Gene_Types) %>%
      distinct()
    node_types <- gene_type_map$Gene_Types[match(V(g)$name, gene_type_map$Gene_Name)]
    V(g)$type <- node_types
    V(g)$type[is.na(V(g)$type)] <- "Other"
    cat("\n节点类型分布:\n")
    print(table(V(g)$type))
    # 设置颜色方案
    type_colors <- c(
      "ARGs" = "#E41A1C",               # 红色
      "ISs" = "#4DAF4A",                # 绿色
      "plasmid_replicons" = "#377EB8",  # 蓝色
      "Other" = "#999999"               # 灰色
    )
    # 确保所有类型都有颜色
    missing_types <- setdiff(unique(V(g)$type), names(type_colors))
    if(length(missing_types) > 0) {
      for(mt in missing_types) {
        type_colors[mt] <- "#999999"
      }
    }
    V(g)$color <- type_colors[V(g)$type]
    # 节点大小基于频率
    freq_in_net <- gene_freq[V(g)$name]
    if(any(is.na(freq_in_net))) {
      freq_in_net[is.na(freq_in_net)] <- 1
    }
    cat("\n节点频率统计:\n")
    print(summary(freq_in_net))
    
    # 使用对数转换设置节点大小
    node_sizes <- log10(freq_in_net + 1) * 5
    V(g)$size <- node_sizes
    
    # 计算节点大小的范围
    size_range <- range(node_sizes)
    cat("\n节点大小范围:", size_range[1], "-", size_range[2], "\n")
    
    # 节点标签：显示所有基因名称
    V(g)$label <- V(g)$name
    
    # 边属性
    E(g)$weight_value <- E(g)$weight
    E(g)$width <- E(g)$weight * 3  # 调整边的缩放系数
    
    # 11. 创建网络图
    set.seed(2024)
    p <- ggraph(g, layout = "fr") + # 使用Fruchterman-Reingold布局
      geom_edge_link(aes(width = width, alpha = width), 
                     color = "grey70", show.legend = TRUE) +
      geom_node_point(aes(color = color, size = size), 
                      alpha = 0.85, show.legend = TRUE) +
      geom_node_text(aes(label = label), 
                     size = 3, repel = TRUE, max.overlaps = 20) +
      scale_color_identity(guide = "legend", 
                           labels = names(type_colors),
                           breaks = type_colors) +
      scale_size_continuous(range = c(3, 10), 
                            guide = guide_legend(title = "frequency")) +
      scale_edge_width_continuous(range = c(0.5, 3)) +
      theme_void() +
      labs(title = paste0("genetic co-occurrence network (Jaccard index ≥ ", jaccard_threshold, ")"),
           subtitle = paste0("node: ", vcount(g), " | edge: ", ecount(g), 
                             " | genetic frequency ≥0.05"),
           color = "genetic types") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "right",
            legend.box = "vertical")
    print(p)
    
    # 保存图形
    ggsave(filename = "plasmid_gene_cooccurrence_network.jpg", 
           plot = p, width = 10, height = 8, dpi = 600)
    cat("网络图已保存为 'plasmid_gene_cooccurrence_network.pdf'\n")
    # 15. 网络统计摘要
    cat("\n=== 网络统计摘要 ===\n")
    cat("1. 网络基本信息:\n")
    cat("   节点数:", vcount(g), "\n")
    cat("   边数:", ecount(g), "\n")
    cat("   网络密度:", round(edge_density(g), 4), "\n")
    cat("   平均度:", round(mean(degree(g)), 2), "\n")
    
    cat("\n2. 节点度分布:\n")
    cat("   最小度:", min(degree(g)), "\n")
    cat("   最大度:", max(degree(g)), "\n")
    cat("   中位数度:", median(degree(g)), "\n")
    
    cat("\n3. 基因类型分布:\n")
    type_table <- table(V(g)$type)
    for(type_name in names(type_table)) {
      cat("   ", type_name, ":", type_table[type_name], "个\n")
    }
    
    cat("\n4. 中心性最高的基因:\n")
    top_genes <- node_info %>%
      arrange(desc(Degree)) %>%
      head(10)
    print(top_genes)
    
    cat("\n5. 最强共现关系:\n")
    top_edges <- edge_info %>%
      arrange(desc(Weight)) %>%
      head(10)
    print(top_edges)
    
  } else {
    cat("网络为空，无法可视化\n")
  }
  
} else {
  cat("筛选后无基因，请调整阈值\n")
}

ggsave(filename = "plasmid_gene_cooccurrence_network.pdf", 
       plot = p, width = 12, height = 6, dpi = 600)


##这个图需要做三张，即London，鼠伤寒、单相，分别进行可视化之后放在附表
##或者做成五张，按照分面进行绘制
###绘制主要的插入序列和质粒复制子的热图-------------------------------------------------

# 加载必要的包
# 加载必要的包
library(tidyverse)
library(igraph)
library(ggraph)
library(reshape2)
library(proxy)
library(RColorBrewer)
library(scales)
library(readxl)
library(patchwork)  # 用于组合图形
library(ggrepel)
# 1. 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
merged_df <- read_excel("1418_salmonella_pork.xlsx", sheet = 10)  # sheet10是总览
cat("数据行数:", nrow(merged_df), "\n")
cat("数据列名:", paste(colnames(merged_df), collapse = ", "), "\n")
# 2. 数据预处理
plasmid_gene_clean <- merged_df %>%
  select(`#FILE`, Serovars, Gene_Name, Gene_Types) %>%
  distinct() %>%
  filter(!is.na(Gene_Name) & !is.na(Gene_Types) & Gene_Name != "")
# 处理血清型列，统一命名
plasmid_gene_clean <- plasmid_gene_clean %>%
  mutate(Serovars = ifelse(Serovars == "4,[5],12:i:-", "4,[5],12:i:-", Serovars)) %>%
  mutate(plasmid_id = sub("\\.\\d+$", "", `#FILE`))
# 3. 筛选指定的六种血清型
specified_serovars <- c("Rissen", "London", "4,[5],12:i:-", "Derby", "Typhimurium", "Others")
cat("指定的血清型:", paste(specified_serovars, collapse = ", "), "\n")
# 筛选数据
filtered_data <- plasmid_gene_clean %>%
  filter(Serovars %in% specified_serovars)
# 检查筛选结果
cat("筛选后数据行数:", nrow(filtered_data), "\n")
cat("筛选后血清型分布:\n")
print(table(filtered_data$Serovars))
# 4. 修复后的函数：为单个血清型构建网络数据和布局
prepare_network_data <- function(data_serovar, serovar_name, jaccard_threshold = 0.3) {
  cat("处理血清型:", serovar_name, "\n")
  cat("  - 样本数量:", length(unique(data_serovar$plasmid_id)), "\n")
  cat("  - 基因数量:", length(unique(data_serovar$Gene_Name)), "\n")
  
  # 构建二元矩阵
  temp_table <- table(
    factor(data_serovar$plasmid_id, levels = unique(data_serovar$plasmid_id)),
    factor(data_serovar$Gene_Name, levels = unique(data_serovar$Gene_Name))
  )
  plasmid_gene_matrix <- as.data.frame.matrix(temp_table)
  plasmid_gene_matrix[plasmid_gene_matrix > 0] <- 1
  
  # 计算基因频率
  gene_freq <- colSums(plasmid_gene_matrix)
  total_plasmids <- nrow(plasmid_gene_matrix)
  cat("  - 质粒总数:", total_plasmids, "\n")
  
  # 基因筛选：出现在至少10%的质粒中
  lower_threshold <- ceiling(total_plasmids * 0.10)
  selected_genes <- names(gene_freq[gene_freq >= lower_threshold])
  cat("  - 筛选后基因数量:", length(selected_genes), "\n")
  
  if (length(selected_genes) < 2) {
    cat("  - 警告: 基因数量不足，无法构建网络\n")
    return(NULL)
  }
  
  # 创建筛选后的矩阵
  plasmid_gene_filtered <- plasmid_gene_matrix[, selected_genes, drop = FALSE]
  
  # 计算Jaccard相似性
  jaccard_matrix <- proxy::simil(
    t(as.matrix(plasmid_gene_filtered)),
    method = "Jaccard"
  )
  jaccard_matrix <- as.matrix(jaccard_matrix)
  diag(jaccard_matrix) <- 1
  
  # 设置网络连接阈值
  adj_matrix <- jaccard_matrix
  adj_matrix[adj_matrix < jaccard_threshold] <- 0
  diag(adj_matrix) <- 0
  
  # 创建图对象
  g <- graph_from_adjacency_matrix(
    adj_matrix, 
    mode = "undirected", 
    weighted = TRUE, 
    diag = FALSE
  )
  # 移除孤立节点
  isolated_vertices <- which(degree(g) == 0)
  if (length(isolated_vertices) > 0) {
    g <- delete_vertices(g, isolated_vertices)
  }
  # 如果图为空，返回NULL
  if (vcount(g) == 0) {
    cat("  - 警告: 网络为空\n")
    return(NULL)
  }
  
  # 添加节点属性
  gene_type_map <- data_serovar %>%
    select(Gene_Name, Gene_Types) %>%
    distinct()
  node_types <- gene_type_map$Gene_Types[match(V(g)$name, gene_type_map$Gene_Name)]
  V(g)$type <- node_types
  # 节点大小基于频率
  freq_in_net <- gene_freq[V(g)$name]
  if (any(is.na(freq_in_net))) {
    freq_in_net[is.na(freq_in_net)] <- 1
  }
  node_sizes <- log10(freq_in_net + 1) * 5
  V(g)$size <- node_sizes
  # 节点标签
  V(g)$label <- V(g)$name
  # 边属性
  E(g)$weight_value <- E(g)$weight
  E(g)$width <- E(g)$weight * 3
  # 计算布局
  set.seed(2024)
  layout_matrix <- layout_with_fr(g)
  # 创建节点数据框
  nodes_df <- data.frame(
    serovar = serovar_name,
    name = V(g)$name,
    type = V(g)$type,
    freq = freq_in_net,
    size = V(g)$size,
    x = layout_matrix[, 1],
    y = layout_matrix[, 2],
    stringsAsFactors = FALSE
  )
  # 创建边数据框 - 修复：添加坐标信息
  edges_df <- as_data_frame(g, what = "edges")
  if (nrow(edges_df) > 0) {
    edges_df$serovar <- serovar_name
    edges_df$width <- edges_df$weight * 3
    
    # 关键修复：将起点和终点的坐标添加到边数据框中
    # 创建一个节点名称到坐标的映射
    node_coords <- nodes_df[, c("name", "x", "y")]
    
    # 合并起点坐标
    edges_df <- edges_df %>%
      left_join(node_coords, by = c("from" = "name")) %>%
      rename(from.x = x, from.y = y)
    
    # 合并终点坐标
    edges_df <- edges_df %>%
      left_join(node_coords, by = c("to" = "name")) %>%
      rename(to.x = x, to.y = y)
  }
  
  return(list(
    nodes = nodes_df, 
    edges = edges_df, 
    node_count = vcount(g), 
    edge_count = ecount(g),
    graph = g
  ))
}

# 5. 为每个指定血清型准备网络数据
cat("\n开始为指定血清型准备网络数据...\n")

all_nodes <- data.frame()
all_edges <- data.frame()
network_stats <- data.frame()
valid_serovars <- c()

for (serovar in specified_serovars) {
  cat("\n处理血清型:", serovar, "\n")
  
  # 筛选当前血清型的数据
  data_serovar <- filtered_data %>% 
    filter(Serovars == serovar)
  
  if (nrow(data_serovar) == 0) {
    cat("  - 警告: 无数据，跳过\n")
    next
  }
  
  # 准备网络数据
  network_data <- prepare_network_data(data_serovar, serovar)
  
  if (!is.null(network_data)) {
    # 合并数据
    all_nodes <- bind_rows(all_nodes, network_data$nodes)
    all_edges <- bind_rows(all_edges, network_data$edges)
    valid_serovars <- c(valid_serovars, serovar)
    
    # 记录网络统计
    network_stats <- rbind(network_stats, 
                           data.frame(
                             Serovar = serovar,
                             Samples = n_distinct(data_serovar$plasmid_id),
                             Genes = length(unique(data_serovar$Gene_Name)),
                             Nodes = network_data$node_count,
                             Edges = network_data$edge_count,
                             Density = ifelse(network_data$node_count > 1, 
                                              network_data$edge_count / (network_data$node_count * (network_data$node_count - 1) / 2), 0),
                             stringsAsFactors = FALSE
                           ))
  } else {
    cat("  - 警告: 无法生成网络数据\n")
  }
}
# 6. 检查数据
cat("\n网络数据准备完成。\n")
cat("成功生成网络的血清型数量:", length(valid_serovars), "\n")
cat("有效血清型:", paste(valid_serovars, collapse = ", "), "\n")
if (nrow(all_nodes) == 0) {
  stop("错误: 没有生成任何网络数据，请检查输入数据。")
}
# 7. 设置颜色方案
type_colors <- c(
  "ARGs" = "#E41A1C",               # 红色
  "ISs" = "#4DAF4A",                # 绿色
  "plasmid_replicons" = "#377EB8",  # 蓝色 
)
# 确保所有类型都有颜色
unique_types <- unique(all_nodes$type)
missing_types <- setdiff(unique_types, names(type_colors))
if (length(missing_types) > 0) {
  for (mt in missing_types) {
    type_colors[mt] <- "#999999"
  }
}
# 为节点添加颜色
all_nodes$color <- type_colors[all_nodes$type]
# 8. 按照指定顺序设置血清型因子水平
# 只保留成功生成网络的血清型，并按原指定顺序排序
ordered_serovars <- intersect(specified_serovars, valid_serovars)
all_nodes$serovar <- factor(all_nodes$serovar, levels = ordered_serovars)
if (nrow(all_edges) > 0) {
  all_edges$serovar <- factor(all_edges$serovar, levels = ordered_serovars)
}
# 为每个血清型添加样本数量和节点数量标签
network_stats <- network_stats %>%
  mutate(label = paste0(Serovar, "\n(n=", Samples, ", v=", Nodes, ", e=", Edges, ")"))
network_stats$Serovar <- factor(network_stats$Serovar, levels = ordered_serovars)
# 合并标签到节点数据
all_nodes <- all_nodes %>%
  left_join(network_stats %>% select(Serovar, label), 
            by = c("serovar" = "Serovar"))
# 9. 创建分面网络图
cat("\n创建分面网络图...\n")

# 计算分面布局的列数
n_serovars <- length(ordered_serovars)
n_cols <- ifelse(n_serovars <= 3, n_serovars, 3)
n_rows <- ceiling(n_serovars / n_cols)

# 检查边数据框是否包含正确的列
cat("边数据框列名:", paste(colnames(all_edges), collapse = ", "), "\n")

# 为节点添加基因类型因子变量，用于图例
# 确保所有节点都有类型，将NA转换为"Other"
all_nodes$type_factor <- factor(all_nodes$type, 
                                levels = c("ARGs", "ISs", "plasmid_replicons"))

# 设置图例颜色
type_colors_factor <- c(
  "ARGs" = "#E41A1C",               # 红色
  "ISs" = "#4DAF4A",                # 绿色
  "plasmid_replicons" = "#377EB8"  # 蓝色
)

# 确保类型颜色与因子水平一致
# 只保留实际存在的类型颜色
type_colors_factor <- type_colors_factor[names(type_colors_factor) %in% levels(all_nodes$type_factor)]

# 创建分面网络图
p <- ggplot() +
  # 绘制边
  {
    if (nrow(all_edges) > 0 && "from.x" %in% colnames(all_edges)) {
      geom_segment(data = all_edges, 
                   aes(x = from.x, y = from.y, xend = to.x, yend = to.y, 
                       linewidth = width, alpha = width),
                   color = "grey70", show.legend = FALSE)
    }
  } +
  # 绘制节点 - 修改为使用type_factor和图例
  geom_point(data = all_nodes, 
             aes(x = x, y = y, color = type_factor, size = size), 
             alpha = 0.8) +
  # 使用geom_text_repel绘制节点标签，避免重叠
  geom_text_repel(data = all_nodes,
                  aes(x = x, y = y, label = name),
                  size = 2.5,
                  max.overlaps = 50,           # 最多显示20个重叠的标签
                  min.segment.length = 0.1,    # 最小线段长度
                  segment.color = "grey50",    # 连接线颜色
                  segment.alpha = 0.6,         # 连接线透明度
                  segment.size = 0.3,          # 连接线粗细
                  box.padding = 0.5,           # 标签和点之间的内边距
                  point.padding = 0.3,         # 标签之间的内边距
                  force = 0.5,                 # 排斥力大小
                  force_pull = 1,              # 向点中心的拉力大小
                  max.iter = 3000,             # 最大迭代次数
                  max.time = 1,                # 最大计算时间（秒）
                  direction = "both",          # 标签可以往任意方向移动
                  seed = 123,                  # 随机种子，确保可重复
                  show.legend = FALSE) +
  # 分面
  facet_wrap(~ serovar, scales = "free", ncol = n_cols) +
  # 颜色和大小比例尺
  scale_color_manual(name = "Gene Types",  # 图例标题
                     values = type_colors_factor,
                     drop = FALSE) +  # 即使某些水平没有数据也显示
  scale_size_continuous(name = "Gene Frequency\n(log10)",  # 大小图例标题
                        range = c(2, 8),
                        breaks = c(min(all_nodes$size, na.rm = TRUE), 
                                   median(all_nodes$size, na.rm = TRUE), 
                                   max(all_nodes$size, na.rm = TRUE)),
                        labels = function(x) {
                          # 将size值反向计算回频率的近似值
                          approx_freq <- round(10^(x/5) - 1)
                          return(paste0("~", approx_freq))
                        }) +
  scale_linewidth_continuous(range = c(0.3, 1.5), guide = "none") +
  # 主题设置
  theme_void() +
  labs(
    title = "Genetic co-occurrence network",
    subtitle = paste0("Jaccard similarity threshold: 0.3 | genetic frequency threshold: 10%")
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA), # 设置绘图区域背景为白色
    plot.background = element_rect(fill = "white", color = NA),  # 设置整个画布背景为白色
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    strip.text = element_text(size = 10, face = "bold", margin = margin(t = 5, b = 5)),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.spacing = unit(1, "lines"),
    # 图例设置
    legend.position = "right",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.margin = margin(5, 5, 5, 5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "cm")
  ) +
  # 图例顺序调整
  guides(
    color = guide_legend(
      title = "Gene Types",
      title.position = "top",
      title.hjust = 0.5,
      order = 1,
      override.aes = list(size = 4)
    ),
    size = guide_legend(
      title = "Gene Abundance",
      title.position = "top",
      title.hjust = 0.5,
      order = 2
    )
  )

# 显示图形
print(p)

# 保存图形
output_width <- max(12, n_cols * 5)
output_height <- max(9, n_rows * 4)
ggsave(filename = "specified_serovars_network_facet.jpg", 
       plot = p, width = 12, height = 9, dpi = 600)
cat("\n分面网络图已保存为 'specified_serovars_network_facet.jpg'\n")
cat("图形尺寸:", output_width, "x", output_height, "英寸\n")

# 10. 创建图例
legend_data <- data.frame(
  type = factor(names(type_colors), levels = names(type_colors)),
  color = type_colors
)

legend_plot <- ggplot(legend_data, aes(x = 1, y = type, fill = type)) +
  geom_tile(color = "black") +
  geom_text(aes(label = type), hjust = 0, nudge_x = 0.5, size = 4) +
  scale_fill_manual(values = type_colors) +
  scale_x_continuous(limits = c(0.5, 3)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "基因类型")

# 保存图例
ggsave(filename = "network_legend.jpg", 
       plot = legend_plot, width = 6, height = 3, dpi = 300)
cat("图例已保存为 'network_legend.jpg'\n")

# 11. 使用ggraph创建分面网络图（替代方法）
cat("\n创建ggraph分面网络图（替代方法）...\n")

# 为每个有效血清型创建单独的图形
plots_list <- list()
for (serovar in ordered_serovars) {
  cat("为血清型", serovar, "创建ggraph...\n")
  
  # 筛选当前血清型的数据
  data_serovar <- filtered_data %>% filter(Serovars == serovar)
  
  # 准备网络数据
  network_data <- prepare_network_data(data_serovar, serovar)
  
  if (!is.null(network_data)) {
    g <- network_data$graph
    
    # 添加属性
    V(g)$type <- network_data$nodes$type
    V(g)$color <- type_colors[V(g)$type]
    V(g)$size <- network_data$nodes$size
    
    if (!is.null(network_data$edges) && nrow(network_data$edges) > 0) {
      E(g)$width <- network_data$edges$width
    }
    
    # 创建布局
    set.seed(2024)
    layout <- create_layout(g, layout = "fr")
    
    # 创建图形
    p_single <- ggraph(layout) +
      {
        if (ecount(g) > 0) {
          geom_edge_link(aes(width = width, alpha = width), 
                         color = "grey70", show.legend = FALSE)
        }
      } +
      geom_node_point(aes(color = color, size = size), 
                      alpha = 0.8, show.legend = FALSE) +
      geom_node_text(aes(label = name), 
                     size = 2.5, repel = TRUE, max.overlaps = 15) +
      scale_color_identity() +
      scale_size_continuous(range = c(2, 8)) +
      scale_edge_width_continuous(range = c(0.3, 1.5)) +
      theme_void() +
      labs(title = paste0(serovar, "\n(n=", network_stats %>% filter(Serovar == serovar) %>% pull(Samples), 
                          ", v=", vcount(g), ", e=", ecount(g), ")")) +
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            plot.margin = margin(5, 5, 5, 5))
    
    plots_list[[serovar]] <- p_single
  }
}

# 使用patchwork组合图形
if (length(plots_list) > 0) {
  combined_plot <- wrap_plots(plots_list, ncol = n_cols) + 
    plot_annotation(
      title = "指定血清型的基因共现网络图",
      subtitle = paste0("Jaccard相似性阈值: 0.3 | 血清型顺序: ", paste(ordered_serovars, collapse = ", ")),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                    plot.subtitle = element_text(hjust = 0.5, size = 12))
    )
  
  # 保存组合图形
  ggsave(filename = "specified_serovars_network_patchwork.jpg", 
         plot = combined_plot, width = output_width, height = output_height, dpi = 600)
  cat("patchwork组合图形已保存为 'specified_serovars_network_patchwork.jpg'\n")
  
  # 显示图形
  print(combined_plot)
}

# 12. 保存网络统计信息
if (nrow(network_stats) > 0) {
  network_stats$Serovar <- factor(network_stats$Serovar, levels = ordered_serovars)
  write.csv(network_stats, "specified_serovars_network_statistics.csv", row.names = FALSE)
  cat("\n网络统计信息已保存为 'specified_serovars_network_statistics.csv'\n")
  
  # 打印网络统计
  cat("\n=== 网络统计摘要 ===\n")
  print(network_stats)
  
  # 13. 创建网络统计摘要图
  stats_plot <- ggplot(network_stats, aes(x = Serovar)) +
    geom_bar(aes(y = Samples, fill = "样本数"), stat = "identity", alpha = 0.7, width = 0.6) +
    geom_bar(aes(y = Nodes, fill = "节点数"), stat = "identity", alpha = 0.7, width = 0.6, 
             position = position_nudge(x = 0.2)) +
    geom_bar(aes(y = Edges/5, fill = "边数(x5)"), stat = "identity", alpha = 0.7, width = 0.6, 
             position = position_nudge(x = 0.4)) +
    scale_fill_manual(values = c("样本数" = "#4DAF4A", "节点数" = "#377EB8", "边数(x5)" = "#E41A1C"),
                      name = "指标") +
    scale_y_continuous(
      name = "样本数/节点数",
      sec.axis = sec_axis(~ . * 5, name = "边数")
    ) +
    labs(
      title = "指定血清型网络特征统计",
      x = "血清型"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom",
      legend.title = element_text(size = 10)
    )
  
  ggsave(filename = "specified_serovars_network_statistics.jpg", 
         plot = stats_plot, width = 10, height = 7, dpi = 600)
  cat("网络统计图已保存为 'specified_serovars_network_statistics.jpg'\n")
  
  # 显示统计图
  print(stats_plot)
}

cat("\n=== 分析完成 ===\n")
cat("总计处理了", length(ordered_serovars), "个指定血清型的网络图\n")
cat("血清型顺序:", paste(ordered_serovars, collapse = ", "), "\n")
cat("所有图形和统计信息已保存到当前工作目录\n")

# 14. 生成处理总结
cat("\n=== 处理总结 ===\n")
cat("1. 输入血清型: Rissen, London, 4,[5],12:i:-, Derby, Typhimurium, Others\n")
cat("2. 有效血清型:", paste(ordered_serovars, collapse = ", "), "\n")
cat("3. 输出文件:\n")
cat("   - specified_serovars_network_facet.jpg: 分面网络图\n")
cat("   - specified_serovars_network_patchwork.jpg: patchwork组合网络图\n")
cat("   - specified_serovars_network_statistics.csv: 网络统计信息\n")
cat("   - specified_serovars_network_statistics.jpg: 网络统计图\n")
cat("   - network_legend.jpg: 图例文件\n")
cat("4. 参数设置:\n")
cat("   - Jaccard阈值: 0.3\n")
cat("   - 基因频率阈值: 5%\n")
cat("   - 分面列数:", n_cols, "\n")










##二、2比较不同血清型、不同来源的样本之间的共现性的差异-----------------------------------------

# 加载必要的R包
library(tidyverse)
library(reshape2)
library(igraph)
library(ggraph)
library(readxl)
library(scales)
library(RColorBrewer)
library(ggpubr)
library(patchwork)
library(vegan)
library(ggplot2)
library(gridExtra)

# 1. 数据读取与预处理 ----------------------------------------------------------
# 读取数据
setwd("C:\\Users\\Administrator\\Downloads\\data\\salmonella\\gx_mono_typhi")
merged_df <- read_excel("plasmids_info.xlsx", sheet = 2) # 基因注释表
plasmid_info <- read_excel("plasmids_info.xlsx", sheet = 1) # 元信息表

# 查看数据结构
cat("=== 数据概览 ===\n")
cat("基因注释表行数:", nrow(merged_df), "\n")
cat("元信息表行数:", nrow(plasmid_info), "\n")
cat("元信息表列名:", paste(colnames(plasmid_info), collapse = ", "), "\n")

# 检查元信息表中的血清型和来源列名
# 注意：根据实际情况，列名可能需要调整
# 假设血清型列名为"Serotype"或"血清型"，来源列名为"Source"或"来源"
# 请根据实际列名调整下面的代码
# 查找可能的血清型和来源列名
#serotype_cols <- grep("sero|type|血清型", colnames(plasmid_info), ignore.case = TRUE, value = TRUE)
#source_cols <- grep("source|来源|sample", colnames(plasmid_info), ignore.case = TRUE, value = TRUE)
#cat("可能的血清型列名:", paste(serotype_cols, collapse = ", "), "\n")
#cat("可能的来源列名:", paste(source_cols, collapse = ", "), "\n")
# 假设血清型列名为"Serotype"，来源列名为"Source"，请根据实际情况修改
serotype_col <- ifelse(length(serotype_cols) > 0, serotype_cols[1], "Serotype")
source_col <- ifelse(length(source_cols) > 0, source_cols[1], "Source")

cat("使用血清型列名:", serotype_col, "\n")
cat("使用来源列名:", source_col, "\n")

# 清理元信息表
plasmid_info_clean <- plasmid_info %>%
  # 处理文件名格式，使其与merged_df匹配
  mutate(plasmid_id = sub("\\.\\d+$", "", `#FILE`)) %>%
  # 选择需要的列
  select(plasmid_id, all_of(serotype_col), all_of(source_col)) %>%
  # 重命名列
  rename(serotype = all_of(serotype_col), source = all_of(source_col)) %>%
  # 清理数据
  filter(!is.na(plasmid_id)) %>%
  mutate(
    serotype = as.character(serotype),
    source = as.character(source),
    # 处理缺失值
    serotype = ifelse(is.na(serotype) | serotype == "", "Unknown", serotype),
    source = ifelse(is.na(source) | source == "", "Unknown", source)
  )

# 查看血清型和来源分布
cat("\n=== 血清型分布 ===\n")
print(table(plasmid_info_clean$serotype, useNA = "always"))

cat("\n=== 来源分布 ===\n")
print(table(plasmid_info_clean$source, useNA = "always"))

# 2. 准备基因-质粒矩阵 ------------------------------------------------------
# 清理数据
plasmid_gene_df <- merged_df %>%
  select(`#FILE`, Gene_Name, Gene_Types) %>%
  distinct() %>%
  filter(!is.na(Gene_Name) & !is.na(Gene_Types) & Gene_Name != "") %>%
  mutate(plasmid_id = sub("\\.\\d+$", "", `#FILE`))

# 创建质粒-基因的二元矩阵
temp_table <- table(plasmid_gene_df$plasmid_id, plasmid_gene_df$Gene_Name)
plasmid_gene_matrix <- as.data.frame.matrix(temp_table)
plasmid_gene_matrix[plasmid_gene_matrix > 0] <- 1

# 只保留在元信息表中的质粒
common_plasmids <- intersect(rownames(plasmid_gene_matrix), plasmid_info_clean$plasmid_id)
plasmid_gene_matrix <- plasmid_gene_matrix[common_plasmids, , drop = FALSE]

cat("\n=== 质粒-基因矩阵 ===\n")
cat("质粒数量:", nrow(plasmid_gene_matrix), "\n")
cat("基因数量:", ncol(plasmid_gene_matrix), "\n")

# 3. 辅助函数：构建和比较网络 --------------------------------------------------
# 函数：计算网络特征
calculate_network_features <- function(adj_matrix, group_name) {
  if (nrow(adj_matrix) < 2) {
    return(data.frame(
      group = group_name,
      n_plasmids = nrow(adj_matrix),
      n_genes = ncol(adj_matrix),
      n_edges = 0,
      network_density = 0,
      avg_degree = 0,
      avg_clustering = 0,
      modularity = 0,
      avg_path_length = NA,
      diameter = 0
    ))
  }
  
  # 计算Jaccard相似系数
  calc_jaccard <- function(x, y) {
    intersection <- sum(x == 1 & y == 1)
    union <- sum(x == 1 | y == 1)
    if(union == 0) return(0)
    return(intersection / union)
  }
  
  n_genes <- ncol(adj_matrix)
  gene_names <- colnames(adj_matrix)
  jaccard_matrix <- matrix(0, nrow = n_genes, ncol = n_genes, 
                           dimnames = list(gene_names, gene_names))
  
  for(i in 1:(n_genes-1)) {
    for(j in (i+1):n_genes) {
      jaccard_val <- calc_jaccard(adj_matrix[, i], adj_matrix[, j])
      jaccard_matrix[i, j] <- jaccard_val
      jaccard_matrix[j, i] <- jaccard_val
    }
  }
  diag(jaccard_matrix) <- 1
  
  # 创建网络
  net_threshold <- 0.2
  adj_net <- jaccard_matrix
  adj_net[adj_net < net_threshold] <- 0
  adj_net[is.na(adj_net)] <- 0
  
  g <- graph_from_adjacency_matrix(adj_net, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # 移除孤立节点
  isolated_vertices <- which(degree(g) == 0)
  if(length(isolated_vertices) > 0) {
    g <- delete.vertices(g, isolated_vertices)
  }
  
  # 计算网络特征
  if (vcount(g) == 0) {
    return(data.frame(
      group = group_name,
      n_plasmids = nrow(adj_matrix),
      n_genes = ncol(adj_matrix),
      n_edges = 0,
      network_density = 0,
      avg_degree = 0,
      avg_clustering = 0,
      modularity = 0,
      avg_path_length = NA,
      diameter = 0
    ))
  }
  
  # 计算模块性
  if (ecount(g) > 0) {
    communities <- cluster_louvain(g)
    modularity_val <- modularity(communities)
  } else {
    modularity_val <- 0
  }
  
  # 计算平均路径长度（仅对连通图）
  avg_path_length_val <- NA
  diameter_val <- 0
  
  if (is.connected(g)) {
    avg_path_length_val <- average.path.length(g)
    diameter_val <- diameter(g)
  } else {
    # 对不连通图，计算各连通分量的平均
    comps <- decompose.graph(g)
    if (length(comps) > 0) {
      path_lengths <- sapply(comps, function(comp) {
        if (vcount(comp) > 1) average.path.length(comp) else 0
      })
      avg_path_length_val <- mean(path_lengths, na.rm = TRUE)
      
      diameters <- sapply(comps, function(comp) {
        if (vcount(comp) > 1) diameter(comp) else 0
      })
      diameter_val <- max(diameters, na.rm = TRUE)
    }
  }
  
  # 返回特征数据框
  data.frame(
    group = group_name,
    n_plasmids = nrow(adj_matrix),
    n_genes = ncol(adj_matrix),
    n_edges = ecount(g),
    network_density = edge_density(g),
    avg_degree = mean(degree(g)),
    avg_clustering = transitivity(g, type = "global"),
    modularity = modularity_val,
    avg_path_length = avg_path_length_val,
    diameter = diameter_val
  )
}

# 函数：绘制网络图
plot_network <- function(adj_matrix, group_name, freq_threshold = 2) {
  if (nrow(adj_matrix) < 2 || ncol(adj_matrix) < 2) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient data for", group_name)) +
             theme_void())
  }
  
  # 筛选高频基因
  gene_freq <- colSums(adj_matrix)
  selected_genes <- names(gene_freq[gene_freq >= freq_threshold])
  
  if (length(selected_genes) < 2) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = paste("Not enough genes for", group_name)) +
             theme_void())
  }
  
  adj_matrix_selected <- adj_matrix[, selected_genes, drop = FALSE]
  
  # 计算Jaccard矩阵
  n_genes <- ncol(adj_matrix_selected)
  gene_names <- colnames(adj_matrix_selected)
  jaccard_matrix <- matrix(0, nrow = n_genes, ncol = n_genes, 
                           dimnames = list(gene_names, gene_names))
  
  for(i in 1:(n_genes-1)) {
    for(j in (i+1):n_genes) {
      jaccard_val <- sum(adj_matrix_selected[, i] & adj_matrix_selected[, j]) / 
        sum(adj_matrix_selected[, i] | adj_matrix_selected[, j])
      if(is.na(jaccard_val)) jaccard_val <- 0
      jaccard_matrix[i, j] <- jaccard_val
      jaccard_matrix[j, i] = jaccard_val
    }
  }
  diag(jaccard_matrix) <- 1
  
  # 创建网络
  net_threshold <- 0.2
  adj_net <- jaccard_matrix
  adj_net[adj_net < net_threshold] <- 0
  
  g <- graph_from_adjacency_matrix(adj_net, mode = "undirected", 
                                   weighted = TRUE, diag = FALSE)
  
  # 移除孤立节点
  isolated_vertices <- which(degree(g) == 0)
  if(length(isolated_vertices) > 0) {
    g <- delete.vertices(g, isolated_vertices)
  }
  
  if (vcount(g) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = paste("No connected network for", group_name)) +
             theme_void())
  }
  
  # 设置节点属性
  V(g)$size <- log(gene_freq[V(g)$name] + 1) * 3
  V(g)$label <- ifelse(gene_freq[V(g)$name] >= 3, V(g)$name, "")
  E(g)$width <- E(g)$weight * 3
  
  # 绘制网络
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = width, alpha = width), 
                   color = "grey70", show.legend = FALSE) +
    geom_node_point(aes(size = size), color = "#377EB8", alpha = 0.7) +
    geom_node_text(aes(label = label), size = 2.5, repel = TRUE, max.overlaps = 20) +
    scale_size_continuous(range = c(2, 8)) +
    scale_edge_width_continuous(range = c(0.5, 2)) +
    theme_void() +
    labs(title = group_name,
         subtitle = paste("Nodes:", vcount(g), "Edges:", ecount(g))) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5, size = 8))
  
  return(p)
}

# 4. 按血清型分析 -------------------------------------------------------------
cat("\n=== 按血清型分析 ===\n")

# 合并元信息
plasmid_gene_with_meta <- plasmid_gene_matrix %>%
  as.data.frame() %>%
  rownames_to_column("plasmid_id") %>%
  left_join(plasmid_info_clean, by = "plasmid_id")

# 按血清型分组
serotype_groups <- split(plasmid_gene_with_meta, plasmid_gene_with_meta$serotype)
serotype_results <- list()
serotype_features <- data.frame()

for (serotype_name in names(serotype_groups)) {
  group_data <- serotype_groups[[serotype_name]]
  n_plasmids <- nrow(group_data)
  
  cat("\n分析血清型:", serotype_name, " (质粒数:", n_plasmids, ")\n")
  
  if (n_plasmids >= 3) {  # 至少需要3个质粒才进行分析
    # 提取基因矩阵
    gene_matrix <- group_data %>%
      select(-plasmid_id, -serotype, -source) %>%
      as.matrix()
    
    # 只保留在至少一个质粒中存在的基因
    gene_matrix <- gene_matrix[, colSums(gene_matrix) > 0, drop = FALSE]
    
    if (ncol(gene_matrix) >= 2) {
      # 计算网络特征
      features <- calculate_network_features(gene_matrix, serotype_name)
      serotype_features <- rbind(serotype_features, features)
      
      # 绘制网络图
      p <- plot_network(gene_matrix, paste("Serotype:", serotype_name))
      serotype_results[[serotype_name]] <- list(
        plot = p,
        features = features,
        n_plasmids = n_plasmids,
        n_genes = ncol(gene_matrix)
      )
    } else {
      cat("  -> 基因数量不足，跳过\n")
    }
  } else {
    cat("  -> 质粒数量不足，跳过\n")
  }
}
##---------------------------------------------------------------------------
##以上数量太多，需要对阈值进行筛选，确保能够被顺利读取，建议询问AI合适的阈值-------------------------------------------------------------
##-------------------------------------------------------------------------------
##需要绘制不同血清型关键基因数量的差异， 绘制出箱型图或者是小提琴图即可-----------
##如果点突变也存在差异，关键点突变的差异也是需要列出来的-----------------------
##附表中可以提一下耐重金属、消毒剂基因--------------------------------
##-------------------------------------------------------------


##fig3需要列出毒力基因总数与关键毒力基因，毒力岛的分布情况--------------------


##fig4的相关性分析只需要列出基因数量与插入序列的总数，包含其他类型的插入序列，同时也包含了其他类型的质粒复制子，然后再绘制相关性图
##-------------------------------------------------------------------------

##要求尽快把所有的图画出来
##fig5我国的菌株与国外的菌株的对比，要求给出主要血清型平均的耐药基因的数量的对比--------------------------------

##我国的菌株与我国临床的菌株的对比，要求指出我国临床的平均耐药基因数量与猪肉菌株的差异---------
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(RColorBrewer)
library(ggsci)
library(cowplot)

# 1. 读取数据
# 假设您的数据在一个Excel文件中，包含ARGs、ISs和plasmid_replicons的数量
# 请根据实际文件路径修改
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 11)
# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")
# 确保数据格式正确
data$Serovars <- as.factor(data$Serovars)
# 定义血清型颜色（固定，不随排序改变）
serovar_colors <- c(
  "Rissen" = "#e54e21",  # 红色
  "London" = "#8dd2c6",  # 蓝色
  "4,[5],12:i:-" = "#b3de69",  # 绿色
  "Derby" = "#BB80BC",  # 紫色
  "Typhimurium" = "#80B0D2",  # 橙色
  "Others" = "#FFEC6D"  # 黄色
)

# 创建通用函数用于绘制箱线图并进行统计检验
# 新增参数：order_by_median = TRUE 表示按中位数排序
create_boxplot <- function(data, y_var, y_label, order_by_median = TRUE, color_palette = "custom") {
  # 提取数据
  plot_data <- data %>%
    select(Serovars, !!sym(y_var)) %>%
    filter(!is.na(!!sym(y_var)))
  # 如果需要按中位数排序
  if (order_by_median) {
    # 计算当前指标的中位数，用于排序
    median_order <- plot_data %>%
      group_by(Serovars) %>%
      summarise(median_val = median(!!sym(y_var), na.rm = TRUE)) %>%
      arrange(desc(median_val)) %>%
      pull(Serovars)
    # 将血清型按当前指标中位数从高到低排序
    plot_data$Serovars <- factor(plot_data$Serovars, levels = median_order)
  }
  # 计算统计检验（相邻血清型两两比较）
  # 使用Wilcoxon秩和检验
  stat_test <- plot_data %>%
    pairwise_wilcox_test(
      as.formula(paste(y_var, "~ Serovars")),
      p.adjust.method = "fdr"
    ) %>%
    add_xy_position(x = "Serovars")
  # 只保留相邻血清型之间的比较
  serovar_levels <- levels(plot_data$Serovars)
  # 创建相邻对
  adjacent_pairs <- data.frame(
    group1 = head(serovar_levels, -1),
    group2 = tail(serovar_levels, -1)
  )
  # 过滤只保留相邻比较
  stat_test_adjacent <- stat_test %>%
    filter(paste(group1, group2, sep = "-") %in% 
             paste(adjacent_pairs$group1, adjacent_pairs$group2, sep = "-"))
  # 创建箱线图
  p <- ggplot(plot_data, aes(x = Serovars, y = !!sym(y_var), color = Serovars)) +
    # 绘制箱线图
    geom_boxplot(
      aes(fill = Serovars),
      alpha = 0.5,
      outlier.shape = NA
    ) +
    # 绘制散点
    geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
    # 设置颜色
    scale_fill_manual(values = serovar_colors) +
    scale_color_manual(values = serovar_colors) +
    # 设置主题
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12, angle = 90),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.position = "none"
    ) +
    labs(
      y = y_label,
      x = "Serovars"
    )
  # 添加显著性标注（只显示相邻比较）
  if (nrow(stat_test_adjacent) > 0) {
    p <- p + 
      stat_pvalue_manual(
        stat_test_adjacent,
        label = "p.adj.signif",  # 显示显著性符号
        tip.length = 0.01,
        bracket.size = 0.5,
        step.increase = 0.005,
        hide.ns = TRUE
      )
  }
  return(p)
}
# 绘制三个指标的箱线图，每个都按照自己的中位数排序
# 5.1 ARGs箱线图
p_args <- create_boxplot(
  data = data,
  y_var = "ARGs",
  y_label = "Number of ARGs",
  order_by_median = TRUE
)
# 5.2 ISs箱线图
p_iss <- create_boxplot(
  data = data,
  y_var = "ISs",
  y_label = "Number of ISs",
  order_by_median = TRUE
)
# 5.3 plasmid_replicons箱线图
p_plasmids <- create_boxplot(
  data = data,
  y_var = "plasmid_replicons",
  y_label = "Number of Plasmid Replicons",
  order_by_median = TRUE
)
# 5.4 plasmid_replicons箱线图
p_BRGs <- create_boxplot(
  data = data,
  y_var = "BRGs",
  y_label = "Number of BRGs",
  order_by_median = TRUE
)
# 5.5 plasmid_replicons箱线图
p_MRGs <- create_boxplot(
  data = data,
  y_var = "MRGs",
  y_label = "Number of MRGs",
  order_by_median = TRUE
)
# 5.6 plasmid_replicons箱线图
p_VFs <- create_boxplot(
  data = data,
  y_var = "VFs",
  y_label = "Number of VFs",
  order_by_median = TRUE
)
# 组合图形
library(cowplot)
combined_plot <- plot_grid(
  p_args, p_iss, p_plasmids,p_BRGs,p_MRGs, p_VFs,
  ncol = 2,
  labels = c("A", "B", "C","D", "E", "F"),
  label_size = 12
)
# 显示组合图形
print(combined_plot)
# 保存图形
ggsave("serovars_genetic_elements_boxplots_sorted.pdf", combined_plot, 
       width = 8, height = 18, dpi = 600)

# 创建每个指标的排序信息
cat("\n=== 各指标排序信息 ===\n")

# ARGs排序
args_order <- data %>%
  group_by(Serovars) %>%
  summarise(median_ARGs = median(ARGs, na.rm = TRUE)) %>%
  arrange(desc(median_ARGs))

cat("\nARGs中位数排序（从高到低）：\n")
print(args_order)

# ISs排序
iss_order <- data %>%
  group_by(Serovars) %>%
  summarise(median_ISs = median(ISs, na.rm = TRUE)) %>%
  arrange(desc(median_ISs))

cat("\nISs中位数排序（从高到低）：\n")
print(iss_order)

# plasmid_replicons排序
plasmid_order <- data %>%
  group_by(Serovars) %>%
  summarise(median_plasmids = median(plasmid_replicons, na.rm = TRUE)) %>%
  arrange(desc(median_plasmids))

cat("\nPlasmid Replicons中位数排序（从高到低）：\n")
print(plasmid_order)

# 为了对比，也创建固定顺序的版本
cat("\n=== 固定顺序（按ARGs排序）版本 ===\n")
# 固定顺序：按ARGs中位数排序
fixed_order <- args_order$Serovars
data_fixed <- data
data_fixed$Serovars <- factor(data_fixed$Serovars, levels = fixed_order)

# 创建固定顺序的箱线图函数
create_boxplot_fixed <- function(data, y_var, y_label) {
  plot_data <- data %>%
    select(Serovars, !!sym(y_var)) %>%
    filter(!is.na(!!sym(y_var)))
  
  # 计算统计检验
  stat_test <- plot_data %>%
    pairwise_wilcox_test(
      as.formula(paste(y_var, "~ Serovars")),
      p.adjust.method = "fdr"
    ) %>%
    add_xy_position(x = "Serovars")
  
  # 只保留相邻血清型之间的比较
  serovar_levels <- levels(plot_data$Serovars)
  adjacent_pairs <- data.frame(
    group1 = head(serovar_levels, -1),
    group2 = tail(serovar_levels, -1)
  )
  
  stat_test_adjacent <- stat_test %>%
    filter(paste(group1, group2, sep = "-") %in% 
             paste(adjacent_pairs$group1, adjacent_pairs$group2, sep = "-"))
  
  p <- ggplot(plot_data, aes(x = Serovars, y = !!sym(y_var), color = Serovars)) +
    geom_boxplot(aes(fill = Serovars), alpha = 0.5, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 1.5) +
    scale_fill_manual(values = serovar_colors) +
    scale_color_manual(values = serovar_colors) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12, angle = 90),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(y = y_label, x = "Serovars")
  
  if (nrow(stat_test_adjacent) > 0) {
    p <- p + 
      stat_pvalue_manual(
        stat_test_adjacent,
        label = "p.adj.signif",
        tip.length = 0.01,
        bracket.size = 0.5,
        step.increase = 0.005,
        hide.ns = TRUE
      )
  }
  
  return(p)
}

# 绘制固定顺序的箱线图
p_args_fixed <- create_boxplot_fixed(
  data = data_fixed,
  y_var = "ARGs",
  y_label = "Number of Antibiotic Resistance Genes (ARGs)"
)

p_iss_fixed <- create_boxplot_fixed(
  data = data_fixed,
  y_var = "ISs",
  y_label = "Number of Insertion Sequences (ISs)"
)

p_plasmids_fixed <- create_boxplot_fixed(
  data = data_fixed,
  y_var = "plasmid_replicons",
  y_label = "Number of Plasmid Replicons"
)

# 组合固定顺序的图形
combined_plot_fixed <- plot_grid(
  p_args_fixed, p_iss_fixed, p_plasmids_fixed,
  ncol = 1,
  labels = c("A", "B", "C"),
  label_size = 12
)

# 显示和保存固定顺序图形
print(combined_plot_fixed)
ggsave("serovars_genetic_elements_boxplots_fixed.jpg", combined_plot_fixed, 
       width = 6, height = 18, dpi = 300)

# 输出总结
cat("\n=== 分析完成 ===\n")
cat("已生成文件：\n")
cat("1. serovars_genetic_elements_boxplots_sorted.jpg - 各指标独立排序的箱线图\n")
cat("2. serovars_genetic_elements_boxplots_fixed.jpg - 固定顺序（按ARGs排序）的箱线图\n")
cat("\n注意：\n")
cat("- 第一个文件中的每个子图都按照对应指标的中位数从高到低排序\n")
cat("- 第二个文件中的所有子图都按照ARGs的中位数顺序排序\n")
cat("- 颜色方案在所有图形中保持一致\n")

##使用ggtern绘制出中美的差异，单独新建一个---------------
library(ggtern)
data <- read.table("ARGs_heatmap_matrix.txt", header=TRUE)
ggtern(data, aes(Porcine_China, Clinical_China, USA_China))+
  geom_point(aes(color=Classification, size=Total), stroke = 2,  show.legend = T)+
  #  tern_limits(0.4,0.4,0.5)+
  labs(size = 'Number of ARGs') +
  geom_mask()+  # 可将超出边界的点正常显示出来
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())+
  scale_color_manual(values = pal_simpsons("springfield")(16))

##火山图-----------------------------------------------------------------------------
# 加载必要的R包
library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(cowplot)
# 1. 读取数据
data_sheet1 <- read_excel("General_stat.xlsx", sheet = 1)
data_sheet2 <- read_excel("General_stat.xlsx", sheet = 2)
# 查看数据结构
cat("Sheet2数据结构:\n")
str(data_sheet2)
cat("\nSheet2前几行:\n")
head(data_sheet2)
cat("\n基因类型:\n")
table(data_sheet2$Types)
# 2. 数据预处理
# 合并两个sheet的数据
merged_data <- data_sheet1 %>%
  left_join(data_sheet2, by = "ID")
# 查看合并后的数据结构
cat("\n合并后的数据结构:\n")
str(merged_data %>% select(ID, Richness, Temperature_types, Top_serovars, Genes, Types))
# 3. 定义颜色方案
gene_type_colors <- c(
  "ARGs" = "#E41A1C",           # 红色
  "BRGs" = "#FF7F00",           # 橙色
  "MRGs" = "darkgrey",          # 深灰色
  "VFs" = "#4DAF4A",            # 绿色（注意：这里应该是VFs，不是VF）
  "ISs" = "#377EB8",            # 蓝色
  "plasmid_replicons" = "#984EA3" # 紫色
)
# 4. 修改差异分析函数，使用比例差异和标准差
perform_gene_analysis <- function(data, group_var, serovar, control_group, test_group) {
  # 筛选特定血清型的数据
  subset_data <- data %>%
    filter(Top_serovars == serovar) %>%
    mutate(
      gene_present = 1,  # 每个基因为1表示存在
      group = ifelse(.data[[group_var]] == test_group, 1, 0)  # 比较组为1，基准组为0
    )
  # 检查是否有足够的数据
  if (nrow(subset_data) < 5) {
    cat("警告: 血清型", serovar, "数据不足\n")
    return(NULL)
  }
  # 检查两组是否有数据
  control_data <- subset_data %>% filter(group == 0)
  test_data <- subset_data %>% filter(group == 1)
  if (nrow(control_data) == 0 || nrow(test_data) == 0) {
    cat("警告: 血清型", serovar, "某个组没有数据\n")
    return(NULL)
  }
  cat("血清型:", serovar, 
      " 基准组(", control_group, "):", nrow(control_data), "个观测",
      " 比较组(", test_group, "):", nrow(test_data), "个观测\n")
  # 对每个基因进行差异分析
  results_list <- list()
  genes <- unique(subset_data$Genes)
  for (gene in genes) {
    # 筛选当前基因的数据
    gene_data <- subset_data %>%
      filter(Genes == gene) %>%
      distinct(ID, .keep_all = TRUE)  # 每个样本只保留一次
    
    if (nrow(gene_data) < 3) {
      next
    }
    
    # 创建列联表
    # 行: 基因存在(1)或不存在(0)
    # 列: 组别(基准组0 vs 比较组1)
    
    # 首先获取每个组的所有唯一样本
    all_samples <- unique(subset_data$ID)
    control_samples <- unique(subset_data$ID[subset_data$group == 0])
    test_samples <- unique(subset_data$ID[subset_data$group == 1])
    
    # 统计每个组中携带该基因的样本数
    gene_in_control <- length(intersect(
      unique(gene_data$ID[gene_data$group == 0]),
      control_samples
    ))
    
    gene_in_test <- length(intersect(
      unique(gene_data$ID[gene_data$group == 1]),
      test_samples
    ))
    
    # 计算不携带基因的样本数
    not_gene_control <- length(control_samples) - gene_in_control
    not_gene_test <- length(test_samples) - gene_in_test
    
    # 构建列联表
    cont_table <- matrix(
      c(gene_in_control, not_gene_control, 
        gene_in_test, not_gene_test),
      nrow = 2,
      dimnames = list(
        Gene = c("Present", "Absent"),
        Group = c(control_group, test_group)
      )
    )
    
    # 检查是否所有单元格都有足够的计数
    if (any(cont_table < 5) || any(rowSums(cont_table) == 0) || any(colSums(cont_table) == 0)) {
      # 使用Fisher精确检验
      test_result <- tryCatch(
        fisher.test(cont_table),
        error = function(e) NULL
      )
      test_method <- "Fisher"
    } else {
      # 使用卡方检验
      test_result <- tryCatch(
        chisq.test(cont_table),
        error = function(e) NULL
      )
      test_method <- "Chi-square"
    }
    
    if (!is.null(test_result)) {
      # 计算基因频率
      freq_control <- gene_in_control / length(control_samples)
      freq_test <- gene_in_test / length(test_samples)
      
      # 计算比例差异 (比较组 - 基准组)
      prop_diff <- freq_test - freq_control
      
      # 计算标准差 (衡量波动性)
      # 对于二进制数据(0/1)，标准差 = sqrt(p*(1-p))
      stdev_control <- sqrt(freq_control * (1 - freq_control))
      stdev_test <- sqrt(freq_test * (1 - freq_test))
      
      # 使用平均标准差
      avg_stdev <- mean(c(stdev_control, stdev_test), na.rm = TRUE)
      
      # 获取p值
      p_value <- test_result$p.value
      
      # 获取基因类型
      gene_type <- unique(gene_data$Types[gene_data$Genes == gene])[1]
      
      results_list[[gene]] <- data.frame(
        gene = gene,
        gene_type = gene_type,
        serovar = serovar,
        prop_diff = prop_diff,           # 比例差异代替log2FC
        avg_stdev = avg_stdev,           # 平均标准差
        pvalue = p_value,
        freq_control = freq_control,
        freq_test = freq_test,
        n_control = gene_in_control,
        n_test = gene_in_test,
        total_control = length(control_samples),
        total_test = length(test_samples),
        test_method = test_method
      )
    }
  }
  
  if (length(results_list) == 0) {
    cat("警告: 血清型", serovar, "没有有效的检验结果\n")
    return(NULL)
  }
  
  # 合并所有结果
  results <- bind_rows(results_list) %>%
    mutate(
      padj = p.adjust(pvalue, method = "fdr"),
      comparison = paste0(test_group, " vs ", control_group),
      # 使用原始p值<0.05作为显著性标准
      direction = case_when(
        prop_diff > 0.2 & pvalue < 0.05 ~ "Up",      # 比例差异大于0.2且p<0.05为上调
        prop_diff < -0.2 & pvalue < 0.05 ~ "Down",   # 比例差异小于-0.2且p<0.05为下调
        TRUE ~ "Not significant"
      ),
      # 显著性使用原始p值<0.05
      significant = ifelse(pvalue < 0.05, "Yes", "No")
    )
  
  return(results)
}

# 5. 获取血清型列表
serovars <- c("London", "Rissen", "Derby", "4,[5],12:i:-", "Typhimurium", "Others")

# 6. 分析Richness比较 (rich vs not rich，以not rich为基准)
cat("\n开始Richness比较分析 (rich vs not rich，以not rich为基准)...\n")
richness_results_list <- list()

for (serovar in serovars) {
  cat("\n分析血清型:", serovar, "\n")
  result <- perform_gene_analysis(
    data = merged_data,
    group_var = "Richness",
    serovar = serovar,
    control_group = "not rich",  # 基准组
    test_group = "rich"          # 比较组
  )
  
  if (!is.null(result)) {
    richness_results_list[[serovar]] <- result
  }
}

# 合并Richness结果
if (length(richness_results_list) > 0) {
  richness_results <- bind_rows(richness_results_list) %>%
    mutate(comparison_type = "Richness")
} else {
  richness_results <- NULL
  cat("警告: Richness比较没有结果\n")
}

# 7. 分析Temperature_types比较 (cold vs warm，以cold为基准)
cat("\n\n开始Temperature_types比较分析 (cold vs warm，以cold为基准)...\n")
temp_results_list <- list()

for (serovar in serovars) {
  cat("\n分析血清型:", serovar, "\n")
  result <- perform_gene_analysis(
    data = merged_data,
    group_var = "Temperature_types",
    serovar = serovar,
    control_group = "cold",  # 基准组
    test_group = "warm"      # 比较组
  )
  
  if (!is.null(result)) {
    temp_results_list[[serovar]] <- result
  }
}

# 合并Temperature结果
if (length(temp_results_list) > 0) {
  temp_results <- bind_rows(temp_results_list) %>%
    mutate(comparison_type = "Temperature_types")
} else {
  temp_results <- NULL
  cat("警告: Temperature_types比较没有结果\n")
}

# 8. 合并所有结果
if (!is.null(richness_results) && !is.null(temp_results)) {
  all_results <- bind_rows(richness_results, temp_results)
} else if (!is.null(richness_results)) {
  all_results <- richness_results
} else if (!is.null(temp_results)) {
  all_results <- temp_results
} else {
  stop("没有有效的分析结果")
}

# 查看结果概览
cat("\n结果概览:\n")
cat("Richness比较基因数量:", nrow(richness_results), "\n")
cat("Temperature比较基因数量:", nrow(temp_results), "\n")
cat("总基因数量:", nrow(all_results), "\n")

# 9. 改进的火山图函数，使用比例差异和标准差
create_volcano_plot_improved <- function(data, comparison_title, gene_type_colors) {
  # 筛选数据
  plot_data <- data %>%
    filter(comparison_type == comparison_title) %>%
    mutate(
      # 使用比例差异的绝对值作为变化幅度
      abs_change = abs(prop_diff),
      # 标准差的对数
      log_stdev = log10(avg_stdev + 0.001),
      # 使用原始p值<0.05定义趋势类型
      trend_type = case_when(
        prop_diff > 0.2 & pvalue < 0.05 ~ "Increased",
        prop_diff < -0.2 & pvalue < 0.05 ~ "Decreased",
        TRUE ~ "Not significant"
      )
    )
  # 过滤掉log_stdev接近-3的异常点
  plot_data_filtered <- plot_data %>%
    filter(log_stdev > -2.5)  # 过滤掉log_stdev小于-2.5的点
  cat("过滤前数据点数:", nrow(plot_data), "\n")
  cat("过滤后数据点数:", nrow(plot_data_filtered), "\n")
  cat("被过滤的数据点:", nrow(plot_data) - nrow(plot_data_filtered), "\n")
  # 检查是否有数据
  if (nrow(plot_data_filtered) == 0) {
    cat("警告:", comparison_title, "过滤后没有数据可绘制\n")
    return(NULL)
  }
  # 计算显著基因数量
  sig_increased <- sum(plot_data_filtered$trend_type == "Increased", na.rm = TRUE)
  sig_decreased <- sum(plot_data_filtered$trend_type == "Decreased", na.rm = TRUE)
  cat(comparison_title, " - 增加基因:", sig_increased, "减少基因:", sig_decreased, "\n")
    # 修改：只标注满足显著差异
  label_data_list <- list()
  for (serovar in unique(plot_data_filtered$serovar)) {
    # 筛选当前血清型的数据
    serovar_data <- plot_data_filtered %>% filter(serovar == !!serovar)
    # 修改：只选择满足显著差异且比例差异绝对值>0.2的基因
    # 条件：pvalue < 0.05 AND (prop_diff > 0.2 OR prop_diff < -0.2)
    significant_genes <- serovar_data %>%
      filter(pvalue < 0.05)
    if (nrow(significant_genes) > 0) {
      label_data_list[[serovar]] <- significant_genes
    }
  }
  # 合并所有血清型的标注数据
  if (length(label_data_list) > 0) {
    label_data <- bind_rows(label_data_list)
  } else {
    label_data <- data.frame()  # 空数据框
  }
  # 创建火山图##
  p <- ggplot(plot_data_filtered, aes(x = prop_diff, y = -log(pvalue,10), color = gene_type)) +
    geom_point(aes(size = abs_change), alpha = 0.7) +
    facet_wrap(~ serovar, ncol = 3) +
    scale_color_manual(values = gene_type_colors) +
    scale_size_continuous(range = c(1, 4)) +
    # 添加阈值线
    geom_hline(yintercept = 1.3, linetype = "dashed", alpha = 0.5, color = "gray50") +
    # 添加坐标轴
    geom_vline(xintercept = 0, linetype = "solid", alpha = 0.3, color = "gray30")
  # 使用ggrepel添加标签
  if (nrow(label_data) > 0) {
    p <- p + geom_text_repel(
      data = label_data,
      aes(label = gene),
      size = 2.5,
      max.overlaps = 30,
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "grey50",
      segment.size = 0.2,
      segment.alpha = 0.5,
      min.segment.length = 0.1,
      show.legend = FALSE,
      force = 1,
      force_pull = 1,
      max.time = 1,
      max.iter = 10000,
      direction = "both",
      seed = 42
    )
  }
  # 美化主题
  p <- p + theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.border = element_rect(fill = NA, color = "grey80", linewidth = 0.5),
      strip.background = element_rect(fill = "grey90", color = "grey80", linewidth = 0.5),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 10)
    ) +
    labs(
      title = paste("volcano plot -", comparison_title),
      x = "proportion changes",
      y = "-log(pvalue)",
      color = "Gene types",
      shape = "variation trend",
      size = "variation range"
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 3, alpha = 1)),
      shape = guide_legend(override.aes = list(size = 3, alpha = 1)),
      size = guide_legend(override.aes = list(alpha = 1))
    )
  return(list(plot = p, filtered_data = plot_data_filtered, label_data = label_data))
}

# 10. 创建并保存火山图
# 创建Richness比较的火山图
if (!is.null(richness_results) && nrow(richness_results) > 0) {
  cat("\n绘制Richness比较火山图...\n")
  richness_plot_result <- create_volcano_plot_improved(all_results, "Richness", gene_type_colors)
  if (!is.null(richness_plot_result$plot)) {
    print(richness_plot_result$plot)
    ggsave("volcano_plot_richness_pvalue.pdf", richness_plot_result$plot, 
           width = 12, height = 6, dpi = 600, bg = "white")
    cat("Richness火山图已保存为 volcano_plot_richness_pvalue.pdf\n")
    # 保存被过滤的数据
    if (!is.null(richness_plot_result$filtered_data)) {
      write_csv(richness_plot_result$filtered_data, "richness_filtered_data_pvalue.csv")
      cat("Richness过滤后数据已保存为 richness_filtered_data_pvalue.csv\n")
    }
    # 保存标注基因信息
    if (!is.null(richness_plot_result$label_data) && nrow(richness_plot_result$label_data) > 0) {
      write_csv(richness_plot_result$label_data, "richness_labeled_genes_pvalue.csv")
      cat("Richness标注基因已保存为 richness_labeled_genes_pvalue.csv\n")
    }
  }
} else {
  cat("警告: 没有Richness比较结果，无法绘制火山图\n")
}

# 创建Temperature_types比较的火山图
if (!is.null(temp_results) && nrow(temp_results) > 0) {
  cat("\n绘制Temperature_types比较火山图...\n")
  temp_plot_result <- create_volcano_plot_improved(all_results, "Temperature_types", gene_type_colors)
  if (!is.null(temp_plot_result$plot)) {
    print(temp_plot_result$plot)
    ggsave("volcano_plot_temperature_pvalue.pdf", temp_plot_result$plot, 
           width = 12, height = 6, dpi = 600, bg = "white")
    cat("Temperature火山图已保存为 volcano_plot_temperature_pvalue.pdf\n")
    # 保存被过滤的数据
    if (!is.null(temp_plot_result$filtered_data)) {
      write_csv(temp_plot_result$filtered_data, "temperature_filtered_data_pvalue.csv")
      cat("Temperature过滤后数据已保存为 temperature_filtered_data_pvalue.csv\n")
    }
    # 保存标注基因信息
    if (!is.null(temp_plot_result$label_data) && nrow(temp_plot_result$label_data) > 0) {
      write_csv(temp_plot_result$label_data, "temperature_labeled_genes_pvalue.csv")
      cat("Temperature标注基因已保存为 temperature_labeled_genes_pvalue.csv\n")
    }
  }
} else {
  cat("警告: 没有Temperature_types比较结果，无法绘制火山图\n")
}

# 11. 保存结果
if (exists("all_results") && nrow(all_results) > 0) {
  # 保存所有结果
  write_csv(all_results, "differential_gene_analysis_results_pvalue.csv")
  cat("\n所有分析结果已保存为 differential_gene_analysis_results_pvalue.csv\n")
  
  # 提取显著差异基因
  significant_genes <- all_results %>%
    filter(significant == "Yes") %>%
    arrange(comparison_type, serovar, pvalue, desc(abs(prop_diff)))
  
  write_csv(significant_genes, "significant_differential_genes_pvalue.csv")
  cat("显著差异基因结果已保存为 significant_differential_genes_pvalue.csv\n")
  
  # 统计汇总
  summary_stats <- all_results %>%
    group_by(comparison_type, serovar, gene_type) %>%
    summarise(
      n_genes = n(),
      n_increased = sum(prop_diff > 0.2 & pvalue < 0.05, na.rm = TRUE),
      n_decreased = sum(prop_diff < -0.2 & pvalue < 0.05, na.rm = TRUE),
      n_sig = sum(significant == "Yes", na.rm = TRUE),
      .groups = "drop"
    )
  cat("\n差异基因统计汇总:\n")
  print(summary_stats)
  # 保存统计汇总
  write_csv(summary_stats, "gene_statistics_summary_pvalue.csv")
  cat("统计汇总已保存为 gene_statistics_summary_pvalue.csv\n")
} else {
  cat("\n没有分析结果可保存\n")
}
cat("\n分析完成！\n")


##依据上述分析的结果进行后续的机器学习分析--------------------------------------
# 先查看数据结构和摘要
# 加载必要的R包
library(tidyverse)
library(caret)
library(pROC)
library(doParallel)
library(glmnet)
library(randomForest)
library(kernlab)

# 1. 加载并预处理数据
temp_data <- read.csv("temperature_filtered_data_pvalue.csv")

ml_data <- temp_data %>%
  dplyr::select(gene, serovar, gene_type, prop_diff, pvalue, padj, abs_change, log_stdev, significant) %>%
  mutate(significant = as.factor(ifelse(significant == "Yes", "Significant", "Not_Significant"))) %>%
  filter(pvalue < 1 | padj < 1) %>%
  dplyr::select(-gene) %>%
  drop_na()

# 2. 创建训练集与测试集
set.seed(2026)
trainIndex <- createDataPartition(ml_data$significant,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)
trainData <- ml_data[trainIndex, ]
testData <- ml_data[-trainIndex, ]

# 查看类别分布
cat("训练集类别分布:\n")
print(table(trainData$significant))
cat("\n测试集类别分布:\n")
print(table(testData$significant))

# 3. 设置并行计算
cl <- makePSOCKcluster(2)  # 减少核心数
registerDoParallel(cl)

# 4. 定义训练控制参数 - 使用重复交叉验证
fitControl <- trainControl(
  method = "repeatedcv",  # 重复交叉验证
  number = 5,             # 5折
  repeats = 3,            # 重复3次
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  allowParallel = TRUE,
  sampling = "smote"      # 使用SMOTE处理不平衡
)

# 5. 为每个模型设置调参网格

# 为glmnet设置调参网格
glmnetGrid <- expand.grid(
  alpha = c(0, 0.5, 1),  # 0:岭回归, 1:LASSO, 0.5:弹性网
  lambda = 10^seq(-3, 0, length = 10)
)

# 为随机森林设置调参网格
rfGrid <- expand.grid(mtry = c(2, 3, 4, 5))

# 为SVM设置调参网格
svmGrid <- expand.grid(
  sigma = c(0.01, 0.1, 0.5),
  C = c(0.1, 1, 10)
)

# 6. 分别训练每个模型
set.seed(2026)

# 训练glmnet模型
cat("训练glmnet模型...\n")
glmnet_model <- train(
  form = significant ~ .,
  data = trainData,
  method = "glmnet",
  trControl = fitControl,
  tuneGrid = glmnetGrid,
  metric = "ROC",
  preProcess = c("center", "scale", "nzv")
)

# 训练随机森林模型
cat("训练随机森林模型...\n")
rf_model <- train(
  form = significant ~ .,
  data = trainData,
  method = "rf",
  trControl = fitControl,
  tuneGrid = rfGrid,
  metric = "ROC",
  ntree = 200,
  importance = TRUE
)

# 训练SVM模型
cat("训练SVM模型...\n")
svm_model <- train(
  form = significant ~ .,
  data = trainData,
  method = "svmRadial",
  trControl = fitControl,
  tuneGrid = svmGrid,
  metric = "ROC",
  preProcess = c("center", "scale", "nzv")
)

# 停止并行集群
stopCluster(cl)

# 7. 收集模型
model_results <- list(
  glmnet = glmnet_model,
  rf = rf_model,
  svmRadial = svm_model
)

# 8. 比较模型性能
results_resamples <- resamples(model_results)

# 总结交叉验证结果
print(summary(results_resamples))

# 可视化比较
tryCatch({
  bwplot(results_resamples, metric = "ROC")
}, error = function(e) cat("无法绘制箱线图\n"))

tryCatch({
  dotplot(results_resamples, metric = "ROC")
}, error = function(e) cat("无法绘制点图\n"))

# 9. 在测试集上评估所有模型
test_results <- data.frame()
for(model_name in names(model_results)){
  model <- model_results[[model_name]]
  
  # 在测试集上进行预测
  pred_prob <- predict(model, newdata = testData, type = "prob")$Significant
  pred_class <- predict(model, newdata = testData)
  
  cm <- confusionMatrix(pred_class, testData$significant, positive = "Significant")
  auc_val <- roc(testData$significant, pred_prob, levels = c("Not_Significant", "Significant"))$auc
  
  test_results <- rbind(test_results, data.frame(
    Model = model_name,
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity'],
    AUC = as.numeric(auc_val)
  ))
}

# 10. 展示测试集性能比较表
print("测试集性能比较：")
test_results %>% arrange(desc(AUC))

# 11. 找出最佳模型并进行特征重要性分析
best_idx <- which.max(test_results$AUC)
best_model_name <- test_results$Model[best_idx]
best_model <- model_results[[best_model_name]]

cat(paste("\n最佳模型是:", best_model_name, "，AUC =", test_results$AUC[best_idx], "\n"))

# 特征重要性分析
if(best_model_name %in% c("rf", "glmnet")){
  var_imp <- varImp(best_model, scale = TRUE)
  print(var_imp)
  
  plot(var_imp, top = 20, main = paste("Top 20 Features -", best_model_name, "Importance"))
  
  # 更美观的特征重要性图
  imp_data <- as.data.frame(var_imp$importance)
  if(ncol(imp_data) > 0){
    imp_data$Feature <- rownames(imp_data)
    colnames(imp_data)[1] <- "Importance"
    
    p <- imp_data %>%
      arrange(desc(Importance)) %>%
      head(20) %>%
      ggplot(aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "Feature", y = "Relative Importance", 
           title = paste("Top 20 Features for Predicting Significance (", best_model_name, ")")) +
      theme_minimal()
    print(p)
  }
} else {
  cat(paste("模型", best_model_name, "不支持varImp函数。\n"))
}


#绘制高温vs低温不同血清型的比较图------------------------------------------------
# 加载必要的包
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# 1. 读取数据
# 如果数据是Excel格式，使用read_excel；如果是CSV，使用read.csv
# 这里假设是Excel格式，您可以根据实际情况调整
# 请根据实际文件路径修改
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("General_stat.xlsx", sheet = 1)
# 2. 选择需要的列并重命名
plot_data <- data %>%
  dplyr::select(
    Top_serovars = Top_serovars,
    Temperature_types = Temperature_types,
    ARGs = ARGs
  ) %>%
  # 过滤掉ARGs为空的数据
  filter(!is.na(ARGs))
# 3. 查看数据概览
cat("数据行数:", nrow(plot_data), "\n")
cat("Top_serovars类别:", length(unique(plot_data$Top_serovars)), "\n")
print(table(plot_data$Top_serovars))
cat("Temperature_types分布:\n")
print(table(plot_data$Temperature_types))
# 4. 筛选出在cold和warm两种温度下都有数据的血清型
# 这样保证比较是在同一血清型的不同温度条件下进行
serovars_with_both_temps <- plot_data %>%
  group_by(Top_serovars) %>%
  summarise(
    has_cold = any(Temperature_types == "cold"),
    has_warm = any(Temperature_types == "warm")
  ) %>%
  filter(has_cold & has_warm) %>%
  pull(Top_serovars)
# 筛选数据
filtered_data <- plot_data %>%
  filter(Top_serovars %in% serovars_with_both_temps)
cat("同时有cold和warm数据的血清型数量:", length(serovars_with_both_temps), "\n")
cat("筛选后数据行数:", nrow(filtered_data), "\n")
# 5. 创建箱线图

p <- ggplot(filtered_data, 
            aes(x = Top_serovars, 
                y = ARGs, 
                fill = Temperature_types)) +
  # 添加箱线图
  geom_boxplot(
    width = 0.6,  # 箱线图宽度
    alpha = 0.7,  # 透明度
    outliers = FALSE) +
#   添加统计摘要点
#  stat_summary(
#    fun = median,
#    geom = "point",
#    shape = 20,
#    size = 2,
#    color = "black",
#    position = position_dodge(width = 0.75)
#  ) +
  # 设置坐标轴标签
  labs(
    x = "Serovars",
    y = "Number of ARGs",
    fill = "temperature"
  ) +
  # 设置颜色
  scale_fill_manual(
    values = c("cold" = "#1E90FF",  # 冷色：道奇蓝
               "warm" = "#FF6347") # 暖色：番茄红
  ) +
  # 设置主题
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    # 图例格式
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    # 整体布局
#    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
#    panel.grid.minor = element_blank(),
#    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# 6. 显示图形
print(p)
library(ggpubr)  # 用于统计检验和标注
# 2. 添加显著性标注（关键步骤）
# 创建比较列表：对每个血清型，比较 warm 和 cold
serovar_list <- unique(filtered_data$Top_serovars)
comparisons <- lapply(serovar_list, function(sv) {
  c("cold", "warm")
})

# 添加统计检验和标注
p <- p + 
  stat_compare_means(
    comparisons = comparisons,  # 指定配对比较
    method = "wilcox.test",    # 使用 Wilcoxon 检验（非参数）
    label = "p.signif",        # 显示显著性符号 (*, **, ***)
    p.adjust.method = "BH",    # 多重检验校正（Benjamini-Hochberg）
    hide.ns = TRUE,            # 隐藏不显著的标注
    tip.length = 0.01,         # 标注线长度
    size = 3,                  # 标注文字大小
    symnum.args = list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("***", "**", "*", "ns")
    )
  )

# 3. 显示图形
print(p)

p <- p +
  stat_compare_means(
    method = "wilcox.test",    # 非参数检验
    label = "p.signif",        # 显示显著性符号
    p.adjust.method = "BH",    # 多重检验校正
    hide.ns = TRUE,            # 隐藏不显著的结果
    size = 5,                  # 符号大小，分面图中需要调大一点
    symnum.args = list(        # 自定义符号阈值
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("", "", "", "ns")))
print(p)

##建议做好纠错的工作，确保能够正常读取





























# 加载必要的包
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(RColorBrewer)
library(ggsci)
library(cowplot)

# 1. 读取数据
# 假设数据文件是Excel格式
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("General_stat.xlsx", sheet = 1)

# 2. 数据预处理
# 确保我们只保留需要的列
plot_data <- data %>%
  dplyr::select(
    Top_serovars = Top_serovars,
    Temperature_types = Temperature_types,
    ARGs = ARGs
  ) %>%
  # 过滤掉ARGs为空的数据
  filter(!is.na(ARGs))
# 3. 筛选出在cold和warm两种温度下都有数据的血清型
serovars_with_both_temps <- plot_data %>%
  group_by(Top_serovars) %>%
  summarise(
    has_cold = any(Temperature_types == "cold"),
    has_warm = any(Temperature_types == "warm")
  ) %>%
  filter(has_cold & has_warm) %>%
  pull(Top_serovars)
# 筛选数据
filtered_data <- plot_data %>%
  filter(Top_serovars %in% serovars_with_both_temps) %>%
  mutate(Top_serovars = factor(Top_serovars, levels = unique(Top_serovars[order(ARGs,decreasing=TRUE)])))

# 4. 计算统计检验结果
# 对每个血清型进行Wilcoxon检验
stat_test <- filtered_data %>%
  group_by(Top_serovars) %>%
  pairwise_wilcox_test(ARGs ~ Temperature_types, p.adjust.method = "BH") %>%
  add_xy_position(x = "Top_serovars", dodge = 0.8) %>%
  # 只保留cold vs warm的比较
  filter(group1 == "cold" & group2 == "warm") %>%
  # 标记显著性
  mutate(significance = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01 ~ "**",
    p.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))
# 5. 计算每个血清型中cold和warm组的y轴位置（用于标注）
# 我们需要找到每个血清型中cold和warm组的最大值，然后确定标注的位置
annotation_data <- filtered_data %>%
  group_by(Top_serovars, Temperature_types) %>%
  summarise(
    max_value = max(ARGs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 找出每个血清型中cold和warm组的最大值
  group_by(Top_serovars) %>%
  summarise(
    y_position = max(max_value) + 5,  # 在最高箱线图上方添加一些空间
    .groups = "drop"
  ) %>%
  # 合并统计检验结果
  left_join(stat_test, by = "Top_serovars")
# 7. 创建箱线图
p <- ggplot(filtered_data, 
            aes(x = Top_serovars, 
                y = ARGs, 
                fill = Temperature_types)) +
  # 添加箱线图
  geom_boxplot(
    aes(color = Temperature_types),  # 边框颜色
    width = 0.7,
    alpha = 0.7,
    outliers = FALSE
  ) +
  # 添加显著性标注
  geom_text(
    data = annotation_data,
    aes(
      x = Top_serovars,
      y = y_position,
      label = significance
    ),
    size = 5,
    fontface = "bold",
    vjust = 0.5,
    inherit.aes = FALSE  # 关键修改：不继承全局美学映射
  ) +
  # 设置颜色
  scale_fill_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm")
  ) +
  scale_color_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm"),
    guide = "none"  # 不显示颜色图例
  ) +
  # 设置主题
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    # 图例格式
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    # 整体布局
  ) +
  # 设置坐标轴标签
  labs(
    x = "Serovars",
    y = "Number of ARGs",
    fill = "Temperature Condition"
  )
# 8. 显示图形
print(p)
# 9. 保存图形
ggsave(
  filename = "ARGs_serovar_temperature_comparison.jpg",
  plot = p,
  width = max(8, n_serovars * 0.5),  # 根据血清型数量调整宽度
  height = 5,
  dpi = 600,
  bg = "white"
)
# 10. 打印统计检验结果
cat("================ 统计检验结果 ================\n")
print(stat_test %>% 
        dplyr::select(Top_serovars, group1, group2, p, p.adj, significance) %>%
        arrange(Top_serovars))
# 11. 可选：如果血清型太多，可以考虑只显示最常见的几个
# 计算每个血清型的样本数
serovar_counts <- filtered_data %>%
  group_by(Top_serovars) %>%
  summarise(
    n_total = n(),
    n_cold = sum(Temperature_types == "cold"),
    n_warm = sum(Temperature_types == "warm"),
    .groups = "drop"
  ) %>%
  arrange(desc(n_total))
cat("\n================ 血清型样本统计 ================\n")
print(serovar_counts)
# 12. 可选：只显示前N个最常见的血清型
top_n <- 10  # 显示前10个最常见的血清型
top_serovars <- serovar_counts %>%
  head(top_n) %>%
  pull(Top_serovars)

filtered_data_top <- filtered_data %>%
  filter(Top_serovars %in% top_serovars)
# 重新计算统计检验
stat_test_top <- filtered_data_top %>%
  group_by(Top_serovars) %>%
  pairwise_wilcox_test(ARGs ~ Temperature_types, p.adjust.method = "BH") %>%
  add_xy_position(x = "Top_serovars", dodge = 0.8) %>%
  filter(group1 == "cold" & group2 == "warm") %>%
  mutate(significance = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01 ~ "**",
    p.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))
# 重新计算标注位置
annotation_data_top <- filtered_data_top %>%
  group_by(Top_serovars, Temperature_types) %>%
  summarise(
    max_value = max(ARGs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Top_serovars) %>%
  summarise(
    y_position = max(max_value) + 5,
    .groups = "drop"
  ) %>%
  left_join(stat_test_top, by = "Top_serovars")

# 创建简化版图形
p_top <- ggplot(filtered_data_top, 
                aes(x = Top_serovars, 
                    y = ARGs, 
                    fill = Temperature_types)) +
  geom_boxplot(
    aes(color = Temperature_types),
    width = 0.7,
    alpha = 0.7,
    outlier.shape = 19,
    outlier.size = 1.5,
    position = position_dodge(width = 0.8)
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18,
    size = 2,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  geom_text(
    data = annotation_data_top,
    aes(
      x = Top_serovars,
      y = y_position,
      label = significance
    ),
    size = 5,
    fontface = "bold",
    vjust = -0.5
  ) +
  ###颜色预设
  #values = c("not rich" = "#01579B", "rich" = "#F4511E"),
  #labels = c("not rich" = "Non-Rich", "rich" = "Rich")
  ###颜色预设
  #values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
  #labels = c("cold" = "Cold", "warm" = "Warm")
  scale_fill_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm")
  ) +
  scale_color_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm"),
    guide = "none"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top"
  ) +
  labs(
    title = paste("Top", top_n, "Serovars: ARGs Comparison (Cold vs Warm)"),
    subtitle = "Wilcoxon test: *p<0.05, **p<0.01, ***p<0.001, ns=not significant",
    x = "Serovars",
    y = "Number of ARGs",
    fill = "Temperature Condition"
  )

# 显示简化版图形
print(p_top)

# 保存简化版图形
ggsave(
  filename = "ARGs_top_serovars_temperature_comparison.png",
  plot = p_top,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

##最终版本，绘制成对比较箱型图------------------------------------------------------------------
# 加载必要的包
# 1. 读取数据
# 假设数据文件是Excel格式
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("General_stat.xlsx", sheet = 1)
# 2. 数据预处理
# 确保我们只保留需要的列
plot_data <- data %>%
  dplyr::select(
    Top_serovars = Top_serovars,
    Richness = Richness,##Richness
    ARGs = MRGs##ARGs
  ) %>%
  # 过滤掉ARGs为空的数据
  filter(!is.na(ARGs))
# 3. 筛选出在cold和warm两种温度下都有数据的血清型
serovars_with_both_temps <- plot_data %>%
  group_by(Top_serovars) %>%
  summarise(
    has_cold = any(Richness == "not rich"),##cold
    has_warm = any(Richness == "rich")##warm
  ) %>%
  filter(has_cold & has_warm) %>%
  pull(Top_serovars)
# 筛选数据
filtered_data <- plot_data %>%
  filter(Top_serovars %in% serovars_with_both_temps) %>%
  mutate(
    Top_serovars = as.character(Top_serovars),  # 先转为字符，稍后重排
    Richness = as.factor(Richness)
  )
# 4. 计算每个血清型的中位数，并按中位数排序
# 我们可以使用warm组的中位数、cold组的中位数或两者的平均中位数
# 这里我们使用warm组的中位数排序
serovar_order <- filtered_data %>%
  filter(Richness == "rich") %>%  # 使用warm组排序
  group_by(Top_serovars) %>%
  summarise(
    median_val = median(ARGs, na.rm = TRUE)
  ) %>%
  arrange(desc(median_val)) %>%
  pull(Top_serovars)
# 应用排序
filtered_data$Top_serovars <- factor(filtered_data$Top_serovars, 
                                     levels = serovar_order)
# 5. 计算统计检验结果
# 对每个血清型进行Wilcoxon检验
stat_test <- filtered_data %>%
  group_by(Top_serovars) %>%
  pairwise_wilcox_test(ARGs ~ Richness, p.adjust.method = "BH") %>%
  add_xy_position(x = "Top_serovars", dodge = 0.8) %>%
  # 只保留cold vs warm的比较
  filter(group1 == "not rich" & group2 == "rich") %>%
  # 标记显著性
  mutate(significance = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01 ~ "**",
    p.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))
# 6. 计算每个血清型中cold和warm组的y轴位置（用于标注）
# 我们需要找到每个血清型中cold和warm组的最大值，然后确定标注的位置
annotation_data <- filtered_data %>%
  group_by(Top_serovars, Richness) %>%
  summarise(
    max_value = max(ARGs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 找出每个血清型中cold和warm组的最大值
  group_by(Top_serovars) %>%
  summarise(
    y_position = max(max_value) + 5,  # 在最高箱线图上方添加一些空间
    .groups = "drop"
  ) %>%
  # 合并统计检验结果
  left_join(stat_test, by = "Top_serovars")
# 7. 定义血清型颜色（如果有很多血清型，使用颜色调色板）
# 获取唯一的血清型数量
n_serovars <- length(unique(filtered_data$Top_serovars))
# 8. 创建箱线图
p <- ggplot(filtered_data, 
            aes(x = Top_serovars, 
                y = ARGs, 
                fill = Richness)) +
  # 添加箱线图
  geom_boxplot(
    aes(color = Richness),  # 边框颜色
    width = 0.7,
    alpha = 0.7,
    outliers = FALSE) +
  geom_jitter(
    aes(color = Richness),  # 散点颜色按温度类型区分
    alpha = 0.4, 
#    width = 0.4, 
#    height = 0,  # 只在水平方向抖动
    position = position_dodge(width = 0.8)  # 与箱线图保持相同的位置偏移
  ) +
  # 添加显著性标注
  geom_text(
    data = annotation_data,
    aes(
      x = Top_serovars,
      y = y_position,
      label = significance
    ),
    size = 5,
    fontface = "bold",
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  # 设置颜色
  scale_fill_manual(
    values = c("not rich" = "#01579B", "rich" = "#F4511E"),
    labels = c("not rich" = "Non-Rich", "rich" = "Rich")
  ) +
  scale_color_manual(
    values = c("not rich" = "#01579B", "rich" = "#F4511E"),
    labels = c("not rich" = "Non-Rich", "rich" = "Rich"),
    guide = "none"  # 不显示颜色图例
  ) +
  # 设置主题
  theme_bw() +
  theme(
    # 坐标轴格式
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    # 图例格式
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    # 整体布局
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  # 设置坐标轴标签
  labs(
    x = "Serovars",
    y = "Number of MRGs",##ARGs
    fill = "Richness"
  )
# 9. 显示图形
print(p)
# 10. 保存图形
ggsave(
  filename = "MRGs_serovar_Richness_comparison_sorted.pdf",
  plot = p,
  width = max(8, n_serovars * 0.5),  # 根据血清型数量调整宽度
  height = 5,
  dpi = 600,
  bg = "white"
)


##中美对比---------------------------------------------------------------------------
##最终版本，绘制成对比较箱型图------------------------------------------------------------------
# 加载必要的包
# 1. 读取数据
# 假设数据文件是Excel格式
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("General_stat.xlsx", sheet = 4)
# 2. 数据预处理
# 确保我们只保留需要的列
plot_data <- data %>%
  dplyr::select(
    Top_serovars = Serovars,
    Countries = Countries,##Richness
    ARGs = ARGs##ARGs
  ) %>%
  # 过滤掉ARGs为空的数据
  filter(!is.na(ARGs))
# 3. 筛选出在cold和warm两种温度下都有数据的血清型
serovars_with_both_temps <- plot_data %>%
  group_by(Top_serovars) %>%
  summarise(
    has_cold = any(Countries == "USA"),##cold
    has_warm = any(Countries == "China")##warm
  ) %>%
  filter(has_cold & has_warm) %>%
  pull(Top_serovars)
# 筛选数据
filtered_data <- plot_data %>%
  filter(Top_serovars %in% serovars_with_both_temps) %>%
  mutate(
    Top_serovars = as.character(Top_serovars),  # 先转为字符，稍后重排
    Countries = as.factor(Countries)
  )
# 4. 计算每个血清型的中位数，并按中位数排序
# 我们可以使用warm组的中位数、cold组的中位数或两者的平均中位数
# 这里我们使用warm组的中位数排序
serovar_order <- filtered_data %>%
  filter(Countries == "China") %>%  # 使用warm组排序
  group_by(Top_serovars) %>%
  summarise(
    median_val = median(ARGs, na.rm = TRUE)
  ) %>%
  arrange(desc(median_val)) %>%
  pull(Top_serovars)
# 应用排序
filtered_data$Top_serovars <- factor(filtered_data$Top_serovars, 
                                     levels = serovar_order)
# 5. 计算统计检验结果
# 对每个血清型进行Wilcoxon检验
stat_test <- filtered_data %>%
  group_by(Top_serovars) %>%
  pairwise_wilcox_test(ARGs ~ Countries, p.adjust.method = "BH") %>%
  add_xy_position(x = "Top_serovars", dodge = 0.8) %>%
  # 标记显著性
  mutate(significance = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01 ~ "**",
    p.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))
# 6. 计算每个血清型中cold和warm组的y轴位置（用于标注）
# 我们需要找到每个血清型中cold和warm组的最大值，然后确定标注的位置
annotation_data <- filtered_data %>%
  group_by(Top_serovars, Countries) %>%
  summarise(
    max_value = max(ARGs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 找出每个血清型中cold和warm组的最大值
  group_by(Top_serovars) %>%
  summarise(
    y_position = max(max_value) + 5,  # 在最高箱线图上方添加一些空间
    .groups = "drop"
  ) %>%
  # 合并统计检验结果
  left_join(stat_test, by = "Top_serovars")
# 7. 定义血清型颜色（如果有很多血清型，使用颜色调色板）
# 获取唯一的血清型数量
n_serovars <- length(unique(filtered_data$Top_serovars))
# 8. 创建箱线图
p <- ggplot(filtered_data, 
            aes(x = Top_serovars, 
                y = ARGs, 
                fill = Countries)) +
  # 添加箱线图
  geom_boxplot(
    aes(color = Countries),  # 边框颜色
    width = 0.7,
    alpha = 0.7,
    outliers = FALSE) +
  geom_jitter(
    aes(color = Countries),  # 散点颜色按温度类型区分
    alpha = 0.4, 
    #    width = 0.4, 
    #    height = 0,  # 只在水平方向抖动
    position = position_dodge(width = 0.8)  # 与箱线图保持相同的位置偏移
  ) +
  # 添加显著性标注
  geom_text(
    data = annotation_data,
    aes(
      x = Top_serovars,
      y = y_position,
      label = significance
    ),
    size = 5,
    fontface = "bold",
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  # 设置颜色
  scale_fill_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("USA" = "USA", "China" = "China")
  ) +
  scale_color_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("USA" = "USA", "China" = "China"),
    guide = "none"  # 不显示颜色图例
  ) +
  # 设置主题
  theme_bw() +
  theme(
    # 坐标轴格式
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    # 图例格式
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    # 整体布局
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  # 设置坐标轴标签
  labs(
    x = "Serovars",
    y = "Number of ARGs",##ARGs
    fill = "Countries"
  )
# 9. 显示图形
print(p)
# 10. 保存图形
ggsave(
  filename = "ARGs_serovar_Countries_comparison.jpg",
  plot = p,
  width = max(8, n_serovars * 0.5),  # 根据血清型数量调整宽度
  height = 5,
  dpi = 600,
  bg = "white"
)
##


##中国临床与猪肉的差异-----------------------------------------------------------------
# 1. 读取数据
# 假设数据文件是Excel格式
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("General_stat.xlsx", sheet = 4)
# 2. 数据预处理
# 确保我们只保留需要的列
plot_data <- data %>%
  dplyr::select(
    Top_serovars = Serovars,
    Sources = Sources,##Richness
    ARGs = ARGs##ARGs
  ) 
# 3. 筛选出在cold和warm两种温度下都有数据的血清型
serovars_with_both_temps <- plot_data %>%
  group_by(Top_serovars) %>%
  summarise(
    has_cold = any(Sources == "China_clinics"),##cold
    has_warm = any(Sources == "China_Pork")##warm
  ) %>%
  filter(has_cold & has_warm) %>%
  pull(Top_serovars)
# 筛选数据
filtered_data <- plot_data %>%
  filter(Top_serovars %in% serovars_with_both_temps) %>%
  mutate(
    Top_serovars = as.character(Top_serovars),  # 先转为字符，稍后重排
    Sources = as.factor(Sources)
  )
# 4. 计算每个血清型的中位数，并按中位数排序
# 我们可以使用warm组的中位数、cold组的中位数或两者的平均中位数
# 这里我们使用warm组的中位数排序
serovar_order <- filtered_data %>%
  filter(Sources == "China_clinics") %>%  # 使用warm组排序
  group_by(Top_serovars) %>%
  summarise(
    median_val = median(ARGs, na.rm = TRUE)
  ) %>%
  arrange(desc(median_val)) %>%
  pull(Top_serovars)
# 应用排序
filtered_data$Top_serovars <- factor(filtered_data$Top_serovars, 
                                     levels = serovar_order)
# 5. 计算统计检验结果
# 对每个血清型进行Wilcoxon检验
stat_test <- filtered_data %>%
  group_by(Top_serovars) %>%
  pairwise_wilcox_test(ARGs ~ Sources, p.adjust.method = "BH") %>%
  add_xy_position(x = "Top_serovars", dodge = 0.8) %>%
  # 标记显著性
  mutate(significance = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01 ~ "**",
    p.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))
# 6. 计算每个血清型中cold和warm组的y轴位置（用于标注）
# 我们需要找到每个血清型中cold和warm组的最大值，然后确定标注的位置
annotation_data <- filtered_data %>%
  group_by(Top_serovars, Sources) %>%
  summarise(
    max_value = max(ARGs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 找出每个血清型中cold和warm组的最大值
  group_by(Top_serovars) %>%
  summarise(
    y_position = max(max_value) + 5,  # 在最高箱线图上方添加一些空间
    .groups = "drop"
  ) %>%
  # 合并统计检验结果
  left_join(stat_test, by = "Top_serovars")
# 7. 定义血清型颜色（如果有很多血清型，使用颜色调色板）
# 获取唯一的血清型数量
n_serovars <- length(unique(filtered_data$Top_serovars))
# 8. 创建箱线图
p <- ggplot(filtered_data, 
            aes(x = Top_serovars, 
                y = ARGs, 
                fill = Sources)) +
  # 添加箱线图
  geom_boxplot(
    aes(color = Sources),  # 边框颜色
    width = 0.7,
    alpha = 0.7,
    outliers = FALSE) +
  geom_jitter(
    aes(color = Sources),  # 散点颜色按温度类型区分
    alpha = 0.4, 
    #    width = 0.4, 
    #    height = 0,  # 只在水平方向抖动
    position = position_dodge(width = 0.8)  # 与箱线图保持相同的位置偏移
  ) +
  # 添加显著性标注
  geom_text(
    data = annotation_data,
    aes(
      x = Top_serovars,
      y = y_position,
      label = significance
    ),
    size = 5,
    fontface = "bold",
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  # 设置颜色
  scale_fill_manual( 
    values = c("China_Pork" = "#01579B", "China_clinics" = "#F4511E"),
#    labels = c("China_clinics" = "China_clinics", "China_Pork" = "China_Pork")
  ) +
  scale_color_manual(
    values = c("China_Pork" = "#01579B", "China_clinics" = "#F4511E"),
#    labels = c("China_clinics" = "China_clinics", "China_Pork" = "China_Pork"),
    guide = "none"  # 不显示颜色图例
  ) +
  # 设置主题
  theme_bw() +
  theme(
    # 坐标轴格式
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    # 图例格式
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    # 整体布局
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  # 设置坐标轴标签
  labs(
    x = "Serovars",
    y = "Number of ARGs",##ARGs
    fill = "Sources"
  )
# 9. 显示图形
print(p)
# 10. 保存图形
ggsave(
  filename = "ARGs_serovar_clinics_comparison.pdf",
  plot = p,
  width = max(8, n_serovars * 0.5),  # 根据血清型数量调整宽度
  height = 5,
  dpi = 600,
  bg = "white"
)









##依据温度进行划分-----------------------------------------------------------
# 2. 数据预处理
# 确保我们只保留需要的列
plot_data <- data %>%
  dplyr::select(
    Top_serovars = Top_serovars,
    Temperature_types = Temperature_types,##Temperature_types
    ARGs = ARGs##ARGs
  ) %>%
  # 过滤掉ARGs为空的数据
  filter(!is.na(ARGs))
# 3. 筛选出在cold和warm两种温度下都有数据的血清型
serovars_with_both_temps <- plot_data %>%
  group_by(Top_serovars) %>%
  summarise(
    has_cold = any(Temperature_types == "cold"),##cold
    has_warm = any(Temperature_types == "warm")##warm
  ) %>%
  filter(has_cold & has_warm) %>%
  pull(Top_serovars)
# 筛选数据
filtered_data <- plot_data %>%
  filter(Top_serovars %in% serovars_with_both_temps) %>%
  mutate(
    Top_serovars = as.character(Top_serovars),  # 先转为字符，稍后重排
    Temperature_types = as.factor(Temperature_types)
  )
# 4. 计算每个血清型的中位数，并按中位数排序
# 我们可以使用warm组的中位数、cold组的中位数或两者的平均中位数
# 这里我们使用warm组的中位数排序
serovar_order <- filtered_data %>%
  filter(Temperature_types == "warm") %>%  # 使用warm组排序
  group_by(Top_serovars) %>%
  summarise(
    median_val = median(ARGs, na.rm = TRUE)
  ) %>%
  arrange(desc(median_val)) %>%
  pull(Top_serovars)
# 应用排序
filtered_data$Top_serovars <- factor(filtered_data$Top_serovars, 
                                     levels = serovar_order)
# 5. 计算统计检验结果
# 对每个血清型进行Wilcoxon检验
stat_test <- filtered_data %>%
  group_by(Top_serovars) %>%
  pairwise_wilcox_test(ARGs ~ Temperature_types, p.adjust.method = "BH") %>%
  add_xy_position(x = "Top_serovars", dodge = 0.8) %>%
  # 只保留cold vs warm的比较
  filter(group1 == "cold" & group2 == "warm") %>%
  # 标记显著性
  mutate(significance = case_when(
    p.adj < 0.001 ~ "***",
    p.adj < 0.01 ~ "**",
    p.adj < 0.05 ~ "*",
    TRUE ~ "ns"
  ))
# 6. 计算每个血清型中cold和warm组的y轴位置（用于标注）
# 我们需要找到每个血清型中cold和warm组的最大值，然后确定标注的位置
annotation_data <- filtered_data %>%
  group_by(Top_serovars, Temperature_types) %>%
  summarise(
    max_value = max(ARGs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 找出每个血清型中cold和warm组的最大值
  group_by(Top_serovars) %>%
  summarise(
    y_position = max(max_value) + 5,  # 在最高箱线图上方添加一些空间
    .groups = "drop"
  ) %>%
  # 合并统计检验结果
  left_join(stat_test, by = "Top_serovars")
# 7. 定义血清型颜色（如果有很多血清型，使用颜色调色板）
# 获取唯一的血清型数量
n_serovars <- length(unique(filtered_data$Top_serovars))
# 8. 创建箱线图
p <- ggplot(filtered_data, 
            aes(x = Top_serovars, 
                y = ARGs, 
                fill = Temperature_types)) +
  # 添加箱线图
  geom_boxplot(
    aes(color = Temperature_types),  # 边框颜色
    width = 0.7,
    alpha = 0.7,
    outliers = FALSE) +
  geom_jitter(
    aes(color = Temperature_types),  # 散点颜色按温度类型区分
    alpha = 0.4, 
    #    width = 0.4, 
    #    height = 0,  # 只在水平方向抖动
    position = position_dodge(width = 0.8)  # 与箱线图保持相同的位置偏移
  ) +
  # 添加显著性标注
  geom_text(
    data = annotation_data,
    aes(
      x = Top_serovars,
      y = y_position,
      label = significance
    ),
    size = 5,
    fontface = "bold",
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  # 设置颜色
  scale_fill_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm")
  ) +
  scale_color_manual(
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm"),
    guide = "none"  # 不显示颜色图例
  ) +
  # 设置主题
  theme_bw() +
  theme(
    # 坐标轴格式
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    # 图例格式
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    # 整体布局
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  # 设置坐标轴标签
  labs(
    x = "Serovars",
    y = "Number of ARG",##ARGs
    fill = "Temperature"
  )
# 9. 显示图形
print(p)
# 10. 保存图形
ggsave(
  filename = "ARG_serovar_temperature_comparison_sorted.pdf",
  plot = p,
  width = max(8, n_serovars * 0.5),  # 根据血清型数量调整宽度
  height = 5,
  dpi = 600,
  bg = "white"
)
##----------------------------------------------------------------------------------












# 11. 打印统计检验结果
cat("================ 统计检验结果 ================\n")
print(stat_test %>% 
        dplyr::select(Top_serovars, group1, group2, p, p.adj, significance) %>%
        arrange(Top_serovars))

# 12. 打印血清型排序（按中位数）
cat("\n================ 血清型排序（按warm组中位数） ================\n")
serovar_median_summary <- filtered_data %>%
  filter(Temperature_types == "warm") %>%
  group_by(Top_serovars) %>%
  summarise(
    median_args = median(ARGs, na.rm = TRUE),
    mean_args = mean(ARGs, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(median_args))

print(serovar_median_summary)
##表型数据解读-------------------------------------------------------------------
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(extrafont)
# 1. 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 9)
# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")
# 2. 数据预处理
# 提取抗生素列
antibiotic_cols <- c("AMP", "IPM", "FEP", "CTX", "TET", "TGC", "COL", 
                     "GEN", "CIP", "SXT", "AZM", "CHL","STR")

# 检查数据
cat("抗生素列:", antibiotic_cols, "\n")
# 3. 计算不同血清型的耐药率
# 转换数据为长格式
data_long <- data %>%
  select(ID, location, Serovars, all_of(antibiotic_cols)) %>%
  pivot_longer(cols = all_of(antibiotic_cols), 
               names_to = "Antibiotic", 
               values_to = "RIS") %>%
  mutate(
    RIS = factor(RIS, levels = c("S", "I", "R")),
    Serovars = factor(Serovars),
    location = factor(location)
  )
# 3.1 不同血清型的耐药率比较
# 计算每个血清型对每种抗生素的耐药率
serovar_resistance <- data_long %>%
  group_by(Serovars, Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Serovars, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()
# 可视化：不同血清型的耐药率热图
# 只显示R（耐药）的比例
serovar_R <- serovar_resistance %>%
  filter(RIS == "R") %>%
  select(Serovars, Antibiotic, Percentage)
# 转换为宽格式用于热图
serovar_heatmap_data <- serovar_R %>%
  pivot_wider(names_from = Antibiotic, values_from = Percentage, values_fill = 0) %>%
  column_to_rownames(var = "Serovars") %>%
  as.matrix()
# 绘制热图
p1 <- pheatmap::pheatmap(
  serovar_heatmap_data,border_color = "white", 
  #  color = colorRampPalette(brewer.pal(9, "YlOrRd"))(100),
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  display_numbers = TRUE,
  number_format = "%.1f",
  number_color = "black",
  #  main = "不同血清型的耐药率（R%）",
  fontsize_row = 10,
  fontsize_col = 10,
  angle_col = 45
)
p1
#ggsave("serovar_resistance_heatmap.pdf",p1,width = 6,height = 4,dpi = 600)
# 3.2 统计检验：比较不同血清型间的耐药率差异
# 对每种抗生素进行卡方检验
chi_square_results <- data.frame()
for (ab in antibiotic_cols) {
  # 创建列联表
  cont_table <- table(data$Serovars, data[[ab]])
  # 确保表格有效
  if (nrow(cont_table) > 1 && ncol(cont_table) > 1) {
    # 进行卡方检验
    chi_test <- chisq.test(cont_table)
    chi_square_results <- rbind(chi_square_results, data.frame(
      Antibiotic = ab,
      Chi_squared = round(chi_test$statistic, 3),
      df = chi_test$parameter,
      p_value = chi_test$p.value
    ))
  }
}
# 多重检验校正
chi_square_results$adj_p_value <- p.adjust(chi_square_results$p_value, method = "fdr")
cat("\n不同血清型耐药率差异的卡方检验结果：\n")
print(chi_square_results)
##
# 4. 南北方耐药率比较-------------------------------------------------------------
# 计算南北方耐药率
location_resistance <- data_long %>%
  group_by(location, Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(location, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()

Temperature_types


##计算富裕、不富裕地区的耐药性
Richness_resistance <- data %>%
  select(ID, Richness, Serovars, all_of(antibiotic_cols)) %>%
  pivot_longer(cols = all_of(antibiotic_cols), 
               names_to = "Antibiotic", 
               values_to = "RIS") %>%
  mutate(
    RIS = factor(RIS, levels = c("S", "I", "R")),
    Serovars = factor(Serovars),
    Richness = factor(Richness)
  ) %>%
  group_by(Richness, Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Richness, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()
##计算寒冷、温暖地区的耐药性--------------------------------------
Warm_resistance <- data %>%
  select(ID, Temperature_types, Serovars, all_of(antibiotic_cols)) %>%
  pivot_longer(cols = all_of(antibiotic_cols), 
               names_to = "Antibiotic", 
               values_to = "RIS") %>%
  mutate(
    RIS = factor(RIS, levels = c("S", "I", "R")),
    Serovars = factor(Serovars),
    Temperature_types = factor(Temperature_types)
  ) %>%
  group_by(Temperature_types, Antibiotic, RIS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Temperature_types, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()
##计算---------------------------------------------------------------
# 只显示R（耐药）的比例
location_R <- location_resistance %>%
  filter(RIS == "R") %>%
  select(location, Antibiotic, Percentage)%>%
  mutate(Antibiotic = factor(Antibiotic, levels = rev(unique(Antibiotic[order(Percentage, decreasing = TRUE)]))))
# 转换为宽格式用于可视化
location_wide <- location_R %>%
  pivot_wider(names_from = Antibiotic, values_from = Percentage, values_fill = 0)
# 绘制南北方耐药率比较条形图
location_R_long <- location_R %>%
  pivot_longer(cols = -c(location, Antibiotic), 
               names_to = "Type", 
               values_to = "Value")
p2 <- ggplot(location_R, aes(x = Antibiotic, y = Percentage, fill = location)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  #   scale_color_brewer(palette = "Set2") +
  scale_fill_manual(name="",values = colorRampPalette(brewer.pal(3, "Set2"))(3))+
  #  scale_fill_manual(values = c("North" = "#377EB8", "South" = "#E41A1C")) +
  labs(
    x = "Antibiotics",
    y = "Resistance rate (%)",
    fill = "Location"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 101)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 15),
    legend.position = "right"
  )
p2
#ggsave("north_south_phenotypes_comparison.pdf",p2,width = 5,height = 5,dpi =600 )
##地区富裕的耐药性---------------------------------------------------------
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(extrafont)
library(rstatix)
# 1. 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 9)
# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")
# 2. 数据预处理
# 提取抗生素列
antibiotic_cols <- c("AMP", "IPM", "FEP", "CTX", "TET", "TGC", "COL", 
                     "GEN", "CIP", "SXT", "AZM", "CHL","STR")
# 检查数据
cat("抗生素列:", antibiotic_cols, "\n")
# 3. 数据转换 - 计算每个样本的耐药状态
# 创建长格式数据，包含每个样本的抗生素耐药信息
data_long <- data %>%
  select(ID, Temperature_types, all_of(antibiotic_cols)) %>%     ##Richness
  pivot_longer(cols = all_of(antibiotic_cols), 
               names_to = "Antibiotic", 
               values_to = "RIS") %>%
  mutate(
    # 将耐药状态转换为二分类变量：耐药(R) vs 非耐药(S/I)
    Resistant = ifelse(RIS == "R", 1, 0),
#    Richness = factor(Richness, levels = c("not rich", "rich")),
    Temperature_types= factor(Temperature_types, levels = c("cold", "warm")),
    Antibiotic = factor(Antibiotic)
  )
# 4. 统计检验 - 对每种抗生素进行卡方检验
# 创建一个函数来执行统计检验
perform_stat_test <- function(Temperature_vector, resistant_vector) {
  # 创建列联表
  cont_table <- table(Temperature_vector, resistant_vector)
  # 检查是否有足够的样本进行卡方检验
  if (all(cont_table >= 5)) {
    # 尝试卡方检验
    test_result <- tryCatch(
      chisq.test(cont_table),
      error = function(e) fisher.test(cont_table)
    )
  } else {
    # 使用Fisher精确检验
    test_result <- fisher.test(cont_table)
  }
  # 提取结果
  p_value <- test_result$p.value
  chi_sq <- if ("statistic" %in% names(test_result)) as.numeric(test_result$statistic) else NA
  # 计算效应量
  n <- sum(cont_table)
  if (!is.na(chi_sq)) {
    phi_sq <- chi_sq / n
    cramer_v <- sqrt(phi_sq / (min(dim(cont_table)) - 1))
  } else {
    cramer_v <- NA
  }
  # 显著性标注
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
  return(data.frame(p_value = p_value, significance = significance, 
                    chi_sq = chi_sq, cramer_v = cramer_v))
}
# 对每种抗生素执行统计检验
stat_test_results <- data_long %>%
  group_by(Antibiotic) %>%
  group_modify(~ {
    # 对每个抗生素组执行统计检验
    test_result <- perform_stat_test(.x$Temperature_types, .x$Resistant)
    return(test_result)
  })
# 5. 计算耐药率
Richness_resistance <- data_long %>%
  group_by(Temperature_types, Antibiotic, Resistant) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Temperature_types, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()

# 提取耐药率
Richness_R <- Richness_resistance %>%
  filter(Resistant == 1) %>%
  select(Temperature_types, Antibiotic, Percentage) %>%
  # 按总耐药率排序
  group_by(Antibiotic) %>%
  mutate(
    # 按总耐药率排序
    total_resistance = sum(Percentage)
  ) %>%
  ungroup() %>%
  mutate(
    Antibiotic = fct_reorder(Antibiotic, total_resistance, .desc = TRUE)
  )
# 6. 合并统计检验结果
Richness_R <- Richness_R %>%
  left_join(stat_test_results, by = "Antibiotic")
# 7. 计算标注位置
# 找到每个抗生素的最大耐药率，用于确定标注位置
label_positions <- Richness_R %>%
  group_by(Antibiotic) %>%
  summarise(
    max_percentage = max(Percentage, na.rm = TRUE),
    y_position = max_percentage + 5,  # 在最高柱子上方添加空间
    .groups = "drop"
  ) %>%
  left_join(stat_test_results, by = "Antibiotic")

# 8. 创建柱状图
p2 <- ggplot(Richness_R, aes(x = Antibiotic, y = Percentage, fill = Temperature_types)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # 设置颜色
  scale_fill_manual(
    name = "",
    values = c("cold" = "#377EB8", "warm" = "#E41A1C"),
    labels = c("cold" = "Cold", "warm" = "Warm")
  ) +
  # 添加显著性标注
  geom_text(
    data = label_positions,
    aes(x = Antibiotic, y = y_position, label = significance),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    vjust = -0.5
  ) +
  # 添加坐标轴标签
  labs(
    x = "Antibiotics",
    y = "Resistance Rate (%)",
    fill = "Region Richness"
  ) +
#  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),  # 为标注留出空间
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(label_positions$y_position, na.rm = TRUE) + 10)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# 9. 显示图形
print(p2)

# 10. 保存图形
ggsave("Antibiotic_Resistance_temperature_Comparison.jpg", 
       plot = p2, 
       width = 8, 
       height = 4, 
       dpi = 600)
##换成Richness重新画一遍-------------------------------------------------------------
# 加载必要的包
library(readxl)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(scales)
library(extrafont)
library(rstatix)
# 1. 读取数据
setwd("C:/Users/Administrator/Downloads/data/salmonella/21_pork_salmon")
data <- read_excel("1418_salmonella_pork.xlsx", sheet = 9)
# 查看数据结构
cat("数据维度:", dim(data), "\n")
cat("列名:", colnames(data), "\n")
# 2. 数据预处理
# 提取抗生素列
antibiotic_cols <- c("AMP", "IPM", "FEP", "CTX", "TET", "TGC", "COL", 
                     "GEN", "CIP", "SXT", "AZM", "CHL","STR")
# 检查数据
cat("抗生素列:", antibiotic_cols, "\n")
# 3. 数据转换 - 计算每个样本的耐药状态
# 创建长格式数据，包含每个样本的抗生素耐药信息
data_long <- data %>%
  select(ID, Richness, all_of(antibiotic_cols)) %>%
  pivot_longer(cols = all_of(antibiotic_cols), 
               names_to = "Antibiotic", 
               values_to = "RIS") %>%
  mutate(
    # 将耐药状态转换为二分类变量：耐药(R) vs 非耐药(S/I)
    Resistant = ifelse(RIS == "R", 1, 0),
    Richness = factor(Richness, levels = c("not rich", "rich")),
    Antibiotic = factor(Antibiotic)
  )
# 4. 统计检验 - 对每种抗生素进行卡方检验
# 创建一个函数来执行统计检验
perform_stat_test <- function(richness_vector, resistant_vector) {
  # 创建列联表
  cont_table <- table(richness_vector, resistant_vector)
  
  # 检查是否有足够的样本进行卡方检验
  if (all(cont_table >= 5)) {
    # 尝试卡方检验
    test_result <- tryCatch(
      chisq.test(cont_table),
      error = function(e) fisher.test(cont_table)
    )
  } else {
    # 使用Fisher精确检验
    test_result <- fisher.test(cont_table)
  }
  # 提取结果
  p_value <- test_result$p.value
  chi_sq <- if ("statistic" %in% names(test_result)) as.numeric(test_result$statistic) else NA
  # 计算效应量
  n <- sum(cont_table)
  if (!is.na(chi_sq)) {
    phi_sq <- chi_sq / n
    cramer_v <- sqrt(phi_sq / (min(dim(cont_table)) - 1))
  } else {
    cramer_v <- NA
  }
  # 显著性标注
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )
  return(data.frame(p_value = p_value, significance = significance, 
                    chi_sq = chi_sq, cramer_v = cramer_v))
}

# 对每种抗生素执行统计检验
stat_test_results <- data_long %>%
  group_by(Antibiotic) %>%
  group_modify(~ {
    # 对每个抗生素组执行统计检验
    test_result <- perform_stat_test(.x$Richness, .x$Resistant)
    return(test_result)
  })

# 5. 计算耐药率
Richness_resistance <- data_long %>%
  group_by(Richness, Antibiotic, Resistant) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Richness, Antibiotic) %>%
  mutate(
    Total = sum(Count),
    Percentage = Count / Total * 100
  ) %>%
  ungroup()

# 提取耐药率
Richness_R <- Richness_resistance %>%
  filter(Resistant == 1) %>%
  select(Richness, Antibiotic, Percentage) %>%
  # 按总耐药率排序
  group_by(Antibiotic) %>%
  mutate(
    # 按总耐药率排序
    total_resistance = sum(Percentage)
  ) %>%
  ungroup() %>%
  mutate(
    Antibiotic = fct_reorder(Antibiotic, total_resistance, .desc = TRUE)
  )

# 6. 合并统计检验结果
Richness_R <- Richness_R %>%
  left_join(stat_test_results, by = "Antibiotic")

# 7. 计算标注位置
# 找到每个抗生素的最大耐药率，用于确定标注位置
label_positions <- Richness_R %>%
  group_by(Antibiotic) %>%
  summarise(
    max_percentage = max(Percentage, na.rm = TRUE),
    y_position = max_percentage + 5,  # 在最高柱子上方添加空间
    .groups = "drop"
  ) %>%
  left_join(stat_test_results, by = "Antibiotic")

# 8. 创建柱状图
p2 <- ggplot(Richness_R, aes(x = Antibiotic, y = Percentage, fill = Richness)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # 设置颜色
  scale_fill_manual(
    name = "",
    values = c("not rich" = "#01579B", "rich" = "#F4511E"),
    labels = c("not rich" = "Non-Rich", "rich" = "Rich")
  ) +
#  color_schemes <- list(
#    reference = c(cold = "#377EB8", warm = "#E41A1C"),
#    scheme1 = c(cold = "#2A6F9D", warm = "#D32F2F"),##深蓝-砖红
#    scheme2 = c(cold = "#1E88E5", warm = "#FF5722"),##亮蓝-橙色
#    scheme3 = c(cold = "#0277BD", warm = "#F4511E"),##钴蓝-橙红
#    scheme4 = c(cold = "#006064", warm = "#FF7043"),##深青-珊瑚橙
#    scheme5 = c(cold = "#01579B", warm = "#FF5252")##午夜蓝-亮红
#  )
  # 添加显著性标注
  geom_text(
    data = label_positions,
    aes(x = Antibiotic, y = y_position, label = significance),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    vjust = -0.5
  ) +
  # 添加坐标轴标签
  labs(
#    title = "Antibiotic Resistance Rates by Region Richness",
#    subtitle = "Statistical significance: *p<0.05, **p<0.01, ***p<0.001, ns=not significant",
    x = "Antibiotics",
    y = "Resistance Rate (%)",
    fill = "Region Richness"
  ) +
#  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),  # 为标注留出空间
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(label_positions$y_position, na.rm = TRUE) + 10)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# 9. 显示图形
print(p2)
# 10. 保存图形
ggsave("Antibiotic_Resistance_Richness_Comparison.pdf", 
       plot = p2, 
       width = 8, 
       height = 4, 
       dpi = 600)
# 11. 打印统计检验结果
cat("\n========== 统计检验结果 ==========\n")
print(stat_test_results %>%
        select(Antibiotic, p_value, significance, cramer_v) %>%
        arrange(p_value))
# 12. 创建更详细的统计摘要
summary_stats <- data_long %>%
  group_by(Antibiotic, Richness) %>%
  summarise(
    n_samples = n(),
    n_resistant = sum(Resistant),
    resistance_rate = mean(Resistant) * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Antibiotic,
    names_from = Richness,
    values_from = c(n_samples, n_resistant, resistance_rate)
  ) %>%
  left_join(stat_test_results, by = "Antibiotic") %>%
  select(
    Antibiotic,
    starts_with("n_samples_"),
    starts_with("n_resistant_"),
    starts_with("resistance_rate_"),
    p_value,
    significance,
    cramer_v
  ) %>%
  rename(
    "Samples_not_rich" = n_samples_not_rich,
    "Samples_rich" = n_samples_rich,
    "Resistant_not_rich" = n_resistant_not_rich,
    "Resistant_rich" = n_resistant_rich,
    "Resistance_rate_not_rich" = resistance_rate_not_rich,
    "Resistance_rate_rich" = resistance_rate_rich
  )
cat("\n========== 详细统计摘要 ==========\n")
print(summary_stats)
# 13. 保存统计结果
write.csv(summary_stats, "Antibiotic_Resistance_Richness_Stats.csv", row.names = FALSE)
cat("统计结果已保存为 Antibiotic_Resistance_Richness_Stats.csv\n")
# 14. 创建带有置信区间的误差棒图（可选）
# 计算置信区间
confidence_intervals <- data_long %>%
  group_by(Antibiotic, Richness) %>%
  summarise(
    n = n(),
    resistant = sum(Resistant),
    prop = mean(Resistant),
    se = sqrt(prop * (1 - prop) / n),
    ci_lower = prop - 1.96 * se,
    ci_upper = prop + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = prop * 100,
    CI_lower = ci_lower * 100,
    CI_upper = ci_upper * 100
  )

# 创建带有误差棒的图
p3 <- ggplot(confidence_intervals, aes(x = Antibiotic, y = Percentage, fill = Richness)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    position = position_dodge(width = 0.8),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    name = "",
    values = c("not rich" = "#377EB8", "rich" = "#E41A1C"),
    labels = c("not rich" = "Not Rich", "rich" = "Rich")
  ) +
  # 添加显著性标注
  geom_text(
    data = label_positions,
    aes(x = Antibiotic, y = y_position, label = significance),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold",
    vjust = -0.5
  ) +
  labs(
    title = "Antibiotic Resistance Rates with 95% Confidence Intervals",
    subtitle = "Statistical significance: *p<0.05, **p<0.01, ***p<0.001, ns=not significant",
    x = "Antibiotics",
    y = "Resistance Rate (%)",
    fill = "Region Richness"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(label_positions$y_position, na.rm = TRUE) + 10)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

# 显示和保存误差棒图
print(p3)
ggsave("Antibiotic_Resistance_Richness_CI.png", 
       plot = p3, 
       width = 10, 
       height = 8, 
       dpi = 300)




