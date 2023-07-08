# Automatically fetch parameter documentation for column definitions

library(rvest)

coldef <- read_html("https://www.ag-grid.com/javascript-data-grid/column-properties/#reference-columns")

all_doc <- coldef |> html_table(trim = T)

doc1 <- all_doc[[1]]

doc1$X1 <- sub("Type.*$","",doc1$X1) |> trimws()
doc1$X2 <- sub("More details.*$ | See: .*$","",doc1$X2) |> trimws()

# 里面有个cellDataType 被替换

# 生成参数文档
apply(doc1, 1, function(x) {
  cat("#' @param",x[1],x[2],"\n")
  invisible()
})

# 生成参数列表等
apply(doc1, 1, function(x) {
  cat(x[1]," = ",x[1],",\n",sep = "")
})

# 宽度
doc16 <- all_doc[[16]]
doc16$X1 <- sub("Type.*$","",doc16$X1) |> trimws()
doc16$X2 <- sub("More details.*$ | See: .*$","",doc16$X2) |> trimws()

apply(doc16, 1, function(x) {
  cat(x[1]," = ",x[1],",\n",sep = "")
})

apply(doc16, 1, function(x) {
  cat("#' @param",x[1],x[2],"\n")
})

doc13 <- all_doc[[13]]

doc13$X1 <- sub("Type.*$","",doc13$X1) |> trimws()
doc13$X2 <- sub("More details.*$ | See: .*$","",doc13$X2) |> trimws()

apply(doc13, 1, function(x) {
  cat(x[1]," = ","NULL",",\n",sep = "")
})

apply(doc13, 1, function(x) {
  cat("#' @param",x[1],x[2],"\n")
})


doc5 <- all_doc[[5]]

doc5$X1 <- sub("Type.*$","",doc5$X1) |> trimws()
doc5$X2 <- sub("More details.*$ | See: .*$","",doc5$X2) |> trimws()

apply(doc5, 1, function(x) {
  cat(x[1]," = ",x[1],",\n",sep = "")
})

apply(doc5, 1, function(x) {
  cat("#' @param",x[1],x[2],"\n")
})

doc6 <- all_doc[[6]]

doc6$X1 <- sub("Type.*$","",doc6$X1) |> trimws()
doc6$X2 <- sub("More details.*$ | See: .*$","",doc6$X2) |> trimws()

apply(doc6, 1, function(x) {
  cat(x[1]," = ",x[1],",\n",sep = "")
})

apply(doc6, 1, function(x) {
  cat("#' @param",x[1],x[2],"\n")
})

doc18 <- rbind(all_doc[[17]],all_doc[[18]])

doc18$X1 <- sub("Type.*$","",doc18$X1) |> trimws()
doc18$X2 <- sub("More details.*$ | See: .*$","",doc18$X2) |> trimws()

apply(doc18, 1, function(x) {
  cat(x[1]," = ",x[1],",\n",sep = "")
})

apply(doc18, 1, function(x) {
  cat("#' @param",x[1],x[2],"\n")
})
