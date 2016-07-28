# start pavian in the background
system('R -e "pavian::runApp(port=5000)"', wait=FALSE)

DISPLAY = FALSE

#install.packages("RSelenium")
RSelenium::checkForServer()
RSelenium::startServer()
require(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")
remDr$open()
remDr$getStatus()
remDr$navigate("http://www.google.com")
remDr$navigate("http://127.0.0.1:5000")
remDr$setWindowSize(900,600)
remDr$screenshot(display = DISPLAY, useViewer = FALSE, file="main-page.png")


btn_load_example <- remDr$findElement(using="css selector","#datafile-btn_load_example")
btn_load_example$clickElement()
remDr$setWindowSize(900,1000)
remDr$screenshot(display = DISPLAY, useViewer = FALSE, file="load-data-set.png")

menu_results <- remDr$findElement(using="css selector","#dy_menu_overview > a:nth-child(1)")
menu_results$clickElement()
remDr$setWindowSize(1200,600)
remDr$screenshot(display = DISPLAY, useViewer = FALSE, file="results-overview.png")

menu_comp <- remDr$findElement(using="css selector","#dy_menu_comp > a:nth-child(1)")
menu_comp$clickElement()
menu_comp1 <- remDr$findElement(using="css selector",".treeview-menu > li:nth-child(1) > a:nth-child(1)")
menu_comp1$clickElement()
remDr$setWindowSize(1200,1000)
remDr$screenshot(display = DISPLAY, file="menu-comp.png")

heatmap_tab <- remDr$findElement(using="css selector","#shiny-tab-Comparison > div:nth-child(3) > div:nth-child(1) > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)")
heatmap_tab$clickElement()
remDr$screenshot(display = DISPLAY, file="comp-heatmap.png")
system("convert comp-heatmap.png -crop 520x280+260+370 comp-heatmap1.png")

menu_sample <- remDr$findElement(using="css selector","#dy_menu_sample > a:nth-child(1)")
menu_sample$clickElement()
#remDr$findElement(using="css selector","#shiny-tab-Sample > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(1)")$clickElement()
#input_sample <- remDr$findElement(using="css selector","#shiny-tab-Sample > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(1) > input:nth-child(2)")
remDr$setWindowSize(1200,1000)
remDr$screenshot(display = DISPLAY, file="flow-pt1.png")

## TODO: Select PT5 by hand
remDr$screenshot(display = DISPLAY, file="flow-pt5.png")


## TODO: Click sunburst, screenshot

## TODO: Click alignment viewer - show alignment pileup - select area
remDr$screenshot(display = DISPLAY, file="alignment_viewer-pt5.png")
