# start pavian in the background
system('R -e "pavian::runApp(port=5004)"', wait=FALSE)

DISPLAY = FALSE

setwd("~/projects/pavian/vignettes")
library(RSelenium)

#install.packages("RSelenium")
#RSelenium::checkForServer()
#RSelenium::startServer()
#selSrv <- startServer(invisible = FALSE, log = FALSE , args = c("-port 3333"))
## run Selenium with 'java -jar /usr/share/selenium-server/selenium-server-standalone.jar -port 6656 -debug' in separate terminal
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 6656, browserName = "chrome")
remDr$open()
remDr$getStatus()
remDr$navigate("http://www.google.com")
remDr$navigate("http://127.0.0.1:5004")
remDr$setWindowSize(900,650)
remDr$screenshot(display = DISPLAY, useViewer = FALSE, file="main-page.png")

remDr$setWindowSize(900,1000)
btn_load_example <- remDr$findElement(using="css selector","#datafile-btn_load_server_dir")
btn_load_example$clickElement()

remDr$screenshot(display = DISPLAY, useViewer = FALSE, file="load-data-set.png")

system("convert load-data-set.png -crop 180x230+0+180 side-bar.png")

menu_results <- remDr$findElement(using="css selector","#dy_menu_overview > a:nth-child(1)")
menu_results$clickElement()
# ... wait until samples are loaded
## hide menu bar
remDr$setWindowSize(1000,650)
remDr$screenshot(display = DISPLAY, useViewer = FALSE, file="results-overview.png")
## show menu bar

menu_comp <- remDr$findElement(using="css selector","#dy_menu_comp > a:nth-child(1)")
menu_comp$clickElement()
menu_comp1 <- remDr$findElement(using="css selector",".treeview-menu > li:nth-child(1) > a:nth-child(1)")
menu_comp1$clickElement()
remDr$setWindowSize(1200,1000)
# wait
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

## Click sunburst, screenshot
#remDr$findElement(using="css selector","div.tabbable:nth-child(1) > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)")$clickElement()
# TODO: Screenshot

## Click alignment viewer - show alignment pileup - select area
remDr$findElement(using="css selector","#tabs > li:nth-child(5) > a:nth-child(1)")$clickElement()
remDr$findElement(using="css selector","#alignment-btn_get_alignment")$clickElement()
## TODO: select region of the genome
remDr$screenshot(display = DISPLAY, file="alignment_viewer-pt5.png")

## Download genomes
remDr$findElement(using="css selector","div.tabbable:nth-child(2) > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)")$clickElement()
## TODO: Select viral and 'Get assembly info', search for JC Pol and select table entry
remDr$setWindowSize(1200,800)
remDr$screenshot(display = DISPLAY, file="download-genome-jcv.png")
