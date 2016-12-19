# HOFI-project
Interactive visualizations of house finch mass data from the spring 2016 feeder study in the McGraw lab at ASU , using RShiny. 

#### How to use the RShiny app: 
1. Download this git repository using the green "Clone or Download" button to the right
2. Extract the files to the directory where you'd like to store / work with them. 
3. Download or access the data files (3 CSV files: `birdmass_long.csv, birdmass_wide.csv, tarsusMass.csv`) 
  (Available in the Feeder study Google Drive) Place these in the hofi-Shiny folder in the directory where you extracted this repository.
4. Open up all 3 R scripts from hofi-Shiny in RStudio (`ui.R, server.R, global.R`). 
Press the green "Run" play button, and an RShiny window should pop up. In this pop up, you can press "Open in Browser" for better viewing. 

*Note: You will need to install the `shiny`, `shinythemes`, `DT`, and `ggplot2` packages to run this app.*
