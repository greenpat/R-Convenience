
# diceroll ----------------------------------------------------------------

# This project is a simple 3-sided dice roller with fun graphical output

# quick flash animation for all 0's or all 2's
jackpot <- function(){
    
    # Either red or green
    if(heat<1){jpcol<-rgb(.6,0,0)}
    if(heat>1){jpcol<-rgb(0,.6,0)}
    
    # Flash a big X three times
    for (i in 1:3){
    abline(0,1,lwd=80, col=jpcol)
    abline(1,-1,lwd=80, col=jpcol)
    
    # First two flashes pause and wipe screen
    Sys.sleep(.15)
    if(i<3){plot.new();Sys.sleep(.15)}
    }
}

# Run the loop
while(TRUE){
    # Prompt
    cat("\n\n")
    n = as.numeric(readline("How many dice?: "))
    roll = sample(c(0,1,2),n,replace=TRUE)
    
    # Intentional lag
    plot.new(); Sys.sleep(.3)
    
    # Text output
    cat("\nThe die settle:\n")
    paste0('[',roll,']', collapse = ' ')
    cat("\n\nYou rolled:\n")
    cat(">>>",as.character((sum(roll))),"<<<")
    
    # Calculate a color based value of dice
    heat <- sum(roll)/(n)
    if(heat<1){heatcol<-rgb(-(heat-1)*.75+.25,0,0)}
    if(heat>1){heatcol<-rgb(0,(heat-1)*.75+.25,0)}
    if(heat==1){heatcol<-rgb(0,0,0)}
    
    # Generate plot
    plot.new()
    if(heat==0|heat==2){jackpot()}
    text(.5,.9,paste0("[",roll,"]",collapse = " "),cex=3)
    text(.52,.38,as.character(sum(roll)), cex=20, col='gray50')  # shadow
    text(.5,.4,as.character(sum(roll)), cex=20, col=heatcol)
}