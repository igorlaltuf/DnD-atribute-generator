# DnD 5e Atribute Generator
# Load libraries
library(dplyr)

print('
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@************@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@*******************(@@@@@@@@@@@******@@@@*****@@(*******************@@@@@@@@@@@
@@@***********************%@@@@@*@***@@@%@@****@@@@************************@@@@@
@@@@@@***********************@@@*@&@(/*@*@@****@@@@@@@***********************@@@
@@@@@@********@@@@@@@**********@/*****@***(*@&@@@@@@@@*@*@@@@/@@@@@@@**********@
@@@@@@********@@@@@@@@@******@@***************@@@@@@@*@*********(@@@@@@*********
@@@@@@********@@@@@@@@@@**/@@#@@***(@**********@&@@*@***@@@@&@@@@@@@@@@@********
@@@@@@********@@@@@@@@@@****@**@@**@@*@@*@*%@@**@@***@%*******@@@@@@@@@@********
@@@@@@********@@@@@@@@@****@********@@@***@@**%@@@**@@********@@@@@@@@@*********
@@@@@@********@@@@@@@*****@@*******/@@@@@@@@@******@@@**@@@@@@@@@@@@@**********@
@@@@@@*********************@********@@@@@@@@@**@**********@*@***@************@@@
@@@@@@********************@@*********@@@@@******#@*******@@******@********(@@@@@
@@******************&@@@@@@@@******************@@@**************@***%@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@/**********@@@@@@@*@@******@@@@@@@@@@@@@@@@@@@@
')

# Input
print('Choose the character name:') 
char.name <- readline()

print('Choose the character class:')
char.class <- readline()

# Store dices values
diced4 <- 1:4 
diced6 <- 1:6
diced8 <- 1:8 
diced10 <- 1:10 
diced12 <- 1:12
diced20 <- 1:20

# Number of times the dice is rolled
nrows <- 1 

# Function to Row dices
roll_dice <- function(diced6,nrows) {
  sample(x = diced6, replace= TRUE, size = nrows)
} 

# Create vector to store dice rolls
dice.rolls <- c(replicate(6,0)) 

# Reroll function
roll_again <- function() {

# Roll 4d6 dices and put away the lower dice roll for each character stat.
for (i in 1:6) {
  dice.rolls[i] <- roll_dice(diced6,4) %>% 
    sort(decreasing=TRUE) %>% 
    head(-1) %>%
    sum()
}

# Sort and store the values in decreasing order
dice.rolls <- sort(dice.rolls, decreasing = TRUE) 

# Identify the primary and the secondary stats of each class
classes_table <- tibble(
  classes   = c('barbarian','bard','cleric', 'druid', 'fighter', 'monk', 'paladin', 'ranger', 'rogue', 'sorcerer', 'warlock', 'wizard'),
  atrib.prim = c('con','cha','wis', 'wis','str', 'dex', 'cha', 'str', 'dex', 'con', 'int', 'int'),
  atrib.sec = c('str','int','cha','dex','con','wis', 'str', 'dex', 'cha', 'int','con', 'wis')
) %>% dplyr::filter(classes == char.class)
  
class.prim <- classes_table$atrib.prim
class.sec <- classes_table$atrib.sec

# Create vector to store the atributes and their names
all.atributes <-  c('str','dex','con','int','wis','cha')
atributes.values <-  c(replicate(6,0))


# Logical vector to show the position of the main atribute of each class
lv.prim <- class.prim == all.atributes
main.pos <- which(lv.prim == TRUE)
atributes.values[main.pos] <- nth(dice.rolls,1)

# Logical vector to show the position of the secondary atribute of each class
lv.sec <- class.sec == all.atributes
sec.pos <- which(lv.sec == TRUE)
atributes.values[sec.pos] <- nth(dice.rolls,2)

# Logical vector to show the position of non main atributes
lv <- class.prim == all.atributes | class.sec == all.atributes 
non.main.pos <- which(lv == FALSE) 

# Make the others atributes always random between then
rand.pos <- sample(non.main.pos, replace= FALSE)
atributes.values[rand.pos[1]] <- nth(dice.rolls,3)
atributes.values[rand.pos[2]] <- nth(dice.rolls,4)
atributes.values[rand.pos[3]] <- nth(dice.rolls,5)
atributes.values[rand.pos[4]] <- nth(dice.rolls,6)

# Print Character atributes sheet
char_table <- tibble(
  atribute = all.atributes,
  values = atributes.values
)

print(char.name)
print(char.class)
print(char_table)

}

print('Roll the dices again?[Y or N]')
answer <- readline()
ifelse(answer=='Y', roll_again(),print('Ok, have a nice game'))