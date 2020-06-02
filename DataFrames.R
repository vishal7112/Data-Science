myCars <- mtcars
myCars
"Steop 1"
"1- Lower Hp is best bercause you get more mielage"
"2= Ford Panters L"

"Step 2"
max(myCars$mpg) 
myCars[which.max(myCars$mpg),]
NewMpg <- myCars[order(myCars$mpg, decreasing=T),]
NewMpg

"Step 3" 
myCars[which.max(myCars$mpg/myCars$hp),] 
"Comparing the highest ratio of Mpg to Hp"
MpgtoHp <- myCars$mpg/myCars$hp
myCars[order(MpgtoHp),]
myCars[which.max(MpgtoHp),]

"Step 4" 
NewMpg <- myCars$mpg/33.9
NewMpg <- myCars$mpg/max(myCars$mpg) 

NewHp <- myCars$hp/max(myCars$hp)
NewMpgToHp <- NewMpg+NewHp
myCars[which.max(NewMpgToHp),]
