// this is pseudocode for a simplified lexer
charsSeenSoFar = "in"
while(streamHasMoreCharacters){
nextChar = readCharFromStream()
if(nextChar == " "){
// we've found a token
switch(charsSeenSoFar):
case "in":
output "IN"
break
case: "int":
output "INT"
break
default:
output ID(charsSeenSoFar)
// not "in" or "int" keywords so must be an identifier
charSeenSoFar= "" // start matching another token
} else {
// longest match: we'll try to pattern match "int" next iteration
charSeenSoFar += nextChar
}
}
