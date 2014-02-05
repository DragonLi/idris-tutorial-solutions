module UsefulDataTypes

-- lists, vectors, and related functions are in Idris standard library

-- dependent pairs

vec : (n ** Vect n Int)
vec = ( _ ** [3,4])
