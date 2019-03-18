xor :: Bool -> Bool -> Bool
xor a b = ((not a) && b) || (a && (not b)) 

impl :: Bool -> Bool -> Bool
impl a b = (not a) || b

equiv :: Bool -> Bool -> Bool 
equiv a b = (impl a b) && (impl b a)