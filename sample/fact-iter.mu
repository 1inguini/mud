fun factIter : Int -> Int -> Int -> Int = {
  product counter maxCount -> if counter > maxCount 
    then product
    else factIter (counter * product) (counter+1) maxCount;
}

fun fact : Int -> Int = n -> { factIter 1 1 n; };

puts (fact 6);