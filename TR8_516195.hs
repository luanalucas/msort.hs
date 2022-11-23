msort :: [Int] -> [Int]
msort = \list ->
    case list of
        [] -> []
        [x] -> [x]
        _ ->
            let (evens, odds) = split list
            in unir (msort evens) (msort odds) 

split :: [Int] -> ([Int], [Int])
split = \list -> 

  case list of
      [] -> ([], [])
      x:xs -> 
          let (odds, evens) = split xs
          in (x:evens, odds)

unir :: [Int] -> [Int] -> [Int]
unir = \a -> \b ->
    case a of
        [] -> b
        x:xs ->
            case b of
                [] -> a
                c:c1 | x>c -> c: unir a c1
                _ -> x: unir xs b

main = print (msort [777, 654, 321, 8910])
