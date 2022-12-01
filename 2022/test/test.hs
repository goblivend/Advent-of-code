

facto :: Int -> Int
facto 1 = 1
facto n = n * facto (n-1)

main = print $ (facto 4)
