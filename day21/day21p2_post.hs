main :: IO ()
main = putStrLn . show . sol $ 26501365

sol :: Int -> Int
sol x1 = let x = x1 `div` 131
         in a+x*(b-a+(x-1)*(c-b-b+a)`div`2)

-- Extract from day21p2:
a, b, c :: Int
a = 3691 -- 65
b = 32975 -- 65 + 131
c = 91439 -- 65 + 131 + 131
