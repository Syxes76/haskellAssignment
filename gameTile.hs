pascal :: [[Integer]]
pascal = repeat 1 : map (scanl1 (+)) pascal

main = do
    putStrLn $ unlines $ take 6 $ map (unwords . map show . take 10) $ pascal