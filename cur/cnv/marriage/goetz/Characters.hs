import Model

main = do
	putStr $ concat $ map (\(x,y) -> unlines [x, show y]) characters

