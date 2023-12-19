import Data.List (intercalate)

-- Bubble Sort

bubbleSort :: (Ord a, Show a) => [a] -> Bool -> IO ()
bubbleSort arr asc = do
    putStrLn $ "Original Array: " ++ show arr
    putStrLn $ "Sorting Order: " ++ if asc then "Ascending" else "Descending"
    putStrLn "Sorting Process:"
    printIterationsBubble arr asc 0

printIterationsBubble :: (Ord a, Show a) => [a] -> Bool -> Int -> IO ()
printIterationsBubble arr asc iter = do
    let (newArr, swapped) = iteratePass arr asc
    putStrLn $ "Iteration " ++ show (iter + 1) ++ ": " ++ intercalate ", " (map show newArr)
    if swapped
        then printIterationsBubble newArr asc (iter + 1)
        else putStrLn $ "Sorted Array: " ++ intercalate ", " (map show newArr)
  where
    iteratePass arr' asc' = bubblePass arr' asc' False
    bubblePass :: (Ord a) => [a] -> Bool -> Bool -> ([a], Bool)
    bubblePass [] _ swapped = ([], swapped)
    bubblePass [x] _ swapped = ([x], swapped)
    bubblePass (x:y:xs) asc''' swapped
        | shouldSwap x y asc''' = let (sortedTail, isSwapped) = bubblePass (x:xs) asc''' True
                                  in (y : sortedTail, isSwapped)
        | otherwise = let (sortedTail, isSwapped) = bubblePass (y:xs) asc''' swapped
                      in (x : sortedTail, isSwapped)
    shouldSwap a b True = a > b
    shouldSwap a b False = a < b

-- Insertion Sort

insertionSort :: (Ord a, Show a) => [a] -> Bool -> IO () 
insertionSort arr asc = do
    putStrLn $ "Original Array: " ++ show arr
    putStrLn $ "Sorting Order: " ++ if asc then "Ascending" else "Descending"
    sorted <- insertionPass arr asc 1
    putStrLn $ "Sorted Array: " ++ intercalate ", " (map show sorted)

insertionPass :: (Ord a, Show a) => [a] -> Bool -> Int -> IO [a]
insertionPass arr _ i | i >= length arr = return arr
insertionPass arr asc i = do
    let (left, right) = splitAt i arr
        current = head right
        sortedLeft = insertion current left asc
        sorted = sortedLeft ++ tail right
    putStrLn $ "Iteration " ++ show i ++ ": " ++ intercalate ", " (map show sorted)
    insertionPass sorted asc (i + 1)

insertion :: Ord a => a -> [a] -> Bool -> [a]
insertion x [] _ = [x]
insertion x (y:ys) asc
    | asc && x <= y = x : y : ys
    | not asc && x >= y = x : y : ys
    | otherwise = y : insertion x ys asc

-- Selection Sort

selectionSort :: (Ord a, Show a) => [a] -> Bool -> IO ()
selectionSort arr asc = do
    putStrLn $ "Original Array: " ++ show arr
    putStrLn $ "Sorting Order: " ++ if asc then "Ascending" else "Descending" 
    putStrLn "Sorting Process:"
    printIterationsSelection arr asc 0

printIterationsSelection :: (Ord a, Show a) => [a] -> Bool -> Int -> IO ()
printIterationsSelection arr asc iter
    | iter >= length arr = putStrLn $ "Sorted Array: " ++ intercalate ", " (map show arr)
    | otherwise = do
        let (newArr, minIndex) = iteratePass arr iter asc
        putStrLn $ "Iteration " ++ show (iter + 1) ++ ": " ++ intercalate ", " (map show newArr)
        if minIndex /= iter
            then do
                let swappedArr = swapElements newArr iter minIndex
                printIterationsSelection swappedArr asc (iter + 1)
            else printIterationsSelection newArr asc (iter + 1)

iteratePass :: Ord a => [a] -> Int -> Bool -> ([a], Int)
iteratePass arr start asc = foldr (findMin asc) (arr, start) [start..length arr - 1]
  where
    findMin asc i (arr', minIndex)
        | asc && current < minValue = (arr', i)
        | not asc && current > minValue = (arr', i)
        | otherwise = (arr', minIndex)
      where
        current = arr' !! i
        minValue = arr' !! minIndex

swapElements :: [a] -> Int -> Int -> [a]
swapElements arr i j
    | i /= j = take i merged ++ [arr !! j] ++ middle ++ [arr !! i] ++ drop (j + 1) merged
    | otherwise = arr
  where
    merged = mergeLists arr
    middle = take (j - i - 1) $ drop (i + 1) merged

mergeLists :: [a] -> [a]
mergeLists = concatMap (\x -> [x])

-- Quick Sort

qsortWithSteps :: (Ord a, Show a) => [a] -> Bool -> IO [a]
qsortWithSteps [] _ = return []
qsortWithSteps [x] _ = return [x]
qsortWithSteps (x:xs) asc = do
    let smaller = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
        sorted =
            if asc
                then smaller ++ [x] ++ larger
                else larger ++ [x] ++ smaller
    putStrLn $ "Step: Smaller: " ++ show smaller ++ ", Pivot: " ++ show x ++ ", Larger: " ++ show larger
    sortedSmaller <- qsortWithSteps smaller asc
    sortedLarger <- qsortWithSteps larger asc
    return $ sortedSmaller ++ [x] ++ sortedLarger

quickSort :: (Ord a, Show a) => [a] -> Bool -> IO ()
quickSort arr asc = do
    putStrLn $ "Original Array: " ++ show arr
    putStrLn $ "Sorting Order: " ++ if asc then "Ascending" else "Descending"
    putStrLn "Sorting Process:"
    sortedArr <- qsortWithSteps arr asc
    putStrLn $ "Sorted Array: " ++ intercalate ", " (map show sortedArr)


-- Merge function
merge :: Ord a => Bool -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge asc (x:xs) (y:ys)
  | asc && x <= y = x : merge asc xs (y:ys)
  | not asc && x >= y = x : merge asc xs (y:ys)
  | otherwise = y : merge asc (x:xs) ys

-- Function to split the list into two halves
split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs

-- Merge Sort function with logging iterations
mergeSort :: (Ord a, Show a) => [a] -> Bool -> IO [a]
mergeSort xs asc = mergeSort' xs 1
  where
    mergeSort' :: (Ord a, Show a) => [a] -> Int -> IO [a]
    mergeSort' ys iter
      | length ys <= 1 = do
          putStrLn $ "Iteration " ++ show iter ++ ": " ++ show ys
          return ys
      | otherwise = do
          let (left, right) = split ys
          putStrLn $ "Iteration " ++ show iter ++ " - Left: " ++ show left ++ " | Right: " ++ show right
          sortedLeft <- mergeSort' left (iter + 1)
          sortedRight <- mergeSort' right (iter + 1)
          let merged = merge asc sortedLeft sortedRight  -- Pass the 'asc' parameter to the merge function
          putStrLn $ "Iteration " ++ show iter ++ ": " ++ show merged
          return merged


main :: IO ()
main = do

    putStrLn "Choose an algorithm:"
    putStrLn "[1] Selection Sort"
    putStrLn "[2] Bubble Sort"
    putStrLn "[3] Insertion Sort"
    putStrLn "[4] Merge Sort"
    putStrLn "[5] Quick Sort"
    putStr "Enter your choice: "
    
    choice <- getLine
    let sortAlgorithm = case choice of
                            "1" -> bubbleSort
                            "2" -> insertionSort
                            "3" -> selectionSort
                            "4" -> quickSort
                            "5" -> (\arr asc -> mergeSort arr asc >>= \sortedArr -> putStrLn $ "Sorted Array: " ++ intercalate ", " (map show sortedArr))
                            _ -> bubbleSort
    
    putStrLn "Enter sorting order (1 for Ascending, 0 for Descending):"
    sortOrder <- getLine
    let asc = sortOrder == "1"
    
    arr <- readIntList
    
    sortAlgorithm arr asc
    
    putStrLn "Do you want to continue? (Type 'no' to exit or any other key to continue)"
    continue <- getLine
    if continue /= "no"
        then main
        else putStrLn "Closing the program..."

-- Function to read a list of integers from user input
readIntList :: IO [Int]
readIntList = do
    putStrLn "Enter a list of integers separated by spaces:"
    input <- getLine
    let parsed = map read $ words input :: [Int]
    return parsed