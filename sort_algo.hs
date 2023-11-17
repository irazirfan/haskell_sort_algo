import Data.List (sort)

-- Sorting algorithms

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = let
  smallest = minimum (x:xs)
  in smallest : selectionSort (filter (/= smallest) (x:xs))

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = sort

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert :: Ord a => a -> [a] -> [a]
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y:z:zs
      | otherwise = z : insert y zs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

-- User interface functions

displayMenu :: IO ()
displayMenu = do
  putStrLn "\nSorting Algorithm Simulation"
  putStrLn "1. Selection Sort"
  putStrLn "2. Bubble Sort"
  putStrLn "3. Insertion Sort"
  putStrLn "4. Merge Sort"
  putStrLn "5. Quick Sort"

getAlgorithmChoice :: IO Int
getAlgorithmChoice = do
  putStr "Choose a sorting algorithm (1-5): "
  choice <- getLine
  return (read choice :: Int)

getOrderChoice :: IO Int
getOrderChoice = do
  putStrLn "Choose sorting order:"
  putStrLn "1. Ascending"
  putStrLn "2. Descending"
  putStr "Enter your choice (1 or 2): "
  choice <- getLine
  return (read choice :: Int)

getDefaultArray :: [Int]
getDefaultArray = [5, 2, 8, 1, 3]

getCustomArray :: IO [Int]
getCustomArray = do
  putStrLn "Enter custom array elements separated by spaces (maximum 5 elements):"
  input <- getLine
  let customArray = take 5 $ map read (words input) :: [Int]
  if length customArray /= length (words input)
    then do
      putStrLn "Error: Too many elements provided. Only the first 5 elements will be considered."
    else return ()
  return customArray

validateArrayRange :: [Int] -> Bool
validateArrayRange [] = False
validateArrayRange array = all (\x -> x >= 0 && x <= 100) array

main :: IO ()
main = do
  displayMenu
  algorithmChoice <- getAlgorithmChoice

  case algorithmChoice of
    1 -> simulateSort (selectionSort :: [Int] -> [Int])
    2 -> simulateSort (bubbleSort :: [Int] -> [Int])
    3 -> simulateSort (insertionSort :: [Int] -> [Int])
    4 -> simulateSort (mergeSort :: [Int] -> [Int])
    5 -> simulateSort (quickSort :: [Int] -> [Int])
    _ -> putStrLn "Invalid input. Please enter a number between 1 and 5."

simulateSort :: ([Int] -> [Int]) -> IO ()
simulateSort sortFunction = do
  orderChoice <- getOrderChoice
  let array = getDefaultArray
  let sortedArray = if orderChoice == 1 then sortFunction array else reverse (sortFunction array)

  putStrLn $ "\nDefault Array: " ++ show array
  putStrLn $ "Sorting in " ++ (if orderChoice == 1 then "Ascending" else "Descending") ++ " order..."
  putStrLn $ "Sorted Array: " ++ show sortedArray

  putStrLn "Do you want to input a custom array? (y/n)"
  customArrayOption <- getLine

  case customArrayOption of
    "y" -> do
      customArray <- getCustomArray
      let customSortedArray = if orderChoice == 1 then sortFunction customArray else reverse (sortFunction customArray)

      if validateArrayRange customArray
        then do
          putStrLn $ "\nCustom Array: " ++ show customArray
          putStrLn $ "Sorting in " ++ (if orderChoice == 1 then "Ascending" else "Descending") ++ " order..."
          putStrLn $ "Sorted Custom Array: " ++ show customSortedArray
        else putStrLn "Invalid input. Please provide a valid array in the specified range: arrays of size 5 within the range of 0 to 100."

    _ -> return ()

  putStrLn "Do you want to try another sorting algorithm? (y/n)"
  restartOption <- getLine

  if restartOption == "y"
    then main
    else putStrLn "Goodbye!"
