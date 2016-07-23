--  @author: Justin Lee, Alec Knutsen
--  @filename: solver.hs
--  @Date: 2016/04/30
-- 	@about: This program uses a brute force method to solve a lotus sudoku puzzle

--Ring indices
r1 = [0,1,2,3,4,5,6]
r2 = [7,8,9,10,11,12,13]
r3 = [14,15,16,17,18,19,20]
r4 = [21,22,23,24,25,26,27]
r5 = [28,29,30,31,32,33,34]
r6 = [35,36,37,38,39,40,41]
r7 = [42,43,44,45,46,47,48]

-- clockwise spirals indices(starting outward working inward)
cw1 = [0,7,15,22,30,37,45]
cw2 = [1,8,16,23,31,38,46]
cw3 = [2,9,17,24,32,39,47]
cw4 = [3,10,18,25,33,40,48]
cw5 = [4,11,19,26,34,41,42]
cw6 = [5,12,20,27,28,35,43]
cw7 = [6,13,14,21,29,36,44]

-- counter-clockwise spirals indices (starting outward working inward)
ccw1 = [0,13,20,26,33,39,46]
ccw2 = [6,12,19,25,32,38,45]
ccw3 = [5,11,18,24,31,37,44]
ccw4 = [4,10,17,23,30,36,43]
ccw5 = [3,9,16,22,29,35,42]
ccw6 = [2,8,15,21,28,41,48]
ccw7 = [1,7,14,27,34,40,47]

--Finds first empty cell 
--	@pre: A valid sudoku list of values is passed in
--	@post: Recursively search the Int list until it reaches a 0. If there are no empty spots returns (-10) 
--	@return: the integer of the index of the first empty cell or (-10) if no cell is empty 
findEmpty:: [Int] -> Int -> Int
findEmpty lotus_array index 
  | index <=48 = if lotus_array!!index == 0 then index else findEmpty lotus_array (index+1)
  | index > 48 = -10
  
  
--	@pre: valid index, value, current lotus list, list of row indices, list of clockwise spiral indices, and list of counter-clockwise indices are passed in
--	@post: Determines if the value inputted at a certain index is valid, if the value at the index in the current lotus list violates any of the 3 conditions, return false, else recurse on the next index of each of the 3 conditions
--	@return: true if the value at an index in the current array doesn't violate any of the three conditions, else false
validMove :: Int -> Int -> [Int] -> Int -> Int -> Int -> Bool
validMove index value lotus_array r_arr_index cw_arr_index ccw_arr_index 
  |index == 0 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 0 && r_arr_index > 6 = True
  |index == 1 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 1 && r_arr_index > 6 = True
  |index == 2 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 2 && r_arr_index > 6 = True
  |index == 3 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 3 && r_arr_index > 6 = True
  |index == 4 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 4 && r_arr_index > 6 = True
  |index == 5 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 5 && r_arr_index > 6 = True
  |index == 6 && r_arr_index<=6 = if(lotus_array!!(r1!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 6 && r_arr_index > 6 = True
  |index == 7 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 7 && r_arr_index > 6 = True
  |index == 8 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 8 && r_arr_index > 6 = True
  |index == 9 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 9 && r_arr_index > 6 = True
  |index == 10 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 10 && r_arr_index > 6 = True
  |index == 11 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 11 && r_arr_index > 6 = True
  |index == 12 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 12 && r_arr_index > 6 = True
  |index == 13 && r_arr_index<=6 = if(lotus_array!!(r2!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 13 && r_arr_index > 6 = True
  |index == 14 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 14 && r_arr_index > 6 = True
  |index == 15 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 15 && r_arr_index > 6 = True
  |index == 16 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 16 && r_arr_index > 6 = True
  |index == 17 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 17 && r_arr_index > 6 = True
  |index == 18 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 18 && r_arr_index > 6 = True
  |index == 19 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 19 && r_arr_index > 6 = True
  |index == 20 && r_arr_index<=6 = if(lotus_array!!(r3!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 20 && r_arr_index > 6 = True
  |index == 21 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 21 && r_arr_index > 6 = True
  |index == 22 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 22 && r_arr_index > 6 = True
  |index == 23 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 23 && r_arr_index > 6 = True
  |index == 24 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 24 && r_arr_index > 6 = True
  |index == 25 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 25 && r_arr_index > 6 = True
  |index == 26 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 26 && r_arr_index > 6 = True
  |index == 27 && r_arr_index<=6 = if(lotus_array!!(r4!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 27 && r_arr_index > 6 = True
  |index == 28 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 28 && r_arr_index > 6 = True
  |index == 29 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 29 && r_arr_index > 6 = True
  |index == 30 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 30 && r_arr_index > 6 = True
  |index == 31 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 31 && r_arr_index > 6 = True
  |index == 32 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 32 && r_arr_index > 6 = True
  |index == 33 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 33 && r_arr_index > 6 = True
  |index == 34 && r_arr_index<=6 = if(lotus_array!!(r5!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 34 && r_arr_index > 6 = True
  |index == 35 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 35 && r_arr_index > 6 = True
  |index == 36 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 36 && r_arr_index > 6 = True
  |index == 37 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 37 && r_arr_index > 6 = True
  |index == 38 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 38 && r_arr_index > 6 = True
  |index == 39 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 39 && r_arr_index > 6 = True
  |index == 40 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 40 && r_arr_index > 6 = True
  |index == 41 && r_arr_index<=6 = if(lotus_array!!(r6!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 41 && r_arr_index > 6 = True
  |index == 42 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw5!!cw_arr_index) == value || lotus_array!!(ccw5!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 42 && r_arr_index > 6 = True
  |index == 43 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw6!!cw_arr_index) == value || lotus_array!!(ccw4!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 43 && r_arr_index > 6 = True
  |index == 44 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw7!!cw_arr_index) == value || lotus_array!!(ccw3!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 44 && r_arr_index > 6 = True
  |index == 45 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw1!!cw_arr_index) == value || lotus_array!!(ccw2!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 45 && r_arr_index > 6 = True
  |index == 46 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw2!!cw_arr_index) == value || lotus_array!!(ccw1!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 46 && r_arr_index > 6 = True
  |index == 47 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw3!!cw_arr_index) == value || lotus_array!!(ccw7!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 47 && r_arr_index > 6 = True
  |index == 48 && r_arr_index<=6 = if(lotus_array!!(r7!!r_arr_index) == value || lotus_array!!(cw4!!cw_arr_index) == value || lotus_array!!(ccw6!!ccw_arr_index) == value) then False else validMove index value lotus_array (r_arr_index+1) (cw_arr_index+1) (ccw_arr_index+1) 
  |index == 48 && r_arr_index > 6 = True
  |otherwise = False
  
 --	@pre: called from lotusSolver function, the current lotus array, index, and number to enter are passed in
--	@post: calls valid move function and if the move is valid, then recurse and add the number to enter to the list until values 1-7 are tried
--	@return: A list of Ints that are all of the valid values that might be put into the lotus_array at a partiular index
getValidMoves:: [Int] -> Int -> Int -> [Int]
getValidMoves lotus_array index_to_enter_in num_to_enter 
  | num_to_enter <= 7 = if validMove index_to_enter_in num_to_enter lotus_array 0 0 0 then num_to_enter:(getValidMoves lotus_array index_to_enter_in (num_to_enter +1)) else getValidMoves lotus_array index_to_enter_in (num_to_enter +1)
  | num_to_enter > 7 = []
  
--	@pre: Called from solver function, the current lotus array, an index,  the current value at the index, and the number to put in at that index are passed in
--	@post: recursively called 
--	@return: A list of Ints that are all of the valid values that might be put into the index partiular index minus the moves that have already been used
getValidMovesMinusUsed:: [Int] -> Int -> Int -> Int -> [Int]
getValidMovesMinusUsed lotus_array current_val index_to_enter_in num_to_enter 
  | num_to_enter <= 7 && num_to_enter <= current_val = getValidMovesMinusUsed lotus_array current_val index_to_enter_in (num_to_enter +1)
  | num_to_enter <= 7 && num_to_enter > current_val= if validMove index_to_enter_in num_to_enter lotus_array 0 0 0 then num_to_enter:(getValidMovesMinusUsed lotus_array current_val index_to_enter_in (num_to_enter +1)) else getValidMovesMinusUsed lotus_array current_val index_to_enter_in (num_to_enter +1)
  | num_to_enter > 7 = []
  
--	@pre: a valid list of Ints, index on the list, and value are passed in 
--	@post: the function modifies the Int list to include the specified value at the specified index
--	@return: The Int list with the new value inserted 
modify:: [Int] -> Int -> Int -> [Int] 
modify lotus_array index value = (take (index) lotus_array)  ++ (value:(drop (index+1) lotus_array))


--	@pre: called from goBack function and the original list, current list, and index are passed in 
--	@post: Gets the max index for which two array indices have different values
--	@return: returns the index 
findMaxDifferenceIndex:: [Int] -> [Int] -> Int -> Int
findMaxDifferenceIndex xs ys index
  | index<0 = -5
  | index>=0 = if last xs /= last ys then (index) else findMaxDifferenceIndex (take index xs) (take index ys) (index-1)

--	@pre:the current lotus list and original are passed in 
--	@post: Calls findMaxDifferenceIndex function to modify the array passed in as the input at the index to be 0 (i.e. make an empty cell for the most recently change cell)
--	@return: the updated current sudoku list (with the newly inserted empty cell)
goBack:: [Int] -> [Int] -> [Int]
goBack lotus_array testboard = if findMaxDifferenceIndex testboard lotus_array ((length testboard) -1) /= -5 then (take (findMaxDifferenceIndex testboard lotus_array ((length testboard) -1)) lotus_array) ++ (0:(drop ((findMaxDifferenceIndex testboard lotus_array ((length testboard) -1))+1) lotus_array)) else lotus_array
 
--	@pre: called from lotusSolver and a valid lotus list is passed in, a list of ints for the testboard, an Int representing the current index and a list of ints for indices of future moves
--	@post: recursively calls itself to move through the indices, and either call the goBack (to recurse backwards) function or modify function (to recurse forward)
--	@return: the Int list representing the solved board, or the empty list if the board is not solvable
solver::[Int] -> [Int] -> Int -> [Int] -> [Int] 
solver lotus_array testboard current_index next_move_arr
  |current_index < 0 = []
  |current_index > 48 = lotus_array
  |current_index < 48  && next_move_arr == [] = solver (goBack lotus_array testboard) testboard (findMaxDifferenceIndex testboard lotus_array ((length testboard)-1)) (getValidMovesMinusUsed lotus_array (lotus_array!!(findMaxDifferenceIndex testboard lotus_array ((length testboard)-1))) (findMaxDifferenceIndex testboard lotus_array ((length testboard)-1)) 1)
  |current_index < 48 &&  next_move_arr /=[]  = solver (modify lotus_array (findEmpty lotus_array 0) (next_move_arr!!0)) testboard (findEmpty (modify lotus_array (findEmpty lotus_array 0) (next_move_arr!!0)) 0) (getValidMoves (modify lotus_array (findEmpty lotus_array 0) (next_move_arr!!0)) (findEmpty (modify lotus_array (findEmpty lotus_array 0) (next_move_arr!!0)) 0) 1)
  |current_index == 48 &&  next_move_arr ==[] = solver (goBack lotus_array testboard) testboard (findMaxDifferenceIndex testboard lotus_array ((length testboard)-1)) (getValidMovesMinusUsed lotus_array (lotus_array!!(findMaxDifferenceIndex testboard lotus_array ((length testboard)-1))) (findMaxDifferenceIndex testboard lotus_array ((length testboard)-1)) 1)
  |current_index == 48 &&  next_move_arr /=[] = modify lotus_array (findEmpty lotus_array 0) (next_move_arr!!0)

--	@pre: A valid lotus sudoku list of Ints is passed in.  
--	@post: The lotus list is solved and stored in an Int list
--	@return: The solved lotus list, from the solver function
lotusSolver::[Int] -> [Int]
lotusSolver testboard = solver testboard testboard (findEmpty testboard 0) (getValidMoves testboard (findEmpty testboard 0) 1)

--Example Input Board
testboard = [0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]


main = do
  putStrLn ("Board Inputted: " ++ (show  testboard))
  putStrLn ("Solution: " ++ (show (lotusSolver testboard)))
 
