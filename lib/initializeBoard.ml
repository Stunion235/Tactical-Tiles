open Board

let initialize_board difficulty size = 
  Array.make size (Array.make size (create_tile (-1)))


let fill_board currBoard = 
  let value = ref 1 in
for row = 0 to Array.length currBoard - 1 do 
  let rowLength = Array.length currBoard.(row) in 
  for col = 0 to rowLength - 1 do
    let currTile = create_tile !value in
    currBoard.(row).(col) <-  currTile;
    incr value
  done
done;
currBoard.(Array.length currBoard - 1).(Array.length currBoard - 1) <- create_tile (-1)


