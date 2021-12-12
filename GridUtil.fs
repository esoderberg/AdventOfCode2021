module GridUtil

type Grid<'a> = 'a[][]

//Bound returned as xmax, ymax assumes rectangular grid
let GridBounds (grid:'a[][]) = (grid.[0].Length-1, grid.Length-1)

let IsCellInGrid (grid:'a[][]) (x,y) = 
    let (xmax, ymax) = GridBounds grid
    0 <= x && x <= xmax && 0 <= y && y <= ymax

// Gets cells in a plus centered on x,y (x,y not included)
// If a cell is out of bounds it is not included
let GetCellPlus (grid:'a[][]) (x,y) = List.filter (IsCellInGrid grid) [(x,y-1);(x+1,y);(x,y+1);(x-1,y)]

// Gets cells in a 1-length square centered on x,y (x,y not included)
// If a cell is out of bounds it is not included
let GetCellSquare (grid:'a[][]) (x,y)= List.filter (IsCellInGrid grid) [(x,y-1);(x+1, y-1);(x+1,y);(x+1,y+1);(x,y+1);(x-1,y+1);(x-1,y);(x-1,y-1)]