
#load "packages/FsLab/FsLab.fsx"
#load "bitmapStuff.fsx"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open bitmapStuff


let smallImage = scaledBitmap 400 "sudoku.jpg"

    
let convolve kernel m =
    let kernelXLength = Matrix.columnCount kernel
    let kernelYLength = Matrix.rowCount kernel
    if kernelYLength % 2 = 0 then failwith "height must be odd"
    if kernelXLength % 2 = 0 then failwith "width must be odd"
    let xMargin = (kernelXLength - 1) / 2
    let yMargin = (kernelYLength - 1) / 2
    let xLength = Matrix.columnCount m
    let yLength = Matrix.rowCount m
    m
    |> Matrix.mapi (fun y x v -> 
        match (x,y) with
        | (x,_) when x < xMargin               -> nan
        | (x,_) when xLength - xMargin - 1 < x -> nan
        | (_,y) when y < yMargin               -> nan
        | (_,y) when yLength - yMargin - 1 < y -> nan
        | _ -> 
            [
                for i= -yMargin to yMargin do
                    for j= -xMargin to xMargin do
                        yield m.[y+i,x+j] * kernel.[yMargin + i,xMargin + j]
                    done
                done
            ] |> List.sum
        )

let matrixMap2 mapping m1 m2 =
    if Matrix.columnCount m1 <> Matrix.columnCount m2 then failwith "Columns don't match"
    if Matrix.rowCount m1 <> Matrix.rowCount m2 then failwith "Rows don't match"
    
    Matrix.mapi (fun y x v ->
        mapping v m2.[y,x]) m1

let m = imageToMatrix smallImage

let newImg = matrixToImage m

let gaussian5 = matrix [[2.;4.;5.;4.;2.];[4.;9.;12.;9.;4.];[5.;12.;15.;12.;5.];[4.;9.;12.;9.;4.];[2.;4.;5.;4.;2.];] / 159.
let edgeV3 = matrix [[1.;2.;1.];[0.;0.;0.];[-1.;-2.;-1.]]
let edgeH3 = matrix [[-1.;0.;1.];[-2.;0.;2.];[-1.;0.;1.]]
let edgeV5 = matrix [[1.;2.;1.];[0.;0.;0.];[-1.;-2.;-1.]]
let edgeH5 = matrix [[-1.;0.;1.];[-2.;0.;2.];[-1.;0.;1.]]

let filtered = m |> convolve gaussian5

let eV = filtered |> convolve edgeV3
let eH = filtered |> convolve edgeH3

let e = matrixMap2 complex eH eV

e
|> Matrix.map Complex.magnitude
|> show

smallImage |> show
eV |> show
eH |> show