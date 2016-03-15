module matrixStuff

#load "packages/FsLab/FsLab.fsx"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
   
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
