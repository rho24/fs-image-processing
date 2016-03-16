module matrixStuff

#I "packages/MathNet.Numerics/lib/net40"
#I "packages/MathNet.Numerics.FSharp/lib/net40"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

let matrixSub height width y x (m:Matrix<'a>) =
    if height % 2 = 0 then failwith "height must be odd"
    if width % 2 = 0 then failwith "width must be odd"
    let xMargin = (height - 1) / 2
    let yMargin = (width - 1) / 2
    m.SubMatrix(y-yMargin, height, x-xMargin, width)
    
let matrixMapSubs height width nan mapping m =
    if height % 2 = 0 then failwith "height must be odd"
    if width % 2 = 0 then failwith "width must be odd"
    let xMargin = (height - 1) / 2
    let yMargin = (width - 1) / 2
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
            let subM = matrixSub height width y x m
            mapping y x subM
        )

 
let convolve kernel m =
    let kernelXLength = Matrix.columnCount kernel
    let kernelYLength = Matrix.rowCount kernel
    m |> matrixMapSubs kernelYLength kernelXLength nan (fun y x sub ->
        let temp = Matrix.op_DotMultiply (kernel,sub)
        Matrix.sum temp
        )

let matrixMap2 mapping m1 m2 =
    if Matrix.columnCount m1 <> Matrix.columnCount m2 then failwith "Columns don't match"
    if Matrix.rowCount m1 <> Matrix.rowCount m2 then failwith "Rows don't match"
    
    Matrix.mapi (fun y x v ->
        mapping v m2.[y,x]) m1
