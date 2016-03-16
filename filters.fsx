module filters
#I "packages/MathNet.Numerics/lib/net40"
#I "packages/MathNet.Numerics.FSharp/lib/net40"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

#load "matrixStuff.fsx"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open matrixStuff

let gaussian5 = matrix [[2.;4.;5.;4.;2.];
                        [4.;9.;12.;9.;4.];
                        [5.;12.;15.;12.;5.];
                        [4.;9.;12.;9.;4.];
                        [2.;4.;5.;4.;2.];] / 159.
let edgeV3 = matrix [[1.;2.;1.];
                     [0.;0.;0.];
                     [-1.;-2.;-1.]]
let edgeH3 = matrix [[1.;0.;-1.];
                     [2.;0.;-2.];
                     [1.;0.;-1.]]

let (|Between|_|) min max input =
    if min <= input && input < max then Some input else None

let isMaximumOrZero other1 other2 v =
    let oMag1 = Complex.magnitude other1
    let oMag2 = Complex.magnitude other2
    let vMag = Complex.magnitude v
    if vMag < oMag1 || vMag < oMag2 then Complex.zero else v 
    
let nonMaximumSuppression m =
    m |> matrixMapSubs 3 3 (complex nan nan) (fun y x sub ->
        let v = sub.[1,1]
        let phase = ((Complex.phase v) / System.Math.PI) // between -1 and 1
        let normPhase = if phase < 0. then phase + 1. else phase // between 0 and 1
        match normPhase * 2. with
        | Between 0.25 0.75 _ -> isMaximumOrZero sub.[0,0] sub.[2,2] v
        | Between 0.75 1.25 _ -> isMaximumOrZero sub.[1,0] sub.[1,2] v
        | Between 1.25 1.75 _ -> isMaximumOrZero sub.[0,2] sub.[2,0] v
        | _ (*Lt 0.25 or Gt 0.75*) -> isMaximumOrZero sub.[0,1] sub.[2,1] v
        )

let hysteresis low high (m:Matrix<float>) =
    let xLength = Matrix.columnCount m
    let yLength = Matrix.rowCount m
    let result = DenseMatrix.create yLength xLength nan
    let rec spread y x =
        match (y,x) with
        | y,x when y < 0 || y >= yLength || x < 0 || x >= xLength -> ()
        | _ ->
        match result.[y,x] with
        | 1.0|0.0 -> ()
        | _ ->
        match m.[y,x] with
        | v when System.Double.IsNaN v || v < low -> result.[y,x] <- 0.0
        | v when v < high -> ()
        | v (*v >= high*) ->
        result.[y,x] <- 1.0
        spread (y-1) (x-1)
        spread (y-1) (x)
        spread (y-1) (x+1)
        spread (y) (x-1)
        spread (y) (x+1)
        spread (y+1) (x-1)
        spread (y+1) (x)
        spread (y+1) (x+1)
         
    m
    |> Matrix.iteri (fun y x v -> spread y x)
    result |> Matrix.map (fun v -> if System.Double.IsNaN v then 0.0 else v)

let canny m =
    let filtered = m |> convolve gaussian5
    let eX = filtered |> convolve edgeV3
    let eY = filtered |> convolve edgeH3
    let e = matrixMap2 complex eX eY
    e 
    |> nonMaximumSuppression
    |> Matrix.map Complex.magnitude
    |> hysteresis 20. 40.