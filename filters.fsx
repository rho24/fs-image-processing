module filters
#load "packages/FsLab/FsLab.fsx"
#load "matrixStuff.fsx"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open matrixStuff

let gaussian5 = matrix [[2.;4.;5.;4.;2.];[4.;9.;12.;9.;4.];[5.;12.;15.;12.;5.];[4.;9.;12.;9.;4.];[2.;4.;5.;4.;2.];] / 159.
let edgeV3 = matrix [[1.;2.;1.];[0.;0.;0.];[-1.;-2.;-1.]]
let edgeH3 = matrix [[-1.;0.;1.];[-2.;0.;2.];[-1.;0.;1.]]
let edgeV5 = matrix [[1.;2.;1.];[0.;0.;0.];[-1.;-2.;-1.]]
let edgeH5 = matrix [[-1.;0.;1.];[-2.;0.;2.];[-1.;0.;1.]]




let canny m =
    let filtered = m |> convolve gaussian5
    let eV = filtered |> convolve edgeV3
    let eH = filtered |> convolve edgeH3
    let e = matrixMap2 complex eH eV
    e |> Matrix.map Complex.magnitude