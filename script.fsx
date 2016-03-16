#load "packages/FsLab/FsLab.fsx"
#load "bitmapStuff.fsx"
#load "matrixStuff.fsx"
#load "filters.fsx"

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open bitmapStuff
open matrixStuff
open filters

let m = scaledBitmap 800 "sudoku.jpg" |> imageToMatrix

m |> canny |> show
