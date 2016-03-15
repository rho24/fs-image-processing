module bitmapStuff

#nowarn "9"

#load "packages/FsLab/FsLab.fsx"

open System
open System.Drawing
open Microsoft.FSharp.NativeInterop
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

let showBitmap (image:Image) =
    let tempFileName = System.IO.Path.GetTempFileName() + ".bmp"
    image.Save(tempFileName)
    System.Diagnostics.Process.Start("chrome", "file:///" + tempFileName) |> ignore


let scaledBitmap height (path:string) =
    let img = new Bitmap(path)
    let scale = float height / float img.Height
    img.GetThumbnailImage(int (float img.Width * scale), height, null, System.IntPtr.Zero) :?> Bitmap
let getColor x =
    Color.FromArgb(Convert.ToInt32(int16 (NativePtr.get x 2)),
        Convert.ToInt32(int16 (NativePtr.get x 1)),
        Convert.ToInt32(int16 (NativePtr.get x 0)),
        Convert.ToInt32(int16 (NativePtr.get x 3)))
        
let setColor (c:Color) x =
    NativePtr.set x 0 (byte c.R)
    NativePtr.set x 1 (byte c.G)
    NativePtr.set x 2 (byte c.B)
    NativePtr.set x 3 (byte c.A)

let floatToColor min max f =
    let v =
        match (min,max,f) with
        | _,_,f when Double.IsNaN f -> 0
        | min,max,_ when min = max -> 0
        | min,_,f when f <= min -> 0
        | _,max,f when f >= max -> 255
        | _ ->
        let scale = 255. / (max-min)
        let fv = ((f-min) * scale)
        int fv
    
    Color.FromArgb(v,v,v)

let imageToMatrix (img:Bitmap) =
    // lockbits on image so that the image can be processed quicker using unsafe means
    let bd = img.LockBits(Rectangle(0,0,img.Width,img.Height),System.Drawing.Imaging.ImageLockMode.ReadOnly,System.Drawing.Imaging.PixelFormat.Format32bppArgb)

    // pointer to use to go through the image
    let mutable (p:nativeptr<byte>) = NativePtr.ofNativeInt (bd.Scan0)
    let pixels = [
        for i=0 to img.Height-1 do
            for j=0 to img.Width-1 do
                // Get the color of the [x,y] pixel
                let colo = getColor p
                // add the ARGB value to our list
                let brightness = float(int colo.R + int colo.G + int colo.B) / 3. 
                
                yield i,j,float brightness
                // move to the next pixel on the row
                p <- NativePtr.add p 4
            done
            // The stride - the whole length (multiplied by four to account for the fact that we are looking at 4 byte pixels
            p <- NativePtr.add p (bd.Stride - bd.Width*4)
        done
    ]
    img.UnlockBits(bd)
    DenseMatrix.ofSeqi img.Height img.Width pixels
      
let matrixToImage (mat:Matrix<float>) =
    let min = Matrix.toSeq mat |> Seq.filter (Double.IsNaN >> not) |> Seq.min
    let max = Matrix.toSeq mat |> Seq.filter (Double.IsNaN >> not) |> Seq.max
    
    let img = new Bitmap(mat.ColumnCount, mat.RowCount)
    
    // lockbits on image so that the image can be processed quicker using unsafe means
    let bd = img.LockBits(Rectangle(0,0,img.Width,img.Height),System.Drawing.Imaging.ImageLockMode.WriteOnly,System.Drawing.Imaging.PixelFormat.Format32bppArgb)

    // pointer to use to go through the image
    let mutable (p:nativeptr<byte>) = NativePtr.ofNativeInt (bd.Scan0)
    for i=0 to img.Height-1 do
        for j=0 to img.Width-1 do
            let brightness = mat.[i,j]
            let colo = floatToColor min max brightness
            
            setColor colo p
            
            // move to the next pixel on the row
            p <- NativePtr.add p 4
        done
        // The stride - the whole length (multiplied by four to account for the fact that we are looking at 4 byte pixels
        p <- NativePtr.add p (bd.Stride - bd.Width*4)
    done
    img.UnlockBits(bd)
    img

let show img =
    match box img with 
    | :? Bitmap as b -> showBitmap b
    | :? Matrix<float> as m -> m |> matrixToImage |> showBitmap
    | _ -> ()