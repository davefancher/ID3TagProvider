
#r "System.Windows.Forms"
#r "System.Drawing"

#load "ID3.fs"

open System.Drawing
open System.IO
open System.Windows.Forms
open DidacticCode.ID3

let sourceFile = @"D:\Music\Tim Berg\Seek Bromance\01 Seek Bromance (Avicii Vocal Edit).mp3"

let tags =
  sourceFile
  |> ID3Reader.readID3Frames

tags
|> Seq.iter (printfn "%A")

let thumb =
  tags.["APIC"]
  |> (function | APIC(pic) -> Some pic.Image | _ -> None)
  |> (function | Some image -> new MemoryStream(image) | None -> new MemoryStream())
  |> (fun stream -> new Bitmap(stream))

let f = new Form(Text = "Thumbnail", BackgroundImage = thumb)

f.ShowDialog()
f.Dispose()
thumb.Dispose()