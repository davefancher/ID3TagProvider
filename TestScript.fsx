#r @"D:\Dev\Sandbox\ID3TagProvider\ID3TagProvider\bin\Debug\ID3TagProvider.dll"
#r "System.Windows.Forms"
#r "System.Drawing"

open System.Drawing
open System.IO
open System.Windows.Forms
open DidacticCode.TypeProviders
open DidacticCode.ID3

let song = new AudioFile< @"D:\Music\Tim Berg\Seek Bromance\01 Seek Bromance (Avicii Vocal Edit).mp3">()
//let song = new AudioFile< @"D:\Music\Afrojack\Take Over Control\02 Take Over Control (Feat  Eva Simons).mp3">()
//let song = new AudioFile< @"D:\Music\Tiësto\Kaleidoscope\02 Escape Me.mp3">()

//if song.HasTag "TIT2" then song.GetTag "TIT2" |> function | Some (TIT2 title) -> printfn "%s" title | _ -> ()

let thumb =
  song.AttachedPicture.Image
  |> (fun image -> new MemoryStream(image))
  |> (fun stream -> new Bitmap(stream))

let f =
  new Form(
    Text = song.TrackTitle,
    BackgroundImage = thumb,
    BackgroundImageLayout = ImageLayout.Center,
    Width = thumb.Width,
    Height = thumb.Height)

f.ShowDialog()
f.Dispose()
thumb.Dispose()