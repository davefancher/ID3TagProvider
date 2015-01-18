namespace DidacticCode.ID3

open System
open System.Collections.Generic
open System.IO
open System.Text
open Microsoft.FSharp.Core.LanguagePrimitives

[<AutoOpen>]
module CoreExtensions =
  let inline tee fn x = x |> fn |> ignore; x
  let inline (|>!) x fn = tee fn x

[<AutoOpen>]
module BinaryReaderExtensions =
  let private synchsafeIntToUInt32 synchsafe =
    (synchsafe &&& 0x7Fu) ||| ((synchsafe &&& 0x7F00u) >>> 1) ||| ((synchsafe &&& 0x7F0000u) >>> 2) ||| ((synchsafe &&& 0x7F000000u) >>> 3)

  type BinaryReader with
    member x.ReadSynchSafeInteger () =
      x.ReadBytes 4
      |> Array.rev
      |> (fun b -> BitConverter.ToUInt32(b, 0))
      |> synchsafeIntToUInt32
    member x.SkipBytes = x.ReadBytes >> ignore

type AttachedPicture = { TextEncoding: byte; MimeType: string; PictureType: byte; Description: string; Image: byte array }
type Popularimeter = { Source: string; Rating: byte; PlayCount: byte }

// There should be a flags label on this as well but I haven't implemented them yet
type ID3Header = { Identifier: string; Version: byte array; Size: uint32 }

type ID3Frame =
| APIC of AttachedPicture
| MCDI of string
| POPM of Popularimeter
| TALB of string
| TIT1 of string
| TIT2 of string
| TIT3 of string
| TRCK of string
| TYER of string
| TPE1 of string
| TPE2 of string
| TPOS of string
| TPUB of string
| TCOM of string
| TCON of string
| TCOP of string
| TLEN of string
  member x.GetContent () =
    match x with
    | APIC pic -> box pic
    | POPM pop -> box pop
    | MCDI content
    | TALB content
    | TIT1 content
    | TIT2 content
    | TIT3 content
    | TRCK content
    | TYER content
    | TPE1 content
    | TPE2 content
    | TPOS content
    | TPUB content
    | TCOM content
    | TCON content
    | TCOP content
    | TLEN content -> box content

type ByteArrayReader (byteArray: byte array) =
  let enumerator = byteArray.GetEnumerator() |>! (fun e -> e.MoveNext())
  member __.ReadByte () =
    enumerator.Current :?> byte |>! (fun _ -> enumerator.MoveNext())
  member __.ReadString () =
    let rec read (sb: StringBuilder) =
      match __.ReadByte() with
      | 0x00uy -> sb.ToString()
      | c -> c |> char |> sb.Append |> read
    read (StringBuilder())
  member __.ReadToEnd() =
    seq { yield enumerator.Current :?> byte
          while enumerator.MoveNext() do
            yield enumerator.Current :?> byte }

module ID3Reader =
  let private readAPICFrame (content: byte array) =
    let reader = ByteArrayReader content
    APIC {
      TextEncoding = reader.ReadByte()
      MimeType = reader.ReadString() |> function | "" -> "image/" | s -> s
      PictureType = reader.ReadByte()
      Description = reader.ReadString()
      Image = reader.ReadToEnd() |> Array.ofSeq }
  let private readMCDIFrame = Encoding.ASCII.GetString >> MCDI
  let private readPOPMFrame (content: byte array) =
    POPM { 
      Source = content.[..content.Length - 3] |> Encoding.ASCII.GetString
      Rating = content.[content.Length - 2]
      PlayCount = content.[content.Length - 1] }
  let private readTALBFrame = Encoding.ASCII.GetString >> TALB
  let private readTIT1Frame = Encoding.ASCII.GetString >> TIT1
  let private readTIT2Frame = Encoding.ASCII.GetString >> TIT2
  let private readTIT3Frame = Encoding.ASCII.GetString >> TIT3
  let private readTRCKFrame = Encoding.ASCII.GetString >> TRCK
  let private readTYERFrame = Encoding.ASCII.GetString >> TYER
  let private readTPE1Frame = Encoding.ASCII.GetString >> TPE1
  let private readTPE2Frame = Encoding.ASCII.GetString >> TPE2
  let private readTPOSFrame = Encoding.ASCII.GetString >> TPOS
  let private readTPUBFrame = Encoding.ASCII.GetString >> TPUB
  let private readTCOMFrame = Encoding.ASCII.GetString >> TCOM
  let private readTCONFrame = Encoding.ASCII.GetString >> TCON
  let private readTCOPFrame = Encoding.ASCII.GetString >> TCOP
  let private readTLENFrame = Encoding.ASCII.GetString >> TLEN

  let private readFrame (reader: BinaryReader) = 
      let id = reader.ReadBytes 4 |> Encoding.ASCII.GetString
      let size = reader.ReadBytes 4 |> Array.rev |> (fun b -> BitConverter.ToUInt32(b, 0))
      reader.SkipBytes 2 // These bytes represent the frame flags; skipping them for now
      let content = size |> int |> reader.ReadBytes

      match id with
      | "APIC" -> Some (id, content |> readAPICFrame)
      | "MCDI" -> Some (id, content.[1..] |> readMCDIFrame) // There's an encoding byte being ignored on these
      | "POPM" -> Some (id, content |> readPOPMFrame)
      | "TALB" -> Some (id, content.[1..] |> readTALBFrame)
      | "TIT1" -> Some (id, content.[1..] |> readTIT1Frame)
      | "TIT2" -> Some (id, content.[1..] |> readTIT2Frame)
      | "TIT3" -> Some (id, content.[1..] |> readTIT3Frame)
      | "TRCK" -> Some (id, content.[1..] |> readTRCKFrame)
      | "TYER" -> Some (id, content.[1..] |> readTYERFrame)
      | "TPE1" -> Some (id, content.[1..] |> readTPE1Frame)
      | "TPE2" -> Some (id, content.[1..] |> readTPE2Frame)
      | "TPOS" -> Some (id, content.[1..] |> readTPOSFrame)
      | "TPUB" -> Some (id, content.[1..] |> readTPUBFrame)
      | "TCOM" -> Some (id, content.[1..] |> readTCOMFrame)
      | "TCON" -> Some (id, content.[1..] |> readTCONFrame)
      | "TCOP" -> Some (id, content.[1..] |> readTCOPFrame)
      | "TLEN" -> Some (id, content.[1..] |> readTLENFrame)
      | "PRIV"
      | "\u0000\u0000\u0000\u0000" -> None
      | x -> printfn "Unsupported Tag: %s" x; None

  let private readID3Header (reader: BinaryReader) =
    let id = reader.ReadBytes 3 |> Encoding.ASCII.GetString
    let version = reader.ReadBytes 2
    // The next byte represent the header flags which aren't supported in this simple reader
    reader.SkipBytes 1
    let size = reader.ReadSynchSafeInteger()
    { Identifier = id; Version = version; Size = size }

  // A proper implementation would keep better track of the tags since there are a number of
  // tags such as APIC that can be repeated. For the purpose of this exercise
  // we'll just use the most recently found value
  let private readFrames (header: ID3Header) (reader: BinaryReader) =
    let rec getFramesImpl (frames: Dictionary<string, ID3Frame>) =
      match reader.BaseStream.Position with
      | p when p >= int64 (header.Size + 3u) ->
        frames
      | _ ->
        (match readFrame reader with
         | Some (id, frame) ->
            frames |>! (fun f -> f.[id] <- frame)
         | _ ->
            frames)
        |> getFramesImpl
    Dictionary<string, ID3Frame>() |> getFramesImpl

  let private openFileForRead fileName = new FileStream(fileName, FileMode.Open, FileAccess.Read)
  let private createBinaryReader input = new BinaryReader(input)

  let readID3Frames fn =
    use reader = fn |> openFileForRead |> createBinaryReader
    let header = reader |> readID3Header
    reader |> readFrames header
