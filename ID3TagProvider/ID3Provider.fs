namespace DidacticCode.ID3

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

// Disable incomplete matches warning
// Incomplete matches are used extensively within this file
// to simplify the code
#nowarn "0025"

[<TypeProvider>]
type ID3TagProvider() as this =
  inherit TypeProviderForNamespaces()
  let ns = "DidacticCode.TypeProviders"
  let assy = Assembly.GetExecutingAssembly()

  let audioFileType = ProvidedTypeDefinition(assy, ns, "AudioFile", None)

  let buildExpr tag =
    fun [tags] -> <@@ (((%%tags:obj) :?> Dictionary<string, ID3Frame>).[tag]).GetContent() |> unbox @@>

  do
    audioFileType.DefineStaticParameters(
      [ ProvidedStaticParameter("fileName", typeof<string>) ],
      instantiationFunction = (
        fun typeName [| :? string as fileName |] -> 
          let ty = ProvidedTypeDefinition(assy, ns, typeName, None)

          makeProvidedConstructor
              [ ]
              (fun [] -> <@@ fileName |> ID3Reader.readID3Tags @@>)
          |> addDelayedXmlComment "Creates a reader for the specified file."
          |> ty.AddMember

          "HasTag"
          |> makeProvidedMethod<bool>
               ([ makeProvidedParameter<string> "tag" ])
               (fun [ tags; tag ] -> <@@ ((%%tags:obj) :?> Dictionary<string, ID3Frame>).ContainsKey(%%tag:string) @@>)
          |> addDelayedXmlComment "Returns a value indicating whether the specified tag was located within the source file"
          |> ty.AddMember

          "GetTag"
          |> makeProvidedMethod<ID3Frame option>
               [ makeProvidedParameter<string> "tag" ]
               (fun [ tags; tag ] -> <@@ let tagDict = ((%%tags:obj) :?> Dictionary<string, ID3Frame>)
                                         if tagDict.ContainsKey(%%tag:string) then Some tagDict.[(%%tag:string)]
                                         else None @@>)
          |> addDelayedXmlComment "Returns an ID3Frame object representing the specific tag"
          |> ty.AddMember

          fileName
          |> ID3Reader.readID3Tags
          |> Seq.map (fun i -> match i.Key with
                                | "APIC" as tag ->
                                    "AttachedPicture"
                                    |> makeReadOnlyProvidedProperty<AttachedPicture> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the album art attached to the file. Corresponds to the APIC tag."
                                | "MCDI" as tag ->
                                    "CdIdentifier"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the CD Identifier. Corresponds to the MCDI tag."
                                | "POPM" as tag ->
                                    "Popularimeter"
                                    |> makeReadOnlyProvidedProperty<Popularimeter> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the Popularimeter data including play count and rating. Corresponds to the POPM tag."
                                | "TALB" as tag ->
                                    "AlbumTitle"
                                    |> makeReadOnlyProvidedProperty<string>  (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the album title. Corresponds to the TALB tag."
                                | "TIT1" as tag ->
                                    "ContentGroup"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the content group. Corresponds to the TIT1 tag."
                                | "TIT2" as tag ->
                                    "TrackTitle"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track title. Corresponds to the TIT2 tag."
                                | "TIT3" as tag ->
                                    "TrackSubtitle"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track subtitle. Corresponds to the TIT3 tag."
                                | "TRCK" as tag ->
                                    "TrackNumber"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track number. Corresponds to the TRCK tag."
                                | "TYER" as tag ->
                                    "Year"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the year the track was released. Corresponds to the TYER tag."
                                | "TPE1" as tag ->
                                    "Performer"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track performer's name. Corresponds to the TPE1 tag."
                                | "TPE2" as tag ->
                                    "Band"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the band name. Corresponds to the TPE2 tag."
                                | "TPOS" as tag ->
                                    "SetIdentifier"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track's position within the set. Corresponds to the TPOS tag."
                                | "TPUB" as tag ->
                                    "Publisher"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track publisher's name. Corresponds to the TPUB tag."
                                | "TCOM" as tag ->
                                    "Composer"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track composer's name. Corresponds to the TCOM tag."
                                | "TCON" as tag ->
                                    "ContentType"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the track's content type. Corresponds to the TCON tag."
                                | "TCOP" as tag ->
                                    "Copyright"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the copyright information for the track. Corresponds to the TCOP tag."
                                | "TLEN" as tag ->
                                    "TrackLength"
                                    |> makeReadOnlyProvidedProperty<string> (buildExpr tag)
                                    |> addDelayedXmlComment "Gets the length of the track. Corresponds to the TLEN tag.")
          |> Seq.toList
          |> ty.AddMembers

          ty
      ))

  do
    this.AddNamespace(ns, [ audioFileType ])

[<assembly:TypeProviderAssembly>]
do ()